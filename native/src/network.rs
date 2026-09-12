//! Residual policy/value network and checkpointable Adam, using libtorch directly.
use anyhow::Result;
use std::{collections::BTreeMap, path::Path};
use tch::{Device, Kind, Tensor, nn};

fn bn_config() -> nn::BatchNormConfig {
    nn::BatchNormConfig {
        ws_init: nn::Init::Const(1.),
        ..Default::default()
    }
}

struct Block {
    c1: nn::Conv2D,
    b1: nn::BatchNorm,
    c2: nn::Conv2D,
    b2: nn::BatchNorm,
}
impl Block {
    fn new(p: nn::Path, n: i64) -> Self {
        let cfg = nn::ConvConfig {
            padding: 1,
            bias: false,
            ..Default::default()
        };
        Self {
            c1: nn::conv2d(&p / "conv1", n, n, 3, cfg),
            b1: nn::batch_norm2d(&p / "bn1", n, bn_config()),
            c2: nn::conv2d(&p / "conv2", n, n, 3, cfg),
            b2: nn::batch_norm2d(&p / "bn2", n, bn_config()),
        }
    }
    fn forward(&self, x: &Tensor, train: bool) -> Tensor {
        (x.apply(&self.c1)
            .apply_t(&self.b1, train)
            .relu()
            .apply(&self.c2)
            .apply_t(&self.b2, train)
            + x)
            .relu()
    }
}
pub struct Network {
    pub vs: nn::VarStore,
    input: nn::Conv2D,
    bn: nn::BatchNorm,
    blocks: Vec<Block>,
    pc: nn::Conv2D,
    pb: nn::BatchNorm,
    pf: nn::Linear,
    vc: nn::Conv2D,
    vb: nn::BatchNorm,
    v1: nn::Linear,
    v2: nn::Linear,
    pub players: usize,
    pub grid: usize,
    pub actions: usize,
}
impl Network {
    pub fn new(
        players: usize,
        grid: usize,
        actions: usize,
        blocks: usize,
        filters: i64,
        device: Device,
    ) -> Self {
        let vs = nn::VarStore::new(device);
        let p = vs.root();
        let cfg = nn::ConvConfig {
            bias: false,
            ..Default::default()
        };
        let input = nn::conv2d(
            &p / "input_conv",
            (6 * players + 22) as i64,
            filters,
            3,
            nn::ConvConfig { padding: 1, ..cfg },
        );
        let bn = nn::batch_norm2d(&p / "input_bn", filters, bn_config());
        let blocks = (0..blocks)
            .map(|i| Block::new(&p / "res_blocks" / i, filters))
            .collect();
        let pc = nn::conv2d(&p / "policy_conv", filters, 2, 1, cfg);
        let pb = nn::batch_norm2d(&p / "policy_bn", 2, bn_config());
        let pf = nn::linear(
            &p / "policy_fc",
            (2 * grid * grid) as i64,
            actions as i64,
            Default::default(),
        );
        let vc = nn::conv2d(&p / "value_conv", filters, 1, 1, cfg);
        let vb = nn::batch_norm2d(&p / "value_bn", 1, bn_config());
        let v1 = nn::linear(
            &p / "value_fc1",
            (grid * grid) as i64,
            256,
            Default::default(),
        );
        let v2 = nn::linear(&p / "value_fc2", 256, players as i64, Default::default());
        Self {
            vs,
            input,
            bn,
            blocks,
            pc,
            pb,
            pf,
            vc,
            vb,
            v1,
            v2,
            players,
            grid,
            actions,
        }
    }
    pub fn import_spatial_features(&mut self, path: &Path) -> Result<usize> {
        let old: BTreeMap<_, _> = Tensor::load_multi_with_device(path, self.vs.device())?
            .into_iter()
            .collect();
        let mut copied = 0;
        tch::no_grad(|| -> Result<()> {
            for (name, mut tensor) in self.vs.variables() {
                if name.starts_with("input_")
                    || name.starts_with("res_blocks.")
                    || name.starts_with("policy_conv.")
                    || name.starts_with("policy_bn.")
                    || name.starts_with("value_conv.")
                    || name.starts_with("value_bn.")
                {
                    let source = old
                        .get(&name)
                        .ok_or_else(|| anyhow::anyhow!("missing transfer tensor {name}"))?;
                    anyhow::ensure!(
                        source.size() == tensor.size(),
                        "incompatible transfer tensor {name}"
                    );
                    tensor.copy_(source);
                    copied += 1;
                }
            }
            Ok(())
        })?;
        Ok(copied)
    }
    pub fn forward(&self, x: &Tensor, train: bool) -> (Tensor, Tensor) {
        let mut h = x.apply(&self.input).apply_t(&self.bn, train).relu();
        for b in &self.blocks {
            h = b.forward(&h, train)
        }
        let p = h
            .apply(&self.pc)
            .apply_t(&self.pb, train)
            .relu()
            .flatten(1, -1)
            .apply(&self.pf)
            .log_softmax(-1, Kind::Float);
        let v = h
            .apply(&self.vc)
            .apply_t(&self.vb, train)
            .relu()
            .flatten(1, -1)
            .apply(&self.v1)
            .relu()
            .apply(&self.v2)
            .tanh();
        (p, v)
    }
    pub fn input(&self, values: &[f32], batch: usize) -> Tensor {
        Tensor::from_slice(values)
            .view([
                batch as i64,
                (6 * self.players + 22) as i64,
                self.grid as i64,
                self.grid as i64,
            ])
            .to_device(self.vs.device())
    }
    pub fn infer(&self, values: &[f32], batch: usize) -> Result<(Vec<f32>, Vec<f32>)> {
        tch::no_grad(|| {
            let (p, v) = self.forward(&self.input(values, batch), false);
            Ok((
                Vec::<f32>::try_from(p.exp().flatten(0, -1).to_device(Device::Cpu))?,
                Vec::<f32>::try_from(v.flatten(0, -1).to_device(Device::Cpu))?,
            ))
        })
    }
}

pub struct Adam {
    m: BTreeMap<String, Tensor>,
    v: BTreeMap<String, Tensor>,
    pub step: u64,
}
impl Adam {
    pub fn new() -> Self {
        Self {
            m: BTreeMap::new(),
            v: BTreeMap::new(),
            step: 0,
        }
    }
    pub fn update(&mut self, net: &Network, loss: &Tensor, lr: f64) -> Result<()> {
        let params: BTreeMap<_, _> = net
            .vs
            .variables()
            .into_iter()
            .filter(|(_, v)| v.requires_grad())
            .collect();
        for p in params.values() {
            let mut p = p.shallow_clone();
            p.zero_grad();
        }
        loss.backward();
        let norms: Vec<_> = params
            .values()
            .map(|p| p.grad().square().sum(Kind::Float))
            .collect();
        let norm = f64::try_from(Tensor::stack(&norms, 0).sum(Kind::Float).sqrt())?;
        let clip = (5. / (norm + 1e-6)).min(1.);
        self.step += 1;
        tch::no_grad(|| -> Result<()> {
            for (name, mut p) in params {
                let g = p.grad() * clip + &p * 1e-4;
                let m = self
                    .m
                    .entry(name.clone())
                    .or_insert_with(|| Tensor::zeros_like(&p));
                let v = self.v.entry(name).or_insert_with(|| Tensor::zeros_like(&p));
                m.copy_(&(&*m * 0.9 + &g * 0.1));
                v.copy_(&(&*v * 0.999 + g.square() * 0.001));
                let update = (&*m / (1. - 0.9f64.powf(self.step as f64)))
                    / ((&*v / (1. - 0.999f64.powf(self.step as f64))).sqrt() + 1e-8);
                p.copy_(&(&p - update * lr));
            }
            Ok(())
        })
    }
    pub fn save(&self, path: &Path) -> Result<()> {
        let mut tensors = vec![("step".to_string(), Tensor::from(self.step as i64))];
        for (n, t) in &self.m {
            tensors.push((format!("m/{n}"), t.shallow_clone()));
        }
        for (n, t) in &self.v {
            tensors.push((format!("v/{n}"), t.shallow_clone()));
        }
        Tensor::save_multi(&tensors, path)?;
        Ok(())
    }
    pub fn load(&mut self, path: &Path, device: Device) -> Result<()> {
        for (n, t) in Tensor::load_multi_with_device(path, device)? {
            if n == "step" {
                self.step = i64::try_from(t)? as u64;
            } else if let Some(n) = n.strip_prefix("m/") {
                self.m.insert(n.into(), t);
            } else if let Some(n) = n.strip_prefix("v/") {
                self.v.insert(n.into(), t);
            }
        }
        Ok(())
    }
}
