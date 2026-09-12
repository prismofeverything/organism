#![cfg(feature = "gpu")]
use organism_train::network::{Adam, Network};
use serde_json::Value;
use tch::{Device, Kind, Tensor};
fn floats(v: &Value) -> Vec<f32> {
    v.as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_f64().unwrap() as f32)
        .collect()
}
fn check(actual: &Tensor, expected: &[f32], tolerance: f32, label: &str) {
    let values = Vec::<f32>::try_from(actual.flatten(0, -1).to_device(Device::Cpu)).unwrap();
    assert_eq!(values.len(), expected.len(), "{label}");
    let error = values
        .iter()
        .zip(expected)
        .map(|(a, b)| (a - b).abs())
        .fold(0f32, f32::max);
    assert!(
        error <= tolerance,
        "{label}: maximum error {error} > {tolerance}"
    );
}
#[test]
#[ignore = "requires generated Python fixture; set ORGANISM_NETWORK_FIXTURE"]
fn python_forward_adam_and_native_resume() -> anyhow::Result<()> {
    tch::set_num_threads(2);
    let root = std::path::PathBuf::from(std::env::var("ORGANISM_NETWORK_FIXTURE")?);
    let f: Value = serde_json::from_reader(std::fs::File::open(root.join("fixture.json"))?)?;
    let device = if std::env::var_os("ORGANISM_TEST_CUDA").is_some() {
        Device::Cuda(0)
    } else {
        Device::Cpu
    };
    let mut net = Network::new(2, 18, 71, 1, 8, device);
    net.vs.load(root.join("initial.pt"))?;
    let x = net.input(&floats(&f["x"]), 4);
    let (p, v) = tch::no_grad(|| net.forward(&x, false));
    check(&p.exp(), &floats(&f["policy"]), 5e-5, "policy");
    check(&v, &floats(&f["value"]), 5e-5, "value");
    let pi = Tensor::from_slice(&floats(&f["pi"]))
        .view([4, 71])
        .to_device(device);
    let target = Tensor::from_slice(&floats(&f["v"]))
        .view([4, 2])
        .to_device(device);
    let mut adam = Adam::new();
    for step in 0..2 {
        let (p, v) = net.forward(&x, true);
        let loss = -(p * &pi)
            .sum_dim_intlist(&[-1i64][..], false, Kind::Float)
            .mean(Kind::Float)
            + v.mse_loss(&target, tch::Reduction::Mean);
        adam.update(&net, &loss, 1e-3)?;
        if step == 0 {
            net.vs.save(root.join("resume.ot"))?;
            adam.save(&root.join("adam.ot"))?;
            net = Network::new(2, 18, 71, 1, 8, device);
            net.vs.load(root.join("resume.ot"))?;
            adam = Adam::new();
            adam.load(&root.join("adam.ot"), device)?;
        }
    }
    assert_eq!(adam.step, 2);
    for (name, tensor) in net.vs.variables() {
        check(&tensor, &floats(&f["parameters"][&name]), 5e-5, &name);
    }
    Ok(())
}
