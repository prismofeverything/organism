pub mod args;
pub mod game;
pub mod search;

// `serve` links libtorch to answer positions with a trained model; `gpu` adds
// the trainer, which also needs CUDA. A host that only plays the bot builds
// `serve` alone against a CPU libtorch — no CUDA toolchain, no 1.6 GB of
// driver libraries.
#[cfg(feature = "torch")]
pub mod network;
#[cfg(feature = "torch")]
pub mod serve;

#[cfg(feature = "gpu")]
pub mod train;
#[cfg(feature = "gpu")]
pub mod evaluate;
#[cfg(feature = "gpu")]
pub mod benchmark;
#[cfg(feature = "gpu")]
pub mod curriculum;
#[cfg(feature = "gpu")]
pub mod compare;
