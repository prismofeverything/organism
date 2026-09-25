//! Command-line reading, shared by every subcommand.
//!
//! This lives outside the training modules because the move server needs it too
//! and is built without them: a host that only serves a trained model links a
//! CPU libtorch and has no CUDA to compile the trainer against.

/// The value following `name`, or `default`. The last occurrence wins, so a
/// wrapper script can set a flag and a caller can still override it.
pub fn argument(args: &[String], name: &str, default: &str) -> String {
    args.iter()
        .rposition(|a| a == name)
        .and_then(|i| args.get(i + 1))
        .cloned()
        .unwrap_or_else(|| default.into())
}
