fn main() {
    if std::env::var_os("CARGO_FEATURE_GPU").is_none() {
        return;
    }
    let root = std::path::PathBuf::from(
        std::env::var("LIBTORCH").expect("set LIBTORCH; use build-gpu.sh"),
    );
    let runtime = root.parent().unwrap().join("nvidia/cuda_runtime/include");
    cc::Build::new()
        .cpp(true)
        .file("cuda_control.cpp")
        .include(root.join("include"))
        .include(root.join("include/torch/csrc/api/include"))
        .include(runtime)
        .include(root.parent().unwrap().join("nvidia/cuda_nvcc/include"))
        .flag("-std=c++17")
        .define("_GLIBCXX_USE_CXX11_ABI", "1")
        .compile("organism_cuda_control");
    println!(
        "cargo:rustc-link-arg=-Wl,-rpath,{}",
        root.join("lib").display()
    );
    println!("cargo:rustc-link-lib=c10_cuda");
    println!("cargo:rustc-link-arg=-Wl,--no-as-needed,-ltorch_cuda,--as-needed");
    println!("cargo:rerun-if-changed=cuda_control.cpp");
}
