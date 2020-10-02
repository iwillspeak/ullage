use std::env;
use std::path::PathBuf;

fn main() {
	//build our library as C++ static lib
    cc::Build::new()
        .cpp(true)
        .file("llvmext/lib.cpp")
        .compile("libllvmext.a");
	
	// Generate bindings for the library's exported types
    let bindings = bindgen::Builder::default()
        .header("llvmext/llvmext.hpp")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Unable to generate bindings");

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
