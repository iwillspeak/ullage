features := "llvm-13"

build:
    cargo build --release --no-default-features --features={{features}}

test: build
    cargo test --no-default-features --features={{features}}
    python3 specs.py

clean:
    rm -f a.out
    rm -rf specbin/
    cargo clean

docs:
    cd docs/; docket

clippy:
    cargo clippy

bench opt_level="3": build
    #!/usr/bin/env python3
    import os
    import glob
    import subprocess
    
    for bench in glob.glob("spec/bench/*.ulg"):
        output = bench.lstrip('spec/').rstrip('.ulg')
        output = os.path.join("specbin", "bench", output)
        try:
            os.makedirs(os.path.dirname(output))
        except OSError:
            pass
        print("bench={0}, output={1}, opt={2}".format(bench, output, {{opt_level}}))
        subprocess.call(["target/release/ullage", bench, "-O{{opt_level}}", "-o", output])
        subprocess.call(["time", output])