from invoke import task
import os
import glob

@task
def clean(ctx):
    ctx.run("rm -f a.out")
    ctx.run("rm -rf specbin/")
    ctx.run("cargo clean")

@task(default=True)
def build(ctx, release=True):
    mode = "--release" if release else "--debug"
    ctx.run("cargo build " + mode)

@task
def docs(ctx):
    with ctx.cd("docs/"):
        ctx.run("docket")

@task(build)
def test(ctx):
    ctx.run("cargo test")
    ctx.run("python specs.py")

@task(build)
def bench(ctx, opt_level=3):
    for bench in glob.glob("spec/bench/*.ulg"):
        output = bench.lstrip('spec/').rstrip('.ulg')
        output = os.path.join("specbin", "bench", output)
        try:
            os.makedirs(os.path.dirname(output))
        except OSError:
            pass
        print "bench={0}, output={1}, opt={2}".format(bench, output, opt_level)
        ctx.run("target/release/ullage {0} -O{1} -o {2}"
                .format(bench, opt_level, output))
        ctx.run("time {0}".format(output))

