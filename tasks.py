from invoke import task

@task
def clean(ctx):
    ctx.run("rm -f a.out")
    ctx.run("rm -rf specbin/")
    ctx.run("cargo clean")

@task(default=True)
def build(ctx, release=False):
    cargo_args = "build"
    if release:
        cargo_args += " --release"
    ctx.run("cargo " + cargo_args)

@task
def docs(ctx):
    with ctx.cd("docs/"):
        ctx.run("docket")

@task(build)
def test(ctx):
    ctx.run("cargo test")
    ctx.run("python specs.py")

@task
def clippy(ctx):
    ctx.run("cargo clippy")
