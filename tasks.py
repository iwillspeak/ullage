from invoke import task

@task
def clean(ctx):
    ctx.run("rm -f a.out")
    ctx.run("rm -rf specbin/")
    ctx.run("cargo clean")

@task(default=True)
def build(ctx):
    ctx.run("cargo build")

@task
def docs(ctx):
    with ctx.cd("docs/"):
        ctx.run("d")

@task
def test(ctx):
    ctx.run("cargo test")
    ctx.run("python specs.py")
