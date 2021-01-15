set -ex

cd "$(dirname "$0")"

git worktree add branch testing
cargo run --example testing -- generate branch/generated-code
pushd branch
git add .
git diff --cached --exit-code || git commit -m "Update generated code"
popd
rm -rf branch
git worktree prune