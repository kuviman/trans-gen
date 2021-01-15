set -ex

cd "$(dirname "$0")"

git worktree add branch testing
cargo run --example testing -- generate branch
pushd branch
git add .
git commit -m "Update generated code" || true
popd
rm -rf branch
git worktree prune