set -ex

cd "$(dirname "$0")"

git worktree add branch testing
cargo run --example testing -- generate branch
pushd branch
git add .
git diff --cached --exit-code
if [ $? -neq 0 ]; then
    git commit -m "Update generated code"
fi
popd
rm -rf branch
git worktree prune