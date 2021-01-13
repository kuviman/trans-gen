set -ex

cd "$(dirname "$0")"

git worktree add branch aicup2020-codecraft
cargo run --example aicup2020-codecraft -- generate branch/generated-code
pushd branch
git add .
git commit -m "Update generated code" || true
popd
rm -rf branch
git worktree prune