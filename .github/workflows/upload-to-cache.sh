#!/usr/bin/env bash
set -euo pipefail

pathsToPush=$(comm -13 <(sort ~/.nix-store-paths) <(./.github/workflows/list-nix-store.sh))

export PATH=$PATH:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/per-user/runner/profile/bin

echo "Signing and uploading paths: [$pathsToPush]"

exec nix copy --verbose --to ./.cache/nix $pathsToPush