#!/bin/bash
set -euo pipefail

scripts_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
source "${scripts_dir}/common.sh"

rm -rf -- "${spew_dir}"

cd "${eval_dir}"
time pnpm run speedometer -- "${@}"
