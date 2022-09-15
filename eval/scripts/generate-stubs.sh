#!/bin/bash
set -euo pipefail

eval_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)"
data_dir="${eval_dir}/data"
spew_dir="${data_dir}/spew"

rm -rf -- "${spew_dir}"

cd "${eval_dir}"
pnpm run speedometer -- "${@}"
