#!/bin/bash
set -euo pipefail

sample_name="${1}"
shift

scripts_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
bpl_file="${repo_dir}/out/${sample_name}.bpl"

"${scripts_dir}/run-corral.sh" "${bpl_file}"
