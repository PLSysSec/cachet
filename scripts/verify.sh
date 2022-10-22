#!/bin/bash
set -euo pipefail

sample_name="${1}"
shift

repo_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)"
bpl_file="${repo_dir}/out/${sample_name}.bpl"
corral_exe="${repo_dir}/vendor/corral/source/Corral/bin/Release/net6.0/corral"

"${corral_exe}" "${bpl_file}" /trackAllVars /recursionBound:4
