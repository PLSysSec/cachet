#!/bin/bash
set -euo pipefail

repo_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)
out_dir="${repo_dir}/out"
bpl_file="${out_dir}/${1}.bpl"
corral_exe="${repo_dir}/../corral/source/Corral/bin/Release/net5.0/corral"

"${corral_exe}" "${bpl_file}" /trackAllVars /recursionBound:4
