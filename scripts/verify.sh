#!/bin/bash
set -euo pipefail

repo_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)"
bpl_file="${repo_dir}/out/${1}.bpl"
corral_exe="${repo_dir}/vendor/corral/source/Corral/bin/Release/net5.0/corral"

"${corral_exe}" "${bpl_file}" /trackAllVars /recursionBound:4
