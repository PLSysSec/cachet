#!/bin/bash
set -euo pipefail

bpl_file="${1}"
shift

# We change directories below, so convert relative paths to absolute based on
# the original working directory.
bpl_file="$(realpath -s "${bpl_file}")"

repo_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)"
corral_exe="${repo_dir}/vendor/corral/source/Corral/bin/Release/net6.0/corral"

# Change to a temporary directory to collect any detritus that Corral leaves
# behind.
tmp_dir="$(mktemp -d)"
trap 'rm -rf -- "${tmp_dir}"' EXIT 
cd "${tmp_dir}"

"${corral_exe}" "${bpl_file}" /trackAllVars /recursionBound:9999999 "${@}"
