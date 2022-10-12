#!/bin/bash
set -euo pipefail
shopt -s nullglob

scripts_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
source "${scripts_dir}/common.sh"

selector="${1}"
shift

stub_file="${stubs_dir}/${selector}.cacheir"
cachet_file="${cachet_dir}/${selector}.cachet"
echo "==> ${stub_file} -> ${cachet_file}"
mk_parent_dir "${cachet_file}"

"${bin_dir}/cachet-cacheir-translator" < "${stub_file}" > "${cachet_file}"
