#!/bin/bash
set -euo pipefail

sample_name="${1}"
shift

scripts_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"

echo Compiling ${sample_name}.cachet
echo
"${scripts_dir}/compile.sh" "${sample_name}"
echo
echo Verifying ${sample_name}.cachet
echo
"${scripts_dir}/verify.sh" "${sample_name}" "${@}"
