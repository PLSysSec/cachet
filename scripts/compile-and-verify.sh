#!/bin/bash
set -euo pipefail

sample_name="${1}"
shift

scripts_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
"${scripts_dir}/compile.sh" "${sample_name}"
"${scripts_dir}/verify.sh" "${sample_name}" "${@}"
