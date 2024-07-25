#!/bin/bash
set -euo pipefail
shopt -s nullglob

scripts_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
source "${scripts_dir}/common.sh"

function count_lines_of_code {
  cloc --force-lang=rust,cachet --json "${@}" | jq .SUM.code
}

echo "Ops:"
echo
echo CacheIR
echo -------
cacheir_op_count="$(grep -h '^ *op [A-Za-z0-9_]\+ *(' "${cacheir_cachet_file}" "${repro_cacheir_cachet_file}" | cut -sd ' ' -f 6 | cut -sd '(' -f 1 | sort | uniq | tee /dev/tty | wc -l)"
helper_cacheir_ops=(
  Assert
  AssertEqValueInput
  AssertEqValueOutput
  AssertStackGuard
  InitStackGuard
)
cacheir_op_count="$((${cacheir_op_count}-${#helper_cacheir_ops[@]}))"
echo "==> CacheIR op count: ${cacheir_op_count}"

echo
echo MASM
echo ----
masm_op_count="$(grep -h '^ *op [A-Za-z0-9_]\+ *(' "${masm_cachet_file}" "${repro_masm_cachet_file}" | cut -sd ' ' -f 6 | cut -sd '(' -f 1 | sort | uniq | tee /dev/tty | wc -l)"
helper_masm_ops=(
  Assert
  AssertEqValue
)
masm_op_count="$((${masm_op_count}-${#helper_masm_ops[@]}))"
echo "==> MASM op count: ${masm_op_count}"

echo

echo "SLOC:"

cacheir_sloc="$(count_lines_of_code "${cacheir_cachet_file}")"
echo "CacheIR-to-MASM compiler SLOC: ${cacheir_sloc}"

masm_sloc="$(count_lines_of_code "${masm_cachet_file}")"
echo "MASM interpreter SLOC: ${masm_sloc}"

js_sloc="$(count_lines_of_code "${js_cachet_file}")"
echo "JS VM model SLOC: ${js_sloc}"

echo

test_stub_count="$(find "${test_stubs_dir}" \( -name '*-*.cachet' \) -prune -o -type f -print | wc -l)"
echo "# test stubs: ${test_stub_count}"
