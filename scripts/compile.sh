#!/bin/bash
set -euo pipefail

sample_name="${1}"
shift

repo_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)"

notes_dir="${repo_dir}/notes"
cachet_file="${notes_dir}/stubs/${sample_name}.cachet"
hacks_bpl_file="${notes_dir}/hacks.bpl"

out_dir="${repo_dir}/out"
mkdir -p "${out_dir}"
cpp_decls_file="${out_dir}/${sample_name}.h"
cpp_defs_file="${out_dir}/${sample_name}.inc"
bpl_file="${out_dir}/${sample_name}.bpl"

time (
  cargo run --bin cachet-compiler -- "${cachet_file}" \
    --cpp-decls "${cpp_decls_file}" \
    --cpp-defs "${cpp_defs_file}" \
    --bpl "${bpl_file}" \
    "${@}"
  cat "${hacks_bpl_file}" "${bpl_file}" | sponge "${bpl_file}"
  cargo run --bin bpl-tree-shaker -- -i "${bpl_file}" -t '#JSOp' -p '#MASM^Op'
)
