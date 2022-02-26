#!/bin/bash
set -euo pipefail

repo_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)"

notes_dir="${repo_dir}/notes"
cachet_file="${notes_dir}/${1}.cachet"
prelude_bpl_file="${notes_dir}/prelude.bpl"

out_dir="${repo_dir}/out"
mkdir -p "${out_dir}"
cpp_decls_file="${out_dir}/${1}.h"
cpp_defs_file="${out_dir}/${1}.inc"
bpl_file="${out_dir}/${1}.bpl"

cargo run --bin cachet-compiler -- \
  "${cachet_file}" "${cpp_decls_file}" "${cpp_defs_file}" "${bpl_file}"
cat "${prelude_bpl_file}" "${bpl_file}" | sponge "${bpl_file}"
