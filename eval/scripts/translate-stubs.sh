#!/bin/bash
set -euo pipefail
shopt -s nullglob

eval_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)"
data_dir="${eval_dir}/data"
spew_dir="${data_dir}/spew"
stubs_dir="${spew_dir}/stubs"
cachet_dir="${data_dir}/cachet"

engine_allow_list=(IonIC)

rm -rf -- "${cachet_dir}"
for engine_stubs_dir in "${stubs_dir}"/*; do
  engine="$(basename "${engine_stubs_dir}")"
  if [[ " ${engine_allow_list} " =~ " ${engine} " ]]; then
    engine_cachet_dir="${cachet_dir}/${engine}"
    for cache_kind_stubs_dir in "${engine_stubs_dir}"/*; do
      cache_kind="$(basename "${cache_kind_stubs_dir}")"
      cache_kind_cachet_dir="${engine_cachet_dir}/${cache_kind}"
      for stub_file in "${cache_kind_stubs_dir}"/*; do
        stub_file_name="$(basename "${stub_file}")"
        cachet_file_name="${stub_file_name%.cacheir}.cachet"
        cachet_file="${cache_kind_cachet_dir}/${cachet_file_name}"
        echo "${stub_file} -> ${cachet_file}"
        mkdir -p "${cache_kind_cachet_dir}"
        cargo run -p cachet-cacheir-translator < "${stub_file}" > "${cachet_file}"
      done
    done
  fi
done
