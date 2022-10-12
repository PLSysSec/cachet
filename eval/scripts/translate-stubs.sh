#!/bin/bash
set -euo pipefail
shopt -s nullglob

scripts_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
source "${scripts_dir}/common.sh"

if [[ "${#}" = 0 ]]; then
  stub_files_glob="${default_glob}"
  rm -rf -- "${cachet_dir}"
else
  stub_files_glob="${1}"
  shift

  rm -f -- "${cachet_dir}/"${stub_files_glob}.cachet
fi

cargo build -p cachet-cacheir-translator
echo

selectors=()
for stub_file in "${stubs_dir}/"${stub_files_glob}.cacheir; do
  stub_file_rel="${stub_file#"${stubs_dir}/"}"
  selector="${stub_file_rel%.cacheir}"
  selectors+=("${selector}")
done

if [[ "${#selectors[@]}" = 0 ]]; then
  >&2 echo "Error: Nothing matched \"${stub_files_glob}\""
  exit 1
else
  parallel "${scripts_dir}/translate-stub.sh" ::: "${selectors[@]}"
fi

#for engine_stubs_dir in "${stubs_dir}"/*; do
#  engine="$(basename "${engine_stubs_dir}")"
#  if [[ " ${engine_allow_list} " =~ " ${engine} " ]]; then
#    engine_cachet_dir="${cachet_dir}/${engine}"
#    for cache_kind_stubs_dir in "${engine_stubs_dir}"/*; do
#      cache_kind="$(basename "${cache_kind_stubs_dir}")"
#      cache_kind_cachet_dir="${engine_cachet_dir}/${cache_kind}"
#      for stub_file in "${cache_kind_stubs_dir}"/*; do
#        stub_file_name="$(basename "${stub_file}")"
#        cachet_file_name="${stub_file_name%.cacheir}.cachet"
#        cachet_file="${cache_kind_cachet_dir}/${cachet_file_name}"
#        echo "${stub_file} -> ${cachet_file}"
#        mkdir -p "${cache_kind_cachet_dir}"
#        cargo run -p cachet-cacheir-translator < "${stub_file}" > "${cachet_file}"
#      done
#    done
#  fi
#done
