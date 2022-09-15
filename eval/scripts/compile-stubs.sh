#!/bin/bash
set -euo pipefail
shopt -s nullglob

eval_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)"

repo_dir="$(cd -- "${eval_dir}" && cd .. && pwd)"
notes_dir="${repo_dir}/notes"
hacks_bpl_file="${notes_dir}/hacks.bpl"

data_dir="${eval_dir}/data"
cachet_dir="${data_dir}/cachet"
bpl_dir="${data_dir}/bpl"

compile_logs_dir="${data_dir}/compile-logs"
all_compile_logs_dir="${compile_logs_dir}/all"
successful_compile_logs_dir="${compile_logs_dir}/successes"
failed_compile_logs_dir="${compile_logs_dir}/failures"

engine_allow_list=(IonIC)

rm -rf -- "${bpl_dir}"
rm -rf -- "${compile_logs_dir}"

for engine_cachet_dir in "${cachet_dir}"/*; do
  engine="$(basename "${engine_cachet_dir}")"
  if [[ " ${engine_allow_list} " =~ " ${engine} " ]]; then
    for cache_kind_cachet_dir in "${engine_cachet_dir}"/*; do
      cache_kind="$(basename "${cache_kind_cachet_dir}")"
      cache_kind_sub_dir="${engine}/${cache_kind}"

      cache_kind_bpl_dir="${bpl_dir}/${cache_kind_sub_dir}"
      mkdir -p "${cache_kind_bpl_dir}"

      cache_kind_compile_logs_dir="${all_compile_logs_dir}/${cache_kind_sub_dir}"
      cache_kind_successful_compile_logs_dir="${successful_compile_logs_dir}/${cache_kind_sub_dir}"
      cache_kind_failed_compile_logs_dir="${failed_compile_logs_dir}/${cache_kind_sub_dir}"
      mkdir -p "${cache_kind_compile_logs_dir}"

      for cachet_file in "${cache_kind_cachet_dir}"/*; do
        cachet_file_name="$(basename "${cachet_file}")"

        bpl_file_name="${cachet_file_name%.cachet}.bpl"
        bpl_file="${cache_kind_bpl_dir}/${bpl_file_name}"

        compile_log_file_name="${cachet_file_name%.cachet}.log"
        compile_log_file="${cache_kind_compile_logs_dir}/${compile_log_file_name}"

        echo "${cachet_file} -> ${bpl_file}"
        
        if cargo run -p cachet-compiler -- "${cachet_file}" --bpl "${bpl_file}" 2>&1 \
            | tee "${compile_log_file}"; then
          mkdir -p "${cache_kind_successful_compile_logs_dir}"
          ln -s "${compile_log_file}" \
            "${cache_kind_successful_compile_logs_dir}/${compile_log_file_name}"

          cat "${hacks_bpl_file}" "${bpl_file}" | sponge "${bpl_file}"
        else
          mkdir -p "${cache_kind_failed_compile_logs_dir}"
          ln -s "${compile_log_file}" \
            "${cache_kind_failed_compile_logs_dir}/${compile_log_file_name}"
        fi
      done
    done
  fi
done
