#!/bin/bash

eval_dir="$(cd -- "${scripts_dir}" && cd .. && pwd)"
repo_dir="$(cd -- "${eval_dir}" && cd .. && pwd)"

data_dir="${eval_dir}/data"
spew_dir="${data_dir}/spew"
stubs_dir="${spew_dir}/stubs"
cachet_dir="${data_dir}/cachet"
bpl_dir="${data_dir}/bpl"

compile_logs_dir="${data_dir}/compile-logs"
all_compile_logs_dir="${compile_logs_dir}/all"
successful_compile_logs_dir="${compile_logs_dir}/successes"
failed_compile_logs_dir="${compile_logs_dir}/failures"

verify_logs_dir="${data_dir}/verify-logs"
all_verify_logs_dir="${verify_logs_dir}/all"
successful_verify_logs_dir="${verify_logs_dir}/successes"
failed_verify_logs_dir="${verify_logs_dir}/failures"

bin_dir="${repo_dir}/target/debug"
corral_exe="${repo_dir}/vendor/corral/source/Corral/bin/Release/net6.0/corral"

notes_dir="${repo_dir}/notes"
repro_notes_dir="${repo_dir}/notes-repro"
test_stubs_dir="${notes_dir}/stubs"
support_bpl_file="${notes_dir}/support.bpl"
cacheir_cachet_file="${notes_dir}/cacheir.cachet"
repro_cacheir_cachet_file="${repro_notes_dir}/cacheir.cachet"
masm_cachet_file="${notes_dir}/masm.cachet"
repro_masm_cachet_file="${repro_notes_dir}/masm.cachet"
js_cachet_file="${notes_dir}/js.cachet"

default_glob='IonIC/*/*'

function mk_parent_dir {
  local path="${1}"
  shift

  mkdir -p -- "$(dirname "${path}")"
}
