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
hacks_bpl_file="${notes_dir}/hacks.bpl"

default_glob='IonIC/*/*'

function mk_parent_dir {
  local path="${1}"
  shift

  mkdir -p -- "$(dirname "${path}")"
}