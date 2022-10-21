#!/bin/bash
set -euo pipefail
shopt -s nullglob

scripts_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
source "${scripts_dir}/common.sh"

grep -REoh 'undefined op `([^`]+)`' "${failed_compile_logs_dir}" \
  | sed 's,undefined op ,,' \
  | sed 's,`,,g' \
  | sort \
  | uniq -c \
  | sort -hr
