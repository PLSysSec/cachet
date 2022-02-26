#!/bin/bash
set -euo pipefail

repo_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)
boogie_dir="${repo_dir}/vendor/boogie"

cd "${boogie_dir}"
dotnet build -c Release Source/Boogie.sln
