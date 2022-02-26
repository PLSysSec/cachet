#!/bin/bash
set -euo pipefail

repo_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)
corral_dir="${repo_dir}/vendor/corral"

cd "${corral_dir}"
dotnet build -c Release source/Corral.sln
