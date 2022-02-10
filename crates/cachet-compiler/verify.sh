#!/bin/bash
set -euo pipefail
mkdir -p solve
pushd solve >/dev/null
corral /trackAllVars ../getprop.bpl /recursionBound:5 || true
popd
rm -r solve
