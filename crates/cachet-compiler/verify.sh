#!/bin/bash
set -euo pipefail
mkdir -p solve
pushd solve >/dev/null
../../../../corral/source/Corral/bin/Release/net5.0/corral ../instanceof.bpl /trackAllVars /recursionBound:4 || true
popd
rm -r solve
