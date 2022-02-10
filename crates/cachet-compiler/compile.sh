#!/bin/bash
set -euo pipefail
cargo run -- ../../notes/getprop.cachet getprop.h getprop.inc getprop.bpl
cat ../../notes/prelude.bpl getprop.bpl | sponge getprop.bpl
