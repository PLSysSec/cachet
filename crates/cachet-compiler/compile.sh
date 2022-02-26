#!/bin/bash
set -euo pipefail
cargo run -- ../../notes/instanceof.cachet instanceof.h instanceof.inc instanceof.bpl
cat ../../notes/prelude.bpl instanceof.bpl | sponge instanceof.bpl
