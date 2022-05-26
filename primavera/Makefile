mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
SRC_DIR := ${CURDIR}
BUILD_DIR := ${CURDIR}/.build
$(shell mkdir -p $(BUILD_DIR))
SHELL := /bin/bash

CORRAL := $(SRC_DIR)/../vendor/corral/source/Corral/bin/Release/net5.0/corral
IMPLS := $(notdir $(basename $(wildcard impls/*)))
OPS := $(shell echo "include(\`macros.m4')foreach(\`op', unops, \`op')" | m4 -I ${SRC_DIR})
VERIF_RESULTS := $(foreach impl, ${IMPLS}, $(foreach op, ${OPS}, results/${impl}/${op}))

CARGO_FLAGS=--manifest-path '$(SRC_DIR)/../Cargo.toml' --quiet --bin cachet-compiler

$(BUILD_DIR)/range_spec.cachet: $(SRC_DIR)/range_spec.cachet.m4
	m4 $< > $@

$(BUILD_DIR)/spec_check.cpp: js_spec/spec_check.m4.cpp
	m4 $< > $@


$(BUILD_DIR)/verif_%.bpl:  $(BUILD_DIR)/verif_%.cachet
	cargo run $(CARGO_FLAGS) $< /dev/null /dev/null $@

$(BUILD_DIR)/verif_%.cachet:  $(SRC_DIR)/impls/%.cachet $(BUILD_DIR)/range_spec.cachet $(SRC_DIR)/js_spec/js_spec.cachet
	echo '$(foreach dep, $^, import "$(dep)")' > $@


# All verif targets are phony so that we can re-run them easily
define VERIF_TARGET
.PHONY: $(SRC_DIR)/results/$(1)/%.out

$(SRC_DIR)/results/$(1)/%.out: $(BUILD_DIR)/verif_$(1).bpl
	mkdir -p $$(dir $$@)
	$(CORRAL) "/main:#$$(notdir $$(basename $$@))_spec" $$< | tee $$@

endef
$(foreach impl, $(IMPLS), $(eval $(call VERIF_TARGET,$(impl))))

.PHONY: fuzz_js_spec
fuzz_js_spec: .spec.out/spec_check.cpp
	mkdir -p .spec.out
	cargo run $(CARGO_FLAGS) js_spec/js_spec.cachet .spec.out/js_spec.h .spec.out/js_spec.inc /dev/null
	clang++  -std=c++17 -fsanitize=fuzzer   -I .spec.out .spec.out/spec_check.cpp -o .spec.out/spec_check

	.spec.out/spec_check
