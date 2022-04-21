repo_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)"

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color
PASS="${GREEN}PASS${NC}"
FAIL="${RED}FAIL${NC}"

TMP_BPL=.test.tmp.bpl

# Frontend Tests
#
# These are just smoke tests that don't inspect the output.
# Currently there's no way to stop after type-checking so these tests will also detect
# panics during code emission.
for f in $(ls -d $repo_dir/tests/frontend/pass/*); do
    test_name="$(basename $f)...\t"
    echo -en "frontend/pass/$test_name"
    cargo run --manifest-path "$repo_dir/Cargo.toml" --quiet --bin cachet-compiler $f /dev/null /dev/null /dev/null
    if [[ $? -eq 0 ]]
    then
        echo -e "$PASS"
    else
        echo -e "$FAIL"
    fi
done


function has_bugs() {
    if [[ "$1" == *"potential bug"* ]]
    then
        return 0
    else
        return 1
    fi
}

# Verifier tests
#
# These tests run corral against the generated boogie, looking for an entrypoint called #test
for f in $(ls -d $repo_dir/tests/verifier/pass/*); do
    test_name="$(basename $f)...\t"
    echo -en "verifier/pass/$test_name"
    cargo run --manifest-path "$repo_dir/Cargo.toml" --quiet --bin cachet-compiler $f /dev/null /dev/null $TMP_BPL
    OUT=$(corral "/main:#test" $TMP_BPL 2>&1)
    if has_bugs "$OUT"
    then
        echo -e "$FAIL"
        echo $OUT
    else
        echo -e "$PASS"
    fi
    rm $TMP_BPL
done

for f in $(ls -d $repo_dir/tests/verifier/fail/*); do
    test_name="$(basename $f)...\t"
    echo -en "verifier/fail/$test_name"
    cargo run --manifest-path "$repo_dir/Cargo.toml" --quiet --bin cachet-compiler $f /dev/null /dev/null $TMP_BPL
    OUT=$(corral "/main:#test" $TMP_BPL 2>&1)
    if has_bugs "$OUT"
    then
        echo -e "$PASS"
    else
        echo -e "$FAIL"
        echo $OUT
    fi
    rm $TMP_BPL
done

INCLUDE_DIR=$(mktemp -d)
TMP_INC="$INCLUDE_DIR/test.inc"
TMP_H="$INCLUDE_DIR/test.h"
TMP_OUT="$INCLUDE_DIR/test"

for f in $(ls -d $repo_dir/tests/cpp/*.cachet); do
    test_name="$(basename $f)"
    cpp_file="${f%.cachet}.cpp"
    echo -en "cpp/$test_name..."
    cargo run --manifest-path "$repo_dir/Cargo.toml" --quiet --bin cachet-compiler $f $TMP_H $TMP_INC /dev/null
    COMP_OUT=$(clang++ -std=c++17 -I $INCLUDE_DIR -I $repo_dir/tests/cpp $cpp_file -o $TMP_OUT)
    if [[ $? -ne 0 ]]
    then
        echo -e "$FAIL"
        echo $COMP_OUT
    fi

    OUT=$($TMP_OUT)
    if [[ $? -ne 0 ]]
    then
        echo -e "$FAIL"
        echo $OUT
    else
        echo -e "$PASS"
    fi

    rm $INCLUDE_DIR/*
done