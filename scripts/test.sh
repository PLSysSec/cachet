shopt -s globstar

repo_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)"

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color
PASS="${GREEN}PASS${NC}"
FAIL="${RED}FAIL${NC}"

INCLUDE_DIR=$(mktemp -d)
TMP_INC="$INCLUDE_DIR/test.inc"
TMP_H="$INCLUDE_DIR/test.h"
TMP_OUT="$INCLUDE_DIR/test"
TMP_BPL="$INCLUDE_DIR/test.bpl"

FILTER=$1
function matches() {
    if [[ $1 == $FILTER ]]
    then
        return 1
    else
        return 0
    fi
}

function build() {
    cachet_file=$1
    cargo run --manifest-path "$repo_dir/Cargo.toml" --quiet --bin cachet-compiler $cachet_file $TMP_H $TMP_INC $TMP_BPL 2>&1
    return $?
}

function cpp_test() {
    cachet_file=$1
    build $cachet_file
    if [ $? -ne 0 ]; then
        return 1
    fi

    cpp_file="${cachet_file%.cachet}.cpp"

    clang++ -std=c++17 -I $INCLUDE_DIR -I $repo_dir/tests/cpp $cpp_file -o $TMP_OUT 2>&1
    if [[ $? -ne 0 ]]; then
        return 1
    fi

    $TMP_OUT
    if [[ $? -ne 0 ]]; then
        echo "Compiled program returned a non-zero status"
        return 1
    fi

    return 0
}

function verifier_test() {
    cachet_file=$1
    build $cachet_file
    if [ $? -ne 0 ]; then
        return 1
    fi

    OUT=$(corral "/main:#test" $TMP_BPL 2>&1)
    echo -e "$OUT"

    if [[ "$OUT" != *"potential bug"* ]]; then
        return 0
    fi

    return 1
}

function should_fail() {
    if [[ $? -eq 0 ]]; then
        echo -e "${RED}Test passed when it should have failed${NC}"
        return 1
    fi

    return 0
}

PASSED_COUNT=0
FAILED_TESTS=()
for f in $(ls -d $repo_dir/tests/**/*.cachet); do
    test_name="${f#"$repo_dir/tests/"}"
    echo -en "$test_name...\t"
    ERROR=$(case $test_name in
        ("cpp/"*) cpp_test "$f" ;;
        ("verifier/pass"*) verifier_test "$f" 0;;
        ("verifier/fail"*) verifier_test "$f"; should_fail ;;
        ("frontend/pass"*) build "$f";;
        ("frontend/fail"*) build "$f"; should_fail;;
        (*) echo "Unknown test $f"; false ;;
    esac)

    if [[ $? -eq 0 ]]; then
        PASSED_COUNT=$((PASSED_COUNT+1))
        echo -e "$PASS"
    else
        FAILED_TESTS+=($test_name)
        echo -e "$FAIL" 
        echo -e "$ERROR"
    fi
done

FAILED_COUNT=${#FAILED_TESTS[@]}
echo -e "${GREEN}$PASSED_COUNT${NC} tests passed"
if [[ $FAILED_COUNT  -eq 0 ]]; then
    exit 0
fi

echo -e "${RED}$FAILED_COUNT${NC} tests failed:"
for failed_test in "${FAILED_TESTS[@]}"; do
    echo -e "\t$failed_test"
done
exit 1