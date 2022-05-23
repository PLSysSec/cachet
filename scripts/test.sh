#! /usr/bin/env bash

shopt -s globstar

export REPO_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)"

export RED='\033[0;31m'
export GRAY='\033[0;90m'
export GREEN='\033[0;32m'
export NC='\033[0m' # No Color
export PASS="${GREEN}PASS${NC}"
export SKIP="${GRAY}SKIP${NC}"
export FAIL="${RED}FAIL${NC}"

export CARGO_FLAGS="--manifest-path "$REPO_DIR/Cargo.toml" --quiet --bin cachet-compiler"
export CORRAL="$REPO_DIR/vendor/corral/source/Corral/bin/Release/net5.0/corral"
export WORK_DIR=$(mktemp -d)


export DEFAULT_CPP_TEST="$REPO_DIR/tests/cpp/default.cpp"

export FILTER="$1"


function run_test() {
    export CACHET_FILE=$1
    export TEST_NAME="${CACHET_FILE#"$REPO_DIR/tests/"}"
    export TEST_DIR="$WORK_DIR/$TEST_NAME"

    mkdir -p "$TEST_DIR"

    printf "%-${MAX_LENGTH}s   " "$TEST_NAME" | tr " " "."



    ERROR=$(case $TEST_NAME in
        ("cpp/"*) build && cpp_test ;;
        ("dual/"*) build && dual_test ;;
        ("verifier/pass"*) build && verifier_test ;;
        ("verifier/fail"*) build && verifier_test; should_fail ;;
        ("frontend/pass"*) build ;;
        ("frontend/fail"*) build ; should_fail;;
        (*) echo "Unknown test "; false ;;
    esac)

    RESULT=$?

    if [[ $RESULT -eq 0 ]]; then
        echo -e "$PASS"
        echo $TEST_NAME >> $WORK_DIR/passed
    else
        echo -e "$FAIL" 
        echo -e "$ERROR"
        echo $TEST_NAME >> $WORK_DIR/failed
    fi

    return $RESULT
}

function build() {
    cargo run $CARGO_FLAGS $CACHET_FILE "$TEST_DIR/test.h" "$TEST_DIR/test.inc" "$TEST_DIR/test.bpl" 2>&1
    return $?
}

function cpp_test() {
    CPP_FILE="${CACHET_FILE%.cachet}.cpp"
    if [[ ! -f $CPP_FILE ]]; then
        CPP_FILE="$DEFAULT_CPP_TEST"
    fi


    clang++ -std=c++17 -I $TEST_DIR -I $REPO_DIR/tests/cpp $CPP_FILE -o "$TEST_DIR/out"
    if [[ $? -ne 0 ]]; then
        return 1
    fi

    $TEST_DIR/out
    if [[ $? -ne 0 ]]; then
        echo -e "Compiled program returned a non-zero status"
        return 1
    fi

    return 0
}

function verifier_test() {
    OUT=$($CORRAL "/main:#test" /trackAllVars /recursionBound:4 $TEST_DIR/test.bpl)
    EXIT_CODE=$?

    echo -e "$OUT"

    if [[ $EXIT_CODE -eq 0 && "$OUT" != *"potential bug"* ]]; then
        return 0
    fi

    return 1
}

function dual_test() {
    cpp_test
    CPP=$?

    verifier_test
    VER=$?

    if [[ $CPP -ne 0 ]] || [[ $VER -ne 0 ]]; then
        return 1
    else
        return 0
    fi
}

function should_fail() {
    if [[ $? -eq 0 ]]; then
        echo -e "${RED}Test passed when it should have failed${NC}"
        return 1
    fi

    return 0
}

export -f run_test
export -f cpp_test
export -f verifier_test
export -f dual_test
export -f should_fail
export -f build


echo -e -n "Building..."
cargo build $CARGO_FLAGS 
if [[ $? -ne 0 ]]; then
    exit 1;
fi
echo -e "DONE\n"

MAX_LENGTH=0
ALL_TESTS=$(ls -d $REPO_DIR/tests/**/*.cachet)
for f in $ALL_TESTS; do
    TEST_NAME="${f#"$REPO_DIR/tests/"}"
    NAME_LENGTH=${#TEST_NAME}
    if [[ $NAME_LENGTH -gt $MAX_LENGTH ]]; then
        MAX_LENGTH=$NAME_LENGTH
    fi
done


export MAX_LENGTH=$MAX_LENGTH

touch $WORK_DIR/passed $WORK_DIR/failed $WORK_DIR/skipped

for f in $ALL_TESTS; do
    if [[ -z "$FILTER" ]] || [[ $f == *"$FILTER"* ]]; then
        echo "$f"
    else
        echo "$f" >> "$WORK_DIR/skipped"
    fi
done | parallel run_test
PARALLEL_PROC=$!

function ctrl_c() {
    echo "INTERRUPTED!"
    kill $PARALLEL_PROC
}

trap ctrl_c INT

wait $PARALLEL_PROC

PASSED_COUNT=$(wc -l  < $WORK_DIR/passed)
echo -e "${GREEN}$PASSED_COUNT${NC} tests passed"

SKIPPED_COUNT=$(wc -l  < $WORK_DIR/skipped)
echo -e "${GRAY}$SKIPPED_COUNT${NC} tests skipped"

FAILED_COUNT=$(wc -l < $WORK_DIR/failed)
if [[ $FAILED_COUNT -ne 0 ]]; then
    echo -e "${RED}$FAILED_COUNT${NC} tests failed:"
    for failed_test in $(cat $WORK_DIR/failed); do
        echo -e "\t$failed_test"
    done
fi

rm -r $WORK_DIR
exit $FAILED_COUNT