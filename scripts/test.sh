repo_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)"

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color
PASS="${GREEN}PASS${NC}"
FAIL="${RED}FAIL${NC}"

for f in $(ls -d $repo_dir/tests/frontend/pass/*); do
    test_name="$(basename $f)...\t"
    echo -en $test_name
    cargo run --manifest-path "$repo_dir/Cargo.toml" --quiet --bin cachet-compiler $f /dev/null /dev/null /dev/null
    if [[ $? -eq 0 ]]
    then
        echo -e "$PASS"
    else
        echo -e "$FAIL"
    fi
done