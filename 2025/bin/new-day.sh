#!/usr/bin/env bash
set -e
cd "$(dirname "$0")"
today=$(date +%d)
lastday=$(find ../src/bin -type f -name "*.rs" | grep -v "day-$today" | sort | tail -n 1 | xargs basename | cut -d '.' -f 1 | cut -d '-' -f 2)
mkdir -p "../input/day-$today"
touch "../input/day-$today/example.txt"
touch "../input/day-$today/input.txt"
cp "../src/bin/day-$lastday.rs" "../src/bin/day-$today.rs"
if ! grep "name = day-$today" ../Cargo.toml; then
    echo ""                      >> ../Cargo.toml
    echo "[[bin]]"               >> ../Cargo.toml
    echo "name = \"day-$today\"" >> ../Cargo.toml
fi
code "../src/bin/day-$today.rs"
code "../input/day-$today/input.txt"
code "../input/day-$today/example.txt"
