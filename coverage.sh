#!/bin/bash
set -e

echo "Cleaning build directory..."
dune clean

echo "Creating coverage directory..."
mkdir -p _coverage

echo "Running tests with coverage instrumentation..."
export BISECT_FILE=$(pwd)/_coverage/bisect
export BISECT_ENABLE=yes
dune runtest --instrument-with bisect_ppx --force

echo ""
echo "Coverage Summary:"
echo "================"
bisect-ppx-report summary --coverage-path _coverage

echo ""
echo "Generating HTML report..."
bisect-ppx-report html --coverage-path _coverage

echo ""
echo "Coverage report generated at: _coverage/index.html"
echo "Open it with: xdg-open _coverage/index.html"
