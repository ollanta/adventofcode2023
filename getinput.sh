#!/bin/sh
set -euo pipefail

# Requires a .env-file with exported year and session attributes
source $(dirname "$0")/.env

curl "https://adventofcode.com/$YEAR/day/$1/input" -H "Cookie: session=$SESSION"
