#!/bin/sh
set -euo pipefail

# Requires a .env-file with exported year, session, and leaderboard id attributes
source $(dirname "$0")/.env

curl "https://adventofcode.com/$YEAR/leaderboard/private/view/$LEADERBOARD.json" -H "Cookie: session=$SESSION"
