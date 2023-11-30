#!/bin/sh
set -euo pipefail


jq "[.members[] | {
  name: .name,
  stars: .completion_day_level[\"$2\"] | objects | map_values(.get_star_ts | todate)
} | { name: .name, one: .stars[\"1\"], two: .stars[\"2\"] }
] | {
  one: sort_by(.one) | [ .[] | { name: .name, time: .one }][:5],
  two: map(select(.two != null)) | sort_by(.two) | [ .[] | { name: .name, time: .two }][:5]
}
" $1
