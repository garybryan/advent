#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail
if [[ "${TRACE-0}" == "1" ]]; then
    set -o xtrace
fi

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
	echo "Usage: $0 [year] [day]"
	exit
fi

year="${1:-$(date +'%Y')}"
day="${2:-$(date +'%d')}"

cd "$(dirname "$0")/.."

copy_file() {
    local path_in_template="$1"
    local path=${path_in_template#template/}
    local path_year=${path//_Y_/$year}
    local path_day=${path_year//_D_/$day}

    local dir=$(dirname "$path_day")
    [[ ! -d "$dir" ]] && mkdir -p "$dir"

    cp -v "$path_in_template" "$path_day"
}

for f in $(find template/ -type f); do
    copy_file "$f"
done
