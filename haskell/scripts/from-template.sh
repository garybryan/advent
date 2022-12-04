#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail
if [[ "${TRACE-0}" == "1" ]]; then
    set -o xtrace
fi

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
    echo "Usage: $0 ([year] [day] | [tomorrow])"
	exit
fi

if [[ "${1-}" =~ ^-*t(omorrow)?$ ]]; then
    # AoC runs until 25 December, so the year for "tomorrow" is always the current ;)
    year="$(date +'%Y')"
    day="$(date --date='tomorrow' +'%d')"
else
    year="${1:-$(date +'%Y')}"
    day="${2:-$(date +'%d')}"
fi

# Attempt to work with GNU or BSD/MacOS sed
case $(sed --help 2>&1) in
  *GNU*) sed_i () { sed -i "$@"; };;
  *) sed_i () { sed -i '' "$@"; };;
esac

cd "$(dirname "$0")/.."

copy_file() {
    local path_in_template="$1"
    local path=${path_in_template#template/}
    local path_year=${path//_Y_/$year}
    local path_day=${path_year//_D_/$day}

    local dir=$(dirname "$path_day")
    [[ ! -d "$dir" ]] && mkdir -p "$dir"

    cp -v "$path_in_template" "$path_day"

    sed_i "s/_Y_/$year/g" $path_day
    sed_i "s/_D_/$day/g" $path_day
}

for f in $(find template/ -type f); do
    copy_file "$f"
done
