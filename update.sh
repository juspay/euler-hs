#!/usr/bin/env nix-shell
#! nix-shell -i bash -p jq -p yq
#
# Update euler-* dependencies in the nix/sources.json file
#

set -euf -o pipefail

usage() {
    >&2 echo "Usage: $0 [-f <path>] [-s] [-r <string>] [-h]"
    >&2 echo "-h: print this help"
    >&2 echo "-f: path to project folder (default is .)"
    >&2 echo "-s: use stack extra-deps format (default is 'false')"
    >&2 echo "-r: a nix set specifying refs to be changed (default is '{}')"
    >&2 echo    
    >&2 echo "Example:"
    >&2 echo "$0 -f ../euler-api-gateway -r '{euler-hs = \"staging\";}'"
    >&2 echo    
}

REFS='{}'
FOLDER=$(pwd -P)
USE_STACK='false'

SOURCES_FILE="nix/sources.json"

abs_path() {
    local path="$1"

    if [[ $path == "." ]]; then
        echo $(pwd -P)
    elif [[ -d $path ]]; then
        echo $(cd $path; pwd -P)
    else
        >&2 echo "Folder ${path} does not exist"
        exit 1
    fi
}

check_nix_set() {
    local input_expr="${1-}"
    [[ -z $input_expr ]] && >&2 echo "Empty refs passed" && exit 1

    local check_expr=$(cat <<-EXPR
{x}: if builtins.isAttrs x
then x
else throw "Not a valid nix attrset"
EXPR
    )

    >&2 echo "Overriding refs:"
    >&2 nix-instantiate --eval --strict --readonly-mode -E "${check_expr}" --arg x $input_expr || exit 1
}

[[ $# -eq 0 ]] && usage && >&2 echo "Running with defaults"

while getopts "hsf:r:" opt; do
    case $opt in
        h) usage
           exit 0
           ;;
        s) USE_STACK='true'
           ;;
        f) FOLDER="$(abs_path $OPTARG)"
           [[ ! -f "${FOLDER}/${SOURCES_FILE}" ]] && >&2 echo "No sources.json file" && exit 1
           ;;
        r) REFS=$OPTARG
           check_nix_set $REFS
           ;;
        *) usage
           exit 1
           ;;
    esac
done
shift $((OPTIND -1))

>&2 echo "Using path: ${FOLDER}"

SOURCES_PATH="${FOLDER}/${SOURCES_FILE}"

SOURCES=$(cat "${SOURCES_PATH}")

NEW_SOURCES=$(nix-instantiate --tarball-ttl 0 --eval --strict nix/update.nix --arg refs "${REFS}" --argstr rawSources "${SOURCES}" --arg forStack $USE_STACK --show-trace --json)

echo >&2

if [[ $USE_STACK == 'true' ]]; then
    echo "${NEW_SOURCES}" | yq . --yaml-output
else
    FORMATTED_SOURCE=$(echo "${NEW_SOURCES}" | jq .)
    cat <<< "${FORMATTED_SOURCE}" > "${SOURCES_PATH}"
    >&2 echo "Wrote the following to ${SOURCES_PATH}:"
    echo "${FORMATTED_SOURCE}" | jq . -C
fi
