#!/usr/bin/env bash
# mksession: given wildcard like "7/gateways/games/go" or "1/gateways/games/go" and optional search arg
# outputs a Gopher menu line:
#   - type 7 -> /gateways/games/go/<session-id>   (search item)
#   - type 1 -> /gateways/games/go/<session-id>   (directory item)
set -euo pipefail

WILDCARD="${1:?usage: mksession <type>/<base> [search]}"
SEARCH_ARG="${2:-}"

TYPE="${WILDCARD%%/*}"
BASE="${WILDCARD#*/}"

# Allow only types 7 (search) and 1 (directory)
if [[ "$TYPE" != "7" && "$TYPE" != "1" ]]; then
  echo -e "iOnly types 7 and 1 supported\tfake\tsomeodd.zip\t70"
  exit 0
fi

# Session ID: use basename of provided search arg (sanitized) or random fallback
if [[ -n "$SEARCH_ARG" ]]; then
  ID="$(basename -- "$SEARCH_ARG")"
else
  ID="$(openssl rand -base64 12 2>/dev/null | tr -dc 'A-Za-z0-9' | head -c 12)"
  : "${ID:=sess$RANDOM}"   # fallback if openssl is missing
fi

SELECTOR="/${BASE%/}/$ID"

# Defaults (adjust to your environment)
HOST="someodd.zip"
PORT="70"

# Info line
printf "iNew session → %s\tfake\t%s\t%s\r\n" "$SELECTOR" "$HOST" "$PORT"

# Menu item based on requested type
if [[ "$TYPE" = "7" ]]; then
  # Search item
  printf "7Start session\t%s\t%s\t%s\r\n" "$SELECTOR" "$HOST" "$PORT"
else
  # Directory item
  printf "1Open session\t%s\t%s\t%s\r\n" "$SELECTOR" "$HOST" "$PORT"
fi

