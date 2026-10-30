#!/usr/bin/env bash
# Build a CA bundle that lets curl verify www.inkar.de.
#
# The INKAR server does not send its intermediate certificate, so curl fails
# with "unable to get local issuer certificate" although browsers work. The
# intermediate chains to "GoDaddy TLS Root CA - R1", which the Mozilla store
# used by curl does not contain, so it cannot be fetched and verified at run
# time. Instead the intermediate is pinned in .github/certs/. This script
# checks that the current server certificate is signed by it and appends it to
# the system bundle (curl accepts a trusted intermediate as anchor).
#
# If INKAR switches to another intermediate, the check fails on purpose: the
# health check then reports it so the pinned file can be reviewed and updated.
#
# Usage: inkar-ca-bundle.sh <output file>

set -euo pipefail

out="${1:?usage: inkar-ca-bundle.sh <output file>}"
host="www.inkar.de"
pinned="$(cd "$(dirname "$0")/.." && pwd)/certs/godaddy-tls-intermediate-dv-r1v1.pem"

roots=""
for candidate in /etc/ssl/certs/ca-certificates.crt /etc/ssl/cert.pem /etc/pki/tls/certs/ca-bundle.crt; do
  if [ -s "$candidate" ]; then roots="$candidate"; break; fi
done
if [ -z "$roots" ]; then
  echo "No system CA bundle found." >&2
  exit 1
fi

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

echo | openssl s_client -connect "$host:443" -servername "$host" 2>/dev/null \
  | openssl x509 > "$work/server.pem"

if ! openssl verify -partial_chain -CAfile "$pinned" "$work/server.pem" > /dev/null 2>&1; then
  echo "The certificate of $host is not signed by the pinned intermediate." >&2
  openssl x509 -in "$work/server.pem" -noout -issuer >&2
  echo "Review the new issuer and update $pinned." >&2
  exit 1
fi

openssl x509 -in "$pinned" > "$work/intermediate.pem"
cat "$roots" "$work/intermediate.pem" > "$out"
echo "$out"
