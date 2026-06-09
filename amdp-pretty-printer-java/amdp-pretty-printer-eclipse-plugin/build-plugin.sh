#!/usr/bin/env bash
# build-plugin.sh
# Builds the AMDP Pretty Printer Eclipse plugin.
# Generates the machine-specific .target file from the template in src/target/,
# substituting the local Eclipse installation path, then runs "mvn package".
#
# Usage:
#   ./build-plugin.sh                                   # uses /opt/eclipse as default
#   ./build-plugin.sh --eclipse-home /path/to/eclipse

set -euo pipefail

ECLIPSE_HOME="/opt/eclipse"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --eclipse-home)
            ECLIPSE_HOME="$2"
            shift 2
            ;;
        --eclipse-home=*)
            ECLIPSE_HOME="${1#*=}"
            shift
            ;;
        *)
            echo "Unknown argument: $1" >&2
            exit 1
            ;;
    esac
done

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEMPLATE="$SCRIPT_DIR/src/target/amdp-pretty-printer-eclipse-plugin.target"
OUTPUT="$SCRIPT_DIR/amdp-pretty-printer-eclipse-plugin.target"

echo "Generating target platform file for Eclipse at: $ECLIPSE_HOME"
sed "s|\${eclipse.install.dir}|$ECLIPSE_HOME|g" "$TEMPLATE" > "$OUTPUT"
echo "Written: $OUTPUT"
echo ""
echo "Running: mvn package"
mvn package
