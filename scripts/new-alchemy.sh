#!/usr/bin/env bash
# Script to bootstrap a new alchemy project

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Usage info
usage() {
    echo -e "${BLUE}Usage:${NC} $0 <alchemy-name> <description>"
    echo ""
    echo "Example:"
    echo "  $0 alchemy-regex \"Regular expression engine\""
    exit 1
}

# Check arguments
if [ $# -lt 2 ]; then
    usage
fi

ALCHEMY_NAME="$1"
DESCRIPTION="$2"

# Validate name format
if [[ ! "$ALCHEMY_NAME" =~ ^alchemy-[a-z0-9-]+$ ]]; then
    echo -e "${RED}Error:${NC} Name must match format 'alchemy-something'"
    echo "  Use lowercase letters, numbers, and hyphens only"
    exit 1
fi

# Check if directory exists
if [ -d "$ALCHEMY_NAME" ]; then
    echo -e "${RED}Error:${NC} Directory $ALCHEMY_NAME already exists"
    exit 1
fi

echo -e "${BLUE}🔮 Creating new alchemy: ${GREEN}$ALCHEMY_NAME${NC}"

# Create directory structure
mkdir -p "$ALCHEMY_NAME"/{src,tests,bin}

# Create dune-project
cat > "$ALCHEMY_NAME/dune-project" <<EOF
(lang dune 3.7)
(name ${ALCHEMY_NAME//-/_})

(generate_opam_files true)

(source (github jorgebaptista/syntax-alchemy))
(license MIT)
(authors "Jorge Baptista")

(package
 (name ${ALCHEMY_NAME//-/_})
 (synopsis "$DESCRIPTION")
 (description "$DESCRIPTION")
 (depends
  (ocaml (>= 4.14))
  dune
  (alcotest :with-test)))
EOF

# Create lib dune file
cat > "$ALCHEMY_NAME/src/dune" <<EOF
(library
 (name ${ALCHEMY_NAME//-/_})
 (public_name ${ALCHEMY_NAME//-/_}))
EOF

# Create bin dune file
cat > "$ALCHEMY_NAME/bin/dune" <<EOF
(executable
 (name main)
 (public_name ${ALCHEMY_NAME//-/_})
 (libraries ${ALCHEMY_NAME//-/_}))
EOF

# Create test dune file
cat > "$ALCHEMY_NAME/tests/dune" <<EOF
(test
 (name test_${ALCHEMY_NAME//-/_})
 (libraries ${ALCHEMY_NAME//-/_} alcotest))
EOF

# Create sample source file
cat > "$ALCHEMY_NAME/src/${ALCHEMY_NAME//-/_}.ml" <<EOF
(** $DESCRIPTION *)

let hello () = 
  "Hello from $ALCHEMY_NAME!"

let run () =
  print_endline (hello ())
EOF

# Create sample interface file
cat > "$ALCHEMY_NAME/src/${ALCHEMY_NAME//-/_}.mli" <<EOF
(** $DESCRIPTION *)

val hello : unit -> string
(** Returns a greeting message *)

val run : unit -> unit
(** Main entry point *)
EOF

# Create main executable
cat > "$ALCHEMY_NAME/bin/main.ml" <<EOF
let () = 
  ${ALCHEMY_NAME//-/_^}.run ()
EOF

# Create sample test
cat > "$ALCHEMY_NAME/tests/test_${ALCHEMY_NAME//-/_}.ml" <<EOF
open ${ALCHEMY_NAME//-/_^}

let test_hello () =
  Alcotest.(check string) "hello returns greeting" 
    "Hello from $ALCHEMY_NAME!" (hello ())

let () =
  Alcotest.run "$ALCHEMY_NAME" [
    "basic", [
      Alcotest.test_case "hello" \`Quick test_hello;
    ];
  ]
EOF

# Create README
cat > "$ALCHEMY_NAME/README.md" <<EOF
# $ALCHEMY_NAME

**$DESCRIPTION**

## Overview

TODO: Describe what this alchemy does, what problem it solves, and the approach taken.

## Building

\`\`\`bash
dune build
\`\`\`

## Running

\`\`\`bash
dune exec $ALCHEMY_NAME
\`\`\`

## Testing

\`\`\`bash
dune test
\`\`\`

## Architecture

TODO: Describe the main components and how they interact.

## Status

🔮 In Development

## References

TODO: Add links to relevant papers, documentation, or resources.
EOF

# Create .ocamlformat
cat > "$ALCHEMY_NAME/.ocamlformat" <<EOF
version = 0.26.0
profile = default
EOF

echo -e "${GREEN}✅ Created $ALCHEMY_NAME${NC}"
echo ""
echo -e "${YELLOW}Next steps:${NC}"
echo "  1. cd $ALCHEMY_NAME"
echo "  2. Edit src/${ALCHEMY_NAME//-/_}.ml with your implementation"
echo "  3. Update README.md with details"
echo "  4. Add entry to docs/LABS.md"
echo "  5. Run: dune build && dune test"
echo ""
echo -e "${BLUE}Happy alchemizing! ⚗️${NC}"
