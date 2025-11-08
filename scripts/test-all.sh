#!/usr/bin/env bash
# Run tests for all alchemy projects

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

FAILED=0
PASSED=0
SKIPPED=0

echo -e "${BLUE}🧪 Running tests for all alchemies...${NC}"
echo ""

# Find all alchemy projects
for dir in alchemy-*/; do
    if [ ! -d "$dir" ]; then
        continue
    fi
    
    PROJECT_NAME="${dir%/}"
    
    # Check if it has a dune-project
    if [ ! -f "$dir/dune-project" ]; then
        echo -e "${YELLOW}⊘ Skipping $PROJECT_NAME (no dune-project)${NC}"
        ((SKIPPED++))
        continue
    fi
    
    echo -e "${BLUE}Testing $PROJECT_NAME...${NC}"
    
    # Build and test
    if (cd "$dir" && dune build && dune test) 2>&1 | sed "s/^/  /"; then
        echo -e "${GREEN}✅ $PROJECT_NAME passed${NC}"
        ((PASSED++))
    else
        echo -e "${RED}❌ $PROJECT_NAME failed${NC}"
        ((FAILED++))
    fi
    
    echo ""
done

# Summary
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}Summary:${NC}"
echo -e "  ${GREEN}Passed:${NC}  $PASSED"
echo -e "  ${RED}Failed:${NC}  $FAILED"
echo -e "  ${YELLOW}Skipped:${NC} $SKIPPED"

if [ $FAILED -gt 0 ]; then
    echo -e "${RED}Some tests failed!${NC}"
    exit 1
else
    echo -e "${GREEN}All tests passed! ✨${NC}"
fi
