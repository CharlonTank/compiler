#!/bin/bash

# Colors
GREEN="[0;32m"
BLUE="[0;34m"
YELLOW="[1;33m"
CYAN="[0;36m"
RED="[0;31m"
NC="[0m"

show_banner() {
    clear
    echo -e "${GREEN}"
    echo "╔═══════════════════════════════════════════╗"
    echo "║         Lamdera Debug Build Watcher       ║"
    echo "╚═══════════════════════════════════════════╝"
    echo -e "${NC}"
}

rebuild_lamdera() {
    timestamp=$(date +%s)
    echo -e "${CYAN}Updating timestamp in Live.hs to: ${YELLOW}$timestamp${NC}"
    lines=$(wc -l < extra/Lamdera/CLI/Live.hs)
    sed -i "" "$lines d" extra/Lamdera/CLI/Live.hs
    echo "x = $timestamp" >> extra/Lamdera/CLI/Live.hs
    echo -e "${BLUE}Building Lamdera...${NC}"
    if LDEBUG=1 cabal build lamdera; then
        if cp $(find dist-newstyle -type f -name lamdera) ~/.cabal/bin/new-lamdera; then
            echo -e "${GREEN}Successfully installed${NC}"
            return 0
        fi
    fi
    echo -e "${RED}Build failed${NC}"
    return 1
}

cleanup() {
    echo -e "
${BLUE}Stopping watcher...${NC}"
    exit 0
}

trap cleanup SIGINT SIGTERM EXIT

show_banner
echo -e "${BLUE}Watching for changes in extra/LocalDev/LocalDev.elm${NC}"
echo -e "${CYAN}Initial build starting...${NC}"
rebuild_lamdera

fswatch -o extra/LocalDev/LocalDev.elm | while read file; do
    show_banner
    echo -e "${YELLOW}File changed: ${CYAN}$file${NC}"
    rebuild_lamdera
done