# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Development Commands

```bash
# Build the compiler
stack build

# Install to local bin (~/.local/bin/)
stack install

# Run all tests
stack ghci
Test.all

# Run specific test suites in ghci
Test.Lamdera.suite
Test.Wire.suite
Test.TypeHashes.suite
Test.Check.suite

# Development workflow in ghci (requires ~/.ghci setup)
:rr  # Recompile and run Test.target
:r   # Just recompile without running

# Format code
lamdera format src/    # Format directory
lamdera format --validate src/  # Check formatting without changes
```

## Code Architecture

The Lamdera compiler extends the Elm compiler using a minimal-invasion approach. All Lamdera-specific code is in the `/extra` directory, with integration points marked by `@LAMDERA` comments or `import.*Lamdera` statements.

### Key Directories

- **compiler/src/** - Core Elm compiler (AST, parsing, type checking, codegen)
- **extra/** - All Lamdera extensions (Wire protocol, Evergreen, Live server)
- **terminal/src/Main.hs** - CLI entry point and command routing
- **test/** - Test suite using vendored EasyTest framework

### Integration Points

Search for `@LAMDERA` or `import.*Lamdera` to find where Lamdera code integrates with the Elm compiler. The integration uses `&` and `$` operators to add lines without modifying existing ones, minimizing merge conflicts.

### Major Components

1. **Wire Protocol** (`extra/Lamdera/Wire3/`) - Type-safe client-server communication
2. **Evergreen Migrations** (`extra/Lamdera/Evergreen/`) - Automatic data migration generation
3. **Live Development** (`extra/Lamdera/Live.hs`) - WebSocket-based hot reloading
4. **Format Command** (`extra/Lamdera/CLI/Format.hs`) - Integrated elm-format

## Development Setup Requirements

```bash
# macOS setup
brew install gcc pkg-config icdiff
git submodule init && git submodule update
stack install hindent

# Configure ~/.ghci for development
cat >> ~/.ghci << 'EOF'
:set -fbyte-code
:set -fobject-code
:def rr const $ return $ unlines [":r","Test.target"]
:set prompt "\ESC[34mλ: \ESC[m"
EOF
```

## Testing

- Test entry point: `test/Test.hs`
- Change `target = Test.all` to run specific tests
- Test scenarios in `test/scenario-*/` directories
- Use `LDEBUG=1` environment variable for development features

## Debugging

For `Map.!: given key is not an element in the map` errors:
1. Run `./addSanity.sh` to add debug helpers
2. Replace `Map.!` with `Sanity.debugFind` to see the actual location
3. Run `./removeSanity.sh` when done

## Important Conventions

- Never add `.cursorrules` to the repository
- Use `LDEBUG=1` for development-only features
- When modifying `live.js`, rebuild with esbuild and force recompile the Haskell module
- Follow the minimal-invasion philosophy when integrating with core Elm code
- Add `@LAMDERA` comments to mark integration points
- **When modifying LocalDev.elm or any Elm code**: Always test compilation before rebuilding the Haskell compiler with `stack build`

## Testing LocalDev.elm Compilation

When modifying `extra/LocalDev/LocalDev.elm`, you MUST test Elm compilation to catch errors early:

### Method 1: Test with Running Project (Recommended)
If you have a Lamdera project running (e.g., `lamdera live` on port 8000):
```bash
# After modifying LocalDev.elm:
stack build && stack install

# IMPORTANT: Restart lamdera live to pick up the new LocalDev.elm
# The LocalDev.elm file is cached in elm-stuff/lamdera/LocalDev.elm
# Kill the lamdera live process and restart it

# Then check for compilation errors
curl localhost:8000 | head -20

# Look for "compile-errors" in the response
# If successful, you'll see HTML page
# If errors, you'll see detailed Elm error messages
```

### Method 2: Test in Scenario Directory
```bash
cd test/scenario-empty-lamdera-init
lamdera make src/Frontend.elm src/Backend.elm
# Should show "Success! Compiled X modules"
```

### Method 3: Quick Syntax Check
```bash
# Check basic Elm syntax (won't catch missing imports)
cd extra/LocalDev
lamdera make LocalDev.elm --output=/dev/null 2>&1 | head -10
# Note: This will show import errors but validates syntax
```

### Method 4: Integration Test
```bash
# Create a minimal test project
lamdera init test-localdev
cd test-localdev
lamdera live  # This will compile LocalDev.elm in project context
```

### Common Import Issues to Watch For

When modifying LocalDev.elm, be careful with imports:
- ✅ **Always available**: `Html`, `Html.Attributes`, `Html.Events`, `Json.Decode`, `Json.Encode`, `Dict`, `List`, `Maybe`, `String`
- ✅ **Lamdera modules**: `Lamdera`, `Lamdera.Debug`, `Lamdera.Json`, `Lamdera.Wire3`, `Types`
- ❌ **Not always available**: `Svg`, `Svg.Attributes`, `Browser.Dom`, third-party packages
- ❌ **Never available**: Other `LocalDev.*` modules (they don't get copied to projects)

### Debugging Compilation Errors

1. **Read the full error message** - Elm gives detailed hints
2. **Check import statements** - Most errors are missing dependencies
3. **Test incrementally** - Comment out new features if needed
4. **Use basic HTML instead of SVG** - SVG requires elm/svg dependency that projects may not have