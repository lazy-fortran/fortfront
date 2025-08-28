# fortfront Documentation

## Useful Documentation (Aligned with Reality)

These guides describe functionality that actually exists and works:

- **ERROR_HANDLING_GUIDE.md** - How to use result_t type for error handling
- **MIXED_CONSTRUCTS_GUIDE.md** - Parsing modules with implicit main programs
- **STATIC_LIBRARY_GUIDE.md** - Using fortfront as a static library
- **WINDOWS_HEAP_ISSUE.md** - Known Windows platform limitations

## Outdated Documentation (References Unimplementable Features)

**WARNING**: These documents reference CST, arenas, or advanced type systems that the team cannot implement:

- NODE_TYPE_IDENTIFICATION.md (references CST architecture)
- PARSE_DECLARATION_REFACTORING.md (mentions arena allocator)
- SEMANTIC_EXTENSIBILITY_GUIDE.md (describes CST features)
- SEMANTIC_PIPELINE_QUICK_REFERENCE.md (references CST/arena)
- TYPE_SAFETY_GUIDE.md (may reference advanced type theory)
- CHARACTER_TYPE_GUIDE.md (implementation details may be outdated)

## What You Actually Need

For basic fortfront usage:

1. Read ERROR_HANDLING_GUIDE.md to understand error management
2. Read STATIC_LIBRARY_GUIDE.md to integrate fortfront
3. Check MIXED_CONSTRUCTS_GUIDE.md for lazy-fortran features
4. Everything else is optional or outdated

## Documentation Principle

If a document describes features that don't exist in the code, ignore it.
Focus on documentation for working functionality only.