# Fortfront Token & Declaration Memory Architecture

## 1. Motivation

Recent profiling on our large regression suite (perf, Massif, Callgrind with 1–5 s sampling windows) shows two dominant costs:

- **Token churn:** `token_assign`, `token_finalize`, `parser_peek`, and `string_utils::to_lower` collectively consume ~40–45 % of all instructions before the parser ever reaches semantic or standardizer phases. Every assignment copies `token%text` (including trivia) and lowercases it again.
- **Declaration tracking blow‑ups:** `standardizer_declarations_collection::add_or_update_alloc_var` continually reallocates a growing array and memcpy’s the entire list every time we encounter another allocatable. For class/allocation heavy programs this drives `_gfortrani_xmallocarray` into the guard path and eventually crashes.

The result is that complex `select rank`/coarray/class(*) tests either timeout (parser dominated) or exhaust allocator safety limits (standardizer). To make the system scalable we need to modernize the token storage and the declaration collector.

## 2. Goals

1. **Shared immutable token payloads** – tokens become cheap handles; copying a token never copies its text or trivia buffers.
2. **Cached normalized forms** – lowercasing happens once per unique token payload; subsequent comparisons are O(1).
3. **Cheap parser snapshots** – parser state assignment aliases shared storage rather than deep copying arrays.
4. **Hash-backed declaration tracking** – allocatable/class(*) declarations use a hash table + chunked storage instead of repeated `xmallocarray` growth.
5. **Pluggable interfaces** – new components expose clean APIs so future optimizations (e.g., arena pools, off-thread tokenization) require minimal churn.

## 3. Token Storage Architecture

### 3.1 Token Text Pool

Introduce a new module `token_text_pool_mod` responsible for owning all token payloads:

```fortran
module token_text_pool_mod
    type :: token_text_handle_t
        integer :: id = 0
        character(len=:), allocatable :: original
        character(len=:), allocatable :: lowered
        integer :: refcount = 0
    end type

    interface
        module subroutine pool_init(pool)
        module function pool_intern(pool, text) result(handle)
        module subroutine pool_release(pool, handle)
        module function pool_get_original(pool, handle) result(text)
        module function pool_get_lower(pool, handle) result(text)
    end interface
end module
```

Key ideas:
- Token text is interned in the pool; identical strings share one entry (case sensitive lookup).
- Lowercased form is cached lazily inside the handle (`lowered` computed on first request).
- Reference counts guarantee deterministic lifetime; parser/token destructors simply release handles instead of deallocating raw character arrays.

### 3.2 Token Struct Changes

`token_t` stores only a handle plus trivia handles:

```fortran
type token_t
    integer :: kind
    integer :: line
    integer :: column
    type(token_text_handle_t) :: text_handle
    type(token_text_handle_t), allocatable :: leading_trivia(:)
    type(token_text_handle_t), allocatable :: trailing_trivia(:)
end type
```

Operations:
- `token_assign` increments reference counts instead of copying text buffers.
- `token_finalize` just releases the handles back to the pool.
- `token_text(parser, token)` retrieves the original string via `pool_get_original` and returns a pointer or allocatable (callers rarely need full copies now).

### 3.3 Lowercasing API

All code that previously called `to_lower(token%text)` must switch to:

```fortran
call token_text_cache_lower(pool, token%text_handle, cached_ptr)
```

This returns the cached lowercase string (computing it once). String comparisons (`parser_control_flow_router`, `parser_dispatcher`, etc.) use the cached pointer.

### 3.4 Parser State Interface

`parser_state_t` no longer owns `type(token_t)` arrays directly. Instead it contains:

```fortran
type(parser_state_t)
    type(token_view_t) :: view
    integer :: current_index
end type
```

`token_view_t` encapsulates:
- Pointer to the canonical token array (the lexer still produces a plain allocatable array, but only the view owns it).
- Start/end indices to support slices without copying.

Assignment becomes trivial: copy the view struct (by value) and increment a single reference count for the underlying token array.

### 3.5 Lexer Pipeline

1. Lexer produces raw token array with handles pointing into the shared pool.
2. Parser receives the array wrapped in a `token_view_t`. No copying occurs for prefix buffers or nested parser states.
3. `parser_prefix_buffer` stores `token_view_t` slices (start/end) instead of copying actual tokens.

## 4. Declaration Collector Redesign

### 4.1 Current Issues

- `add_or_update_alloc_var` stores every encountered variable in a dynamically resized allocatable array, copying the entire set for every insert.
- Lookups are O(n) by scanning the array.
- For class(*)/select rank trees with hundreds of branches, we end up copying megabytes of metadata repeatedly.

### 4.2 Proposed Structure

Use a hash table keyed by `(procedure_id, symbol_name, rank_signature)`:

```fortran
type :: decl_entry_t
    integer :: proc_id
    integer :: rank_signature
    type(token_text_handle_t) :: name_handle
    type(decl_entry_t), pointer :: next => null()
end type

type :: decl_hash_table_t
    type(decl_entry_t), pointer :: buckets(:)
    integer :: bucket_count
end type
```

Insertion looks up the bucket by hash, traverses a short linked list, and only allocates a new node when the entry is new. All nodes are allocated from a chunked arena associated with the declaration pass, so there is no global reallocation.

Benefits:
- O(1) expected insertion/update.
- No repeated copying of the entire set.
- Memory usage grows linearly with unique declarations and is freed en masse when the arena resets after each procedure.

### 4.3 API

Expose a clean API in `standardizer_declarations_collection_mod`:

```fortran
module standardizer_decl_table_mod
    type :: decl_table_t
        type(decl_hash_table_t) :: table
        type(arena_t) :: arena
    contains
        procedure :: init
        procedure :: reset
        procedure :: add_or_update
        procedure :: iterate
    end type
end module
```

`generate_and_insert_declarations` simply iterates the table when emitting declarations, decoupling declaration collection from emission order.

## 5. Implementation Plan

1. **Token Text Pool**
   - Add `token_text_pool_mod` with intern/release/get APIs.
   - Update lexer to intern every token’s text and trivia.
   - Update `token_t` to store handles and update assign/finalize accordingly.
   - Replace direct `token%text` access with helper functions (e.g., `token_original`, `token_lower`).

2. **Parser State + Prefix Buffer**
   - Introduce `token_view_t` and refactor `parser_state_t`, prefix buffer, and parser utilities to operate on views instead of copying arrays.
   - Ensure `parser_state_cleanup` simply releases the view reference instead of deallocating.

3. **Lowercase Cache**
   - Add `token_text_get_lower(pool, handle)` that caches the lowercase string inside the handle.
   - Replace all uses of `string_utils::to_lower` on tokens with the cached getter.

4. **Declaration Hash Table**
   - Implement `decl_table_t` with chunked allocation + hashing.
   - Refactor `collect_allocate_vars` and `generate_and_insert_declarations` to use the table API.
   - Remove the old `allocatable_list` arrays to avoid quadratic behavior.

5. **Testing & Profiling**
   - Update unit tests to cover token pool reference counting and declaration dedup logic.
   - Re-run Callgrind/Massif on PR100103, class/coarray tests, and confirm the target functions drop out of the top cost list.
   - Re-run the full round-trip harness (with the usual 100 ms cap) to verify the timeout count decreases.

## 6. Risks & Mitigations

- **Token handle lifetime:** enforce reference counting rules in `token_assign`/`token_finalize` and add assertions in debug builds.
- **Thread-safety:** the pool operates per-parser-instance; we don’t share it across concurrent compilations yet.
- **Hash collisions:** fall back to short linked lists; bucket count chosen to keep load factor ≤0.5.
- **Memory pressure:** arenas reset after each file, so memory scales with largest single compilation unit.

## 7. Deliverables

- Code: new token pool module, refactored lexer/parser, declaration hash table.
- Tests: parser stress tests for large select-rank programs, declaration emission correctness checks.
- Docs: update README/CLAUDE with summary, profiling before/after metrics, and any new debug flags.

---
This modernization aligns the lexer/parser/standardizer with contemporary compiler practices (immutable token handles, arena-backed hash tables). It drastically reduces copying/logical work per token and makes declaration tracking scale with the number of unique identifiers rather than the number of statements. EOF
