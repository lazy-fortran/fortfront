# Pratt Parser Modernization

## Operator Table

| Symbol                                      | Fixity    | Associativity | Precedence | Handler |
|---------------------------------------------|-----------|---------------|------------|---------|
| `:`                                         | infix     | left          | 10         | `range` |
| `.eqv.`, `.neqv.`                           | infix     | left          | 20         | `binary`
| `.or.`                                      | infix     | left          | 30         | `binary`
| `.and.`                                     | infix     | left          | 40         | `binary`
| `==`, `/=`, `<`, `<=`, `>`, `>=`            | infix     | non-assoc     | 50         | `binary`
| `//`                                        | infix     | left          | 60         | `binary`
| `+`, `-`                                    | infix     | left          | 70         | `binary`
| `*`, `/`                                    | infix     | left          | 80         | `binary`
| `**`                                        | infix     | right         | 90         | `binary`
| `+`, `-`, `.not.`                           | prefix    | right         | 95         | `prefix`
| `%`                                         | postfix   | left          | 110        | `component`
| `()` (call/subscript)                       | postfix   | left          | 110        | `call`
| `[]` (modern literal / indexing)           | postfix   | left          | 110        | `square`
| `(/`, `/)` (legacy literal sentinel pair)   | grouping  | n/a           | n/a        | `legacy`

*Precedence values monotonically increase with tighter binding; colon is lowest.*

## Token SoA Layout

Convert the active slice of `token_t` into structure-of-arrays buffers before entering the Pratt loop.

> **Status:** Operator tables and the stack-driven Pratt core are implemented. The Pratt engine now pre-builds a SoA token view for the hot path, while keeping the authoritative storage in `parser%tokens` for downstream consumers.

```fortran
character(len=max_text), allocatable :: token_text(:)
integer, allocatable :: token_kind(:)
integer, allocatable :: token_line(:)
integer, allocatable :: token_column(:)
```

- `max_text` computed once per slice.
- Buffers allocated from arena slab when provided, otherwise fallback to heap.
- `parser_state_t%current_token` only stores cursor; actual reads operate on SoA arrays.
- Maintain side table of original indices for diagnostics and re-slicing.

## Loop Skeleton

```
expect_operand = .true.
init operand/operator/prefix stacks

DO WHILE (.true.)
  tk = peek_token()
  IF terminator(tk) EXIT

  IF expect_operand THEN
     IF prefix_operator(tk) THEN
        push_prefix(tk)
        advance(); CYCLE
     ELSE IF tk == ':' THEN
        push_operand(0)
        call parse_range_postfix()
        expect_operand = .false.
        CYCLE
     ELSE
        expr = parse_primary_atom()
        expr = apply_prefix(expr)
        push_operand(expr)
        call consume_postfix_chain(expr)
        expect_operand = .false.
     END IF
  ELSE
     IF tk == ':' THEN
        call parse_range_postfix()
        call consume_postfix_chain()
        CYCLE
     ELSE IF postfix_operator(tk) THEN
        call consume_postfix_chain()
        CYCLE
     ELSE IF infix_operator(tk) THEN
        call reduce_until(precedence(tk))
        push_operator(tk)
        expect_operand = .true.
        advance()
        CYCLE
     ELSE IF closing_group(tk) THEN
        call reduce_until_group()
        advance()
        call consume_postfix_chain()
        expect_operand = .false.
        CYCLE
     END IF
  END IF
END DO

call reduce_all()
expr_index = pop_operand()
```

- `reduce_until(p)` collapses operators whose precedence is greater than (or equal to if left associative) `p`.
- `parse_range_postfix` parses optional upper bound and stride via nested Pratt invocations with `min_precedence = PREC_RANGE + 1` and shared terminators `(:,),],,)`.

## Postfix Dispatch

1. **Component access (`%`)**: uses existing `parse_component_access_postfix` helper; operate on AST index on top of operand stack.
2. **Call/Subscript (`(` `)` )**: - capture argument slices by repeatedly invoking `parse_range` until `)` with comma terminators. Calls re-use SoA view to avoid copying tokens.
3. **Square index**: translate to call/subscript path with bracket tokens.
4. **Array literals**: when `expect_operand` is true, detect `(/` and `[` patterns with bounded lookahead and delegate to literal helpers.

## Range Handling

- Colon consumes left operand (or `0` when absent) and parses optional upper and stride:
  - `upper` stops at `:`, `,`, `)` or `]`.
  - When stride colon present, parse third expression with identical terminators.
  - Construct node via `push_range_expression(arena, lower, upper, stride, line, column)`.
- Range operator participates in Pratt loop but never placed on operator stack; evaluation occurs immediately to preserve ternary semantics and avoid nested ranges.

## Memory & Spill Policy

- Operand/operator stacks grow geometrically. Default capacity `32`, doubling when exceeded.
- When requested size surpasses 512 (configurable), allocate via arena slab to keep deterministic heap footprint.
- Prefix stack kept on stack (max depth equal to operator chain); fallback to arena buffer on overflow.

## Error Strategy

- On unexpected token in operand position, emit literal node with diagnostic text (mirrors legacy behaviour) and advance to avoid infinite loops.
- During reduction, sanity check operand availability; on underflow, push synthetic error literal and clear stacks, ensuring parser continues.

## Integration Hooks

- Existing public API (`parse_expression`, `parse_range`, `parse_logical_*`, etc.) map to `parse_expression_core(parser, arena, min_prec)` with the operator table controlling allowed reductions.
- `parse_primary` delegates to dedicated `parse_primary_atom` + postfix chain.
- `parse_unary` simply harvests prefix stack then calls `parse_primary`.

## Open Questions

- Should colon allow chained ranges (`a:b:c:d`) beyond stride? Proposal: treat subsequent colon after stride as new range anchored to previous result to preserve backwards compatibility.
- Investigate whether existing semantic passes rely on zero literal inserted for unary minus; consider introducing dedicated unary node for clarity.
- Confirm whether arena slabs can expose raw byte buffers for SoA allocation without additional copies.
