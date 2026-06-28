program test_codegen_generics_named_blocks
    use, intrinsic :: iso_fortran_env, only: output_unit
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_factory, only: push_declaration
    use ast_nodes_generics, only: create_trait_block, create_requirement_block, &
        create_implements_block, trait_block_node, &
        requirement_block_node, implements_block_node
    use codegen_core, only: initialize_codegen
    use codegen_generics, only: generate_code_trait_block, &
        generate_code_requirement_block, &
        generate_code_implements_block
    implicit none

    type(ast_arena_t) :: arena
    type(trait_block_node) :: trait_node
    type(requirement_block_node) :: requirement_node
    type(implements_block_node) :: implements_node
    character(len=1) :: params(2)
    integer :: trait_decl_index
    integer :: requirement_decl_index
    integer :: implements_decl_index
    character(len=:), allocatable :: code

    arena = create_ast_arena()
    call initialize_codegen()
    params = ["T", "U"]

    trait_decl_index = push_declaration(arena, "integer", ["trait_value"])
    requirement_decl_index = push_declaration(arena, "integer", ["requirement_value"])
    implements_decl_index = push_declaration(arena, "integer", ["implements_value"])

    trait_node = create_trait_block("IComparable", parameter_names=params, &
        declaration_indices=[trait_decl_index], &
        has_contains=.true.)
    code = generate_code_trait_block(arena, trait_node)
    call assert_contains(code, "trait IComparable(T, U)", "trait header mismatch")
    call assert_contains(code, "integer :: trait_value", "trait declaration missing")
    call assert_contains(code, "contains", "trait contains missing")
    call assert_contains(code, "end trait IComparable", "trait end mismatch")

    requirement_node = create_requirement_block("Ordered", parameter_names=params, &
        declaration_indices=[ &
        requirement_decl_index], &
        has_contains=.true.)
    code = generate_code_requirement_block(arena, requirement_node)
    call assert_contains(code, "requirement Ordered(T, U)", &
        "requirement header mismatch")
    call assert_contains(code, "integer :: requirement_value", &
        "requirement declaration missing")
    call assert_contains(code, "contains", "requirement contains missing")
    call assert_contains(code, "end requirement Ordered", "requirement end mismatch")

    implements_node = create_implements_block("IComparable", parameter_names=params, &
        declaration_indices=[ &
        implements_decl_index], &
        has_contains=.true.)
    code = generate_code_implements_block(arena, implements_node)
    call assert_contains(code, "implements IComparable(T, U)", &
        "implements header mismatch")
    call assert_contains(code, "integer :: implements_value", &
        "implements declaration missing")
    call assert_contains(code, "contains", "implements contains missing")
    call assert_contains(code, "end implements IComparable", "implements end mismatch")

    write (output_unit, '(A)') "PASS: codegen named blocks"

contains

    subroutine assert_contains(haystack, needle, fail_message)
        character(len=*), intent(in) :: haystack
        character(len=*), intent(in) :: needle
        character(len=*), intent(in) :: fail_message

        if (index(haystack, needle) == 0) then
            write (output_unit, '(A)') "FAIL: " // fail_message
            write (output_unit, '(A)') "Missing: " // needle
            write (output_unit, '(A)') haystack
            error stop 1
        end if
    end subroutine assert_contains

end program test_codegen_generics_named_blocks
