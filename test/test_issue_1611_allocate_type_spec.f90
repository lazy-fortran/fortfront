program test_issue_1611_allocate_type_spec
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_allocate_class_typespec()
    call test_allocate_type_typespec()
    call test_allocate_no_typespec()
    print *, ""
    print *, "All allocate type-spec tests passed!"

contains

    subroutine test_allocate_class_typespec()
        character(:), allocatable :: input_code, output_code, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        input_code = "module mymod" // new_line('A') // &
                     "   type :: mytype" // new_line('A') // &
                     "      integer :: value" // new_line('A') // &
                     "   end type mytype" // new_line('A') // &
                     "contains" // new_line('A') // &
                     "   function create() result(obj)" // new_line('A') // &
                     "      class(mytype), allocatable :: obj" // new_line('A') // &
                     "      allocate(mytype :: obj)" // new_line('A') // &
                     "   end function create" // new_line('A') // &
                     "end module mymod"

        print *, "=== Test 1: allocate(TypeName :: var) preservation ==="
        print *, "Input:"
        print *, input_code

        call lex_source(input_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Lexer error:", trim(error_msg)
            error stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Parser error:", trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output_code)

        print *, "Output:"
        print *, output_code

        if (index(output_code, "allocate(mytype :: obj)") > 0) then
            print *, "PASS: Type-spec correctly preserved"
        else
            print *, "FAIL: Type-spec not found in output"
            print *, "Expected: allocate(mytype :: obj)"
            error stop 1
        end if
    end subroutine test_allocate_class_typespec

    subroutine test_allocate_type_typespec()
        character(:), allocatable :: input_code, output_code, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        input_code = "program test" // new_line('A') // &
                     "   type :: circle" // new_line('A') // &
                     "      real :: radius" // new_line('A') // &
                     "   end type circle" // new_line('A') // &
                     "   type(circle), allocatable :: c" // new_line('A') // &
                     "   allocate(circle :: c)" // new_line('A') // &
                     "   c%radius = 5.0" // new_line('A') // &
                     "end program test"

        print *, ""
        print *, "=== Test 2: allocate with type() instead of class() ==="
        print *, "Input:"
        print *, input_code

        call lex_source(input_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Lexer error:", trim(error_msg)
            error stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Parser error:", trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output_code)

        print *, "Output:"
        print *, output_code

        if (index(output_code, "allocate(circle :: c)") > 0) then
            print *, "PASS: Type-spec with type() correctly preserved"
        else
            print *, "FAIL: Type-spec not found in output"
            print *, "Expected: allocate(circle :: c)"
            error stop 1
        end if
    end subroutine test_allocate_type_typespec

    subroutine test_allocate_no_typespec()
        character(:), allocatable :: input_code, output_code, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        input_code = "program test" // new_line('A') // &
                     "   integer, allocatable :: arr(:)" // new_line('A') // &
                     "   allocate(arr(10))" // new_line('A') // &
                     "end program test"

        print *, ""
        print *, "=== Test 3: allocate without type-spec (backward compat) ==="
        print *, "Input:"
        print *, input_code

        call lex_source(input_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Lexer error:", trim(error_msg)
            error stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Parser error:", trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output_code)

        print *, "Output:"
        print *, output_code

        if (index(output_code, "allocate(arr(10))") > 0) then
            print *, "PASS: Regular allocate without type-spec works"
        else
            print *, "FAIL: Regular allocate broken"
            print *, "Expected: allocate(arr(10))"
            error stop 1
        end if
    end subroutine test_allocate_no_typespec

end program test_issue_1611_allocate_type_spec
