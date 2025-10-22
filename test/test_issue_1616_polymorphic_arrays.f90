program test_issue_1616_polymorphic_arrays
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_allocatable_polymorphic_array()
    call test_pointer_polymorphic_array()
    call test_regular_type_array()
    print *, ""
    print *, "All polymorphic array tests passed!"

contains

    subroutine test_allocatable_polymorphic_array()
        character(:), allocatable :: input_code, output_code, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        input_code = "module test_mod" // new_line('A') // &
                     "   implicit none" // new_line('A') // &
                     "   type :: List_circle" // new_line('A') // &
                     "      type(Circle), allocatable :: alloc_type(:)" // new_line('A') // &
                     "      class(Circle), allocatable :: alloc_class(:)" // new_line('A') // &
                     "   end type" // new_line('A') // &
                     "end module"

        print *, "=== Test 1: class(T), allocatable :: arr(:) ==="
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

        if (index(output_code, "class(Circle)") > 0 .and. &
            index(output_code, "allocatable") > 0) then
            print *, "PASS: Polymorphic allocatable array preserved"
        else
            print *, "FAIL: Polymorphic allocatable array not found"
            print *, "Expected: class(Circle), allocatable :: alloc_class(:)"
            error stop 1
        end if
    end subroutine test_allocatable_polymorphic_array

    subroutine test_pointer_polymorphic_array()
        character(:), allocatable :: input_code, output_code, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        input_code = "module test_mod" // new_line('A') // &
                     "   implicit none" // new_line('A') // &
                     "   type :: List_circle" // new_line('A') // &
                     "      class(Circle), pointer :: ptr_class(:)" // new_line('A') // &
                     "   end type" // new_line('A') // &
                     "end module"

        print *, ""
        print *, "=== Test 2: class(T), pointer :: arr(:) ==="
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

        if (index(output_code, "class(Circle)") > 0 .and. &
            index(output_code, "pointer") > 0) then
            print *, "PASS: Polymorphic pointer array preserved"
        else
            print *, "FAIL: Polymorphic pointer array not found"
            print *, "Expected: class(Circle), pointer :: ptr_class(:)"
            error stop 1
        end if
    end subroutine test_pointer_polymorphic_array

    subroutine test_regular_type_array()
        character(:), allocatable :: input_code, output_code, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        input_code = "module test_mod" // new_line('A') // &
                     "   implicit none" // new_line('A') // &
                     "   type :: Container" // new_line('A') // &
                     "      type(Circle), allocatable :: regular(:)" // new_line('A') // &
                     "      class(Shape), allocatable :: polymorphic(:)" // new_line('A') // &
                     "   end type" // new_line('A') // &
                     "end module"

        print *, ""
        print *, "=== Test 3: Mixed type and class arrays ==="
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

        if (index(output_code, "type(Circle)") > 0 .and. &
            index(output_code, "class(Shape)") > 0) then
            print *, "PASS: Mixed type and class arrays preserved"
        else
            print *, "FAIL: Mixed arrays not correctly preserved"
            error stop 1
        end if
    end subroutine test_regular_type_array

end program test_issue_1616_polymorphic_arrays
