program test_derived_type_parsing
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        iostat_eor
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_simple_derived_type()
    call test_derived_type_with_multiple_components()
    call test_nested_derived_types()
    call test_derived_type_with_end_field()
    print *, ""
    print *, "All derived type tests completed."

contains

    include 'common/read_example.inc'

    subroutine test_simple_derived_type()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        ! Test simple derived type with one component
        call read_example('examples/lf/derived_type_simple.lf', input_code)

        print *, ""
        print *, "=== Test 1: Simple derived type ==="
        print *, "Input:"
        print *, input_code

        ! Parse and generate code
        call lex_source(input_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "✗ FAIL: Lexer error:", trim(error_msg)
            error stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "✗ FAIL: Parser error:", trim(error_msg)
            error stop 1
        end if

        if (prog_index > 0) then
            call emit_fortran(arena, prog_index, output_code)
            print *, "Output:"
            print *, output_code

            ! Check if component is preserved
            if (index(output_code, "integer :: age") > 0) then
                print *, "✓ PASS: Component preserved"
            else
                print *, "✗ FAIL: Component missing"
                error stop 1
            end if
        else
            print *, "✗ FAIL: Parsing failed"
            error stop 1
        end if
    end subroutine

    subroutine test_derived_type_with_multiple_components()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        ! Test derived type with multiple components
        call read_example('examples/lf/derived_type_multiple_components.lf', &
            input_code)

        print *, ""
        print *, "=== Test 2: Multiple components ==="
        print *, "Input:"
        print *, input_code

        call lex_source(input_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "✗ FAIL: Lexer error:", trim(error_msg)
            error stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "✗ FAIL: Parser error:", trim(error_msg)
            error stop 1
        end if

        if (prog_index > 0) then
            call emit_fortran(arena, prog_index, output_code)
            print *, "Output:"
            print *, output_code

            ! Check if all components are preserved
            if (index(output_code, "integer :: age") > 0 .and. &
                index(output_code, "character") > 0 .and. &
                (index(output_code, "real :: height") > 0 .or. &
                index(output_code, "real(dp) :: height") > 0 .or. &
                index(output_code, "real(8) :: height") > 0)) then
                print *, "✓ PASS: All components preserved"
            else
                print *, "✗ FAIL: Some components missing"
                error stop 1
            end if
        else
            print *, "✗ FAIL: Parsing failed"
            error stop 1
        end if
    end subroutine

    subroutine test_nested_derived_types()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        ! Test with nested derived types
        call read_example('examples/lf/derived_type_nested.lf', input_code)

        print *, ""
        print *, "=== Test 3: Nested derived types ==="
        print *, "Input:"
        print *, input_code

        call lex_source(input_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "✗ FAIL: Lexer error:", trim(error_msg)
            error stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "✗ FAIL: Parser error:", trim(error_msg)
            error stop 1
        end if

        if (prog_index > 0) then
            call emit_fortran(arena, prog_index, output_code)
            print *, "Output:"
            print *, output_code

            ! Check if both types are preserved
            if (index(output_code, "type :: address_t") > 0 .and. &
                index(output_code, "type :: person_t") > 0) then
                print *, "✓ PASS: Both types preserved"
            else
                print *, "✗ FAIL: Type definitions incomplete"
                error stop 1
            end if
        else
            print *, "✗ FAIL: Parsing failed"
            error stop 1
        end if
    end subroutine

    subroutine test_derived_type_with_end_field()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        ! Test derived type with field named 'end' (issue #1542)
        call read_example('examples/lf/derived_type_end_field.lf', input_code)

        print *, ""
        print *, "=== Test 4: Derived type with 'end' field (issue #1542) ==="
        print *, "Input:"
        print *, input_code

        call lex_source(input_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "✗ FAIL: Lexer error:", trim(error_msg)
            error stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "✗ FAIL: Parser error:", trim(error_msg)
            error stop 1
        end if

        if (prog_index > 0) then
            call emit_fortran(arena, prog_index, output_code)
            print *, "Output:"
            print *, output_code

            ! Check if both components are preserved, especially the 'end' field
            if ((index(output_code, "real :: start") > 0 .or. &
                index(output_code, "real(dp) :: start") > 0 .or. &
                index(output_code, "real(8) :: start") > 0) .and. &
                (index(output_code, "real :: end") > 0 .or. &
                index(output_code, "real(dp) :: end") > 0 .or. &
                index(output_code, "real(8) :: end") > 0)) then
                print *, "✓ PASS: Both 'start' and 'end' fields preserved"
            else
                print *, "✗ FAIL: Missing 'end' field"
                error stop 1
            end if
        else
            print *, "✗ FAIL: Parsing failed"
            error stop 1
        end if
    end subroutine

end program test_derived_type_parsing
