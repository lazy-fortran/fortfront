program test_frontend_codegen_parameter_grouping
    use ast_core
    use ast_factory
    use codegen_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. test_basic_grouping()) all_passed = .false.
    if (.not. test_mixed_types()) all_passed = .false.
    if (.not. test_different_intents()) all_passed = .false.
    if (.not. test_mixed_declarations()) all_passed = .false.

    if (all_passed) then
        print *, "All parameter grouping tests passed!"
        stop 0
    else
        print *, "Some parameter grouping tests failed!"
        stop 1
    end if

contains

    logical function test_basic_grouping()
        type(ast_arena_t) :: arena
        integer :: func_idx, body_start_idx
        integer :: a_idx, b_idx
        integer :: decl_a_idx, decl_b_idx
        integer :: implicit_none_idx
        character(len=:), allocatable :: generated_code

        test_basic_grouping = .true.

        ! Create function with two real(8) parameters
        arena = create_ast_arena(100)

        ! Create parameter identifiers
        a_idx = push_identifier(arena, "a", 1, 1)
        b_idx = push_identifier(arena, "b", 1, 1)

        ! Add implicit none
        implicit_none_idx = push_literal(arena, "implicit none", LITERAL_STRING, 2, 1)

        ! Create function with initial body
        func_idx = push_function_def(arena, "add_numbers", [a_idx, b_idx], "real(8)", [implicit_none_idx], 1, 1)

        ! Create parameter declarations
        decl_a_idx = push_parameter_declaration(arena, "a", "real", kind_value=8, &
                                                intent_value=1, line=3, column=1)  ! 1 = intent(in)
        decl_b_idx = push_parameter_declaration(arena, "b", "real", kind_value=8, &
                                                intent_value=1, line=4, column=1)  ! 1 = intent(in)

        ! Update function body
        select type (func_node => arena%entries(func_idx)%node)
        type is (function_def_node)
            func_node%body_indices = [implicit_none_idx, decl_a_idx, decl_b_idx]
        end select

        ! Generate code
        generated_code = generate_code_from_arena(arena, func_idx)

        print *, "Generated code:"
        print *, generated_code

        ! Check that parameters are grouped
        if (index(generated_code, "real(8), intent(in) :: a, b") > 0) then
            print *, "PASS: Basic parameter grouping works"
        else
            print *, "FAIL: Basic parameter grouping"
            test_basic_grouping = .false.
        end if

    end function test_basic_grouping

    logical function test_mixed_types()
        type(ast_arena_t) :: arena
        integer :: func_idx
        integer :: x_idx, y_idx, n_idx
        integer :: decl_x_idx, decl_y_idx, decl_n_idx
        integer :: implicit_none_idx
        character(len=:), allocatable :: generated_code

        test_mixed_types = .true.

        arena = create_ast_arena(100)

        ! Create parameters
        x_idx = push_identifier(arena, "x", 1, 1)
        y_idx = push_identifier(arena, "y", 1, 1)
        n_idx = push_identifier(arena, "n", 1, 1)

        ! Add implicit none
        implicit_none_idx = push_literal(arena, "implicit none", LITERAL_STRING, 2, 1)

        ! Create function
        func_idx = push_function_def(arena, "mixed_func", [x_idx, y_idx, n_idx], "", [implicit_none_idx], 1, 1)

        ! Create declarations - two real(8), one integer
        decl_x_idx = push_parameter_declaration(arena, "x", "real", kind_value=8, &
                                                intent_value=1, line=3, column=1)  ! 1 = intent(in)
        decl_y_idx = push_parameter_declaration(arena, "y", "real", kind_value=8, &
                                                intent_value=1, line=4, column=1)  ! 1 = intent(in)
        decl_n_idx = push_parameter_declaration(arena, "n", "integer", kind_value=0, &
                                                intent_value=1, line=5, column=1)  ! 1 = intent(in)

        ! Update function body
        select type (func_node => arena%entries(func_idx)%node)
        type is (function_def_node)
        func_node%body_indices = [implicit_none_idx, decl_x_idx, decl_y_idx, decl_n_idx]
        end select

        ! Generate code
        generated_code = generate_code_from_arena(arena, func_idx)

        ! Check grouping
        if (index(generated_code, "real(8), intent(in) :: x, y") > 0 .and. &
            index(generated_code, "integer, intent(in) :: n") > 0) then
            print *, "PASS: Mixed type grouping works"
        else
            print *, "FAIL: Mixed type grouping"
            print *, "Generated code:"
            print *, generated_code
            test_mixed_types = .false.
        end if

    end function test_mixed_types

    logical function test_different_intents()
        type(ast_arena_t) :: arena
        integer :: func_idx
        integer :: a_idx, b_idx, c_idx
        integer :: decl_a_idx, decl_b_idx, decl_c_idx
        integer :: implicit_none_idx
        character(len=:), allocatable :: generated_code

        test_different_intents = .true.

        arena = create_ast_arena(100)

        ! Create parameters
        a_idx = push_identifier(arena, "a", 1, 1)
        b_idx = push_identifier(arena, "b", 1, 1)
        c_idx = push_identifier(arena, "c", 1, 1)

        implicit_none_idx = push_literal(arena, "implicit none", LITERAL_STRING, 2, 1)

        ! Create subroutine
        func_idx = push_subroutine_def(arena, "intent_sub", [a_idx, b_idx, c_idx], [implicit_none_idx], 1, 1)

        ! Create declarations with different intents
        decl_a_idx = push_parameter_declaration(arena, "a", "real", kind_value=8, &
                                                intent_value=1, line=3, column=1)  ! 1 = intent(in)
        decl_b_idx = push_parameter_declaration(arena, "b", "real", kind_value=8, &
                                                intent_value=2, line=4, column=1)  ! 2 = intent(out)
        decl_c_idx = push_parameter_declaration(arena, "c", "real", kind_value=8, &
                                                intent_value=3, line=5, column=1)  ! 3 = intent(inout)

        ! Update body
        select type (sub_node => arena%entries(func_idx)%node)
        type is (subroutine_def_node)
         sub_node%body_indices = [implicit_none_idx, decl_a_idx, decl_b_idx, decl_c_idx]
        end select

        ! Generate code
        generated_code = generate_code_from_arena(arena, func_idx)

        ! Check that different intents are NOT grouped
        if (index(generated_code, "real(8), intent(in) :: a") > 0 .and. &
            index(generated_code, "real(8), intent(out) :: b") > 0 .and. &
            index(generated_code, "real(8), intent(inout) :: c") > 0) then
            print *, "PASS: Different intents kept separate"
        else
            print *, "FAIL: Different intents handling"
            print *, "Generated code:"
            print *, generated_code
            test_different_intents = .false.
        end if

    end function test_different_intents

    logical function test_mixed_declarations()
        type(ast_arena_t) :: arena
        integer :: func_idx
        integer :: a_idx, b_idx, c_idx, result_idx
        integer :: decl_a_idx, decl_b_idx, decl_c_idx, decl_result_idx
        integer :: implicit_none_idx, stmt_idx
        character(len=:), allocatable :: generated_code

        test_mixed_declarations = .true.

        arena = create_ast_arena(100)

        ! Create parameters
        a_idx = push_identifier(arena, "a", 1, 1)
        b_idx = push_identifier(arena, "b", 1, 1)
        c_idx = push_identifier(arena, "c", 1, 1)

        implicit_none_idx = push_literal(arena, "implicit none", LITERAL_STRING, 2, 1)

        ! Create function
        func_idx = push_function_def(arena, "mixed_decls", [a_idx, b_idx, c_idx], "real(8)", [implicit_none_idx], 1, 1)

        ! Create mixed declarations
        decl_a_idx = push_parameter_declaration(arena, "a", "real", kind_value=8, &
                                                intent_value=1, line=3, column=1)  ! 1 = intent(in)
        decl_b_idx = push_parameter_declaration(arena, "b", "real", kind_value=8, &
                                                intent_value=1, line=4, column=1)  ! 1 = intent(in)

        ! Add a non-declaration statement
      stmt_idx = push_literal(arena, "! This breaks the grouping", LITERAL_STRING, 5, 1)

        decl_c_idx = push_parameter_declaration(arena, "c", "real", kind_value=8, &
                                                intent_value=1, line=6, column=1)  ! 1 = intent(in)
        decl_result_idx = push_declaration(arena, "real", "result", kind_value=8, line=7, column=1)

        ! Update body with mixed content
        select type (func_node => arena%entries(func_idx)%node)
        type is (function_def_node)
            func_node%body_indices = [implicit_none_idx, decl_a_idx, decl_b_idx, stmt_idx, decl_c_idx, decl_result_idx]
        end select

        ! Generate code
        generated_code = generate_code_from_arena(arena, func_idx)

        ! Check that grouping is interrupted by non-declaration
        if (index(generated_code, "real(8), intent(in) :: a, b") > 0 .and. &
            index(generated_code, "! This breaks the grouping") > 0 .and. &
            index(generated_code, "real(8), intent(in) :: c") > 0 .and. &
            index(generated_code, "real(8) :: result") > 0) then
            print *, "PASS: Grouping interrupted by non-declaration"
        else
            print *, "FAIL: Mixed declaration handling"
            print *, "Generated code:"
            print *, generated_code
            test_mixed_declarations = .false.
        end if

    end function test_mixed_declarations

end program test_frontend_codegen_parameter_grouping
