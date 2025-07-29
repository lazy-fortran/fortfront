program test_frontend_codegen_program_structure
    use ast_core
    use ast_factory
    use codegen_core, only: generate_code_from_arena
    use type_system_hm
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_program_with_implicit_none()
    call test_program_with_declarations()
    call test_program_with_contains()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_program_with_implicit_none()
        type(ast_arena_t) :: arena
        integer :: prog_idx, assign_idx, id_x_idx, lit_idx
        integer :: body_indices(1)
        character(len=:), allocatable :: code

        test_count = test_count + 1

        ! Create arena
        arena = create_ast_stack()

        ! Create simple program: x = 42
        id_x_idx = push_identifier(arena, "x", 1, 1)
        lit_idx = push_literal(arena, "42", LITERAL_INTEGER, 1, 5)
        assign_idx = push_assignment(arena, id_x_idx, lit_idx, 1, 1)
        
        ! Create program with the assignment
        body_indices(1) = assign_idx
        prog_idx = push_program(arena, "test", body_indices, 1, 1)

        ! Generate code
        code = generate_code_from_arena(arena, prog_idx)

        ! Check that program structure is generated correctly
        if (index(code, "program test") > 0 .and. &
            index(code, "x = 42") > 0 .and. &
            index(code, "end program test") > 0) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Program structure generated correctly"
        else
            write (*, '(A)') "FAIL: Program structure generated correctly"
            write (*, '(A)') "Generated code:"
            write (*, '(A)') code
        end if
        
        ! Clean up
        call arena%clear()
    end subroutine test_program_with_implicit_none

    subroutine test_program_with_declarations()
        type(ast_arena_t) :: arena
        integer :: prog_idx, assign1_idx, assign2_idx
        integer :: id_x_idx, id_y_idx, lit_int_idx, lit_real_idx
        integer :: body_indices(2)
        type(mono_type_t) :: int_type, real_type
        character(len=:), allocatable :: code

        test_count = test_count + 1

        ! Create arena
        arena = create_ast_stack()
        
        ! x = 42
        id_x_idx = push_identifier(arena, "x", 1, 1)
        lit_int_idx = push_literal(arena, "42", LITERAL_INTEGER, 1, 5)
        assign1_idx = push_assignment(arena, id_x_idx, lit_int_idx, 1, 1)
        
        ! y = 3.14
        id_y_idx = push_identifier(arena, "y", 2, 1)
        lit_real_idx = push_literal(arena, "3.14", LITERAL_REAL, 2, 5)
        assign2_idx = push_assignment(arena, id_y_idx, lit_real_idx, 2, 1)
        
        ! Create program with multiple assignments
        body_indices(1) = assign1_idx
        body_indices(2) = assign2_idx
        prog_idx = push_program(arena, "test", body_indices, 1, 1)
        
        ! Set inferred types manually for testing
        ! Note: In the arena-based system, we need to access nodes to set their types
        select type (node => arena%entries(id_x_idx)%node)
        type is (identifier_node)
            allocate(node%inferred_type)
            node%inferred_type%kind = TINT
        end select
        
        select type (node => arena%entries(id_y_idx)%node)
        type is (identifier_node)
            allocate(node%inferred_type)
            node%inferred_type%kind = TREAL
        end select

        ! Generate code
        code = generate_code_from_arena(arena, prog_idx)

        ! Check for multiple assignments in program
        if (index(code, "x = 42") > 0 .and. &
            index(code, "y = 3.14") > 0) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Program generates multiple assignments"
        else
            write (*, '(A)') "FAIL: Program generates multiple assignments"
            write (*, '(A)') "Generated code:"
            write (*, '(A)') code
        end if
        
        ! Clean up
        call arena%clear()
    end subroutine test_program_with_declarations

    subroutine test_program_with_contains()
        type(ast_arena_t) :: arena
        integer :: prog_idx, func_idx, assign_idx, func_assign_idx
        integer :: id_x_idx, id_f_idx, fcall_idx, lit_idx
        integer :: param_x_idx, func_value_x_idx
        integer :: arg_indices(1), param_indices(1), func_body_indices(1), prog_body_indices(2)
        character(len=:), allocatable :: code

        test_count = test_count + 1

        ! Create arena
        arena = create_ast_stack()
        
        ! Main: x = f(5)
        id_x_idx = push_identifier(arena, "x", 2, 5)
        lit_idx = push_literal(arena, "5", LITERAL_INTEGER, 2, 11)
        arg_indices(1) = lit_idx
        fcall_idx = push_call_or_subscript(arena, "f", arg_indices, 2, 9)
        assign_idx = push_assignment(arena, id_x_idx, fcall_idx, 2, 5)
        
        ! Function: function f(x)
        ! Parameter
        param_x_idx = push_identifier(arena, "x", 5, 20)
        param_indices(1) = param_x_idx
        
        ! Function body: f = x
        id_f_idx = push_identifier(arena, "f", 6, 9)
        func_value_x_idx = push_identifier(arena, "x", 6, 13)
        func_assign_idx = push_assignment(arena, id_f_idx, func_value_x_idx, 6, 9)
        func_body_indices(1) = func_assign_idx
        
        ! Create function
        func_idx = push_function_def(arena, "f", param_indices, "real", func_body_indices, 5, 5)
        
        ! Build program
        prog_body_indices(1) = assign_idx
        prog_body_indices(2) = func_idx
        prog_idx = push_program(arena, "test", prog_body_indices, 1, 1)

        ! Generate code
        code = generate_code_from_arena(arena, prog_idx)

        ! Check for contains section
        if (index(code, "contains") > 0 .and. &
            index(code, "function f") > 0) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Program generates contains section"
        else
            write (*, '(A)') "FAIL: Program generates contains section"
            write (*, '(A)') "Generated code:"
            write (*, '(A)') code
        end if
        
        ! Clean up
        call arena%clear()
    end subroutine test_program_with_contains

end program test_frontend_codegen_program_structure
