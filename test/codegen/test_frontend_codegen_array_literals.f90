program test_frontend_codegen_array_literals
    use ast_core
    use ast_factory
    use codegen_core, only: generate_code_from_arena
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Array Literal Code Generation Tests ==='
    print *

    ! Run comprehensive array literal codegen tests
    if (.not. test_empty_array_codegen()) all_passed = .false.
    if (.not. test_integer_array_codegen()) all_passed = .false.
    if (.not. test_real_array_codegen()) all_passed = .false.
    if (.not. test_mixed_array_codegen()) all_passed = .false.
    if (.not. test_character_array_codegen()) all_passed = .false.
    if (.not. test_logical_array_codegen()) all_passed = .false.
    if (.not. test_nested_array_codegen()) all_passed = .false.
    if (.not. test_array_in_assignment()) all_passed = .false.
    if (.not. test_array_constructor_style()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All array literal code generation tests passed!'
        stop 0
    else
        print *, 'Some array literal code generation tests failed!'
        stop 1
    end if

contains

    logical function test_empty_array_codegen()
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: code
        integer, allocatable :: indices(:)
        integer :: array_index

        test_empty_array_codegen = .true.
        print *, 'Testing empty array code generation...'

        arena = create_ast_arena()
        
        ! Create empty array
        allocate(indices(0))
        array_index = push_array_literal(arena, indices, 1, 1)
        
        ! Generate code
        code = generate_code_from_arena(arena, array_index)
        
        ! Check generated code
        if (code == "[integer ::]" .or. code == "[]") then
            print *, '  PASS: Empty array generated as "', trim(code), '"'
        else
            print *, '  FAIL: Expected "[integer ::]" or "[]", got "', trim(code), '"'
            test_empty_array_codegen = .false.
        end if
        
        call arena%clear()
        
    end function test_empty_array_codegen

    logical function test_integer_array_codegen()
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: code
        integer, allocatable :: indices(:)
        integer :: i, elem_index, array_index

        test_integer_array_codegen = .true.
        print *, 'Testing integer array code generation...'

        arena = create_ast_arena()
        
        ! Create array [1, 2, 3]
        allocate(indices(3))
        indices(1) = push_literal(arena, "1", LITERAL_INTEGER, 1, 1)
        indices(2) = push_literal(arena, "2", LITERAL_INTEGER, 1, 1)
        indices(3) = push_literal(arena, "3", LITERAL_INTEGER, 1, 1)
        array_index = push_array_literal(arena, indices, 1, 1)
        
        ! Generate code
        code = generate_code_from_arena(arena, array_index)
        
        ! Check generated code - should be array constructor syntax
        if (code == "[1, 2, 3]" .or. code == "(/ 1, 2, 3 /)") then
            print *, '  PASS: Integer array generated as "', trim(code), '"'
        else
            print *, '  FAIL: Expected "[1, 2, 3]" or "(/ 1, 2, 3 /)", got "', trim(code), '"'
            test_integer_array_codegen = .false.
        end if
        
        call arena%clear()
        
    end function test_integer_array_codegen

    logical function test_real_array_codegen()
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: code
        integer, allocatable :: indices(:)
        integer :: elem_index, array_index

        test_real_array_codegen = .true.
        print *, 'Testing real array code generation...'

        arena = create_ast_arena()
        
        ! Create array [1.0, 2.5, 3.14]
        allocate(indices(3))
        elem_index = push_literal(arena, "1.0", LITERAL_REAL, 1, 1)
        indices(1) = elem_index
        elem_index = push_literal(arena, "2.5", LITERAL_REAL, 1, 1)
        indices(2) = elem_index
        elem_index = push_literal(arena, "3.14", LITERAL_REAL, 1, 1)
        indices(3) = elem_index
        array_index = push_array_literal(arena, indices, 1, 1)
        
        ! Generate code
        code = generate_code_from_arena(arena, array_index)
        
        ! Check generated code - real literals get d0 suffix in codegen
        if (code == "[1.0, 2.5, 3.14]" .or. code == "(/ 1.0, 2.5, 3.14 /)" .or. &
            code == "(/ 1.0d0, 2.5d0, 3.14d0 /)" .or. code == "[1.0d0, 2.5d0, 3.14d0]") then
            print *, '  PASS: Real array generated as "', trim(code), '"'
        else
            print *, '  FAIL: Expected real array syntax, got "', trim(code), '"'
            test_real_array_codegen = .false.
        end if
        
        call arena%clear()
        
    end function test_real_array_codegen

    logical function test_mixed_array_codegen()
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: code
        integer, allocatable :: indices(:)
        integer :: elem_index, array_index

        test_mixed_array_codegen = .true.
        print *, 'Testing mixed type array code generation...'

        arena = create_ast_arena()
        
        ! Create array [1, 2.0, 3] - mixed int/real
        allocate(indices(3))
        elem_index = push_literal(arena, "1", LITERAL_INTEGER, 1, 1)
        indices(1) = elem_index
        elem_index = push_literal(arena, "2.0", LITERAL_REAL, 1, 1)
        indices(2) = elem_index
        elem_index = push_literal(arena, "3", LITERAL_INTEGER, 1, 1)
        indices(3) = elem_index
        array_index = push_array_literal(arena, indices, 1, 1)
        
        ! Generate code
        code = generate_code_from_arena(arena, array_index)
        
        ! For mixed types, should promote to real
        if (index(code, "1") > 0 .and. index(code, "2.0") > 0 .and. index(code, "3") > 0) then
            print *, '  PASS: Mixed array generated as "', trim(code), '"'
        else
            print *, '  FAIL: Mixed array should contain all elements, got "', trim(code), '"'
            test_mixed_array_codegen = .false.
        end if
        
        call arena%clear()
        
    end function test_mixed_array_codegen

    logical function test_character_array_codegen()
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: code
        integer, allocatable :: indices(:)
        integer :: elem_index, array_index

        test_character_array_codegen = .true.
        print *, 'Testing character array code generation...'

        arena = create_ast_arena()
        
        ! Create array ["hello", "world"]
        allocate(indices(2))
        elem_index = push_literal(arena, '"hello"', LITERAL_STRING, 1, 1)
        indices(1) = elem_index
        elem_index = push_literal(arena, '"world"', LITERAL_STRING, 1, 1)
        indices(2) = elem_index
        array_index = push_array_literal(arena, indices, 1, 1)
        
        ! Generate code
        code = generate_code_from_arena(arena, array_index)
        
        if (index(code, '"hello"') > 0 .and. index(code, '"world"') > 0) then
            print *, '  PASS: Character array generated'
        else
            print *, '  FAIL: Character array should contain strings'
            test_character_array_codegen = .false.
        end if
        
        call arena%clear()
        
    end function test_character_array_codegen

    logical function test_logical_array_codegen()
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: code
        integer, allocatable :: indices(:)
        integer :: elem_index, array_index

        test_logical_array_codegen = .true.
        print *, 'Testing logical array code generation...'

        arena = create_ast_arena()
        
        ! Create array [.true., .false., .true.]
        allocate(indices(3))
        elem_index = push_literal(arena, ".true.", LITERAL_LOGICAL, 1, 1)
        indices(1) = elem_index
        elem_index = push_literal(arena, ".false.", LITERAL_LOGICAL, 1, 1)
        indices(2) = elem_index
        elem_index = push_literal(arena, ".true.", LITERAL_LOGICAL, 1, 1)
        indices(3) = elem_index
        array_index = push_array_literal(arena, indices, 1, 1)
        
        ! Generate code
        code = generate_code_from_arena(arena, array_index)
        
        if (index(code, ".true.") > 0 .and. index(code, ".false.") > 0) then
            print *, '  PASS: Logical array generated'
        else
            print *, '  FAIL: Logical array should contain .true./.false.'
            test_logical_array_codegen = .false.
        end if
        
        call arena%clear()
        
    end function test_logical_array_codegen

    logical function test_nested_array_codegen()
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: code
        integer, allocatable :: indices(:)
        integer :: elem_index, array_index, expr_index

        test_nested_array_codegen = .true.
        print *, 'Testing array with expressions code generation...'

        arena = create_ast_arena()
        
        ! Create array [a + b, c * 2]
        allocate(indices(2))
        
        ! First element: a + b
        elem_index = push_identifier(arena, "a", 1, 1)
        expr_index = push_identifier(arena, "b", 1, 1)
        elem_index = push_binary_op(arena, elem_index, expr_index, "+")
        indices(1) = elem_index
        
        ! Second element: c * 2
        elem_index = push_identifier(arena, "c", 1, 1)
        expr_index = push_literal(arena, "2", LITERAL_INTEGER, 1, 1)
        elem_index = push_binary_op(arena, elem_index, expr_index, "*")
        indices(2) = elem_index
        
        array_index = push_array_literal(arena, indices, 1, 1)
        
        ! Generate code
        code = generate_code_from_arena(arena, array_index)
        
        ! Arrays with expressions should show the elements
        if (index(code, "a") > 0 .and. index(code, "b") > 0 .and. &
            index(code, "c") > 0 .and. index(code, "2") > 0) then
            print *, '  PASS: Array with expressions generated: "', trim(code), '"'
        else
            print *, '  FAIL: Array should contain expressions, got "', trim(code), '"'
            test_nested_array_codegen = .false.
        end if
        
        call arena%clear()
        
    end function test_nested_array_codegen

    logical function test_array_in_assignment()
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: code
        integer, allocatable :: indices(:)
        integer :: i, elem_index, array_index, assign_index

        test_array_in_assignment = .true.
        print *, 'Testing array literal in assignment...'

        arena = create_ast_arena()
        
        ! Create array [1, 2, 3]
        allocate(indices(3))
        indices(1) = push_literal(arena, "1", LITERAL_INTEGER, 1, 1)
        indices(2) = push_literal(arena, "2", LITERAL_INTEGER, 1, 1)
        indices(3) = push_literal(arena, "3", LITERAL_INTEGER, 1, 1)
        array_index = push_array_literal(arena, indices, 1, 1)
        
        ! Create assignment: arr = [1, 2, 3]
        elem_index = push_identifier(arena, "arr", 1, 1)
        assign_index = push_assignment(arena, elem_index, array_index)
        
        ! Generate code
        code = generate_code_from_arena(arena, assign_index)
        
        if (index(code, "arr =") > 0 .and. &
            (index(code, "[1, 2, 3]") > 0 .or. index(code, "(/ 1, 2, 3 /)") > 0)) then
            print *, '  PASS: Array assignment generated: "', trim(code), '"'
        else
            print *, '  FAIL: Expected "arr = [1, 2, 3]", got "', trim(code), '"'
            test_array_in_assignment = .false.
        end if
        
        call arena%clear()
        
    end function test_array_in_assignment

    logical function test_array_constructor_style()
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: code
        integer, allocatable :: indices(:)
        integer :: elem_index, array_index

        test_array_constructor_style = .true.
        print *, 'Testing array constructor style preference...'

        arena = create_ast_arena()
        
        ! Create simple array for testing constructor style
        allocate(indices(2))
        elem_index = push_literal(arena, "10", LITERAL_INTEGER, 1, 1)
        indices(1) = elem_index
        elem_index = push_literal(arena, "20", LITERAL_INTEGER, 1, 1)
        indices(2) = elem_index
        array_index = push_array_literal(arena, indices, 1, 1)
        
        ! Generate code
        code = generate_code_from_arena(arena, array_index)
        
        ! We should generate standard Fortran array constructor syntax
        if (code == "(/ 10, 20 /)" .or. code == "[10, 20]") then
            print *, '  PASS: Array constructor style: "', trim(code), '"'
            print *, '  INFO: Using ', merge('F2003 style', 'F90 style  ', index(code, '[') > 0)
        else
            print *, '  FAIL: Unexpected array syntax: "', trim(code), '"'
            test_array_constructor_style = .false.
        end if
        
        call arena%clear()
        
    end function test_array_constructor_style

end program test_frontend_codegen_array_literals