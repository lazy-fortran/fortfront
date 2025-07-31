program test_frontend_codegen_type_declarations
    use ast_core
    use ast_factory
    use codegen_core
    use type_system_hm
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_integer_declaration()
    call test_real_declaration()
    call test_character_declaration()
    call test_array_declaration()
    call test_declaration_with_inference()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_integer_declaration()
        type(ast_arena_t) :: arena
        type(declaration_node) :: decl
        type(mono_type_t) :: int_type
        integer :: decl_index
        character(len=:), allocatable :: code

        test_count = test_count + 1

        ! Create arena
        arena = create_ast_arena()

        ! Create integer declaration with inferred type
        decl%var_name = "count"
        decl%type_name = "integer"
        decl%has_kind = .false.
        decl%line = 1
        decl%column = 1

        ! Set inferred type
        int_type = create_mono_type(TINT)
        allocate (decl%inferred_type, source=int_type)

        ! Push to arena
        call arena%push(decl, "declaration")
        decl_index = arena%current_index

        ! Generate code
        code = generate_code_from_arena(arena, decl_index)

        if (code == "integer :: count") then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Integer declaration"
        else
            write (*, '(A)') "FAIL: Integer declaration"
            write (*, '(A,A)') "  Expected: integer :: count"
            write (*, '(A,A)') "  Got:      ", code
        end if
    end subroutine test_integer_declaration

    subroutine test_real_declaration()
        type(ast_arena_t) :: arena
        type(declaration_node) :: decl
        type(mono_type_t) :: real_type
        integer :: decl_index
        character(len=:), allocatable :: code

        test_count = test_count + 1

        ! Create arena
        arena = create_ast_arena()

        ! Create real declaration with inferred type
        decl%var_name = "temperature"
        decl%type_name = "real"
        decl%has_kind = .true.
        decl%kind_value = 8
        decl%line = 1
        decl%column = 1

        ! Set inferred type
        real_type = create_mono_type(TREAL)
        allocate (decl%inferred_type, source=real_type)

        ! Push to arena
        call arena%push(decl, "declaration")
        decl_index = arena%current_index

        ! Generate code
        code = generate_code_from_arena(arena, decl_index)

        if (code == "real(8) :: temperature") then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Real declaration with kind"
        else
            write (*, '(A)') "FAIL: Real declaration with kind"
            write (*, '(A,A)') "  Expected: real(8) :: temperature"
            write (*, '(A,A)') "  Got:      ", code
        end if
    end subroutine test_real_declaration

    subroutine test_character_declaration()
        type(ast_arena_t) :: arena
        type(declaration_node) :: decl
        type(mono_type_t) :: char_type
        integer :: decl_index
        character(len=:), allocatable :: code

        test_count = test_count + 1

        ! Create arena
        arena = create_ast_arena()

        ! Create character declaration with length
        decl%var_name = "name"
        decl%type_name = "character"
        decl%has_kind = .true.
        decl%kind_value = 20  ! Using kind_value for length
        decl%line = 1
        decl%column = 1

        ! Set inferred type with size
        char_type = create_mono_type(TCHAR, char_size=20)
        allocate (decl%inferred_type, source=char_type)

        ! Push to arena
        call arena%push(decl, "declaration")
        decl_index = arena%current_index

        ! Generate code
        code = generate_code_from_arena(arena, decl_index)

        if (code == "character(len=20) :: name") then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Character declaration with length"
        else
            write (*, '(A)') "FAIL: Character declaration with length"
            write (*, '(A,A)') "  Expected: character(len=20) :: name"
            write (*, '(A,A)') "  Got:      ", code
        end if
    end subroutine test_character_declaration

    subroutine test_array_declaration()
        type(ast_arena_t) :: arena
        type(declaration_node) :: decl
        type(literal_node) :: dim
        integer :: decl_index, dim_index
        character(len=:), allocatable :: code

        test_count = test_count + 1

        ! Create arena
        arena = create_ast_arena()

        ! Create dimension literal
        dim%value = "100"
        dim%literal_kind = LITERAL_INTEGER
        call arena%push(dim, "literal")
        dim_index = arena%current_index

        ! Create array declaration
        decl%var_name = "data"
        decl%type_name = "real"
        decl%has_kind = .true.
        decl%kind_value = 8
        decl%is_array = .true.
        decl%line = 1
        decl%column = 1

        ! Add dimension indices
        allocate (decl%dimension_indices(1))
        decl%dimension_indices(1) = dim_index

        ! Set inferred type
        allocate (decl%inferred_type)
        decl%inferred_type = create_mono_type(TREAL)

        ! Push declaration to arena
        call arena%push(decl, "declaration")
        decl_index = arena%current_index

        ! Generate code
        code = generate_code_from_arena(arena, decl_index)

        if (code == "real(8) :: data(100)") then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Array declaration"
        else
            write (*, '(A)') "FAIL: Array declaration"
            write (*, '(A,A)') "  Expected: real(8) :: data(100)"
            write (*, '(A,A)') "  Got:      ", code
        end if
    end subroutine test_array_declaration

    subroutine test_declaration_with_inference()
        type(ast_arena_t) :: arena
        type(declaration_node) :: decl
        type(literal_node) :: init_val
        type(mono_type_t) :: inferred_type
        integer :: decl_index, init_index
        character(len=:), allocatable :: code

        test_count = test_count + 1

        ! Create arena
        arena = create_ast_arena()

        ! Create initializer literal
        init_val%value = "3.14159"
        init_val%literal_kind = LITERAL_REAL
        call arena%push(init_val, "literal")
        init_index = arena%current_index

        ! Create declaration without explicit type (will be inferred)
        decl%var_name = "pi"
        decl%type_name = ""  ! Empty - to be inferred
        decl%has_kind = .false.
        decl%line = 1
        decl%column = 1
        decl%has_initializer = .true.
        decl%initializer_index = init_index

        ! Set inferred type
        inferred_type = create_mono_type(TREAL)
        allocate (decl%inferred_type)
        decl%inferred_type = inferred_type

        ! Push declaration to arena
        call arena%push(decl, "declaration")
        decl_index = arena%current_index

        ! Generate code - should generate declaration from inferred type
        code = generate_code_from_arena(arena, decl_index)

        if (code == "real(8) :: pi = 3.14159d0") then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Declaration with type inference"
        else
            write (*, '(A)') "FAIL: Declaration with type inference"
            write (*, '(A,A)') "  Expected: real(8) :: pi = 3.14159d0"
            write (*, '(A,A)') "  Got:      ", code
        end if
    end subroutine test_declaration_with_inference

end program test_frontend_codegen_type_declarations