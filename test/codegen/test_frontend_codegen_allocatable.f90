program test_frontend_codegen_allocatable
    use ast_core
    use ast_factory
    use codegen_core, only: generate_code_from_arena
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Allocatable Declaration Code Generation Tests ==='
    print *

    if (.not. test_allocatable_scalar()) all_passed = .false.
    if (.not. test_allocatable_array()) all_passed = .false.
    if (.not. test_allocatable_with_type()) all_passed = .false.
    if (.not. test_allocatable_character()) all_passed = .false.
    if (.not. test_non_allocatable()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All allocatable codegen tests passed!'
        stop 0
    else
        print *, 'Some allocatable codegen tests failed!'
        stop 1
    end if

contains

    logical function test_allocatable_scalar()
        type(ast_arena_t) :: arena
        type(declaration_node) :: decl
        character(len=:), allocatable :: code
        integer :: decl_index
        
        test_allocatable_scalar = .true.
        print *, 'Testing allocatable scalar declaration...'
        
        arena = create_ast_arena()
        
        ! Create allocatable scalar declaration
        decl%type_name = "real(8)"
        decl%var_name = "scalar_var"
        decl%is_array = .false.
        decl%is_allocatable = .true.
        decl%has_kind = .false.
        decl%initializer_index = 0
        decl%line = 1
        decl%column = 1
        
        call arena%push(decl, "declaration", 0)
        decl_index = arena%size
        if (decl_index <= 0) then
            print *, '  FAIL: Failed to push allocatable scalar declaration to arena'
            test_allocatable_scalar = .false.
            return
        end if
        
        ! Generate code
        code = generate_code_from_arena(arena, decl_index)
        
        ! Check result
        if (index(code, "allocatable") > 0 .and. index(code, "real(8)") > 0 .and. &
            index(code, "scalar_var") > 0) then
            print *, '  PASS: Allocatable scalar generated: "', trim(code), '"'
        else
            print *, '  FAIL: Expected allocatable scalar, got: "', trim(code), '"'
            test_allocatable_scalar = .false.
        end if
        
        call arena%clear()
        
    end function test_allocatable_scalar
    
    logical function test_allocatable_array()
        type(ast_arena_t) :: arena
        type(declaration_node) :: decl
        character(len=:), allocatable :: code
        integer :: decl_index
        
        test_allocatable_array = .true.
        print *, 'Testing allocatable array declaration...'
        
        arena = create_ast_arena()
        
        ! Create allocatable array declaration
        decl%type_name = "integer"
        decl%var_name = "array_var"
        decl%is_array = .true.
        decl%is_allocatable = .true.
        decl%has_kind = .false.
        decl%initializer_index = 0
        decl%line = 1
        decl%column = 1
        
        ! Set dimension to deferred (:)
        allocate(decl%dimension_indices(1))
        decl%dimension_indices(1) = 0  ! 0 means deferred dimension
        
        call arena%push(decl, "declaration", 0)
        decl_index = arena%size
        if (decl_index <= 0) then
            print *, '  FAIL: Failed to push allocatable array declaration to arena'
            test_allocatable_array = .false.
            return
        end if
        
        ! Generate code
        code = generate_code_from_arena(arena, decl_index)
        
        ! Check result
        if (index(code, "allocatable") > 0 .and. index(code, "integer") > 0 .and. &
            index(code, "array_var(:)") > 0) then
            print *, '  PASS: Allocatable array generated: "', trim(code), '"'
        else
            print *, '  FAIL: Expected allocatable array, got: "', trim(code), '"'
            test_allocatable_array = .false.
        end if
        
        call arena%clear()
        
    end function test_allocatable_array
    
    logical function test_allocatable_with_type()
        type(ast_arena_t) :: arena
        type(declaration_node) :: decl
        character(len=:), allocatable :: code
        integer :: decl_index
        
        test_allocatable_with_type = .true.
        print *, 'Testing allocatable with custom type...'
        
        arena = create_ast_arena()
        
        ! Create allocatable with custom type
        decl%type_name = "type(my_type)"
        decl%var_name = "custom_var"
        decl%is_array = .false.
        decl%is_allocatable = .true.
        decl%has_kind = .false.
        decl%initializer_index = 0
        decl%line = 1
        decl%column = 1
        
        call arena%push(decl, "declaration", 0)
        decl_index = arena%size
        if (decl_index <= 0) then
            print *, '  FAIL: Failed to push allocatable custom type declaration to arena'
            test_allocatable_with_type = .false.
            return
        end if
        
        ! Generate code
        code = generate_code_from_arena(arena, decl_index)
        
        ! Check result
        if (index(code, "allocatable") > 0 .and. index(code, "type(my_type)") > 0) then
            print *, '  PASS: Allocatable custom type generated'
        else
            print *, '  FAIL: Expected allocatable custom type'
            test_allocatable_with_type = .false.
        end if
        
        call arena%clear()
        
    end function test_allocatable_with_type
    
    logical function test_allocatable_character()
        type(ast_arena_t) :: arena
        type(declaration_node) :: decl
        character(len=:), allocatable :: code
        integer :: decl_index
        
        test_allocatable_character = .true.
        print *, 'Testing allocatable character array...'
        
        arena = create_ast_arena()
        
        ! Create allocatable character array
        decl%type_name = "character(len=10)"
        decl%var_name = "string_array"
        decl%is_array = .true.
        decl%is_allocatable = .true.
        decl%has_kind = .false.
        decl%initializer_index = 0
        decl%line = 1
        decl%column = 1
        
        ! Set dimension
        allocate(decl%dimension_indices(1))
        decl%dimension_indices(1) = 0
        
        call arena%push(decl, "declaration", 0)
        decl_index = arena%size
        if (decl_index <= 0) then
            print *, '  FAIL: Failed to push allocatable character declaration to arena'
            test_allocatable_character = .false.
            return
        end if
        
        ! Generate code
        code = generate_code_from_arena(arena, decl_index)
        
        ! Check result
        if (index(code, "allocatable") > 0 .and. index(code, "character(len=10)") > 0 .and. &
            index(code, "string_array(:)") > 0) then
            print *, '  PASS: Allocatable character array generated'
        else
            print *, '  FAIL: Expected allocatable character array'
            test_allocatable_character = .false.
        end if
        
        call arena%clear()
        
    end function test_allocatable_character
    
    logical function test_non_allocatable()
        type(ast_arena_t) :: arena
        type(declaration_node) :: decl
        character(len=:), allocatable :: code
        integer :: decl_index
        
        test_non_allocatable = .true.
        print *, 'Testing non-allocatable declaration...'
        
        arena = create_ast_arena()
        
        ! Create non-allocatable declaration
        decl%type_name = "real"
        decl%var_name = "regular_var"
        decl%is_array = .false.
        decl%is_allocatable = .false.
        decl%has_kind = .false.
        decl%initializer_index = 0
        decl%line = 1
        decl%column = 1
        
        call arena%push(decl, "declaration", 0)
        decl_index = arena%size
        if (decl_index <= 0) then
            print *, '  FAIL: Failed to push non-allocatable declaration to arena'
            test_non_allocatable = .false.
            return
        end if
        
        ! Generate code
        code = generate_code_from_arena(arena, decl_index)
        
        ! Check result - should NOT contain allocatable
        if (index(code, "allocatable") == 0 .and. index(code, "real :: regular_var") > 0) then
            print *, '  PASS: Non-allocatable declaration correct'
        else
            print *, '  FAIL: Unexpected allocatable in regular declaration'
            test_non_allocatable = .false.
        end if
        
        call arena%clear()
        
    end function test_non_allocatable

end program test_frontend_codegen_allocatable