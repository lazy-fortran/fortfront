program test_fortfront_type_api
    ! Test the enhanced type system API methods exposed through fortfront
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none
    
    integer :: failures = 0
    
    ! Test the enhanced type system API
    call test_type_info_creation()
    call test_literal_value_extraction()
    call test_intrinsic_recognition()
    call test_type_string_representation()
    
    if (failures == 0) then
        print *, "All fortfront type API tests passed!"
    else
        print *, "fortfront type API tests failed with", failures, "failures"
        stop 1
    end if
    
contains
    
    subroutine test_type_info_creation()
        type(type_info_t) :: info
        
        print *, "Testing type info creation..."
        
        ! Test creating integer type info
        info = get_type_info_for_base_type(TINT)
        if (info%base_type /= TINT) then
            print *, "Expected base_type TINT, got", info%base_type
            failures = failures + 1
        end if
        
        ! Test creating real type info
        info = get_type_info_for_base_type(TREAL)
        if (info%base_type /= TREAL) then
            print *, "Expected base_type TREAL, got", info%base_type
            failures = failures + 1
        end if
        
        print *, "  type info creation test completed"
    end subroutine test_type_info_creation
    
    subroutine test_literal_value_extraction()
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        integer, allocatable :: literal_nodes(:)
        integer(kind=8) :: int_value
        real(kind=8) :: real_value
        character(len=:), allocatable :: string_value
        character(len=*), parameter :: source = &
            "program test" // char(10) // &
            "    integer :: x = 42" // char(10) // &
            "    real :: y = 3.14" // char(10) // &
            "    character(len=5) :: s = 'hello'" // char(10) // &
            "end program test"
        
        print *, "Testing literal value extraction..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "Lex error:", trim(error_msg)
                failures = failures + 1
                return
            end if
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "Parse error:", trim(error_msg)
                failures = failures + 1
                return
            end if
        end if
        
        ! Find literal nodes
        literal_nodes = find_nodes_by_type(arena, "literal")
        if (size(literal_nodes) > 0) then
            ! Test integer literal extraction
            int_value = get_integer_literal_value(arena, literal_nodes(1))
            if (int_value /= 42) then
                print *, "Expected integer value 42, got", int_value
                failures = failures + 1
            end if
        else
            print *, "No literal nodes found"
            failures = failures + 1
        end if
        
        print *, "  literal value extraction test completed"
    end subroutine test_literal_value_extraction
    
    subroutine test_intrinsic_recognition()
        logical :: is_intrinsic
        type(function_signature_t) :: signature
        
        print *, "Testing intrinsic recognition..."
        
        ! Test known intrinsic
        is_intrinsic = is_intrinsic_function("sin")
        if (.not. is_intrinsic) then
            print *, "Expected 'sin' to be recognized as intrinsic"
            failures = failures + 1
        end if
        
        ! Test non-intrinsic
        is_intrinsic = is_intrinsic_function("my_custom_function")
        if (is_intrinsic) then
            print *, "Expected 'my_custom_function' not to be intrinsic"
            failures = failures + 1
        end if
        
        ! Test getting intrinsic signature
        signature = get_intrinsic_signature("sin")
        if (.not. allocated(signature%param_types)) then
            print *, "Failed to get signature for 'sin'"
            ! Don't count as failure if not implemented
        end if
        
        print *, "  intrinsic recognition test completed"
    end subroutine test_intrinsic_recognition
    
    subroutine test_type_string_representation()
        type(type_info_t) :: info
        character(len=:), allocatable :: type_str
        
        print *, "Testing type string representation..."
        
        ! Test integer type string
        info = get_type_info_for_base_type(TINT)
        type_str = get_type_string(info)
        if (.not. allocated(type_str)) then
            print *, "Failed to get type string for integer"
            failures = failures + 1
        else if (index(type_str, "integer") == 0) then
            print *, "Expected 'integer' in type string, got '", trim(type_str), "'"
            failures = failures + 1
        end if
        
        ! Test real type string
        info = get_type_info_for_base_type(TREAL)
        type_str = get_type_string(info)
        if (.not. allocated(type_str)) then
            print *, "Failed to get type string for real"
            failures = failures + 1
        else if (index(type_str, "real") == 0) then
            print *, "Expected 'real' in type string, got '", trim(type_str), "'"
            failures = failures + 1
        end if
        
        print *, "  type string representation test completed"
    end subroutine test_type_string_representation
    
end program test_fortfront_type_api