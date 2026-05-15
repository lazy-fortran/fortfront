program test_issue_1353_program_derived_type
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_api, only: lex_source
    use transformation_api, only: transform_lazy_fortran_string
    use lexer_core, only: token_t, TK_KEYWORD, to_lower
    implicit none

    call test_type_definition_inside_program()
    call test_type_definition_with_attributes()
    call test_module_type_definition()
    call test_class_type_declaration()
    call test_type_definition_with_extends()
    print *, ""
    print *, "All tests passed for issue 1353."

contains

    include '../common/read_example.inc'

    logical function has_real_decl(segment, name) result(found)
        character(len=*), intent(in) :: segment
        character(len=*), intent(in) :: name

        found = index(segment, "real :: "//name) > 0 .or. &
                index(segment, "real(dp) :: "//name) > 0 .or. &
                index(segment, "real(8) :: "//name) > 0
    end function has_real_decl

    subroutine test_type_definition_inside_program()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: normalized_output
        integer :: idx_type, idx_end, idx_decl
        integer :: header_count

        call read_example('examples/f90/issue_1353_derived_type.f90', input_code)

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: unexpected error:", trim(error_msg)
            error stop 1
        end if

        idx_type = index(output_code, "type :: point_t")
        if (idx_type <= 0) then
            print *, "FAIL: derived type definition missing"
            error stop 1
        end if

        idx_end = index(output_code, "end program test_derived_type")
        if (idx_end <= 0) then
            print *, "FAIL: end program statement missing"
            error stop 1
        end if

        if (idx_type > idx_end) then
            print *, "FAIL: type definition emitted after program end"
            error stop 1
        end if

        idx_decl = index(output_code, "type(point_t) :: p")
        if (idx_decl <= 0) then
            print *, "FAIL: derived type variable declaration missing"
            error stop 1
        end if

        if (index(output_code, "p%x = 1.0") <= 0) then
            if (index(output_code, "p%x = 1.0d0") <= 0) then
                print *, "FAIL: assignment to p%x missing"
                error stop 1
            end if
        end if

        if (index(output_code, "print *, p%x, p%y") <= 0) then
            print *, "FAIL: print statement for p missing"
            error stop 1
        end if

        normalized_output = to_lower(output_code)
        header_count = count_occurrences(normalized_output, "type :: point_t")
        if (header_count /= 1) then
            print *, "FAIL: duplicate derived type header detected"
            error stop 1
        end if

        print *, "PASS: Derived type stays inside program with correct declarations"
    end subroutine test_type_definition_inside_program

    subroutine test_type_definition_with_attributes()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: type_segment
        character(len=:), allocatable :: normalized_output
        integer :: idx_type, idx_end, idx_decl
        integer :: header_count

        call read_example('examples/f90/issue_1353_type_public_attribute.f90', &
                          input_code)

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: unexpected error for attribute test:", trim(error_msg)
            error stop 1
        end if

        idx_type = index(output_code, "type, bind(c) :: point_t")
        if (idx_type <= 0) then
            print *, "FAIL: attribute test missing derived type definition"
            error stop 1
        end if

        idx_end = index(output_code, "end type point_t")
        if (idx_end <= 0) then
            print *, "FAIL: attribute test missing end type statement"
            error stop 1
        end if

        if (idx_end <= idx_type) then
            print *, "FAIL: attribute test emitted end type before type header"
            error stop 1
        end if

        normalized_output = to_lower(output_code)
        header_count = count_occurrences(normalized_output, "type, bind(c) :: point_t")
        if (header_count /= 1) then
            print *, "FAIL: duplicate derived type header detected"
            error stop 1
        end if

        type_segment = output_code(idx_type:idx_end)
        if (index(type_segment, "type(point_t) ::") > 0) then
            print *, "FAIL: derived type variable declared inside type definition"
            error stop 1
        end if

        idx_decl = index(output_code, "type(point_t) :: p")
        if (idx_decl <= 0) then
            print *, "FAIL: attribute test missing type variable declaration"
            error stop 1
        end if

        if (idx_decl < idx_end) then
            print *, "FAIL: attribute test left declaration inside type"
            error stop 1
        end if

        print *, "PASS: Derived type attributes keep declarations outside type"
    end subroutine test_type_definition_with_attributes

    subroutine test_module_type_definition()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: type_segment
        character(len=:), allocatable :: normalized_output
        integer :: idx_type, idx_end, idx_contains
        integer :: header_count

        call read_example('examples/f90/issue_1353_module_type_definition.f90', &
                          input_code)

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: unexpected error for module type:", trim(error_msg)
            error stop 1
        end if

        idx_type = index(output_code, "type, public :: point_t")
        if (idx_type <= 0) then
            print *, "FAIL: module output missing derived type header"
            error stop 1
        end if

        idx_end = index(output_code, "end type point_t")
        if (idx_end <= 0) then
            print *, "FAIL: module output missing end type statement"
            error stop 1
        end if

        if (idx_type >= idx_end) then
            print *, "FAIL: module emitted end type before type header"
            error stop 1
        end if

        if (idx_type > 1) then
            if (has_real_decl(output_code(1:idx_type - 1), "x") .or. &
                has_real_decl(output_code(1:idx_type - 1), "y")) then
                print *, "FAIL: module leaked type components before definition"
                error stop 1
            end if
        end if

        type_segment = output_code(idx_type:idx_end)
        if (index(type_segment, "type(point_t) ::") > 0) then
            print *, "FAIL: module placed type variable inside definition"
            error stop 1
        end if

        if ((.not. has_real_decl(type_segment, "x")) .or. &
            (.not. has_real_decl(type_segment, "y"))) then
            print *, "FAIL: module missing component declarations inside type"
            error stop 1
        end if

        idx_contains = index(output_code, "contains")
        if (idx_contains <= 0) then
            print *, "FAIL: module missing contains section"
            error stop 1
        end if

        if (idx_contains <= idx_end) then
            print *, "FAIL: contains section appears before end type"
            error stop 1
        end if

        if (index(output_code, "type(point_t) :: p") <= 0) then
            print *, "FAIL: module lost type variable declaration in subroutine"
            error stop 1
        end if

        if (index(output_code, "p%x = 1.0") <= 0 .and. &
            index(output_code, "p%x = 1.0d0") <= 0) then
            print *, "FAIL: module lost assignment to p%x"
            error stop 1
        end if

        normalized_output = to_lower(output_code)
        header_count = count_occurrences(normalized_output, "type, public :: point_t")
        if (header_count /= 1) then
            print *, "FAIL: module emitted duplicate derived type headers"
            error stop 1
        end if

        print *, "PASS: Module type definition preserved correctly"
    end subroutine test_module_type_definition

    subroutine test_type_definition_with_extends()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: type_segment
        character(len=:), allocatable :: normalized_output
        integer :: idx_type, idx_end
        integer :: header_count

        call read_example('examples/f90/issue_1353_extends_attribute.f90', input_code)

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: unexpected error for extends test:", trim(error_msg)
            error stop 1
        end if

        idx_type = index(output_code, "type, extends(base_t) :: point_t")
        if (idx_type <= 0) then
            print *, "FAIL: extends attribute missing from derived type header"
            error stop 1
        end if

        idx_end = index(output_code, "end type point_t")
        if (idx_end <= idx_type) then
            print *, "FAIL: extends test has malformed type block"
            error stop 1
        end if

        type_segment = output_code(idx_type:idx_end)
        if (.not. has_real_decl(type_segment, "y")) then
            print *, "FAIL: extends test lost component declaration inside type"
            error stop 1
        end if

        normalized_output = to_lower(output_code)
        header_count = count_occurrences(normalized_output, &
                                         "type, extends(base_t) :: point_t")
        if (header_count /= 1) then
            print *, "FAIL: extends test produced duplicate derived type headers"
            error stop 1
        end if

        if (index(output_code, "type(point_t) :: p") <= 0) then
            print *, "FAIL: extends test lost type usage outside definition"
            error stop 1
        end if

        print *, "PASS: Derived type with extends attribute preserved"
    end subroutine test_type_definition_with_extends

    subroutine test_class_type_declaration()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: lex_error
        integer :: idx_class_decl, i
        logical :: class_token_found

        call read_example('examples/f90/issue_1353_class_pointer_declaration.f90', &
                          input_code)

        call lex_source(input_code, tokens, lex_error)

        if (len_trim(lex_error) > 0) then
            print *, "FAIL: lexer reported error for class declaration:", &
                trim(lex_error)
            error stop 1
        end if

        class_token_found = .false.
        if (allocated(tokens)) then
            do i = 1, size(tokens)
                if (trim(tokens(i)%text) == "class") then
                    class_token_found = .true.
                    if (tokens(i)%kind /= TK_KEYWORD) then
                        print *, "FAIL: class token not lexed as keyword"
                        error stop 1
                    end if
                    exit
                end if
            end do
        end if

        if (.not. class_token_found) then
            print *, "FAIL: class keyword missing from token stream"
            error stop 1
        end if

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: unexpected error for class declaration:", trim(error_msg)
            error stop 1
        end if

        idx_class_decl = index(output_code, "class(base_t), pointer :: p")
        if (idx_class_decl <= 0) then
            print *, "FAIL: class declaration missing or malformed"
            error stop 1
        end if

        if (index(output_code, "real(dp) :: p") > 0 .or. &
            index(output_code, "real(8) :: p") > 0 .or. &
            index(output_code, "real :: p") > 0) then
            print *, "FAIL: class declaration downgraded to real"
            error stop 1
        end if

        if (index(output_code, "p => storage") <= 0) then
            print *, "FAIL: pointer assignment was removed"
            error stop 1
        end if

        print *, "PASS: Class declaration preserved with pointer attribute"
    end subroutine test_class_type_declaration

    integer function count_occurrences(text, pattern) result(count)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        integer :: start_pos
        integer :: found_pos
        integer :: pattern_len

        count = 0
        pattern_len = len(pattern)
        if (pattern_len == 0) return

        start_pos = 1
        do
            if (start_pos > len(text)) exit
            found_pos = index(text(start_pos:), pattern)
            if (found_pos <= 0) exit
            count = count + 1
            start_pos = start_pos + found_pos + pattern_len - 1
        end do
    end function count_occurrences


end program test_issue_1353_program_derived_type
