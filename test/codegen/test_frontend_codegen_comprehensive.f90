program test_frontend_codegen_comprehensive
    ! Comprehensive frontend code generation test covering:
    ! - Basic constructs (literals, identifiers, assignments)
    ! - Expressions (binary ops, array literals, allocatable)
    ! - Program structure (programs, subroutines, functions)
    ! - Advanced features (no duplication, API integration)
    use ast_core
    use ast_factory
    use codegen_core
    use fortfront
    implicit none

    integer :: test_count = 0, tests_passed = 0

    print *, "=== Comprehensive Frontend Code Generation Tests ==="

    ! Test all codegen scenarios
    call test_basic_constructs()
    call test_expressions()
    call test_array_literals()
    call test_assignments()
    call test_program_structure()
    call test_allocatable_handling()
    call test_no_duplication()
    call test_api_integration()

    ! Report results
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", tests_passed, "/", test_count, " tests"
    
    if (tests_passed == test_count) then
        print *, "All frontend code generation tests passed!"
        stop 0
    else
        print *, "Some frontend code generation tests failed!"
        stop 1
    end if

contains

    subroutine test_basic_constructs()
        type(ast_arena_t) :: arena
        integer :: lit_index, id_index
        character(len=:), allocatable :: code

        call test_start("Basic constructs (literals, identifiers)")

        arena = create_ast_arena()

        ! Test integer literal
        lit_index = push_literal(arena, "42", LITERAL_INTEGER, 1, 1)
        code = generate_code_from_arena(arena, lit_index)
        
        if (code /= "42") then
            call test_fail("Integer literal generation incorrect: expected '42', got '" // code // "'")
            return
        end if

        ! Test identifier
        id_index = push_identifier(arena, "test_var", 1, 1)
        code = generate_code_from_arena(arena, id_index)
        
        if (code /= "test_var") then
            call test_fail("Identifier generation incorrect: expected 'test_var', got '" // code // "'")
            return
        end if

        call test_pass()
    end subroutine test_basic_constructs

    subroutine test_expressions()
        call test_start("Expression code generation")

        block
            character(len=:), allocatable :: input, output, error_msg

            input = 'program test' // new_line('a') // &
                   '    real :: x, y, result' // new_line('a') // &
                   '    x = 2.5' // new_line('a') // &
                   '    y = 3.0' // new_line('a') // &
                   '    result = x * y + 1.0' // new_line('a') // &
                   'end program test'

            call transform_lazy_fortran_string(input, output, error_msg)

            call assert_no_error(error_msg)
            call assert_contains(output, "x*y + 1.0", "Binary expression preserved")
            call assert_contains(output, "result =", "Assignment preserved")
        end block

        call test_pass()
    end subroutine test_expressions

    subroutine test_array_literals()
        call test_start("Array literal code generation")

        block
            character(len=:), allocatable :: input, output, error_msg

            input = 'program test' // new_line('a') // &
                   '    integer :: arr(3)' // new_line('a') // &
                   '    arr = [1, 2, 3]' // new_line('a') // &
                   '    real :: matrix(2, 2)' // new_line('a') // &
                   '    matrix = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])' // new_line('a') // &
                   'end program test'

            call transform_lazy_fortran_string(input, output, error_msg)

            call assert_no_error(error_msg)
            call assert_contains(output, "[1, 2, 3]", "Integer array literal preserved")
            call assert_contains(output, "reshape", "Array constructor preserved")
        end block

        call test_pass()
    end subroutine test_array_literals

    subroutine test_assignments()
        call test_start("Assignment statement generation")

        block
            character(len=:), allocatable :: input, output, error_msg

            input = 'program test' // new_line('a') // &
                   '    integer :: i, j' // new_line('a') // &
                   '    real :: x, y' // new_line('a') // &
                   '    i = 10' // new_line('a') // &
                   '    j = i + 5' // new_line('a') // &
                   '    x = real(i)' // new_line('a') // &
                   '    y = x ** 2' // new_line('a') // &
                   'end program test'

            call transform_lazy_fortran_string(input, output, error_msg)

            call assert_no_error(error_msg)
            call assert_contains(output, "i = 10", "Simple assignment preserved")
            call assert_contains(output, "j = i + 5", "Expression assignment preserved")
            call assert_contains(output, "x**2", "Exponentiation preserved")
        end block

        call test_pass()
    end subroutine test_assignments

    subroutine test_program_structure()
        call test_start("Program structure generation")

        block
            character(len=:), allocatable :: input, output, error_msg

            input = 'program main' // new_line('a') // &
                   '    implicit none' // new_line('a') // &
                   '    integer :: result' // new_line('a') // &
                   '    result = add_numbers(5, 3)' // new_line('a') // &
                   '    print *, result' // new_line('a') // &
                   'contains' // new_line('a') // &
                   '    function add_numbers(a, b) result(sum)' // new_line('a') // &
                   '        integer, intent(in) :: a, b' // new_line('a') // &
                   '        integer :: sum' // new_line('a') // &
                   '        sum = a + b' // new_line('a') // &
                   '    end function add_numbers' // new_line('a') // &
                   'end program main'

            call transform_lazy_fortran_string(input, output, error_msg)

            call assert_no_error(error_msg)
            call assert_contains(output, "program main", "Program declaration preserved")
            call assert_contains(output, "function add_numbers", "Function declaration preserved")
            call assert_contains(output, "contains", "Contains section preserved")
        end block

        call test_pass()
    end subroutine test_program_structure

    subroutine test_allocatable_handling()
        call test_start("Allocatable variable handling")

        block
            character(len=:), allocatable :: input, output, error_msg

            input = 'program test' // new_line('a') // &
                   '    integer, allocatable :: arr(:)' // new_line('a') // &
                   '    character(len=:), allocatable :: str' // new_line('a') // &
                   '    allocate(arr(10))' // new_line('a') // &
                   '    str = "hello world"' // new_line('a') // &
                   '    arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]' // new_line('a') // &
                   'end program test'

            call transform_lazy_fortran_string(input, output, error_msg)

            call assert_no_error(error_msg)
            call assert_contains(output, "allocatable", "Allocatable attribute preserved")
            call assert_contains(output, '"hello world"', "String literal preserved")
        end block

        call test_pass()
    end subroutine test_allocatable_handling

    subroutine test_no_duplication()
        call test_start("No duplication in code generation")

        block
            character(len=:), allocatable :: input, output, error_msg
            integer :: program_count, end_program_count

            input = 'program test' // new_line('a') // &
                   '    integer :: x' // new_line('a') // &
                   '    x = 1' // new_line('a') // &
                   'end program test'

            call transform_lazy_fortran_string(input, output, error_msg)

            call assert_no_error(error_msg)

            ! Count occurrences of program declarations
            program_count = count_occurrences(output, "program test" // new_line('a'))
            end_program_count = count_occurrences(output, "end program")

            if (program_count > 1) then
                call test_fail("Duplicate program declarations found")
                return
            end if

            if (end_program_count > 1) then
                call test_fail("Duplicate end program statements found")
                return
            end if
        end block

        call test_pass()
    end subroutine test_no_duplication

    subroutine test_api_integration()
        call test_start("API integration")

        block
            character(len=:), allocatable :: input, output, error_msg

            ! Test complete transformation through API
            input = 'program api_test' // new_line('a') // &
                   '    implicit none' // new_line('a') // &
                   '    integer :: i' // new_line('a') // &
                   '    do i = 1, 5' // new_line('a') // &
                   '        print *, i' // new_line('a') // &
                   '    end do' // new_line('a') // &
                   'end program api_test'

            call transform_lazy_fortran_string(input, output, error_msg)

            call assert_no_error(error_msg)
            call assert_contains(output, "integer :: i", "Variable declaration preserved")
            call assert_contains(output, "print *, i", "Print statement preserved")
        end block

        call test_pass()
    end subroutine test_api_integration

    ! Helper functions
    function count_occurrences(text, pattern) result(count)
        character(len=*), intent(in) :: text, pattern
        integer :: count, pos, start
        
        count = 0
        start = 1
        do
            pos = index(text(start:), pattern)
            if (pos == 0) exit
            count = count + 1
            start = start + pos + len(pattern) - 1
        end do
    end function count_occurrences

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A,A)', advance='no') "Testing ", test_name, "... "
    end subroutine test_start

    subroutine test_pass()
        tests_passed = tests_passed + 1
        print *, "PASS"
    end subroutine test_pass

    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, "FAIL: ", reason
    end subroutine test_fail

    subroutine assert_no_error(error_msg)
        character(len=:), allocatable, intent(in) :: error_msg
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            call test_fail("Unexpected error: " // trim(error_msg))
        end if
    end subroutine assert_no_error

    subroutine assert_contains(text, pattern, description)
        character(len=*), intent(in) :: text, pattern, description
        if (index(text, pattern) == 0) then
            call test_fail(description // " - expected to find '" // pattern // "'")
        end if
    end subroutine assert_contains

end program test_frontend_codegen_comprehensive