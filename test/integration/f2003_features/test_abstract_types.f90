program test_abstract_types
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, "=== Abstract Types and Deferred Procedures Tests ==="
    print *

    if (.not. test_abstract_type_basic()) all_passed = .false.
    if (.not. test_abstract_interface()) all_passed = .false.
    if (.not. test_program_abstract_interface()) all_passed = .false.
    if (.not. test_type_extension()) all_passed = .false.

    print *
    if (all_passed) then
        print *, "All abstract type tests passed!"
        stop 0
    else
        print *, "Some abstract type tests failed!"
        stop 1
    end if

contains
    subroutine set_source_from_lines(target, lines)
        character(len=:), allocatable, intent(out) :: target
        character(len=*), intent(in) :: lines(:)
        integer :: idx

        target = ""
        do idx = 1, size(lines)
            if (idx == 1) then
                target = lines(idx)
            else
                target = target // new_line('a') // lines(idx)
            end if
        end do
    end subroutine set_source_from_lines

    subroutine build_abstract_type_source(source)
        character(len=:), allocatable, intent(out) :: source

        call set_source_from_lines(source, [character(len=96) :: &
            "module myclass_base", &
            "use, intrinsic :: iso_fortran_env, only: dp => real64", &
            "implicit none", &
            "", &
            "type, abstract :: myclass_t", &
            "contains", &
            "    procedure(get_value_i), deferred :: get_value", &
            "end type myclass_t", &
            "", &
            "abstract interface", &
            "    subroutine get_value_i(self, value)", &
            "        import myclass_t", &
            "        class(myclass_t), intent(in) :: self", &
            "        real(dp), intent(out) :: value", &
            "    end subroutine get_value_i", &
            "end interface", &
            "", &
            "end module myclass_base"])
    end subroutine build_abstract_type_source

    subroutine build_abstract_interface_source(source)
        character(len=:), allocatable, intent(out) :: source

        call set_source_from_lines(source, [character(len=96) :: &
            "abstract interface", &
            "    subroutine test_sub(x)", &
            "        real, intent(in) :: x", &
            "    end subroutine test_sub", &
            "end interface"])
    end subroutine build_abstract_interface_source

    subroutine build_program_abstract_interface_source(source)
        character(len=:), allocatable, intent(out) :: source

        call set_source_from_lines(source, [character(len=96) :: &
            "program test_abstract_interface", &
            "implicit none", &
            "", &
            "abstract interface", &
            "    function func_template(x) result(y)", &
            "        real, intent(in) :: x", &
            "        real :: y", &
            "    end function func_template", &
            "end interface", &
            "", &
            "procedure(func_template), pointer :: fptr", &
            "print *, ""Testing abstract interface""", &
            "", &
            "end program test_abstract_interface"])
    end subroutine build_program_abstract_interface_source

    subroutine build_type_extension_source(source)
        character(len=:), allocatable, intent(out) :: source

        call set_source_from_lines(source, [character(len=96) :: &
            "module myclass_impl", &
            "use, intrinsic :: iso_fortran_env, only: dp => real64", &
            "implicit none", &
            "type :: base_t", &
            "    integer :: x", &
            "end type base_t", &
            "", &
            "type, extends(base_t) :: derived_t", &
            "    real(dp) :: y", &
            "contains", &
            "    procedure :: get_value => get_value_impl", &
            "end type derived_t", &
            "", &
            "contains", &
            "", &
            "subroutine get_value_impl(self, value)", &
            "    class(derived_t), intent(in) :: self", &
            "    real(dp), intent(out) :: value", &
            "    value = 1.0d0", &
            "end subroutine get_value_impl", &
            "", &
            "end module myclass_impl"])
    end subroutine build_type_extension_source

    subroutine check_contains(text, snippet, message, passed)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: snippet
        character(len=*), intent(in) :: message
        logical, intent(inout) :: passed

        if (index(text, snippet) == 0) then
            print *, "  ERROR: " // trim(message)
            passed = .false.
        end if
    end subroutine check_contains

    function test_abstract_type_basic() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        call build_abstract_type_source(source)
        passed = .true.
        print *, "Testing abstract type with deferred procedure..."

        call transform_lazy_fortran_string(source, output, error_msg)

        if (error_msg /= "") then
            print *, "  ERROR: Failed to parse:", trim(error_msg)
            passed = .false.
        else if (.not. allocated(output)) then
            print *, "  ERROR: No output generated"
            passed = .false.
        else
            call check_contains(output, "type, abstract", &
                "abstract attribute missing", passed)
            call check_contains(output, "procedure(get_value_i), deferred", &
                "procedure interface reference missing", passed)
            if (passed) then
                print *, "  PASS: Abstract type with deferred procedure"
            end if
        end if
    end function test_abstract_type_basic

    function test_abstract_interface() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        call build_abstract_interface_source(source)
        passed = .true.
        print *, "Testing abstract interface block..."

        call transform_lazy_fortran_string(source, output, error_msg)

        if (error_msg /= "") then
            print *, "  ERROR: Parsing failed: ", trim(error_msg)
            passed = .false.
        else if (.not. allocated(output)) then
            print *, "  ERROR: No output generated"
            passed = .false.
        else
            if (index(output, "abstract interface") == 0) then
                print *, "  ERROR: abstract keyword missing from output"
                passed = .false.
            else
                print *, "  PASS: Abstract interface block"
                passed = .true.
            end if
        end if
    end function test_abstract_interface

    function test_program_abstract_interface() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        call build_program_abstract_interface_source(source)
        passed = .true.
        print *, "Testing abstract interface inside program..."

        call transform_lazy_fortran_string(source, output, error_msg)

        if (error_msg /= "") then
            print *, "  ERROR: Parsing failed: ", trim(error_msg)
            passed = .false.
        else if (.not. allocated(output)) then
            print *, "  ERROR: No output generated"
            passed = .false.
        else
            call check_contains(output, "abstract interface", &
                "abstract keyword missing from program output", &
                passed)
            if (passed) then
                print *, "  PASS: Program abstract interface block"
            end if
        end if
    end function test_program_abstract_interface

    function test_type_extension() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        call build_type_extension_source(source)
        passed = .true.
        print *, "Testing type extension with extends..."

        call transform_lazy_fortran_string(source, output, error_msg)

        if (error_msg /= "") then
            print *, "  ERROR: Failed to parse:", trim(error_msg)
            passed = .false.
        else if (.not. allocated(output)) then
            print *, "  ERROR: No output generated"
            passed = .false.
        else
            call check_contains(output, "extends(base_t)", &
                "extends attribute missing", passed)
            call check_contains(output, "procedure :: get_value => get_value_impl", &
                "procedure binding missing", passed)
            if (passed) then
                print *, "  PASS: Type extension with extends"
            end if
        end if
    end function test_type_extension

end program test_abstract_types
