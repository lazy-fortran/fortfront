program test_type_string_utils
    use type_string_utils, only: is_character_type_string, mono_type_to_string
    use type_system_unified, only: create_mono_type, mono_type_t, TINT, TREAL, &
                                   TCHAR, TLOGICAL, TARRAY
    implicit none

    integer :: total, passed

    total = 0
    passed = 0

    call expect_true(is_character_type_string("character(len=4)"), &
                     "detects character type prefix")
    call expect_true(is_character_type_string("   CHARACTER(LEN=10)"), &
                     "handles leading spaces and case")
    call expect_true(is_character_type_string("character(kind=4)"), &
                     "accepts kind specifier")
    call expect_false(is_character_type_string("integer(4)"), &
                      "rejects integer type")
    call expect_false(is_character_type_string("char(len=4)"), &
                      "rejects partial match")
    call expect_false(is_character_type_string(""), "rejects empty input")

    call test_mono_type_to_string_cases()

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0)') "Passed ", passed, "/", total
    if (passed /= total) error stop 1

contains

    subroutine test_mono_type_to_string_cases()
        type(mono_type_t) :: int_type
        type(mono_type_t) :: real_type
        type(mono_type_t) :: array_type
        type(mono_type_t) :: alloc_array
        type(mono_type_t) :: char_type
        type(mono_type_t) :: char_unknown
        type(mono_type_t) :: invalid_type
        character(len=:), allocatable :: type_str
        logical :: success

        int_type = create_mono_type(TINT)
        call expect_string_equal(mono_type_to_string(int_type), "integer", &
                                 "mono_type_to_string integer")

        real_type = create_mono_type(TREAL)
        call expect_string_equal(mono_type_to_string(real_type), "real", &
                                 "mono_type_to_string real default")

        type_str = mono_type_to_string(real_type, standardize_real=.true.)
        call expect_string_equal(type_str, "real(dp)", &
                                 "mono_type_to_string real standardize")

        char_type = create_mono_type(TCHAR, char_size=4)
        call expect_string_equal(mono_type_to_string(char_type), &
                                 "character(len=4)", &
                                 "mono_type_to_string character len")

        char_unknown = create_mono_type(TCHAR)
        char_unknown%alloc_info%needs_allocatable_string = .true.
        call expect_string_equal(mono_type_to_string(char_unknown), &
                                 "character(len=:), allocatable", &
                                 "mono_type_to_string character allocatable")

        char_unknown = create_mono_type(TCHAR)
        type_str = mono_type_to_string(char_unknown, prefer_len_zero_char=.true.)
        call expect_string_equal(type_str, "character(len=0)", &
                                 "mono_type_to_string character zero length")

        array_type = create_mono_type(TARRAY, args=[real_type], array_size=3)
        type_str = mono_type_to_string(array_type, include_shape=.true.)
        call expect_string_equal(type_str, "real, dimension(3)", &
                                 "mono_type_to_string array dimension")

        type_str = mono_type_to_string(array_type)
        call expect_string_equal(type_str, "real", &
                                 "mono_type_to_string array element")

        alloc_array = create_mono_type(TARRAY, args=[real_type])
        alloc_array%alloc_info%is_allocatable = .true.
        type_str = mono_type_to_string(alloc_array, include_shape=.true.)
        call expect_string_equal(type_str, "real, dimension(:), allocatable", &
                                 "mono_type_to_string allocatable array")

        invalid_type = mono_type_t()
        type_str = mono_type_to_string(invalid_type, success=success)
        call expect_true(.not. success, &
                         "mono_type_to_string reports failure for invalid type")
        call expect_string_equal(type_str, "", &
                                 "mono_type_to_string invalid type string")

        type_str = mono_type_to_string(invalid_type, fallback="real", &
                                       success=success)
        call expect_true(.not. success, &
                         "mono_type_to_string preserves failure with fallback")
        call expect_string_equal(type_str, "real", &
                                 "mono_type_to_string applies fallback")
    end subroutine test_mono_type_to_string_cases

    subroutine expect_true(condition, name)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: name

        total = total + 1
        write (*, '(A,A)', advance='no') "Testing: ", name
        if (condition) then
            passed = passed + 1
            print *, " ... PASSED"
        else
            print *, " ... FAILED"
        end if
    end subroutine expect_true

    subroutine expect_false(condition, name)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: name

        total = total + 1
        write (*, '(A,A)', advance='no') "Testing: ", name
        if (.not. condition) then
            passed = passed + 1
            print *, " ... PASSED"
        else
            print *, " ... FAILED"
        end if
    end subroutine expect_false

    subroutine expect_string_equal(actual, expected, name)
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: name

        total = total + 1
        write (*, '(A,A)', advance='no') "Testing: ", name
        if (trim(actual) == trim(expected)) then
            passed = passed + 1
            print *, " ... PASSED"
        else
            print *, " ... FAILED"
            print *, "    expected:", trim(expected)
            print *, "    actual  :", trim(actual)
        end if
    end subroutine expect_string_equal

end program test_type_string_utils
