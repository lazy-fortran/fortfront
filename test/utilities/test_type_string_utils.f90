program test_type_string_utils
    use type_string_utils, only: is_character_type_string
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

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0)') "Passed ", passed, "/", total
    if (passed /= total) error stop 1

contains

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

end program test_type_string_utils
