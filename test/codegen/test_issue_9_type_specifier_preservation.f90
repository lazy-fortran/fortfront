program test_issue_9_type_specifier_preservation
    ! Test for GitHub issue #9: Type specifiers are modified during emit_fortran
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    
    print *, "=== Issue #9: Type specifiers are modified during emit_fortran ==="
    
    ! Test with various type specifiers that should be preserved exactly
    source = "real(kind=real64) :: x" // new_line('a') // &
             "integer(kind=int32) :: i" // new_line('a') // &
             "character(len=128) :: name" // new_line('a') // &
             "logical(kind=logical_kind) :: flag"
    
    call transform_lazy_fortran_string(source, output, error_msg)
    
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "ERROR: ", trim(error_msg)
            stop 1
        end if
    end if
    
    print *, "Input source:"
    print *, trim(source)
    print *
    print *, "Output:"
    print *, trim(output)
    print *
    
    ! Check if type specifiers are preserved exactly
    if (index(output, "real(kind=real64)") > 0 .and. &
        index(output, "integer(kind=int32)") > 0 .and. &
        index(output, "character(len=128)") > 0 .and. &
        index(output, "logical(kind=logical_kind)") > 0) then
        print *, "PASS: All type specifiers preserved exactly"
    else
        print *, "FAIL: Type specifiers were modified"
        stop 1
    end if
    
end program test_issue_9_type_specifier_preservation