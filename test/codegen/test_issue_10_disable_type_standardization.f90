program test_issue_10_disable_type_standardization
    ! Test for GitHub issue #10: Add option to disable type kind standardization during formatting
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output_standardized
    character(len=:), allocatable :: output_preserved
    character(len=:), allocatable :: error_msg
    type(format_options_t) :: format_opts_std, format_opts_preserve
    
    print *, "=== Issue #10: Add option to disable type kind standardization ==="
    
    ! Test source with explicit types without kinds (F90-to-F90 case)
    source = "real :: x" // new_line('a') // &
             "integer :: i" // new_line('a') // &
             "character(len=80) :: name"
    
    ! Configure formatting with type standardization enabled (default)
    format_opts_std%indent_size = 4
    format_opts_std%use_tabs = .false.
    format_opts_std%standardize_types = .true.  ! Enable type standardization
    
    ! Configure formatting with type standardization disabled
    format_opts_preserve%indent_size = 4
    format_opts_preserve%use_tabs = .false.
    format_opts_preserve%standardize_types = .false.  ! Disable type standardization
    
    call transform_lazy_fortran_string_with_format(source, output_standardized, error_msg, format_opts_std)
    
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "ERROR in standardized: ", trim(error_msg)
            stop 1
        end if
    end if
    
    call transform_lazy_fortran_string_with_format(source, output_preserved, error_msg, format_opts_preserve)
    
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "ERROR in preserved: ", trim(error_msg)
            stop 1
        end if
    end if
    
    print *, "Input source:"
    print *, trim(source)
    print *
    print *, "Output with type standardization enabled:"
    print *, trim(output_standardized)
    print *
    print *, "Output with type standardization disabled:"
    print *, trim(output_preserved)
    print *
    
    ! Check if type standardization can be controlled  
    ! With standardization enabled, "real" should become "real(8)"
    ! With standardization disabled, "real" should stay as "real"
    if (index(output_standardized, "real(8)") > 0 .and. &
        index(output_preserved, "real ::") > 0 .and. &
        index(output_preserved, "real(8)") == 0) then
        print *, "PASS: Type standardization option controls real type generation"  
        print *, "INFO: Standardized output uses real(8), preserved uses real"
    else if (index(output_standardized, "real(8)") > 0 .and. &
             index(output_preserved, "real(8)") > 0) then
        print *, "FAIL: Type standardization disable option not working"
        print *, "Both outputs show real(8) - standardization not disabled"
        stop 1
    else
        print *, "FAIL: Type standardization not working as expected"
        print *, "Expected standardized: real(8), preserved: real ::"
        stop 1
    end if
    
end program test_issue_10_disable_type_standardization