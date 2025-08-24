program test_issue_441_subroutine_parsing
    ! Critical test for Issue #441: Subroutines and functions not handled properly
    !
    ! BUG SYMPTOMS:
    ! 1. Print statements inside subroutines are dropped during parsing/codegen
    ! 2. Subroutines appear duplicated in output (subroutine bar appears twice) 
    ! 3. Subroutine bodies are empty in generated code
    
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    character(len=:), allocatable :: input_code, output_code, error_msg
    character(len=:), allocatable :: search_text
    integer :: count, pos, start_pos
    logical :: bug1_present, bug2_present, bug3_present
    
    ! Build input code
    input_code = 'module m' // new_line('A') // &
                 'contains' // new_line('A') // &
                 'subroutine foo()' // new_line('A') // &
                 'print*,"foo"' // new_line('A') // &
                 'end subroutine' // new_line('A') // &
                 '' // new_line('A') // &
                 'subroutine bar()' // new_line('A') // &
                 'print*,"bar"' // new_line('A') // &
                 'end subroutine bar' // new_line('A') // &
                 'end module'
    
    print *, "=== Issue #441 Subroutine Parsing Bug Test ==="
    print *, ""
    print *, "Input code:"
    print *, trim(input_code)
    print *, ""
    
    ! Transform the code
    call transform_lazy_fortran_string(input_code, output_code, error_msg)
    
    if (error_msg /= "") then
        print *, "Errors during processing:"
        print *, trim(error_msg)
        print *, ""
    end if
    
    print *, "Output code:"
    print *, trim(output_code)
    print *, ""
    
    ! Check for the bugs
    print *, "=== Bug Analysis ==="
    
    ! Bug 1: Missing print statements
    bug1_present = index(output_code, 'print*,"foo"') == 0
    if (bug1_present) then
        print *, "BUG 1 CONFIRMED: print statements missing from subroutine foo"
    else
        print *, "BUG 1 FIXED: print statement present in subroutine foo"
    end if
    
    bug2_present = index(output_code, 'print*,"bar"') == 0
    if (bug2_present) then
        print *, "BUG 2 CONFIRMED: print statements missing from subroutine bar"
    else
        print *, "BUG 2 FIXED: print statement present in subroutine bar"
    end if
    
    ! Bug 3: Duplicate subroutine bar - count occurrences
    search_text = "subroutine bar"
    count = 0
    start_pos = 1
    
    do
        pos = index(output_code(start_pos:), search_text)
        if (pos == 0) exit
        count = count + 1
        start_pos = start_pos + pos
    end do
    
    bug3_present = count > 1
    if (bug3_present) then
        print *, "BUG 3 CONFIRMED: subroutine bar appears", count, "times (should be 1)"
    else if (count == 1) then
        print *, "BUG 3 FIXED: subroutine bar appears exactly once"
    else
        print *, "UNEXPECTED: subroutine bar not found at all"
    end if
    
    print *, ""
    print *, "=== Summary ==="
    if (bug1_present .or. bug2_present .or. bug3_present) then
        print *, "CRITICAL: Multiple parsing/codegen bugs confirmed in Issue #441"
        print *, "This affects subroutine and function parsing throughout the system"
        stop 1
    else
        print *, "SUCCESS: All bugs appear to be fixed!"
        stop 0
    end if
    
end program test_issue_441_subroutine_parsing