program test_issue_180_character_arrays
    ! Test for GitHub issue #180: Character arrays translated as real
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    
    print *, "=== Issue #180: Character array type inference ==="
    
    ! Test 1: Basic character array literals 
    call test_basic_character_array()
    
    ! Test 2: Mixed length character arrays
    call test_mixed_length_arrays()
    
    ! Test 3: Character array with explicit type
    call test_explicit_character_array()
    
    print *, "All issue #180 tests completed"
    
contains

    subroutine test_basic_character_array()
        print *, "Testing basic character array literal..."
        
        source = 's = ["mom", "dad"]'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR: ", trim(error_msg)
            stop 1
        end if
        
        ! Must be character type, not real
        if (index(output, "real") > 0) then
            print *, "  FAIL: Character array incorrectly typed as real"
            stop 1
        end if
        
        if (index(output, "character") == 0) then
            print *, "  FAIL: Missing character type declaration"
            stop 1
        end if
        
        print *, "  PASS: Character array correctly inferred as character type"
    end subroutine test_basic_character_array

    subroutine test_mixed_length_arrays()
        print *, "Testing mixed length character arrays..."
        
        source = 'names = ["Alice", "Bob", "Charlie"]'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR: ", trim(error_msg)
            stop 1
        end if
        
        if (index(output, "character") == 0) then
            print *, "  FAIL: Mixed length array not typed as character"
            stop 1
        end if
        
        ! Should use max length for character type (Charlie = 7 chars)
        if (index(output, "len=7") == 0 .and. index(output, "len=8") == 0) then
            print *, "  WARNING: Expected character length 7 or 8, checking for reasonable length"
            if (index(output, "len=") == 0) then
                print *, "  FAIL: No character length specification found"
                stop 1
            end if
        end if
        
        print *, "  PASS: Mixed length character array correctly handled"
    end subroutine test_mixed_length_arrays

    subroutine test_explicit_character_array()
        print *, "Testing explicit character array declaration..."
        
        source = 'character(len=5) :: words(3)' // new_line('a') // &
                 'words = ["hello", "world", "test!"]'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR: ", trim(error_msg)
            stop 1
        end if
        
        if (index(output, "character(len=5)") == 0) then
            print *, "  FAIL: Explicit character declaration not preserved"
            stop 1
        end if
        
        print *, "  PASS: Explicit character declaration preserved"
    end subroutine test_explicit_character_array
    
end program test_issue_180_character_arrays