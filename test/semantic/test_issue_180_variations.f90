program test_issue_180_variations  
    ! Test variations of character arrays to verify Issue #180 is fixed
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    
    print *, "=== Issue #180 Variations: Character arrays ==="
    
    ! Test case 1: Simple character array from the issue
    call test_simple_character_array()
    
    ! Test case 2: Mixed length character arrays
    call test_mixed_length_character_array()
    
    ! Test case 3: Character array initialization
    call test_character_array_init()
    
    print *, "All Issue #180 variations test completed"
    
contains

    subroutine test_simple_character_array()
        print *, "Test 1: Simple character array..."
        
        source = 's = ["mom", "dad"]' // new_line('a') // &
                 'print*,s'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        print *, "  OUTPUT: ", trim(output)
        
        ! Should be character, not real
        if (index(output, "real") > 0) then
            print *, "  FAIL: Character array incorrectly typed as real"
            stop 1
        end if
        
        if (index(output, "character") == 0) then
            print *, "  FAIL: Missing character type declaration"
            stop 1
        end if
        
        print *, "  PASS: Simple character array correctly typed"
    end subroutine test_simple_character_array

    subroutine test_mixed_length_character_array()
        print *, "Test 2: Mixed length character array..."
        
        source = 'names = ["Alice", "Bob", "Charlie"]' // new_line('a') // &
                 'print*, names'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        print *, "  OUTPUT: ", trim(output)
        
        if (index(output, "real") > 0) then
            print *, "  FAIL: Mixed length character array incorrectly typed as real"
            stop 1
        end if
        
        if (index(output, "character") == 0) then
            print *, "  FAIL: Missing character type declaration for mixed array"
            stop 1
        end if
        
        print *, "  PASS: Mixed length character array correctly typed"
    end subroutine test_mixed_length_character_array

    subroutine test_character_array_init()
        print *, "Test 3: Character array with explicit initialization..."
        
        source = 'character(len=5) :: words(3)' // new_line('a') // &
                 'words = ["hello", "world", "test!"]' // new_line('a') // &
                 'print*, words'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        print *, "  OUTPUT: ", trim(output)
        
        ! Should preserve explicit character declaration
        if (index(output, "character(len=5)") == 0) then
            print *, "  FAIL: Explicit character declaration not preserved"
            stop 1
        end if
        
        print *, "  PASS: Explicit character array correctly handled"
    end subroutine test_character_array_init
    
end program test_issue_180_variations