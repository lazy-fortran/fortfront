program test_issue_1407_character_function
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Codegen: preserve character function result length ==="

    source = '! issue 1407 sample' // new_line('a') // &
             'function greet(name)' // new_line('a') // &
             '    implicit none' // new_line('a') // &
             '    character(len=*), intent(in) :: name' // new_line('a') // &
             '    character(len=len(name)) :: greet' // new_line('a') // &
             "    greet = 'Hello, ' // name" // new_line('a') // &
             'end function greet' // new_line('a') // &
             '' // new_line('a') // &
             "print *, greet('Ada')"

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (.not. allocated(output)) success = .false.

    if (success) then
        if (index(output, 'function greet(name)') == 0) success = .false.
        if (index(output, 'character(len=*), intent(in) :: name') == 0) success = .false.
        if (index(output, 'character(len=len(name)) :: greet') == 0) success = .false.
        if (index(output, 'len=))') > 0) success = .false.
        if (index(output, 'character(len=:), allocatable') > 0) success = .false.
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED: character result not preserved'
        if (allocated(output)) then
            print *, 'OUTPUT:'
            print *, trim(output)
        end if
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, 'ERRORS:'
                print *, trim(error_msg)
            end if
        end if
        stop 1
    end if

end program test_issue_1407_character_function
