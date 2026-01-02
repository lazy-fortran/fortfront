program test_issue_1407_character_function
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Codegen: preserve character function result length ==="

    call read_example('examples/lf/character_function_len.lf', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (.not. allocated(output)) success = .false.

    if (success) then
        if (index(output, 'function greet(name) result(greet_result)') == 0) &
            success = .false.
        if (index(output, 'character(len=len(name)) :: greet_result') == 0) &
            success = .false.
        if (index(output, "greet_result = 'Hello, ' // name") == 0) then
            if (index(output, "greet_result = 'Hello, ' //name") == 0) &
                success = .false.
        end if
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


contains


    include '../common/read_example.inc'
end program test_issue_1407_character_function
