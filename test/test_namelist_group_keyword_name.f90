! A namelist group may be named by a keyword such as `case`; the READ and
! WRITE nml= clauses must parse it without diagnostics and keep it in the output.
program test_namelist_group_keyword_name
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input_code, output_code, error_msg

    input_code = "program p"//new_line('a')// &
        "    implicit none"//new_line('a')// &
        "    integer :: u"//new_line('a')// &
        "    real :: r0"//new_line('a')// &
        "    namelist /case/ r0"//new_line('a')// &
        "    read (u, nml=case)"//new_line('a')// &
        "    write (u, nml=case)"//new_line('a')// &
        "end program p"//new_line('a')

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: unexpected diagnostics: ", trim(error_msg)
        error stop 1
    end if
    if (index(output_code, "read(u, nml=case)") == 0 .and. &
        index(output_code, "read (u, nml=case)") == 0) then
        print *, "FAIL: read nml=case lost"
        print *, trim(output_code)
        error stop 1
    end if
    print *, "PASS"
end program test_namelist_group_keyword_name
