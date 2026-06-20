program test_issue_2835_walrus_target_resolves
    ! Issue #2835: a walrus declaration `z := 42` must produce an analyzed AST
    ! whose assignment target references a resolvable identifier node, so a
    ! backend can walk it. The crash was a dangling target index reported as
    ! "identifier index does not reference an AST node" during lowering.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: compiler_frontend_options_t, &
                         compiler_frontend_result_t, &
                         compile_frontend_from_string, &
                         get_program_body_info, get_identifier_name, &
                         assignment_node, INPUT_MODE_LAZY
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    character(len=:), allocatable :: prog_name, error_msg, target_name
    integer, allocatable :: body_indices(:)
    integer :: i, walrus_target, walrus_count

    ! Backends lower from the analyzed (non-standardized) arena.
    options%input_mode = INPUT_MODE_LAZY
    options%run_semantics = .true.
    options%standardize = .false.

    call compile_frontend_from_string('z := 42'//new_line('a')//'print *, z', &
                                      result, options)
    if (.not. result%success()) then
        write (error_unit, '(A)') 'FAIL: frontend rejected walrus source: '// &
            trim(result%diagnostic_text)
        error stop 1
    end if

    call get_program_body_info(result%arena, result%root_index, prog_name, &
                               body_indices, error_msg)
    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: get_program_body_info: '//trim(error_msg)
        error stop 1
    end if

    walrus_count = 0
    walrus_target = 0
    do i = 1, size(body_indices)
        if (.not. result%arena%has_node_at(body_indices(i))) cycle
        select type (stmt => result%arena%entries(body_indices(i))%node)
        type is (assignment_node)
            if (stmt%is_walrus) then
                walrus_count = walrus_count + 1
                walrus_target = stmt%target_index
            end if
        end select
    end do

    if (walrus_count /= 1) then
        write (error_unit, '(A,I0)') 'FAIL: expected one walrus assignment, got ', &
            walrus_count
        error stop 1
    end if

    ! The exact backend operation that crashed in #2835: resolve the target.
    call get_identifier_name(result%arena, walrus_target, target_name, error_msg)
    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: walrus target unresolvable: '// &
            trim(error_msg)
        error stop 1
    end if
    if (trim(target_name) /= 'z') then
        write (error_unit, '(A)') 'FAIL: walrus target name is "'// &
            trim(target_name)//'", expected "z"'
        error stop 1
    end if

    print *, 'PASS: walrus := target resolves to a valid identifier node'
end program test_issue_2835_walrus_target_resolves
