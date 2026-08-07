program test_issue_2974_compound_declaration
    ! Issue #2974: a non-constant array specification in the first entity of
    ! a compound declaration must not hide the following scalar entities.
    !
    ! The source is accepted by an independent GNU Fortran syntax oracle. The
    ! FortFront check uses the typed compiler-front-end result, rather than
    ! source spelling or private parser state, and verifies that each declared
    ! entity has its own public declaration node and correct shape.
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fortfront, only: compile_frontend_from_string, &
        compiler_frontend_options_t, compiler_frontend_result_t, &
        INPUT_MODE_STANDARD, declaration_node
    use lexer_core, only: to_lower
    implicit none

    character(:), allocatable :: source_code, scratch_dir, source_path
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    integer :: exit_code, source_unit
    logical :: is_windows, has_gfortran

    source_code = 'SUBROUTINE s(n, a)'//new_line('a')// &
        '  INTEGER n'//new_line('a')// &
        '  DOUBLE PRECISION a(n+1), res'//new_line('a')// &
        '  res = 2.0d0'//new_line('a')// &
        '  print *, res'//new_line('a')// &
        'END SUBROUTINE s'//new_line('a')// &
        'PROGRAM p'//new_line('a')// &
        '  DOUBLE PRECISION b(5)'//new_line('a')// &
        '  call s(4, b)'//new_line('a')// &
        'END PROGRAM p'//new_line('a')

    is_windows = check_if_windows()
    if (is_windows) then
        scratch_dir = 'build'
    else
        scratch_dir = '/var/tmp/ert/fortfront_issue_2974'
    end if
    source_path = scratch_dir//'/compound_declaration.f90'
    if (.not. is_windows) then
        call execute_command_line('mkdir -p '//scratch_dir, exitstat=exit_code)
        call require_zero(exit_code, 'create scratch directory')
    end if
    open (newunit=source_unit, file=source_path, status='replace', &
        action='write', iostat=exit_code)
    call require_zero(exit_code, 'open source oracle')
    write (source_unit, '(a)') source_code
    close (source_unit)

    has_gfortran = .false.
    if (.not. is_windows) then
        call execute_command_line( &
            'command -v gfortran >/dev/null 2>&1', exitstat=exit_code)
        has_gfortran = (exit_code == 0)
    end if
    if (has_gfortran) then
        call execute_command_line( &
            'gfortran -fsyntax-only '//source_path, exitstat=exit_code)
        call require_zero(exit_code, 'gfortran accepts compound declaration')
    else
        write (output_unit, '(a)') &
            'SKIP: gfortran syntax oracle is unavailable on this lane'
    end if

    options = compiler_frontend_options_t()
    options%run_semantics = .true.
    options%standardize = .false.
    options%input_mode = INPUT_MODE_STANDARD
    call compile_frontend_from_string(source_code, result, options)
    if (.not. result%success()) then
        write (error_unit, '(a)') 'FAIL: FortFront rejected accepted source: '// &
            trim(result%diagnostic_text)
        error stop 1
    end if

    call require_declaration(result, 'n', .false., 'integer')
    call require_declaration(result, 'a', .true., 'double precision')
    call require_declaration(result, 'res', .false., 'double precision')
    call require_declaration(result, 'b', .true., 'double precision')

    write (output_unit, '(a)') &
        'PASS: #2974 preserves all compound declaration entities'

contains

    subroutine require_declaration(frontend, wanted_name, wanted_array, &
            wanted_type)
        type(compiler_frontend_result_t), intent(in) :: frontend
        character(len=*), intent(in) :: wanted_name, wanted_type
        logical, intent(in) :: wanted_array
        integer :: i
        logical :: found

        found = .false.
        do i = 1, frontend%arena%size
            if (.not. allocated(frontend%arena%entries(i)%node)) cycle
            select type (node => frontend%arena%entries(i)%node)
                type is (declaration_node)
                if (.not. allocated(node%var_name)) cycle
                if (trim(node%var_name) /= wanted_name) cycle
                if (node%is_array .neqv. wanted_array) then
                    write (error_unit, '(a)') 'FAIL: wrong array shape for '// &
                        wanted_name
                    error stop 1
                end if
                if (to_lower(trim(node%type_name)) /= to_lower(wanted_type)) then
                    write (error_unit, '(a)') 'FAIL: wrong type for '//wanted_name// &
                        ': got '//trim(node%type_name)//', want '//wanted_type
                    error stop 1
                end if
                found = .true.
            end select
        end do

        if (.not. found) then
            write (error_unit, '(a)') 'FAIL: missing declaration for '//wanted_name
            error stop 1
        end if
    end subroutine require_declaration

    subroutine require_zero(status, action)
        integer, intent(in) :: status
        character(len=*), intent(in) :: action

        if (status /= 0) then
            write (error_unit, '(a,i0)') 'FAIL: '//action//' (status ', status
            error stop 1
        end if
    end subroutine require_zero

    logical function check_if_windows()
        character(len=16) :: value
        integer :: status

        check_if_windows = .false.
        call get_environment_variable('OS', value, status=status)
        if (status == 0 .and. len_trim(value) >= 7) then
            if (value(1:7) == 'Windows') check_if_windows = .true.
        end if
        call get_environment_variable('WINDIR', value, status=status)
        if (status == 0) check_if_windows = .true.
    end function check_if_windows

end program test_issue_2974_compound_declaration
