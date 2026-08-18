program test_procedure_actual_interface_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, procedure_actual_argument_query_t, &
        query_procedure_actual_argument, get_subroutine_call_name
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(procedure_actual_argument_query_t) :: query
    character(len=:), allocatable :: source, mismatch_source, call_name, error_msg
    character(len=:), allocatable :: executable
    integer :: i, compatible_count, status

    call read_example('examples/f90/procedure_actual_interface_query.f90', source)
    executable = test_executable_path('fortfront_procedure_actual_interface_query')
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'procedure interface fixture was rejected: '// &
        trim(result%diagnostic_text))

    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-o '//executable//' '// &
        'examples/f90/procedure_actual_interface_query.f90', &
        wait=.true., exitstat=status)
    call require(status == 0, 'GNU Fortran rejected the interface fixture')
    call execute_command_line(executable, &
        wait=.true., exitstat=status)
    call require(status == 0, 'interface fixture runtime oracle failed')
    call test_remove_file(executable)

    compatible_count = 0
    do i = 1, result%arena%size
        call get_subroutine_call_name(result%arena, i, call_name, error_msg)
        if (len_trim(error_msg) > 0 .or. trim(call_name) /= 'apply') cycle
        query = query_procedure_actual_argument(result%arena, i, 'operation')
        call require(query%found, 'apply call lost actual/formal identity')
        call require(query%formal_signature%found .and. &
            trim(query%formal_interface_name) == 'callback_iface' .and. &
            query%formal_interface_node_index == &
            query%formal_signature%procedure_node_index .and. &
            query%formal_signature%dummy_count == 1, &
            'named formal procedure interface facts are incomplete')
        call require(trim(query%formal_signature%dummies(1)%name) == 'value', &
            'formal interface dummy identity was not preserved')
        call require(query%is_resolved .and. query%is_interface_compatible .and. &
            .not. query%has_incompatible_interface .and. .not. query%is_refused, &
            'compatible procedure actual/interface pair was not proven')
        compatible_count = compatible_count + 1
    end do
    call require(compatible_count == 2, &
        'direct and procedure-pointer interface mappings were not both found')

    call read_example('examples/f90/procedure_actual_interface_mismatch.f90', &
        mismatch_source)
    options%run_semantics = .false.
    call compile_frontend_from_string(mismatch_source, result, options)
    call require(result%parse_ok, 'mismatch fixture did not parse')
    call execute_command_line('gfortran -std=f2018 -pedantic -fsyntax-only '// &
        'examples/f90/procedure_actual_interface_mismatch.f90', &
        wait=.true., exitstat=status)
    call require(status /= 0, 'GNU Fortran accepted the incompatible callback')

    do i = 1, result%arena%size
        call get_subroutine_call_name(result%arena, i, call_name, error_msg)
        if (len_trim(error_msg) > 0 .or. trim(call_name) /= 'apply') cycle
        query = query_procedure_actual_argument(result%arena, i, 'operation')
        call require(query%found .and. query%formal_signature%found .and. &
            query%has_incompatible_interface .and. query%is_refused .and. &
            query%is_unresolved .and. .not. query%is_resolved .and. &
            .not. query%is_interface_compatible, &
            'incompatible procedure interface was not an explicit refusal')
    end do

    print *, 'PASS: procedure actual/formal interface query contract'

contains

    include '../common/read_example.inc'
    include '../common/test_command_helpers.inc'

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_procedure_actual_interface_query
