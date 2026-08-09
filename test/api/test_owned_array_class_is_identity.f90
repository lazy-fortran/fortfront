program test_owned_array_class_is_identity
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, control_statement_query_t, &
        query_control_statement, CONTROL_SELECT_TYPE, &
        select_type_owned_array_query_t, query_select_type_owned_array, &
        STORAGE_OWNED, STORAGE_MODULE, STORAGE_SAVE
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: control
    type(select_type_owned_array_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, j, select_count, syntax_status

    call read_example('examples/f90/owned_array_class_is_dynamic_identity.f90', &
        source)
    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-fsyntax-only examples/f90/owned_array_class_is_dynamic_identity.f90', &
        wait=.true., exitstat=syntax_status)
    call require(syntax_status == 0, 'GNU Fortran rejected owned-array fixture')

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'owned-array fixture did not parse')

    select_count = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'select_type') cycle
        select_count = select_count + 1
        control = query_control_statement(result%arena, i)
        call require(control%found .and. &
            control%statement_kind == CONTROL_SELECT_TYPE .and. &
            size(control%type_arms) == 1, 'SELECT TYPE arm facts are incomplete')
        query = query_select_type_owned_array(result%arena, &
            control%type_arms(1)%arm_node_index)
        if (query%has_unresolved_alias) then
            call require(query%is_refused .and. query%is_unresolved .and. &
                .not. query%is_resolved .and. &
                trim(query%selector_associate_name) == 'alias', &
                'associate alias identity was not refused')
        else
            select case (trim(query%selector_name))
            case ('items')
                if (query%has_control_flow_boundary) then
                    call require(query%is_refused .and. query%is_unresolved .and. &
                        .not. query%is_resolved .and. &
                        query%has_control_flow_boundary, &
                        'control-flow owned-array identity was not refused')
                else if (query%selector_storage%storage_class == STORAGE_OWNED) then
                    call require(query%found .and. query%is_resolved .and. &
                        .not. query%is_refused .and. query%is_class_is .and. &
                        query%is_owned_array .and. query%is_dynamic_type_concrete, &
                        'owned-array CLASS IS identity was not resolved')
                    call require(query%selector_rank == 1 .and. &
                        query%selector_declaration_index > 0 .and. &
                        query%selector_storage%storage_class == STORAGE_OWNED .and. &
                        query%is_declared_type_abstract, &
                        'owned-array storage mapping is incomplete')
                    call require(trim(query%declared_type_name) == 'base_t' .and. &
                        trim(query%dynamic_type_name) == 'child_t' .and. &
                        query%declared_type_index > 0 .and. query%dynamic_type_index > 0, &
                        'owned-array declared/dynamic type mapping is wrong')
                else
                    call require(.false., 'unexpected items storage classification')
                end if
            case ('global_items')
                call require(query%is_refused .and. query%is_unresolved .and. &
                    .not. query%is_resolved .and. query%has_global_mutable_state .and. &
                    (query%selector_storage%storage_class == STORAGE_MODULE .or. &
                    query%selector_storage%storage_class == STORAGE_SAVE) .and. &
                    index(query%refusal_reason, 'global') > 0, &
                    'global owned-array identity was not refused')
            case default
                call require(.false., 'unexpected owned-array selector')
            end select
        end if
        do j = 1, size(control%type_arms)
            call require(control%type_arms(j)%is_class_is, &
                'fixture arm stopped being CLASS IS')
        end do
    end do

    call require(select_count == 4, 'owned-array SELECT TYPE coverage is incomplete')
    query = query_select_type_owned_array(result%arena, 0)
    call require(.not. query%found .and. query%is_refused .and. &
        query%is_unresolved, 'invalid owned-array query was not refused')
    print *, 'PASS: owned-array CLASS IS dynamic identity API oracle'

contains

    include '../common/read_example.inc'

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_owned_array_class_is_identity
