program test_procedure_target_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, declaration_query_t, &
        query_declaration, procedure_target_query_t, query_procedure_target, &
        program_unit_query_t, query_program_unit
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(procedure_target_query_t) :: target
    type(declaration_query_t) :: pointer_declaration
    type(program_unit_query_t) :: scope
    character(len=:), allocatable :: source
    integer :: i, found_targets
    logical :: found_internal, found_external, found_null, found_unresolved
    logical :: found_negative

    call read_example('examples/f90/procedure_target_query.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'procedure target example did not parse')

    found_targets = 0
    found_internal = .false.
    found_external = .false.
    found_null = .false.
    found_unresolved = .false.
    found_negative = .false.

    do i = 1, result%arena%size
        target = query_procedure_target(result%arena, i)
        if (.not. target%found) then
            if (trim(get_node_type_at(result%arena, i)) == 'assignment') then
                found_negative = .true.
            end if
            cycle
        end if

        found_targets = found_targets + 1
        call require(target%assignment_node_index == i, &
            'assignment identity was not preserved')
        call require(target%pointer_node_index > 0 .and. &
            target%pointer_declaration_index > 0, &
            'pointer declaration identity is missing')
        pointer_declaration = query_declaration(result%arena, &
            target%pointer_declaration_index)
        call require(pointer_declaration%found .and. &
            pointer_declaration%is_pointer, &
            'pointer declaration fact is incomplete')
        call require(target%scope_node_index > 0, 'scope identity is missing')
        scope = query_program_unit(result%arena, target%scope_node_index)
        call require(scope%found .and. trim(scope%unit_kind) == 'program', &
            'lexical scope fact is incorrect')

        select case (trim(target%procedure_name))
        case ('internal_scale')
            found_internal = .true.
            call require(target%is_resolved .and. .not. target%is_unresolved, &
                'internal procedure target was not resolved')
            call require(target%target_procedure_index == &
                target%binding_node_index .and. target%target_procedure_index > 0, &
                'internal procedure binding identity is missing')
            call require(trim(target%binding_name) == 'internal_scale', &
                'internal procedure binding name is wrong')
        case ('external_scale')
            found_external = .true.
            call require(target%is_resolved .and. &
                target%target_declaration_index > 0, &
                'external procedure declaration binding is missing')
            call require(target%target_procedure_index == 0, &
                'external procedure received an invented procedure node')
        case ('null')
            found_null = .true.
            call require(target%is_null .and. .not. target%is_unresolved, &
                'NULL target state is incorrect')
        case ('missing_scale')
            found_unresolved = .true.
            call require(target%is_unresolved .and. .not. target%is_resolved, &
                'unresolved procedure target state is incorrect')
            call require(target%binding_node_index == 0, &
                'unresolved target received an invented binding')
        case default
            call require(.false., 'unexpected procedure target was reported')
        end select
    end do

    call require(found_targets == 4, 'procedure target assignments are incomplete')
    call require(found_internal .and. found_external .and. found_null .and. &
        found_unresolved, 'procedure target cases are incomplete')
    call require(found_negative, 'ordinary assignment was not rejected')

    target = query_procedure_target(result%arena, -1)
    call require(.not. target%found .and. .not. target%is_unresolved, &
        'invalid node received procedure target facts')

    print *, 'PASS: procedure target query contract'

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

end program test_procedure_target_query
