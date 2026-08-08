program test_procedure_callback_signature_query
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, TINT, TREAL, get_node_type_at, &
        procedure_target_query_t, &
        query_procedure_target
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(procedure_target_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, resolved_count, unresolved_count, null_count
    logical :: saw_resolved, saw_incompatible, saw_unresolved, saw_null
    logical :: saw_negative

    call read_example('examples/f90/procedure_callback_signature_query.f90', &
        source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'callback signature example was rejected')

    resolved_count = 0
    unresolved_count = 0
    null_count = 0
    saw_resolved = .false.
    saw_incompatible = .false.
    saw_unresolved = .false.
    saw_null = .false.
    saw_negative = .false.

    do i = 1, result%arena%size
        query = query_procedure_target(result%arena, i)
        if (.not. query%found) then
            if (trim(get_node_type_at(result%arena, i)) == 'assignment') then
                saw_negative = .true.
            end if
            cycle
        end if

        if (query%is_null) then
            null_count = null_count + 1
            saw_null = trim(query%pointer_name) == 'null_callback'
            call require(.not. query%signature%found, &
                'NULL target received an invented signature')
            cycle
        end if
        if (query%is_unresolved) then
            unresolved_count = unresolved_count + 1
            saw_unresolved = trim(query%pointer_name) == 'unresolved_callback'
            call require(.not. query%signature%found, &
                'unresolved target received an invented signature')
            cycle
        end if
        if (.not. query%signature%found) then
            unresolved_count = unresolved_count + 1
            saw_unresolved = trim(query%pointer_name) == 'unresolved_callback'
            call require(query%is_resolved .and. &
                .not. query%is_unresolved, &
                'external callback boundary state is incorrect')
            cycle
        end if

        call require(query%is_resolved .and. query%signature%found, &
            'resolved callback signature facts are missing')
        resolved_count = resolved_count + 1
        select case (trim(query%procedure_name))
        case ('callback_target')
            saw_resolved = .true.
            call check_callback_signature(query)
        case ('scalar_target')
            saw_incompatible = .true.
            call require(query%signature%dummy_count == 1, &
                'incompatible target signature was not preserved')
        case default
            call require(.false., 'unexpected resolved callback target')
        end select
    end do

    call require(resolved_count == 2 .and. unresolved_count == 1 .and. &
        null_count == 1, 'callback target boundary cases are incomplete')
    call require(saw_resolved .and. saw_incompatible .and. saw_unresolved .and. &
        saw_null .and. saw_negative, &
        'resolved, incompatible, unresolved, NULL, or negative cases missing')

    query = query_procedure_target(result%arena, -1)
    call require(.not. query%found .and. .not. query%signature%found, &
        'negative node received callback signature facts')

    print *, 'PASS: procedure callback signature query contract'

contains

    include '../common/read_example.inc'

    subroutine check_callback_signature(query)
        type(procedure_target_query_t), intent(in) :: query

        call require(query%signature%is_function, &
            'resolved callback was not identified as a function')
        call require(query%signature%result_type_known .and. &
            query%signature%result_type_kind == TREAL .and. &
            query%signature%result_rank_known .and. &
            query%signature%result_rank == 0, &
            'callback result type or rank facts are wrong')
        call require(query%signature%result_category_known .and. &
            trim(query%signature%result_category) == 'real', &
            'callback result category is wrong')
        call require(query%signature%result_kind_known .and. &
            query%signature%result_kind_value > 0, &
            'callback result kind is missing')
        call require(query%signature%dummy_count == 3, &
            'callback dummy count is wrong')

        call require(trim(query%signature%dummies(1)%name) == 'scalar' .and. &
            query%signature%dummies(1)%type_known .and. &
            query%signature%dummies(1)%category_known .and. &
            query%signature%dummies(1)%kind_known .and. &
            query%signature%dummies(1)%rank_known .and. &
            trim(query%signature%dummies(1)%type_category) == 'real' .and. &
            query%signature%dummies(1)%type_kind == TREAL .and. &
            query%signature%dummies(1)%rank == 0 .and. &
            trim(query%signature%dummies(1)%intent) == 'in' .and. &
            .not. query%signature%dummies(1)%is_optional .and. &
            .not. query%signature%dummies(1)%is_value, &
            'scalar dummy facts are wrong')
        call require(trim(query%signature%dummies(2)%name) == 'values' .and. &
            query%signature%dummies(2)%type_known .and. &
            query%signature%dummies(2)%category_known .and. &
            query%signature%dummies(2)%kind_known .and. &
            query%signature%dummies(2)%rank_known .and. &
            trim(query%signature%dummies(2)%type_category) == 'real' .and. &
            query%signature%dummies(2)%type_kind == TREAL .and. &
            query%signature%dummies(2)%rank == 1 .and. &
            trim(query%signature%dummies(2)%intent) == 'inout' .and. &
            query%signature%dummies(2)%is_optional, &
            'array dummy facts are wrong')
        call require(trim(query%signature%dummies(3)%name) == 'scale' .and. &
            query%signature%dummies(3)%type_known .and. &
            query%signature%dummies(3)%category_known .and. &
            query%signature%dummies(3)%kind_known .and. &
            query%signature%dummies(3)%rank_known .and. &
            trim(query%signature%dummies(3)%type_category) == 'integer' .and. &
            query%signature%dummies(3)%type_kind == TINT .and. &
            query%signature%dummies(3)%rank == 0 .and. &
            query%signature%dummies(3)%is_value, &
            'VALUE dummy facts are wrong')
    end subroutine check_callback_signature

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_procedure_callback_signature_query
