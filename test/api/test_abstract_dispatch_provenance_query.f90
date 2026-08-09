program test_abstract_dispatch_provenance_query
    use iso_fortran_env, only: error_unit
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, derived_type_query_t, &
        query_derived_type, binding_resolution_query_t, &
        query_type_binding_resolution, type_bound_call_query_t, &
        query_type_bound_call
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(derived_type_query_t) :: derived
    type(binding_resolution_query_t) :: binding
    type(type_bound_call_query_t) :: call_binding
    character(len=:), allocatable :: source
    character(len=:), allocatable :: syntax_command
    integer :: syntax_status, syntax_exitstat
    integer :: i, root_index, middle_index, leaf_index, generic_index
    integer :: target

    call read_example('examples/f90/abstract_dispatch_provenance_query.f90', &
        source)
    syntax_command = 'gfortran -fsyntax-only examples/f90/'// &
        'abstract_dispatch_provenance_query.f90'
    call execute_command_line(syntax_command, wait=.true., &
        exitstat=syntax_exitstat, cmdstat=syntax_status)
    call require(syntax_status == 0 .and. syntax_exitstat == 0, &
        'GNU Fortran rejected the abstract dispatch fixture')

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'abstract provenance example did not parse')

    root_index = 0
    middle_index = 0
    leaf_index = 0
    generic_index = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) /= 'derived_type') cycle
        derived = query_derived_type(result%arena, i)
        if (.not. derived%found) cycle
        select case (trim(derived%name))
        case ('root_t')
            root_index = i
        case ('middle_t')
            middle_index = i
        case ('leaf_t')
            leaf_index = i
        case ('generic_t')
            generic_index = i
        end select
    end do
    call require(root_index > 0 .and. middle_index > 0 .and. &
        leaf_index > 0 .and. generic_index > 0, &
        'abstract provenance types are missing')

    ! Independent oracle: the leaf is concrete, but its WORK implementation
    ! is declared by the abstract intermediate type.
    binding = query_type_binding_resolution(result%arena, root_index, 'work')
    call require(binding%found .and. binding%is_abstract_type, &
        'abstract WORK binding was not found')
    call require(size(binding%dispatch_target_type_indices) == 1 .and. &
        binding%dispatch_target_type_indices(1) == leaf_index, &
        'concrete leaf dispatch target is wrong')
    call require(size(binding%dispatch_target_declaring_type_indices) == 1 .and. &
        binding%dispatch_target_declaring_type_indices(1) == middle_index, &
        'effective inherited binding declaration is wrong')
    call require(size(binding%dispatch_target_is_inherited) == 1 .and. &
        binding%dispatch_target_is_inherited(1), &
        'inherited dispatch provenance was not exposed')

    ! A deferred contract is implemented by the leaf itself, not inherited.
    binding = query_type_binding_resolution(result%arena, root_index, 'run')
    call require(binding%found .and. binding%is_deferred, &
        'deferred RUN binding was not found')
    target = find_target(binding%dispatch_target_type_indices, leaf_index)
    call require(target > 0, 'deferred RUN leaf target is missing')
    call require(binding%dispatch_target_declaring_type_indices(target) == &
        leaf_index .and. .not. binding%dispatch_target_is_inherited(target), &
        'leaf deferred implementation provenance is wrong')

    call_binding = query_type_bound_call(result%arena, 0)
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        call_binding = query_type_bound_call(result%arena, i)
        if (.not. call_binding%found) cycle
        if (trim(call_binding%binding_name) == 'work') then
            call require(size(call_binding%dispatch_target_type_indices) == 1 .and. &
                call_binding%dispatch_target_type_indices(1) == leaf_index, &
                'type-bound WORK target was not preserved')
            call require(call_binding%dispatch_target_declaring_type_indices(1) == &
                middle_index .and. call_binding%dispatch_target_is_inherited(1), &
                'type-bound WORK provenance was not copied')
            exit
        end if
    end do
    call require(call_binding%found .and. &
        trim(call_binding%binding_name) == 'work', &
        'type-bound WORK call was not found')

    ! Generic dispatch remains refusal-only and must not gain provenance facts.
    binding = query_type_binding_resolution(result%arena, generic_index, &
        'choose')
    call require(binding%found .and. binding%is_generic, &
        'generic refusal was not reported')
    call require(size(binding%dispatch_target_type_indices) == 0 .and. &
        size(binding%dispatch_target_declaring_type_indices) == 0 .and. &
        size(binding%dispatch_target_is_inherited) == 0, &
        'generic dispatch exposed a guessed target')

    binding = query_type_binding_resolution(result%arena, root_index, &
        'missing')
    call require(.not. binding%found .and. &
        size(binding%dispatch_target_declaring_type_indices) == 0, &
        'unresolved binding exposed provenance')

    print *, 'PASS: abstract dispatch provenance contract'

contains

    include '../common/read_example.inc'

    integer function find_target(indices, wanted) result(found)
        integer, intent(in) :: indices(:), wanted
        integer :: j

        found = 0
        do j = 1, size(indices)
            if (indices(j) == wanted) then
                found = j
                return
            end if
        end do
    end function find_target

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            write (error_unit, '(A)') 'FAIL: '//trim(message)
            error stop 1
        end if
    end subroutine require

end program test_abstract_dispatch_provenance_query
