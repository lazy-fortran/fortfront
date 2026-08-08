program test_associate_selector_facts
    use fortfront, only: associate_selector_query_t, &
        compiler_frontend_options_t, compiler_frontend_result_t, &
        compile_frontend_from_string, get_node_type_at, &
        INPUT_MODE_STANDARD, query_associate_selectors, ACCESS_READ, &
        ACCESS_WRITE, ACCESS_READ_WRITE
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(associate_selector_query_t), allocatable :: facts(:)
    character(len=:), allocatable :: source
    integer :: i, associate_index

    call read_example('examples/f90/associate_selector_facts.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'ASSOCIATE selector fixture was rejected')

    associate_index = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        if (trim(get_node_type_at(result%arena, i)) == 'associate') then
            associate_index = i
            exit
        end if
    end do
    call require(associate_index > 0, 'ASSOCIATE node was not found')

    facts = query_associate_selectors(result%arena, associate_index)
    call require(size(facts) == 4, 'ASSOCIATE selector count is wrong')

    ! These expected values are the source contract, not values recovered
    ! from query_storage or from the implementation under test.
    call require(trim(facts(1)%associate_name) == 'component', &
        'component association name is wrong')
    call require(facts(1)%is_resolved .and. facts(1)%is_selector_designator .and. &
        facts(1)%is_alias .and. .not. facts(1)%is_alias_boundary, &
        'component selector identity boundary is wrong')
    call require(facts(1)%selector_path%found .and. &
        size(facts(1)%selector_path%component_names) == 1 .and. &
        trim(facts(1)%selector_path%component_names(1)) == 'payload' .and. &
        facts(1)%selector_path%is_array_element, &
        'component base/path facts are wrong')
    call require(trim(facts(1)%selector_declared_type) == 'type(payload_t)' .and. &
        trim(facts(1)%selector_dynamic_type) == 'payload_t' .and. &
        facts(1)%is_dynamic_type_known .and. facts(1)%has_write_reference .and. &
        facts(1)%association_access_kind == ACCESS_WRITE, &
        'component type/access facts are wrong')

    call require(trim(facts(2)%associate_name) == 'element', &
        'array-element association name is wrong')
    call require(facts(2)%is_resolved .and. facts(2)%is_selector_designator .and. &
        facts(2)%is_alias .and. facts(2)%selector_storage%is_allocatable .and. &
        facts(2)%selector_storage%is_array_element .and. &
        facts(2)%association_access_kind == ACCESS_WRITE, &
        'array-element storage/access facts are wrong')
    call require(facts(2)%selector_path%found .and. &
        facts(2)%selector_path%base_node_index > 0, &
        'array-element component path identity is missing')

    call require(trim(facts(3)%associate_name) == 'pointer', &
        'pointer association name is wrong')
    call require(facts(3)%is_resolved .and. facts(3)%is_alias .and. &
        facts(3)%is_pointer .and. facts(3)%is_alias_boundary .and. &
        facts(3)%is_ambiguous .and. .not. facts(3)%is_dynamic_type_known .and. &
        facts(3)%has_write_reference, &
        'pointer alias boundary was guessed or lost')

    call require(trim(facts(4)%associate_name) == 'expression', &
        'expression association name is wrong')
    call require(facts(4)%is_resolved .and. .not. facts(4)%is_alias .and. &
        .not. facts(4)%is_storage_resolved .and. facts(4)%is_read_only .and. &
        facts(4)%has_read_reference .and. &
        facts(4)%association_access_kind == ACCESS_READ, &
        'expression read-only boundary was guessed or lost')

    print *, 'PASS: ASSOCIATE selector facts contract'

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

end program test_associate_selector_facts
