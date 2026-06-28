program test_variable_usage_scoped_shadowing
    use fortfront, only: ast_arena_t, create_ast_arena, lex_source, parse_tokens, &
        token_t
    use variable_usage_tracker_module, only: get_scoped_variable_usages, &
        scoped_variable_usage_t
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: error_msg
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    type(scoped_variable_usage_t), allocatable :: usages(:)
    integer :: root_index
    integer :: outer_scope_id
    integer :: inner_scope_id
    integer :: outer_depth
    integer :: inner_depth

    call read_example('examples/f90/variable_usage_shadowed_block.f90', source)
    call lex_source(source, tokens, error_msg)
    call assert_no_error('lexing', error_msg)

    arena = create_ast_arena()
    call parse_tokens(tokens, arena, root_index, error_msg)
    if (root_index <= 0) call fail('parsing failed: ' // trim(error_msg))

    usages = get_scoped_variable_usages(arena, root_index)
    call find_shadowed_declarations(usages, outer_scope_id, inner_scope_id, &
        outer_depth, inner_depth)

    if (outer_scope_id == inner_scope_id) then
        call fail('shadowed value declarations share a scope id')
    end if
    if (inner_depth <= outer_depth) then
        call fail('inner block declaration did not get deeper scope depth')
    end if
    if (count_references_in_scope(usages, 'value', outer_scope_id) == 0) then
        call fail('outer value scope has no identifier references')
    end if
    if (count_references_in_scope(usages, 'value', inner_scope_id) == 0) then
        call fail('inner value scope has no identifier references')
    end if

    print '(a)', 'Scoped variable usage shadowing test passed'

contains

    include '../common/read_example.inc'

    subroutine find_shadowed_declarations(usages, outer_scope_id, inner_scope_id, &
            outer_depth, inner_depth)
        type(scoped_variable_usage_t), intent(in) :: usages(:)
        integer, intent(out) :: outer_scope_id
        integer, intent(out) :: inner_scope_id
        integer, intent(out) :: outer_depth
        integer, intent(out) :: inner_depth
        integer :: i
        integer :: declaration_count

        outer_scope_id = 0
        inner_scope_id = 0
        outer_depth = 0
        inner_depth = 0
        declaration_count = 0

        do i = 1, size(usages)
            if (.not. usages(i)%is_declaration) cycle
            if (.not. allocated(usages(i)%name)) cycle
            if (usages(i)%name /= 'value') cycle

            declaration_count = declaration_count + 1
            if (declaration_count == 1) then
                outer_scope_id = usages(i)%scope_id
                outer_depth = usages(i)%scope_depth
            else if (declaration_count == 2) then
                inner_scope_id = usages(i)%scope_id
                inner_depth = usages(i)%scope_depth
            end if
        end do

        if (declaration_count /= 2) then
            write (error_unit, '(a,i0)') &
                'FAIL: expected 2 value declarations, got ', declaration_count
            error stop 1
        end if
    end subroutine find_shadowed_declarations

    integer function count_references_in_scope(usages, name, scope_id)
        type(scoped_variable_usage_t), intent(in) :: usages(:)
        character(len=*), intent(in) :: name
        integer, intent(in) :: scope_id
        integer :: i

        count_references_in_scope = 0
        do i = 1, size(usages)
            if (usages(i)%is_declaration) cycle
            if (.not. allocated(usages(i)%name)) cycle
            if (usages(i)%name == name .and. usages(i)%scope_id == scope_id) then
                count_references_in_scope = count_references_in_scope + 1
            end if
        end do
    end function count_references_in_scope

    subroutine assert_no_error(stage, message)
        character(len=*), intent(in) :: stage
        character(len=*), intent(in) :: message

        if (len_trim(message) > 0) call fail(trim(stage) // ' failed: ' // message)
    end subroutine assert_no_error

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(a)') 'FAIL: ' // trim(message)
        error stop 1
    end subroutine fail

end program test_variable_usage_scoped_shadowing
