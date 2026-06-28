program test_if_else_body_type_inference
    ! Regression test for the inference scheduler skipping else-body
    ! statements.  schedule_if_node in
    ! semantic_analyzer_infer_type_locals_part1.inc previously pushed
    ! only condition + then_body_indices onto the worklist; assignments
    ! inside else branches never reached handle_array_assignment, so
    ! call_or_subscript_node targets in the else body kept
    ! is_array_access = .false. even though the same array name was
    ! correctly typed by the then branch.  Downstream consumers (notably
    ! lazy-fortran/ffc lowering) then needed local workarounds to
    ! recover the array-access intent.
    use fortfront, only: compile_frontend_from_string, &
        compiler_frontend_options_t, &
        compiler_frontend_result_t, INPUT_MODE_STANDARD
    use ast_nodes_control, only: if_node
    use ast_nodes_core, only: assignment_node, call_or_subscript_node
    implicit none

    character(:), allocatable :: src
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: frontend_result
    integer :: i, j
    integer :: then_target, else_target
    logical :: ok

    src = 'program test'//new_line('a')// &
        '  integer :: a(3)'//new_line('a')// &
        '  integer :: flag'//new_line('a')// &
        '  flag = 1'//new_line('a')// &
        '  if (flag == 1) then'//new_line('a')// &
        '    a(1) = 11'//new_line('a')// &
        '  else'//new_line('a')// &
        '    a(2) = 22'//new_line('a')// &
        '  end if'//new_line('a')// &
        '  stop a(1)'//new_line('a')// &
        'end program test'

    options = compiler_frontend_options_t()
    options%run_semantics = .true.
    options%input_mode = INPUT_MODE_STANDARD
    call compile_frontend_from_string(src, frontend_result, options)
    if (.not. frontend_result%success()) then
        print *, 'Frontend rejected source: ', &
            trim(frontend_result%diagnostic_text)
        error stop 1
    end if

    then_target = 0
    else_target = 0
    do i = 1, frontend_result%arena%size
        if (.not. allocated(frontend_result%arena%entries(i)%node)) cycle
        select type (n => frontend_result%arena%entries(i)%node)
            type is (if_node)
            if (allocated(n%then_body_indices) .and. &
                size(n%then_body_indices) >= 1) then
                j = n%then_body_indices(1)
                select type (a => frontend_result%arena%entries(j)%node)
                    type is (assignment_node)
                    then_target = a%target_index
                end select
            end if
            if (allocated(n%else_body_indices) .and. &
                size(n%else_body_indices) >= 1) then
                j = n%else_body_indices(1)
                select type (a => frontend_result%arena%entries(j)%node)
                    type is (assignment_node)
                    else_target = a%target_index
                end select
            end if
        end select
    end do

    if (then_target == 0 .or. else_target == 0) then
        print *, 'FAIL: did not locate if branch assignment targets'
        error stop 1
    end if

    ok = .true.
    select type (t => frontend_result%arena%entries(then_target)%node)
        type is (call_or_subscript_node)
        if (.not. t%is_array_access) then
            print *, 'FAIL: then-branch a(1) has is_array_access = .false.'
            ok = .false.
        end if
    end select
    select type (t => frontend_result%arena%entries(else_target)%node)
        type is (call_or_subscript_node)
        if (.not. t%is_array_access) then
            print *, 'FAIL: else-branch a(2) has is_array_access = .false.'
            ok = .false.
        end if
    end select
    if (.not. ok) error stop 1

    print *, 'PASS: both then- and else-body call_or_subscript targets '// &
        'have is_array_access set'
end program test_if_else_body_type_inference
