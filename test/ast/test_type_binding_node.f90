program test_type_binding_node
    use ast_nodes_data, only: type_binding_node, derived_type_node, &
        create_type_binding
    implicit none

    type(type_binding_node) :: binding1, binding2, binding3
    type(derived_type_node) :: dtype

    binding1 = create_type_binding("method", implementation="impl_method")
    if (.not. allocated(binding1%binding_name)) then
        print *, "FAIL: binding_name not allocated"
        stop 1
    end if
    if (binding1%binding_name /= "method") then
        print *, "FAIL: binding_name mismatch"
        stop 1
    end if
    if (.not. allocated(binding1%implementation)) then
        print *, "FAIL: implementation not allocated"
        stop 1
    end if
    if (binding1%implementation /= "impl_method") then
        print *, "FAIL: implementation mismatch"
        stop 1
    end if
    if (binding1%is_generic) then
        print *, "FAIL: is_generic should be false by default"
        stop 1
    end if
    if (binding1%is_final) then
        print *, "FAIL: is_final should be false by default"
        stop 1
    end if
    if (binding1%is_deferred) then
        print *, "FAIL: is_deferred should be false by default"
        stop 1
    end if
    if (.not. binding1%pass_arg) then
        print *, "FAIL: pass_arg should be true by default"
        stop 1
    end if

    binding2 = create_type_binding("generic_op", is_generic=.true., &
        pass_arg=.false.)
    if (binding2%binding_name /= "generic_op") then
        print *, "FAIL: generic binding name mismatch"
        stop 1
    end if
    if (.not. binding2%is_generic) then
        print *, "FAIL: is_generic should be true"
        stop 1
    end if
    if (binding2%pass_arg) then
        print *, "FAIL: pass_arg should be false"
        stop 1
    end if

    binding3 = create_type_binding("destructor", is_final=.true., &
        accessibility="private")
    if (binding3%binding_name /= "destructor") then
        print *, "FAIL: final binding name mismatch"
        stop 1
    end if
    if (.not. binding3%is_final) then
        print *, "FAIL: is_final should be true"
        stop 1
    end if
    if (.not. allocated(binding3%accessibility)) then
        print *, "FAIL: accessibility not allocated"
        stop 1
    end if
    if (binding3%accessibility /= "private") then
        print *, "FAIL: accessibility mismatch"
        stop 1
    end if

    dtype%name = "test_type"
    dtype%has_contains = .true.
    allocate (dtype%binding_indices(3))
    dtype%binding_indices = [1, 2, 3]

    if (.not. dtype%has_contains) then
        print *, "FAIL: has_contains should be true"
        stop 1
    end if
    if (.not. allocated(dtype%binding_indices)) then
        print *, "FAIL: binding_indices not allocated"
        stop 1
    end if
    if (size(dtype%binding_indices) /= 3) then
        print *, "FAIL: binding_indices size mismatch"
        stop 1
    end if

    print *, "PASS: type_binding_node and derived_type_node extensions work"
end program test_type_binding_node
