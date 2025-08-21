program test_type_system_comprehensive
    use type_system_hm
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Type System Comprehensive Tests ==="
    
    ! Test basic type creation
    call test_primitive_types()
    call test_type_variables()
    call test_function_types()
    call test_array_types()
    
    ! Test type operations
    call test_type_equality()
    call test_type_substitution()
    call test_occurs_check()
    call test_free_vars()
    
    ! Test polymorphic types
    call test_poly_types()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All type system tests passed!"
        stop 0
    else
        print *, "Some type system tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_primitive_types()
        type(mono_type_t) :: int_type, real_type, char_type, logical_type
        
        call test_start("Primitive type creation")
        
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)
        char_type = create_mono_type(TCHAR, char_size=10)
        logical_type = create_mono_type(TLOGICAL)
        
        if (int_type%kind == TINT .and. &
            real_type%kind == TREAL .and. &
            char_type%kind == TCHAR .and. char_type%size == 10 .and. &
            logical_type%kind == TLOGICAL) then
            call test_pass()
        else
            call test_fail("Primitive types not created correctly")
        end if
    end subroutine test_primitive_types
    
    subroutine test_type_variables()
        type(type_var_t) :: var1, var2
        type(mono_type_t) :: var_type
        
        call test_start("Type variable creation")
        
        var1 = create_type_var(1, "a")
        var2 = create_type_var(2, "b")
        var_type = create_mono_type(TVAR, var=var1)
        
        if (var1%id == 1 .and. var1%name == "a" .and. &
            var2%id == 2 .and. var2%name == "b" .and. &
            var_type%kind == TVAR .and. var_type%var%id == 1) then
            call test_pass()
        else
            call test_fail("Type variables not created correctly")
        end if
    end subroutine test_type_variables
    
    subroutine test_function_types()
        type(mono_type_t) :: int_type, real_type, fun_type
        
        call test_start("Function type creation")
        
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)
        fun_type = create_fun_type(int_type, real_type)
        
        if (fun_type%kind == TFUN .and. &
            allocated(fun_type%args) .and. &
            size(fun_type%args) == 2) then
            if (fun_type%args(1)%kind == TINT .and. &
                fun_type%args(2)%kind == TREAL) then
                call test_pass()
            else
                call test_fail("Function type arguments incorrect")
            end if
        else
            call test_fail("Function type not created correctly")
        end if
    end subroutine test_function_types
    
    subroutine test_array_types()
        type(mono_type_t) :: int_type, array_type
        type(mono_type_t), allocatable :: elem_types(:)
        
        call test_start("Array type creation")
        
        int_type = create_mono_type(TINT)
        
        ! Create array with element type
        allocate(elem_types(1))
        elem_types(1) = int_type
        array_type = create_mono_type(TARRAY, args=elem_types)
        array_type%size = 100  ! Set size explicitly
        
        if (array_type%kind == TARRAY .and. &
            array_type%size == 100 .and. &
            allocated(array_type%args) .and. &
            array_type%args(1)%kind == TINT) then
            call test_pass()
        else
            call test_fail("Array type not created correctly")
        end if
    end subroutine test_array_types
    
    subroutine test_type_equality()
        type(mono_type_t) :: int1, int2, real1
        
        call test_start("Type equality")
        
        int1 = create_mono_type(TINT)
        int2 = create_mono_type(TINT)
        real1 = create_mono_type(TREAL)
        
        if ((int1%kind == int2%kind) .and. .not. (int1%kind == real1%kind)) then
            call test_pass()
        else
            call test_fail("Type equality not working correctly")
        end if
    end subroutine test_type_equality
    
    subroutine test_type_substitution()
        type(type_var_t) :: var_a
        type(mono_type_t) :: var_type, int_type, result_type
        type(substitution_t) :: subst
        
        call test_start("Type substitution")
        
        var_a = create_type_var(1, "a")
        var_type = create_mono_type(TVAR, var=var_a)
        int_type = create_mono_type(TINT)
        
        ! Create substitution mapping var_a -> int
        call subst%add(var_a, int_type)
        
        result_type = apply_substitution(subst, var_type)
        
        if (result_type%kind == TINT) then
            call test_pass()
        else
            call test_fail("Type substitution not working")
        end if
    end subroutine test_type_substitution
    
    subroutine test_occurs_check()
        type(type_var_t) :: var_a
        type(mono_type_t) :: var_type, fun_type
        logical :: occurs
        
        call test_start("Occurs check")
        
        var_a = create_type_var(1, "a")
        var_type = create_mono_type(TVAR, var=var_a)
        
        ! Create function type 'a -> int  
        fun_type = create_fun_type(var_type, create_mono_type(TINT))
        
        occurs = occurs_check(var_a, fun_type)
        
        if (occurs) then
            call test_pass()
        else
            call test_fail("Occurs check should detect variable in function type")
        end if
    end subroutine test_occurs_check
    
    subroutine test_free_vars()
        type(type_var_t) :: var_a, var_b
        type(mono_type_t) :: var_type_a, var_type_b, fun_type
        type(type_var_t), allocatable :: free_vars(:)
        
        call test_start("Free type variables")
        
        var_a = create_type_var(1, "a")
        var_b = create_type_var(2, "b")
        var_type_a = create_mono_type(TVAR, var=var_a)
        var_type_b = create_mono_type(TVAR, var=var_b)
        
        ! Create function type 'a -> 'b
        fun_type = create_fun_type(var_type_a, var_type_b)
        
        call free_type_vars(fun_type, free_vars)
        
        if (allocated(free_vars) .and. size(free_vars) >= 2) then
            call test_pass()
        else
            call test_fail("Free type variables not found correctly")
        end if
    end subroutine test_free_vars
    
    subroutine test_poly_types()
        type(type_var_t) :: var_a
        type(mono_type_t) :: var_type, fun_type
        type(poly_type_t) :: poly_type
        
        call test_start("Polymorphic type creation")
        
        var_a = create_type_var(1, "a")
        var_type = create_mono_type(TVAR, var=var_a)
        fun_type = create_fun_type(var_type, var_type)  ! 'a -> 'a
        
        block
            type(type_var_t), allocatable :: forall_vars(:)
            allocate(forall_vars(1))
            forall_vars(1) = var_a
            poly_type = create_poly_type(forall_vars, fun_type)
        end block
        
        if (allocated(poly_type%forall_vars) .and. &
            size(poly_type%forall_vars) == 1 .and. &
            poly_type%mono%kind == TFUN) then
            call test_pass()
        else
            call test_fail("Polymorphic type not created correctly")
        end if
    end subroutine test_poly_types
    
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start
    
    subroutine test_pass()
        print *, " ... PASSED"
        pass_count = pass_count + 1
    end subroutine test_pass
    
    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, " ... FAILED"
        print *, "  Reason: ", reason
    end subroutine test_fail
    
end program test_type_system_comprehensive