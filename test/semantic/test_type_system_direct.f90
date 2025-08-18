program test_type_system_direct
    use type_system_hm, only: type_var_t, mono_type_t, poly_type_t, substitution_t, &
                              type_env_t, create_type_var, create_mono_type, &
                              create_poly_type, create_fun_type, occurs_check, &
                              free_type_vars, apply_substitution, compose_substitutions, &
                              TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN, TARRAY
    implicit none

    integer :: total_tests, passed_tests
    type(type_var_t) :: var1, var2
    type(mono_type_t) :: int_type, real_type, var_type, fun_type, char_type
    type(poly_type_t) :: scheme
    type(substitution_t) :: subst
    type(type_env_t) :: env
    type(type_var_t), allocatable :: free_vars(:)
    logical :: result_bool

    total_tests = 0
    passed_tests = 0

    print *, "=== Type System Direct Function Tests ==="
    print *, ""

    ! Test 1: Create type variables
    call test_start("Create type variables")
    var1 = create_type_var(1, "a")
    var2 = create_type_var(2, "b")
    if (var1%id == 1 .and. var1%name == "a" .and. &
        var2%id == 2 .and. var2%name == "b") then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: var1(id=1,name='a'), var2(id=2,name='b')"
        print *, "  Got: var1(id=", var1%id, ",name='", var1%name, &
                 "'), var2(id=", var2%id, ",name='", var2%name, "')"
    end if

    ! Test 2: Create basic monomorphic types
    call test_start("Create basic types")
    int_type = create_mono_type(TINT)
    real_type = create_mono_type(TREAL)
    char_type = create_mono_type(TCHAR, char_size=10)
    if (int_type%data%kind == TINT .and. real_type%data%kind == TREAL .and. &
        char_type%data%kind == TCHAR .and. char_type%data%size == 10) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: int(kind=", TINT, "), real(kind=", TREAL, &
                 "), char(kind=", TCHAR, ",size=10)"
        print *, "  Got: int(kind=", int_type%data%kind, "), real(kind=", &
                 real_type%data%kind, "), char(kind=", char_type%data%kind, &
                 ",size=", char_type%data%size, ")"
    end if

    ! Test 3: Create type variable type
    call test_start("Create type variable type")
    var_type = create_mono_type(TVAR, var=var1)
    if (var_type%data%kind == TVAR .and. var_type%data%var%id == 1) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: kind=", TVAR, ", var%id=1"
        print *, "  Got: kind=", var_type%data%kind, ", var%id=", var_type%data%var%id
    end if

    ! Test 4: Create function type (simplified type system)
    call test_start("Create function type")
    fun_type = create_fun_type(int_type, real_type)  ! int -> real
    if (fun_type%data%kind == TFUN) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: kind=", TFUN
        print *, "  Got: kind=", fun_type%data%kind
    end if

    ! Test 5: Type equality
    call test_start("Type equality")
    if (int_type%equals(create_mono_type(TINT)) .and. &
        .not. int_type%equals(real_type)) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: int==int is true, int==real is false"
    end if

    ! Test 6: Create polymorphic type scheme
    call test_start("Create polymorphic type")
    scheme = create_poly_type([var1], var_type)  ! forall a. a
    if (allocated(scheme%forall) .and. size(scheme%forall) == 1 .and. &
        scheme%forall(1)%id == 1 .and. scheme%mono%data%kind == TVAR) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: forall with 1 var, mono is type var"
        if (allocated(scheme%forall)) then
            print *, "  Got: forall size=", size(scheme%forall), &
                     ", mono%kind=", scheme%mono%data%kind
        else
            print *, "  Got: forall not allocated"
        end if
    end if

    ! Test 7: Occurs check (positive case)
    call test_start("Occurs check positive")
    result_bool = occurs_check(var1, var_type)  ! var1 occurs in type of var1
    if (result_bool) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: var1 occurs in var_type(var1)"
    end if

    ! Test 8: Occurs check (negative case)
    call test_start("Occurs check negative")
    result_bool = occurs_check(var1, int_type)  ! var1 does not occur in int
    if (.not. result_bool) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: var1 does not occur in int_type"
    end if

    ! Test 9: Free type variables in simple type
    call test_start("Free variables simple")
    call free_type_vars(int_type, free_vars)
    if (allocated(free_vars) .and. size(free_vars) == 0) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: no free vars in int_type"
        if (allocated(free_vars)) then
            print *, "  Got: free_vars size=", size(free_vars)
        else
            print *, "  Got: free_vars not allocated"
        end if
    end if

    ! Test 10: Free type variables in variable type
    call test_start("Free variables in type var")
    call free_type_vars(var_type, free_vars)
    if (allocated(free_vars) .and. size(free_vars) == 1 .and. &
        free_vars(1)%id == 1) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: 1 free var with id=1"
        if (allocated(free_vars)) then
            print *, "  Got: free_vars size=", size(free_vars)
            if (size(free_vars) > 0) then
                print *, "       first var id=", free_vars(1)%id
            end if
        else
            print *, "  Got: free_vars not allocated"
        end if
    end if

    ! Test 11: Substitution operations
    call test_start("Substitution operations")
    subst%count = 0  ! Initialize
    call subst%add(var1, int_type)  ! var1 := int
    if (subst%count == 1) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: subst%count=1"
        print *, "  Got: subst%count=", subst%count
    end if

    ! Test 12: Type environment operations
    call test_start("Type environment")
    env%count = 0
    env%capacity = 10
    allocate(character(len=256) :: env%names(env%capacity))
    allocate(env%schemes(env%capacity))
    
    call env%extend("x", scheme)
    if (env%count == 1 .and. env%names(1) == "x") then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: env%count=1, names(1)='x'"
        print *, "  Got: env%count=", env%count
        if (env%count > 0) then
            print *, "       names(1)='", trim(env%names(1)), "'"
        end if
    end if

    ! Test 13: Type to string conversion
    call test_start("Type to string")
    if (len_trim(int_type%to_string()) > 0 .and. &
        len_trim(fun_type%to_string()) > 0) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: non-empty string representations"
        print *, "  Got: int='", trim(int_type%to_string()), &
                 "', fun='", trim(fun_type%to_string()), "'"
    end if

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All type system direct tests passed!"
        stop 0
    else
        print *, "Some type system tests failed!"
        stop 1
    end if

contains

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_pass()
        print *, " ... PASSED"
        passed_tests = passed_tests + 1
    end subroutine test_pass

    subroutine test_fail()
        print *, " ... FAILED"
    end subroutine test_fail

end program test_type_system_direct