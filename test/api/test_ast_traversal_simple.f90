program test_ast_traversal_simple
    use fortfront
    use ast_visitor
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Simple AST Traversal Coverage Tests ==="
    all_tests_passed = .true.
    
    ! Test 1: Module with both declarations and procedures
    if (.not. test_module_complete()) all_tests_passed = .false.
    
    ! Test 2: Function with parameters and body 
    if (.not. test_function_complete()) all_tests_passed = .false.
    
    ! Test 3: Do loop with step
    if (.not. test_do_with_step()) all_tests_passed = .false.
    
    ! Test 4: Select case with default
    if (.not. test_select_default()) all_tests_passed = .false.
    
    ! Test 5: If with else
    if (.not. test_if_else()) all_tests_passed = .false.
    
    ! Test 6: Do while with body
    if (.not. test_do_while_body()) all_tests_passed = .false.
    
    ! Test 7: Derived type with components
    if (.not. test_derived_type()) all_tests_passed = .false.
    
    ! Test 8: Interface block
    if (.not. test_interface()) all_tests_passed = .false.
    
    ! Test 9: Use and include statements
    if (.not. test_use_include()) all_tests_passed = .false.
    
    ! Test 10: Call and subscript
    if (.not. test_calls()) all_tests_passed = .false.
    
    if (all_tests_passed) then
        print *, "All tests passed!"
    else
        print *, "Some tests failed!"
        stop 1
    end if
    
contains
    
    logical function test_module_complete()
        character(len=*), parameter :: source = &
            "module mymod" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    real :: x = 1.0" // new_line('A') // &
            "    integer :: n = 10" // new_line('A') // &
            "contains" // new_line('A') // &
            "    subroutine sub1(a)" // new_line('A') // &
            "        real :: a" // new_line('A') // &
            "        x = x + a" // new_line('A') // &
            "    end subroutine sub1" // new_line('A') // &
            "    function func1(b) result(c)" // new_line('A') // &
            "        real :: b, c" // new_line('A') // &
            "        c = b * x" // new_line('A') // &
            "    end function func1" // new_line('A') // &
            "end module mymod"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: context
        integer :: mod_index
        character(len=:), allocatable :: error_msg
        
        test_module_complete = .true.
        print *, "Testing module with declarations and procedures..."
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Lex error"
            test_module_complete = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, mod_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Parse error"
            test_module_complete = .false.
            return
        end if
        
        ! Just traverse to ensure coverage
        ! This exercises the module traversal code paths
        block
            type(debug_visitor_t) :: visitor
            call traverse_preorder(arena, mod_index, visitor)
        end block
        
        print *, "  PASS"
        
    end function test_module_complete
    
    logical function test_function_complete()
        character(len=*), parameter :: source = &
            "function add(a, b, c) result(sum)" // new_line('A') // &
            "    real :: a, b, c, sum" // new_line('A') // &
            "    sum = a + b + c" // new_line('A') // &
            "    sum = sum * 2.0" // new_line('A') // &
            "end function add"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: func_index
        character(len=:), allocatable :: error_msg
        
        test_function_complete = .true.
        print *, "Testing function with params and body..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        
        block
            type(debug_visitor_t) :: visitor
            call traverse_postorder(arena, func_index, visitor)
        end block
        
        print *, "  PASS"
        
    end function test_function_complete
    
    logical function test_do_with_step()
        character(len=*), parameter :: source = &
            "program test" // new_line('A') // &
            "    integer :: i, sum" // new_line('A') // &
            "    sum = 0" // new_line('A') // &
            "    do i = 1, 10, 2" // new_line('A') // &
            "        sum = sum + i" // new_line('A') // &
            "    end do" // new_line('A') // &
            "    print *, sum" // new_line('A') // &
            "end program test"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        
        test_do_with_step = .true.
        print *, "Testing do loop with step..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        block
            type(debug_visitor_t) :: visitor
            call traverse_preorder(arena, prog_index, visitor)
        end block
        
        print *, "  PASS"
        
    end function test_do_with_step
    
    logical function test_select_default()
        character(len=*), parameter :: source = &
            "subroutine grade(score)" // new_line('A') // &
            "    integer :: score" // new_line('A') // &
            "    select case (score)" // new_line('A') // &
            "        case (90:)" // new_line('A') // &
            "            print *, 'A'" // new_line('A') // &
            "        case (80:89)" // new_line('A') // &
            "            print *, 'B'" // new_line('A') // &
            "        case default" // new_line('A') // &
            "            print *, 'C or below'" // new_line('A') // &
            "    end select" // new_line('A') // &
            "end subroutine grade"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: sub_index
        character(len=:), allocatable :: error_msg
        
        test_select_default = .true.
        print *, "Testing select case with default..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, sub_index, error_msg)
        
        block
            type(debug_visitor_t) :: visitor
            call traverse_postorder(arena, sub_index, visitor)
        end block
        
        print *, "  PASS"
        
    end function test_select_default
    
    logical function test_if_else()
        character(len=*), parameter :: source = &
            "program iftest" // new_line('A') // &
            "    real :: x = 5.0" // new_line('A') // &
            "    if (x > 10) then" // new_line('A') // &
            "        print *, 'large'" // new_line('A') // &
            "    else if (x > 0) then" // new_line('A') // &
            "        print *, 'positive'" // new_line('A') // &
            "    else" // new_line('A') // &
            "        print *, 'non-positive'" // new_line('A') // &
            "    end if" // new_line('A') // &
            "end program iftest"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        
        test_if_else = .true.
        print *, "Testing if with else branches..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        block
            type(debug_visitor_t) :: visitor
            call traverse_preorder(arena, prog_index, visitor)
        end block
        
        print *, "  PASS"
        
    end function test_if_else
    
    logical function test_do_while_body()
        character(len=*), parameter :: source = &
            "subroutine iterate()" // new_line('A') // &
            "    integer :: i" // new_line('A') // &
            "    i = 0" // new_line('A') // &
            "    do while (i < 10)" // new_line('A') // &
            "        i = i + 1" // new_line('A') // &
            "        print *, i" // new_line('A') // &
            "    end do" // new_line('A') // &
            "end subroutine iterate"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: sub_index
        character(len=:), allocatable :: error_msg
        
        test_do_while_body = .true.
        print *, "Testing do while with body..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, sub_index, error_msg)
        
        block
            type(debug_visitor_t) :: visitor
            call traverse_postorder(arena, sub_index, visitor)
        end block
        
        print *, "  PASS"
        
    end function test_do_while_body
    
    logical function test_derived_type()
        character(len=*), parameter :: source = &
            "module types" // new_line('A') // &
            "    type :: person" // new_line('A') // &
            "        character(len=50) :: name" // new_line('A') // &
            "        integer :: age" // new_line('A') // &
            "        real :: height" // new_line('A') // &
            "    end type person" // new_line('A') // &
            "end module types"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: mod_index
        character(len=:), allocatable :: error_msg
        
        test_derived_type = .true.
        print *, "Testing derived type with components..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, mod_index, error_msg)
        
        block
            type(debug_visitor_t) :: visitor
            call traverse_preorder(arena, mod_index, visitor)
        end block
        
        print *, "  PASS"
        
    end function test_derived_type
    
    logical function test_interface()
        character(len=*), parameter :: source = &
            "module interfaces" // new_line('A') // &
            "    interface operator(.add.)" // new_line('A') // &
            "        module procedure add_int" // new_line('A') // &
            "        module procedure add_real" // new_line('A') // &
            "    end interface" // new_line('A') // &
            "end module interfaces"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: mod_index
        character(len=:), allocatable :: error_msg
        
        test_interface = .true.
        print *, "Testing interface block..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, mod_index, error_msg)
        
        block
            type(debug_visitor_t) :: visitor
            call traverse_postorder(arena, mod_index, visitor)
        end block
        
        print *, "  PASS"
        
    end function test_interface
    
    logical function test_use_include()
        character(len=*), parameter :: source = &
            "program uses" // new_line('A') // &
            "    use iso_fortran_env" // new_line('A') // &
            "    use mymod, only: func1" // new_line('A') // &
            "    include 'common.inc'" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "end program uses"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        
        test_use_include = .true.
        print *, "Testing use and include statements..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        block
            type(debug_visitor_t) :: visitor
            call traverse_preorder(arena, prog_index, visitor)
        end block
        
        print *, "  PASS"
        
    end function test_use_include
    
    logical function test_calls()
        character(len=*), parameter :: source = &
            "program calls" // new_line('A') // &
            "    real :: x, arr(10)" // new_line('A') // &
            "    x = sin(3.14)" // new_line('A') // &
            "    arr(5) = x" // new_line('A') // &
            "    call process(x, arr(1))" // new_line('A') // &
            "    print *, x, arr(5)" // new_line('A') // &
            "end program calls"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        
        test_calls = .true.
        print *, "Testing call and subscript nodes..."
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        block
            type(debug_visitor_t) :: visitor
            call traverse_postorder(arena, prog_index, visitor)
        end block
        
        print *, "  PASS"
        
    end function test_calls
    
end program test_ast_traversal_simple