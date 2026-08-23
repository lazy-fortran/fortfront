program test_compiler_scope_resolution
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_nodes_associate, only: associate_node, block_construct_node
    use ast_nodes_core, only: program_node
    use ast_nodes_data, only: declaration_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, declaration_binding_t, &
        procedure_signature_query_t, query_procedure_signature, &
        resolve_name_in_scope, resolve_name_at_node, &
        BINDING_DECLARATION, BINDING_NAMED_CONSTANT, BINDING_FUNCTION, &
        BINDING_GENERIC_INTERFACE, BINDING_DERIVED_TYPE, &
        BINDING_ASSOCIATE_NAME, &
        ASSOCIATION_DIRECT, ASSOCIATION_HOST, ASSOCIATION_USE
    implicit none

    call test_host_parameter()
    call test_later_declaration()
    call test_compound_declaration_identity()
    call test_block_shadow()
    call test_associate_scope()
    call test_associate_selector_local_binding()
    call test_nested_block_shadow()
    call test_use_rename()
    call test_nested_associate_selector_binding()
    call test_nested_associate_owner_boundary()
    call test_module_accessibility()
    call test_procedure_and_generic()
    call test_derived_dummy_type_identity()
    print *, 'PASS: compiler scope resolution queries'

contains

    subroutine test_host_parameter()
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        integer :: subroutine_index
        integer :: decl_index
        integer :: array_decl_index
        integer :: bound_index
        integer :: program_index

        call compile_standard( &
            'program p'//new_line('a')// &
            '  implicit none'//new_line('a')// &
            '  integer, parameter :: n = 4'//new_line('a')// &
            'contains'//new_line('a')// &
            '  subroutine s()'//new_line('a')// &
            '    integer :: a(n)'//new_line('a')// &
            '  end subroutine s'//new_line('a')// &
            'end program p', result)
        subroutine_index = find_subroutine(result, 's')
        program_index = find_program(result, 'p')
        decl_index = find_declaration(result, 'n')
        array_decl_index = find_declaration(result, 'a')
        bound_index = first_dimension_index(result, array_decl_index)
        call resolve_name_in_scope(result%arena, subroutine_index, 'n', binding, &
            error_msg)
        call require_binding(error_msg, binding, decl_index, BINDING_NAMED_CONSTANT, &
            ASSOCIATION_HOST, 'host parameter n')
        if (binding%scope_node_index /= program_index) then
            write (error_unit, '(A)') 'FAIL: host binding has wrong owning scope'
            error stop 1
        end if
        call resolve_name_at_node(result%arena, array_decl_index, 'n', binding, &
            error_msg)
        call require_binding(error_msg, binding, decl_index, BINDING_NAMED_CONSTANT, &
            ASSOCIATION_HOST, 'host parameter n at declaration')
        call resolve_name_at_node(result%arena, bound_index, 'n', binding, &
            error_msg)
        call require_binding(error_msg, binding, decl_index, BINDING_NAMED_CONSTANT, &
            ASSOCIATION_HOST, 'host parameter n at bound')
    end subroutine test_host_parameter

    subroutine test_later_declaration()
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        integer :: array_decl_index
        integer :: parameter_index

        call compile_standard( &
            'program p'//new_line('a')// &
            '  integer :: a(n)'//new_line('a')// &
            '  integer, parameter :: n = 4'//new_line('a')// &
            'end program p', result)
        array_decl_index = find_declaration(result, 'a')
        parameter_index = find_declaration(result, 'n')
        call resolve_name_at_node(result%arena, array_decl_index, 'n', binding, &
            error_msg)
        call require_binding(error_msg, binding, parameter_index, &
            BINDING_NAMED_CONSTANT, ASSOCIATION_DIRECT, 'later parameter n')
    end subroutine test_later_declaration

    subroutine test_compound_declaration_identity()
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: a_binding
        type(declaration_binding_t) :: b_binding
        character(len=:), allocatable :: error_msg
        integer :: program_index

        call compile_standard( &
            'program p'//new_line('a')// &
            '  integer :: a, b'//new_line('a')// &
            'end program p', result)
        program_index = find_program(result, 'p')
        call resolve_name_in_scope(result%arena, program_index, 'a', a_binding, &
            error_msg)
        call require_found(error_msg, a_binding, BINDING_DECLARATION, &
            ASSOCIATION_DIRECT, 'compound declaration a')
        call resolve_name_in_scope(result%arena, program_index, 'b', b_binding, &
            error_msg)
        call require_found(error_msg, b_binding, BINDING_DECLARATION, &
            ASSOCIATION_DIRECT, 'compound declaration b')
        if (a_binding%declaration_node_index /= &
            b_binding%declaration_node_index) then
            write (error_unit, '(A)') 'FAIL: compound names have different nodes'
            error stop 1
        end if
        if (a_binding%scope_node_index /= b_binding%scope_node_index) then
            write (error_unit, '(A)') 'FAIL: compound names have different scopes'
            error stop 1
        end if
        if (a_binding%declaration_entity_index == &
            b_binding%declaration_entity_index) then
            write (error_unit, '(A)') 'FAIL: compound names share one identity'
            error stop 1
        end if
    end subroutine test_compound_declaration_identity

    subroutine test_block_shadow()
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: inner_binding
        type(declaration_binding_t) :: outer_binding
        character(len=:), allocatable :: error_msg
        integer :: block_index
        integer :: program_index

        call compile_standard( &
            'program p'//new_line('a')// &
            '  integer :: x'//new_line('a')// &
            '  block'//new_line('a')// &
            '    integer :: x'//new_line('a')// &
            '  end block'//new_line('a')// &
            'end program p', result)
        block_index = find_block(result)
        program_index = find_program(result, 'p')
        call resolve_name_in_scope(result%arena, block_index, 'x', inner_binding, &
            error_msg)
        call require_binding(error_msg, inner_binding, &
            inner_binding%declaration_node_index, &
            BINDING_DECLARATION, ASSOCIATION_DIRECT, 'BLOCK shadow x')
        call resolve_name_in_scope(result%arena, program_index, 'x', outer_binding, &
            error_msg)
        call require_binding(error_msg, outer_binding, &
            outer_binding%declaration_node_index, &
            BINDING_DECLARATION, ASSOCIATION_DIRECT, 'host x')
        if (inner_binding%declaration_node_index == &
            outer_binding%declaration_node_index) then
            write (error_unit, '(A)') 'FAIL: BLOCK shadow reused host binding'
            error stop 1
        end if
        if (inner_binding%scope_node_index /= block_index) then
            write (error_unit, '(A)') 'FAIL: BLOCK shadow has wrong owning scope'
            error stop 1
        end if
    end subroutine test_block_shadow

    subroutine test_nested_block_shadow()
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: outer_binding
        type(declaration_binding_t) :: inner_binding
        character(len=:), allocatable :: error_msg
        integer :: outer_block
        integer :: inner_block

        call compile_standard( &
            'program p'//new_line('a')// &
            '  integer :: x'//new_line('a')// &
            '  block'//new_line('a')// &
            '    integer :: x'//new_line('a')// &
            '    block'//new_line('a')// &
            '      integer :: x'//new_line('a')// &
            '    end block'//new_line('a')// &
            '  end block'//new_line('a')// &
            'end program p', result)
        outer_block = find_block_at_line(result, 3)
        inner_block = find_block_at_line(result, 5)
        if (result%arena%entries(inner_block)%parent_index /= outer_block) then
            write (error_unit, '(A)') 'FAIL: nested BLOCK has wrong parent scope'
            error stop 1
        end if
        call resolve_name_in_scope(result%arena, outer_block, 'x', outer_binding, &
            error_msg)
        call require_found(error_msg, outer_binding, BINDING_DECLARATION, &
            ASSOCIATION_DIRECT, 'outer BLOCK x')
        call resolve_name_in_scope(result%arena, inner_block, 'x', inner_binding, &
            error_msg)
        call require_found(error_msg, inner_binding, BINDING_DECLARATION, &
            ASSOCIATION_DIRECT, 'inner BLOCK x')
        if (inner_binding%declaration_node_index == &
            outer_binding%declaration_node_index) then
            write (error_unit, '(A)') 'FAIL: nested BLOCK reused outer binding'
            error stop 1
        end if
    end subroutine test_nested_block_shadow

    subroutine test_associate_scope()
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: x_binding
        type(declaration_binding_t) :: y_binding
        type(declaration_binding_t) :: selector_binding
        type(declaration_binding_t) :: outer_binding
        character(len=:), allocatable :: error_msg
        integer :: associate_index
        integer :: program_index
        integer :: selector_index

        call compile_standard( &
            'program p'//new_line('a')// &
            '  integer :: x'//new_line('a')// &
            '  associate (x => 7, y => x)'//new_line('a')// &
            '    print *, x + y'//new_line('a')// &
            '  end associate'//new_line('a')// &
            'end program p', result)
        associate_index = find_associate(result)
        program_index = find_program(result, 'p')
        call resolve_name_in_scope(result%arena, associate_index, 'x', x_binding, &
            error_msg)
        call require_binding(error_msg, x_binding, associate_index, &
            BINDING_ASSOCIATE_NAME, ASSOCIATION_DIRECT, 'associate x')
        call resolve_name_in_scope(result%arena, associate_index, 'y', y_binding, &
            error_msg)
        call require_binding(error_msg, y_binding, associate_index, &
            BINDING_ASSOCIATE_NAME, ASSOCIATION_DIRECT, 'associate y')
        if (x_binding%declaration_entity_index == &
            y_binding%declaration_entity_index) then
            write (error_unit, '(A)') 'FAIL: associate names share one identity'
            error stop 1
        end if
        select type (node => result%arena%entries(associate_index)%node)
            type is (associate_node)
            selector_index = node%associations(2)%expr_index
        class default
            error stop 'FAIL: associate query has wrong node type'
        end select
        call resolve_name_at_node(result%arena, selector_index, 'x', &
            selector_binding, error_msg)
        call resolve_name_in_scope(result%arena, program_index, 'x', outer_binding, &
            error_msg)
        if (selector_binding%declaration_node_index /= &
            outer_binding%declaration_node_index) then
            write (error_unit, '(A)') 'FAIL: selector sees an associate name'
            error stop 1
        end if
    end subroutine test_associate_scope

    subroutine test_associate_selector_local_binding()
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: scope_binding
        type(declaration_binding_t) :: selector_binding
        character(len=:), allocatable :: error_msg
        integer :: function_index
        integer :: associate_index
        integer :: selector_index

        call compile_standard( &
            'program one_level'//new_line('a')// &
            '  print *, evaluate(2.0)'//new_line('a')// &
            'contains'//new_line('a')// &
            '  pure function evaluate(value) result(result_value)'//new_line('a')// &
            '    real, intent(in) :: value'//new_line('a')// &
            '    real :: result_value'//new_line('a')// &
            '    associate(alias => value + 1.0)'//new_line('a')// &
            '      result_value = alias'//new_line('a')// &
            '    end associate'//new_line('a')// &
            '  end function evaluate'//new_line('a')// &
            'end program one_level', result)

        function_index = find_function(result, 'evaluate')
        associate_index = find_associate(result)
        select type (node => result%arena%entries(associate_index)%node)
            type is (associate_node)
            selector_index = node%associations(1)%expr_index
        class default
            error stop 'FAIL: one-level ASSOCIATE has wrong node type'
        end select

        call resolve_name_in_scope(result%arena, function_index, 'value', &
            scope_binding, error_msg)
        call require_found(error_msg, scope_binding, BINDING_DECLARATION, &
            ASSOCIATION_DIRECT, 'one-level local dummy')
        call resolve_name_at_node(result%arena, selector_index, 'value', &
            selector_binding, error_msg)
        call require_binding(error_msg, selector_binding, &
            scope_binding%declaration_node_index, BINDING_DECLARATION, &
            ASSOCIATION_DIRECT, 'one-level ASSOCIATE local dummy selector')
    end subroutine test_associate_selector_local_binding

    subroutine test_nested_associate_selector_binding()
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        integer :: inner_associate
        integer :: function_index
        integer :: selector_index
        integer :: i

        call compile_standard( &
            'program g1'//new_line('a')// &
            '  implicit none'//new_line('a')// &
            '  real :: x(4), y(4)'//new_line('a')// &
            '  integer :: i'//new_line('a')// &
            '  do i = 1, 4'//new_line('a')// &
            '    y(i) = compute(x(i))'//new_line('a')// &
            '  end do'//new_line('a')// &
            'contains'//new_line('a')// &
            '  pure function compute(x) result(r)'//new_line('a')// &
            '    real, intent(in) :: x'//new_line('a')// &
            '    real :: r'//new_line('a')// &
            '    associate(n => 1)'//new_line('a')// &
            '      associate(z => x + real(n))'//new_line('a')// &
            '        r = z'//new_line('a')// &
            '      end associate'//new_line('a')// &
            '    end associate'//new_line('a')// &
            '  end function compute'//new_line('a')// &
            'end program g1', result)

        inner_associate = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
                type is (associate_node)
                if (result%arena%entries(i)%parent_index <= 0) cycle
                select type (parent_node => &
                        result%arena%entries(result%arena%entries(i)%parent_index)%node)
                    type is (associate_node)
                    inner_associate = i
                end select
            end select
        end do
        if (inner_associate <= 0) then
            write (error_unit, '(A)') 'FAIL: nested ASSOCIATE not found'
            error stop 1
        end if
        function_index = find_function(result, 'compute')
        select type (node => result%arena%entries(inner_associate)%node)
            type is (associate_node)
            selector_index = node%associations(1)%expr_index
        class default
            error stop 'FAIL: nested ASSOCIATE has wrong node type'
        end select
        call resolve_name_at_node(result%arena, selector_index, 'x', binding, &
            error_msg)
        call require_found(error_msg, binding, BINDING_DECLARATION, &
            ASSOCIATION_DIRECT, 'nested ASSOCIATE local dummy')
        if (binding%scope_node_index /= function_index) then
            write (error_unit, '(A)') &
                'FAIL: nested ASSOCIATE dummy binding escaped its procedure'
            error stop 1
        end if
    end subroutine test_nested_associate_selector_binding

    subroutine test_nested_associate_owner_boundary()
        ! A nested selector must preserve both sides of the ownership
        ! boundary: a local dummy remains DIRECT, while a same-named host
        ! variable remains HOST. The declaration node and entity identities
        ! must agree with the corresponding scope query in both cases.
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: dummy_scope_binding
        type(declaration_binding_t) :: host_scope_binding
        type(declaration_binding_t) :: dummy_selector_binding
        type(declaration_binding_t) :: host_selector_binding
        character(len=:), allocatable :: error_msg
        integer :: outer_associate
        integer :: inner_associate
        integer :: outer_selector
        integer :: inner_selector
        integer :: function_index
        integer :: program_index
        integer :: i

        call compile_standard( &
            'program owner_boundary'//new_line('a')// &
            '  implicit none'//new_line('a')// &
            '  real :: x, y'//new_line('a')// &
            '  y = compute(x)'//new_line('a')// &
            '  print *, y'//new_line('a')// &
            'contains'//new_line('a')// &
            '  pure function compute(value) result(r)'//new_line('a')// &
            '    real, intent(in) :: value'//new_line('a')// &
            '    real :: r'//new_line('a')// &
            '    associate(host_alias => x)'//new_line('a')// &
            '      associate(sum => value + host_alias)'//new_line('a')// &
            '        r = sum'//new_line('a')// &
            '      end associate'//new_line('a')// &
            '    end associate'//new_line('a')// &
            '  end function compute'//new_line('a')// &
            'end program owner_boundary', result)

        outer_associate = 0
        inner_associate = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
                type is (associate_node)
                if (result%arena%entries(i)%parent_index <= 0) cycle
                select type (parent_node => result%arena%entries( &
                        result%arena%entries(i)%parent_index)%node)
                    type is (associate_node)
                    inner_associate = i
                    outer_associate = result%arena%entries(i)%parent_index
                end select
            end select
        end do
        if (outer_associate <= 0 .or. inner_associate <= 0) then
            write (error_unit, '(A)') &
                'FAIL: owner-boundary nested ASSOCIATE not found'
            error stop 1
        end if

        function_index = find_function(result, 'compute')
        program_index = find_program(result, 'owner_boundary')
        call resolve_name_in_scope(result%arena, function_index, 'value', &
            dummy_scope_binding, error_msg)
        call require_found(error_msg, dummy_scope_binding, BINDING_DECLARATION, &
            ASSOCIATION_DIRECT, 'owner-boundary local dummy')
        call resolve_name_in_scope(result%arena, program_index, 'x', &
            host_scope_binding, error_msg)
        call require_found(error_msg, host_scope_binding, BINDING_DECLARATION, &
            ASSOCIATION_DIRECT, 'owner-boundary host variable')

        select type (node => result%arena%entries(outer_associate)%node)
            type is (associate_node)
            outer_selector = node%associations(1)%expr_index
        class default
            error stop 'FAIL: outer ASSOCIATE has wrong node type'
        end select
        select type (node => result%arena%entries(inner_associate)%node)
            type is (associate_node)
            inner_selector = node%associations(1)%expr_index
        class default
            error stop 'FAIL: inner ASSOCIATE has wrong node type'
        end select

        call resolve_name_at_node(result%arena, outer_selector, 'x', &
            host_selector_binding, error_msg)
        call require_binding(error_msg, host_selector_binding, &
            host_scope_binding%declaration_node_index, BINDING_DECLARATION, &
            ASSOCIATION_HOST, 'owner-boundary host selector')
        if (host_selector_binding%scope_node_index /= program_index) then
            write (error_unit, '(A)') &
                'FAIL: host selector owner escaped the host program'
            error stop 1
        end if

        call resolve_name_at_node(result%arena, inner_selector, 'value', &
            dummy_selector_binding, error_msg)
        call require_binding(error_msg, dummy_selector_binding, &
            dummy_scope_binding%declaration_node_index, BINDING_DECLARATION, &
            ASSOCIATION_DIRECT, 'owner-boundary local selector')
        if (dummy_selector_binding%scope_node_index /= function_index) then
            write (error_unit, '(A)') &
                'FAIL: local selector owner escaped its function'
            error stop 1
        end if
        if (dummy_selector_binding%declaration_entity_index /= &
            dummy_scope_binding%declaration_entity_index) then
            write (error_unit, '(A)') &
                'FAIL: local selector changed declaration entity identity'
            error stop 1
        end if
        if (host_selector_binding%declaration_entity_index /= &
            host_scope_binding%declaration_entity_index) then
            write (error_unit, '(A)') &
                'FAIL: host selector changed declaration entity identity'
            error stop 1
        end if
    end subroutine test_nested_associate_owner_boundary

    subroutine test_use_rename()
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        integer :: program_index
        integer :: decl_index

        call compile_standard( &
            'module m'//new_line('a')// &
            '  integer, parameter :: remote = 7'//new_line('a')// &
            'end module m'//new_line('a')// &
            'program p'//new_line('a')// &
            '  use m, only: local => remote'//new_line('a')// &
            '  integer :: a(local)'//new_line('a')// &
            'end program p', result)
        program_index = find_program(result, 'p')
        decl_index = find_declaration(result, 'remote')
        call resolve_name_in_scope(result%arena, program_index, 'local', binding, &
            error_msg)
        call require_binding(error_msg, binding, decl_index, BINDING_NAMED_CONSTANT, &
            ASSOCIATION_USE, 'use-renamed local')
        if (trim(binding%module_name) /= 'm') then
            write (error_unit, '(A)') 'FAIL: use binding module_name mismatch'
            error stop 1
        end if
        if (trim(binding%remote_name) /= 'remote') then
            write (error_unit, '(A)') 'FAIL: use binding remote_name mismatch'
            error stop 1
        end if
        if (binding%scope_node_index /= find_module_scope(result)) then
            write (error_unit, '(A)') 'FAIL: use binding has wrong owning scope'
            error stop 1
        end if
    end subroutine test_use_rename

    subroutine test_module_accessibility()
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        integer :: program_index

        call compile_standard( &
            'module m'//new_line('a')// &
            '  private'//new_line('a')// &
            '  public :: exposed'//new_line('a')// &
            '  integer, parameter :: hidden = 3'//new_line('a')// &
            '  integer, parameter :: exposed = 4'//new_line('a')// &
            'end module m'//new_line('a')// &
            'program p'//new_line('a')// &
            '  use m'//new_line('a')// &
            'end program p', result)
        program_index = find_program(result, 'p')
        call resolve_name_in_scope(result%arena, program_index, 'hidden', binding, &
            error_msg)
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: '//trim(error_msg)
            error stop 1
        end if
        if (binding%found) then
            write (error_unit, '(A)') 'FAIL: private module name resolved through use'
            error stop 1
        end if
        call resolve_name_in_scope(result%arena, program_index, 'exposed', binding, &
            error_msg)
        call require_found(error_msg, binding, BINDING_NAMED_CONSTANT, &
            ASSOCIATION_USE, 'public module name')
        call resolve_name_in_scope(result%arena, program_index, 'absent', binding, &
            error_msg)
        if (len_trim(error_msg) > 0 .or. binding%found) then
            write (error_unit, '(A)') 'FAIL: absent name produced a binding'
            error stop 1
        end if
    end subroutine test_module_accessibility

    subroutine test_procedure_and_generic()
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        integer :: module_index

        call compile_standard( &
            'module m'//new_line('a')// &
            '  interface g'//new_line('a')// &
            '    module procedure f'//new_line('a')// &
            '  end interface'//new_line('a')// &
            'contains'//new_line('a')// &
            '  integer function f(x)'//new_line('a')// &
            '    integer, intent(in) :: x'//new_line('a')// &
            '    f = x'//new_line('a')// &
            '  end function f'//new_line('a')// &
            'end module m', result)
        module_index = find_module_scope(result)
        call resolve_name_in_scope(result%arena, module_index, 'f', binding, &
            error_msg)
        call require_found(error_msg, binding, BINDING_FUNCTION, &
            ASSOCIATION_DIRECT, 'module procedure f')
        call resolve_name_in_scope(result%arena, module_index, 'g', binding, &
            error_msg)
        call require_found(error_msg, binding, BINDING_GENERIC_INTERFACE, &
            ASSOCIATION_DIRECT, 'generic interface g')
    end subroutine test_procedure_and_generic

    subroutine test_derived_dummy_type_identity()
        type(compiler_frontend_result_t) :: result
        type(declaration_binding_t) :: producer_type
        type(declaration_binding_t) :: consumer_type
        type(procedure_signature_query_t) :: signature
        character(len=:), allocatable :: error_msg
        integer :: producer_index
        integer :: consumer_index
        integer :: procedure_index

        call compile_standard( &
            'module producer'//new_line('a')// &
            '  type :: payload_t'//new_line('a')// &
            '    integer :: value'//new_line('a')// &
            '  end type payload_t'//new_line('a')// &
            'contains'//new_line('a')// &
            '  subroutine consume(payload)'//new_line('a')// &
            '    type(payload_t), intent(inout) :: payload'//new_line('a')// &
            '  end subroutine consume'//new_line('a')// &
            'end module producer'//new_line('a')// &
            'module consumer'//new_line('a')// &
            '  use producer, only: payload_t, consume'//new_line('a')// &
            'contains'//new_line('a')// &
            '  subroutine caller(payload)'//new_line('a')// &
            '    type(payload_t), intent(inout) :: payload'//new_line('a')// &
            '    call consume(payload)'//new_line('a')// &
            '  end subroutine caller'//new_line('a')// &
            'end module consumer', result)

        producer_index = find_module_scope(result)
        consumer_index = find_module_scope_named(result, 'consumer')
        procedure_index = find_subroutine(result, 'consume')
        call resolve_name_in_scope(result%arena, producer_index, 'payload_t', &
            producer_type, error_msg)
        call require_binding(error_msg, producer_type, &
            producer_type%declaration_node_index, BINDING_DERIVED_TYPE, &
            ASSOCIATION_DIRECT, 'producer derived type')
        call resolve_name_in_scope(result%arena, consumer_index, 'payload_t', &
            consumer_type, error_msg)
        call require_binding(error_msg, consumer_type, &
            producer_type%declaration_node_index, BINDING_DERIVED_TYPE, &
            ASSOCIATION_USE, 'imported derived type')

        signature = query_procedure_signature(result%arena, procedure_index)
        if (.not. signature%found .or. signature%dummy_count /= 1) then
            write (error_unit, '(A)') &
                'FAIL: derived dummy procedure signature was not found'
            error stop 1
        end if
        if (.not. signature%dummies(1)%derived_type_binding%found .or. &
            signature%dummies(1)%derived_type_binding%binding_kind /= &
            BINDING_DERIVED_TYPE .or. &
            signature%dummies(1)%derived_type_binding%declaration_node_index /= &
            producer_type%declaration_node_index .or. &
            signature%dummies(1)%derived_type_binding%scope_node_index /= &
            producer_index) then
            write (error_unit, '(A)') &
                'FAIL: derived dummy lost its defining type identity'
            error stop 1
        end if
    end subroutine test_derived_dummy_type_identity

    subroutine compile_standard(source, result)
        character(len=*), intent(in) :: source
        type(compiler_frontend_result_t), intent(out) :: result
        type(compiler_frontend_options_t) :: options

        options = compiler_frontend_options_t()
        options%input_mode = INPUT_MODE_STANDARD
        options%run_semantics = .true.
        call compile_frontend_from_string(source, result, options)
        if (result%success()) return
        write (error_unit, '(A)') 'FAIL: frontend rejected test source'
        if (allocated(result%error_msg)) write (error_unit, '(A)') result%error_msg
        error stop 1
    end subroutine compile_standard

    subroutine require_binding(error_msg, binding, node_index, kind, association, &
            label)
        character(len=*), intent(in) :: error_msg
        type(declaration_binding_t), intent(in) :: binding
        integer, intent(in) :: node_index
        integer, intent(in) :: kind
        integer, intent(in) :: association
        character(len=*), intent(in) :: label

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: '//trim(error_msg)
            error stop 1
        end if
        if (.not. binding%found) then
            write (error_unit, '(A)') 'FAIL: unresolved '//trim(label)
            error stop 1
        end if
        if (binding%declaration_node_index /= node_index) then
            write (error_unit, '(A)') 'FAIL: wrong node for '//trim(label)
            error stop 1
        end if
        if (binding%node_index /= binding%declaration_node_index) then
            write (error_unit, '(A)') 'FAIL: inconsistent declaration identity'
            error stop 1
        end if
        if (binding%binding_kind /= kind) then
            write (error_unit, '(A)') 'FAIL: wrong binding kind for '//trim(label)
            error stop 1
        end if
        if (binding%association /= association) then
            write (error_unit, '(A)') 'FAIL: wrong association for '//trim(label)
            error stop 1
        end if
    end subroutine require_binding

    subroutine require_found(error_msg, binding, kind, association, label)
        character(len=*), intent(in) :: error_msg
        type(declaration_binding_t), intent(in) :: binding
        integer, intent(in) :: kind
        integer, intent(in) :: association
        character(len=*), intent(in) :: label

        call require_binding(error_msg, binding, binding%declaration_node_index, &
            kind, association, label)
    end subroutine require_found

    integer function find_block(result) result(index)
        type(compiler_frontend_result_t), intent(in) :: result
        integer :: i

        index = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
                type is (block_construct_node)
                index = i
                return
            end select
        end do
        write (error_unit, '(A)') 'FAIL: BLOCK not found'
        error stop 1
    end function find_block

    integer function find_block_at_line(result, line) result(index)
        type(compiler_frontend_result_t), intent(in) :: result
        integer, intent(in) :: line
        integer :: i

        index = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
                type is (block_construct_node)
                if (node%line == line) then
                    index = i
                    return
                end if
            end select
        end do
        write (error_unit, '(A,I0)') 'FAIL: BLOCK not found at line ', line
        error stop 1
    end function find_block_at_line

    integer function find_associate(result) result(index)
        type(compiler_frontend_result_t), intent(in) :: result
        integer :: i

        index = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
                type is (associate_node)
                index = i
                return
            end select
        end do
        write (error_unit, '(A)') 'FAIL: ASSOCIATE not found'
        error stop 1
    end function find_associate

    integer function find_module_scope(result) result(index)
        use ast_nodes_data, only: module_node
        type(compiler_frontend_result_t), intent(in) :: result
        integer :: i

        index = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
                type is (module_node)
                index = i
                return
            end select
        end do
        write (error_unit, '(A)') 'FAIL: module not found'
        error stop 1
    end function find_module_scope

    integer function find_module_scope_named(result, name) result(index)
        use ast_nodes_data, only: module_node
        type(compiler_frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: name
        integer :: i

        index = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
                type is (module_node)
                if (.not. allocated(node%name)) cycle
                if (trim(node%name) == trim(name)) then
                    index = i
                    return
                end if
            end select
        end do
        write (error_unit, '(A)') 'FAIL: module not found: '//trim(name)
        error stop 1
    end function find_module_scope_named

    integer function find_program(result, name) result(index)
        type(compiler_frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: name
        integer :: i

        index = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
                type is (program_node)
                if (.not. allocated(node%name)) cycle
                if (trim(node%name) == trim(name)) index = i
            end select
        end do
        if (index > 0) return
        write (error_unit, '(A)') 'FAIL: program not found: '//trim(name)
        error stop 1
    end function find_program

    integer function find_subroutine(result, name) result(index)
        type(compiler_frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: name
        integer :: i

        index = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
                type is (subroutine_def_node)
                if (.not. allocated(node%name)) cycle
                if (trim(node%name) == trim(name)) index = i
            end select
        end do
        if (index > 0) return
        write (error_unit, '(A)') 'FAIL: subroutine not found: '//trim(name)
        error stop 1
    end function find_subroutine

    integer function find_function(result, name) result(index)
        type(compiler_frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: name
        integer :: i

        index = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
                type is (function_def_node)
                if (.not. allocated(node%name)) cycle
                if (trim(node%name) == trim(name)) index = i
            end select
        end do
        if (index > 0) return
        write (error_unit, '(A)') 'FAIL: function not found: '//trim(name)
        error stop 1
    end function find_function

    integer function find_declaration(result, name) result(index)
        type(compiler_frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: name
        integer :: i

        index = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
                type is (declaration_node)
                if (declaration_names(node, name)) index = i
            end select
        end do
        if (index > 0) return
        write (error_unit, '(A)') 'FAIL: declaration not found: '//trim(name)
        error stop 1
    end function find_declaration

    logical function declaration_names(node, name) result(matches)
        type(declaration_node), intent(in) :: node
        character(len=*), intent(in) :: name
        integer :: i

        matches = .false.
        if (allocated(node%var_name)) then
            if (trim(node%var_name) == trim(name)) then
                matches = .true.
                return
            end if
        end if
        if (.not. allocated(node%var_names)) return
        do i = 1, size(node%var_names)
            if (trim(node%var_names(i)) == trim(name)) then
                matches = .true.
                return
            end if
        end do
    end function declaration_names

    integer function first_dimension_index(result, decl_index) result(dim_index)
        type(compiler_frontend_result_t), intent(in) :: result
        integer, intent(in) :: decl_index

        dim_index = 0
        if (.not. result%arena%has_node_at(decl_index)) return
        select type (node => result%arena%entries(decl_index)%node)
            type is (declaration_node)
            if (.not. allocated(node%dimension_indices)) return
            if (size(node%dimension_indices) < 1) return
            dim_index = node%dimension_indices(1)
        end select
        if (dim_index > 0) return
        write (error_unit, '(A)') 'FAIL: declaration has no dimension'
        error stop 1
    end function first_dimension_index

end program test_compiler_scope_resolution
