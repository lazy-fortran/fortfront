program test_compiler_program_unit_queries
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fortfront_compiler, only: ast_arena_t, &
        compiler_frontend_options_t, compiler_frontend_result_t, &
        compile_frontend_from_string, INPUT_MODE_STANDARD, &
        program_unit_query_t, declaration_query_t, derived_type_query_t, &
        type_binding_query_t, use_statement_query_t, interface_query_t, &
        visibility_query_t, namelist_query_t, data_statement_query_t, &
        common_block_query_t, enum_query_t, statement_function_query_t, &
        block_data_query_t, query_program_units, query_program_unit, &
        query_declarations, query_declaration, query_derived_type, &
        query_type_binding, query_use_statements, query_interface, &
        query_visibility, query_namelist, query_data_statement, &
        query_common_block, query_enum, query_statement_function, &
        query_block_data
    implicit none

    call test_program_units_and_declarations()
    call test_construct_queries()
    call test_block_data_query()
    call test_wrong_kind_and_parse_diagnostic()

    write (output_unit, '(a)') 'PASS: compiler program-unit queries'

contains

    subroutine test_program_units_and_declarations()
        type(compiler_frontend_result_t) :: result
        type(program_unit_query_t), allocatable :: units(:)
        type(declaration_query_t), allocatable :: declarations(:)
        type(derived_type_query_t) :: derived
        type(type_binding_query_t) :: binding
        type(use_statement_query_t), allocatable :: uses(:)
        character(len=:), allocatable :: source
        integer :: i

        source = 'module m'//new_line('a')// &
            '  use dep, only: local => remote'//new_line('a')// &
            '  private'//new_line('a')// &
            '  integer, parameter :: n = 2'//new_line('a')// &
            '  type :: pair'//new_line('a')// &
            '    integer :: x'//new_line('a')// &
            '  contains'//new_line('a')// &
            '    procedure :: show'//new_line('a')// &
            '  end type pair'//new_line('a')// &
            'contains'//new_line('a')// &
            '  subroutine show(self)'//new_line('a')// &
            '    type(pair) :: self'//new_line('a')// &
            '  end subroutine show'//new_line('a')// &
            'end module m'//new_line('a')// &
            'program p'//new_line('a')// &
            '  use m'//new_line('a')// &
            'end program p'
        call compile_parse_only(source, result, 'module and program')

        units = query_program_units(result%arena, result%root_index)
        call require(size(units) == 2, 'two top-level units in source order')
        units = query_program_units(result%arena)
        call require(size(units) == 2, 'implicit root query preserves unit order')
        call require(trim(units(1)%unit_kind) == 'module', 'first unit is module')
        call require(trim(units(1)%name) == 'm', 'module name')
        call require(trim(units(2)%unit_kind) == 'program', 'second unit is program')
        call require(trim(units(2)%name) == 'p', 'program name')
        call require(units(1)%line > 0 .and. units(1)%column > 0, &
            'program-unit source location')
        call require(size(units(1)%declaration_indices) > 0, &
            'module declaration indices')

        declarations = query_declarations(result%arena, units(1)%node_index)
        call require(size(declarations) >= 1, 'module declarations are queryable')
        do i = 1, size(declarations)
            if (trim(declarations(i)%name) == 'n') exit
        end do
        call require(i <= size(declarations), 'parameter declaration is present')
        call require(declarations(i)%is_parameter, 'parameter attribute is copied')
        call require(declarations(i)%has_initializer, 'initializer attribute is copied')
        call require(declarations(i)%initializer_index > 0, &
            'initializer child index is copied')

        derived = find_derived_type(result%arena)
        call require(derived%found, 'derived type query finds pair')
        call require(trim(derived%name) == 'pair', 'derived type name')
        call require(derived%has_contains, 'derived type contains attribute')
        call require(size(derived%component_indices) == 1, &
            'derived type component order')
        call require(size(derived%binding_indices) == 1, &
            'derived type binding order')

        binding = find_type_binding(result%arena)
        call require(binding%found, 'type binding query finds show')
        call require(trim(binding%binding_name) == 'show', 'type binding name')

        uses = query_use_statements(result%arena)
        call require(size(uses) == 2, 'all USE statements are queryable')
        call require(trim(uses(1)%module_name) == 'dep', 'USE module name')
        call require(uses(1)%has_only, 'USE ONLY attribute')
        call require(size(uses(1)%rename_list) == 2, 'USE rename pair')
        call require(trim(uses(1)%rename_list(1)) == 'local', &
            'USE local rename is first')
        call require(trim(uses(1)%rename_list(2)) == 'remote', &
            'USE remote rename is second')
    end subroutine test_program_units_and_declarations

    subroutine test_construct_queries()
        type(compiler_frontend_result_t) :: result
        type(interface_query_t) :: interface_query
        type(visibility_query_t) :: visibility_query
        type(namelist_query_t) :: namelist_query
        type(data_statement_query_t) :: data_query
        type(common_block_query_t) :: common_query
        type(enum_query_t) :: enum_query
        type(statement_function_query_t) :: statement_query
        character(len=:), allocatable :: source

        source = 'program legacy'//new_line('a')// &
            '  integer :: a, b'//new_line('a')// &
            '  common /blk/ a, b'//new_line('a')// &
            '  namelist /group/ a, b'//new_line('a')// &
            '  data a, b /1, 2/'//new_line('a')// &
            '  enum, bind(c)'//new_line('a')// &
            '    enumerator :: red = 1, blue'//new_line('a')// &
            '  end enum'//new_line('a')// &
            'end program legacy'
        call compile_parse_only(source, result, 'legacy constructs')

        common_query = find_common_block(result%arena)
        call require(common_query%found, 'COMMON query finds block')
        call require(size(common_query%block_names) == 1, 'COMMON block count')
        call require(trim(common_query%block_names(1)) == 'blk', 'COMMON block name')
        call require(size(common_query%member_names) == 2, 'COMMON member order')
        call require(all(common_query%member_block == 1), 'COMMON member ownership')

        namelist_query = find_namelist(result%arena)
        call require(namelist_query%found, 'NAMELIST query finds group')
        call require(trim(namelist_query%group_name) == 'group', 'NAMELIST group name')
        call require(size(namelist_query%variable_names) == 2, &
            'NAMELIST variable order')

        data_query = find_data_statement(result%arena)
        call require(data_query%found, 'DATA query finds statement')
        call require(size(data_query%object_indices) == 2, 'DATA object order')
        call require(size(data_query%value_indices) == 2, 'DATA value order')

        enum_query = find_enum(result%arena)
        call require(enum_query%found, 'ENUM query finds construct')
        call require(enum_query%is_bind_c, 'ENUM BIND(C) attribute')
        call require(size(enum_query%enumerator_names) == 2, 'ENUM name order')
        call require(enum_query%enumerator_values(1) == 1, 'ENUM explicit value')
        call require(enum_query%enumerator_values(2) == 2, 'ENUM implicit value')

        source = 'f(x) = x + 1'//new_line('a')//'end'
        call compile_parse_only(source, result, 'statement function')
        statement_query = find_statement_function(result%arena)
        call require(statement_query%found, 'statement-function query finds f')
        call require(trim(statement_query%name) == 'f', 'statement-function name')
        call require(size(statement_query%argument_names) == 1, &
            'statement-function argument count')
        call require(statement_query%body_expression_index > 0, &
            'statement-function body index')

        source = 'module visible'//new_line('a')// &
            '  private'//new_line('a')// &
            '  public :: x'//new_line('a')// &
            '  integer :: x'//new_line('a')// &
            'end module visible'
        call compile_parse_only(source, result, 'visibility statements')
        visibility_query = find_visibility(result%arena)
        call require(visibility_query%found, 'visibility query finds PRIVATE')
        call require(visibility_query%is_private, 'PRIVATE attribute')
    end subroutine test_construct_queries

    subroutine test_block_data_query()
        type(compiler_frontend_result_t) :: result
        type(program_unit_query_t), allocatable :: units(:)
        type(block_data_query_t) :: block_data_query
        character(len=:), allocatable :: source

        source = 'block data init'//new_line('a')// &
            '  integer :: a, b'//new_line('a')// &
            '  common /shared/ a, b'//new_line('a')// &
            '  data a, b /1, 2/'//new_line('a')// &
            'end block data init'
        call compile_parse_only(source, result, 'BLOCK DATA')
        units = query_program_units(result%arena, result%root_index)
        call require(size(units) == 1, 'one BLOCK DATA unit')
        call require(trim(units(1)%unit_kind) == 'block_data', 'BLOCK DATA kind')
        call require(trim(units(1)%name) == 'init', 'BLOCK DATA name')
        call require(size(units(1)%statement_indices) >= 3, &
            'BLOCK DATA statement order')
        block_data_query = query_block_data(result%arena, units(1)%node_index)
        call require(block_data_query%found, 'BLOCK DATA record query')
        call require(size(block_data_query%statement_indices) == &
            size(units(1)%statement_indices), 'BLOCK DATA record matches unit')
    end subroutine test_block_data_query

    subroutine test_wrong_kind_and_parse_diagnostic()
        type(compiler_frontend_result_t) :: result
        type(program_unit_query_t), allocatable :: units(:)
        type(declaration_query_t) :: declaration
        type(interface_query_t) :: interface_result

        call compile_parse_only( &
            'interface g'//new_line('a')// &
            '  subroutine s(x)'//new_line('a')// &
            '    integer :: x'//new_line('a')// &
            '  end subroutine s'//new_line('a')// &
            'end interface g', result, 'interface')
        declaration = query_declaration(result%arena, result%root_index)
        call require(.not. declaration%found, 'wrong-kind declaration is not invented')
        call require(declaration%node_index == result%root_index, &
            'wrong-kind declaration retains queried index')
        interface_result = find_interface(result%arena)
        call require(interface_result%found, 'interface query finds named interface')

        call compile_frontend_from_string( &
            'program broken'//new_line('a')// &
            '  value = identity{integer'//new_line('a')// &
            'end program broken', result)
        call require(.not. result%parse_ok, 'malformed source returns parse diagnostic')
        units = query_program_units(result%arena, result%root_index)
        call require(size(units) == 0, 'malformed source has no invented units')
    end subroutine test_wrong_kind_and_parse_diagnostic

    subroutine compile_parse_only(source, result, label)
        character(len=*), intent(in) :: source, label
        type(compiler_frontend_result_t), intent(out) :: result
        type(compiler_frontend_options_t) :: options

        options = compiler_frontend_options_t()
        options%input_mode = INPUT_MODE_STANDARD
        options%run_semantics = .false.
        call compile_frontend_from_string(source, result, options)
        if (result%parse_ok) return
        write (error_unit, '(2a)') 'FAIL: parse failed for ', trim(label)
        if (allocated(result%error_msg)) write (error_unit, '(a)') result%error_msg
        error stop 1
    end subroutine compile_parse_only

    function find_derived_type(arena) result(query)
        type(ast_arena_t), intent(in) :: arena
        type(derived_type_query_t) :: query
        integer :: i

        query = query_derived_type(arena, 0)
        do i = 1, arena%size
            query = query_derived_type(arena, i)
            if (query%found) return
        end do
    end function find_derived_type

    function find_type_binding(arena) result(query)
        type(ast_arena_t), intent(in) :: arena
        type(type_binding_query_t) :: query
        integer :: i

        query = query_type_binding(arena, 0)
        do i = 1, arena%size
            query = query_type_binding(arena, i)
            if (query%found) return
        end do
    end function find_type_binding

    function find_common_block(arena) result(query)
        type(ast_arena_t), intent(in) :: arena
        type(common_block_query_t) :: query
        integer :: i

        query = query_common_block(arena, 0)
        do i = 1, arena%size
            query = query_common_block(arena, i)
            if (query%found) return
        end do
    end function find_common_block

    function find_namelist(arena) result(query)
        type(ast_arena_t), intent(in) :: arena
        type(namelist_query_t) :: query
        integer :: i

        query = query_namelist(arena, 0)
        do i = 1, arena%size
            query = query_namelist(arena, i)
            if (query%found) return
        end do
    end function find_namelist

    function find_data_statement(arena) result(query)
        type(ast_arena_t), intent(in) :: arena
        type(data_statement_query_t) :: query
        integer :: i

        query = query_data_statement(arena, 0)
        do i = 1, arena%size
            query = query_data_statement(arena, i)
            if (query%found) return
        end do
    end function find_data_statement

    function find_enum(arena) result(query)
        type(ast_arena_t), intent(in) :: arena
        type(enum_query_t) :: query
        integer :: i

        query = query_enum(arena, 0)
        do i = 1, arena%size
            query = query_enum(arena, i)
            if (query%found) return
        end do
    end function find_enum

    function find_statement_function(arena) result(query)
        type(ast_arena_t), intent(in) :: arena
        type(statement_function_query_t) :: query
        integer :: i

        query = query_statement_function(arena, 0)
        do i = 1, arena%size
            query = query_statement_function(arena, i)
            if (query%found) return
        end do
    end function find_statement_function

    function find_visibility(arena) result(query)
        type(ast_arena_t), intent(in) :: arena
        type(visibility_query_t) :: query
        integer :: i

        query = query_visibility(arena, 0)
        do i = 1, arena%size
            query = query_visibility(arena, i)
            if (query%found) return
        end do
    end function find_visibility

    function find_interface(arena) result(query)
        type(ast_arena_t), intent(in) :: arena
        type(interface_query_t) :: query
        integer :: i

        query = query_interface(arena, 0)
        do i = 1, arena%size
            query = query_interface(arena, i)
            if (query%found) return
        end do
    end function find_interface

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) return
        write (error_unit, '(2a)') 'FAIL: ', trim(message)
        error stop 1
    end subroutine require

end program test_compiler_program_unit_queries
