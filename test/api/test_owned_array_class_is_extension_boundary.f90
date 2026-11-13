program test_owned_array_class_is_extension_boundary
    use fortfront_ast, only: get_node_type_at
    use fortfront_compiler, only: compiler_frontend_options_t, &
                             compiler_frontend_result_t, compile_frontend_from_string, &
                                  control_statement_query_t, query_control_statement, &
                       select_type_owned_array_query_t, query_select_type_owned_array, &
          select_type_owned_array_binding_query_t, query_select_type_owned_array_binding
    use fortfront_semantic, only: INPUT_MODE_STANDARD
    implicit none
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(control_statement_query_t) :: control
    type(select_type_owned_array_query_t) :: identity
    type(select_type_owned_array_binding_query_t) :: binding
    character(len=:), allocatable :: source, executable, module_dir
    character(len=*), parameter :: fixture = &
                              'examples/f90/owned_array_class_is_extension_boundary.f90'
    integer :: i, status, count

    call read_example(fixture, source)
    executable = test_executable_path('owned_array_extension_boundary')
    call create_temp_directory(module_dir, is_windows())
    call require(len_trim(module_dir) > 0, 'cannot create compiler module directory')
    call execute_command_line('gfortran -std=f2018 -J "'//module_dir// &
                              '" -o '//executable//' '//fixture, exitstat=status)
    call cleanup_temp_directory(module_dir, is_windows())
    call require(status == 0, 'GNU rejected extension boundary fixture')
    call execute_command_line(executable, exitstat=status)
    call test_remove_file(executable)
    call require(status == 0, 'GNU did not retain grandchild dispatch in CLASS IS')
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'extension boundary fixture did not parse')
    count = 0
    do i = 1, result%arena%entry_count
        if (get_node_type_at(result%arena, i) /= 'select_type') cycle
        control = query_control_statement(result%arena, i)
        call require(size(control%type_arms) == 1, 'CLASS IS arm is absent')
        identity = query_select_type_owned_array(result%arena, &
                                                 control%type_arms(1)%arm_node_index)
        call require(identity%is_refused .and. identity%is_unresolved .and. &
                     .not. identity%is_resolved .and. &
                     .not. identity%is_dynamic_type_concrete, &
                     'CLASS IS guard was mistaken for exact dynamic identity')
        binding = query_select_type_owned_array_binding(result%arena, &
                                             control%type_arms(1)%arm_node_index, 'run')
        call require(binding%is_refused .and. .not. binding%is_resolved .and. &
                     binding%implementation_node_index == 0, &
                     'extension guard leaked parent implementation target')
        count = count + 1
    end do
    call require(count == 3, 'extension boundaries were not inspected')
    print *, 'PASS: CLASS IS extension does not prove exact dynamic identity'
contains
    include '../common/read_example.inc'
    include '../common/test_command_helpers.inc'
    include '../common/filesystem_helpers.inc'
    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) then
            print *, 'FAIL: ', message
            error stop 1
        end if
    end subroutine
end program
