program test_fortfront_component_access_query
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: compiler_frontend_options_t, compiler_frontend_result_t, &
        compile_frontend_from_string, component_access_query_t, &
        query_component_access, get_identifier_name, INPUT_MODE_STANDARD
    implicit none

    character(len=*), parameter :: source = &
        'program component_query'//new_line('a')// &
        '    type :: record_t'//new_line('a')// &
        '        integer :: field'//new_line('a')// &
        '    end type record_t'//new_line('a')// &
        '    type(record_t) :: obj'//new_line('a')// &
        '    print *, obj%field'//new_line('a')// &
        'end program component_query'
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(component_access_query_t) :: component
    character(len=:), allocatable :: base_name, error_msg
    integer :: i, component_count

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) call fail('facade frontend rejected source')

    component_count = 0
    do i = 1, result%arena%size
        component = query_component_access(result%arena, i)
        if (.not. component%found) cycle
        component_count = component_count + 1

        if (.not. allocated(component%component_name)) then
            call fail('component name is not allocated')
        end if
        if (component%component_name /= 'field') then
            call fail('component name mismatch: '//component%component_name)
        end if

        call get_identifier_name(result%arena, component%base_node_index, &
            base_name, error_msg)
        if (len_trim(error_msg) > 0) call fail(error_msg)
        if (base_name /= 'obj') call fail('base name mismatch: '//base_name)
    end do

    if (component_count /= 1) call fail('expected one component access')
    print *, 'PASS: fortfront facade component-access query'

contains

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(a)') 'FAIL: '//message
        error stop 1
    end subroutine fail

end program test_fortfront_component_access_query
