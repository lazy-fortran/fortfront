program test_nested_include_statements
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string
    use semantic_input_mode, only: INPUT_MODE_STANDARD
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_core, only: initialize_codegen
    implicit none
    character(len=1), parameter :: nl = new_line('a')
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    character(:), allocatable :: source

    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call check_construct('do i = 1, 3', 'end do')
    call check_construct('if (i == 1) then', 'end if')
    call check_construct('select case (i)'//nl//'case (1)', 'end select')
    call check_construct('block', 'end block')

    source = 'program invalid_include'//nl//'integer :: i'//nl// &
        'do i = 1, 3'//nl//'include 7'//nl//'end do'//nl// &
        'end program invalid_include'//nl
    call compile_frontend_from_string(source, result, options)
    if (result%parse_ok) error stop 'non-character INCLUDE must be rejected'

    source = 'program keyword_name'//nl//'integer :: include, i'//nl// &
        'do i = 1, 3'//nl//'include = i'//nl//'end do'//nl// &
        'end program keyword_name'//nl
    call compile_frontend_from_string(source, result, options)
    if (.not. result%parse_ok) error stop 'INCLUDE remains a valid variable name'
    print *, 'PASS: nested INCLUDE statements and invalid neighbor'
contains
    subroutine check_construct(opener, closer)
        character(len=*), intent(in) :: opener, closer
        character(:), allocatable :: text, emitted

        text = 'module fixture'//nl//'contains'//nl// &
            'subroutine run()'//nl//'integer :: i, total'//nl// &
            'i = 1'//nl//'total = 0'//nl//opener//nl// &
            'include "body.inc"'//nl//closer//nl// &
            'print *, total'//nl//'end subroutine run'//nl// &
            'end module fixture'//nl
        call compile_frontend_from_string(text, result, options)
        if (.not. result%parse_ok) then
            print *, opener
            if (allocated(result%error_msg)) print *, result%error_msg
            error stop 'valid INCLUDE inside construct rejected'
        end if
        call initialize_codegen()
        emitted = generate_code_from_arena(result%arena, result%root_index)
        if (index(emitted, 'include "body.inc"') == 0) &
            error stop 'INCLUDE filename lost during emission'
        if (index(emitted, 'print *, total') == 0) &
            error stop 'statement after construct lost during emission'
    end subroutine check_construct
end program test_nested_include_statements
