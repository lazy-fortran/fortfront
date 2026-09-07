program test_issue_2970_character_kind_selector
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, compiler_diagnostic_t, get_compiler_diagnostics, &
        DIAGNOSTIC_PHASE_SEMANTIC, DIAGNOSTIC_CODE_SEMANTIC
    use fortfront_types, only: DIAGNOSTIC_ERROR
    use ast_nodes_data, only: declaration_node
    use ast_nodes_core, only: call_or_subscript_node
    implicit none

    type(compiler_frontend_result_t) :: result
    type(compiler_frontend_options_t) :: options
    type(compiler_diagnostic_t), allocatable :: diagnostics(:)
    character(len=:), allocatable :: source
    integer :: i, matched, selector_index

    options%input_mode = INPUT_MODE_STANDARD
    options%standardize = .false.
    call read_example( &
        'examples/f90/issue_2970_character_kind_function_invalid.f90', source)
    call compile_frontend_from_string(source, result, options)
    if (.not. result%parse_ok) error stop 'valid selector syntax was not parsed'
    selector_index = find_selector(result, 'text')
    if (.not. result%arena%has_node_at(selector_index)) then
        error stop 'CHARACTER kind selector is absent from typed AST'
    end if
    select type (node => result%arena%entries(selector_index)%node)
    type is (call_or_subscript_node)
        if (node%name /= 'choose_kind') error stop 'selector call identity lost'
    class default
        error stop 'selector call AST was not preserved'
    end select
    if (result%semantic_ok) error stop 'nonconstant kind selector was accepted'
    diagnostics = get_compiler_diagnostics(result)
    matched = 0
    do i = 1, size(diagnostics)
        if (diagnostics(i)%category /= 'semantic_kind_selector') cycle
        matched = matched + 1
        call check_diagnostic(diagnostics(i))
    end do
    if (matched /= 1) error stop 'expected exactly one kind-selector diagnostic'

    call read_example('examples/f90/issue_2970_character_kind_neighbors.f90', source)
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) then
        print *, result%error_msg
        error stop 'valid kind-selector neighbor was rejected'
    end if
    if (find_selector(result, 'intrinsic_text') <= 0) error stop 'KIND call lost'
    if (find_selector(result, 'table_text') <= 0) error stop 'array selector lost'
    if (find_selector(result, 'literal_text') <= 0) error stop 'literal selector lost'
    if (find_selector(result, 'inquiry_text') <= 0) error stop 'nested inquiry lost'
    print *, 'PASS: CHARACTER kind AST and diagnostic boundary'

contains

    include '../common/read_example.inc'

    integer function find_selector(frontend, name) result(selector)
        type(compiler_frontend_result_t), intent(in) :: frontend
        character(len=*), intent(in) :: name
        integer :: j

        selector = 0
        do j = 1, frontend%arena%size
            if (.not. frontend%arena%has_node_at(j)) cycle
            select type (node => frontend%arena%entries(j)%node)
            type is (declaration_node)
                if (.not. allocated(node%var_name)) cycle
                if (node%var_name /= name) cycle
                selector = node%kind_selector_index
                return
            end select
        end do
    end function find_selector

    subroutine check_diagnostic(diagnostic)
        type(compiler_diagnostic_t), intent(in) :: diagnostic

        if (diagnostic%phase /= DIAGNOSTIC_PHASE_SEMANTIC) error stop 'wrong phase'
        if (diagnostic%code /= DIAGNOSTIC_CODE_SEMANTIC) error stop 'wrong code'
        if (diagnostic%severity /= DIAGNOSTIC_ERROR) error stop 'wrong severity'
        if (diagnostic%span%start%line /= 7) error stop 'wrong diagnostic line'
        if (diagnostic%span%start%column /= 24) then
            print *, 'column=', diagnostic%span%start%column
            error stop 'wrong diagnostic column'
        end if
        if (diagnostic%span%end%line /= 7) error stop 'wrong end line'
        if (diagnostic%span%end%column /= 35) error stop 'wrong end column'
        if (index(diagnostic%message, 'constant kind selector') == 0) then
            error stop 'missing kind-selector diagnostic message'
        end if
    end subroutine check_diagnostic

end program test_issue_2970_character_kind_selector
