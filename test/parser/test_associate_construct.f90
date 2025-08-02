program test_associate_construct
    use iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core
    use parser_dispatcher_module, only: parse_statement_dispatcher
    use parser_control_flow_module, only: parse_associate
    use parser_state_module, only: parser_state_t, create_parser_state
    use ast_core, only: create_ast_arena
    use ast_core
    use ast_nodes_control, only: associate_node
    use codegen_core, only: generate_code_from_arena
    implicit none

    integer :: total_tests, passed_tests
    
    total_tests = 0
    passed_tests = 0
    
    ! Run all tests
    call test_simple_associate()
    call test_multiple_associations()
    call test_associate_with_expressions()
    call test_associate_with_body()
    call test_nested_associate()
    call test_associate_codegen()
    call test_associate_integration()
    
    ! Report results
    write (*, '(A)') ''
    write (*, '(A,I0,A,I0,A)') 'Passed ', passed_tests, ' out of ', total_tests, ' tests'
    if (passed_tests /= total_tests) then
        stop 1
    end if

contains

    subroutine test_simple_associate()
        character(len=*), parameter :: test_name = "test_simple_associate"
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        logical :: found_associate
        integer :: i
        
        write (*, '(A)', advance='no') test_name // ' ... '
        total_tests = total_tests + 1
        
        ! Test simple ASSOCIATE with one association
        source = 'associate (x => a + b)' // new_line('a') // &
                'end associate'
        
        call tokenize_core(source, tokens)
        
        ! Debug: check tokens
        write (error_unit, '(A,I0)') 'DEBUG: Number of tokens: ', size(tokens)
        do i = 1, min(5, size(tokens))
            write (error_unit, '(A,I0,A,A)') 'Token ', i, ': ', trim(tokens(i)%text)
        end do
        
        arena = create_ast_arena()
        
        ! Parse just the associate statement
        block
            type(parser_state_t) :: parser
            parser = create_parser_state(tokens)
            root_index = parse_associate(parser, arena)
        end block
        
        write (error_unit, '(A,I0)') 'DEBUG: root_index = ', root_index
        write (error_unit, '(A,I0)') 'DEBUG: arena%size = ', arena%size
        
        ! Debug: print all nodes
        do i = 1, arena%size
            write (error_unit, '(A,I0,A,A)') 'Node ', i, ': ', arena%entries(i)%node_type
        end do
        
        ! Check if ASSOCIATE node was created
        found_associate = .false.
        do i = 1, arena%size
            select type (node => arena%entries(i)%node)
            type is (associate_node)
                found_associate = .true.
                if (allocated(node%associations)) then
                    if (size(node%associations) == 1) then
                        if (node%associations(1)%name == "x") then
                            write (*, '(A)') 'PASS'
                            passed_tests = passed_tests + 1
                        else
                            write (*, '(A)') 'FAIL - Wrong association name'
                        end if
                    else
                        write (*, '(A)') 'FAIL - Wrong number of associations'
                    end if
                else
                    write (*, '(A)') 'FAIL - No associations allocated'
                end if
            end select
        end do
        
        if (.not. found_associate) then
            write (*, '(A)') 'FAIL - No ASSOCIATE node found'
        end if
    end subroutine test_simple_associate

    subroutine test_multiple_associations()
        character(len=*), parameter :: test_name = "test_multiple_associations"
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        logical :: found_associate
        integer :: i
        
        write (*, '(A)', advance='no') test_name // ' ... '
        total_tests = total_tests + 1
        
        ! Test ASSOCIATE with multiple associations
        source = 'associate (x => a + b, y => c * d, z => e / f)' // new_line('a') // &
                'end associate'
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        root_index = parse_statement_dispatcher(tokens, arena)
        
        ! Check if ASSOCIATE node was created with correct associations
        found_associate = .false.
        do i = 1, arena%size
            select type (node => arena%entries(i)%node)
            type is (associate_node)
                found_associate = .true.
                if (allocated(node%associations)) then
                    if (size(node%associations) == 3) then
                        if (node%associations(1)%name == "x" .and. &
                            node%associations(2)%name == "y" .and. &
                            node%associations(3)%name == "z") then
                            write (*, '(A)') 'PASS'
                            passed_tests = passed_tests + 1
                        else
                            write (*, '(A)') 'FAIL - Wrong association names'
                        end if
                    else
                        write (*, '(A,I0)') 'FAIL - Wrong number of associations: ', &
                                           size(node%associations)
                    end if
                else
                    write (*, '(A)') 'FAIL - No associations allocated'
                end if
            end select
        end do
        
        if (.not. found_associate) then
            write (*, '(A)') 'FAIL - No ASSOCIATE node found'
        end if
    end subroutine test_multiple_associations

    subroutine test_associate_with_expressions()
        character(len=*), parameter :: test_name = "test_associate_with_expressions"
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        logical :: found_associate
        integer :: i
        
        write (*, '(A)', advance='no') test_name // ' ... '
        total_tests = total_tests + 1
        
        ! Test ASSOCIATE with complex expressions
        source = 'associate (s => sqrt(x**2 + y**2), avg => (a + b + c) / 3.0)' // &
                 new_line('a') // 'end associate'
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        root_index = parse_statement_dispatcher(tokens, arena)
        
        ! Check if ASSOCIATE node was created
        found_associate = .false.
        do i = 1, arena%size
            select type (node => arena%entries(i)%node)
            type is (associate_node)
                found_associate = .true.
                if (allocated(node%associations)) then
                    if (size(node%associations) == 2) then
                        if (node%associations(1)%name == "s" .and. &
                            node%associations(2)%name == "avg") then
                            ! Check that expression indices are valid
                            if (node%associations(1)%expr_index > 0 .and. &
                                node%associations(2)%expr_index > 0) then
                                write (*, '(A)') 'PASS'
                                passed_tests = passed_tests + 1
                            else
                                write (*, '(A)') 'FAIL - Invalid expression indices'
                            end if
                        else
                            write (*, '(A)') 'FAIL - Wrong association names'
                        end if
                    else
                        write (*, '(A)') 'FAIL - Wrong number of associations'
                    end if
                else
                    write (*, '(A)') 'FAIL - No associations allocated'
                end if
            end select
        end do
        
        if (.not. found_associate) then
            write (*, '(A)') 'FAIL - No ASSOCIATE node found'
        end if
    end subroutine test_associate_with_expressions

    subroutine test_associate_with_body()
        character(len=*), parameter :: test_name = "test_associate_with_body"
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        logical :: found_associate
        integer :: i
        
        write (*, '(A)', advance='no') test_name // ' ... '
        total_tests = total_tests + 1
        
        ! Test ASSOCIATE with body statements
        source = 'associate (x => a + b)' // new_line('a') // &
                'print *, x' // new_line('a') // &
                'y = x * 2' // new_line('a') // &
                'end associate'
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        root_index = parse_statement_dispatcher(tokens, arena)
        
        ! Check if ASSOCIATE node has body statements
        found_associate = .false.
        do i = 1, arena%size
            select type (node => arena%entries(i)%node)
            type is (associate_node)
                found_associate = .true.
                if (allocated(node%body_indices)) then
                    if (size(node%body_indices) >= 2) then
                        write (*, '(A)') 'PASS'
                        passed_tests = passed_tests + 1
                    else
                        write (*, '(A,I0)') 'FAIL - Too few body statements: ', &
                                           size(node%body_indices)
                    end if
                else
                    write (*, '(A)') 'FAIL - No body statements allocated'
                end if
            end select
        end do
        
        if (.not. found_associate) then
            write (*, '(A)') 'FAIL - No ASSOCIATE node found'
        end if
    end subroutine test_associate_with_body

    subroutine test_nested_associate()
        character(len=*), parameter :: test_name = "test_nested_associate"
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        integer :: associate_count
        integer :: i
        
        write (*, '(A)', advance='no') test_name // ' ... '
        total_tests = total_tests + 1
        
        ! Test nested ASSOCIATE constructs
        source = 'associate (x => a + b)' // new_line('a') // &
                '  associate (y => x * 2)' // new_line('a') // &
                '    print *, y' // new_line('a') // &
                '  end associate' // new_line('a') // &
                'end associate'
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        root_index = parse_statement_dispatcher(tokens, arena)
        
        ! Count ASSOCIATE nodes
        associate_count = 0
        do i = 1, arena%size
            select type (node => arena%entries(i)%node)
            type is (associate_node)
                associate_count = associate_count + 1
            end select
        end do
        
        if (associate_count >= 2) then
            write (*, '(A)') 'PASS'
            passed_tests = passed_tests + 1
        else
            write (*, '(A,I0)') 'FAIL - Expected 2 ASSOCIATE nodes, found ', associate_count
        end if
    end subroutine test_nested_associate

    subroutine test_associate_codegen()
        character(len=*), parameter :: test_name = "test_associate_codegen"
        character(len=:), allocatable :: source, generated
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        integer :: i
        
        write (*, '(A)', advance='no') test_name // ' ... '
        total_tests = total_tests + 1
        
        ! Test code generation
        source = 'associate (x => a + b, y => c * d)' // new_line('a') // &
                'z = x + y' // new_line('a') // &
                'end associate'
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        root_index = parse_statement_dispatcher(tokens, arena)
        
        ! Generate code from ASSOCIATE node
        do i = 1, arena%size
            select type (node => arena%entries(i)%node)
            type is (associate_node)
                generated = generate_code_from_arena(arena, i)
                if (index(generated, "associate") > 0 .and. &
                    index(generated, "x => ") > 0 .and. &
                    index(generated, "y => ") > 0 .and. &
                    index(generated, "end associate") > 0) then
                    write (*, '(A)') 'PASS'
                    passed_tests = passed_tests + 1
                    return
                else
                    write (*, '(A)') 'FAIL - Generated code missing components'
                    write (error_unit, '(A)') 'Generated code:'
                    write (error_unit, '(A)') generated
                    return
                end if
            end select
        end do
        
        write (*, '(A)') 'FAIL - No ASSOCIATE node found for codegen'
    end subroutine test_associate_codegen

    subroutine test_associate_integration()
        character(len=*), parameter :: test_name = "test_associate_integration"
        character(len=:), allocatable :: source, code
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        
        write (*, '(A)', advance='no') test_name // ' ... '
        total_tests = total_tests + 1
        
        ! Test full compilation with ASSOCIATE
        source = 'program test' // new_line('a') // &
                'implicit none' // new_line('a') // &
                'real :: a = 3.0, b = 4.0' // new_line('a') // &
                'associate (s => sqrt(a**2 + b**2))' // new_line('a') // &
                '  print *, "Hypotenuse:", s' // new_line('a') // &
                'end associate' // new_line('a') // &
                'end program test'
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        root_index = parse_statement_dispatcher(tokens, arena)
        
        if (root_index > 0) then
            code = generate_code_from_arena(arena, root_index)
            if (index(code, "associate") > 0 .and. &
                index(code, "s => sqrt(a**2 + b**2)") > 0) then
                write (*, '(A)') 'PASS'
                passed_tests = passed_tests + 1
            else
                write (*, '(A)') 'FAIL - Missing ASSOCIATE in generated code'
                write (error_unit, '(A)') 'Generated code:'
                write (error_unit, '(A)') code
            end if
        else
            write (*, '(A)') 'FAIL - Parse failed'
        end if
    end subroutine test_associate_integration

end program test_associate_construct