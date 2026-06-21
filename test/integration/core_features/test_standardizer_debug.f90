program test_standardizer_debug
    use lexer_api, only: lex_file
    use parser_api, only: parse_tokens
    use standardizer, only: standardize_ast
    use ast_arena_modern, only: ast_arena_t
    use codegen_core, only: codegen_core_generate_arena, initialize_codegen
    use lexer_core, only: token_t
    use test_filesystem_helpers, only: check_if_windows, create_temp_directory, &
                                       cleanup_temp_directory, join_path, &
                                       path_separator_for
    implicit none

    character(len=:), allocatable :: input_file
    character(len=256) :: error_msg
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: root_index
    character(len=:), allocatable :: generated_code
    integer :: unit, i
    character(len=:), allocatable :: source_code
    integer :: file_size, iostat
    character(len=1) :: ch
    logical :: is_windows
    character(len=:), allocatable :: temp_dir
    character(len=1) :: sep

    print *, '=== Standardizer Debug Test ==='

    ! Initialize codegen system for arena operations
    call initialize_codegen()

    is_windows = check_if_windows()
    call create_temp_directory(temp_dir, is_windows)
    if (len_trim(temp_dir) == 0) then
        print *, 'FAIL: could not create temporary directory'
        stop 1
    end if
    sep = path_separator_for(temp_dir)

    ! Create test input
    input_file = join_path(temp_dir, 'test_slice_debug.lf', sep)
    open (newunit=unit, file=input_file, status='replace')
    write (unit, '(a)') 'integer :: arr(5)'
    write (unit, '(a)') 'arr(1) = 1'
    write (unit, '(a)') 'arr(2) = 2'
    write (unit, '(a)') 'arr(3) = 3'
    write (unit, '(a)') 'arr(4) = 4'
    write (unit, '(a)') 'arr(5) = 5'
    write (unit, '(a)') 'slice = arr(2:4)'
    write (unit, '(a)') 'print *, slice'
    close (unit)

    ! Read file
    ! Get file size
    inquire (file=input_file, size=file_size)
    allocate (character(len=file_size) :: source_code)

    ! Read file contents
    open (newunit=unit, file=input_file, status='old', access='stream')
    read (unit, iostat=iostat) source_code
    close (unit)

    ! Lex
    call lex_file(source_code, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, 'Lex error:', trim(error_msg)
        stop 1
    end if

    print *, 'Tokens:', size(tokens)
    do i = 1, min(10, size(tokens))
        print *, 'Token', i, ':', trim(tokens(i)%text)
    end do

    ! Parse
    call parse_tokens(tokens, arena, root_index, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, 'Parse error:', trim(error_msg)
        stop 1
    end if

    print *, 'Before standardization:'
    generated_code = codegen_core_generate_arena(arena, root_index)
    print *, trim(generated_code)

    ! Standardize
    call standardize_ast(arena, root_index)

    print *, 'After standardization:'
    generated_code = codegen_core_generate_arena(arena, root_index)
    print *, trim(generated_code)

    call cleanup_temp_directory(temp_dir, is_windows)

end program test_standardizer_debug
