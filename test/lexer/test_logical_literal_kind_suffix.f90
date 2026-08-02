program test_logical_literal_kind_suffix
    use lexer_api, only: lex_source
    use lexer_core, only: token_t
    implicit none

    character(len=*), parameter :: source = &
        'x = .true._1'//new_line('a')// &
        'y = .FALSE._logical_kind'
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    logical :: found_true, found_false
    integer :: i

    call lex_source(source, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: lexer rejected logical kind suffix:', trim(error_msg)
        stop 1
    end if

    found_true = .false.
    found_false = .false.
    do i = 1, size(tokens)
        if (.not. allocated(tokens(i)%text)) cycle
        if (trim(tokens(i)%text) == '.true._1') found_true = .true.
        if (trim(tokens(i)%text) == '.FALSE._logical_kind') found_false = .true.
    end do

    if (.not. found_true .or. .not. found_false) then
        print *, 'FAIL: logical kind suffix was split or lost'
        stop 1
    end if

    print *, 'PASS: logical kind suffix remains one token'
end program test_logical_literal_kind_suffix
