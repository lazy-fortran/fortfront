program test_lexer_trivia_simple
    use lexer_core
    implicit none
    
    type(lexer_options_t) :: options
    type(token_t), allocatable :: tokens(:)
    character(len=*), parameter :: source = "  x = 1  ! comment"
    integer :: i
    
    print *, "Testing lexer trivia collection..."
    
    ! Test 1: Default options (no trivia)
    print *, "Test 1: Default options"
    options = options%default()
    call tokenize_with_options(source, tokens, options)
    
    if (allocated(tokens)) then
        print *, "  Token count:", size(tokens)
        do i = 1, size(tokens) - 1  ! Skip EOF
            print *, "  Token", i, ":", trim(token_type_name(tokens(i)%kind)), &
                     " '", tokens(i)%text, "'"
            if (allocated(tokens(i)%leading_trivia)) then
                print *, "    Has", size(tokens(i)%leading_trivia), "leading trivia"
            end if
        end do
    else
        print *, "  ERROR: No tokens generated"
    end if
    
    ! Test 2: With trivia collection
    print *, ""
    print *, "Test 2: With trivia collection"
    options = options%with_trivia()
    call tokenize_with_options(source, tokens, options)
    
    if (allocated(tokens)) then
        print *, "  Token count:", size(tokens)
        do i = 1, size(tokens) - 1  ! Skip EOF
            print *, "  Token", i, ":", trim(token_type_name(tokens(i)%kind)), &
                     " '", tokens(i)%text, "'"
            if (allocated(tokens(i)%leading_trivia)) then
                print *, "    Has", size(tokens(i)%leading_trivia), "leading trivia items:"
                block
                    integer :: j
                    do j = 1, size(tokens(i)%leading_trivia)
                        print *, "      Trivia", j, ":", &
                                trim(token_type_name(tokens(i)%leading_trivia(j)%kind)), &
                                " '", tokens(i)%leading_trivia(j)%text, "'"
                    end do
                end block
            end if
        end do
    else
        print *, "  ERROR: No tokens generated"
    end if
    
    print *, ""
    print *, "Tests complete!"
    
end program test_lexer_trivia_simple