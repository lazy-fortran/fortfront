program debug_args
    implicit none
    integer :: num_args, i, arg_len
    character(len=100) :: arg

    num_args = command_argument_count()
    print *, "Number of arguments:", num_args

    do i = 0, num_args
        call get_command_argument(i, arg, length=arg_len)
        print *, "Arg", i, ":", trim(arg), " (length:", arg_len, ")"
    end do
end program debug_args
