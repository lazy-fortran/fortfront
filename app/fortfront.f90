program fortfront_cli
    use iso_fortran_env, only: output_unit, error_unit
    implicit none
    
    character(len=1024) :: arg
    integer :: argc, i
    
    argc = command_argument_count()
    
    if (argc == 0) then
        call print_usage()
        stop 1
    end if
    
    call get_command_argument(1, arg)
    
    select case (trim(arg))
    case ('--help', '-h')
        call print_usage()
    case ('--version', '-v')
        call print_version()
    case default
        write(error_unit, '(A)') 'Error: Unknown command: ' // trim(arg)
        call print_usage()
        stop 1
    end select
    
contains
    
    subroutine print_usage()
        write(output_unit, '(A)') 'fortfront - Core analysis frontend for lazy fortran'
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'Usage: fortfront [OPTIONS] [FILE]'
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'Options:'
        write(output_unit, '(A)') '  -h, --help      Show this help message'
        write(output_unit, '(A)') '  -v, --version   Show version information'
        write(output_unit, '(A)') ''
        write(output_unit, '(A)') 'Commands (planned):'
        write(output_unit, '(A)') '  tokens          Dump tokens as JSON'
        write(output_unit, '(A)') '  ast             Dump AST as JSON'
        write(output_unit, '(A)') '  typed-ast       Dump typed AST as JSON'
        write(output_unit, '(A)') '  emit-f90        Emit Standard Fortran code'
    end subroutine print_usage
    
    subroutine print_version()
        write(output_unit, '(A)') 'fortfront version 0.1.0'
        write(output_unit, '(A)') 'Core analysis frontend for lazy fortran'
    end subroutine print_version
    
end program fortfront_cli