module frontend_utilities
    ! fortfront - Utility functions module
    ! Contains helper functions and utilities

    use path_validation, only: validate_output_path, path_validation_result_t
    use lexer_core, only: token_t, TK_KEYWORD, TK_OPERATOR, TK_IDENTIFIER

    implicit none
    private

    public :: write_output_file, int_to_str, has_declarations_before_program

contains

    ! Write output to file
    subroutine write_output_file(filename, content, error_msg)
        character(len=*), intent(in) :: filename, content
        character(len=*), intent(out) :: error_msg

        integer :: unit, iostat
        type(path_validation_result_t) :: validation_result

        ! Validate output file path for security
        validation_result = validate_output_path(filename)
        if (.not. validation_result%is_valid()) then
            error_msg = "Output path validation failed: " // validation_result%get_message()
            return
        end if

        open (newunit=unit, file=filename, status='replace', action='write', iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Cannot create output file: "//filename
            return
        end if

        write (unit, '(A)') content
        close (unit)
        error_msg = ""
    end subroutine write_output_file

    ! Helper function to convert integer to string
    function int_to_str(num) result(str)
        integer, intent(in) :: num
        character(len=20) :: str
        write (str, '(I0)') num
    end function int_to_str

    ! Check if there are declarations before an explicit program (Issue #511)
    ! This is used by codegen to detect when implicit module wrapping is needed
    function has_declarations_before_program(tokens) result(has_decls)
        type(token_t), intent(in) :: tokens(:)
        logical :: has_decls
        integer :: i
        logical :: found_declarations, found_explicit_program
        
        has_decls = .false.
        found_declarations = .false.
        found_explicit_program = .false.
        
        ! Scan tokens to find declarations before explicit program
        do i = 1, size(tokens)
            if (tokens(i)%kind == TK_KEYWORD) then
                ! Check for explicit program statement
                if (tokens(i)%text == "program") then
                    ! Check that it's not "end program"
                    if (i == 1) then
                        found_explicit_program = .true.
                        exit
                    else if (i > 1) then
                        if (tokens(i-1)%kind /= TK_KEYWORD .or. tokens(i-1)%text /= "end") then
                            found_explicit_program = .true.
                            exit
                        end if
                    end if
                    
                ! Check for declaration keywords that appear before program
                else if (.not. found_explicit_program) then
                    select case (tokens(i)%text)
                    case ("type", "interface", "integer", "real", "logical", &
                          "character", "double", "complex")
                        ! For type, check if it's a type definition (type ::)
                        if (tokens(i)%text == "type") then
                            if (i + 1 <= size(tokens)) then
                                if (tokens(i+1)%kind == TK_OPERATOR .and. &
                                    tokens(i+1)%text == "::") then
                                    found_declarations = .true.
                                else if (i + 2 <= size(tokens) .and. &
                                         tokens(i+1)%kind == TK_IDENTIFIER .and. &
                                         tokens(i+2)%kind == TK_OPERATOR .and. &
                                         tokens(i+2)%text == "::") then
                                    ! type name :: case
                                    found_declarations = .true.
                                end if
                            end if
                        else
                            ! Other declaration keywords
                            found_declarations = .true.
                        end if
                    end select
                end if
            end if
        end do
        
        ! We have declarations before program if both conditions are met
        has_decls = found_declarations .and. found_explicit_program
    end function has_declarations_before_program

end module frontend_utilities