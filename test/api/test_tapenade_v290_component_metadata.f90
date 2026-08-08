program test_tapenade_v290_component_metadata
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_file, &
        compile_frontend_from_string, INPUT_MODE_STANDARD, &
        component_path_query_t, query_component_path, &
        declaration_query_t, query_declaration, &
        derived_type_query_t, query_derived_type
    implicit none

    character(len=*), parameter :: fixture = &
        'examples/f90/issue_tapenade_v290_keyword_subroutine.f90'
    character(len=*), parameter :: compound_fixture = &
        'build/issue_tapenade_v290_compound_components.f90'
    character(len=:), allocatable :: source
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    integer :: unit, status

    call require_gfortran(fixture, 'exact v290 source')
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_file(fixture, result, options)
    call require(result%success(), &
        'FortFront rejected exact v290 source: '//result%error_msg)
    call require_solutiondata_components(result, 'exact v290 source')
    call require_component_paths(result, 'exact v290 source')

    call read_example(fixture, source)
    source = replace(source, &
        'real(kind=8), dimension(mcell) :: a'//new_line('a')// &
        '    real(kind=8), dimension(mcell) :: b'//new_line('a')// &
        '    real(kind=8), dimension(mcell) :: c', &
        'real(kind=8), dimension(mcell) :: a, b, c')
    open (newunit=unit, file=compound_fixture, status='replace', &
        access='stream', form='unformatted', action='write')
    write (unit) source
    close (unit)
    call require_gfortran(compound_fixture, 'compound component source')
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), &
        'FortFront rejected compound v290 component declaration: '// &
        result%error_msg)
    call require_solutiondata_components(result, 'compound declaration')
    call require_component_paths(result, 'compound declaration')

    print *, 'PASS: Tapenade v290 derived component metadata contract'

contains

    include '../common/read_example.inc'

    subroutine require_gfortran(path, description)
        character(len=*), intent(in) :: path, description
        character(len=:), allocatable :: command

        command = 'gfortran -std=f2018 -pedantic -Wall -Wextra '// &
            '-fsyntax-only '//trim(path)
        call execute_command_line(command, wait=.true., exitstat=status)
        call require(status == 0, 'gfortran rejected '//trim(description))
    end subroutine require_gfortran

    subroutine require_solutiondata_components(result, description)
        type(compiler_frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: description
        type(derived_type_query_t) :: derived
        type(declaration_query_t) :: declaration
        character(len=16), parameter :: expected(4) = [character(len=16) :: &
            'n', 'a', 'b', 'c']
        integer :: i, j, matches

        matches = 0
        do i = 1, result%arena%size
            derived = query_derived_type(result%arena, i)
            if (.not. derived%found) cycle
            if (trim(derived%name) /= 'solutiondata') cycle
            matches = matches + 1
            call require(size(derived%component_indices) == size(expected), &
                trim(description)//': component count')
            do j = 1, size(expected)
                declaration = query_declaration(result%arena, &
                    derived%component_indices(j))
                call require(declaration%found, &
                    trim(description)//': component declaration missing')
                call require(trim(declaration%name) == trim(expected(j)), &
                    trim(description)//': component order/name')
                if (j > 1) then
                    call require(declaration%is_array, &
                        trim(description)//': array flag for '// &
                        trim(declaration%name))
                    call require(size(declaration%dimension_indices) == 1, &
                        trim(description)//': dimension metadata for '// &
                        trim(declaration%name))
                else
                    call require(.not. declaration%is_array, &
                        trim(description)//': scalar metadata for n')
                end if
            end do
        end do
        call require(matches == 1, trim(description)//': type query count')
    end subroutine require_solutiondata_components

    subroutine require_component_paths(result, description)
        type(compiler_frontend_result_t), intent(in) :: result
        character(len=*), intent(in) :: description
        type(component_path_query_t) :: path
        type(declaration_query_t) :: declaration
        character(len=16), parameter :: expected(2) = ['b', 'c']
        integer :: i, j
        logical :: found

        do j = 1, size(expected)
            found = .false.
            do i = 1, result%arena%size
                path = query_component_path(result%arena, i)
                if (.not. path%found) cycle
                if (size(path%component_names) /= 1) cycle
                if (trim(path%component_names(1)) /= expected(j)) cycle
                call require(size(path%component_declaration_indices) == 1, &
                    trim(description)//': component path metadata for '// &
                    expected(j))
                declaration = query_declaration(result%arena, &
                    path%component_declaration_indices(1))
                call require(declaration%found .and. &
                    trim(declaration%name) == expected(j), &
                    trim(description)//': declaration identity for '//expected(j))
                found = .true.
                exit
            end do
            call require(found, trim(description)// &
                ': component path missing for '//expected(j))
        end do
    end subroutine require_component_paths

    function replace(text, old, new) result(output)
        character(len=*), intent(in) :: text, old, new
        character(len=:), allocatable :: output
        integer :: position

        position = index(text, old)
        call require(position > 0, 'test fixture replacement anchor missing')
        output = text(:position - 1)//new//text(position + len(old):)
    end function replace

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            write (error_unit, '(A)') 'FAIL: '//trim(message)
            error stop 1
        end if
    end subroutine require

end program test_tapenade_v290_component_metadata
