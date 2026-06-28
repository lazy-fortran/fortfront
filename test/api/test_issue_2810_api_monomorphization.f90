program test_issue_2810_api_monomorphization
    use fortfront, only: compiler_frontend_result_t, &
        compiler_frontend_options_t, &
        compile_frontend_from_string, emit_fortran
    implicit none

    logical :: all_passed

    print *, '=== Issue 2810: compile_frontend_from_string monomorphization ==='
    print *

    all_passed = test_polymorphic_calls_monomorphized()

    print *
    if (all_passed) then
        print *, 'Issue 2810 API monomorphization test passed!'
        stop 0
    else
        print *, 'Issue 2810 API monomorphization test failed!'
        stop 1
    end if

contains

    include '../common/read_example.inc'

    logical function test_polymorphic_calls_monomorphized()
        type(compiler_frontend_result_t) :: result
        type(compiler_frontend_options_t) :: options
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output

        test_polymorphic_calls_monomorphized = .true.
        print *, 'Testing standardized output is monomorphized...'

        call read_example('examples/lf/monomorphization_simple.lf', source)

        options%standardize = .true.
        call compile_frontend_from_string(source, result, options)

        if (.not. result%success()) then
            print *, '  FAIL: unexpected diagnostic -> ', &
                trim(result%diagnostic_text)
            test_polymorphic_calls_monomorphized = .false.
            return
        end if

        call emit_fortran(result%arena, result%root_index, output)

        if (index(output, 'interface add') == 0) then
            print *, '  FAIL: generic interface missing'
            print *, '  Output:', output
            test_polymorphic_calls_monomorphized = .false.
            return
        end if

        if (index(output, 'add__i32_i32') == 0) then
            print *, '  FAIL: integer specialization add__i32_i32 missing'
            print *, '  Output:', output
            test_polymorphic_calls_monomorphized = .false.
            return
        end if

        if (index(output, 'add__r64_r64') == 0) then
            print *, '  FAIL: real(8) specialization add__r64_r64 missing'
            print *, '  Output:', output
            test_polymorphic_calls_monomorphized = .false.
            return
        end if

        print *, '  PASS: API output monomorphized via interface and variants'
    end function test_polymorphic_calls_monomorphized

end program test_issue_2810_api_monomorphization
