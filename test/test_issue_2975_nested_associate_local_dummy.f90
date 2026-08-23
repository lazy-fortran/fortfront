program test_issue_2975_nested_associate_local_dummy
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD
    implicit none
    character(len=:), allocatable :: source
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result

    call read_example( &
        'examples/f90/issue_2975_nested_associate_local_dummy.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) error stop 'issue-2975 example was rejected'

    print *, 'PASS: issue-2975 nested ASSOCIATE local dummy example'

contains

    include 'common/read_example.inc'

end program test_issue_2975_nested_associate_local_dummy
