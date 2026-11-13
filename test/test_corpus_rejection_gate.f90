program test_corpus_rejection_gate
    implicit none
    character(len=16) :: os
    integer :: status
    call get_environment_variable('OS', os, status=status)
    if (status == 0) then
        if (os(1:7) == 'Windows') then
            print *, 'SKIP: corpus rejection gate requires a POSIX shell'
            stop 0
        end if
    end if
    call execute_command_line('bash scripts/test_corpus_rejection_gate.sh', &
                              exitstat=status)
    if (status /= 0) error stop 'corpus rejection gate regression failed'
end program
