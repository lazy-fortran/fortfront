read (*, *) x
write (*, '(A)') 'test'
allocate (arr(10))
deallocate (arr)
stop 'error'
return
cycle
exit
where (arr > 0)
    arr = 1
end where
forall (i = 1:10)
    arr(i) = i
end forall
ptr => target
