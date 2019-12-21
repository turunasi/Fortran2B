program main
    implicit none
    write(*,*) "parallel"
    !$omp parallel
        write(*,*) "Hello!!"
    !$omp end parallel
    stop
end program main
