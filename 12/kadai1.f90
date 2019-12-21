program main
    !$ use omp_lib
    implicit none
    !$omp parallel
        write(*,*) "Hello!!", omp_get_thread_num()
    !$omp end parallel
    stop
end program main
