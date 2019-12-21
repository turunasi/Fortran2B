program main
    !$ use omp_lib
    implicit none
    integer :: I, j
    !$omp parallel
    !$omp do
    do i=1, 5
        do j=1, 5
            write(*,*) i, j, omp_get_thread_num()
        end do
    end do
    !$omp end do
    do i=1, 5
        !$omp do
        do j=1, 5
            write(*,*) i, j, omp_get_thread_num()
        end do
        !$omp end do
    end do
    !$omp end parallel
    stop
end program main
