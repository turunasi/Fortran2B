program main
    use myfunc
    implicit none
    integer :: i,j,tbl_int(100)
    real :: tbl_real(100)
    real(8) :: rnd

    do i = 1,100
        call random_number(rnd)
        tbl_real(i) = rnd
        tbl_int(i) = INT(rnd*100.0)
    end do

    tbl_real = mysort(tbl_real, 100, 0)
    tbl_int = mysort(tbl_int, 100, 1)
    write (*, *) 'Real Array: '
    do i = 0, 10
        do j = 10*i+1, 10*i+10
            write (*, '(F5.4)', advance='no') tbl_real(j)
        end do
        write (*,*)
    end do

    write (*, *) 'Integer Array: '
    do i = 0, 10
        do j = 10*i+1, 10*i+10
            write (*, '(I4)', advance='no') tbl_int(j)
        end do
        write (*, *)
    end do
    stop
end program main
