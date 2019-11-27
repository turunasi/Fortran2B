program a8_main3
  use myfunc
  implicit none
  integer :: tbl_int(100)
  integer :: i
  real :: tbl_real(100)
  real(8) :: rnd
  
  do i = 1, 100
    call random_number(rnd)
    tbl_real(i) = rnd
    tbl_int(i) = int(rnd * 100.0) + 1
  end do

  tbl_real = mysort(tbl_real, 100, 0)
  tbl_int = mysort(tbl_int, 100, 1)
  write (*, *) 'Result: '
  call showArrays(tbl_int, tbl_real)
end program a8_main3

subroutine showArrays(tbl_int, tbl_real)
  integer, intent(in) :: tbl_int(100)
  real, intent(in) :: tbl_real(100)
  integer :: i, j
  write (*, *) 'Real: '
  do i = 0, 24
    do j = 4 * i + 1, 4 * i + 4 
      write (*, '(F5.4)', advance='no') tbl_real(j)
    end do
    write (*, *)
  end do

  write (*, *) 'Int: '
  do i = 0, 24
    do j = 4 * i + 1, 4 * i + 4 
      write (*, '(I4)', advance='no') tbl_int(j)
    end do
    write (*, *)
  end do
end subroutine showArrays
