module mymod
  implicit none
  contains
  subroutine pivot(target_i, TBL)
    implicit none
    real(8), intent(inout) :: TBL(:,:)
    integer,intent(in) :: target_i
    integer :: i, j, max_i, max_j, wk_i
    real(8) :: wk_f
    max_i = SIZE(TBL,1)
    max_j = SIZE(TBL,2)
    if (target_i <= 0) return
    if (target_i >= max_i) return
    wk_i = target_i
    wk_f = ABS(TBL(target_i, target_i))
    do i = target_i+1, max_i
      if (wk_f < ABS(TBL(i, target_i))) then
        wk_i = i
        wk_f = ABS(TBL(i, target_i))
      endif
    enddo
    if (wk_i /= target_i) then
      do j = target_i, max_j
        wk_f = TBL(target_i,j)
        TBL(target_i,j) = TBL(wk_i,j)
        TBL(wk_i,j) = wk_f
      enddo
    endif
  end subroutine pivot

  subroutine gaussian(a, b, n, x)
    implicit none
    integer, intent(in) :: n
    real(8), intent(in) :: a(n, n), b(n)
    real(8), intent(out) :: x(n)
    real(8) :: dd, wk, limit = 1.0e-5
    real(8) , allocatable :: Ab(:, :)
    integer :: i, j, k
    x(:) = 0.0
    k = n + 1
    allocate(Ab(n, k))
    do i = 1, n
      do j = 1, n
        Ab(i, j) = a(i, j)
      enddo
      Ab(i, n + 1) = b(i)
    enddo
    do i = 1, n
      call pivot(i, Ab)
      wk = Ab(i, i)
      do j = i, n + 1
        Ab(i, j) = Ab(i, j) / wk
      enddo
      do k = i + 1, n
        wk = Ab(k,i)
        do j = i, n + 1
          Ab(k,j) = Ab(k,j) - Ab(i,j) * wk
        enddo
      enddo
    enddo

    x(n) = Ab(n,n+1) / Ab(n,n)
    do i = n-1, 1, -1
      dd = Ab(i,n+1)
      do j = i+1, n
        dd = dd - Ab(i,j) * x(j)
      enddo
      x(i) = dd / Ab(i,i)
    enddo
  end subroutine gaussian
end module mymod

program a8_main4
  use mymod
  implicit none
  integer :: i
  real(8) :: a(3, 3), b(3), x(3)
  a(1, 1) = 3; a(1, 2) = 2; a(1, 3) = 1
  a(2, 1) = 2; a(2, 2) = 3; a(2, 3) = 1
  a(3, 1) = 1; a(3, 2) = 2; a(3, 3) = 3
  b(1) = 39; b(2) = 34; b(3) = 26
  call gaussian(a, b, 3, x)
  write(*, *) 'x = '
  do i = 1, 3
    write(*, '(F10.3)', advance='no') x(i) 
  end do
  write(*, *)
end program a8_main4
