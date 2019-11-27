module myfunc
  implicit none
  interface mysort
    module procedure mysort_int, mysort_real
  end interface mysort

  contains
    function mysort_int(arr, arr_size, order) result(ret)
      implicit none
      integer, intent(in) :: arr_size, order, arr(arr_size)
      integer:: ret(arr_size)
      integer :: i, j, swap
      
      ret(:) = arr(:)
      do i = 2, arr_size
        do j = i - 1, 1, -1
          if (order == 0 .and. ret(j) <= ret(j + 1)) exit
          if (order == 1 .and. ret(j + 1) <= ret(j)) exit
          swap = ret(j)
          ret(j) = ret(j + 1)
          ret(j + 1) = swap
        end do
      end do
    end function mysort_int

    function mysort_real(arr, arr_size, order) result(ret)
      implicit none
      integer, intent(in) :: arr_size, order
      real, intent(in) :: arr(arr_size)
      real :: ret(arr_size)
      real(8) :: swap
      integer :: i, j
      
      ret(:) = arr(:)
      do i = 2, arr_size
        do j = i - 1, 1, -1
          if (order == 0 .and. ret(j) <= ret(j + 1)) exit
          if (order == 1 .and. ret(j + 1) <= ret(j)) exit
          swap = ret(j)
          ret(j) = ret(j + 1)
          ret(j + 1) = swap
        end do
      end do
    end function mysort_real
end module myfunc
