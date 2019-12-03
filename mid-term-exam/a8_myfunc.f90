module myfunc
  implicit none
  interface mysort
    module procedure mysort_int, mysort_real
  end interface mysort

  contains
    function mysort_int(arr, arr_size, order)result(sorted_array)
      implicit none
      integer, intent(in) :: arr_size, order, arr(arr_size)
      integer:: sorted_array(arr_size)
      integer :: i, j

      sorted_array(:) = arr(:)
      do i = 2, arr_size
        do j = i - 1, 1, -1
          if (order == 0 .and. sorted_array(j) <= sorted_array(j + 1)) exit
          if (order == 1 .and. sorted_array(j + 1) <= sorted_array(j)) exit
          call swap_int(sorted_array,arr_size,j,j+1)
        end do
      end do
    end function mysort_int

    function mysort_real(arr, arr_size, order) result(sorted_array)
      implicit none
      integer, intent(in) :: arr_size, order
      real, intent(in) :: arr(arr_size)
      real :: sorted_array(arr_size)
      integer :: i, j

      sorted_array(:) = arr(:)
      do i = 2, arr_size
        do j = i - 1, 1, -1
          if (order == 0 .and. sorted_array(j) <= sorted_array(j + 1)) exit
          if (order == 1 .and. sorted_array(j + 1) <= sorted_array(j)) exit
          call swap_real(sorted_array,arr_size,j,j+1)
        end do
      end do
    end function mysort_real

    subroutine swap_int(array,array_size,a,b)

        implicit none
        integer,intent(in) :: a,b,array_size
        integer,intent(inout) :: array(array_size)
        integer :: c 

        c = array(a)
        array(a) = array(b)
        array(b) = c

    end subroutine swap_int

    subroutine swap_real(array,array_size,a,b)

        implicit none
        integer,intent(in) :: a,b,array_size
        real,intent(inout) :: array(array_size)
        real :: c 

        c = array(a)
        array(a) = array(b)
        array(b) = c

    end subroutine swap_real

end module myfunc
