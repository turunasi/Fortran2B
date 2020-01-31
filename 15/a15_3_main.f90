program matrix_33
  use fraction
  implicit none
  real:: a(3,3),b(3),x(3)
  a(1,1)=3; a(1,2)=2; a(1,3)=1
  a(2,1)=2; a(2,2)=3; a(2,3)=1
  a(3,1)=1; a(3,2)=2; a(3,3)=3
  b(1)=39; b(2)=34; b(3)=26
  call gaussian(a,b,3,x)
  write(*,*) 'x=',x(1),x(2),x(3)
  stop
  contains
  subroutine gaussian (a,b,n,x)
    implicit none
    real,intent(in) :: a(n,n), b(n)
    integer,intent(in) :: n
    real,intent(out) :: x(n)
    type(t_fraction), allocatable :: Ab(:,:)
    integer :: i,j,k

    allocate(Ab(n,n+1))
    do i=1,n
        do j=i,n
            Ab(i,j) = fracset(INT(a(i,j)),1)
        end do
        Ab(i,n+1) = fracset(INT(b(i)),1)
    end do

    do i=1,n
        do j=1,n+1
            call disp(Ab(i,j))
        end do
    end do

    do i=1,n+1
        Ab(1,i) = Ab(1,i) / Ab(1,1)
    end do

    do i=1,n+1
        Ab(2,i)=Ab(2,i) - Ab(1,i) * Ab(2,1)
        Ab(3,i)=Ab(3,i) - Ab(1,i) * Ab(2,1)
    enddo

    do i=1,n+1
        Ab(2,i)=Ab(2,i) / Ab(2,2)
    enddo

    do i=1,n+1
        Ab(1,i)=Ab(1,i) - Ab(2,i) * Ab(1,2)
        Ab(3,i)=Ab(3,i) - Ab(2,i) * Ab(3,2)
    enddo

    do i=1,n+1
        Ab(3,i)=Ab(3,i) / Ab(3,3)
    enddo

    do i=1,n+1
        Ab(1,i)=Ab(1,i) - Ab(3,i) * Ab(1,3)
        Ab(2,i)=Ab(2,i) - Ab(3,i) * Ab(2,3)
    enddo

    x(3)=frac_to_real(Ab(3,4))
    x(2)=frac_to_real(Ab(2,4))
    x(1)=frac_to_real(Ab(1,4))

  end subroutine gaussian
end program matrix_33
