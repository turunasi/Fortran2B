module mygaussian1
    
    implicit none

    contains

    subroutine gaussian(a,b,n,x)

        implicit none
        real,intent(in) :: a(n,n),b(n)
        real,intent(out) :: x(n)
        real :: dd
        real,allocatable :: Ab(:,:)
        integer :: n,i,j,k

        allocate(Ab(n,n+1))
        do i=1,n
            Ab(i,:) = a(i,:)
            Ab(i,n+1) = b(i)
        end do

        do i=1,n
            write(*,*) Ab(1,:)
        end do

        do i=1,n+1
            Ab(1,i) = Ab(1,i) * 0.5
        end do

        do i=1,n+1
            Ab(2,i)=Ab(2,i) - Ab(1,i) * 3.0
            Ab(3,i)=Ab(3,i) - Ab(1,i) * 4.0
        enddo

        do i=1,n+1
            Ab(2,i)=Ab(2,i) * (-1.0)
        enddo

        do i=1,n+1
            Ab(3,i)=Ab(3,i) - Ab(2,i) * (-2.0)
        enddo

        do i=1,n+1
            Ab(3,i)=Ab(3,i) * 0.5
        enddo

        x(3)=Ab(3,4)
        x(2)=Ab(2,2) - Ab(2,3) * x(3)
        x(1)=Ab(1,1) * 3.0 - Ab(1,2)*x(2) - Ab(1,3)*x(3)

    end subroutine gaussian
end module mygaussian1
