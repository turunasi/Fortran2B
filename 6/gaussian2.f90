module mygaussian2
    implicit none
    contains

    subroutine gaussian(a,b,n,x)

        implicit none
        integer,intent(in):: n
        real,intent(in):: a(n,n),b(n)
        real,intent(out):: x(n)
        real:: dd, wk, limit=1.0e-5
        real,allocatable:: Ab(:,:)
        integer:: i, j, k
        x(:) = 0.0

        k=n+1
        allocate(Ab(n,k))
        do i=1, n
            do j=1, n
                Ab(i,j)=a(i,j)
            end do
            Ab(i,n+1)=b(i)
        enddo

        do i=1, n
            wk=Ab(i,i)
            if (ABS(wk) < limit) then
                write(*,*) 'error: Zero value !!'
                return
            end if
            do j=i, n+1
                Ab(i,j)=Ab(i,j) / wk
            end do
            do k=i+1, n
                wk=Ab(k,i)
                do j=i,n+1
                    Ab(k,j)= Ab(k,j) - Ab(i,j) * wk
                end do
            end do
        end do

        x(n)=Ab(n,n+1) / Ab(n,n)
        do i=n-1, 1, -1
            dd = Ab(i,n+1)
            do j=i+1, n
                dd=dd - Ab(i,j) * x(j)
            enddo
            x(i)=dd / Ab(i,i)
        enddo
    end subroutine gaussian
end module mygaussian2
