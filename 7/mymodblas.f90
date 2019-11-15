module mymodblas
    implicit none
    integer :: count=0
    contains

        subroutine disp(TBL)
            real(8),allocatable :: TBL(:,:)
            integer :: i,j,iN,jN
            if(.not. allocated(TBL)) then
                write(*,*) 'disp: Table not allocated!'
                return
            end if
            iN = size(TBL,1)
            jN = size(TBL,2)
            if((iN .le. 0) .or. (jN .le. 0)) then
                write(*,*) ' disp: Table size is less equal 0 !'
                return
            end if
            write(*,*) '--disp--'
            do i=1,iN
                write(*,'(A,I0,A)',advance="no")'(',i,',:)='
                do j=1,jN
                    write(*,'(1X, F10.5)',advance="no") TBL(i,j)
                end do
                write(*,*)
            end do
        end subroutine disp

        subroutine rand(A, n_row, n_col)
            integer :: ier, n_row, n_col, seed_size, i, j
            real(8),allocatable :: A(:,:)
            integer,allocatable :: seed(:)

            if (allocated(A)) then
                deallocate(A)
            end if
            allocate(A(n_row,n_col), stat=ier)
            if (ier /= 0) then
                write(*,*) 'Memory allocate Faild. stat=', ier
                return
            end if
            count = count + 1
            call random_seed(size=seed_size)
            allocate(seed(seed_size))
            do i=1,seed_size
                call system_clock(count=seed(i))
                seed(i) = seed(i) + 1
            end do
            call random_seed(put=seed)

            do i=1,n_col
                do j=1,n_row
                    call random_number(A(i,j))
                end do
            end do
            deallocate(seed)
        end subroutine rand

        subroutine mydlss(A,b,x)

            real(8),intent(in) :: A(:,:),b(:,:)
            real(8),allocatable,intent(inout) :: x(:,:)
            integer,allocatable :: ipiv(:)
            integer :: an,am,bn,bm,info
            an = size(A,1)
            am = size(A,2)
            bn = size(b,1)
            bm = size(b,2)
            if (allocated(x)) then
                deallocate(x)
            end if
            allocate(x(bn,bm))
            x = b
            allocate(ipiv(an))
            if (an /= am) then
                write(*,*) "Error:mydlss"
                return
            end if
            call dgesv(an,bm,A,an,ipiv,x,bn,info)
            if (info /= 0) then
                write(*,*) "Error:mylss:info=", info
                if (info < 0) then
                    write(*,'(A,I0,A)')'the ',info,'-th argument had an illigal value'
                else
                    write(*,'(A,I0,A,I0,A)')'U(',info,',',info,') is exactly zero.'
                end if
                return
            end if
        end subroutine mydlss

        subroutine mydsymeig(A,E)
            real(8),intent(in) :: A(:,:)
            real(8),allocatable,intent(inout) :: E(:,:)
            real(8), allocatable :: V(:,:),work(:)
            real(8) :: lw
            integer :: an, am, info, lda, lwork
            an = size(A,1)
            am = size(A,2)
            if (an == am ) then
                lda = an
                if (allocated(E)) then
                    deallocate(E)
                end if
                allocate(E(an,1))
                allocate(V(an,am))
                V = A
                call dsyev('N','U',an,V,an,E,lw,-1,info)
                lwork = int(lw)
                allocate(work(1:lwork))
                call dsyev('N','U',an,V,an,E,work,lwork,info)
                deallocate(work)
                if (info == 0) then
                    return
                else
                    write(*,*) 'Error : mydsymeig : info is', info
                end if
            else
                write(*,*) 'Error : mydsymeig : an .neq. am'
            end if
        end subroutine mydsymeig
end module mymodblas
