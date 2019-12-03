program main
    implicit none
    integer :: i,j,n,m,ier
    real, allocatable :: A(:,:)
    character(32) :: fmt

    write(*,'(A)',advance='no') 'Input number of columns in matrix :'
    read(*,*) n
    write(*,'(A)',advance='no') 'Input number of rows in matrix :'
    read(*,*) m
    allocate(A(m,n),stat=ier)
    if (ier /= 0) goto 100

    A(:,:) = -1
    do i=1,m,2
        do j=2,n,2
            A(i,j) = REAL(i * j)
        enddo
    enddo

    call printMatrix(A)
    stop
100 write(*,*) 'Memory allocate Failed!'
    stop
    contains
        subroutine printMatrix (TBL)
            implicit none
            real,allocatable :: TBL(:,:)
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
            do i=1,iN
                write(*,'(A,I0,A)',advance="no")'(',i,',:)='
                do j=1,jN
                    write(*,'(I5)',advance="no") INT(TBL(i,j))
                end do
                write(*,*)
            end do
        end subroutine printMatrix
end program main
