program main
    implicit none
    real(8), allocatable:: A(:,:), B(:,:), C(:,:)
    integer:: i, j, t1, num
    num = 1000
    call rand(A, num, num)
    call rand(B, num, num)
    allocate(C(num, num))
    call tic(t1)
    call mymatmul(A, B, C)
    call toc(t1)
    call tic(t1)
    call mymatmulwithOpenMP(A, B, C)
    call toc(t1)
    contains
    subroutine tic(t1)
        integer, intent(inout) :: t1
        call system_clock(t1)
    end subroutine tic
    subroutine toc(t1)
        integer, intent(in) :: t1
        integer :: t2, t_rate, t_max, diff
        call system_clock(t2,t_rate,t_max)
        if (t2 < t1) then
            diff = (t_max - t1) + t2 + 1
        else
            diff = t2 - t1
        end if
        write(*,*) "Time = ", diff/dble(t_rate)
    end subroutine toc
    subroutine rand(A, n_row, n_col)
        integer :: ier, n_row, n_col, seed_size, i, j, count
        real(8),allocatable :: A(:,:)
        integer,allocatable :: seed(:)
        count = 0

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
    subroutine mymatmul(A, B, C)
        real(8),intent(in),allocatable::A(:,:), B(:,:)
        real(8),intent(inout),allocatable::C(:,:)
        integer:: An, Am, Bn, Bm, i, j, k, ier
        if (.not. allocated(A)) then
            write(*,*) ' mymatmul: 1st demension argument not allocated.'
            return
        endif
        if (.not. allocated(B)) then
            write(*,*) ' mymatmul: 2nd demension argument not allocated.'
            return
        endif
        An=size(A,1)
        Am=size(A,2)
        Bn=size(B,1)
        Bm=size(B,2)
        if (Am /= Bn) then
            write(*,*) ' mymatmul: errori, Am /= Bn...'
            return
        endif
        if (allocated(C)) then
            deallocate(C)
        endif
        allocate(C(An, Bm), stat=ier)
        if (ier /= 0) then
            write(*,*) ' mymatmul: Memory allocate Faild.., stat=',ier
            return
        endif
        C(:,:)=0.0d0
        do i=1,An
            do j=1,Bn
                do k=1,Bm
                    C(i,k)=C(i,k) + A(i,j) * B(j,k)
                enddo
            enddo
        enddo
    end subroutine mymatmul
    subroutine mymatmulwithOpenMP(A, B, C)
        real(8),intent(in),allocatable::A(:,:), B(:,:)
        real(8),intent(inout),allocatable::C(:,:)
        integer:: An, Am, Bn, Bm, i, j, k, ier
        if (.not. allocated(A)) then
            write(*,*) ' mymatmul: 1st demension argument not allocated.'
            return
        endif
        if (.not. allocated(B)) then
            write(*,*) ' mymatmul: 2nd demension argument not allocated.'
            return
        endif
        An=size(A,1)
        Am=size(A,2)
        Bn=size(B,1)
        Bm=size(B,2)
        if (Am /= Bn) then
            write(*,*) ' mymatmul: errori, Am /= Bn...'
            return
        endif
        if (allocated(C)) then
            deallocate(C)
        endif
        allocate(C(An, Bm), stat=ier)
        if (ier /= 0) then
            write(*,*) ' mymatmul: Memory allocate Faild.., stat=',ier
            return
        endif
        C(:,:)=0.0d0
        do i=1,An
            !$omp do
            do j=1,Bn
                do k=1,Bm
                    C(i,k)=C(i,k) + A(i,j) * B(j,k)
                enddo
            enddo
            !$omp end do
        enddo
    end subroutine mymatmulwithOpenMP
end program main
