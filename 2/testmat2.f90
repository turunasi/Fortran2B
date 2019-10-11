program mat
	implicit none
	integer :: i,j,n,m,ier
	real,allocatable :: A(:,:)
	character(32) :: fmt

	write(*,'(A)',advance='no')'input number of elements m,n (ex. "4 5"):'
	read(*,*) m,n

	allocate(A(m,n) stat=ier)
	if (ier /= 0) then
		write(*,*) 'Memory allocate Failed!'
		stop
	endif

	A(:,:) =0.0

	do i=2,m,2
		do j=2,n,2
			A(i,j) = REAL(i*j)
		enddo
	enddo

	write(fmt, '(A,I0,A')'(',n,'F5.1)'
	write(*.fmt)(A(i,:),i=1,m)
	write(*,*) A

	deallocate(A)
	stop
end program mat
