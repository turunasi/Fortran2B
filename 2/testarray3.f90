program array
	implicit none
	integer :: i,j,n,ier
	real,allocatable :: a(:), b(:), c(:)

	write(*,'(A)',advance='no') 'input number of elements (?):'
	read(*,*) n

	if (n <= 0) then
		write(*,*) 'Error: number of elements is less equal ZERO.'
		stop
	endif

	allocate(a(n),b(n),c(n),stat=ier)
	if (ier /= 0) then
		if (allocated(a)) deallocate(a)
		if (allocated(b)) deallocate(b)
		if (allocated(c)) deallocate(c)
		write(*,*) 'memory allocate faild !.'
		stop
	endif

	a(:)=(/(2*i,i=1,n)/)
	b(:)=(/(-i,i=11,10+n)/)
	
	write(*,*) '-----<a>-----'
	do i=1, n, 10
		write(*,'(10(" ",F5.1))') (a(j), j=i, MIN(i+9, n))
	enddo
	write(*,*) '-----<b>-----'

	c = a / b

	write(*,*) ' c = a / b'
	write(*,*) '-----<c>-----'
	do i=1, n, 10
		write(*,'(10(" ",F5.1))') (c(j), j=i, MIN(i+9, n))
	enddo

	c = b ** 2 + 3

	write(*,*)
	write(*,*) ' c = b ** 2 + 3'
	write(*,*) '-----<c>-----'
	
	do i=1, n, 10
		write(*,'(10(" ",F5.1))') (c(j), j=i, MIN(i+9, n))
	enddo
	
	c = sqrt(a) - b
	
	write(*,*)
	write(*,*) ' c = sqrt(a) - b'
	write(*,*) '-----<c>-----'
	
	do i=1, n, 10
		write(*,'(10(" ",F5.1))') (c(j), j=i, MIN(i+9, n))
	enddo
	
	if (allocated(a)) deallocate(a)
	if (allocated(b)) deallocate(b)
	if (allocated(c)) deallocate(c)
	
	stop
end program array
