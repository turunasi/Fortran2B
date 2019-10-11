program arr
	implicit none
	integer :: i,j,n,ier
	real,allocatable :: a(:)

	write(*,'(A)',advance='no')'input number of elements a(?):'
	read(*,*) n
	
	do while(n>0)
		allocate(a(n),stat=ier)
		if (ier == 0) then
			a(:) = 0.0
			
			do i=2,n,2
					a(i) = i*i
			enddo
			
			do i=1,n,10
					write(*,'(10(" ",F7.1,/))')(a(j),j=i,MIN(i+9,n))
			enddo
			deallocate(a)
		else
			write(*,*) 'Memory allocate Faild !'
		endif

		write(*,'(A)',advance='no') 'input number of elements a(?):'
		read (*,*) n
	enddo
	write(*,*) 'Exit'
	stop
end program arr
