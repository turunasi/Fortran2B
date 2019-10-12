program extra

	implicit none
	integer::i, n, ier
	integer, allocatable::sco(:)
	real::average, sd, deviation, sd2

	write(*,'(A)',advance='no') 'input number of person (?) : '
	read(*,*) n

	if (n <= 0) then
		write(*,*) 'Error: number of person is less equal ZERO.'
		stop
	endif

	allocate(sco(n), stat=ier)

	if (ier /= 0) then
		write(*,*) 'Memory allocate Faild !.'
		stop
	endif

	write(*,*) 'sco input (0-100)(ex:80,70,68,76,82,77,91,69,66,81):'
	read(*,*) sco
	average = sum(sco) / REAL(n)
	sd = sqrt(sum(sco ** 2) / DBLE(n) - average ** 2.0)
	write(*,*) 'average=',average
	write(*,*) 'sd =',sd
	write(*,*) '--deviation value--'
	write(*,'(100(F6.2,/))') ((sco - average) * 10.0 / sd + 50.0)

	deallocate(sco)
	stop

end program extra
