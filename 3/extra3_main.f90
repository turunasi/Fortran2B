program extra3_main
	use extra3_module
	implicit none
	integer :: n,i
	real(8) :: x(1),x2

	write(*,'(A)',advance='no') 'Input integer number n :'
	read(*,*) n

	x(1) = 1.005
	x2 = 1.005

	write(*,'(A,F10.5,A)') '===x(1):',x(1),'==='
	write(*,*) 'exp(x) =',exp( x2 )
	write(*,*) 'myexp =',myexp(n,x2)
	write(*,*) 'exp(ix) =',exp( complex(0.0, x(1)) )
	write(*,*) 'myexp(ix) =',myexp(n,x(1))
	stop
end program extra3_main
