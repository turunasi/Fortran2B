program extra2_main
	use extra2_module
	implicit none
	integer :: n,i
	real(8) :: x(1)
	complex(8) :: ans

	write(*,'(A)',advance='no') 'Input integer number n :'
	read(*,*) n

	x(1) = 1.005
	ans = mydmexp(n,x)

	write(*,'(A,I0,A,F10.5,A)') '===x(',1,'):',x(1),'==='
	write(*,*) 'exp(x) =',exp( complex(0.0, x(1)) )
	write(*,*) 'mydmexp(x) =',ans

	stop
end program extra2_main
