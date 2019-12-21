program main
	use extra1_module
	implicit none
	integer :: n
	real(8) :: x,ans

	write(*,'(A)',advance='no') 'Input integer number n :'
	read(*,*) n
	write(*,'(A)',advance='no') 'Input real number x :'
	read(*,*) x

	ans = mydexp(n,x)

	write(*,*) 'exp(x) =',exp(x)
	write(*,*) 'mydexp(x) =',ans

	stop
end program main
