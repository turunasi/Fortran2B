module extra1_module
	implicit none
	contains

	function mydexp(n,x)
		integer,intent(in) :: n
		real(8),intent(in) :: x
		integer :: i
		real(8) :: x_in_func,n_in_func,mydexp

		mydexp = 1.0d0
		x_in_func = 1.0d0
		n_in_func = 1.0d0

		do i=1,n
			x_in_func = x_in_func * x
			n_in_func = n_in_func * i
			mydexp = mydexp + (x_in_func/n_in_func)
		enddo
	end function mydexp
end module extra1_module

