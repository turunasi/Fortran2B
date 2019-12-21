module extra3_module
	implicit none
	interface myexp
		module procedure mydexp,mydmexp
	end interface
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

	function mydmexp(n,x)
		integer,intent(in) :: n
		real(8),intent(in) :: x(:)
		complex :: i = (0.0e0,1.0e0)
		integer :: j
		real(8) :: x_in_func,n_in_func
		complex(8) :: mydmexp
		
		mydmexp = (1.0d0,0.0d0)
		x_in_func = 1.0d0
		n_in_func = 1.0d0

		do j=1,n
			x_in_func = x_in_func * x(1)
			n_in_func = n_in_func * j
			mydmexp = mydmexp + (x_in_func/n_in_func) * (i**j)
		enddo
	end function mydmexp
end module extra3_module
