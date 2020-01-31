module fraction
    implicit none
    type t_fraction
        integer, private :: numerator, denominator
    end type t_fraction
    interface operator(+)
        module procedure frac_plus_frac, int_plus_frac, frac_plus_int
    end interface
    interface operator(-)
        module procedure frac_minus_frac, int_minus_frac, frac_minus_int
    end interface
    interface operator(*)
        module procedure frac_mult_frac, int_mult_frac, frac_mult_int
    end interface
    interface operator(/)
        module procedure frac_divi_frac, int_divi_frac, frac_divi_int
    end interface
    interface disp
        module procedure dispfrac
    end interface disp
    contains
    function frac_to_real(n) result(f)
        type(t_fraction),intent(in) :: n
        real :: f
        f = n%numerator/n%denominator
    end function frac_to_real
    function fracset(n,d) result(f)
        integer, intent(in) :: n,d
        type(t_fraction) :: f
        f%numerator = n
        f%denominator = d
    end function fracset
    subroutine dispfrac(f)
        type(t_fraction), intent(in) :: f
        write(*,*) 'numerator:', f%numerator, 'denominator:', f%denominator
    end subroutine dispfrac
    function frac_plus_frac(f1, f2) result(g)
        type(t_fraction), intent(in) :: f1, f2
        type(t_fraction) :: g
        g = fracset(f1%numerator*f2%denominator+f2%numerator*f1%denominator, f1%denominator*f2%denominator)
        call disp(g)
        call euclidean(g)
    end function frac_plus_frac
    function int_plus_frac(c, f) result(g)
        integer, intent(in) :: c
        type(t_fraction), intent(in) :: f
        type(t_fraction) :: g
        g = fracset(f%numerator+f%denominator*c, f%denominator)
        call euclidean(g)
    end function int_plus_frac
    function frac_plus_int(f, c) result(g)
        integer, intent(in) :: c
        type(t_fraction), intent(in) :: f
        type(t_fraction) :: g
        g = fracset(f%numerator+f%denominator*c, f%denominator)
        call euclidean(g)
    end function frac_plus_int
    function frac_minus_frac(f1, f2) result(g)
        type(t_fraction), intent(in) :: f1, f2
        type(t_fraction) :: g
        g = fracset(f1%numerator*f2%denominator-f2%numerator*f1%denominator, f1%denominator*f2%denominator)
        call euclidean(g)
    end function frac_minus_frac
    function int_minus_frac(c, f) result(g)
        integer, intent(in) :: c
        type(t_fraction), intent(in) :: f
        type(t_fraction) :: g
        g = fracset(f%denominator*c-f%numerator, f%denominator)
        call euclidean(g)
    end function int_minus_frac
    function frac_minus_int(f, c) result(g)
        integer, intent(in) :: c
        type(t_fraction), intent(in) :: f
        type(t_fraction) :: g
        g = fracset(f%numerator-f%denominator*c, f%denominator)
        call euclidean(g)
    end function frac_minus_int
    function frac_mult_frac(f1, f2) result(g)
        type(t_fraction), intent(in) :: f1, f2
        type(t_fraction) :: g
        g = fracset(f1%numerator*f2%numerator, f1%denominator*f2%denominator)
        call euclidean(g)
    end function frac_mult_frac
    function int_mult_frac(c, f) result(g)
        integer, intent(in) :: c
        type(t_fraction), intent(in) :: f
        type(t_fraction) :: g
        g = fracset(c*f%numerator, f%denominator)
        call euclidean(g)
    end function int_mult_frac
    function frac_mult_int(f, c) result(g)
        integer, intent(in) :: c
        type(t_fraction), intent(in) :: f
        type(t_fraction) :: g
        g = fracset(f%numerator*c, f%denominator)
        call euclidean(g)
    end function frac_mult_int
    function frac_divi_frac(f1, f2) result(g)
        type(t_fraction), intent(in) :: f1, f2
        type(t_fraction) :: g
        g = fracset(f1%numerator*f2%denominator, f1%denominator*f2%numerator)
        call euclidean(g)
    end function frac_divi_frac
    function int_divi_frac(c, f) result(g)
        integer, intent(in) :: c
        type(t_fraction), intent(in) :: f
        type(t_fraction) :: g
        g = fracset(c*f%denominator,f%numerator)
    end function int_divi_frac
    function frac_divi_int(f, c) result(g)
        integer, intent(in) :: c
        type(t_fraction), intent(in) :: f
        type(t_fraction) :: g
        g = fracset(f%numerator, f%denominator*c)
    end function frac_divi_int
    subroutine euclidean(A)
        type(t_fraction), intent(inout) :: A
        integer :: amari,i,j,GCD
        if (A%numerator .gt. A%denominator) then
            i = A%numerator
            j = A%denominator
        else if (A%numerator .lt. A%denominator) then
            i = A%denominator
            j = A%numerator
        else
            GCD = 1
            goto 100
        end if
        do while ( i /= 0 )
            amari = MOD(i,j)
            i = j
            j = amari
        end do
        GCD = i
100     A%numerator = A%numerator/GCD
        A%denominator = A%denominator/GCD
    end subroutine euclidean
end module fraction
