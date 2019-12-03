module mymod
    implicit none
    type bunnsuu
        integer :: bunbo, bunsi
    end type bunnsuu
    interface disp
        module procedure dispbunnsuu
    end interface
    contains

    function bunnsuuplus (A,B) result(C)
        type(bunnsuu), intent(in) :: A,B
        type(bunnsuu) :: C
        C%bunsi = A%bunsi * B%bunbo + B%bunsi * A%bunbo
        C%bunbo = A%bunbo * B%bunbo
        call euclidean(C)
    end function bunnsuuplus

    function bunnsuuminus (A,B) result(C)
        type(bunnsuu), intent(in) :: A,B
        type(bunnsuu) :: C
        C%bunsi = A%bunsi * B%bunbo - B%bunsi * A%bunbo
        C%bunbo = A%bunbo * B%bunbo
        call euclidean(C)
    end function bunnsuuminus

    function bunnsuumul (A,B) result(C)
        type(bunnsuu), intent(in) :: A,B
        type(bunnsuu) :: C
        C%bunsi = A%bunsi * B%bunsi
        C%bunbo = A%bunbo * B%bunbo
        call euclidean(C)
    end function bunnsuumul

    function bunnsuudevide (A,B) result(C)
        type(bunnsuu), intent(in) :: A,B
        type(bunnsuu) :: C
        C%bunsi = A%bunsi * B%bunbo
        C%bunbo = A%bunsi * B%bunbo
        call euclidean(C)
    end function bunnsuudevide

    subroutine dispbunnsuu(A)
        type(bunnsuu), intent(in) :: A
        write (*,'(I0,A,I0)') A%bunsi, '/', A%bunbo
    end subroutine dispbunnsuu

    subroutine euclidean(A)
        type(bunnsuu), intent(inout) :: A
        integer :: amari,i,j,GCD
        if (A%bunsi .gt. A%bunbo) then
            i = A%bunsi
            j = A%bunbo
        else if (A%bunsi .lt. A%bunbo) then
            i = A%bunbo
            j = A%bunsi
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
100     A%bunsi = A%bunsi/GCD
        A%bunbo = A%bunbo/GCD
    end subroutine euclidean

end module mymod
