program main
    implicit none
    write(*,*) Fibo(3)
    write(*,*) FiboWithDo(3)
    stop
contains
    recursive function Fibo(n) result(y)
        integer, intent(in) :: n
        integer :: y
        if (n==0) then
            y = 0
        else if (n==1) then
            y = 1
        else if (n > 1) then
            y = Fibo(n-2) + Fibo(n-1)
        end if
    end function Fibo
    recursive function FiboWithDo(n) result(y)
        integer, intent(in) :: n
        integer :: i
        integer :: F0, F1
        integer :: y
        F0 = 0
        F1 = 1
        do i=2,n
            if (n==0) then
                y = 0
                exit
            else if (n==1) then
                y = 1
                exit
            else
                y = F0 + F1
                F0 = F1
                F1 = y
            end if
        end do
    end function FiboWithDo
end program main
