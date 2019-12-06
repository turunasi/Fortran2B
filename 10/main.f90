program main
    use autodiff
    implicit none
    type(diff) :: A,B
    integer :: C
    A = diffset1(3.0)
    B = diffset2(1.0,3.0)
    C = 5
end program main
