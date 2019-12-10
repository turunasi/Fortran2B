program main
    use autodiff
    implicit none
    type(diff) :: A,B,C
    real(8) :: D
    A = diffset(3.0d0)
    B = diffset(1.0d0,4.0d0)
    D = 5.0d0
    C = A+B
    call disp(C)
    C = B+D
    call disp(C)
    C = D+A
    call disp(C)
    C = A-B
    call disp(C)
    C = B-D
    call disp(C)
    C = D-A
    call disp(C)
    C = A*B
    call disp(C)
    C = B*D
    call disp(C)
    C = D*A
    call disp(C)
    C = A/B
    call disp(C)
    C = B/D
    call disp(C)
    C = D/A
    call disp(C)
    C = log(A)
    call disp(C)
    C = log(B)
    call disp(C)
    C = exp(A)
    call disp(C)
    C = exp(B)
    call disp(C)
end program main
