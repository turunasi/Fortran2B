program main
    use mymodblas
    implicit none
    real(8),allocatable :: A(:,:), b(:,:), x(:,:)
    call rand(A,3,3)
    call rand(b,3,1)

    call disp(A)
    call disp(B)

    call mydlss(A,b,x)
    call disp(x)
    stop
end program main
