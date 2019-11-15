program main
    use mymodblas
    implicit none
    real(8),allocatable :: A(:,:), b(:,:), x(:,:)
    allocate(A(3,3))
    allocate(B(3,1))

    a(1,1)=2; a(1,2)=4; a(1,3)=8
    a(2,1)=3; a(2,2)=5; a(2,3)=7
    a(3,1)=4; a(3,2)=6; a(3,3)=8
    b(1,1)=6; b(2,1)=8; b(3,1)=10

    call disp(A)
    call disp(B)

    call mydlss(A,b,x)
    call disp(x)
    stop
end program main
