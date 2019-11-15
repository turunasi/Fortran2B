program main
    use mymodblas
    implicit none
    real(8),allocatable :: A(:,:), l(:,:)
    call rand(A,3,3)
    A = A + Transpose(A)
    call disp(A)
    call mydsymeig(A,l)
    call disp(l)
    stop
end program main
