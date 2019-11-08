program main
    
    use mygaussian2
    implicit none
    real :: a(3,3),b(3),x(3)

    a(1,1)=2; a(1,2)=4; a(1,3)=8
    a(2,1)=3; a(2,2)=5; a(2,3)=7
    a(3,1)=4; a(3,2)=6; a(3,3)=8

    b(1)=6; b(2)=8; b(3)=10

    call gaussian(a,b,3,x)
    write(*,*) 'x=',x(1),x(2),x(3)

    stop
end program main
