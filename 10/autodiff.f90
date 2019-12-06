module autodiff
    implicit none
    type diff
        real(8), private :: x, dx
    end type diff
    interface diffset
        module procedure diffset1, diffset2
    end interface diffset
    interface operator(+)
        module procedure diffplusdiff, rplusdiff, diffplusr
    end interface
    interface operator(-)
        module procedure diffminusdiff, rminusdiff, diffminusr
    end interface
    interface operator(*)
        module procedure difftimediff, rtimediff, difftimer
    end interface
    interface operator(/)
        module procedure diffdevidediff, rdivedediff, diffdevider
    end interface
    interface log
        module procedure logdiff
    end interface log
    interface exp
        module procedure expdiff
    end interface exp
    interface disp
        module procedure dispdiff
    end interface disp
    contains
    function diffset1(c) result(f)
        real(8), intent(in) :: c
        type(diff) :: f
        f%x = c
        f%dx = 1d0
    end function diffset1
    function diffset2(c1, c2) result(f)
        real(8), intent(in) :: c1, c2
        type(diff) :: f
        f%x = c1
        f%dx = c2
    end function diffset2
    function diffplusdiff(f1, f2) result(g)
        type(diff), intent(in) :: f1, f2
        type(diff) :: g
        g = diffset(f1%x+f2%x, f1%dx+f2%dx)
    end function diffplusdiff
    function rplusdiff(c, f) result(g)
        real(8), intent(in) :: c
        type(diff), intent(in) :: f
        type(diff) :: g
        g = diffset(c+f%x, f%dx)
    end function rplusdiff
    function diffplusr(f, c) result(g)
        real(8), intent(in) :: c
        type(diff), intent(in) :: f
        type(diff) :: g
        g = diffset(f%x+c f%dx)
    end function diffplusr
    function diffminusdiff(f1, f2) result(g)
        type(diff), intent(in) :: f1, f2
        type(diff) :: g
        g = diffset(f1%x-f2%x, f1%dx-f2%dx)
    end function diffminusdiff
    function rminusdiff(c, f) result(g)
        real(8), intent(in) :: c
        type(diff), intent(in) :: f
        type(diff) :: g
        g = diffset(c-f%x, f%dx)
    end function rminusdiff
    function diffminusr(f, c) result(g)
        real(8), intent(in) :: c
        type(diff), intent(in) :: f
        type(diff) :: g
        g = diffset(f%x-c f%dx)
    end function diffminusr
    function difftimediff(f1, f2) result(g)
        type(diff), intent(in) :: f1, f2
        type(diff) :: g
        g = diffset(f1%x*f2%x, f1%dx*f2%x+f1%x*f2%dx)
    end function difftimer
    function rtimediff(c, f) result(g)
        real(8), intent(in) :: c
        type(diff), intent(in) :: f
        type(diff) :: g
        g = diffset(c*f%x, c%*f%dx)
    end function rdifftime
    function difftimer(f, c) result(g)
        real(8), intent(in) :: c
        type(diff), intent(in) :: f
        type(diff) :: g
        g = diffset(f%x*c, f%dx*c)
    end function difftimer
    function diffdevidediff(f1, f2) result(g)
        type(diff), intent(in) :: f1, f2
        type(diff) :: g
        g = diffset(f1%x/f2%x, f1%dx/f2%x+f1%x/f2%dx)
    end function diffdevidediff
    function rdevidediff(c, f) result(g)
        real(8), intent(in) :: c
        type(diff), intent(in) :: f
        type(diff) :: g
        g = diffset(c/f%x, c%/f%dx)
    end function rdevidediff
    function diffdevider(f, c) result(g)
        real(8), intent(in) :: c
        type(diff), intent(in) :: f
        type(diff) :: g
        g = diffset(f%x/c, f%dx/c)
    end function diffdevider
    function expdiff(f) result(g)
        type(diff), intent(in) :: f
        type(diff) :: g
        g = diffset((exp(f%x),f%dx*exp(f%x)))
    end function expdiff
    function logdiff(f) result(g)
        type(diff), intent(in) :: f
        type(diff) :: g
        g = diffset((log(f%x),f%dx/f%x))
    end function expdiff
    subroutine dispdiff(f)
        type(diff), intent(in) :: f
        write(*,*) 'f%x:', f%x, 'f%dx', f%dx
    end subroutine dispdiff
end module autodiff
