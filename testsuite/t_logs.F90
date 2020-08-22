! testing subroutine logs
program test1
use vp_ramatograds
implicit none
integer :: i
character(len=32)::arg
    do i = 1, iargc()
       call getarg(i, arg)
       if(arg .eq."--version") print *,"Version 3.0"
    end do
	call logs("Testing subroutine logs")
end program
