!test function num2char
program t_num2char
use vp_ramatograds
implicit none
integer ::i
character(len=32)::arg
    do i = 1, iargc()
       call getarg(i, arg)
       if(arg .eq."--version") print *,"Version 3.0"
    end do
	do i=1,12
	write(6,'(A3,x)',advance='no') num2char(i)
	end do
end program
