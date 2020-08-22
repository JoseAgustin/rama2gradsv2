!test function num2char
program t_num2char
use vp_ramatograds
implicit none
integer ::i
	do i=1,12
	write(6,'(1(A3,x))',advance='no') (num2char(i))
	end do
end program
