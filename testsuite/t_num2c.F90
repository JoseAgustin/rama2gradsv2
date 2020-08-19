!test function num2char
program t_num2char
use vp_ramatograds
implicit none
integer ::i
character(len=2)::cmes
	do i=1,12
	write(cmes,'(I2.2)') i
	write(6,'(1(A3,x))') (num2char(cmes))
	end do
end program
