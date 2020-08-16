! testing function  vconvert
program test4
use vp_ramatograds
implicit none
integer :: i
character(len=3),dimension(14):: variable
 variable=(/"PBa","TMP","WDR","WSP","RH ","O3 ","CO ","SO2","NOX",&
"NO ","NO2","PM1","PM2","PMC"/)

call logs("Testing function vconvert")
do i=1,14
 print *,variable(i),vconvert(variable(i))
end do
end program test4
