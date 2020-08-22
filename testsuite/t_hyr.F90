!  Test function hourinyr
program t_hourinyr
use vp_ramatograds
implicit none
integer :: i, diai,mesi,anioi
character(len=32):: arg
character(len=5) :: hour
    do i = 1, iargc()
       call getarg(i, arg)
       if(arg .eq."--version") print *,"Version 3.0"
    end do
diai=1
mesi=1
anioi=2011
hour="23:00"
	write(6,140)diai,mesi,anioi,hour
	if(hourinyr(diai,mesi,anioi,hour).eq.23) then
		 print *,.true.
	 else
		 print *,.false.
	 end if
	 diai=1
	 mesi=3
	 anioi=2011
	 hour="23:00"
	 write(6,140)diai,mesi,anioi,hour
if(hourinyr(diai,mesi,anioi,hour).eq.1439) then
	 print *,.true.
 else
	 print *,.false.
 end if
 diai=1
mesi=3
anioi=2016
hour="23:00"
write(6,140)diai,mesi,anioi,hour
if(hourinyr(diai,mesi,anioi,hour).eq.1463) then
	 print *,.true.
 else
	 print *,.false.
 end if
140 format(I2.2,"/",I2.2,"/",I4,x,A5)
end program
