!  Test function hourinyr
program t_hourinyr
use vp_ramatograds
implicit none
character(len=10):: date
character(len=5) :: hour
date="01-01-2011";hour="23:00"
print *,date," ",hour
	if(hourinyr(date,hour).eq.23) then
		 print *,.true.
	 else
		 print *,.false.
	 end if
	 date="01-03-2011";hour="23:00"
	 print *,date," ",hour
if(hourinyr(date,hour).eq.1439) then
	 print *,.true.
 else
	 print *,.false.
 end if
date="01-03-2016"; hour="23:00"
print *,date," ",hour
if(hourinyr(date,hour).eq.1463) then
	 print *,.true.
 else
	 print *,.false.
 end if
end program
