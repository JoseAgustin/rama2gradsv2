!  Test namelist
program t_lee_nml
use vp_ramatograds
integer :: i
character(len=32):: arg
    do i = 1, iargc()
       call getarg(i, arg)
       if(arg .eq."--version") print *,"Version 3.0"
    end do
	call logs("Testing subroutine lee_nml")
	call lee_nml("namelist.test")
    print *,anio," ",  imes," ",idia
    print *, "    ",fmes," ", fdia
    print *,met_file,pol_file,hrs_yr
    print *,hr_ini,hr_end
end program t_lee_nml
