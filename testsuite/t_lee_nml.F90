!  Test namelist
program t_lee_nml
use vp_ramatograds
	call logs("Testing subroutine lee_nml")
	call lee_nml("namelist.nml")
    print *,anio," ",  imes," ",idia
    print *, "    ",fmes," ", fdia
    print *,met_file,pol_file,hrs_yr
    print *,hr_ini,hr_end
end program t_lee_nml
