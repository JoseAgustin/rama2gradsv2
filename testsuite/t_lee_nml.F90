!  Test namelist
program t_lee_nml
use vp_ramatograds
	call logs("Testing subroutine lee_nml")
	call lee_nml("namelist.nml")
    print *,anio," ",  imes," ",idia," ",ihr
    print *, "    ",fmes," ", fdia," ", fhr
    print *,met_file,pol_file
end program t_lee_nml
