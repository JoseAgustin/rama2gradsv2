!  Test namelist
program t_lee_nml
use vp_ramatograds
	call logs("Testing subroutine lee_nml")
    call system("ln -s ../namelist.nml")
	call lee_nml
    print *,anio," ",  imes," ",idia," ",ihr
    print *, "    ",fmes," ", fdia," ", fhr
    call system("rm namelist.nml")
end program t_lee_nml
