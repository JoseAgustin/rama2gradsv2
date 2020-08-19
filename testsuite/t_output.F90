!  test output subroutine
program tet_ouput
use vp_ramatograds
implicit none
    call lee_nml("namelist.nml")
    call lee_estaciones_rama
    call lee_simat_data(met_file)
    call lee_simat_data(pol_file)
    call output
end program tet_ouput
