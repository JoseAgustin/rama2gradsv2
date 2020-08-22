!  test output subroutine
program tet_ouput
use vp_ramatograds
implicit none
integer ::i
character(len=32)::arg
    do i = 1, iargc()
       call getarg(i, arg)
       if(arg .eq."--version") print *,"Version 3.0"
    end do
    call lee_nml("namelist.nml")
    call lee_estaciones_rama
    call lee_simat_data(met_file)
    call lee_simat_data(pol_file)
    call output
end program tet_ouput
