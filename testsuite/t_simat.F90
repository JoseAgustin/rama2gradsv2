!  Test function hourinyr
program t_simat
use vp_ramatograds
implicit none
integer :: i
character(len=32) :: arg
    do i = 1, iargc()
       call getarg(i, arg)
       if(arg .eq."--version") print *,"Version 3.0"
    end do
    hrs_yr=17
    call lee_estaciones_rama
    call lee_simat_data("met_data.csv")
    write(6,'(7(F7.1,x))')(rama(1,10,i),i=1,nvars)
    write(6,'(7(F7.1,x))')(rama(1,44,i),i=1,nvars)

    call lee_simat_data("pol_data.csv")
    write(6,'(7(F7.1,x))')(rama(1,10,i),i=1,nvars)
    write(6,'(7(F7.1,x))')(rama(1,44,i),i=1,nvars)

end program t_simat
