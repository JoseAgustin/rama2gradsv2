! test subroutine lee_estaciones_rama
program t_lest
use vp_ramatograds
implicit none
integer :: i
integer,parameter :: inst=65
character(len=3),dimension(inst):: eid
    call logs("Testing subroutine lee_estaciones_rama")
    call lee_estaciones_rama
    write(6,'(12A3,x)')id_name
    call logs("End Testing         ")
end program t_lest
