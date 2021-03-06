! test function estacion
program t_est
use vp_ramatograds
implicit none
integer :: i
integer,parameter :: inst=65
character(len=3),dimension(inst):: eid
 CHARACTER(len=32) :: arg
eid=['ACO', 'AJM', 'AJU', 'ATI', 'BJU', 'CAM', 'CCA', 'CHO',&
  'COR', 'CUA', 'CUT', 'DIC', 'EAJ', 'EDL', 'FAC', 'FAR', 'GAM',&
  'HGM', 'IBM', 'INN', 'IZT', 'LAA', 'LLA', 'LOM', 'LPR', 'MCM',&
  'MER', 'MGH', 'MON', 'MPA', 'NEZ', 'PED', 'SAC', 'SAG', 'SFE',&
  'SNT', 'TAH', 'TEC', 'TLA', 'TLI', 'UAX', 'UIZ', 'VIF', 'XAL',&
  'ARA', 'AZC', 'CES', 'COY', 'CUI', 'HAN', 'IMP', 'LAG', 'LVI',&
  'MIN', 'PAR', 'PER', 'PLA', 'SHA', 'SJA', 'SUR', 'TAC', 'TAX',&
  'TEC', 'TPN', 'VAL']
    call logs("Testing function estacion    ")
    DO i = 1, iargc()
       CALL getarg(i, arg)
       if(arg .eq."--version") print *,"Version 3.0"
    END DO
    call lee_estaciones_rama
    do i=1,inst
        write(6,'((A3,x,I2,x))',advance='no') eid(i), estacion(eid(i))
        if(mod(i,5)== 0 ) write(6,*)'  <'
    end do
    call logs("End Testing         ")
end program t_est
