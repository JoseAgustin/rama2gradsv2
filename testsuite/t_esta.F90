! test function estacion
program t_est
use vp_ramatograds
implicit none
integer :: i
integer,parameter :: inst=65
character(len=3),dimension(inst):: eid
eid=['ACO', 'AJM', 'AJU', 'ATI', 'BJU', 'CAM', 'CCA', 'CHO',&
  'COR', 'CUA', 'CUT', 'DIC', 'EAJ', 'EDL', 'FAC', 'FAR', 'GAM',&
  'HGM', 'IBM', 'INN', 'IZT', 'LAA', 'LLA', 'LOM', 'LPR', 'MCM',&
  'MER', 'MGH', 'MON', 'MPA', 'NEZ', 'PED', 'SAC', 'SAG', 'SFE',&
  'SNT', 'TAH', 'TEC', 'TLA', 'TLI', 'UAX', 'UIZ', 'VIF', 'XAL',&
  'ARA', 'AZC', 'CES', 'COY', 'CUI', 'HAN', 'IMP', 'LAG', 'LVI',&
  'MIN', 'PAR', 'PER', 'PLA', 'SHA', 'SJA', 'SUR', 'TAC', 'TAX',&
  'TEC', 'TPN', 'VAL']
    call logs("Testing funtion estacion    ")
    call lee_estaciones_rama
    do i=1,inst
    write(6,'(6(x,A3,x,I2,x))')&
     eid(i), estacion(eid(i))
    end do
    call logs("End Testing         ")
end program t_est
