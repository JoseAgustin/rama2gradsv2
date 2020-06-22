!****************************************************************************
!
!  Module: rama2gradsv2
!
!  Version 1            13 September 2004
!          2            16 June      2016
!          2.1          25 May       2020
!
!****************************************************************************
!  ifort -O2 -axAVX -o rama2gradsv2.exe rama2gradsv2.f90
!  ifort -O2 -axAVX -o rama2gradsv2.exe -qopenmp rama2gradsv2.F90
!> @brief Variables used for the conversion from ascii to bin format
!> @param n_rama Number of stations in localization file est_rama.txt
!> @param n_ramau Number of stations in output file
!> @param hpy Number of hours per year
!> @param nvars SIMAT/RAMA variables (TMP,WSP,WMD,RH,PBa,O3,SO2,NOx,NO2,NO,CO,PM10,PM2.5)
!> @param rnulo Null value if missing
!> @param lon  longitud localization for rama station
!> @param lat  latitude localization for rama station
!> @param msn Station Altitud
!> @param rama Array with all data for all the time period and stations
!> @param id_name ID of the station
!> @param est_util true if the station contains data
!> @author  Dr. Agustin Garcia Reynoso
!> @date  2020,2016,2004
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico.
module variables
    integer n_rama,n_ramau,hpy,nvars
    parameter (n_rama=62)
    parameter (rnulo=-99.)
    parameter (hpy=24*365) ! NoBisiesto       1  2   3   4   5  6  7  8    9  10 11  12   13
    parameter(nvars=13)
    real,dimension(n_rama) :: lon,lat,msn
    real,dimension(hpy,n_rama,nvars):: rama
    character(len=3),dimension(n_rama)    :: id_name
    logical,dimension(n_rama)    :: est_util

    common /STATIONS/ est_util,lon,lat,rama,n_ramau,msn,id_name

end module variables
!> @brief     Main program for convert ascii files  SIMAT/RAMA to binary file for <a href="http://cola.gmu.edu/grads/">GrADS</a>
!
!> @author  Dr. Agustin Garcia Reynoso
!> @date  2020,2016,2004
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico.
!>
program  rama2gradsv2
use variables
!                            ____                     _           ____
!  _ __ __ _ _ __ ___   __ _|___ \ __ _ _ __ __ _  __| |_____   _|___ \
! | '__/ _` | '_ ` _ \ / _` | __) / _` | '__/ _` |/ _` / __\ \ / / __) |
! | | | (_| | | | | | | (_| |/ __/ (_| | | | (_| | (_| \__ \\ V / / __/
! |_|  \__,_|_| |_| |_|\__,_|_____\__, |_|  \__,_|\__,_|___/ \_/ |_____|
!                                 |___/
    call lee

    call lee_simat

    call output

contains
!
  !> @brief     Creates binary file (simat_2011.dat) and descripting file (simat2011.ctl) for <a href="http://cola.gmu.edu/grads/">GrADS</a>
  !> @author Agustin Garcia
  !> @date 28/08/2012.
  !>   @version  2.1
  subroutine output
!              _               _
!   ___  _   _| |_ _ __  _   _| |_
!  / _ \| | | | __| '_ \| | | | __|
! | (_) | |_| | |_| |_) | |_| | |_
!  \___/ \__,_|\__| .__/ \__,_|\__|
!                 |_|
!
implicit none
integer :: IFLAG,NLEV,NFLAG
integer :: i,j,k,icont
real :: tim,val,deg2rad
character(len=8) stid(n_rama)
!
!     Writing RAMA data bases
!
 deg2rad =4*ATAN(1.0)/180.0
    write(6,*)'      Storing data in dat file simat_2011.dat'
    open(unit=10,file='simat_2011.dat',FORM='UNFORMATTED', RECORDTYPE='STREAM',&
  & carriagecontrol='none',convert="big_endian")
    NLEV =1
    NFLAG=1
    tim = 0.0
   do i= 1, hpy   !Numero horas año
    do j =1, n_rama    ! Stations number
       if(est_util(j)) then
          stid(j)= id_name(j)
          write(10) stid(j),lat(j),lon(j),tim,NLEV,NFLAG
          do icont =1,nvars ! Varible number
            val=rama(i ,j ,icont)
            if(icont.eq.2 .and.val.ne.rnulo)&
            & val=rama(i,j,2)*sin(deg2rad*(180+rama(i,j,3)))
            if(icont.eq.3 .and.val.ne.rnulo)&
            & val=rama(i,j,2)*cos(deg2rad*(180+rama(i,j,3)))
           write(10) val
          end do  ! icont
       end if   ! es util
    end do  !j rama stations
    write(10) stid(1),lat(1),lon(1),tim,0,NFLAG
end do ! i
close(10)
write(6,*)'    Writing ctl file'
open (unit=20,file='simat2011.ctl')
	  write(20,'(A)')"dset ^simat_2011.dat"
	  write(20,'(A)')"dtype station"
	  write(20,'(A)')"options big_endian"
	  write(20,'(A)')"stnmap ^simat.map "
	  write(20,'(A6,F6.2)')"undef ",rnulo
	  write(20,'(A)')"title Met y Cons SIMAT ppb 2011  "
	  write(20,'(A5,I8,A27)')"tdef ",hpy," linear 7z01jan2011 1hr"
	  write(20,'(A5,I3)')"vars ",nvars
	  write(20,'(A)')"t   0 99 Temperatura C  "
	  write(20,'(A)')"u   0 99 Viento en x m/s"
	  write(20,'(A)')"v   0 99 Viento en y m/s"
	  write(20,'(A)')"rh  0 99 Humedad relativ"
      write(20,'(A)')"pb  0 99 Press Bar  Pa"
      write(20,'(A)')"o3  0 99 ozono  conc ppb"
      write(20,'(A)')"co  0 99 CO  conc ppm   "
      write(20,'(A)')"so2 0 99 SO2  conc ppb  "
	  write(20,'(A)')"nox 0 99 NOx  conc ppb  "
	  write(20,'(A)')"no  0 99 NO    conc ppb "
      write(20,'(A)')"no2 0 99 NO2   conc ppb "
	  write(20,'(A)')"pm10 0  99  PM10  ug/m3 "
      write(20,'(A)')"pm25 0  99  PM2.5 ug/m3 "
      write(20,'(A)')"endvars"
end subroutine output
!> @brief     Reads meteorological (meteorologia_2011.csv) and
!> pollutant concentration (contaminantes_2011.csv) files stores values in matrix rama
!> @author Agustin Garcia
!> @date 28/08/2012.
!> @version  2.1
subroutine lee_simat
! _                   _                 _
!| | ___  ___     ___(_)_ __ ___   __ _| |_
!| |/ _ \/ _ \   / __| | '_ ` _ \ / _` | __|
!| |  __/  __/   \__ \ | | | | | | (_| | |_
!|_|\___|\___|___|___/_|_| |_| |_|\__,_|\__|
!           |_____|
#ifdef _OPENMP
    use omp_lib
#endif
implicit none
integer :: i,j,ist
integer :: ivar
integer :: imet,ipol
integer ::ifecha
logical salir
real :: rval
character(len=22) :: fname, fname2, cdum
character(len=10)::fecha
character(len=5) hora
character(len=3) c_id,cvar
salir=.true.
fname ='meteorologia_2011.csv'
fname2='contaminantes_2011.csv'

rama=rnulo
print *,"   Lee archivo  ",fname

open (newunit=imet,file=fname ,status='old',action='read')
open (newunit=ipol,file=fname2,status='old',action='read')
do i=1,11
    read(imet,*) cdum
    read(ipol,*) cdum
end do
!$omp parallel sections num_threads (2) private(salir)
do while (salir)
    rval=rnulo
    read(imet,*,END=200)fecha,hora,c_id,cvar,rval !meteorologia
    !print *,fecha,hora,c_id," ",cvar,rval
!    if (fecha(4:5).eq.'02') then
    ifecha= juliano(fecha,hora)
    ist = estacion(c_id)
    ivar = vconvert(cvar)
    !print *,ifecha,ist,ivar
    if(rval.ne.rnulo.and.ivar.eq.5 ) rval=rval*101325/760 ! conversion de mmHg a Pa
    if(rval.eq.0 .and. ivar.eq.2) rval=rnulo
    if(rval.eq.0 .and. ivar.eq.3.and.rama(ifecha,ist,2).eq.rnulo) rval=rnulo

    rama(ifecha,ist,ivar)=rval
    !print *,rval
!    end if ! fecha
!    if(fecha(4:5).eq.'04'.and. hora(1:2).eq.'07') salir=.false.
!     if(ifecha.eq.hpy) salir=.false.
end do  !salir
200 continue
close(imet)
!$omp section
print *,"   Lee archivo  ",fname2
salir=.true.
do while (salir)
    rval=rnulo
    read(ipol,*,END=300)fecha,hora,c_id,cvar,rval  ! contaminantes
!    if (fecha(4:5).eq.'02') then
    ifecha= juliano(fecha,hora)
    ist = estacion(c_id)
    ivar = vconvert(cvar)
    if (rval.eq.0)then
     rval=rnulo
     else
      rama(ifecha,ist,ivar)=rval
     end if
!    end if ! fecha
!    if(fecha(4:5).eq.'04'.and. hora(1:2).eq.'07') salir=.false.
!     if(ifecha.eq.hpy) salir=.false.

end do  !while salir
300 continue
close(ipol)
!$omp end parallel sections
 n_ramau=0
 do i=1,n_rama
   if(est_util(i)) n_ramau=n_ramau+1
  end do
print *,"Numero de estaciones utiles",n_ramau
end subroutine lee_simat

!> @brief     Reads est_rama.txt file containing localization stations
subroutine lee
!> @author Agustin Garcia
!> @date 28/08/2012.
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico.
!  _
! | | ___  ___
! | |/ _ \/ _ \
! | |  __/  __/
! |_|\___|\___|
    implicit none
    integer i,j
    character(len=13) :: fname, cdum
    fname='est_rama.txt'
        print *,"   Lee archivo ",fname
        open (unit=11,file=fname,status='OLD',action='read')
        read (11,'(A)')cdum
        do i=1,n_rama
            read (11,*) id_name(i),lat(i),lon(i),msn(i)
            !print *,id_name(i),lat(i),lon(i),msn(i)
        end do
    est_util=.false.
    close(11)

end subroutine lee
!> @brief     Identify the statios in the data set
integer function estacion(cvar)
!> @author Agustin Garcia
!> @date 28/08/2012.
!>   @version  2.1
!>
! Identifies the station number id
!            _             _
!   ___  ___| |_ __ _  ___(_) ___  _ __
!  / _ \/ __| __/ _` |/ __| |/ _ \| '_ \
! |  __/\__ \ || (_| | (__| | (_) | | | |
!  \___||___/\__\__,_|\___|_|\___/|_| |_|
!>  station name for identification
character (len=3),intent(in) ::cvar
integer i
do i=1,n_rama
  if(cvar.eq.id_name(i)) then
   estacion=i
   est_util(i)=.true.
   return
   end if
end do
return
end function
!> @brief     Converts the variable name into integer ID number
!> @author Agustin Garcia
!> @date 28/08/2012.
!>   @version  2.1
integer function vconvert(cvar)
!                                    _
!__   _____ ___  _ ____   _____ _ __| |_
!\ \ / / __/ _ \| '_ \ \ / / _ \ '__| __|
! \ V / (_| (_) | | | \ V /  __/ |  | |_
!  \_/ \___\___/|_| |_|\_/ \___|_|   \__|
! Identifies the variables id
!> name of the variable to convert
character (len=3),intent(in) ::cvar
    select case (cvar)
    case ("PBa")
    vconvert=5
    case ("TMP")
    vconvert=1
    case ("WSP")
    vconvert=2
    case ("WDR")
    vconvert=3
    case ("RH")
    vconvert=4
    case ("O3")
    vconvert=6
    case ("CO")
    vconvert=7
    case("SO2")
    vconvert=8
    case("NOX")
    vconvert=9
    case("NO")
    vconvert=10
    case("NO2")
    vconvert=11
    case("PM1")
    vconvert=12
    case("PM2")
    vconvert=13
    case DEFAULT
    vconvert=-99
    end select
return
end function
!> @brief  Obtains the number of hours in a year  from date and hour
!> @author Agustin Garcia
!> @date 28/08/2012.
!>   @version  2.1
integer function juliano(fecha,hora)
!   _       _ _
!  (_)_   _| (_) __ _ _ __   ___
!  | | | | | | |/ _` | '_ \ / _ \
!  | | |_| | | | (_| | | | | (_) |
! _/ |\__,_|_|_|\__,_|_| |_|\___/
!|__/
!> YYYY-MM-DD formate date
character(len=10),intent(in):: fecha
!>   Day hour
character (len=5),intent(in):: hora
character (len=2) dia,mes,chora
character (len=4) anio
integer :: ih,idia,imes,ianio
        juliano=0
        anio=fecha(7:10)
        dia =fecha(1:2)
        mes = fecha(4:5)
        chora=hora(1:2)
        READ (anio, '(I4)'), ianio
        READ (dia, '(I2)'), idia
        READ (mes, '(I2)'), imes
        READ (hora, '(I2)'), ih
        select case (imes)
        case (1)
         juliano=(idia-1)*24+ih
        case (2)
         juliano=(idia-1+31)*24+ih
        case(3)
        juliano=(idia-1+60)*24+ih
        case(4)
        juliano=(idia-1+91)*24+ih
        case (5)
        juliano=(idia-1+121)*24+ih
        case (6)
        juliano=(idia-1+152)*24+ih
        case(7)
        juliano=(idia-1+182)*24+ih
        case(8)
        juliano=(idia-1+213)*24+ih
        case (9)
        juliano=(idia-1+244)*24+ih
        case (10)
        juliano=(idia-1+274)*24+ih
        case(11)
        juliano=(idia-1+305)*24+ih
        case(12)
        juliano=(idia-1+335)*24+ih
        case DEFAULT
        end select
        !print *,imes,idia,ih, juliano,(idia-1)*24+ih
        return
end function juliano
end program
