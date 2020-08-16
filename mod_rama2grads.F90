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
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico.
module vp_ramatograds
    integer n_rama,n_ramau,hpy,nvars
    parameter (n_rama=62)
    parameter (rnulo=-99.)
    parameter (hpy=24*366) !
    parameter(nvars=13)
    real,dimension(n_rama) :: lon,lat,msn
    real,dimension(hpy,n_rama,nvars):: rama
    character(len=3),dimension(n_rama)    :: id_name;!> year from input data
    character(len=4):: anio ;!> start day for output
    character(len=2):: idia ;!> start month for output
    character(len=2):: imes;!> end day for output
    character(len=2):: fdia ;!> end month for output
    character(len=2):: fmes ;!> start hour for output
    character(len=2):: ihr ;!> end hour for output
    character(len=2):: fhr ;!>  used sataions from est_rama.txt
    logical,dimension(n_rama)    :: est_util

    NAMELIST /FECHA/ anio,ihr, idia, imes,fhr, fdia, fmes
    common /STATIONS/ est_util,lon,lat,rama,n_ramau,msn,id_name

contains
!>  @brief read namelist input file for selecting specific days
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/02/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
subroutine lee_nml
    integer ::unit_nml
    logical :: existe
    existe = .FALSE.
    call logs('Start reading file - namelist.nml')
    inquire ( FILE = 'namelist.nml' , EXIST = existe )
        if ( existe ) then
        !  Opening the file.
            open ( FILE   = 'namelist.nml' ,      &
            UNIT   =  unit_nml        ,      &
            STATUS = 'OLD'            ,      &
            FORM   = 'FORMATTED'      ,      &
            ACTION = 'READ'           ,      &
            ACCESS = 'SEQUENTIAL'     )
            !  Reading the file
            READ (unit_nml , NML = FECHA )
            else
            stop '***** No namelist.met'
        ENDIF
end subroutine lee_nml
  !> @brief     Creates binary file (simat_2011.dat) and descripting file (simat2011.ctl) for <a href="http://cola.gmu.edu/grads/">GrADS</a>
  !> @author Agustin Garcia
  !> @date 28/08/2012.
  !>   @version  3.0
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
real,parameter :: deg2rad= 4*ATAN(1.0)/180.0
real :: tim,val
character(len=8) stid(n_rama)
!
!     Writing RAMA data bases
!
    call logs('Storing data in dat file simat_2011.dat')
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
call logs ('Writing ctl file')
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
!> @version  3.0
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
fname ='meteorologia_2011.csv'
fname2='contaminantes_2011.csv'

rama=rnulo

open (newunit=imet,file=fname ,status='old',action='read')
open (newunit=ipol,file=fname2,status='old',action='read')
do i=1,11
    read(imet,*) cdum
    read(ipol,*) cdum
end do
!$omp parallel sections num_threads (2) private(salir,ifecha,ist,ivar,rval,fecha,hora,c_id,cvar)
call logs("Lee archivo  "//fname)
salir=.true.
do while(salir)
    rval=rnulo
    read(imet,*,END=200)fecha,hora,c_id,cvar,rval !meteorologia
    ifecha= hourinyr(fecha,hora)
    ist = estacion(c_id)
    ivar = vconvert(cvar)
    !print *,ifecha,ist,ivar
    if(rval.ne.rnulo.and.ivar.eq.5 ) rval=rval*101325/760 ! conversion de mmHg a Pa
    if(rval.eq.0 .and. ivar.eq.2) rval=rnulo
    if(rval.eq.0 .and. ivar.eq.3.and.rama(ifecha,ist,2).eq.rnulo) rval=rnulo

    rama(ifecha,ist,ivar)=rval
    !print *,rval
!    if(fecha(4:5).eq.'04'.and. hora(1:2).eq.'07') salir=.false.
!     if(ifecha.eq.hpy) salir=.false.
end do  !salir
200 close(imet)
!$omp section
call logs("Lee archivo  "//fname2)
salir=.true.
do while (salir)
    rval=rnulo
    read(ipol,*,END=300)fecha,hora,c_id,cvar,rval  ! contaminantes
!    if (fecha(4:5).eq.'02') then
    ifecha= hourinyr(fecha,hora)
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
call logs("Numero de estaciones utiles")
print *,n_ramau
end subroutine lee_simat
!> @author Agustin Garcia
!> @date 28/08/2012.
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico.
!> @brief     Reads est_rama.txt file containing localization stations
subroutine lee
!  _
! | | ___  ___
! | |/ _ \/ _ \
! | |  __/  __/
! |_|\___|\___|
    implicit none
    integer i,j
    character(len=13) :: fname, cdum
    fname='est_rama.txt'
        call logs("   Lee archivo "//fname)
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
!>   @version  3.0
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
!>   @version  3.0
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
!>   @version  2.2
!  _                      _
! | |__   ___  _   _ _ __(_)_ __  _   _ _ __
! | '_ \ / _ \| | | | '__| | '_ \| | | | '__|
! | | | | (_) | |_| | |  | | | | | |_| | |
! |_| |_|\___/ \__,_|_|  |_|_| |_|\__, |_|
!                                 |___/
!> @param fecha DD--MM-YYYY date format
!> @param hora Day hour
integer function hourinyr(date,hora)
implicit none
character(len=10),intent(in):: date
character (len=5),intent(in):: hora
character (len=2) cdia,cmes,chora
character (len=4) canio
integer ::i, ih,ndia,nmes,nanio
integer,dimension(12)::month=[31,28,31,30,31,30,31,31,30,31,30,31]
        canio= date(7:10)
        cdia = date(1:2)
        cmes = date(4:5)
        chora= hora(1:2)
        READ (canio, '(I4)') nanio
        READ (cdia, '(I2)')  ndia
        READ (cmes, '(I2)')  nmes
        READ (chora,'(I2)')  ih
if (mod(nanio,4)==0.and.mod((nanio-1500),400)/=0) month(2)=29
if (nmes==1) then
  hourinyr=(ndia-1)*24+ih
  else
  hourinyr=0
  do i=1,nmes-1
    hourinyr=hourinyr+month(i)*24
  end do
  hourinyr=hourinyr+(ndia-1)*24+ih
end if
        !print *,nmes,ndia,ih, hourinyr,(ndia-1)*24+ih
        return
end function hourinyr
!>  @brief count the number of rowns in a file
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/13/2020
!>   @version  2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param  iunit file unit where the count has to be made
integer function  cuenta(iunit)
    implicit none
    integer,intent(IN) :: iunit
    integer :: io
    cuenta = 0
    DO
        READ(iunit,*,iostat=io)
        IF (io/=0) EXIT
        cuenta = cuenta + 1
    END DO
    rewind(iunit)
    return
end
!>  @brief display log during different program stages
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  08/08/2020
!>   @version  2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param texto text to be displayed
subroutine logs(texto)
    implicit none
    character(len=*),intent(in):: texto
    write(6,333) texto
333 format(3x,5("*"),x,A35,x,"******")
end subroutine
end module vp_ramatograds
