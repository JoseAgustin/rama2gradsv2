!****************************************************************************
!
!  Module: rama2gradsv2
!
!  Version 1            13 September 2004
!          2            16 June      2016
!          2.1          25 May       2020
!          3.0          16 August    2020
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
    integer n_ramau,hpy,nvars
    integer :: n_rama=65
    parameter (rnulo=-99.)
    parameter (hpy=24*366) !
    parameter(nvars=14)
    real,allocatable :: lon(:)
    real,allocatable :: lat(:)
    real,allocatable :: msn(:)
    real,dimension(hpy,65,nvars):: rama
    character(len=3),allocatable,dimension(:) :: id_name
!> year from input data
    character(len=4):: anio ;!> start day for output
    character(len=2):: idia ;!> start month for output
    character(len=2):: imes;!> end day for output
    character(len=2):: fdia ;!> end month for output
    character(len=2):: fmes ;!> start hour for output
    character(len=2):: ihr ;!> end hour for output
    character(len=2):: fhr; !> SIMAT meteorological data file
    character(len=23):: met_file; !> SIMAT pollution data file
    character(len=23):: pol_file;!>  used sataions from est_rama.txt
    logical,allocatable,dimension(:) :: est_util

    NAMELIST /FECHA/ anio,ihr, idia, imes,fhr, fdia, fmes,met_file,pol_file
    common /STATIONS/ n_ramau

contains
!>  @brief read namelist input file for selecting specific days
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/02/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
subroutine lee_nml(fnml)
    character(len=*),intent(IN)::fnml
    integer ::unit_nml
    logical :: existe
    existe = .FALSE.
    call logs('Start reading file - '//fnml)
    inquire ( FILE = fnml , EXIST = existe )
        if ( existe ) then
        !  Opening the file.
            open ( FILE   = fnml ,      &
            UNIT   =  unit_nml        ,      &
            STATUS = 'OLD'            ,      &
            FORM   = 'FORMATTED'      ,      &
            ACTION = 'READ'           ,      &
            ACCESS = 'SEQUENTIAL'     )
            !  Reading the file
            READ (unit_nml , NML = FECHA )
            else
            stop '***** No namelist file'
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
character(len=14) :: out_file,out_filctl
!
!     Writing RAMA data bases
!
    out_file='simat_'//anio//'.dat'
    out_filctl='simat'//anio//'.ctl'
    call logs('Storing data in dat file '//out_file)
    open(unit=10,file=out_file,FORM='UNFORMATTED', RECORDTYPE='STREAM',&
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
    call logs ('Writing '//out_filctl)
    open (unit=20,file=out_filctl)
	  write(20,'(A)')"dset ^"//out_file
	  write(20,'(A)')"dtype station"
	  write(20,'(A)')"options big_endian"
	  write(20,'(A)')"stnmap ^simat"//anio//".map "
	  write(20,'(A6,F6.2)')"undef ",rnulo
	  write(20,'(A)')"title Met y Cons SIMAT ppb "//anio
	  write(20,'(A5,I8,A27)')"tdef ",hpy," linear 7z01JAN"//anio//" 1hr"
	  write(20,'(A5,I3)')"vars ",nvars
	  write(20,'(A)')"t   0 99 Temperatura C  "
	  write(20,'(A)')"u   0 99 Viento en x m/s"
	  write(20,'(A)')"v   0 99 Viento en y m/s"
	  write(20,'(A)')"rh  0 99 Humedad relativ"
      write(20,'(A)')"pb  0 99 Press Bar  Pa"
      write(20,'(A)')"o3  0 99 ozono  conc ppb"
      write(20,'(A)')"co  0 99 CO  conc ppm   "
      write(20,'(A)')"so2 0 99 SO2 conc ppb  "
	  write(20,'(A)')"nox 0 99 NOx conc ppb  "
	  write(20,'(A)')"no  0 99 NO  conc ppb "
      write(20,'(A)')"no2 0 99 NO2 conc ppb "
	  write(20,'(A)')"pm10 0 99  PM10  ug/m3 "
      write(20,'(A)')"pm25 0 99  PM2.5 ug/m3 "
      write(20,'(A)')"pmco 0 99  PMCO ug/m3 "
      write(20,'(A)')"endvars"
end subroutine output
!> @brief Reads SIMAT database filesand stores values in matrix rama
!> @author Agustin Garcia
!> @date 16/08/2020.
!> @version  3.0
!> @param file_read file to read
subroutine lee_simat_data(file_read)
implicit none
integer :: i,ifile
integer :: ist
integer :: ivar
integer :: imet
integer :: ifecha
logical salir
!> conversion de mmHg a Pa
real,parameter :: mmHg2Pa =101325./760.
real :: rval
character(len=*),intent(in):: file_read
character(len=32) :: cdum
character(len=10) ::fecha
character(len=5)  :: hora
character(len=3)  :: c_id,cvar

open (newunit=ifile,file=file_read,status='old',action='read')
!reading headings
    do i=1,11
        read(ifile,*) cdum
    end do
    call logs("Lee archivo "//file_read)
    salir=.true.
    do while(salir)
        rval=rnulo
        read(ifile,*,END=200)fecha,hora,c_id,cvar,rval !meteorologia
        ifecha= hourinyr(fecha,hora)
        ist = estacion(c_id)
        ivar = vconvert(cvar)
        
        if(rval.ne.rnulo.and.ivar.eq.5 ) rval=rval*mmHg2Pa
        if(rval.eq.0 .and. ivar.eq.2) rval=rnulo
        if(rval.eq.0 .and. ivar.eq.3.and.rama(ifecha,ist,2).eq.rnulo) rval=rnulo
        !print *,ifecha,ist,ivar,rval
        rama(ifecha,ist,ivar)=rval
    !     if(ifecha.eq.hpy) salir=.false.
    end do  !salir

200 close(ifile)
n_ramau=0
 do i=1,n_rama
   if(est_util(i)) n_ramau=n_ramau+1
  end do
  write(cdum,'(A27,x,I2)')"Numero de estaciones utiles",n_ramau
call logs(cdum)
end subroutine lee_simat_data
!> @author Agustin Garcia
!> @date 28/08/2012.
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico.
!> @brief     Reads est_rama.txt file containing localization stations
!  _                        _             _
! | | ___  ___     ___  ___| |_ __ _  ___(_) ___  _ __   ___  ___
! | |/ _ \/ _ \   / _ \/ __| __/ _` |/ __| |/ _ \| '_ \ / _ \/ __|
! | |  __/  __/  |  __/\__ \ || (_| | (__| | (_) | | | |  __/\__ \
! |_|\___|\___|___\___||___/\__\__,_|\___|_|\___/|_| |_|\___||___/
!           |_____|
!  _ __ __ _ _ __ ___   __ _
! | '__/ _` | '_ ` _ \ / _` |
! | | | (_| | | | | | | (_| |
! |_|  \__,_|_| |_| |_|\__,_|
    subroutine lee_estaciones_rama
    implicit none
    integer :: i,j,iunit
    character(len=13) :: fname, cdum
    fname='est_rama.txt'
        call logs(" Lee archivo "//fname//"  ")
        open (newunit=iunit,file=fname,status='OLD',action='read')
        n_rama=cuenta(iunit)-1
        allocate(lat(n_rama),lon(n_rama),id_name(n_rama))
        allocate(msn(n_rama),est_util(n_rama))
        read (iunit,'(A)')cdum
        do i=1,n_rama
            read (iunit,*) id_name(i),lat(i),lon(i),msn(i)
            !print *,id_name(i),lat(i),lon(i),msn(i)
        end do
    est_util=.false.
    close(iunit)
end subroutine lee_estaciones_rama
!> @brief     Identify the statios in the data set
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
integer function estacion(cvar)
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
!> @brief Set ID number to the variable name
!> @author Agustin Garcia
!> @date 28/08/2012.
!>   @version  3.0
!> @param cvar name of the variable to convert
integer function vconvert(cvar)
!                                    _
!__   _____ ___  _ ____   _____ _ __| |_
!\ \ / / __/ _ \| '_ \ \ / / _ \ '__| __|
! \ V / (_| (_) | | | \ V /  __/ |  | |_
!  \_/ \___\___/|_| |_|\_/ \___|_|   \__|
! Identifies the variables id
    character (len=3),intent(in) ::cvar
    character(len=3),dimension(nvars):: parametro
    parametro=["TMP","WSP","WDR","RH","PBa","O3","CO",&
               "SO2","NOX","NO","NO2","PM1","PM2","PMC"]
    do i=1,size(parametro)
        if(trim(cvar).eq.parametro(i)) then
            vconvert=i
            return
        end if
    end do
    vconvert=-99
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
!> @param date DD--MM-YYYY date format
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
!                       _
!   ___ _   _  ___ _ __ | |_ __ _
!  / __| | | |/ _ \ '_ \| __/ _` |
! | (__| |_| |  __/ | | | || (_| |
!  \___|\__,_|\___|_| |_|\__\__,_|
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
!  _
! | | ___   __ _ ___
! | |/ _ \ / _` / __|
! | | (_) | (_| \__ \
! |_|\___/ \__, |___/
!          |___/
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
