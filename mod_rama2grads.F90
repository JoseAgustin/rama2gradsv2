!****************************************************************************
!
!  Module: rama2gradsv2
!
!  Version 1            13 September 2004
!          2            16 June      2016
!          2.1          25 May       2020
!          3.0          21 August    2020
!****************************************************************************
!  ifort -O2 -axAVX -o rama2gradsv2.exe mod_rama2grads.F90 rama2grads.F90
!>   @brief Variables used for the conversion from ascii to GrADS station data file
!>   @author  Dr. Agustin Garcia Reynoso
!>   @date  2020,2016,2004
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico.
module vp_ramatograds
    !> number used for represent null value
    real,parameter :: rnulo=-99.
!>SIMAT/RAMA variables (TMP,WSP,WMD,RH,PBa, O3,SO2,NOx,NO2,NO,CO,PM10,PM25,PMCO)
    integer,parameter::nvars=14 ;!> Number of stations in output file
    integer:: n_ramau
    !> n_rama Number of stations in localization file est_rama.txt
    integer :: n_rama=65 ;!> Total hour in year
    integer :: hrs_yr ;!> Initial hour in year for the storing data
    integer :: hr_ini ;!> End hour in year for the storing data
    integer :: hr_end
    !>longitud localization of SIMAT station
    real,allocatable :: lon(:) ; !> latitude localization of SIMAT station
    real,allocatable :: lat(:) ;!>  Altitude of station
    real,allocatable :: msn(:) ;!>Array with all data for all the time period and stations
    real,allocatable :: rama(:,:,:) ;!> Station identification name
    character(len=3),allocatable,dimension(:) :: id_name
    !> year from input data
    integer :: anio ;!> start day for output
    integer :: idia ;!> start month for output
    integer :: imes;!> end day for output
    integer :: fdia ;!> end month for output
    integer :: fmes ;!> SIMAT meteorological data file
    character(len=23):: met_file; !> SIMAT pollution data file
    character(len=23):: pol_file;!>  used stations from est_rama.txt
    logical,allocatable,dimension(:) :: est_util

    NAMELIST /FECHA/ anio, idia, imes, fdia, fmes,met_file,pol_file
    common /STATIONS/ n_rama,n_ramau

contains
!>  @brief read namelist input file for selecting specific days
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/02/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
!>  @param fnml namelist file name
subroutine lee_nml(fnml)
    character(len=*),intent(IN)::fnml
    integer ::unit_nml=10
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
            close(unit_nml)
            else
            stop '***** No namelist file'
        ENDIF
    hrs_yr=hourinyr(31,12,anio,"23:00")
    hr_ini=hourinyr(idia,imes,anio,"01:00")
    hr_end=hourinyr(fdia,fmes,anio,"24:00")
end subroutine lee_nml
!> @brief     Creates binary file (simat_2011.dat) and descripting file (simat2011.ctl) for <a href="http://cola.gmu.edu/grads/">GrADS</a>
!> @author Agustin Garcia
!> @date 28/08/2012.
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
  subroutine output
!              _               _
!   ___  _   _| |_ _ __  _   _| |_
!  / _ \| | | | __| '_ \| | | | __|
! | (_) | |_| | |_| |_) | |_| | |_
!  \___/ \__,_|\__| .__/ \__,_|\__|
!                 |_|
!
implicit none
!> Number of data groups following the header.
integer :: NLEV  = 1;!>If set to 1, then there are surface variables following the header.
integer :: NFLAG = 1
integer :: i,j,k,icont
real,parameter :: deg2rad= 4*ATAN(1.0)/180.0
!>The time of this report, in grid-relative units. Typically have the range of - 0.5 to 0.5
real :: tim;!> value of the parameter to store
real :: val
character(len=4) cyear
character(len=8),allocatable::stid(:)
character(len=8) inicia
character(len=14) :: out_file,out_filctl
allocate(stid(n_rama))
!
!     Writing RAMA data bases
!
    write(cyear,'(I4)')anio

    out_file='simat_'//cyear//'.dat'
    out_filctl='simat_'//cyear//'.ctl'
    call logs('Storing data in dat file '//out_file)
    open(unit=10,file=out_file,FORM='UNFORMATTED',convert="big_endian" &
    ,ACCESS="STREAM", carriagecontrol='NONE')
    tim = 0.0
    do i= hr_ini, hr_end   !Numero horas año
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
    write(inicia,'("07z",I2.2,A3)') idia,num2char(imes)
    open (unit=20,file=out_filctl,FORM='FORMATTED')
    write(20,'(20A)')"DSET ^"//out_file
    write(20,'(13A)')"DTYPE station"
    write(20,'(20A)')"STNMAP ^simat"//cyear//".map "
    write(20,'(18A)')"OPTIONS big_endian"
    write(20,'(A6,F6.2)')"UNDEF ",rnulo
    write(20,'(51A)')"TITLE Meteorological and pollutants from SIMAT "//cyear
    write(20,'(A5,I6,A24)')&
    "TDEF ",hr_end-hr_ini+1," linear "//inicia//cyear//" 1hr"
    write(20,'(A5,I3)')"VARS ",nvars
    write(20,'(A)')"t    0 99 Temperature C"
    write(20,'(A)')"u    0 99 eastward wind component m/s"
    write(20,'(A)')"v    0 99 orthward wind component. m/s"
    write(20,'(A)')"rh   0 99 Relative humidity %"
    write(20,'(A)')"pb   0 99 Pressure Bar  Pa"
    write(20,'(A)')"o3   0 99 ozone conc ppb"
    write(20,'(A)')"co   0 99 CO  conc ppm "
    write(20,'(A)')"so2  0 99 SO2 conc ppb "
    write(20,'(A)')"nox  0 99 NOx conc ppb "
    write(20,'(A)')"no   0 99 NO  conc ppb "
    write(20,'(A)')"no2  0 99 NO2 conc ppb "
    write(20,'(A)')"pm10 0 99 PM10  ug/m3  "
    write(20,'(A)')"pm25 0 99 PM2.5 ug/m3  "
    write(20,'(A)')"pmco 0 99 PM Organic Carbon ug/m3"
    write(20,'(A)')"endvars"
    close(20)
    if(allocated(stid)) deallocate(stid)
end subroutine output
!> @brief Find if an array has been allocated and release the memory
!> @author Agustin Garcia
!> @date 23/08/2020.
!> @version  3.0
!> @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine libera_memoria
call logs("Libera Memoria      ")
if(allocated(rama)) deallocate(rama)
if(allocated(lat))  deallocate(lat)
if(allocated(lon))  deallocate(lon)
if(allocated(msn))  deallocate(msn)
if(allocated(est_util)) deallocate(est_util)
if(allocated(id_name))  deallocate(id_name)
if(allocated(id_name))  deallocate(id_name)
end subroutine libera_memoria
!> @brief Reads SIMAT database files and stores values in matrix rama
!> @author Agustin Garcia
!> @date 16/08/2020.
!> @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!> @param file_read datafile from SIMAT to be read
!  _                   _                 _          _       _
! | | ___  ___     ___(_)_ __ ___   __ _| |_     __| | __ _| |_ __ _
! | |/ _ \/ _ \   / __| | '_ ` _ \ / _` | __|   / _` |/ _` | __/ _` |
! | |  __/  __/   \__ \ | | | | | | (_| | |_   | (_| | (_| | || (_| |
! |_|\___|\___|___|___/_|_| |_| |_|\__,_|\__|___\__,_|\__,_|\__\__,_|
!            |_____|                       |_____|
subroutine lee_simat_data(file_read)
implicit none
integer :: i,ifile
integer :: ist
integer :: ivar
integer :: imet
integer :: ifecha
integer :: diai
integer :: mesi
integer :: anioi
integer :: io
logical salir
!> conversion de mmHg a Pa
real,parameter :: mmHg2Pa =101325./760.
real :: rval
character(len=*),intent(in):: file_read
character(len=32) :: cdum
character(len=10) ::fecha
character(len=5)  :: hora
character(len=3)  :: c_id,cvar
character(len=1)  :: sep
    if (.not. allocated(rama)) then
        allocate(rama(hrs_yr,n_rama,nvars))
        rama=rnulo
    end if
    open (newunit=ifile,file=file_read,status='old',action='read')
!reading headings
    do i=1,11
        read(ifile,*) cdum
    end do
    call logs("Lee archivo "//file_read)
    salir=.true.
    do while(salir)
        rval=rnulo
        read(ifile,133,advance='no',IOSTAT=io)diai,sep,mesi,sep,anioi,hora,c_id
        if (0>io) exit
        if (io>0) stop 'Problem reading file'
        read(ifile,*,IOSTAT=io) cvar,rval
        if (0>io) exit
        if (io>0) stop 'Problem reading file 2'
        ifecha= hourinyr(diai,mesi,anioi,hora)
        ist = estacion(c_id)
        ivar = vconvert(cvar)
        if(rval.ne.rnulo.and.ivar.eq.5 ) rval=rval*mmHg2Pa
        if(rval.eq.0 .and. ivar.eq.2) rval=rnulo
        if(rval.eq.0 .and. ivar.eq.3.and.rama(ifecha,ist,2).eq.rnulo) rval=rnulo
        rama(ifecha,ist,ivar)=rval
    end do  !salir

200 continue
    close(ifile)
    n_ramau=0
    do i=1,n_rama
        if(est_util(i)) n_ramau=n_ramau+1
    end do
    write(cdum,'(A27,x,I2)')"Numero de estaciones utiles",n_ramau
    call logs(cdum)
133 format (I2,A,I2,A,I4,x,A5,x,A3,x,A)
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
!>   @brief Identify the statios in the data set
!>   @author Agustin Garcia
!>   @date 28/08/2012.
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
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
!>   @brief Set ID number to the variable name
!>   @author Agustin Garcia
!>   @date 28/08/2012.
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
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
    parametro=["TMP","WSP","WDR","RH ","PBa","O3 ","CO ",&
               "SO2","NOX","NO ","NO2","PM1","PM2","PMC"]

    do i=1,size(parametro)
        if(trim(cvar).eq.trim(parametro(i))) then
            vconvert=i
            return
        end if
    end do
    vconvert=-99
    return
end function
!>   @brief converts  month numbert to its name
!>   @author Agustin Garcia
!>   @date 28/08/2012.
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!> @param month number to convert
!                        ____      _
!  _ __  _   _ _ __ ___ |___ \ ___| |__   __ _ _ __
! | '_ \| | | | '_ ` _ \  __) / __| '_ \ / _` | '__|
! | | | | |_| | | | | | |/ __/ (__| | | | (_| | |
! |_| |_|\__,_|_| |_| |_|_____\___|_| |_|\__,_|_|
character(len=3) function num2char(month)
    integer,intent(IN):: month
    character(len=3),dimension(12)::cmonth
    integer :: im
    cmonth=["JAN","FEB","MAR","APR","MAY","JUN",&
            "JUL","AUG","SEP","OCT","NOV","DEC"]
    num2char=cmonth(month)
    return
end function
!>   @brief  Obtains the number of hours in a year from day, month, year and hour
!>   @author Agustin Garcia
!>   @date 28/08/2012.
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!  _                      _
! | |__   ___  _   _ _ __(_)_ __  _   _ _ __
! | '_ \ / _ \| | | | '__| | '_ \| | | | '__|
! | | | | (_) | |_| | |  | | | | | |_| | |
! |_| |_|\___/ \__,_|_|  |_|_| |_|\__, |_|
!                                 |___/
!> @param ndia day for evaluation
!> @param nmes month for evaluation
!> @param nanio year for evaluation
!> @param hora Day hour
integer function hourinyr(ndia,nmes,nanio,hora)
implicit none
character (len=5),intent(in):: hora
character (len=2) chora
integer,intent(in)::ndia,nmes,nanio
integer ::i, ih
integer,dimension(12)::month=[31,28,31,30,31,30,31,31,30,31,30,31]
    chora= hora(1:2)
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
!>   @brief count the number of rowns in a file
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
!>   @brief display log during different program stages
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  25/08/2020
!>   @version  2.3
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param texto text to be displayed
subroutine logs(texto)
    implicit none
    character(len=*),intent(in):: texto
    character(len=50):: FMT
    integer :: lef
    lef=(40-len(trim(texto)))/2
    if(lef.lt.1) lef=1
    write(FMT,"('(3x,7(''*''),',I0,'x,A,',I0,'X,7(''*''))')") lef,lef
    write(6,FMT) trim(texto)
end subroutine
end module vp_ramatograds
