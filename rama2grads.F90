!> @brief     Main program for convert ascii files  SIMAT/RAMA to binary file for <a href="http://cola.gmu.edu/grads/">GrADS</a>
!
!> @author  Dr. Agustin Garcia Reynoso
!> @date  2020,2016,2004
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico.
!>
!                            ____                     _
!  _ __ __ _ _ __ ___   __ _|___ \ __ _ _ __ __ _  __| |___
! | '__/ _` | '_ ` _ \ / _` | __) / _` | '__/ _` |/ _` / __\
! | | | (_| | | | | | | (_| |/ __/ (_| | | | (_| | (_| \__ \
! |_|  \__,_|_| |_| |_|\__,_|_____\__, |_|  \__,_|\__,_|___/
!                                 |___/
program  rama2grads
use vp_ramatograds
    call lee

    call lee_simat

    call output

end program
