! Test cuenta
program t_cuenta
  use vp_ramatograds
  integer :: i
  character(len=32):: arg
    do i = 1, iargc()
       call getarg(i, arg)
       if(arg .eq."--version") print *,"Version 3.0"
    end do
    call logs("testintg function cuenta")
    open(Unit=10,file="est_rama.txt",status="OLD",action="READ")
    print *,cuenta(10)
end program t_cuenta
