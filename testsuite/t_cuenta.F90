! Test cuenta
program t_cuenta
  use vp_ramatograds
    call logs("testintg function cuenta")
    open(Unit=10,file="est_rama.txt",status="OLD",action="READ")
    print *,cuenta(10)
end program t_cuenta
