! Test cuenta
program t_cuenta
  use vp_ramatograds
    call logs("testintg function cuenta")
    call system("ln -s ../est_rama.txt")
    open(Unit=10,file="est_rama.txt",status="OLD",action="READ")
    print *,cuenta(10)
    call system("rm est_rama.txt")
end program t_cuenta
