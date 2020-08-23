#!/bin/bash
#: Title       : test.sh
#: Date        : 19/08/2020
#: Author      : "Jose Agustin Garcia Reynoso" <agustin@atmosfera.unam.mx>
#: Version     : 1.0  19/08/2020 Pruebas de archivo de estaciones
#: Description : Realiza la secuencia de pasos para desplegar las estaciones
#              
#: Options     : None
# set variables for program grads and stmap
export STNMAP=/opt/local/bin/stnmap
export GRDS=/opt/local/bin/grads

echo Creates STNMAP
$STNMAP -i simat2011.ctl > sal_stnmap.log
 
cat << End_of_Scr > prueba.gs
#
#  Grads TEST script
'reinit'
'open simat2011.ctl'
'open dummy.ctl'
'set lat 19.15 19.75'
'set lon -99.40 -98.80'
'set gxout shaded'
'set mpdset util/estados'
'set map 15 1 7'
'd oacres(a.2,stnmax(t,t=1,t=240),10,8,6,4,2)'
'd stnmax(t,t=1,t=240)'
'draw title Max Temperature in 10 dias'
'cbarn'
'gxprint ftemp.png x768 y1024 white'
'clear'
'd oacres(a.2,stnmax(o3,t=1,t=240),10,8,6,4,2)'
'd stnmax(o3,t=1,t=240)'
'draw title Max ozone in 10 dias'
'cbarn'
'gxprint fozone.png x768 y1024 white'
'clear'


End_of_Scr
cat << End_dumm > dummy.ctl
* +-======-+
DSET %d1may11.dat
UNDEF   -.999000E+03
OPTIONS BYTESWAPPED
OPTIONS TEMPLATE
TITLE  UN-NAMED
*
XDEF   100 LINEAR -99.80 0.0121
YDEF    62 LINEAR  19.15  0.010
ZDEF     1 LINEAR 1 1
TDEF     1 LINEAR jan2011 1hr
*
VARS  1
a     0 99 var
ENDVARS

End_dumm
$GRDS -pc 'prueba.gs'

