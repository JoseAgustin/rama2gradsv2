# RAMA2GrADS

## Introduction
Air quality and meteorological observation data  is commnly measured and available, one way to study and analize it is by visualization it  in a map. In the case of Mexico City the Integral Air Quality Monitorig System (SIMAT) collects data since 1986 up to now, the data is in an ascii comma separated values (CSV) format and the location of the stations is also available. The Grid Analisis and Display System (GrADS) is an interactive desktop tool capable to display station data and model result.  in order to use the larga data set of measurements it is necesary to convert the station database in format useful for the GrADS. This convertion from SIMAT data base to in made by this system.


## Code Description

The conversion system  uses the station locations and data from different files, after maching the location with the measured vairalbe it is  written to a binary file following the format requiered for station GrADS format.

The RAMA2GrADS system contains functions and subroutines to acomplish this task. The subroutine __lee_simat_dat__  loads the measured data, int this case meteorological data and pollutnat data are in different files . __lee_estaciones_rama__ subroutine  reads the stations locations. In the __output__ subroutine it is located the code to write surface station data.

## Usage

The RAMA2GrADS system requieres a configuration file for setting the time period and names for the meteorological and pollutant files. This is obtained by setting the variables in the __namelis.nml__ file

     &FECHA
     anio=2011
     imes=01
     fmes=12
     idia=01
     fdia=31
     met_file="meteorologia_2011.csv"
     pol_file="contaminantes_2011.csv"
     /

- __anio__  year of the data.
- __imes__  starting moth for storing the data.
- __fmes__  end month.
- __idia__  starting day for storing the data
- __fdia__  ending day 
- __met_file__ meteorolgical data with SIMAT format  
- __pol_file__ contains the pollutant measured data with SIMAT format 

The files met and pol contain 11 header lines with the following format:

    01/01/2011 01:00,ACO,RH,,6
    01/01/2011 01:00,MON,RH,38,6
    01/01/2011 01:00,CHO,RH,,6)

the est_rama.txt stations file has one header with the following format:

    Alias   Latitud    Longitud  Altitud  Estacion description
    ACO    19.635501  -98.912003  2198    Acolman
    AJM    19.2721    -99.207658  2619    Ajusco Medio
    HGM   19.411617   -99.152207  2234    Hospital General de M<8E>xico

### output
Thhe code generated two files one with the binary data file and the descriptor file (ctl)  

![TMP stations](/assets/images/ftemp.png "Temperature max 10 days  stations")

![O3 stations](/assets/images/fozone.png "Ozone max 10 days stations")


## References

Grid Analysis and Display System (Grads) [Creating a Station Data File]  (http://cola.gmu.edu/grads/gadoc/gadocindex.html)

Mexico Air quality Network [SIMAT] (http://www.aire.cdmx.gob.mx/default.php)
