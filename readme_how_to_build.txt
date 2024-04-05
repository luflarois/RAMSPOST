This file contains instructions for compiling RAMSPOST version 6.0.
The goal is pos-processing BRAMS 5.2 output files (analysis) for visualization with GrADS software.

Follow the steps:
1)making the executable "ramspost_60"
  a) set your fortran and C compilers using the file named "include_ramspost.mk"
  b) execute the command:  make
  
The file "ramspost.inp" is a namelist read by the executable "ramspost_50" which will create 
GrADS files with the variables defined in the namelist.
For a complete list of the variables avalaible, look at the file 

"README_var_library_for_BRAMS5.2"
