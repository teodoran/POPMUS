#!/bin/bash
gfortran -c matrix_util.f90 fem_util.f90 physical_util.f90
gfortran -o popmus_analysis matrix_util.o fem_util.o physical_util.o popmus_analysis.f90
./popmus_analysis

gcc -I/usr/X11R6/include popmus_viewer.c -lglut -lGLU -lGL -lm -o popmus_viewer
./popmus_viewer