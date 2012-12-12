!   File: physical_util.f90
!   Author: Teodor Ande Elstad
!
!   General description: Module with collection of functions for calculating physical coordinates
!                        for a frame described by beam length, rotation and IEG matrix.
!                        Assumes that beam 1 starts at point 0,0.
!
!   System reqirements: none.
!
!   Notes on content: none.
!


Module physical_util

  Implicit None

Contains

  Function next_coord(length, angle, prev_coord)
    Real, Intent(In) :: length, angle
    Real, Intent(In), Dimension(2) :: prev_coord
    Real, Dimension(2) :: next_coord

    next_coord(1) = snap_zero(prev_coord(1) + length*cos(angle))
    next_coord(2) = snap_zero(prev_coord(2) + length*sin(angle))
  End Function next_coord

  Function snap_zero(n)
    Real, Intent(In) :: n
    Real :: snap_zero

    snap_zero = n
    
    If (n<0.01 .And. n>-0.01) Then
       snap_zero = 0.0
    End If
  End Function snap_zero

  Function prev_coord(coord_list, no_of_calculated, ieg_start)
    Integer, Intent(In) :: no_of_calculated, ieg_start
    Integer :: i
    Real, Intent(In), Dimension(no_of_calculated,5) :: coord_list
    Real, Dimension(2) :: prev_coord

    prev_coord(1) = 0.0
    prev_coord(2) = 0.0

    Do, i = 1,no_of_calculated
       If (ieg_start == Int(coord_list(i,5))) Then
          prev_coord = coord_list(i,3:4)
          Exit
       End If
    End Do
  End Function prev_coord

End Module physical_util
