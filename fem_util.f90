!   File: fem_util.f90
!   Author: Teodor Ande Elstad
!
!   General description: Module with collection of fem-spesific functions and sub-routines 
!                        for preforming fem analysis on a system of beam elements with 6DOF.
!
!   System reqirements: none.
!
!   Notes on content: Consult rapport by author or "Dimensjonering ved hjelp av elementmetoden"
!                     by Åge Ø. Waløen for a theoretical understanding of the functions and sub-routines 
!                     in this module.


Module fem_util

  Implicit None

Contains

  Function local_k(E, I, A, L)
    Integer :: m, n
    Real, Intent(In) :: E, I, A, L
    Real :: EA_L, EI_LLL, EI_LL, EI_L, EEI_L
    Real, Dimension(6,6) :: local_k

    EA_L = (E*A)/L
    EI_LLL = (12*E*I)/(L*L*L)
    EI_LL = (6*E*I)/(L*L)
    EI_L = (4*E*I)/L
    EEI_L = (2*E*I)/L

    Do, m = 1,6
       Do, n = 1,6
          local_k(m,n) = 0.0
       End Do
    End Do

    local_k(1,1) = EA_L
    local_k(1,4) = -EA_L
    local_k(2,2) = EI_LLL
    local_k(2,3) = -EI_LL
    local_k(2,5) = -EI_LLL
    local_k(2,6) = -EI_LL
    local_k(3,2) = -EI_LL
    local_k(3,3) = EI_L
    local_k(3,5) = EI_LL
    local_k(3,6) = EEI_L
    local_k(4,1) = -EA_L
    local_k(4,4) = EA_L
    local_k(5,2) = -EI_LLL
    local_k(5,3) = EI_LL
    local_k(5,5) = EI_LLL
    local_k(5,6) = EI_LL
    local_k(6,2) = -EI_LL
    local_k(6,3) = EEI_L
    local_k(6,5) = EI_LL
    local_k(6,6) = EI_L
  End Function local_k

  Function rotated_local_k(local_k, angle)
    Integer :: m, n
    Real, Intent(In) :: angle
    Real, Intent(In), Dimension(6,6) :: local_k
    Real, Dimension(6,6) :: rotated_local_k, transformation_matrix

    Do, m = 1,6
       Do, n = 1,6
          transformation_matrix(m,n) = 0.0
       End Do
    End Do

    transformation_matrix(1,1) = cos(angle)
    transformation_matrix(1,2) = -sin(angle)
    transformation_matrix(2,1) = sin(angle)
    transformation_matrix(2,2) = cos(angle)
    transformation_matrix(3,3) = 1.0
    transformation_matrix(4,4) = cos(angle)
    transformation_matrix(4,5) = -sin(angle)
    transformation_matrix(5,4) = sin(angle)
    transformation_matrix(5,5) = cos(angle)
    transformation_matrix(6,6) = 1.0

    rotated_local_k = Matmul(transformation_matrix, Matmul(local_k, Transpose(transformation_matrix)))
  End Function rotated_local_k

  Function format_axb(global_k, global_size, r_vector)
    Integer, Intent(In) :: global_size
    Real, Intent(In), Dimension(global_size,global_size) :: global_k
    Real, Intent(In), Dimension(global_size) :: r_vector
    Real, Dimension(global_size,global_size+1) :: format_axb
    
    format_axb(1:global_size,1:global_size) = global_k
    format_axb(1:global_size,global_size+1) = r_vector
  End Function format_axb

  Function local_beam_loads(local_k, ieg_element, r_vector, global_size)
    Real, Intent(In), Dimension(6,6) :: local_k
    Real, Intent(In), Dimension(global_size) :: r_vector
    Real, Dimension(6) :: v_vector
    Real, Dimension(6) :: local_beam_loads

    Integer, Intent(In), Dimension(2) :: ieg_element
    Integer :: u, v, i, global_size

    u = (ieg_element(1)-1)*3
    v = ((ieg_element(2)-1)*3)-3

    Do, i = 1,6
       If (i<=3) Then
          v_vector(i) = r_vector(i+u)
       Else
          v_vector(i) = r_vector(i+v)
       End If
    End Do

    local_beam_loads = matmul(local_k, v_vector)
  End Function local_beam_loads

  Subroutine set_degrees_of_freedom(axb, col_size, row_size, displacement_vector)
    Integer  displacement_vector(col_size), col_size, row_size, i
    Real axb(col_size,row_size), null_row(row_size), null_col(col_size)

    Do, i = 1,row_size
       null_row(i) = 0.0
    End Do

    Do, i = 1,col_size
       null_col(i) = 0.0
    End Do

    Do, i = 1,col_size
       If (displacement_vector(i) == 0) Then
          axb(i,1:row_size) = null_row
          axb(1:col_size,i) = null_col
          axb(i,i) = 1.0
       End If
    End Do
  End Subroutine set_degrees_of_freedom

  Subroutine blank_global_k(global_k, global_size)
    Integer global_size, i, j
    Real global_k(global_size,global_size)

    Do, i = 1,global_size
       Do, j = 1,global_size
          global_k(i,j) = 0.0
       End Do
    End Do
  End Subroutine blank_global_k

  Subroutine add_one_local_k(global_k, global_size, local_K, ieg_element)
    Integer global_size, ieg_element(2), i, j, u, v
    Real local_k(6,6), global_k(global_size,global_size)

    u = (ieg_element(1)-1)*3
    v = ((ieg_element(2)-1)*3)-3

    Do, i = 1,6
       Do, j = 1,6
          If (i<=3 .And. j<=3) Then
             global_k(i+u,j+u) = global_k(i+u,j+u) + local_k(i,j)
          Else If (i<=3 .And. j>3) Then
             global_k(i+u,j+v) = global_k(i+u,j+v) + local_k(i,j)
          Else If (i>3 .And. j<=3) Then
             global_k(i+v,j+u) = global_k(i+v,j+u) + local_k(i,j)
          Else
             global_k(i+v,j+v) = global_k(i+v,j+v) + local_k(i,j)
          End If
       End Do
    End Do
  End Subroutine add_one_local_k

  
End Module fem_util
