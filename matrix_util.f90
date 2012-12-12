!   File: matrix_util.f90
!   Author: Teodor Ande Elstad
!
!   General description: Module with collection of general functions and sub-routines 
!                        for preforming matrix-related tasks that is of a general nature.
!
!   System reqirements: none.
!
!   Notes on content: The subroutine gauss_jordan preforms gauss-jordan elimination in place on a axb matrix.
!                     The print_matrix subroutine formattes a matrix for viewing in the console.


Module matrix_util

  Implicit None

Contains
  
  Subroutine gauss_jordan(matrix, col_size, row_size)
    Integer col_size, row_size, i, i2, j
    Real matrix(col_size, row_size), c

    Do, i = 1,col_size
       Call max_pivot(matrix, col_size, row_size, i)
       If (matrix(i,i) == 0) Exit !Matrix is singulare

       c = matrix(i,i)
       Do, j = 1, row_size
          matrix(i,j) = matrix(i,j)/c
       End Do

       Do, i2 = i+1,col_size
          c = matrix(i2,i) / matrix(i,i)
          Do, j = 1,row_size
             matrix(i2,j) = matrix(i2,j) - matrix(i,j) * c
          End Do
       End Do
    End Do

    Do, j = col_size,1,-1
       Do, i = j-1,1,-1
          matrix(i,row_size) = matrix(i,row_size) - matrix(i,j) * matrix(j,row_size)
          matrix(i,j) = 0.0
       End Do
    End Do
  End Subroutine gauss_jordan

  Subroutine max_pivot(matrix, col_size, row_size, piv_index)
    Integer col_size, row_size, piv_index, i, row_max
    Real matrix(col_size, row_size), val_max
    
    val_max = matrix(piv_index, piv_index)
    row_max = piv_index
    
    Do, i = piv_index, col_size
       If (Abs(matrix(i, piv_index)) > val_max) Then
          val_max = Abs(matrix(i, piv_index))
          row_max = i
       End If
    End Do
    
    If (piv_index /= row_max) Then
       Call swap_row(matrix, col_size, row_size, piv_index, row_max)
    End If
  End Subroutine max_pivot

  Subroutine swap_row(matrix, col_size, row_size, row_a, row_b)
    Integer col_size, row_size, row_a, row_b, j
    Real matrix(col_size, row_size), swap
    
    Do, j = 1,row_size
       swap = matrix(row_a,j)
       matrix(row_a,j) = matrix(row_b,j)
       matrix(row_b,j) = swap
    End Do
  End Subroutine swap_row
  
  Subroutine add_matrix(matrixA, matrixB,  col_size, row_size)
    Integer col_size, row_size, row_1, row_b, i, j
    Real matrixA(col_size, row_size), matrixB(col_size, row_size)

    Do, i = 1,col_size
       Do, j = 1,row_size
          matrixA(i,j) = matrixA(i,j) + matrixB(i,j)
       End Do
    End Do
  End Subroutine add_matrix

  Subroutine print_matrix(matrix, col_size, row_size)
    Integer col_size, row_size, i, j
    Real matrix(col_size, row_size)

    Do, i = 1,col_size
       Write(*,*) (matrix(i,j), j = 1,row_size) 
    EndDo
  End Subroutine print_matrix
  
End Module matrix_util
