!   File: popmus_analysis.f90
!   Author: Teodor Ande Elstad
!
!   General description: FEM analysis application for the POPMUS system. 
!                        Reads a input file form the user and calculates 
!		         beam loads and physical coordinates. This information is
!                        written to the file results.dat.
!                        Uses matrix_util, fem_util and physical_util to do this calculation
!
!   System reqirements: Input file from user.
!
!   Notes on content: The reading loop is tasked with calling functions to correctly format the input.
!                     


Program popmus_analysis

  Use matrix_util
  Use fem_util
  Use physical_util

  Integer :: no_of_beams, no_of_vertices, global_size
  Integer, Dimension(2) :: temp_ieg_element
  Integer, Allocatable :: ieg(:,:)
  Integer, Allocatable :: displacement_vector(:)

  Real, Dimension(6,6) :: temp_local_k
  Real, Dimension(8) :: temp_data
  Real, Dimension(2) :: p_coord
  Real, Dimension(2) :: n_coord
  Real, Dimension(10) :: results_format
  Real, Allocatable :: global_k(:,:)
  Real, Allocatable :: global_load_vector(:)
  Real, Allocatable :: axb(:,:)
  Real, Allocatable :: local_k_list(:,:,:)
  Real, Allocatable :: global_coord_list(:,:)
  
  Open(15, file='input.dat')
  
  Read(15,*)
  Read(15,*) no_of_beams
  Read(15,*)
  Read(15,*) no_of_vertices
  Read(15,*)

  global_size = 3*no_of_vertices

  Allocate(global_k(global_size,global_size), axb(global_size,global_size+1))
  Allocate(global_load_vector(global_size), displacement_vector(global_size))
  Allocate(ieg(no_of_beams,2), local_k_list(no_of_beams,6,6))
  Allocate(global_coord_list(no_of_beams,5))

  Call blank_global_k(global_k, global_size)
  
  ! Reading loop
  Do, i = 1,no_of_beams
     Read(15,*) temp_data
     temp_local_k = rotated_local_k(local_k(temp_data(2), temp_data(3), temp_data(4), temp_data(5)), temp_data(6))
     temp_ieg_element(1) = temp_data(7)
     temp_ieg_element(2) = temp_data(8)

     p_coord = prev_coord(global_coord_list(1:i,1:5), i, temp_ieg_element(1))
     n_coord = next_coord(temp_data(5), temp_data(6), p_coord)     
     
     global_coord_list(i,1:2) = p_coord
     global_coord_list(i,3:4) = n_coord
     global_coord_list(i,5) = temp_ieg_element(2)

     local_k_list(i,1:6,1:6) = temp_local_k
     ieg(i,1:2) = temp_ieg_element

     Call add_one_local_k(global_k, global_size, temp_local_k, temp_ieg_element)
  End Do

  Read(15,*)
  Read(15,*) global_load_vector
  Read(15,*)
  Read(15,*) displacement_vector

  Close(15)

  axb = format_axb(global_k, global_size, global_load_vector)
  Call set_degrees_of_freedom(axb, Size(axb, 1), Size(axb, 2), displacement_vector)
  Call gauss_jordan(axb, Size(axb, 1), Size(axb, 2))
  
  Open (16,file="results.dat",action="write",status="replace")
  
  Write(16,*) "Number of beams:"
  Write(16,*) no_of_beams

  Write(16,*) "Beam coordinates and local forces table:"
  Write(16,*) "|   x0   |   y0   |   x1   |   y1   |   v1   |   v2   |   v3   |   v4   |   v5   |   v6   |"
  Do, i = 1, no_of_beams
     results_format(1:4) = global_coord_list(i,1:4)
     results_format(5:10) = local_beam_loads(local_k_list(i,1:6,1:6), ieg(i,1:2), axb(1:global_size, global_size+1), global_size)
     Write(16,*) results_format
  End Do
  
  Close(16)

End Program popmus_analysis
