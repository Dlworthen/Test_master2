module param

  implicit none

  integer, parameter :: ni = 1440, nj = 1080, nv = 4

  ! super-grid source variables
  integer, parameter :: nx  = ni*2, ny  = nj*2

  ! ij offsets moving counter-clockwise around each Ct(i,j) from UR corner
  integer, parameter, dimension(nv) :: iVertCt = (/0, -1, -1,  0/)
  integer, parameter, dimension(nv) :: jVertCt = (/0,  0, -1, -1/)
  ! ij offsets moving counter-clockwise around each Ct(i,j) from LL corner
  !integer, parameter, dimension(nv) :: iVertCt = (/-1,  0,  0, -1/)
  !integer, parameter, dimension(nv) :: jVertCt = (/-1, -1,  0,  0/)

  integer, parameter :: ncoord = 2 
  integer, parameter :: nverts = 2           
  integer, parameter ::  nvars = ncoord + nverts

end module param
