module fixgriddefs

  implicit none

  integer, parameter :: maxvars = 40

  type grid_defs
    character(len=12)   ::  var_name
    character(len=64)   :: long_name
    character(len=20)   :: unit_name
    character(len= 2)   ::  var_type
    character(len=20)   ::  vertices
  end type grid_defs

  type(grid_defs) :: fixgrid(maxvars)
  contains

  subroutine fixgrid_typedefine

  integer :: ii = 0
  
   !default
   fixgrid(:)%var_type  = 'r8'
   fixgrid(:)%vertices  = ' '

   ii = ii + 1
   fixgrid(ii)%var_name  = 'lonCt'
   fixgrid(ii)%long_name = 'Longitude of center (Ct) points'
   fixgrid(ii)%unit_name = 'degrees'
   fixgrid(ii)%vertices  = 'lonCt_vert'

   ii = ii + 1
   fixgrid(ii)%var_name  = 'latCt'
   fixgrid(ii)%long_name = 'Latitude of center (Ct) points'
   fixgrid(ii)%unit_name = 'degrees'
   fixgrid(ii)%vertices  = 'latCt_vert'

   ii = ii + 1
   fixgrid(ii)%var_name  = 'lonCt_vert'
   fixgrid(ii)%long_name = 'Longitude Vertices of Ct points'
   fixgrid(ii)%unit_name = 'degrees'

   ii = ii + 1
   fixgrid(ii)%var_name  = 'latCt_vert'
   fixgrid(ii)%long_name = 'Latitude Vertices of Ct points'
   fixgrid(ii)%unit_name = 'degrees'

 end subroutine fixgrid_typedefine
end module fixgriddefs
