module debugprint

  use param
  use grdvar

  implicit none

  contains

  subroutine checkpoint

   integer :: i,j

    i = 1; j = 1
    print '(f12.5,a,f12.5)',latCt_vert(i,j,2),'        ',latCt_vert(i,j,1)
    print '(a12,f12.5)','          ',latCt(i,j)
    print '(f12.5,a,f12.5)',latCt_vert(i,j,3),'        ',latCt_vert(i,j,4)
    print *
    print '(f12.5,a,f12.5)',lonCt_vert(i,j,2),'        ',lonCt_vert(i,j,1)
    print '(a12,f12.5)','          ',lonCt(i,j)
    print '(f12.5,a,f12.5)',lonCt_vert(i,j,3),'        ',lonCt_vert(i,j,4)
    print *
    print *

    i = 1440; j = 1
    print '(f12.5,a,f12.5)',latCt_vert(i,j,2),'        ',latCt_vert(i,j,1)
    print '(a12,f12.5)','          ',latCt(i,j)
    print '(f12.5,a,f12.5)',latCt_vert(i,j,3),'        ',latCt_vert(i,j,4)
    print *
    print '(f12.5,a,f12.5)',lonCt_vert(i,j,2),'        ',lonCt_vert(i,j,1)
    print '(a12,f12.5)','          ',lonCt(i,j)
    print '(f12.5,a,f12.5)',lonCt_vert(i,j,3),'        ',lonCt_vert(i,j,4)
    print *
    print *

    print *,"latCt minmax ",minval(latCt),maxval(latCt)
    print *,minval(lonCt_vert),maxval(lonCt_vert)
  end subroutine checkpoint
end module debugprint
