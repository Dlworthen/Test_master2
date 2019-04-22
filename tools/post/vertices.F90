subroutine fill_vertices(jbeg,jend,iVert,jVert,lat,lon,latvert,lonvert)

  use param

  implicit none

                            integer, intent( in) :: jbeg,jend
                            integer, intent( in) :: iVert(nv), jVert(nv)
  real(kind=8), dimension(ni,nj),    intent( in) ::  lat, lon

  real(kind=8), dimension(ni,nj,nv), intent(out) :: latvert,lonvert

  integer :: i,j,n,ii,jj

  do j = jbeg,jend
   do i = 1,ni
    do n = 1,nv
      ii = i + iVert(n); jj = j + jVert(n)
      if(ii .eq.    0)ii = ni
      if(ii .eq. ni+1)ii = 1
      latvert(i,j,n)   = lat(ii,jj)
      lonvert(i,j,n)   = lon(ii,jj)
    enddo
   enddo
  enddo
end subroutine fill_vertices
 
subroutine fill_bottom(iVert,jVert,lat,lon,latvert,lonvert)

  use param

  implicit none

                            integer, intent( in) :: iVert(nv), jVert(nv)
  real(kind=8), dimension(ni,nj),    intent( in) ::  lat, lon

  real(kind=8), dimension(ni,nj,nv), intent(out) :: latvert,lonvert

  integer :: i,j,n,ii,jj

  ! fill in grid bottom (j=1) 
  ! vertices 1,2 are available
  ! vertices 3,4 must be set manually
      j = 1
   do i = 1,ni
    do n = 1,2
      ii = i + iVert(n); jj = j + jVert(n)
      if(ii .eq.    0)ii = ni
      if(ii .eq. ni+1)ii = 1
      latvert(i,j,n)   = lat(ii,jj)
      lonvert(i,j,n)   = lon(ii,jj)
    enddo
    ! south of grid bottom
      latvert(i,j, 3) = -82.d0  
      latvert(i,j, 4) = -82.d0  
      lonvert(i,j, 3) = lonvert(i,j,2)
      lonvert(i,j, 4) = lonvert(i,j,1)
   enddo
#ifdef test
  ! if define vertices from LL
  ! fill in grid bottom (j=1) 
  ! vertices 3,4 are available
  ! vertices 1,2 must be set manually
      j = 1
   do i = 1,ni
    do n = 3,4
      ii = i + iVert(n); jj = j + jVert(n)
      if(ii .eq.    0)ii = ni
      if(ii .eq. ni+1)ii = 1
      latvert(i,j,n)   = lat(ii,jj)
      lonvert(i,j,n)   = lon(ii,jj)
    enddo
    ! south of grid bottom
      latvert(i,j, 1) = -82.d0
      latvert(i,j, 2) = -82.d0
      lonvert(i,j, 1) = lonvert(i,j,4)
      lonvert(i,j, 2) = lonvert(i,j,3)
   enddo
#endif
end subroutine fill_bottom

