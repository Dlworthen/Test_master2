module scrip

  use param

  implicit none

  integer, parameter :: nij = ni*nj

  contains

   subroutine write_scrip(cdffile,stagger,lat,lon,latv,lonv,mask)

   use netcdf
    implicit none

    character(len=*), intent(in) :: cdffile
    character(len=*), intent(in) :: stagger

       real(kind=8), dimension(ni,nj),    intent(in) :: lat,lon
       real(kind=8), dimension(ni,nj,nv), intent(in) :: latv,lonv
    integer(kind=4), dimension(ni,nj),    intent(in) :: mask

    ! local variables

       character(len= 20) :: varname, vunit
                  integer :: n,i,j,ij
                  integer :: xdim, ydim, xcdim, datid, rc, ncid
    integer, dimension(1) :: ndim1, dim1
    integer, dimension(2) :: ndim2, dim2

       real(kind=8), dimension(nij)    :: r1d
       real(kind=8), dimension(nv,nij) :: r2d
    integer(kind=4), dimension(nij)    :: i1d

    ! create the file

    rc = nf90_create(trim(cdffile), nf90_write, ncid)
    print *, 'writing grid to ',trim(cdffile)
    !print *, 'nf90_create = ',trim(nf90_strerror(rc))

    rc = nf90_def_dim(ncid,'grid_rank',     2,  xdim)
    rc = nf90_def_dim(ncid,'grid_size',   nij,  ydim)
    rc = nf90_def_dim(ncid,'grid_corners', nv, xcdim)

     dim1(1) = xdim
     varname = 'grid_dims'
     rc = nf90_def_var(ncid, trim(varname), nf90_int, dim1, datid)
    if(stagger .eq. 'Ct')then
     dim1(1) = ydim
     varname = 'grid_imask'
     rc = nf90_def_var(ncid, trim(varname), nf90_int, dim1, datid)
     rc = nf90_put_att(ncid, datid, 'units', 'unitless')
    end if

    dim1(1) = ydim
     varname = 'grid_center_lat'
     rc = nf90_def_var(ncid, trim(varname), nf90_double, dim1, datid)
     rc = nf90_put_att(ncid, datid, 'units', 'degrees')
     varname = 'grid_center_lon'
     rc = nf90_def_var(ncid, trim(varname), nf90_double, dim1, datid)
     rc = nf90_put_att(ncid, datid, 'units', 'degrees')

    dim2(2) =  ydim
    dim2(1) = xcdim
     varname = 'grid_corner_lat'
     rc = nf90_def_var(ncid, trim(varname), nf90_double, dim2, datid)
     rc = nf90_put_att(ncid, datid, 'units', 'degrees')
     varname = 'grid_corner_lon'
     rc = nf90_def_var(ncid, trim(varname), nf90_double, dim2, datid)
     rc = nf90_put_att(ncid, datid, 'units', 'degrees')
     rc = nf90_enddef(ncid)

     rc = nf90_inq_varid(ncid,  'grid_dims', datid)
     rc = nf90_put_var(ncid,          datid,   (/1440,1080/))

    if(stagger .eq. 'Ct')then
       ij = 0
     do j = 1,nj
      do i = 1,ni
            ij = ij+1
       i1d(ij) = mask(i,j)
      enddo
     enddo
     rc = nf90_inq_varid(ncid, 'grid_imask', datid)
     rc = nf90_put_var(ncid,          datid,   i1d)
    endif

       ij = 0
     do j = 1,nj
      do i = 1,ni
            ij = ij+1
       r1d(ij) = lat(i,j)
      enddo
     enddo
     rc = nf90_inq_varid(ncid, 'grid_center_lat', datid)
     rc = nf90_put_var(ncid,               datid,   r1d)

       ij = 0
     do j = 1,nj
      do i = 1,ni
            ij = ij+1
       r1d(ij) = lon(i,j)
      enddo
     enddo
     rc = nf90_inq_varid(ncid, 'grid_center_lon', datid)
     rc = nf90_put_var(ncid,               datid,   r1d)

       ij = 0
     do j = 1,nj
      do i = 1,ni
         ij = ij+1
       do n = 1,nv
        r2d(n,ij) = latv(i,j,n)
       enddo
      enddo
     enddo
     rc = nf90_inq_varid(ncid, 'grid_corner_lat', datid)
     rc = nf90_put_var(ncid,               datid,   r2d)

       ij = 0
     do j = 1,nj
      do i = 1,ni
         ij = ij+1
       do n = 1,nv
        r2d(n,ij) = lonv(i,j,n)
       enddo
      enddo
     enddo
     rc = nf90_inq_varid(ncid, 'grid_corner_lon', datid)
     rc = nf90_put_var(ncid,               datid,   r2d)
     rc = nf90_close(ncid)

   end subroutine write_scrip
end module scrip
