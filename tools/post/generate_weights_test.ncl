;------------------------------------------------------------------
; This file still has to be loaded manually
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"
;------------------------------------------------------------------
begin

    generate_tripole_SCRIP_files = False
    generate_dstgrid_SCRIP_files = True
    generate_tripole_latlon_weights = False

;---Specify a location to use
        nemsrc     = "./"
;---Interpolation methods
    methods        = (/"bilinear" ,"conserve"/)
;---Source tripole grid location 
            srcloc = nemsrc
          gridfile = "tripole.mx025.nc"

       dstgrds     = (/    "0p25",     "0p5",  "1p0"/)
       dstres      = (/ "0.25deg",  "0.5deg", "1deg"/)

       dstlonbeg   = (/  0.125,    0.25,   0.50/)
       dstlonend   = (/359.875,  359.75, 359.50/)
       dstlatbeg   = (/-89.875,  -89.75, -89.50/)
       dstlatend   = (/ 89.875,   89.75,  89.50/)

        dstxdim    = (/1440,    720,   360/)
        dstydim    = (/ 720,    360,   180/)

   if(generate_tripole_SCRIP_files)then
;----------------------------------------------------------------------
;
;----------------------------------------------------------------------

      gf = addfile(nemsrc+gridfile,"r")

    Opt                   = True
    Opt@ForceOverwrite    = True
    Opt@PrintTimings      = True
    Opt@Debug             = True
    Opt@Check             = True
    Opt@GridMask          = gf->wet

          srcfile = nemsrc+"Ct_SCRIP.nc"
       srclonname = "lonCt"
       srclatname = "latCt"
    cornerlonname = "lonCt_vert"
    cornerlatname = "latCt_vert"

      Opt@GridCornerLon = gf->$cornerlonname$
      Opt@GridCornerLat = gf->$cornerlatname$
      print("using locations "+srclatname+"  "+\
                               srclonname+"  "+\
                            cornerlatname+"  "+\
                            cornerlonname)

    curvilinear_to_SCRIP(srcfile, gf->$srclatname$, gf->$srclonname$, Opt)
    delete(Opt) 
    delete(gf)
   end if

   ii = 0

   if(generate_dstgrid_SCRIP_files)then
;----------------------------------------------------------------------
;
;----------------------------------------------------------------------

    ; do corners here fail?

     ; rectilinear destination
     Opt                   = True
     Opt@ForceOverwrite    = True
     Opt@PrintTimings      = True
     Opt@Debug             = True
     Opt@Check             = True

     lons = fspan(dstlonbeg(ii),dstlonend(ii),dstxdim(ii))
     lats = fspan(dstlatbeg(ii),dstlatend(ii),dstydim(ii))
     print(dimsizes(lons))
     print(dimsizes(lats))
     ;Opt@LLCorner          = (/ -90.d,   0.d/)
     ;Opt@URCorner          = (/  90.d, 360.d/)

     Opt@DstGridLat = lats
     Opt@DstGridLon = lons
  
     dstfile = nemsrc+"rect."+dstgrds(ii)+"_SCRIP.nc"
     print("generating dstfile "+dstfile)
     rectilinear_to_SCRIP(dstfile,lats,lons,Opt)

     delete(lats)
     delete(lons)

     ; latlon destination
     dstfile = nemsrc+"latlon."+dstgrds(ii)+"_SCRIP.nc"
     print("generating dstfile "+dstfile)
     latlon_to_SCRIP(dstfile,dstres(ii),Opt)
     delete(Opt)
   end if

   if(generate_tripole_latlon_weights)then
;----------------------------------------------------------------------
;
;----------------------------------------------------------------------

    ;srcfile = nemsrc+"Ct_SCRIP.nc"
    srcfile = nemsrc+"Ct_SCRIP_grid.nc"
    do jj = 0,dimsizes(methods)-1
      Opt                   = True
      Opt@ForceOverwrite    = True
      Opt@PrintTimings      = True
      Opt@InterpMethod      = methods(jj)
      Opt@Debug             = True
      Opt@Check             = True
      Opt@SrcESMF           = False
      Opt@DstESMF           = False

      dstfile = nemsrc+"latlon."+dstgrds(ii)+"_SCRIP.nc"
     ; wgtfile = nemsrc+"tripole.mx025.Ct.to.latlon."+dstgrds(ii)+"."+methods(jj)+".nc"
      wgtfile = nemsrc+"tripole.mx025.Ct.to.latlon."+dstgrds(ii)+"."+methods(jj)+"_grid.nc"
      ESMF_regrid_gen_weights(srcfile,dstfile,wgtfile,Opt)

      dstfile = nemsrc+"rect."+dstgrds(ii)+"_SCRIP.nc"
      ;wgtfile = nemsrc+"tripole.mx025.Ct.to.rect."+dstgrds(ii)+"."+methods(jj)+".nc"
      wgtfile = nemsrc+"tripole.mx025.Ct.to.rect."+dstgrds(ii)+"."+methods(jj)+"_grid.nc"
      ESMF_regrid_gen_weights(srcfile,dstfile,wgtfile,Opt)
     end do

   end if
exit
end

