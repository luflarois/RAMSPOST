module modrconfig

!############################# Change Log ##################################
! 5.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000, 2016 - All Rights Reserved
! Brazilian Regional Atmospheric Modeling System - BRAMS
!  Mission Research Corporation / *ASTeR Division
!###########################################################################

!---------------------------------------------------------------------------
! Set maximum values of parameters

integer :: maxgrds != 1 ! maxgrds - maximum number of grids

integer :: nxpmax      != 60 nxpmax  - maximum number of points in x-direction
integer :: nypmax      != 60 nypmax  - maximum number of points in y-direction
integer :: nzpmax      != 60 nzpmax  - maximum number of points in z-direction
integer :: nzgmax      != 15 nzgmax  - maximum number of soil levels

integer, parameter :: maxsclr = 50  ! maxsclr - maximum number of additional scalars
integer, parameter :: maxhp = 1000  ! maxhp   - maximum number of u, v, or t
                                    !           points in a single vertical
                                    !           level interpolated from
                                    !           opposite hemispheric grid

!---------------------------------------------------------------------------
! set maxdim to the largest of nxpmax,nypmax,nzpmax+10,nzgmax
integer :: maxdim ! = 60

!---------------------------------------------------------------------------
! maxmach is the max number of processors that can be used in a parallel run
integer, parameter :: maxmach = 1
include 'micphys.h'

!-------------------------------------------------------------------------------
!        Parameters for some array dimensions
integer maxdimp
integer, parameter :: nstyp=12,nvtyp=30  &
                     ,nkeep=90,nke=nkeep,nintgm=12,maxsndg=200  &
                     ,maxvarf=200,maxsstf=200

!-------------------------------------------------------------------------------
!        COMMON block includes

!-------------------------------------------------------------------------------
character(len=64) :: expnme

!-------------------------------------------------------------------------------
integer                     :: itopo,initial,impl,iadvl,iadvf,lonrad,ngrids  &
                              ,lsflg,ibnd,jbnd,icorflg,ilwrtyp,iswrtyp,iref  &
                              ,jref,ihtran,nfpt,nsndg,ideltat,nacoust,iflag  &
                              ,ntopsmth,izflat,iyear1,imonth1,idate1,ihour1  &
                              ,itime1,isfcl,ihorgrad
! MAXGRDS
integer, allocatable, dimension(:) :: idiffk

!-------------------------------------------------------------------------------
integer                     :: naddsc,nestz1,nestz2,nzg,nzs,iversion,npatch  &
                              ,nvegpat
! MAXGRDS
integer, allocatable, dimension(:) :: nnqparm,nndtrat,nstratx  &
                              ,nstraty, ngbegun,nnacoust,nxtnest,nnsttop  &
                              ,nnstbot, nnxp,nnyp,nnzp,ninest,njnest,nknest
!NZPMAX
integer, allocatable, dimension(:)  :: nstratz1,nstratz2
integer, dimension(nvtyp)   :: kroot

!-------------------------------------------------------------------------------
integer, parameter                     :: maxsched=200,maxschent=5
integer                                :: nsubs
integer, dimension(maxsched,maxschent) :: isched

!-------------------------------------------------------------------------------
real                     :: brunt,wcldbs,drtcon,rmin,radfrq,distim,seatmp  &
                           ,confrq,ubmin,eps,albedo,dthcon,rmax  &
                           ,cphas,dtlong,topref,sspct,polelat,polelon
! MAXGRDS
real, allocatable, dimension(:) :: platn,plonn,centlat,centlon  &
                           ,zkhkm,xkhkm,csz,csx,akmin ! not used cflxy cflz
integer                  :: nhemgrd2

!-------------------------------------------------------------------------------
integer                     :: nhemt,nhemu,nhemv
integer, dimension(4,maxhp) :: ihem1tt,jhem1tt,ihem1uu,jhem1uu  &
                              ,ihem1uv,jhem1uv,ihem1vu,jhem1vu  &
                              ,ihem1vv,jhem1vv
integer, dimension(maxhp)   :: ihem2tt,jhem2tt,ihem2uu,jhem2uu  &
                              ,ihem2uv,jhem2uv,ihem2vu,jhem2vu  &
                              ,ihem2vv,jhem2vv
real, dimension(maxhp)      :: hlatt,hlatu,hlatv,hlont,hlonu,hlonv
real, dimension(4,maxhp)    :: whem1tt,whem1uu,whem1uv,whem1vu,whem1vv

!-------------------------------------------------------------------------------
! NZPMAX , MAXGRDS
real, allocatable, dimension(:,:) :: u01dn,v01dn,pi01dn,th01dn,dn01dn,rt01dn

!-------------------------------------------------------------------------------
integer              :: ipsflg,itsflg,irtsflg,iusflg
real, dimension(maxsndg) :: us,vs,ts,thds,ps,hs,rts

!-------------------------------------------------------------------------------
real, dimension(nstyp)        :: slden,slcpd,slbs,slcond  &
                                ,slcons,slmsts,slpots,ssand,sclay  &
                                ,sorgan,sporo,soilcp,slfc,emisg
real, dimension(nvtyp)        :: albedv,emisv,vglai,vgdlai,vgfrac,vgdfrac  &
                                ,vegzo,vgdisp
real                          :: cmin,corg,cwat,cair,cka,ckw
!nzgmax
real, allocatable, dimension(:) :: slz
!nzgmax,nvtyp
real, allocatable, dimension(:,:) :: root

!-------------------------------------------------------------------------------
real                     :: time,ztop,dzrat,dzmax
! MAXGRDS
real, allocatable, dimension(:) :: deltazn,deltaxn,deltayn,dimove,djmove  &
                           ,gridu,gridv ! not used dtlongn
! NZPMAX
real, allocatable, dimension(:)  :: zz

!-------------------------------------------------------------------------------
character(len=8), dimension(50) :: plfmt,pltit,iplfld

!-------------------------------------------------------------------------------
integer                :: nplt
integer, dimension(50) :: ixsctn,iplvect,isbval,iaa,iab,joa,job,naavg,noavg
real                   :: frqprt
real, dimension(50)    :: plconlo,plconhi,plconin

!-------------------------------------------------------------------------------
character (len=10) :: runtype
character(len=1)   :: timeunit
character (len=32) :: vtabcust
character(len=80)  :: hfilin,afilin,hfilout,afilout,sfcfiles
character(len=20)  :: xlite,ylite,zlite

!-------------------------------------------------------------------------------
integer :: ioutput,iinput,iopunt,kwrite,ihistdel,iclobber
real    :: frqhis,frqanl,timstr,avgtim,frqlite,frqmean,frqboth

!-------------------------------------------------------------------------------

! MAXGRDS
integer, allocatable, dimension(:) :: itoptflg,isstflg,ivegtflg,isoilflg  &
                              ,nofilflg,itopsflg,iz0flg
real                        :: z0fact
! MAXGRDS
real, allocatable, dimension(:)    :: z0max,toptenh,toptwvl

!-------------------------------------------------------------------------------
! MAXGRDS
character(len=80), allocatable, dimension(:) :: itoptfn,isstfn,ivegtfn,isoilfn

!-------------------------------------------------------------------------------
integer :: ngridsh

!-------------------------------------------------------------------------------
integer :: level,nqparm,itopbc,mcphys_type

!-------------------------------------------------------------------------------

! MAXGRDS
integer, allocatable, dimension(:) :: nnx,nnx1,nnx2,nny,nny1,nny2,nnz,nnz1  &
                              ,nnxyzp,nnxysp,nnxyp

!-------------------------------------------------------------------------------
! NZPMAX, MAXGRDS
real, allocatable, dimension(:,:) :: htn,ht2n,ht4n,hwn,hw2n,hw4n

!-------------------------------------------------------------------------------
! NZPMAX, MAXGRDS
real, allocatable, dimension(:,:) :: dztn,dzmn,ztn,zmn ! not used dzt2n dzm2n
real, allocatable, dimension(:,:) :: xtn,xmn
real, allocatable, dimension(:,:) :: ytn,ymn

!-------------------------------------------------------------------------------
! NXPMAX, ,6,6,2, MAXGRDS
real, allocatable, dimension(:,:,:,:,:) :: advwtx
! NYPMAX, ,6,6,2, MAXGRDS
real, allocatable, dimension(:,:,:,:,:) :: advwty
! NZPMAX, ,6,6,2, MAXGRDS
real, allocatable, dimension(:,:,:,:,:) :: advwtz

!-------------------------------------------------------------------------------
integer                 :: nslcon,nvgcon
real                    :: zrough,pctlcon

! NZGMAX
real, allocatable, dimension(:) :: stgoff,slmstr

!-------------------------------------------------------------------------------
integer :: nxp,nx,nx1,nx2,nyp,ny,ny1,ny2,nzp,nzpp,nz,nz1  &
          ,nxyzp,nxyp,nxysp,nscl,nsttop,nstbot,ndtrat,jdim

!-------------------------------------------------------------------------------
real                    :: deltax,deltay,deltaz

! NZPMAX
real, allocatable, dimension(:) :: ht,ht2,ht4,hw,hw2,hw4,zt,zm,dzt,dzm,dzt2,dzm2
! NXPMAX
real, allocatable, dimension(:) :: xt,xm
! NYPMAX
real, allocatable, dimension(:) :: yt,ym

!-------------------------------------------------------------------------------

! TODO - cphx uses NYPMAX instead of NZPMAX. The same analogy for cphy.
! variables not used
! NZPMAX, NYPMAX, 4
!real, allocatable, dimension(:,:,:) :: cphx
! NZPMAX, NXPMAX, 4
!real, allocatable, dimension(:,:,:) :: cphy

!-------------------------------------------------------------------------------

character(len=80)                     :: varfil1,varfil2,varfpfx
character(len=80), dimension(maxvarf) :: varfil

!-------------------------------------------------------------------------------
real                    :: vtime1,vtime2,vwait1,vwaittot
real,dimension(maxvarf) :: vtime
integer                 :: nvarf

!-------------------------------------------------------------------------------
character(len=80)                             :: sstfpfx
! MAXGRDS
character(len=80), allocatable, dimension(:)         :: sstfil1,sstfil2
! maxsstf, MAXGRDS
! character(len=80), allocatable, dimension(maxsstf,MAXGRDS) :: vsstfil,sstfil

!-------------------------------------------------------------------------------
integer                          :: iupdsst
! MAXGRDS
integer, allocatable, dimension(:)      :: nvsstf,nsstf,isstf,isstrecy
integer, dimension(maxsstf)      :: iyearvs,imonthvs,idatevs,ihourvs
! MAXGRDS
real, allocatable, dimension(:)         :: ssttime1,ssttime2
! maxsstf, MAXGRDS
!real, allocatable, dimension(maxsstf,MAXGRDS) :: ssttime

!-------------------------------------------------------------------------------
!maxdimp
real, allocatable, dimension(:)    :: vctr1 ,vctr2 ,vctr11,vctr12
integer, allocatable, dimension(:) :: ivctr

!-------------------------------------------------------------------------------
integer :: isstp,istp,initfld
real    :: timmax,dts,dtlt,dtlv

!-------------------------------------------------------------------------------
! NZPMAX
real, allocatable, dimension(:) :: pi,p0,temp,prt,rc,thet,rvls
real                    :: pfactr,tnudlat,tnudcent,tnudtop,znudtop
integer                 :: nudlat

!-------------------------------------------------------------------------------
!      I/O table commons and information

!-------------------------------------------------------------------------------
integer :: ngrid,ngridc,ngrido,iscr1,iscr2

!-------------------------------------------------------------------------------
integer                    :: memsize,iounit,maxpro,memscr,memind  &
                             ,iogrid,maxpts,maxnzp,maxnxp,maxnyp,i2dvar
! MAXGRDS
integer, allocatable, dimension(:) :: memgrd

!-------------------------------------------------------------------------------
integer                     :: marker3  &
                              ,iup     ,iuc     ,ivp     ,ivc     ,iwp  &
                              ,iwc     ,ipp     ,ipc     ,ithp    ,irtp  &
                              ,ircp    ,irrp    ,irpp    ,irsp    ,irap  &
                              ,irgp    ,irhp    ,iccp    ,icrp    ,icpp  &
                              ,icsp    ,icap    ,icgp    ,ichp    ,icccnp  &
                              ,icifnp  ,ihkm    ,iq2     ,iq6     ,iq7  &
                              ,irv     ,itheta  ,itkep   ,itklp   ,ithsrc  &
                              ,irtsrc  ,ifthrd  ,ivarup  ,ivarvp  ,ivartp  &
                              ,ivarrp  ,ivaruf  ,ivarvf  ,ivartf  ,ivarrf  &
                              ,ivarwts ,ipi0    ,idn0    ,ith0    ,ivkm  &
                              ,ivkh    ,idn0u   ,idn0v &
                              ! add the averaged 3d variables starting at
                              ! index 54 --> 86
                              ,iupm     ,ivpm   ,iwpm    ,ippm    ,ircpm  &
                              ,irrpm    ,irppm  ,irspm   ,irapm   ,irgpm  &
                              ,irhpm    ,iccpm  ,icrpm   ,icppm   ,icspm  &
                              ,icapm    ,icgpm  ,ichpm   ,icccnpm ,icifnpm  &
                              ,ihkmm    ,iq2m   ,iq6m    ,iq7m    ,irvm  &
                              ,ithetam  ,itkepm ,itklpm  ,ithsrcm ,irtsrcm  &
                              ,ifthrdm  ,ivkmm  ,ivkhm
integer, dimension(maxsclr) :: isclp

! would normally add isclpm(maxsclr) here, but don't have an index
! to assign in vtable, so lets not allow averaging of added scalars

!-------------------------------------------------------------------------------
integer                     :: marker3m  &
                              ,iut     ,ivt     ,iwt     ,ipt     ,itht  &
                              ,irtt    ,irct    ,irrt    ,irpt    ,irst  &
                              ,irat    ,irgt    ,irht    ,icct    ,icrt  &
                              ,icpt    ,icst    ,icat    ,icgt    ,icht  &
                              ,icccnt  ,icifnt  ,idum1t  ,itket   ,itklt  &
                              ,ivt3da  ,ivt3db  ,ivt3dc  ,ivt3dd  ,ivt3de  &
                              ,ivt3df  ,ivt3dg  ,ivt3dh  ,ivt3di  ,ivt3dj  &
                              ,ivt3dk  ,ivt3dl  ,ivt3dm  ,ivt3dn  ,ivt3do  &
                              ,ivt3dp
integer, dimension(maxsclr) :: isclt

!-------------------------------------------------------------------------------
integer :: marker2  &
          ,itopt    ,itopu    ,itopv    ,itopm    ,irtgt  &
          ,irtgu    ,irtgv    ,irtgm    ,if13t    ,if13u  &
          ,if13v    ,if13m    ,if23t    ,if23u    ,if23v  &
          ,if23m    ,idxu     ,idxv     ,idxt     ,idxm  &
          ,idyu     ,idyv     ,idyt     ,idym     ,ifmapu  &
          ,ifmapv   ,ifmapt   ,ifmapm   ,ifmapui  ,ifmapvi  &
          ,ifmapti  ,ifmapmi  ,iglat    ,iglon    ,iuw  &
          ,ivw      ,iwfz     ,itfz     ,iqfz     ,iaccpr  &
          ,iaccpp   ,iaccps   ,iaccpa   ,iaccpg   ,iaccph  &
          ,ipcprr   ,ipcprp   ,ipcprs   ,ipcpra   ,ipcprg  &
          ,ipcprh   ,ipcpg    ,iqpcpg   ,idpcpg   ,iaconpr  &
          ,iconprr  ,irshort  ,irlong   ,irlongup ,ialbedt  &
          ,ivarp    ,ivarp2   ,ifcoru   ,ifcorv   ,ivt2da  &
          ,ivt2db   ,ivt2dc   ,ivt2dd   ,ivt2de   ,ivt2df  &
          ,icosz    ,itopzo  &
          ! add the averaged 2d variables starting at
          ! index 73 --> 103
          ,itoptm   ,iglatm   ,iglonm   ,iuwm    ,ivwm  &
          ,iwfzm    ,itfzm    ,iqfzm    ,iaccprm ,iaccppm  &
          ,iaccpsm  ,iaccpam  ,iaccpgm  ,iaccphm ,ipcprrm  &
          ,ipcprpm  ,ipcprsm  ,ipcpram  ,ipcprgm ,ipcprhm  &
          ,ipcpgm   ,iqpcpgm  ,idpcpgm  ,iaconprm,iconprrm  &
          ,irshortm ,irlongm  ,irlongupm,ialbedtm,icoszm  &
          ,topzom

!-------------------------------------------------------------------------------
integer :: marker4s  &
          ,itgp     ,iwgp     ,ischar   ,igsf  &
          ! add the averaged 3d soil variables starting at
          ! index 5 --> 8
          ,itgpm    ,iwgpm    ,ischarm  ,igsfm

!-------------------------------------------------------------------------------
integer :: iscc,iscp,isct

parameter (maxfiles=2000)
parameter (nplmax=50,maxpatch=5)

character*240 fprefix, gprefix
integer nvp, ipresslev,iplevs(nplmax), inplevs
character*16 vp(200), anl2gra, proj, mean_type, ascii_data
real, allocatable :: lati(:), latf(:), loni(:), lonf(:)
integer, allocatable :: zlevmax(:)
real site_lat,site_lon

integer ftimes(maxfiles), ifdates(maxfiles), iftimes(maxfiles)
real nfgrids(maxfiles), startutc, httop

integer, allocatable :: flevels(:,:,:), fdelx(:,:), fdely(:,:)
real, allocatable :: nfgpnts(:,:,:)

namelist/rp_input/ fprefix,nvp, vp,gprefix,anl2gra,proj,mean_type,lati,latf,  &
loni,lonf,zlevmax,ipresslev,inplevs,iplevs,ascii_data,site_lat,site_lon

contains

  subroutine readDimensionsFromHeadFile()

    integer :: nc, nfile, nfn, iunit, ioerr, arrRconfigValues(4)
    integer, parameter :: maxFiles=2000
    logical there
    character(len=256) key, fnames(maxFiles)

    !FAKE allocate = 1. In After reading the head.txt ew got maxgrds
    allocate (zlevmax(10), lati(10),latf(10),loni(10),lonf(10))

    print *, ' '
    print *, 'Opening ramspost.inp file to get fprefix for reading __nnxp, __nnyp, __nnzp, __ngrids, __nzg from *head.txt file'

    open(5,file='ramspost.inp',status='old')
    read(5,rp_input)
    close(5)
    deallocate (zlevmax, lati, latf, loni, lonf )

    nc=lastchar(fprefix)+1
    nfile=-1
    fprefix(nc:)='*-head.txt'
    print*,'RAMS_filelist searching for: ',fprefix(1:nc+10)

    call RAMS_filelist(fnames,fprefix,nfile)

    ! ! construct arrays of various stuff

    print*, "# arquivos = ", nfile

    ! take the first to catch args:__nnxp, __nnyp, __nnzp, __ngrids, __nzg
    !do nfn=1,nfile
    nfn=1
    print*, "Using the file ", fnames(nfn), " for reading __nnxp, __nnyp, __nnzp, __ngrids, __nzg"

    there=.true.
    iunit = 30
    do while(there)
        iunit = iunit + 1
        inquire(iunit,opened=there)
    enddo

    open(iunit,file=fnames(nfn),form='formatted')

    do
       read(iunit,*, iostat=ioerr) key

       if (ioerr /= 0) then
          exit
       else
          if(key .eq. "__ngrids") then
              read(iunit,*) ignore !ignore 1
              read(iunit,*) MAXGRDS
              print*, "__ngrids = ", MAXGRDS
          elseif(key .eq. "__nnxp") then
              read(iunit,*) ignore !ignore 1
              read(iunit,*) NXPMAX
              print*, "__nnxp = ", NXPMAX
          elseif(key .eq. "__nnyp") then
              read(iunit,*) ignore !ignore 1
              read(iunit,*) NYPMAX
              print*, "__nnyp = ", NYPMAX
          elseif(key  .eq. "__nnzp") then
              read(iunit,*) ignore !ignore 1
              read(iunit,*) NZPMAX
              print*, "__nnzp = ", NZPMAX
          elseif(key .eq. "__nzg") then
              read(iunit,*) ignore !ignore 1
              read(iunit,*) NZGMAX
              print*, "__nzg = ", NZGMAX
          end if
          if(MAXGRDS > 0 .and. NXPMAX > 0 .and. NYPMAX > 0 .and. NZPMAX > 0 &
             .and. NZGMAX  > 0) then
             print*, "all variables read !"
             exit
          end if
       end if
    end do

    arrRconfigValues(1:4) = (/NXPMAX,NYPMAX,NZPMAX+10,NZGMAX/)
    MAXDIM = maxval(arrRconfigValues)
    maxdimp=maxdim+1

    close(iunit)

  end subroutine readDimensionsFromHeadFile


  subroutine RAMS_filelist(fnames,file_prefix,nfile)

    character  fnames(*)*(*),file_prefix*(*)
    character file*240,command*240

    ! This version uses nfile as flag for whether to stop if no files exist.
    !    If nfile.ge.0, then stop

    iflag=nfile

    nfile = 0
    print *, ' '
    print *, 'RAMS file analysis: Checking directory - ',file_prefix

    iprelen=index(file_prefix,' ')
    if(iprelen.eq.0) iprelen=len(file_prefix)


  !-srf temporary filelist is now written in the local directory
  !      '/bin/ls -1 '//file_prefix(1:iprelen)//' >/tmp/RAMS_filelist'
    command='/bin/ls -1 '//file_prefix(1:iprelen)//' >./RAMS_filelist'
    print*, command
    call system(command)

    command= 'chmod 777 ./RAMS_filelist'
    call system(command)

    !     Open the directory list and read through the files
    iun=98
    open(unit=iun,file='./RAMS_filelist',status='old',err=15)
    rewind iun

    do nf=1,1000000
       read(iun,'(a128)',end=30,err=30) file
       fnames(nf) = file
    enddo

  30 continue

    close(iun)
    command= '/bin/rm -f ./RAMS_filelist'
    call system(command)

    nfile=nf-1

    if (nfile .eq. 0) then
       print *, 'No RAMS files for prefix:',file_prefix
       if(iflag.ge.0) stop 'RAMS_filelist-no_files'
    endif

    return

  15 print *, 'RAMS_filelist: Error opening ./RAMS_filelist'
  !15 print *, 'RAMS_filelist: Error opening /tmp/RAMS_filelist'
    stop 'RAMS_filelist-/tmp file error : run again'
    return

  100 continue

    return

  end subroutine RAMS_filelist

  subroutine allocateVariables()

    allocate ( flevels(nzpmax,maxgrds,maxfiles), fdelx(maxgrds,maxfiles), &
      fdely(maxgrds,maxfiles), nfgpnts(4,maxgrds,maxfiles) )

    allocate ( zlevmax(maxgrds), nnxp(maxgrds), nnyp(maxgrds), nnzp(maxgrds) &
      ,nnqparm(maxgrds), nndtrat(maxgrds), nstratx(maxgrds), nstraty(maxgrds) &
      ,ngbegun(maxgrds), nnacoust(maxgrds), nxtnest(maxgrds), nnsttop(maxgrds)&
      ,nnstbot(maxgrds), ninest(maxgrds), njnest(maxgrds), nknest(maxgrds) &
      ,deltazn(maxgrds), deltaxn(maxgrds), deltayn(maxgrds), dimove(maxgrds) &
      ,djmove(maxgrds) ,gridu(maxgrds), gridv(maxgrds), idiffk(maxgrds) &
      ,platn(maxgrds), plonn(maxgrds), centlat(maxgrds), centlon(maxgrds) &
      ,zkhkm(maxgrds), xkhkm(maxgrds), csz(maxgrds), csx(maxgrds) &
      ,akmin(maxgrds) )

    allocate ( zz(nzpmax), nstratz1(nzpmax), nstratz2(nzpmax) )

    allocate (dztn(nzpmax,maxgrds), dzmn(nzpmax,maxgrds), ztn(nzpmax,maxgrds) &
    ,zmn(nzpmax,maxgrds), xtn(nzpmax,maxgrds), xmn(nzpmax,maxgrds) &
    ,ytn(nzpmax,maxgrds), ymn(nzpmax,maxgrds), u01dn(nzpmax,maxgrds) &
    ,v01dn(nzpmax,maxgrds), pi01dn(nzpmax,maxgrds), th01dn(nzpmax,maxgrds) &
    ,dn01dn(nzpmax,maxgrds), rt01dn(nzpmax,maxgrds) )

    ! allocate ( root(nzgmax,nvtyp) )

    allocate ( slz(nzgmax) )

  end subroutine allocateVariables
end module modrconfig
