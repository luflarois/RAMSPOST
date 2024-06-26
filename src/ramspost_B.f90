!*************************************************************
subroutine RAMS_anal_init(nfile,fnames,file_prefix,          &
      dep_zlev,iep_nx,iep_ny,iep_nz,iep_ng,iep_np,           &
      iep_stdate,iep_step,iep_ngrids)

use an_header
use modrconfig

!SRF
dimension iep_stdate(6),iep_step(6)

integer, intent(inout) :: iep_nx(maxgrds), iep_ny(maxgrds), iep_nz(maxgrds)
real, intent(inout) :: dep_zlev(nzpmax,maxgrds)
!SRF

character*240 fnames(maxfiles),fnamesp(maxfiles)  &
     ,file_prefix*(*),fpref*256
common /mem/ memsize4

dimension fdata(*),idata(*)
character*(*) cdata(*)
dimension mondays(13)
data mondays/31,28,31,30,31,30,31,31,30,31,30,31,31/

maxmem=0
fpref=file_prefix
nc=lastchar(fpref)+1
nfile=-1
fpref=file_prefix
fpref(nc:)='*-head.txt'
print*,'RAMS_filelist searching for: ',fpref(1:nc+10)

 call RAMS_filelist(fnames,fpref,nfile)

! construct arrays of various stuff

do nfn=1,nfile
   open(10,file=fnames(nfn),form='formatted')

   read(10,*) nvbtab

print*,fnames(nfn)(1:lastchar(fnames(nfn))),nvbtab

   allocate (anal_table(nvbtab))
   do nv=1,nvbtab
      read(10,*) anal_table(nv)%string   &
                ,anal_table(nv)%npointer  &
                ,anal_table(nv)%idim_type  &
                ,anal_table(nv)%ngrid  &
                ,anal_table(nv)%nvalues


!      print*, anal_table(nv)%string   &
!                ,anal_table(nv)%npointer  &
!                ,anal_table(nv)%idim_type  &
!                ,anal_table(nv)%ngrid  &
!                ,anal_table(nv)%nvalues


   enddo
   call commio('ANAL','READ',10)
   close(10)
   ftimes(nfn)=time
   istrhrs=nint(float(itime1)/100.+0.0001)
   strtim=istrhrs+float(itime1-istrhrs*100)/60.
   startutc=strtim
!   print*,'X',nfn,ftimes(nfn)
   deallocate (anal_table)
enddo

call RAMS_fltsort(nfile,ftimes,fnames)

do nfn=1,nfile
   open(10,file=fnames(nfn),form='formatted')
   read(10,*) nvbtab
   allocate (anal_table(nvbtab))
   do nv=1,nvbtab
      read(10,*) anal_table(nv)%string   &
                ,anal_table(nv)%npointer  &
                ,anal_table(nv)%idim_type  &
                ,anal_table(nv)%ngrid  &
                ,anal_table(nv)%nvalues
   enddo
   call commio('ANAL','READ',10)
   close(10)
   ftimes(nfn)=time
!!!!!!!
!SRF
 if(nfn.eq.1) then
!!!!!!!!!!!!!!!!!!!   call ep_setdate(iyear1,imonth1,idate1,strtim,iep_stdate)
   iep_ngrids=ngrids
   do n=1,ngrids
    maxmem=max(maxmem,nnxp(n)*nnyp(n)*nnzp(n))
    iep_nx(n)=nnxp(n)
    iep_ny(n)=nnyp(n)
    iep_nz(n)=nnzp(n)
    iep_ng   =nzg
    iep_np   =npatch
!!!
!    print*,n, iep_nx(n), iep_ny(n), iep_nz(n) ,iep_ng,iep_np
!!!
    do nn=1,nnzp(n)
      dep_zlev(nn,n)=ztn(nn,n)
    enddo
   enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  call ep_setdate(0,0,0,0.,iep_step)
endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!if(nfn.eq.2) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  call ep_setdate(0,0,0,time/3600.,iep_step)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  if(time/3600. .ge. 24.)iep_step(3)=int((time/3600.)/24.)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!endif
!SRF
!!!!!!
!
   itme=nint(strtim*3600+time)
   iadddays=itme/86400
   izhours =(itme-iadddays*86400)/3600
   izmin   =(itme-iadddays*86400-izhours*3600)/60
   izsec   =itme-iadddays*86400-izhours*3600-izmin*60

   iftimes(nfn)= izhours
   iftimes(nfn)= iftimes(nfn) *100 + izmin
   iftimes(nfn)= iftimes(nfn) *100 + izsec
!---------------------------
!   print*,'#### nfn iftimes=',nfn,iftimes(nfn),izhours,izmin,itme,strtim
!   print*,'---',iadddays,izhours,izmin,izsec,ftimes(nfn)
!
!   if(nfn.ge.nfile) stop
!---------------------------
   if(iyear1.gt.50.and.iyear1.lt.100) iyear1=iyear1+1900
   if(iyear1.lt.50) iyear1=iyear1+2000
   mondays(2)=28+(1-min(1,mod(iyear1,4)))

   iiyear=iyear1
   iidate=idate1+iadddays
   iimon=imonth1
   101 if(iidate.gt.mondays(iimon)) then
   iidate=iidate-mondays(iimon)
   iimon=iimon+1
   goto 101
   endif
      102 if(iimon.gt.12) then
      iiyear=iiyear+1
      iimon=iimon-12
      go to 102
   endif

   ifdates(nfn)=iiyear       *100  + iimon
   ifdates(nfn)=ifdates(nfn) *100  + iidate

   nfgrids(nfn)=ngrids

!   call grdcoords()
   do ng=1,ngrids
      nfgpnts(1,ng,nfn)=nnxp(ng)
      nfgpnts(2,ng,nfn)=nnyp(ng)
      nfgpnts(3,ng,nfn)=nnzp(ng)
      nfgpnts(4,ng,nfn)=nzg
      fdelx(ng,nfn)=DELTAXN(NG)
      fdely(ng,nfn)=DELTAYN(NG)
         !print*,ng,nnxp(ng),nnyp(ng),nnzp(ng),nzg

      do k=1,nnzp(ng)
         flevels(k,ng,nfn)=ztn(k,ng)
!         print*,ng,k,flevels(k,ng,nfn)
      enddo
   enddo
   httop=zmn(nnzp(1)-1,1)

!   print*,'files-',nfn,ftimes(nfn),ifdates(nfn),iftimes(nfn),fnames(nfn)
   close(10)
!!!!!!!!!!
!SRF : nao desaloque no final, sera usado em outra rotina
   if(nfn.lt.nfile) deallocate (anal_table)
!!!!!!!!!!
enddo

memsize4=maxmem

return
!srf

entry RAMS_get_time_init(nfl,iyear,imonth,idate,ihour,imin)
 iyear =iyear1
 imonth=imonth1
 idate =idate1
 ihour =int(float(iftimes(nfl))/10000.)
 imin  =int(float(iftimes(nfl)-10000*ihour)/100.)
return

entry RAMS_get_time_step(iistep,hunit,nfiles)


 if(nfiles.eq.1) then
  iistep = 1
  hunit  = 3
  return
 endif
! iistep=iftimes(2)-iftimes(1)
 iistep=int(ftimes(2)-ftimes(1))


! if(iistep/86400 .ge. 1) then
! iistep=int(float(iistep)/86400.)
! hunit=4 ! dia
!  return
! endif

 if(iistep/3600 .ge. 1) then
  iistep=int(float(iistep)/3600.)
  hunit=3 ! horas
  return
 endif
 if(iistep/60.ge.1) then
  iistep=int(float(iistep)/60.)
  hunit=2 !min
  return
 endif
 if(iistep.lt.60) then
  hunit=1 !seg
  return
 endif
return

!print*,iihour,iimin,iftimes(nfl)
return

entry RAMS_get_idata(nopt,nfl,ngr,idata,nval)

if(nopt.eq.0)then
   nval=4
   do k=1,nval
      idata(k)=nfgpnts(k,ngr,nfl)
   enddo
elseif(nopt.eq.1)then
   nval=1
   idata(1)=nfgrids(nfl)
elseif(nopt.eq.2)then
   nval=1
   idata(1)=ifdates(nfl)
elseif(nopt.eq.3)then
   nval=1
   idata(1)=iftimes(nfl)
endif
return

entry RAMS_get_fdata(nopt,nfl,ngr,fdata,nval)

if(nopt.eq.0)then
   nval=nfgpnts(3,ngr,nfl)
   do k=1,nval
      fdata(k)=flevels(k,ngr,nfl)
   enddo
elseif(nopt.eq.1)then
   nval=1
   fdata(1)=ftimes(nfl)
elseif(nopt.eq.2)then
   nval=1
   fdata(1)=startutc
   print*,'RAMS_get_fdata: startutc',startutc
elseif(nopt.eq.3)then
   nval=1
   fdata(1)=httop
elseif(nopt.eq.4)then
   nval=1
   fdata(1)=fdelx(ngr,nfl)
elseif(nopt.eq.5)then
   nval=1
   fdata(1)=fdely(ngr,nfl)
endif

return

entry RAMS_get_cdata(nopt,nfl,cdata,nval)

if(nopt.eq.0)then
   cdata(1)=fnames(nfl)
   nval=1
elseif(nopt.eq.1)then
   cdata(1)=fnamesp(nfl)
   nval=1
endif

return

end
!***************************************************************************

integer function RAMS_getvar(stringg,itype,ngrd,a,b,flnm)

use an_header

implicit none
!include 'interface.h'

real :: a(*),b(*)
integer :: itype,ngrd,il,lastchar,ill
character*(*) flnm,cgrid*1,flng*240,errmsg*120,stringg,string*20
logical there
integer :: ierr_getvar,ifound,ni,npts,iword
common /getvar/ierr_getvar,ifound

!print*,'----------------------------'
!print*,stringg,string
!print*,'----------------------------'
!!!!!!!!!!!!
!para leitura de analises de medias
il=lastchar(flnm)
!print*,il,flnm
!print*,'---- ',flnm(il-18:il-18)

ierr_getvar=0
RAMS_getvar=0
ill=lastchar(stringg)

if(flnm(il-18:il-18).eq.'M') then
string=stringg(1:ill)//'M'
!print*,ill,string(1:ill+1),' ',string,' ',char(0)

else

string=stringg(1:ill)
endif
!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!if (allocated(anal_table)) deallocate (anal_table)
!
!open(10,file=' /home/sfreitas/rams/model/analysis/a3-A-1992-09-24-080000-head.txt'&
!,form='formatted')
!read(10,*) nvbtab
!print*,'2',nvbtab
!
!   allocate (anal_table(nvbtab))
!   do ni=1,nvbtab
!      read(10,*) anal_table(ni)%string,anal_table(ni)%npointer  &
!                ,anal_table(ni)%idim_type,anal_table(ni)%ngrid  &
!                 ,anal_table(ni)%nvalues
!
!
!      print*, anal_table(ni)%string,anal_table(ni)%npointer  &
!             ,anal_table(ni)%idim_type,anal_table(ni)%ngrid  &
!             ,anal_table(ni)%nvalues
!
!
!   enddo
!close(10)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!SRF nao aloque.
!if (allocated(anal_table)) deallocate (anal_table)
!allocate (anal_table(nvbtab))

do ni=1,nvbtab

!  print*,'----------------ni=',ni
!  print*, anal_table(ni)%string,anal_table(ni)%npointer  &
!         ,anal_table(ni)%idim_type,anal_table(ni)%ngrid  &
!         ,anal_table(ni)%nvalues


   if(string.eq.anal_table(ni)%string.and.ngrd.eq.anal_table(ni)%ngrid) then
      write(cgrid,'(i1)') ngrd
      flng=flnm//'-g'//cgrid//'.vfm'//char(0)
      inquire(file=flng,exist=there)
      if(.not.there) then
         errmsg='File not found - '//flng
         call error_mess(errmsg)
         return
      endif
      npts=anal_table(ni)%nvalues
      itype=anal_table(ni)%idim_type
      iword=anal_table(ni)%npointer

!
            !print*,'-------------------------------------------------------------'
            print*,'Get Var: ',anal_table(ni)%string,' itype=', itype
            print*,'Tamanho do record=',npts,' Ponteiro=',iword
            print*,'-------------------------------------------------------------'
!
      call RAMS_c_open(flng,'r'//char(0))
      call vfirecr(10,a,npts,'LIN',b,iword)
      call RAMS_c_close()

!!!!!!!!!!!
!if(anal_table(ni)%string.eq.'UP') &
!      call lixo(a,npts)
!!!!!!!!!!!


      RAMS_getvar=0
      ifound=ifound+1
      return

   endif
enddo

errmsg='Variable not available in this run/grid - '//string
call error_mess(errmsg)
errmsg='-------------------------------------------------------------'
call error_mess(errmsg)
RAMS_getvar=1
ierr_getvar=1

return
end
!************
subroutine lixo(a,n)
dimension a(n)
do i=1,n
print*,i,a(i)
enddo
!stop
return
end
!************
!
!***************************************************************************
!############################# Change Log ##################################
! 2.3.0.3
!
! 000830 CJT RAMS_varlib ##
!            Removed interface.h ##
! 000829 CJT RAMS_varlib ##
!            Changed from passing arguments to using an_header module ##
! 000828 MJB RAMS_varlib ##
!            Replaced c dynamic allocations to f90. ##
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!  Mission Research Corporation / *ASTeR Division
!###########################################################################

subroutine RAMS_varlib(cvar,n1,n2,n3,n4,n5,ngrd,flnm  &
                 ,cdname,cdunits,ivar_type,a,b,a2)
use modrconfig
implicit none

integer :: n1,n2,n3,ngrd,n4,n5

integer :: memsiz4

common /mem/memsiz4

character*(*) cvar,flnm,cdname,cdunits

real :: a(*),b(*),a2(*)

!real, allocatable, save :: c(:),d(:),e(:),f(:)
real, allocatable :: c(:),d(:),e(:),f(:),g(:)
integer :: lv,lv2,idim_type,irecind,irecsize,irecsizep,ind,ispec
integer :: memsave4,ierr,kp

integer, external :: RAMS_getvar, lastchar, irfree, iralloc
integer :: ierr_getvar,ifound,ivar_type
real  f1(n1,n2,n3),f2(n1,n2,n3),fxx
common /getvar/ierr_getvar,ifound
data memsave4/0/

real, allocatable :: pv1(:,:,:),pv2(:,:,:),pv3(:,:,:),pv4(:,:,:)
real, allocatable :: pv5(:,:,:)

integer, parameter :: nwave=50
real, allocatable :: aot(:,:,:)
logical :: is_memsiz4_gt_memsave4

if (allocated(aot)) deallocate (aot)
allocate(aot(n1,n2,nwave))

is_memsiz4_gt_memsave4 = memsiz4 > memsave4

if (is_memsiz4_gt_memsave4) then
  if (allocated(c)) deallocate (c)
  if (allocated(d)) deallocate (d)
  if (allocated(e)) deallocate (e)
  if (allocated(f)) deallocate (f)
  if (allocated(g)) deallocate (g)
endif

if (.not. allocated(c)) then
  allocate(c(memsiz4))
endif
if (.not. allocated(d)) then
  allocate(d(memsiz4))
endif
if (.not. allocated(e)) then
  allocate(e(memsiz4))
endif
if (.not. allocated(f)) then
  allocate(f(memsiz4))
endif
if (.not. allocated(g)) then
  allocate(g(memsiz4))
endif

if (is_memsiz4_gt_memsave4) then
   memsave4 = memsiz4
endif

lv=lastchar(cvar)
lv2=min(lv,index(cvar,':')-1)  ! for HYPACT fields
!print*,'===> varlib- ',cvar,n1,n2,n3,ngrd

ivar_type=0
ierr_getvar=0
ierr=0
ifound=0

! 3D VELOCITY AND VORTICITY VARIABLES

if(cvar(1:lv).eq.'u') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   cdname='u'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'v') then
   ivar_type=3
   ierr= RAMS_getvar('VP',idim_type,ngrd,a,b,flnm)
   cdname='v'
   cdunits='m/s'

!Incluso por Demerval

!<7/12>elseif(cvar(1:lv).eq.'zitheta') then
!<7/12>   ivar_type=2
!<7/12>!DSM   ierr=RAMS_getvar('TKEP',idim_type,ngrd,a,b,flnm)
!<7/12>   ierr=RAMS_getvar('TOPT',idim_type,ngrd,c,b,flnm)
!<7/12>   call RAMS_comp_pbl(n1,n2,n3,a,c,ngrd)
!<7/12>   cdname='PBL height;'
!<7/12>   cdunits='m;'

elseif(cvar(1:lv).eq.'zitheta') then
    ivar_type=2
    ierr= RAMS_getvar('THETA',idim_type,ngrd,c,b,flnm)
    ierr= RAMS_getvar('RCP',idim_type,ngrd,e,b,flnm)
    call get_ZItheta(n1,n2,n3,a,c,e,ngrd)
    cdname='Height PBL'
    cdunits='m -sigmaz'

!FIM (Demerval)

elseif(cvar(1:lv).eq.'nebu') then
   ivar_type=2

   call RAMS_comp_zero(n1,n2,n3,a)
   ierr= RAMS_getvar('RCP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RRP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RPP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RSP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RAP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RGP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RHP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   call RAMS_comp_noneg(n1,n2,n3,a)

   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd) ! d=dens_ar
   call RAMS_comp_mult(n1,n2,n3,a,d)         ! aqui a=rv*dens_ar
   call RAMS_comp_vertint(n1,n2,n3,a,e,ngrd) ! agua em kg/m^2
   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
  cdname='tot cond vert integrated'
   cdunits='g/m2'

elseif(cvar(1:lv).eq.'zlcl') then
   ivar_type=2
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)

   call RAMS_comp_zlcl(n1,n2,n3,a,c,d)
   cdname='LCL height'
   cdunits='m'

elseif(cvar(1:lv).eq.'zi') then
   ivar_type=2
   ierr= RAMS_getvar('TKEP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_pbl(n1,n2,n3,a,c,ngrd)
   cdname='Zi'
   cdunits='m'

elseif(cvar(1:lv).eq.'zirh') then
   ivar_type=2
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)

   call RAMS_comp_rh(n1,n2,n3,e,c,d) ! e=rh
   call RAMS_comp_noneg(n1,n2,n3,e)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,c,b,flnm)
   call get_ZI_RELHUM(n1,n2,n3,a,c,e,ngrd)

   cdname='ZI by relative humidity'
   cdunits='m'



elseif(cvar(1:lv).eq.'ue') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_rotate(n1,n2,n3,a,c,ngrd)
   cdname='ue'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'ve') then
   ivar_type=3
   ierr= RAMS_getvar('VP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('UP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_rotate(n1,n2,n3,c,a,ngrd)
   cdname='ve'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'ue_avg') then
   ivar_type=3
   ierr=RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr=RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_rotate(n1,n2,n3,a,c,ngrd)
   call RAMS_comp_avgu(n1,n2,n3,a)
   cdname='ue_avg'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'ve_avg') then
   ivar_type=3
   ierr=RAMS_getvar('VP',idim_type,ngrd,a,b,flnm)
   ierr=RAMS_getvar('UP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_rotate(n1,n2,n3,c,a,ngrd)
   call RAMS_comp_avgv(n1,n2,n3,a)
   cdname='ve_avg'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'w') then
   ivar_type=3
   ierr= RAMS_getvar('WP',idim_type,ngrd,a,b,flnm)
   cdname='w'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'wcms') then
   ivar_type=3
   ierr= RAMS_getvar('WP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_wcms(n1,n2,n3,a)
   cdname='w'
   cdunits='cm/s'

elseif(cvar(1:lv).eq.'w_avg') then
   ivar_type=3
   ierr=RAMS_getvar('WP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_avgw(n1,n2,n3,a)
   cdname='w_avg'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'speed') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_speed(n1,n2,n3,a,c)
   cdname='speed'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'speed_mph') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_speed(n1,n2,n3,a,c)
   call RAMS_comp_mults(n1,n2,n3,a,2.237)
   cdname='speed'
   cdunits='mph'



!srf----
elseif(cvar(1:lv).eq.'tempf2m') then
   ivar_type=2
   ierr=RAMS_getvar('UP',idim_type,ngrd,c,b,flnm)
   ierr=RAMS_getvar('VP',idim_type,ngrd,d,b,flnm)
   call RAMS_comp_speed(n1,n2,n3,c,d)
   ierr=RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
   ierr=RAMS_getvar('PI',idim_type,ngrd,f,b,flnm)
   ierr=RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   allocate (pv1(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv2(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv3(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv4(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv5(nnxp(ngrd),nnyp(ngrd),npatch))
   ierr=RAMS_getvar('USTAR',idim_type,ngrd,pv1,b,flnm)
   ierr=RAMS_getvar('PATCH_ROUGH',idim_type,ngrd,pv2,b,flnm)
   ierr=RAMS_getvar('CAN_TEMP',idim_type,ngrd,pv3,b,flnm)
   ierr=RAMS_getvar('PATCH_AREA',idim_type,ngrd,pv4,b,flnm)
   ierr=RAMS_getvar('TSTAR',idim_type,ngrd,pv5,b,flnm)
   call RAMS_reduced_temp(nnxp(ngrd),nnyp(ngrd),nnzp(ngrd),npatch  &
                          ,a,c,pv1,pv5,2.,ztn(2,ngrd),pv2,pv4,pv3,d,f,e  &
                          ,zmn(nnzp(1)-1,1))
   deallocate (pv1,pv2,pv3,pv4,pv5)
   call RAMS_comp_tempK(n1,n2,1,a,f)
   call RAMS_comp_tempF(n1,n2,1,a)
   cdname='temp - 2m AGL;'
   cdunits='F'

elseif(cvar(1:lv).eq.'tempc2m') then
   ivar_type=2
   ierr=RAMS_getvar('UP',idim_type,ngrd,c,b,flnm)
   ierr=RAMS_getvar('VP',idim_type,ngrd,d,b,flnm)
   call RAMS_comp_speed(n1,n2,n3,c,d)
   ierr=RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
   ierr=RAMS_getvar('PI',idim_type,ngrd,f,b,flnm)
   ierr=RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   allocate (pv1(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv2(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv3(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv4(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv5(nnxp(ngrd),nnyp(ngrd),npatch))
   ierr=RAMS_getvar('USTAR'      ,idim_type,ngrd,pv1,b,flnm)
   ierr=RAMS_getvar('PATCH_ROUGH',idim_type,ngrd,pv2,b,flnm)
   ierr=RAMS_getvar('CAN_TEMP'   ,idim_type,ngrd,pv3,b,flnm)
   ierr=RAMS_getvar('PATCH_AREA' ,idim_type,ngrd,pv4,b,flnm)
   ierr=RAMS_getvar('TSTAR'      ,idim_type,ngrd,pv5,b,flnm)

    call RAMS_reduced_temp(nnxp(ngrd),nnyp(ngrd),nnzp(ngrd),npatch       &
                          ,a,c,pv1,pv5  &
			  ,2.,ztn(2,ngrd),pv2,pv4,pv3,d,f,e  &
                          ,zmn(nnzp(1)-1,1))
   deallocate (pv1,pv2,pv3,pv4,pv5)
   call RAMS_comp_tempK(n1,n2,1,a,f) !converte de temp. potencial para
                                     !temperatura termodinamica
   call RAMS_comp_tempC(n1,n2,1,a)
   cdname='temp - 2m AGL;'
   cdunits='C'

!
elseif(cvar(1:lv).eq.'tempc5m') then
   ivar_type=2
   ierr=RAMS_getvar('UP',idim_type,ngrd,c,b,flnm)
   ierr=RAMS_getvar('VP',idim_type,ngrd,d,b,flnm)
   call RAMS_comp_speed(n1,n2,n3,c,d)
   ierr=RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
   ierr=RAMS_getvar('PI',idim_type,ngrd,f,b,flnm)
   ierr=RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   allocate (pv1(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv2(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv3(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv4(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv5(nnxp(ngrd),nnyp(ngrd),npatch))
   ierr=RAMS_getvar('USTAR'      ,idim_type,ngrd,pv1,b,flnm)
   ierr=RAMS_getvar('PATCH_ROUGH',idim_type,ngrd,pv2,b,flnm)
   ierr=RAMS_getvar('CAN_TEMP'   ,idim_type,ngrd,pv3,b,flnm)
   ierr=RAMS_getvar('PATCH_AREA' ,idim_type,ngrd,pv4,b,flnm)
   ierr=RAMS_getvar('TSTAR'      ,idim_type,ngrd,pv5,b,flnm)

    call RAMS_reduced_temp(nnxp(ngrd),nnyp(ngrd),nnzp(ngrd),npatch       &
                          ,a,c,pv1,pv5  &
			  ,5.,ztn(2,ngrd),pv2,pv4,pv3,d,f,e  &
                          ,zmn(nnzp(1)-1,1))
   deallocate (pv1,pv2,pv3,pv4,pv5)
   call RAMS_comp_tempK(n1,n2,1,a,f) !converte de temp. potencial para
                                     !temperatura termodinamica
   call RAMS_comp_tempC(n1,n2,1,a)
   cdname='temp - 5m AGL;'
   cdunits='C'

!
elseif(cvar(1:lv).eq.'rv2m') then

stop ' not working yet'

   ivar_type=2
   ierr=RAMS_getvar('UP',idim_type,ngrd,c,b,flnm)
   ierr=RAMS_getvar('VP',idim_type,ngrd,d,b,flnm)
   call RAMS_comp_speed(n1,n2,n3,c,d)
   ierr=RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
   ierr=RAMS_getvar('PI',idim_type,ngrd,f,b,flnm)
   ierr=RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   allocate (pv1(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv2(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv3(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv4(nnxp(ngrd),nnyp(ngrd),npatch))
   allocate (pv5(nnxp(ngrd),nnyp(ngrd),npatch))
   ierr=RAMS_getvar('USTAR'      ,idim_type,ngrd,pv1,b,flnm)
   ierr=RAMS_getvar('PATCH_ROUGH',idim_type,ngrd,pv2,b,flnm)
   ierr=RAMS_getvar('CAN_RVAP'   ,idim_type,ngrd,pv3,b,flnm)
   ierr=RAMS_getvar('PATCH_AREA' ,idim_type,ngrd,pv4,b,flnm)
   ierr=RAMS_getvar('RSTAR'      ,idim_type,ngrd,pv5,b,flnm)

    call RAMS_reduced_temp(nnxp(ngrd),nnyp(ngrd),nnzp(ngrd),npatch       &
                          ,a,c,pv1,pv5  &
			  ,2.,ztn(2,ngrd),pv2,pv4,pv3,d,f,e  &
                          ,zmn(nnzp(1)-1,1))
   deallocate (pv1,pv2,pv3,pv4,pv5)
   cdname='RV - 2m AGL;'
   cdunits='C'




elseif(cvar(1:lv).eq.'speed10m') then

   ivar_type=2
   ierr=RAMS_getvar('UP',idim_type,ngrd  &
         ,c,b,flnm)
   ierr=RAMS_getvar('VP',idim_type,ngrd  &
         ,d,b,flnm)
   call RAMS_comp_speed(n1,n2,n3,c,d)
   ierr=RAMS_getvar('THETA',idim_type,ngrd  &
         ,d,b,flnm)
   ierr=RAMS_getvar('PI',idim_type,ngrd  &
         ,f,b,flnm)
!        Get topo
   ierr= RAMS_getvar('TOPT',idim_type,ngrd  &
         ,e,b,flnm)
   allocate (pv1(nnxp(ngrd),nnyp(ngrd),npatch) )
   allocate (pv2(nnxp(ngrd),nnyp(ngrd),npatch) )
   allocate (pv3(nnxp(ngrd),nnyp(ngrd),npatch) )
   allocate (pv4(nnxp(ngrd),nnyp(ngrd),npatch) )

!           Get ustar
   ierr = RAMS_getvar('USTAR',idim_type,ngrd,pv1,b,flnm)
!           Get net roughness
   ierr = RAMS_getvar('PATCH_ROUGH',idim_type,ngrd   &
        ,pv2,b,flnm)
!           Get patch canopy temperature
   ierr = RAMS_getvar('CAN_TEMP',idim_type,ngrd   &
        ,pv3,b,flnm)
!           Get % coverage
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,pv4,b,flnm)
   call RAMS_reduced_wind(nnxp(ngrd),nnyp(ngrd)  &
      ,nnzp(ngrd),npatch  &
      ,a,c,pv1,10.,ztn(2,ngrd)  &
      ,pv2,pv4,pv3,d,f,e  &
      ,zmn(nnzp(1)-1,1))

   deallocate (pv1,pv2,pv3,pv4)
   cdname='speed - 10m AGL'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'direction') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_dir(n1,n2,n3,a,c,ngrd)
   cdname='direction'
   cdunits='deg'

elseif(cvar(1:lv).eq.'relvortx') then
   ivar_type=3
   ierr= RAMS_getvar('VP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('WP',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,d,b,flnm)
   call RAMS_comp_relvortx(n1,n2,n3,a,c,b,d,ngrd)
   cdname='x-vorticity'
   cdunits='rad/s'

elseif(cvar(1:lv).eq.'relvorty') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('WP',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,d,b,flnm)
   call RAMS_comp_relvorty(n1,n2,n3,a,c,b,d,ngrd)
   cdname='y-vorticity'
   cdunits='rad/s'

elseif(cvar(1:lv).eq.'relvortz') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,d,b,flnm)
   call RAMS_comp_relvortz(n1,n2,n3,a,c,b,d,ngrd)
   cdname='relative z-vorticity'
   cdunits='rad/s'

elseif(cvar(1:lv).eq.'absvortz') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,d,b,flnm)
   call RAMS_comp_totvortz(n1,n2,n3,a,c,b,d,ngrd)
   cdname='absolute z-vorticity'
   cdunits='rad/s'

elseif(cvar(1:lv).eq.'potvortz') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('VP',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,d,b,flnm)
   call RAMS_comp_totvortz(n1,n2,n3,a,c,b,d,ngrd)
   call RAMS_comp_dn0(n1,n2,n3,e,b,c,d,ngrd)

   ierr= RAMS_getvar('THETA',idim_type,ngrd,b,e,flnm)
   call RAMS_comp_potvortz(n1,n2,n3,a,b,c,e,d,ngrd)
   cdname='potential z-vorticity'
   cdunits='rad/s'

elseif(cvar(1:lv).eq.'horiz_div') then
   ivar_type=3
   ierr= RAMS_getvar('WP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_horizdiv(n1,n2,n3,a)
   cdname='horizontal divergence'
   cdunits='/s'

! 3D THERMODYNAMIC PROPERTIES OF AIR

elseif(cvar(1:lv).eq.'pi') then
   ivar_type=3
   ierr= RAMS_getvar('PI',idim_type,ngrd,a,b,flnm)
   cdname='Exner function'
   cdunits='J/(kg K)'

elseif(cvar(1:lv).eq.'press') then
   ivar_type=3
   ierr= RAMS_getvar('PI',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_press(n1,n2,n3,a)
   cdname='pressure'
   cdunits='mb'

elseif(cvar(1:lv).eq.'theta') then
   ivar_type=3
   ierr= RAMS_getvar('THETA',idim_type,ngrd,a,b,flnm)
   cdname='potential temp'
   cdunits='K'

elseif(cvar(1:lv).eq.'dn0') then
   ivar_type=3
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,c,b,a,e,ngrd)
   cdname='ref density'

   cdunits='kg/m3'

elseif(cvar(1:lv).eq.'pi0') then
   ivar_type=3
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,a,b,c,e,ngrd)
   cdname='ref Exner func'
   cdunits='J/(kg K)'

elseif(cvar(1:lv).eq.'th0') then
   ivar_type=3
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,a,c,e,ngrd)
   cdname='reference virtual potential temp'
   cdunits='K'

elseif(cvar(1:lv).eq.'pert_pres') then
   ivar_type=3
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,c,a,b,e,ngrd)
   ierr= RAMS_getvar('PI',idim_type,ngrd,a,b,flnm)
   if (ierr.eq.0) call RAMS_comp_ppress(n1,n2,n3,a,c)
   cdname='pert pressure'
   cdunits='mb'

elseif(cvar(1:lv).eq.'dnp') then
   ivar_type=3
   ierr= RAMS_getvar('DNP',idim_type,ngrd,a,b,flnm)
   cdname='true air density'
   cdunits='kg/m3'


elseif(cvar(1:lv).eq.'tempk') then
   ivar_type=3
   ierr= RAMS_getvar('THETA',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,a,c)
   cdname='temperature'
   cdunits='K'

elseif(cvar(1:lv).eq.'tempc') then
   ivar_type=3
   ierr= RAMS_getvar('THETA',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,a,c)
   call RAMS_comp_tempC(n1,n2,n3,a)
   cdname='temperature'
   cdunits='C'


elseif(cvar(1:lv).eq.'tempf') then
   ivar_type=3
   ierr= RAMS_getvar('THETA',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,a,c)
   call RAMS_comp_tempF(n1,n2,n3,a)
   cdname='temperature'
   cdunits='F'

!<Demerval

elseif(cvar(1:lv).eq.'theta_e') then
   ivar_type=3
   ierr= RAMS_getvar('RV',idim_type,ngrd,e,b,flnm)
!!  e=rvap
!
   ierr= RAMS_getvar('THETA',idim_type,ngrd,f1,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,f1,c)
!
!! f1=tempk
!
   ivar_type=3
   ierr= RAMS_getvar('RV',idim_type,ngrd,f2,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
!
   call RAMS_comp_rh(n1,n2,n3,f2,c,d)
!
!
!! f2=umidade relativa %
!
   ierr= RAMS_getvar('THETA',idim_type,ngrd,a,b,flnm)
!! a=theta
!
   call RAMS_comp_thete(n1,n2,n3,a,e,f1,f2)
   cdname='Equiv pot temp;'
   cdunits='K'


!elseif(cvar(1:lv).eq.'theta_e') then
!   ivar_type=3
!   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
!   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
!   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
!
!   call RAMS_comp_thete(n1,n2,n3,a,c,d)
!   cdname='equiv pot temp'
!   cdunits='K'

!Demerval>

elseif(cvar(1:lv).eq.'theta_v') then
   ivar_type=3
   ierr= RAMS_getvar('THETA',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('RV',idim_type,ngrd,c,b,flnm)

   call RAMS_comp_thetv(n1,n2,n3,a,c)
   cdname='virtual pot temp'
   cdunits='K'

! 3D MOISTURE MASS MIXING RATIOS AND HUMIDITY

elseif(cvar(1:lv).eq.'rv') then
   ivar_type=3
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='vapor mix ratio'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'cloud') then
   ivar_type=3
   ierr= RAMS_getvar('RCP',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='cloud mix ratio'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'rei') then
   ivar_type=3
   ierr= RAMS_getvar('REI',idim_type,ngrd,a,b,flnm)
   cdname='effective radius ice'
   cdunits='micrometer'

elseif(cvar(1:lv).eq.'rel') then
   ivar_type=3
   ierr= RAMS_getvar('REL',idim_type,ngrd,a,b,flnm)
   cdname='effective radius liq'
   cdunits='micrometer'

elseif(cvar(1:lv).eq.'rain') then
   ivar_type=3
   ierr= RAMS_getvar('RRP',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='rain mix ratio'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'pristine') then
   ivar_type=3
   ierr= RAMS_getvar('RPP',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='pristine mix ratio'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'snow') then
   ivar_type=3
   ierr= RAMS_getvar('RSP',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='snow mix ratio'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'aggregates') then
   ivar_type=3
   ierr= RAMS_getvar('RAP',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='aggregate mix ratio'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'graupel') then
   ivar_type=3
   ierr= RAMS_getvar('RGP',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='graupel mix ratio'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'hail') then
   ivar_type=3
   ierr= RAMS_getvar('RHP',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='hail mix ratio'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'liquid') then
   ivar_type=3
   call RAMS_comp_zero(n1,n2,n3,a)
   ierr= RAMS_getvar('RCP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RRP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)

   ierr= RAMS_getvar('RGP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) then
      ierr= RAMS_getvar('Q6',idim_type,ngrd,d,b,flnm)
      if(ierr.eq.0) then
         call RAMS_comp_fracliq(n1,n2,n3,d)
         call RAMS_comp_mult(n1,n2,n3,c,d)
      endif
      call RAMS_comp_accum(n1,n2,n3,a,c)
   endif

   ierr= RAMS_getvar('RHP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) then
      ierr= RAMS_getvar('Q7',idim_type,ngrd,d,b,flnm)
      if(ierr.eq.0) then
         call RAMS_comp_fracliq(n1,n2,n3,d)
         call RAMS_comp_mult(n1,n2,n3,c,d)
      endif
      call RAMS_comp_accum(n1,n2,n3,a,c)
    endif

   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='liquid mix ratio'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'ice') then
   ivar_type=3
   call RAMS_comp_zero(n1,n2,n3,a)
   ierr= RAMS_getvar('RPP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RSP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RAP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)

   ierr= RAMS_getvar('RGP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) then
      ierr= RAMS_getvar('Q6',idim_type,ngrd,d,b,flnm)
      if(ierr.eq.0) then
         call RAMS_comp_fracice(n1,n2,n3,d)
         call RAMS_comp_mult(n1,n2,n3,c,d)
      endif
      call RAMS_comp_accum(n1,n2,n3,a,c)
   endif

   ierr= RAMS_getvar('RHP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) then
      ierr= RAMS_getvar('Q7',idim_type,ngrd,d,b,flnm)
      if(ierr.eq.0) then
         call RAMS_comp_fracliq(n1,n2,n3,d)
         call RAMS_comp_mult(n1,n2,n3,c,d)
      endif
      call RAMS_comp_accum(n1,n2,n3,a,c)
   endif

   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='ice mix ratio'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'total_cond') then
   ivar_type=3
   call RAMS_comp_zero(n1,n2,n3,a)
   ierr= RAMS_getvar('RCP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RRP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RPP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RSP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RAP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RGP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RHP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)

   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='cloud mix ratio'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'rtotal') then
   ivar_type=3
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('RCP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RRP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RPP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RSP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RAP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RGP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RHP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)

   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='total mix ratio'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'rtotal_orig') then
   ivar_type=3
   ierr= RAMS_getvar('RTP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='orig rtotal'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'dewptk') then
   ivar_type=3
   ivar_type=3
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)

   call RAMS_comp_dewK(n1,n2,n3,a,c,d)
   call RAMS_comp_tempK(n1,n2,n3,a,c)
   cdname='dewpoint temp'
   cdunits='K'

elseif(cvar(1:lv).eq.'dewptf') then
   ivar_type=3
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)

   call RAMS_comp_dewK(n1,n2,n3,a,c,d)
   call RAMS_comp_tempF(n1,n2,n3,a)
   cdname='dewpoint temp'
   cdunits='F'

elseif(cvar(1:lv).eq.'dewptc') then
   ivar_type=3
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)

   call RAMS_comp_dewK(n1,n2,n3,a,c,d)
   call RAMS_comp_tempC(n1,n2,n3,a)
   cdname='dewpoint temp'
   cdunits='C'

elseif(cvar(1:lv).eq.'rh') then
   ivar_type=3
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)

   call RAMS_comp_rh(n1,n2,n3,a,c,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='relative humidity'
   cdunits='pct'

elseif(cvar(1:lv).eq.'rhsfc') then
   ivar_type=2
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)

   call RAMS_comp_rh(n1,n2,n3,a,c,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_2d(n1,n2,n3,a)
   cdname='1st layer relative humidity 40 m'
   cdunits='pct'

elseif(cvar(1:lv).eq.'tempcsfc') then
   ivar_type=2
   ierr= RAMS_getvar('THETA',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,a,c)
   call RAMS_comp_tempC(n1,n2,n3,a)
   call RAMS_comp_2d(n1,n2,n3,a)
   cdname='1st layer temperature - 40 m'
   cdunits='C'

elseif(cvar(1:lv).eq.'clear_frac') then
   ivar_type=2
   ierr= RAMS_getvar('RV',idim_type,ngrd,b,a,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,a,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,a,flnm)

   call RAMS_comp_rh(n1,n2,n3,b,c,d)
   call RAMS_comp_noneg(n1,n2,n3,b)

   call cldfraction(n1,n2,n3,a,c,b)

   cdname='clear sky'
   cdunits='frac'

! 3D HYDROMETEOR, CCN, CN, Dep N, AND NONHYGROSCOPIC AEROSOL NUMBER CONCEN

elseif(cvar(1:lv).eq.'cloud_concen_mg') then
   ivar_type=3
! variable 18 is iccp
   ierr= RAMS_getvar('CCP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='cloud concen'
   cdunits='#/mg'

elseif(cvar(1:lv).eq.'rain_concen_kg') then
   ivar_type=3
   ierr= RAMS_getvar('CRP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='rain concen'
   cdunits='#/kg'

elseif(cvar(1:lv).eq.'pris_concen_kg') then
   ivar_type=3
   ierr= RAMS_getvar('CPP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='pristine concen'
   cdunits='#/kg'

elseif(cvar(1:lv).eq.'snow_concen_kg') then
   ivar_type=3
   ierr= RAMS_getvar('CSP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='snow concen'
   cdunits='#/kg'

elseif(cvar(1:lv).eq.'agg_concen_kg') then
   ivar_type=3
   ierr= RAMS_getvar('CAP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='aggregate concen'
   cdunits='#/kg'

elseif(cvar(1:lv).eq.'graup_concen_kg') then
   ivar_type=3
   ierr= RAMS_getvar('CGP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='graupel concen'
   cdunits='#/kg'

elseif(cvar(1:lv).eq.'hail_concen_kg') then
   ivar_type=3
   ierr= RAMS_getvar('CHP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='hail concen'
   cdunits='#/kg'

elseif(cvar(1:lv).eq.'cloud_concen_cm3') then
   ivar_type=3
   ierr= RAMS_getvar('CCP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='cloud concen'
   cdunits='#/cm3'

elseif(cvar(1:lv).eq.'rain_concen_m3') then
   ivar_type=3
   ierr= RAMS_getvar('CRP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='rain concen'
   cdunits='#/m3'

elseif(cvar(1:lv).eq.'pris_concen_m3') then
   ivar_type=3
   ierr= RAMS_getvar('CPP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='pristine concen'
   cdunits='#/m3'

elseif(cvar(1:lv).eq.'snow_concen_m3') then
   ivar_type=3
   ierr= RAMS_getvar('CSP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='snow concen'
   cdunits='#/m3'

elseif(cvar(1:lv).eq.'agg_concen_m3') then
   ivar_type=3
   ierr= RAMS_getvar('CAP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='aggregates concen'
   cdunits='#/m3'

elseif(cvar(1:lv).eq.'graup_concen_m3') then
   ivar_type=3
   ierr= RAMS_getvar('CGP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='graupel concen'
   cdunits='#/m3'

elseif(cvar(1:lv).eq.'hail_concen_m3') then
   ivar_type=3
   ierr= RAMS_getvar('CHP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='hail concen'
   cdunits='#/m3'

elseif(cvar(1:lv).eq.'ccn_concen') then
   ivar_type=3
   ierr= RAMS_getvar('CCCNP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)
   cdname='ccn1 concen'
   cdunits='#/mg'

elseif(cvar(1:lv).eq.'ifn_conc') then
   ivar_type=3
   ierr= RAMS_getvar('CIFNP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='CN mix ratio'
   cdunits='#/kg'

! 3D HYDROMETEOR DIAMETERS

elseif(cvar(1:lv).eq.'cloud_diam') then
   ivar_type=3
   ierr= RAMS_getvar('RCP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CCP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_hydrodiam(n1,n2,n3,a,c,cfmas(1),pwmas(1))
   call RAMS_comp_mults(n1,n2,n3,a,1.e6)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='cloud diam'
   cdunits='microns'

elseif(cvar(1:lv).eq.'rain_diam') then
   ivar_type=3
   ierr= RAMS_getvar('RRP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CRP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_hydrodiam(n1,n2,n3,a,c,cfmas(2),pwmas(2))
   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='rain diam'
   cdunits='mm'

elseif(cvar(1:lv).eq.'pris_diam') then
   ivar_type=3
   ierr= RAMS_getvar('RPP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CPP',idim_type,ngrd,c,b,flnm)
! more general case: write habit to anal file for cfmas & pwmas index
   call RAMS_comp_hydrodiam(n1,n2,n3,a,c,cfmas(3),pwmas(3))
   call RAMS_comp_mults(n1,n2,n3,a,1.e6)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='pristine diam'
   cdunits='microns'

elseif(cvar(1:lv).eq.'snow_diam') then
   ivar_type=3
   ierr= RAMS_getvar('RSP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CSP',idim_type,ngrd,c,b,flnm)
! more general case: write habit to anal file for cfmas & pwmas index
   call RAMS_comp_hydrodiam(n1,n2,n3,a,c,cfmas(4),pwmas(4))
   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='snow diam'
   cdunits='mm'

elseif(cvar(1:lv).eq.'agg_diam') then
   ivar_type=3
   ierr= RAMS_getvar('RAP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CAP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_hydrodiam(n1,n2,n3,a,c,cfmas(5),pwmas(5))
   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='aggregates diam'
   cdunits='mm'

elseif(cvar(1:lv).eq.'graup_diam') then
   ivar_type=3
   ierr= RAMS_getvar('RGP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CGP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_hydrodiam(n1,n2,n3,a,c,cfmas(6),pwmas(6))
   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='graupel diam'
   cdunits='mm'

elseif(cvar(1:lv).eq.'hail_diam') then
   ivar_type=3
   ierr= RAMS_getvar('RHP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CHP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_hydrodiam(n1,n2,n3,a,c,cfmas(7),pwmas(7))
   call RAMS_comp_mults(n1,n2,n3,a,1.e3)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='hail diam'
   cdunits='mm'

! 3D HYDROMETEOR TEMPERATURE, THERMAL ENERGY, LIQUID WATER FRACTION

elseif(cvar(1:lv).eq.'q2') then
   ivar_type=3
   ierr= RAMS_getvar('Q2',idim_type,ngrd,a,b,flnm)
   cdname='q2'
   cdunits='J/kg'

elseif(cvar(1:lv).eq.'q6') then
   ivar_type=3
   ierr= RAMS_getvar('Q6',idim_type,ngrd,a,b,flnm)
   cdname='q6'
   cdunits='J/kg'

elseif(cvar(1:lv).eq.'q7') then
   ivar_type=3
   ierr= RAMS_getvar('Q7',idim_type,ngrd,a,b,flnm)
   cdname='q7'
   cdunits='J/kg'

elseif(cvar(1:lv).eq.'rain_temp') then
   ivar_type=3
   ierr= RAMS_getvar('Q2',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_raintemp(n1,n2,n3,a)
   cdname='rain temperature'
   cdunits='K'

elseif(cvar(1:lv).eq.'graup_temp') then
   ivar_type=3
   ierr= RAMS_getvar('Q6',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_qtcpcp(n1,n2,n3,a)
   cdname='graupel temperature'
   cdunits='C'

elseif(cvar(1:lv).eq.'hail_temp') then
   ivar_type=3
   ierr= RAMS_getvar('Q7',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_qtcpcp(n1,n2,n3,a)
   cdname='hail temperature'
   cdunits='C'

elseif(cvar(1:lv).eq.'rain_air_tempdif') then
   ivar_type=3
   ierr= RAMS_getvar('Q2',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_raintemp(n1,n2,n3,a)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,d,c)
   call RAMS_comp_tempC(n1,n2,n3,d)
   call RAMS_comp_subt(n1,n2,n3,a,d)
   cdname='rain-air temp'
   cdunits='K'

elseif(cvar(1:lv).eq.'graup_air_tempdf') then
   ivar_type=3
   ierr= RAMS_getvar('Q6',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_qtcpcp(n1,n2,n3,a)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,d,c)
   call RAMS_comp_tempC(n1,n2,n3,d)
   call RAMS_comp_subt(n1,n2,n3,a,d)
   cdname='graupel-air temp'
   cdunits='K'

elseif(cvar(1:lv).eq.'hail_air_tempdif') then
   ivar_type=3
   ierr= RAMS_getvar('Q7',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_qtcpcp(n1,n2,n3,a)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,d,c)
   call RAMS_comp_tempC(n1,n2,n3,d)
   call RAMS_comp_subt(n1,n2,n3,a,d)
   cdname='hail-air temp'
   cdunits='K'

elseif(cvar(1:lv).eq.'graup_fracliq') then
   ivar_type=3
   ierr= RAMS_getvar('Q6',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_fracliq(n1,n2,n3,a)
   cdname='graupel liq frac'
   cdunits=' '

elseif(cvar(1:lv).eq.'hail_fracliq') then
   ivar_type=3
   ierr= RAMS_getvar('Q7',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_fracliq(n1,n2,n3,a)
   cdname='hail liq frac'
   cdunits=' '

! 3D MISCELLANEOUS FIELDS

elseif(cvar(1:lv).eq.'geo') then
   ivar_type=3
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_z(n1,n2,n3,a,c,ngrd)
   cdname='geopotential height'
   cdunits='m'

elseif(cvar(1:lv).eq.'tke') then
   ivar_type=3
   ierr= RAMS_getvar('TKEP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='turb kinetic energy'
   cdunits='m2/s2'
!-------------
!trace gases

elseif(cvar(1:lv).eq.'CO2_old') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP001',idim_type,ngrd,a,b,flnm)
!   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='CO2 Concentration'
   cdunits='ppm'

!     ; etc. for scalars

elseif(cvar(1:lv).eq.'TKUO') then
   ivar_type=3
   ierr= RAMS_getvar('DUM3',idim_type,ngrd,a,b,flnm)
   call RAMS_transf_ppb_day(n1,n2,n3,a)
   cdname='CO tend conc due conv trans'
   cdunits='ppb/day'

elseif(cvar(1:lv).eq.'cuthsh1') then
   ivar_type=3
   ierr= RAMS_getvar('THSRCSH',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   cdname='shallow conv heat rate for nnshcu =1'
   cdunits='K/day'

elseif(cvar(1:lv).eq.'curtsh1') then
   ivar_type=3
   ierr= RAMS_getvar('RTSRCSH',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   call RAMS_comp_mults(n1,n2,n3,a,1000.)
   cdname='shallow conv moist rate for nnshcu =1'
   cdunits='g/kg/day'

elseif(cvar(1:lv).eq.'cuthsh') then
   ivar_type=3
   ierr= RAMS_getvar('THSRC_SH',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   cdname='shallow conv heat rate'
   cdunits='K/day'

elseif(cvar(1:lv).eq.'curtsh') then
   ivar_type=3
   ierr= RAMS_getvar('RTSRC_SH',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   call RAMS_comp_mults(n1,n2,n3,a,1000.)
   cdname='shallow conv moist rate'
   cdunits='g/kg/day'


elseif(cvar(1:lv).eq.'cuthdp') then
   ivar_type=3
   ierr= RAMS_getvar('THSRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   cdname='deep conv heat rate'
   cdunits='K/day'

elseif(cvar(1:lv).eq.'mup') then
   ivar_type=3
   ierr= RAMS_getvar('MUP',idim_type,ngrd,a,b,flnm)
   cdname='deep and shallow mass flux'
   cdunits='kg/m2/s'

elseif(cvar(1:lv).eq.'cuudp') then
   ivar_type=3
   ierr= RAMS_getvar('USRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   cdname='deep conv momentum U rate'
   cdunits='m/s/day'

elseif(cvar(1:lv).eq.'cuvdp') then
   ivar_type=3
   ierr= RAMS_getvar('VSRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   cdname='deep conv momentum V rate'
   cdunits='m/s/day'

elseif(cvar(1:lv).eq.'curtdp') then
   ivar_type=3
   ierr= RAMS_getvar('RTSRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   call RAMS_comp_mults(n1,n2,n3,a,1000.)
   cdname='deep conv moist rate'
   cdunits='g/kg/day'

elseif(cvar(1:lv).eq.'cucldp') then
   ivar_type=3
   ierr= RAMS_getvar('CLSRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   call RAMS_comp_mults(n1,n2,n3,a,1000.)
   cdname='deep conv cloud/ice mass rate'
   cdunits='g/kg/day'

elseif(cvar(1:lv).eq.'ttens') then
   ivar_type=3
   ierr= RAMS_getvar('TTENS',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   cdname='env sub heating rate'
   cdunits='K/day'

elseif(cvar(1:lv).eq.'forc2') then
   ivar_type=3
   ierr= RAMS_getvar('QVTTENS',idim_type,ngrd,a,b,flnm)
   cdname='forc2'
   cdunits=' '

elseif(cvar(1:lv).eq.'qvttens') then
   ivar_type=3
   ierr= RAMS_getvar('QVTTENS',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   call RAMS_comp_mults(n1,n2,n3,a,1000.)
   cdname='env sub drying rate'
   cdunits='g/kg/day'

elseif(cvar(1:lv).eq.'curidp') then
   ivar_type=3
   ierr= RAMS_getvar('d3005',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   call RAMS_comp_mults(n1,n2,n3,a,1000.)
   cdname='conv liquid/ice rate'
   cdunits='g/kg/day'
elseif(cvar(1:lv).eq.'thpin') then
   ivar_type=3
   ierr= RAMS_getvar('d3004',idim_type,ngrd,a,b,flnm)
   cdname='thp before advmnt'
   cdunits='K'
elseif(cvar(1:lv).eq.'thpout') then
   ivar_type=3
   ierr= RAMS_getvar('d3005',idim_type,ngrd,a,b,flnm)
   cdname='thp after advmnt'
   cdunits='K'
elseif(cvar(1:lv).eq.'forc') then
   ivar_type=3
   ierr= RAMS_getvar('d3001',idim_type,ngrd,a,b,flnm)
   cdname='g3d forc'
   cdunits=' '

!----
elseif(cvar(1:lv).eq.'trad') then
   ivar_type=3
   ierr= RAMS_getvar('d3001',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   cdname='rad tend'
   cdunits='K/day'
elseif(cvar(1:lv).eq.'tpbl') then
   ivar_type=3
   ierr= RAMS_getvar('d3002',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   cdname='pbl tend'
   cdunits='K/day'
elseif(cvar(1:lv).eq.'ttot') then
   ivar_type=3
   ierr= RAMS_getvar('d3004',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   cdname='total tend'
   cdunits='K/day'

elseif(cvar(1:lv).eq.'theta2') then
   ivar_type=3
   ierr= RAMS_getvar('d3003',idim_type,ngrd,a,b,flnm)
   cdname='theta timestep'
   cdunits='K'





elseif(cvar(1:lv).eq.'qs1') then
   ivar_type=3
   ierr= RAMS_getvar('d3002',idim_type,ngrd,a,b,flnm)
   cdname='entr'
   cdunits='m-1'
elseif(cvar(1:lv).eq.'qsr') then
   ivar_type=3
   ierr= RAMS_getvar('d3003',idim_type,ngrd,a,b,flnm)
   cdname='entr'
   cdunits='m-1'
elseif(cvar(1:lv).eq.'x1') then
   ivar_type=3
   ierr= RAMS_getvar('d3006',idim_type,ngrd,a,b,flnm)
   cdname='entr'
   cdunits='m-1'
elseif(cvar(1:lv).eq.'x2') then
   ivar_type=3
   ierr= RAMS_getvar('d3007',idim_type,ngrd,a,b,flnm)
   cdname='entr'
   cdunits='m-1'
elseif(cvar(1:lv).eq.'cd') then
   ivar_type=3
   ierr= RAMS_getvar('d3003',idim_type,ngrd,a,b,flnm)
   cdname='entr'
   cdunits='m-1'

elseif(cvar(1:lv).eq.'fthrd') then
   ivar_type=3
   ierr= RAMS_getvar('FTHRD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   cdname='rad heat rate'
   cdunits='K/day'

elseif(cvar(1:lv).eq.'khh') then
   ivar_type=3
   ierr= RAMS_getvar('HKH',idim_type,ngrd,a,b,flnm)
   cdname='horiz diffusion coeff'
   cdunits='m2/s'

elseif(cvar(1:lv).eq.'khv') then
   ivar_type=3
   ierr= RAMS_getvar('VKH',idim_type,ngrd,a,b,flnm)
   cdname='vert diffusion coeff'
   cdunits='m2/s'

! 2D SURFACE PRECIPITATION

!      elseif(cvar(1:lv).eq.'accpc') then
!         ivar_type=2
!         ierr= RAMS_getvar('ACCPC',idim_type,ngrd,a,b,flnm)
!         cdname='accum fog precip'
!         cdunits='kg/m2'

elseif(cvar(1:lv).eq.'accpr') then
   ivar_type=2
   ierr= RAMS_getvar('ACCPR',idim_type,ngrd,a,b,flnm)
   cdname='accum rain'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'accpp') then
   ivar_type=2
   ierr= RAMS_getvar('ACCPP',idim_type,ngrd,a,b,flnm)
   cdname='accum pristine'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'accps') then
   ivar_type=2
   ierr= RAMS_getvar('ACCPS',idim_type,ngrd,a,b,flnm)
   cdname='accum snow'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'accpa') then
   ivar_type=2
   ierr= RAMS_getvar('ACCPA',idim_type,ngrd,a,b,flnm)
   cdname='accum aggregates'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'accpg') then
   ivar_type=2
   ierr= RAMS_getvar('ACCPG',idim_type,ngrd,a,b,flnm)
   cdname='accum graupel'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'accph') then
   ivar_type=2
   ierr= RAMS_getvar('ACCPH',idim_type,ngrd,a,b,flnm)
   cdname='accum hail'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'totpcp' .or. cvar(1:lv).eq.'totpcp_in' .or.  &
       cvar(1:lv).eq.'precip' .or. cvar(1:lv).eq.'precip_in') then
   ivar_type=2
   call RAMS_comp_zero(n1,n2,1,a)

   ierr= RAMS_getvar('ACCPR',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   !- GThompson microphysics : ACCPR is already the total precip
   print*,"mcphys_type=",mcphys_type
   if(mcphys_type .le. 1) then
     ierr= RAMS_getvar('ACCPP',idim_type,ngrd,c,b,flnm)
     if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
     ierr= RAMS_getvar('ACCPS',idim_type,ngrd,c,b,flnm)
     if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
     ierr= RAMS_getvar('ACCPA',idim_type,ngrd,c,b,flnm)
     if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
     ierr= RAMS_getvar('ACCPG',idim_type,ngrd,c,b,flnm)
     if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
     ierr= RAMS_getvar('ACCPH',idim_type,ngrd,c,b,flnm)
     if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   endif

   if (cvar(1:lv).eq.'precip'.or.cvar(1:lv).eq.'precip_in') then
      ierr= RAMS_getvar('ACONPR',idim_type,ngrd,c,b,flnm)
      if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
      cdname='total accum precip'
   else
      cdname='total resolved precip'
   endif

   if(cvar(1:lv).eq.'totpcp'.or.cvar(1:lv).eq.'precip') then
      cdunits='mm liq'
   else
      call RAMS_comp_mults(n1,n2,n3,a,.03937)
      cdunits='in liq'
   endif
   call RAMS_comp_noneg(n1,n2,1,a)

elseif(cvar(1:lv).eq.'totpcp_gt' .or. cvar(1:lv).eq.'totpcp_in_gt' .or.  &
       cvar(1:lv).eq.'precip_gt' .or. cvar(1:lv).eq.'precip_in_gt') then
   ivar_type=2
   call RAMS_comp_zero(n1,n2,1,a)
   ierr= RAMS_getvar('ACCPR',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)

   if (cvar(1:lv).eq.'precip_gt'.or.cvar(1:lv).eq.'precip_in_gt') then
      ierr= RAMS_getvar('ACONPR',idim_type,ngrd,c,b,flnm)
      if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
      cdname='total accum precip'
   else
      cdname='total resolved precip'
   endif

   if(cvar(1:lv).eq.'totpcp_gt'.or.cvar(1:lv).eq.'precip_gt') then
      cdunits='mm liq'
   else
      call RAMS_comp_mults(n1,n2,n3,a,.03937)
      cdunits='in liq'
   endif
   call RAMS_comp_noneg(n1,n2,1,a)

elseif(cvar(1:lv).eq.'pcprr') then
   ivar_type=2
   ierr= RAMS_getvar('PCPRR',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,3600.)
   cdname='rain precip rate'
   cdunits='mm/hr liq equiv'

elseif(cvar(1:lv).eq.'pcprp') then
   ivar_type=2
   ierr= RAMS_getvar('PCPRP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,3600.)
   cdname='pristine precip rate'
   cdunits='mm/hr liq equiv'

elseif(cvar(1:lv).eq.'psprs') then
   ivar_type=2
   ierr= RAMS_getvar('PCPRS',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,3600.)
   cdname='snow precip rate'
   cdunits='mm/hr liq equiv'

elseif(cvar(1:lv).eq.'pcpra') then
   ivar_type=2
   ierr= RAMS_getvar('PCPRA',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,3600.)
   cdname='aggregates precip rate'
   cdunits='mm/hr liq equiv'

elseif(cvar(1:lv).eq.'pcprg') then
   ivar_type=2
   ierr= RAMS_getvar('PCPRG',idim_type,ngrd,a,b,flnm)
   cdname='graupel precip rate'
   cdunits='mm/hr liq equiv'

elseif(cvar(1:lv).eq.'pcprh') then
   ivar_type=2
   ierr= RAMS_getvar('PCPRH',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,3600.)
   cdname='hail precip rate'
   cdunits='mm/hr liq equiv'

elseif(cvar(1:lv).eq.'pcpg') then
   ivar_type=2
   ierr= RAMS_getvar('PCPG',idim_type,ngrd,a,b,flnm)
   cdname='pcpg'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'qpcpg') then
   ivar_type=2
   ierr= RAMS_getvar('QPCPG',idim_type,ngrd,a,b,flnm)
   cdname='qpcpg'
   cdunits='J/m2'

elseif(cvar(1:lv).eq.'dpcpg') then
   ivar_type=2
   ierr= RAMS_getvar('DPCPG',idim_type,ngrd,a,b,flnm)
   cdname='dpdpg'
   cdunits='m'

elseif(cvar(1:lv).eq.'pcprate'.or.cvar(1:lv).eq.'pcprate_in'.or.  &
       cvar(1:lv).eq.'precipr'.or.cvar(1:lv).eq.'precipr_in') then
   ivar_type=2
   call RAMS_comp_zero(n1,n2,1,a)
   ierr= RAMS_getvar('PCPRR',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   !- GThompson microphysics : ACCPR is already the total precip
   print*,"mcphys_type=",mcphys_type
   if(mcphys_type .le. 1) then
    ierr= RAMS_getvar('PCPRS',idim_type,ngrd,c,b,flnm)
    if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
    ierr= RAMS_getvar('PCPRA',idim_type,ngrd,c,b,flnm)
    if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
    ierr= RAMS_getvar('PCPRG',idim_type,ngrd,c,b,flnm)
    if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
    ierr= RAMS_getvar('PCPRH',idim_type,ngrd,c,b,flnm)
    if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
    ierr= RAMS_getvar('PCPRP',idim_type,ngrd,c,b,flnm)
    if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   endif
   call RAMS_comp_noneg(n1,n2,1,a)

   if (cvar(1:lv).eq.'precipr'.or.cvar(1:lv).eq.'precipr_in') then
      ierr= RAMS_getvar('CONPRR',idim_type,ngrd,c,b,flnm)
      if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
      cdname='total precip rate'
   else
      cdname='resolved precip rate'
   endif

   if(cvar(1:lv).eq.'pcprate'.or.cvar(1:lv).eq.'precipr') then
      call RAMS_comp_mults(n1,n2,1,a,3600.)
      cdunits='mm/hr'
   elseif(cvar(1:lv).eq.'pcprate_in'.or.cvar(1:lv).eq.'precipr_in') then
      call RAMS_comp_mults(n1,n2,1,a,141.732)
      cdunits='in/hr'
   endif

elseif(cvar(1:lv).eq.'pcprate_gt'.or.cvar(1:lv).eq.'pcprate_in_gt'.or.  &
       cvar(1:lv).eq.'precipr_gt'.or.cvar(1:lv).eq.'precipr_in_gt') then
   ivar_type=2
   call RAMS_comp_zero(n1,n2,1,a)
   ierr= RAMS_getvar('PCPRR',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
   !- GThompson microphysics : ACCPR is already the total precip
   call RAMS_comp_noneg(n1,n2,1,a)

   if (cvar(1:lv).eq.'precipr_gt'.or.cvar(1:lv).eq.'precipr_in_gt') then
      ierr= RAMS_getvar('CONPRR',idim_type,ngrd,c,b,flnm)
      if(ierr.eq.0) call RAMS_comp_accum(n1,n2,1,a,c)
      cdname='total precip rate'
   else
      cdname='resolved precip rate'
   endif

   if(cvar(1:lv).eq.'pcprate_gt'.or.cvar(1:lv).eq.'precipr_gt') then
      call RAMS_comp_mults(n1,n2,1,a,3600.)
      cdunits='mm/hr'
   elseif(cvar(1:lv).eq.'pcprate_in_gt'.or.cvar(1:lv).eq.'precipr_in_gt') then
      call RAMS_comp_mults(n1,n2,1,a,141.732)
      cdunits='in/hr'
   endif

elseif(cvar(1:lv).eq.'conpcp') then
   ivar_type=2
   ierr= RAMS_getvar('CONPRR',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,3600.)
   call RAMS_comp_noneg(n1,n2,1,a)
   cdname='convective pcp rate'
   cdunits='mm/hr'

elseif(cvar(1:lv).eq.'acccon') then
   ivar_type=2
   ierr= RAMS_getvar('ACONPR',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,1,a)
   cdname='accum convective pcp'
   cdunits='mm'


elseif(cvar(1:lv).eq.'orage') then
   ivar_type=3
   ierr= RAMS_getvar('d3010',idim_type,ngrd,a,b,flnm)
   !call D3toD2(n1,n2,n3,2,a,c)
   cdname='orage'
   cdunits='NO emiss by orage'

elseif(cvar(1:lv).eq.'o3ddep') then
   ivar_type=2
   ierr= RAMS_getvar('d2006',idim_type,ngrd,a,b,flnm)
   !call D3toD2(n1,n2,n3,2,a,c)
   cdname='ozone dry dep vel'
   cdunits='m/s'


elseif(cvar(1:lv).eq.'cape') then
   ivar_type=2
!- rel hum (e)
   ierr= RAMS_getvar('RV',idim_type,ngrd,e,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)

   call RAMS_comp_rh(n1,n2,n3,e,c,d)
   call RAMS_comp_noneg(n1,n2,n3,e)
!- tempk (d)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,d,c)
!- press (c)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_press(n1,n2,n3,c)
!- cape
   call cape_cine(n1,n2,n3,c,d,e,a,'cape',-9.99e33)

   cdname='cape'
   cdunits='J/kg'

elseif(cvar(1:lv).eq.'cine') then
   ivar_type=2
!- rel hum (e)
   ierr= RAMS_getvar('RV',idim_type,ngrd,e,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)

   call RAMS_comp_rh(n1,n2,n3,e,c,d)
   call RAMS_comp_noneg(n1,n2,n3,e)
!- tempk (d)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,d,c)
!- press (c)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_press(n1,n2,n3,c)
!- cape
   call cape_cine(n1,n2,n3,c,d,e,a,'cine',-9.99e33)

   cdname='cine'
   cdunits='J/kg'





! Vertically-integrated atmospheric moisture

elseif(cvar(1:lv).eq.'vertint_rt' .or. cvar(1:lv).eq.'vertint_cond') then
   ivar_type=2

   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,c,b,d,e,ngrd)

   if (cvar(1:lv).eq.'vertint_rt') then
      ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
      cdname='vertint total water'
   else
      call RAMS_comp_zero(n1,n2,n3,a)
      cdname='vertint condensate'
   endif

   ierr= RAMS_getvar('RCP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RRP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RPP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RSP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RAP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RGP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RHP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)

   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_vertint(n1,n2,n3,a,e,ngrd)

   cdunits='mm'


! 2D SURFACE HEAT, MOISTURE, MOMENTUM AND RADIATIVE FLUXES

elseif(cvar(1:lv).eq.'SFLUX_T') then
   ivar_type=2
   ierr= RAMS_getvar('SFLUX_T',idim_type,ngrd,a,b,flnm)
   cdname='SFLUX_T'
   cdunits='m'

elseif(cvar(1:lv).eq.'SFLUX_R') then
   ivar_type=2
   ierr= RAMS_getvar('SFLUX_R',idim_type,ngrd,a,b,flnm)
   cdname='SFLUX_R'
   cdunits='m'

elseif(cvar(1:lv).eq.'uw') then
   ivar_type=2
   ierr= RAMS_getvar('UW',idim_type,ngrd,a,b,flnm)
   cdname='uw'
   cdunits='m'

elseif(cvar(1:lv).eq.'vw') then
   ivar_type=2
   ierr= RAMS_getvar('VW',idim_type,ngrd,a,b,flnm)
   cdname='vw'
   cdunits='m'

elseif(cvar(1:lv).eq.'SFLUX_W') then
   ivar_type=2
   ierr= RAMS_getvar('SFLUX_W',idim_type,ngrd,a,b,flnm)
   cdname='SFLUX_W'
   cdunits='m'

elseif(cvar(1:lv).eq.'h') then
   ivar_type=2
   ierr= RAMS_getvar('SFLUX_T',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,1,a,d)
   call RAMS_comp_mults(n1,n2,1,a,1004.)
   cdname='sfc sens heat flx'
   cdunits='W/m2'

elseif(cvar(1:lv).eq.'le') then
   ivar_type=2
   ierr= RAMS_getvar('SFLUX_R',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,1,a,d)
   call RAMS_comp_mults(n1,n2,1,a,2.5e6)
   cdname='sfc lat heat flx'
   cdunits='W/m2'

elseif(cvar(1:lv).eq.'fvap') then
   ivar_type=2
   ierr= RAMS_getvar('SFLUX_R',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,1,a,d)
   call RAMS_comp_mults(n1,n2,1,a,1000.)
   cdname='sfc water flx'
   cdunits='g/m2 s'

elseif(cvar(1:lv).eq.'etrans') then
   ivar_type=2
   ierr= RAMS_getvar('SFLUX_R',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,1,a,d)
!                 Divide by water density to get depth and
!                   convert units from m/s to mm/hour (3600./1000.)
   call RAMS_comp_mults(n1,n2,1,a,3.6)
   cdname='evapo-transpiration'
   cdunits='mm/hour'

elseif(cvar(1:lv).eq.'etrans_in') then
   ivar_type=2
   ierr= RAMS_getvar('SFLUX_R',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,1,a,d)
!                 Divide by water density to get depth and
!                   convert units from m/s to in/hour (39.37 * 3600./1000.)
   call RAMS_comp_mults(n1,n2,n3,a,141.732)
   cdname='evapo-transpiration'
   cdunits='in/hour'

elseif(cvar(1:lv).eq.'umom_flx') then
   ivar_type=2
   ierr= RAMS_getvar('UW',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,1,a,d)
   cdname='sfc u-momentum flx'
   cdunits='Pa'

elseif(cvar(1:lv).eq.'vmom_flx') then
   ivar_type=2
   ierr= RAMS_getvar('VW',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,1,a,d)
   cdname='sfc v-momentum flx'
   cdunits='Pa'

elseif(cvar(1:lv).eq.'wmom_flx') then
   ivar_type=2
   ierr= RAMS_getvar('SFLUX_W',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,1,a,d)
   cdname='sfc w-momentum flx'
   cdunits='Pa'

elseif(cvar(1:lv).eq.'bowen') then
   ivar_type=2
   ierr= RAMS_getvar('SFLUX_T',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('SFLUX_R',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_bowen(n1,n2,1,a,c)
   cdname='bowen ratio'
   cdunits=' '

elseif(cvar(1:lv).eq.'rshort') then
   ivar_type=2
   ierr= RAMS_getvar('RSHORT',idim_type,ngrd,a,b,flnm)
   cdname='rshort'
   cdunits='W/m2'

elseif(cvar(1:lv).eq.'rlong') then
   ivar_type=2
   ierr= RAMS_getvar('RLONG',idim_type,ngrd,a,b,flnm)
   cdname='rlong'
   cdunits='W/m2'

elseif(cvar(1:lv).eq.'rlongup') then
   ivar_type=2
   ierr= RAMS_getvar('RLONGUP',idim_type,ngrd,a,b,flnm)
   cdname='rlongup'
   cdunits='W/m2'

elseif(cvar(1:lv).eq.'albedt') then
   ivar_type=2
   ierr= RAMS_getvar('ALBEDT',idim_type,ngrd,a,b,flnm)
   cdname='albedt'
   cdunits=' '

!2D misc
elseif(cvar(1:lv).eq.'qsc1') then
   ivar_type=2
   ierr= RAMS_getvar('DUM1',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,2,a,c)
   cdname='qsc1'
   cdunits='????'
elseif(cvar(1:lv).eq.'cpmax') then
   ivar_type=2
   ierr= RAMS_getvar('d3009',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,2,a,c)
   cdname='cpmax'
   cdunits='????'

elseif(cvar(1:lv).eq.'wu') then
   ivar_type=3
   ierr= RAMS_getvar('d3009',idim_type,ngrd,a,b,flnm)
   !call D3toD2(n1,n2,n3,2,a,c)
   cdname='Wu'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'enup') then
   ivar_type=3
   ierr= RAMS_getvar('d3010',idim_type,ngrd,a,b,flnm)
   !call D3toD2(n1,n2,n3,2,a,c)
   cdname='enup'
   cdunits=' '
elseif(cvar(1:lv).eq.'endn') then
   ivar_type=3
   ierr= RAMS_getvar('d3011',idim_type,ngrd,a,b,flnm)
   !call D3toD2(n1,n2,n3,2,a,c)
   cdname='endn'
   cdunits=' '
elseif(cvar(1:lv).eq.'deup') then
   ivar_type=3
   ierr= RAMS_getvar('d3012',idim_type,ngrd,a,b,flnm)
   !call D3toD2(n1,n2,n3,2,a,c)
   cdname='endn'
   cdunits=' '
elseif(cvar(1:lv).eq.'dedn') then
   ivar_type=3
   ierr= RAMS_getvar('d3013',idim_type,ngrd,a,b,flnm)
   !call D3toD2(n1,n2,n3,2,a,c)
   cdname='endn'
   cdunits=' '
elseif(cvar(1:lv).eq.'zup') then
   ivar_type=3
   ierr= RAMS_getvar('d3014',idim_type,ngrd,a,b,flnm)
   !call D3toD2(n1,n2,n3,2,a,c)
   cdname='endn'
   cdunits=' '
elseif(cvar(1:lv).eq.'zdn') then
   ivar_type=3
   ierr= RAMS_getvar('d3015',idim_type,ngrd,a,b,flnm)
   !call D3toD2(n1,n2,n3,2,a,c)
   cdname='endn'
   cdunits=' '

!DSM{
!--- Variaveis 2D do JULES
elseif(cvar(1:lv).eq.'t2mJ') then
   ivar_type=2
   ierr= RAMS_getvar('T2MJ',idim_type,ngrd,a,b,flnm)
   call undef (n1,n2,n3,a,1,193.0,353.0)

   cdname='temperature at 1.5m height'
   cdunits='K'
elseif(cvar(1:lv).eq.'rv2mJ') then
   ivar_type=2
   ierr= RAMS_getvar('RV2MJ',idim_type,ngrd,a,b,flnm)
   cdname='rv at 2.0m height'
   cdunits='kg kg-1'
elseif(cvar(1:lv).eq.'u10mJ') then
   ivar_type=2
   ierr= RAMS_getvar('U10MJ',idim_type,ngrd,a,b,flnm)
   call undef (n1,n2,n3,a,1,-300.0,300.0)

   cdname='westerly wind component at 10 m height'
   cdunits='m s-1'

elseif(cvar(1:lv).eq.'v10mJ') then
   ivar_type=2
   ierr= RAMS_getvar('V10MJ',idim_type,ngrd,a,b,flnm)
   call undef (n1,n2,n3,a,1,-300.0,300.0)

   cdname='southerly wind component at 10m heightt'
   cdunits='m s-1'

elseif(cvar(1:lv).eq.'cs') then
   ivar_type=2
   ierr= RAMS_getvar('CSJ',idim_type,ngrd,a,b,flnm)
   cdname='Soil carbon'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'npp') then
   ivar_type=2
   ierr= RAMS_getvar('NPP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,8.333e7)
   cdname='net primary productivity'
   cdunits='umC/m2/s'

elseif(cvar(1:lv).eq.'gpp') then
   ivar_type=2
   ierr= RAMS_getvar('GPP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,8.333e7)
   cdname='gross primary productivity'
   cdunits='umC/m2/s'

elseif(cvar(1:lv).eq.'respp') then
   ivar_type=2
   ierr= RAMS_getvar('RESP_P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,8.333e7)
   cdname='plant respiration'
   cdunits='umC/m2/s'

elseif(cvar(1:lv).eq.'resps') then
   ivar_type=2
   ierr= RAMS_getvar('RESP_S',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,1,a,8.333e7)
   cdname='soil respiration (total)'
   cdunits='umC/m2/s'


elseif(cvar(1:lv).eq.'td2mJ') then
   ivar_type=2

   !--- rv_2.0m ---
      ierr= RAMS_getvar('RV2MJ',idim_type,ngrd,a,b,flnm)
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)

   !--- tempk2m ---
      ierr= RAMS_getvar('T2MJ',idim_type,ngrd,c,b,flnm)

   !--- PI ---
      ierr=RAMS_getvar('PI',idim_type,ngrd,f,b,flnm)

   call RAMS_comp_dewK_2m(n1,n2,n3,a,f,c)
   call RAMS_comp_tempC(n1,n2,n3,a)

   call undef (n1,n2,n3,a,1,-80.0,80.0)

   cdname='dewpoint temp at 2.0m height'
   cdunits='C'


!DSM}


! 2D TOPOGRAPHY AND GEOGRAPHIC VALUES

elseif(cvar(1:lv).eq.'topoa') then
   ivar_type=2
   ierr= RAMS_getvar('TOPTA',idim_type,ngrd,a,b,flnm)
   cdname='topo'
   cdunits='m'

elseif(cvar(1:lv).eq.'topo') then
   ivar_type=2
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,a,b,flnm)
   cdname='topo'
   cdunits='m'

elseif(cvar(1:lv).eq.'lat') then
   ivar_type=2
   ierr= RAMS_getvar('GLAT',idim_type,ngrd,a,b,flnm)
   cdname='latitude'
   cdunits='deg'

elseif(cvar(1:lv).eq.'fmapu') then
   ivar_type=2
   ierr= RAMS_getvar('FMAPU',idim_type,ngrd,a,b,flnm)
   cdname='FMAPU'
   cdunits=' '

elseif(cvar(1:lv).eq.'fmapv') then
   ivar_type=2
   ierr= RAMS_getvar('FMAPV',idim_type,ngrd,a,b,flnm)
   cdname='FMAPV'
   cdunits=' '

elseif(cvar(1:lv).eq.'lon') then
   ivar_type=2
   ierr= RAMS_getvar('GLON',idim_type,ngrd,a,b,flnm)
   cdname='longitude'
   cdunits='deg'

! 2D MISCELLANEOUS FIELDS

elseif(cvar(1:lv).eq.'slp_rams') then
   ivar_type=2
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_z(n1,n2,n3,c,a,ngrd)

   ierr= RAMS_getvar('PI',idim_type,ngrd,d,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,a,b,flnm)

   call RAMS_comp_slpress(n1,n2,n3,a,d,c,a)
   cdname='sea level pressure'
   cdunits='mb'

elseif(cvar(1:lv).eq.'slp_brams') then

  ivar_type=2
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_z(n1,n2,n3,c,e,ngrd)

   ierr= RAMS_getvar('PI',idim_type,ngrd,d,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,f,b,flnm)

   call BRAMS_comp_slpress(n1,n2,n3,f,d,c,a,e,ztn(1,ngrd))
   cdname='sea level pressure'
   cdunits='mb'

elseif(cvar(1:lv).eq.'slp') then
   ivar_type=2
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,d,b,flnm)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,e,b,flnm)
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)

   call RAMS_comp_thetv(n1,n2,n3,e,a)

! get terrain in c(1+ioffc)
!         ierr= RAMS_getvar(1,ind,nind,2,ngrd,c(1+ioffc),b,flnm)
! get Exner function in d(1+ioffd)
!         ierr= RAMS_getvar(7,ind,nind,3,ngrd,d(1+ioffd),b,flnm)
! get theta in 1+ioffe
!         ierr= RAMS_getvar(32,ind,nind,3,ngrd,e(1+ioffe),b,flnm)

         call RAMS_comp_slpmm5(n1,n2,n3,e,d,c,a)
         cdname='sea level pressure;'
         cdunits='mb;'

elseif(cvar(1:lv).eq.'sea_press') then
   ivar_type=2
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,d,b,flnm)

   ierr= RAMS_getvar('THETA',idim_type,ngrd,e,b,flnm)
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)

   call RAMS_comp_thetv(n1,n2,n3,e,a)

   call RAMS_comp_slpmm5(n1,n2,n3,e,d,c,a)
   cdname='sea level pressure;'
   cdunits='mb;'

elseif(cvar(1:lv).eq.'sfc_press') then
   ivar_type=2
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_press_sfc(n1,n2,n3,c,a)
   cdname='sfc pressure'
   cdunits='mb'

elseif(cvar(1:lv).eq.'sfc_div') then
   ivar_type=2
   ierr= RAMS_getvar('WP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_sfcdiv(n1,n2,n3,a,ngrd)
   cdname='surface divergence'
   cdunits='1/s'

!DSM{
elseif(cvar(1:lv).eq.'slp_metar') then
   ivar_type=2
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,d,b,flnm)
   call RAMS_comp_press(n1,n2,n3,d)

   ierr= RAMS_getvar('THETA',idim_type,ngrd,a,b,flnm)

   call RAMS_comp_tempK(n1,n2,n3,a,d)

   call RAMS_comp_slp_metar(n1,n2,n3,d,c,a,ztn(2,ngrd))
   cdname='sea level pressure -  METAR formulation'
   cdunits='mb'
!DSM}
! Special use of sst: acquired for patch #1 even where no water exists

elseif(cvar(1:lv).eq.'sst') then
   ivar_type=2

   ierr = RAMS_getvar('SOIL_ENERGY',idim_type,ngrd   &
        ,c,b,flnm)
   kp = nzg
   call rams_fill_sst(n1,n2,nzg*npatch,kp,a,c)

!   call RAMS_comp_tempC(n1,n2,1,a)
   cdname='water temperature'
   cdunits='C'


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! LEAF2 variables section

! If want a horiz plot, specify a string like 'tgpatch'; it will
!   return i,j,ip array.
! Specify a new ivar_type, not corresponding to anal file var type.  With
!   horiz plot, get back into iplt.  If have this var type, don't slice.
! Need replacement for rams3to2d because windowing is done in there.
! Replacement would window but not slice.
! Then, if want xz (vert cross section) have name like tgpatch_vert.
! This would return entire 4d array from hvlib.f.
! Then we have to slice and window with yet another replacement to rams3to2d.

! nkk is the record number, where n is the LEAF field number (1, 2, 3, or 4)
! and kk is the k level.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


elseif(cvar(1:lv).eq.'pfarea') then

   ivar_type = 7
   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   cdname='patch fractional area'
   cdunits=''

elseif(cvar(1:lv).eq.'soil_z0_p' .or. cvar(1:lv).eq.'soil_z0_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('SOIL_Z0',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'soil_z0_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='soil roughness'
   cdunits='m'

elseif(cvar(1:lv).eq.'vtype' .or. cvar(1:lv).eq.'veg_class_bp') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!       ,a(irecind),b,flnm)

!   irecind = irecind + irecsize
!!!!
   ierr = RAMS_getvar('LEAF_CLASS',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_comp_vegclass(irecsize,1,1,a(irecind))
   if (cvar(1:lv).eq.'vtype') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_bigpatch(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif
   cdname='vegetation class'
   cdunits='#'

elseif(cvar(1:lv).eq.'ndvi') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!       ,a(irecind),b,flnm)

!   irecind = irecind + irecsize
!!!!
   ierr = RAMS_getvar('VEG_NDVIC',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if (cvar(1:lv).eq.'ndvi') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_bigpatch(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='ndvi'
   cdunits='#'


elseif(cvar(1:lv).eq.'qveg_class_p' .or. cvar(1:lv).eq.'qveg_class_bp') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('DATQ_CLASS',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_comp_vegclass(irecsize,1,1,a(irecind))

   if (cvar(1:lv).eq.'qveg_class_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_bigpatch(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='q vegetation class'
   cdunits='#'

elseif(cvar(1:lv).eq.'vegfrac' .or. cvar(1:lv).eq.'veg_fracarea_ps')then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!
!   irecind = irecind + irecsize
!!!
   ierr = RAMS_getvar('VEG_FRACAREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'vegfrac') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='vegetation frac area'
   cdunits=''

elseif(cvar(1:lv).eq.'land') then

   ivar_type = 2
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_1minus(nnxp(ngrd),nnyp(ngrd),1,a)
   cdname='land frac area'
   cdunits=''


elseif(cvar(1:lv).eq.'grsize') then
    ivar_type=2
    call get_grid_size(n1,n2,n3,a,ngrd)
    cdname='grid size'
    cdunits='m'

elseif(cvar(1:lv).eq.'dxt') then
    ivar_type=2
    call get_grid_sizeX(n1,n2,n3,a,ngrd)
    cdname='grid size dxt'
    cdunits='m'

elseif(cvar(1:lv).eq.'dyt') then
    ivar_type=2
    call get_grid_sizeY(n1,n2,n3,a,ngrd)
    cdname='grid size DYT'
    cdunits='m'

elseif(cvar(1:lv).eq.'lai' .or. cvar(1:lv).eq.'veg_lai_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!
!   irecind = irecind + irecsize
!!!
   ierr = RAMS_getvar('VEG_LAI',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'lai') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='green leaf area index'
   cdunits=''


elseif(cvar(1:lv).eq.'tai' .or. cvar(1:lv).eq.'veg_tai_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!
!   irecind = irecind + irecsize
!!!
   ierr = RAMS_getvar('VEG_TAI',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'tai') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname=' total leaf area index'
   cdunits=''



elseif(cvar(1:lv).eq.'net_z0_p' .or. cvar(1:lv).eq.'net_z0_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('NET_Z0',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'net_z0_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='net roughness'
   cdunits='m'

elseif(cvar(1:lv).eq.'vegz0' .or. cvar(1:lv).eq.'veg_z0_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!
!   irecind = irecind + irecsize
!!!

   ierr = RAMS_getvar('VEG_ROUGH',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'vegz0') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='vegetation roughness'
   cdunits='m'

elseif(cvar(1:lv).eq.'vegdisp' .or. cvar(1:lv).eq.'veg_disp_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!
!   irecind = irecind + irecsize
!!!
   ierr = RAMS_getvar('VEG_DISP',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'vegdisp') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='vegetation displacement height'
   cdunits='m'

elseif(cvar(1:lv).eq.'patch_wetind') then

   ivar_type = 7
   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('WET_INDEX',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   cdname='patch wetness index'
   cdunits=''

elseif(cvar(1:lv).eq.'snowlevels') then

   ivar_type = 7
   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('KSNOW',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   cdname='number of snow levels'
   cdunits='#'

elseif(cvar(1:lv).eq.'grnd_mixrat_p' .or. cvar(1:lv).eq.'grnd_mixrat_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('SFC_RS',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_comp_mults(n1,n2,npatch,a(irecind),1.e3)

   if(cvar(1:lv).eq.'grnd_mixrat_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='ground mixing ratio'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'soil_mixrat_p' .or. cvar(1:lv).eq.'soil_mixrat_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('SOIL_RS',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_comp_mults(n1,n2,npatch,a(irecind),1.e3)

   if(cvar(1:lv).eq.'soil_mixrat_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='soil mixing ratio'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'veg_moist_p' .or. cvar(1:lv).eq.'veg_moist_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('VEG_MOIST',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'veg_moist_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

    cdname='vegetation moisture'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'canopy_mixrat_p' .or. cvar(1:lv).eq.'canopy_mixrat_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('CAN_RV',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_comp_mults(n1,n2,npatch,a(irecind),1.e3)

   if(cvar(1:lv).eq.'canopy_mixrat_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='canopy mixing ratio'
   cdunits='g/kg'

elseif(cvar(1:lv).eq.'tveg' .or. cvar(1:lv).eq.'veg_temp_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!
!   irecind = irecind + irecsize
!!!
   ierr = RAMS_getvar('VEG_TEMP',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_comp_tempC(n1,n2,npatch,a(irecind))

   if(cvar(1:lv).eq.'tveg') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='vegetation temperature'
   cdunits='C'

elseif(cvar(1:lv).eq.'tcan' .or. cvar(1:lv).eq.'canopy_temp_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!
!   irecind = irecind + irecsize
!!!
   ierr = RAMS_getvar('CAN_TEMP',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_comp_tempC(n1,n2,npatch,a(irecind))

   if(cvar(1:lv).eq.'tcan') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='canopy temperature'
   cdunits='C'

! sib - stuffs
!itb...src_co2
elseif(cvar(1:lv).eq.'src_co2') then
   ivar_type=2
   ierr= RAMS_getvar('SRC_CO2',idim_type,ngrd,a,b,flnm)
   if(ierr.eq.0) then
      call RAMS_comp_mults(n1,n2,n3,a,1.e6)
      call RAMS_comp_noneg(n1,n2,n3,a)
   endif
   cdname='CO2 flux'
   cdunits='umol/m**2/sec'

  elseif(cvar(1:lv).eq.'CO2_SIB') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP001',idim_type,ngrd,a,b,flnm)
   cdname='CO2 Concentration'
   cdunits='ppm'



elseif(cvar(1:lv).eq.'pco2ap' ) then
   ivar_type=2
   ierr= RAMS_getvar('pco2ap',idim_type,ngrd,a,b,flnm)
    cdname='CAS CO2'
   cdunits='Pa'

elseif(cvar(1:lv).eq.'pco2m' ) then
   ivar_type=2
   ierr= RAMS_getvar('pco2m',idim_type,ngrd,a,b,flnm)
    cdname='REF LEVEL CO2'
   cdunits='Pa'


!itb...
elseif(cvar(1:lv).eq.'rst') then
   ivar_type=2
   ierr= RAMS_getvar('rst',idim_type,ngrd,a,b,flnm)
   cdname='stomatal resistance'
   cdunits='sec/meter'

!...itb - NEW DIAGNOSTICS...

elseif(cvar(1:lv).eq.'fss') then
   ivar_type=2
   ierr= RAMS_getvar('fss',idim_type,ngrd,a,b,flnm)
   cdname='sensible heat flux'
   cdunits='W/m^2'

elseif(cvar(1:lv).eq.'fws') then
   ivar_type=2
   ierr= RAMS_getvar('fws',idim_type,ngrd,a,b,flnm)
   cdname='latent heat flux'
   cdunits='kg H2O/m^2/sec'

elseif(cvar(1:lv).eq.'assimn') then
   ivar_type=2
   ierr= RAMS_getvar('assimn',idim_type,ngrd,a,b,flnm)
   cdname='canopy net assimilation'
   cdunits='mol/m^2/sec'

elseif(cvar(1:lv).eq.'respg') then
   ivar_type=2
   ierr= RAMS_getvar('respg',idim_type,ngrd,a,b,flnm)
   cdname='ground respiration'
   cdunits='mol/m^2/sec'

elseif(cvar(1:lv).eq.'rstfac1') then
   ivar_type=2
   ierr= RAMS_getvar('rstfac1',idim_type,ngrd,a,b,flnm)
   cdname='stress factor 1-leaf to CAS humidity'
   cdunits='(-)'

elseif(cvar(1:lv).eq.'rstfac2') then
   ivar_type=2
   ierr= RAMS_getvar('rstfac2',idim_type,ngrd,a,b,flnm)
   cdname='stress factor 2-soil moisture'
   cdunits='(-)'

elseif(cvar(1:lv).eq.'rstfac3') then
   ivar_type=2
   ierr= RAMS_getvar('rstfac3',idim_type,ngrd,a,b,flnm)
   cdname='stress factor 3-temperature'
   cdunits='(-)'

elseif(cvar(1:lv).eq.'rstfac4') then
   ivar_type=2
   ierr= RAMS_getvar('rstfac4',idim_type,ngrd,a,b,flnm)
   cdname='stress factor 4-combination of factors 1-3'
   cdunits='(-)'

elseif(cvar(1:lv).eq.'ect') then
   ivar_type=2
   ierr= RAMS_getvar('ect',idim_type,ngrd,a,b,flnm)
   cdname='canopy transpiration'
   cdunits='W/m^2'

elseif(cvar(1:lv).eq.'eci') then
   ivar_type=2
   ierr= RAMS_getvar('eci',idim_type,ngrd,a,b,flnm)
   cdname='canopy interception evaporation'
   cdunits='W/m^2'

elseif(cvar(1:lv).eq.'egi') then
   ivar_type=2
   ierr= RAMS_getvar('egi',idim_type,ngrd,a,b,flnm)
   cdname='ground interception evaporation'
   cdunits='W/m^2'

elseif(cvar(1:lv).eq.'egs') then
   ivar_type=2
   ierr= RAMS_getvar('egs',idim_type,ngrd,a,b,flnm)
   cdname='top soil layer evaporation'
   cdunits='W/m^2'

elseif(cvar(1:lv).eq.'hc') then
   ivar_type=2
   ierr= RAMS_getvar('hc',idim_type,ngrd,a,b,flnm)
   cdname='canopy sensible heat flux'
   cdunits='W/m^2'

elseif(cvar(1:lv).eq.'hg') then
   ivar_type=2
   ierr= RAMS_getvar('hg',idim_type,ngrd,a,b,flnm)
   cdname='ground sensible heat flux'
   cdunits='W/m^2'


elseif(cvar(1:lv).eq.'capac1' ) then
   ivar_type=2
   ierr= RAMS_getvar('capac1',idim_type,ngrd,a,b,flnm)
   cdname='VEGETATION INTERCEPTION STORE'
   cdunits='kg/m^2'


elseif(cvar(1:lv).eq.'capac2'.or. cvar(1:lv).eq.'capac2_ps' ) then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!
!   irecind = irecind + irecsize
!!!
   ierr = RAMS_getvar('capac2',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'capac2') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   call RAMS_comp_noneg(n1,n2,n3,a)

   cdname='GROUND INTERCEPTION STORE'
   cdunits='kg/m^2'



elseif(cvar(1:lv).eq.'ustar' .or. cvar(1:lv).eq.'ustar_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!
!   irecind = irecind + irecsize
!!!
   ierr = RAMS_getvar('USTAR',idim_type,ngrd,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'ustar') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='ustar'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'tstar' .or. cvar(1:lv).eq.'tstar_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!
!   irecind = irecind + irecsize
!!!
   ierr = RAMS_getvar('TSTAR',idim_type,ngrd,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'tstar') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='tstar'
   cdunits='K'

elseif(cvar(1:lv).eq.'rstar' .or. cvar(1:lv).eq.'rstar_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!
!   irecind = irecind + irecsize
!!!
   ierr = RAMS_getvar('RSTAR',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if(cvar(1:lv).eq.'rstar') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='rstar'
   cdunits='kg/kg'

elseif(cvar(1:lv).eq.'hp' .or.  &
       cvar(1:lv).eq.'sens_heat_flux_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!
!!!   irecind = irecind + irecsize
   ierr = RAMS_getvar('USTAR',idim_type,ngrd,a(irecind),b,flnm)
   ierr = RAMS_getvar('TSTAR',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_mult(n1,n2,npatch,a(irecind),c)

   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_multap(n1,n2,1,npatch,a(irecind),d)
   call RAMS_comp_mults(n1,n2,npatch,a(irecind),-1004.)

   if(cvar(1:lv).eq.'hp') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='sfc sens heat flx'
   cdunits='W/m2'

elseif(cvar(1:lv).eq.'lep' .or.  &
       cvar(1:lv).eq.'lat_heat_flux_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!
!   irecind = irecind + irecsize
!!!
   ierr = RAMS_getvar('USTAR',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   ierr = RAMS_getvar('RSTAR',idim_type,ngrd   &
        ,c,b,flnm)
   call RAMS_comp_mult(n1,n2,npatch,a(irecind),c)

   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_multap(n1,n2,1,npatch,a(irecind),d)
   call RAMS_comp_mults(n1,n2,npatch,a(irecind),-2.5e6)

   if(cvar(1:lv).eq.'lep') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='sfc lat heat flx'
   cdunits='W/m2'

elseif(cvar(1:lv).eq.'snow_depth_p' .or. cvar(1:lv).eq.'snow_depth_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('SNOW_DEPTH',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_sum_snowlayers(nnxp(ngrd)*nnyp(ngrd),nzs,npatch,a(irecind))

   if(cvar(1:lv).eq.'snow_depth_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='snow depth'
   cdunits='m'

elseif(cvar(1:lv).eq.'snowcover_p' .or. cvar(1:lv).eq.'snowcover_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('SNOW_MOIST',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call RAMS_sum_snowlayers(nnxp(ngrd)*nnyp(ngrd),nzs,npatch,a(irecind))

   if(cvar(1:lv).eq.'snow_depth_p') then
      ivar_type = 7
   else
      ivar_type = 2
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),1,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='snowcover'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'sltex_p' .or. cvar(1:lv).eq.'sltex_bp') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   irecind = irecind + irecsize
   ierr = RAMS_getvar('SOIL_TEXT',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   if (cvar(1:lv).eq.'sltex_p') then
      ivar_type = 8
   else
      ivar_type = 3
      call RAMS_comp_bigpatch(nnxp(ngrd),nnyp(ngrd),nzg,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='soil textural class'
   cdunits='#'

elseif(cvar(1:lv).eq.'soilq' .or. cvar(1:lv).eq.'soilq_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!   irecind = irecind + irecsize
!!!
   ierr = RAMS_getvar('SOIL_ENERGY',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   call get_leaf_soil(n1,n2,n3,n4,n5,a(irecind),a2)

   if (cvar(1:lv).eq.'soilq') then
      ivar_type = 8
   else
      ivar_type = 3
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),nzg,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='soil q'
   cdunits='J/m3'

elseif(cvar(1:lv).eq.'tsoil' .or. cvar(1:lv).eq.'soil_temp_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!   irecind = irecind + irecsize
!!!
   ierr = RAMS_getvar('SOIL_ENERGY',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   ierr = RAMS_getvar('SOIL_WATER',idim_type,ngrd   &
        ,c,b,flnm)
   ierr = RAMS_getvar('SOIL_TEXT',idim_type,ngrd   &
        ,d,b,flnm)
   call RAMS_comp_copysst(n1,n2,nzg,a(irecind))

   irecsizep = nnxp(ngrd) * nnyp(ngrd) * nzg
   call RAMS_comp_qwtc(n1,n2,nzg*(npatch-1),a(irecind+irecsizep)  &
      ,c(1+irecsizep),d(1+irecsizep))

   call get_leaf_soil(n1,n2,n3,n4,n5,a(irecind),a2)

   if (cvar(1:lv).eq.'tsoil') then
      ivar_type = 8
   else
      ivar_type = 3
      call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),nzg,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='soil/sea temp'
   cdunits='C'

elseif(cvar(1:lv).eq.'5050_temp_ps' .or. cvar(1:lv).eq.'5050_tempf_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   ierr = RAMS_getvar('CAN_TEMP',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   ivar_type = 2
   call RAMS_comp_patchsum(nnxp(ngrd),nnyp(ngrd),1,npatch  &
      ,a(irecind),a(1),b)

   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,d,c)
   call RAMS_comp_5050(n1,n2,n3,a,d)

   if(cvar(1:lv).eq.'5050_temp_ps') then
      call RAMS_comp_tempC(n1,n2,n3,a)
      cdname='5050 tempC'
      cdunits='C'
   else
      call RAMS_comp_tempF(n1,n2,n3,a)
      cdname='5050 tempF'
      cdunits='F'
   endif


elseif(cvar(1:lv).eq.'smoist' .or. cvar(1:lv).eq.'SOIL_WATER_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!   irecind = irecind + irecsize
!!!
   ierr = RAMS_getvar('SOIL_WATER',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   call get_leaf_soil(n1,n2,n3,n4,n5,a(irecind),a2)

   if (cvar(1:lv).eq.'smoist') then
      ivar_type = 8
   else
      ivar_type = 3
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),nzg,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='soil moisture'
   cdunits='m3/m3'

elseif(cvar(1:lv).eq.'stext' .or. cvar(1:lv).eq.'stext_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
!!!
!   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
!        ,a(irecind),b,flnm)
!   irecind = irecind + irecsize
!!!
   ierr = RAMS_getvar('SOIL_TEXT',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   call get_leaf_soil(n1,n2,n3,n4,n5,a(irecind),a2)

   if (cvar(1:lv).eq.'stext') then
      ivar_type = 8
   else
      ivar_type = 3
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),nzg,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='soil twxture'
   cdunits=''

elseif(cvar(1:lv).eq.'SOIL_WATERf_p' .or. cvar(1:lv).eq.'SOIL_WATERf_ps') then

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   irecind = irecind + irecsize
   ierr = RAMS_getvar('SOIL_WATER',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   ierr = RAMS_getvar('SOIL_TEXT',idim_type,ngrd   &
        ,c,b,flnm)
   call rams_comp_slmstf(irecsize,1,1,a(irecind),c)

   if (cvar(1:lv).eq.'SOIL_WATERf_p') then
      ivar_type = 8
   else
      ivar_type = 3
      call RAMS_comp_patchsum_l(nnxp(ngrd),nnyp(ngrd),nzg,npatch  &
         ,a(irecind),a(1),b)
   endif

   cdname='soil moisture frac'
   cdunits='m3/m3'

elseif(cvar(1:lv).eq.'leaf2_moisture') then

! These values should somehow be scaled across soil, snow, vegetation, and canopy air
! using calls to rams_comp_snownorm, rams_comp_vegnorm, and rams_comp_cannorm,
! which are not yet completed.

   ivar_type = 10

   irecind = 1
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd   &
        ,a(irecind),b,flnm)

   irecind = irecind + irecsize
   irecsize = nnxp(ngrd) * nnyp(ngrd) * nzg * npatch
   ierr = RAMS_getvar('SOIL_WATER',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   ierr = RAMS_getvar('SOIL_TEXT',idim_type,ngrd   &
        ,c,b,flnm)
   call rams_comp_slmstf(irecsize,1,1,a(irecind),c)

   irecind = irecind + irecsize
   irecsize = nnxp(ngrd) * nnyp(ngrd) * nzs * npatch
   ierr = RAMS_getvar('SNOW_MOIST',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call rams_comp_snownorm(irecsize,1,1,a(irecind),c)

   irecind = irecind + irecsize
   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   ierr = RAMS_getvar('VEG_MOIST',idim_type,ngrd   &
        ,a(irecind),b,flnm)
   call rams_comp_vegnorm(irecsize,1,1,a(irecind),c)

   irecsize = nnxp(ngrd) * nnyp(ngrd) * npatch
   irecind = irecind + irecsize
   ierr = RAMS_getvar('CAN_RV',idim_type,ngrd   &
        ,c,b,flnm)
   call rams_comp_cannorm(irecsize,1,1,a(irecind),c)

   ! also get pcpg, and vapor for k=2

   cdname='leaf2 moisture frac'
   cdunits='m3/m3'

elseif(cvar(1:lv).eq.'leaf2_temp') then

   ! similar to leaf2_moisture


elseif(cvar(1:lv).eq.'ctprof') then
   ivar_type=2

!   Determine RH so need species AND 0.99 saturated for cloud.
!   ierr=RAMS_getvar(20,ind,nind,3,ngrd,a,b,flnm)
!   ierr=RAMS_getvar(6,ind,nind,3,ngrd,c,b,flnm)
!   ierr=RAMS_getvar(19,ind,nind,3,ngrd,d,b,flnm)
!   call RAMS_comp_rh(n1,n2,n3,a,c,d)
!   call RAMS_comp_noneg(n1,n2,n3,a)
!   call ae1(n1*n2*n3,d,a)

!        Now accumulate the uphysics species.
!   call RAMS_comp_zero(n1,n2,n3,a)
!   call RAMS_comp_zero(n1,n2,n3,c)
!   ierr=RAMS_getvar(61,ind,nind,3,ngrd,a,b,flnm)
!   ierr=RAMS_getvar(9,ind,nind,3,ngrd,c,b,flnm)
!   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
!   ierr=RAMS_getvar(10,ind,nind,3,ngrd,c,b,flnm)
!   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
!   ierr=RAMS_getvar(11,ind,nind,3,ngrd,c,b,flnm)
!   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
!   ierr=RAMS_getvar(12,ind,nind,3,ngrd,c,b,flnm)
!   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
!   ierr=RAMS_getvar(13,ind,nind,3,ngrd,c,b,flnm)
!   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)

!   call RAMS_comp_ctprof(n1,n2,n3,a,d,ngrd)
!   ierr_getvar=0

!   cdname='cloud top height'
!   cdunits='m'
!SIB
  elseif(cvar(1:lv).eq.'CO2X') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP001',idim_type,ngrd,a,b,flnm)
!!!   ierr= RAMS_getvar('SCLR004',idim_type,ngrd,a,b,flnm)
!!!  call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='CO2 Concentration'
   cdunits='ppm'

! CATT

elseif(cvar(1:lv).eq.'COX') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP001',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='CO Concentration'
   cdunits='ppb'

elseif(cvar(1:lv).eq.'PRNO2') then
   ivar_type=3
   ierr= RAMS_getvar('DCBP',idim_type,ngrd,a,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,a)
   ierr= RAMS_getvar('TPANP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('HC5P',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('HC8P',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('ETEP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('OLTP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('OLIP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('ISOP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('APIP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
 !  ierr= RAMS_getvar('LIMP',idim_type,ngrd,c,b,flnm)
 !  if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('TOLP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('XYLP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('CSLP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('ACO3P',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('KETP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('ETEP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)

   cdname='PRNO2 mix ratio'
   cdunits='ppbm'

elseif(cvar(1:lv).eq.'PRCO') then
   ivar_type=3
   ierr= RAMS_getvar('DCBP',idim_type,ngrd,a,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,a)
   ierr= RAMS_getvar('TPANP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
 !  ierr= RAMS_getvar('LIMP',idim_type,ngrd,c,b,flnm)
 !  if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('APIP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('ISOP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('DIENP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('OLIP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('ETEP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('HC3P',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('MACRP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('GLYP',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)

   cdname='PRCO mix ratio'
   cdunits='ppbm'

elseif(cvar(1:lv).eq.'TCCOold') then
   ivar_type=2
   ierr= RAMS_getvar('SCLP001',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg

! add background 100 ppb
!   call RAMS_comp_add(n1,n2,n3,a,100.*1e-9*28./28.96)

   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd) ! d=dens_ar
   call RAMS_comp_mult(n1,n2,n3,a,d)         ! aqui a=CO*dens_ar
   call RAMS_comp_vertint(n1,n2,n3,a,e,ngrd) ! CO em kg/m^2
   call RAMS_comp_mults(n1,n2,n3,a,1000.) ! CO em g/m2
   call RAMS_comp_mults(n1,n2,n3,a,1./28.) ! numero de moles de CO por m2
!   call RAMS_comp_mults(n1,n2,n3,a,6.022e23) !numero de moleculas de CO por m2
   call RAMS_comp_mults(n1,n2,n3,a,1e-4) !numero de moleculas de CO por cm2
   cdname='CO total column'
!   cdunits='molec/cm^2'
   cdunits='moles/cm^2'

elseif(cvar(1:lv).eq.'CO') then
   ivar_type=3
   ierr= RAMS_getvar('COP',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname='CO Concentration'
   cdunits='ppbv'
elseif(cvar(1:lv).eq.'CH4') then
   ivar_type=3
   ierr= RAMS_getvar('CH4P',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,16.)
   cdname='CH4 Concentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'CORR1') then
   ivar_type=3
   ierr= RAMS_getvar('CH4P',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname='CORR 1 Concentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'CORR2') then
   ivar_type=3
   ierr= RAMS_getvar('NOP',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname='CORR 2 Concentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'CORR4') then
   ivar_type=3
   ierr= RAMS_getvar('O3P',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname='CORR 2 Concentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'SC3TR2') then
   ivar_type=3
   ierr= RAMS_getvar('FORMP',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname=' sum cons 3tr tr2 Cncentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'SC3TR3') then
   ivar_type=3
   ierr= RAMS_getvar('NO2P',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname=' sum cons 3tr tr3 Cncentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'SC4TR2') then
   ivar_type=3
   ierr= RAMS_getvar('FORMP',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname=' sum cons 4tr tr2 Cncentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'SC4TR3') then
   ivar_type=3
   ierr= RAMS_getvar('PANP',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname=' sum cons 4tr tr3 Cncentration'
   cdunits='ppbv'
elseif(cvar(1:lv).eq.'SC4TR4') then
   ivar_type=3
   ierr= RAMS_getvar('NO3P',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname=' sum cons 4tr tr4 Cncentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'COA') then
   ivar_type=3
   ierr= RAMS_getvar('O3P',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname='CO2 Concentration'
   cdunits='ppbv'
elseif(cvar(1:lv).eq.'COS') then
   ivar_type=3
   ierr= RAMS_getvar('NOP',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname='CO sum Concentration'
   cdunits='ppbv'


elseif(cvar(1:lv).eq.'SO2') then
   ivar_type=3
   ierr= RAMS_getvar('SO2P',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,64.)
   cdname='SO2 Concentration'
   cdunits='ppbv'

!elseif(cvar(1:lv).eq.'d2006') then
!   ivar_type=2
!   ierr= RAMS_getvar('d2006',idim_type,ngrd,a,b,flnm)
!   cdname='co2bio'
!   cdunits='ppm'
!---------- temporary output
elseif(cvar(1:lv).eq.'AA0') then
   ivar_type=2
   ierr= RAMS_getvar('accpr_gr',idim_type,ngrd,a,b,flnm)
   cdname='AA0'
   cdunits=' '

elseif(cvar(1:lv).eq.'AA1') then
   ivar_type=2
   ierr= RAMS_getvar('accpr_w',idim_type,ngrd,a,b,flnm)
   cdname='AA1'
   cdunits=' '

elseif(cvar(1:lv).eq.'XAA0') then
   ivar_type=2
   ierr= RAMS_getvar('accpr_mc',idim_type,ngrd,a,b,flnm)
   cdname='XAA0'
   cdunits='mm'

elseif(cvar(1:lv).eq.'XK') then
   ivar_type=2
   ierr= RAMS_getvar('accpr_st',idim_type,ngrd,a,b,flnm)
   cdname='XK'
   cdunits='mm '

elseif(cvar(1:lv).eq.'AA1BL') then
   ivar_type=2
   ierr= RAMS_getvar('accpr_as',idim_type,ngrd,a,b,flnm)
   cdname='AA1BL'
   cdunits='mm '
!===================
elseif(cvar(1:lv).eq.'train') then
   ivar_type=2
   ierr= RAMS_getvar('d2005',idim_type,ngrd,a,b,flnm)
   cdname='train'
   cdunits=' '
!---------- training
elseif(cvar(1:lv).eq.'APRGR') then
   ivar_type=2
   ierr= RAMS_getvar('accpr_gr',idim_type,ngrd,a,b,flnm)
   cdname='accum precip from GR closure'
   cdunits='mm '

elseif(cvar(1:lv).eq.'APRW') then
   ivar_type=2
   ierr= RAMS_getvar('accpr_w',idim_type,ngrd,a,b,flnm)
   cdname='acccum precip from W closure'
   cdunits='mm '

elseif(cvar(1:lv).eq.'APRMC') then
   ivar_type=2
   ierr= RAMS_getvar('accpr_mc',idim_type,ngrd,a,b,flnm)
   cdname='acccum precip from MC closure'
   cdunits='mm'

elseif(cvar(1:lv).eq.'APRST') then
   ivar_type=2
   ierr= RAMS_getvar('accpr_st',idim_type,ngrd,a,b,flnm)
   cdname='accum precip from ST closure'
   cdunits='mm '

elseif(cvar(1:lv).eq.'APRAS') then
   ivar_type=2
   ierr= RAMS_getvar('accpr_as',idim_type,ngrd,a,b,flnm)
   cdname='acccum precip from AS closure'
   cdunits='mm '

elseif(cvar(1:lv).eq.'WEIGHTAS') then
   ivar_type=2
   ierr= RAMS_getvar('weight_as',idim_type,ngrd,a,b,flnm)
   cdname='weight for  AS closure'
   cdunits=' '

elseif(cvar(1:lv).eq.'WEIGHTST') then
   ivar_type=2
   ierr= RAMS_getvar('weight_st',idim_type,ngrd,a,b,flnm)
   cdname='weight for  st closure'
   cdunits=' '
elseif(cvar(1:lv).eq.'WEIGHTGR') then
   ivar_type=2
   ierr= RAMS_getvar('weight_gr',idim_type,ngrd,a,b,flnm)
   cdname='weight for  gr closure'
   cdunits=' '
elseif(cvar(1:lv).eq.'WEIGHTMC') then
   ivar_type=2
   ierr= RAMS_getvar('weight_mc',idim_type,ngrd,a,b,flnm)
   cdname='weight for  mc closure'
   cdunits=' '
!----------
elseif(cvar(1:lv).eq.'CO2BIO') then
   ivar_type=3
   ierr= RAMS_getvar('CO2BIOP',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-3)  ! converte de mg/kg para kg/kg
   !call RAMS_transf_ppb(n1,n2,n3,a,44.)
   cdname='CO2BIOP '
   cdunits='ppm'


elseif(cvar(1:lv).eq.'COGF') then
   ivar_type=3
   ierr= RAMS_getvar('COGFP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname='CO GFEDv2 Concentration'
   cdunits='ppbv'

 elseif(cvar(1:lv).eq.'COSP') then
   ivar_type=3
   ierr= RAMS_getvar('COSPP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname='CO without plumerise Concentration'
   cdunits='ppbv'

   elseif(cvar(1:lv).eq.'COAB') then
   ivar_type=3
   ierr= RAMS_getvar('COABP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname='CO ANTO+BIOGE Concentration'
   cdunits='ppbv'


elseif(cvar(1:lv).eq.'COWD') then
   ivar_type=2
   ierr= RAMS_getvar('COWD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   cdname='Wet deposition mass CO'
   cdunits='kg/m2'
elseif(cvar(1:lv).eq.'CODD') then
   ivar_type=2
   ierr= RAMS_getvar('CODD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   cdname='dry deposition mass CO'
   cdunits='kg/m2'


elseif(cvar(1:lv).eq.'NOWD') then
   ivar_type=2
   ierr= RAMS_getvar('NOWD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   cdname='Wet deposition mass NO'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'O3WD') then
   ivar_type=2
   ierr= RAMS_getvar('O3WD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   cdname='Wet deposition mass O3'
   cdunits='kg/m2'
elseif(cvar(1:lv).eq.'O3DD') then
   ivar_type=2
   ierr= RAMS_getvar('O3DD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   cdname='dry deposition mass O3'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'H2O2WD') then
   ivar_type=2
   ierr= RAMS_getvar('H2O2WD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   cdname='Wet deposition mass H@O@'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'N2O5') then
   ivar_type=3
   ierr= RAMS_getvar('N2O5P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,108.)
   cdname='N2O5 Concentration'
   cdunits='ppbv'
elseif(cvar(1:lv).eq.'HO2') then
   ivar_type=3
   ierr= RAMS_getvar('HO2P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,33.)
   cdname='HO2 Concentration'
   cdunits='ppbv'
elseif(cvar(1:lv).eq.'NO') then
   ivar_type=3
   ierr= RAMS_getvar('NOP',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,30.)
   cdname='CO Concentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'CO2') then
   ivar_type=3
   ierr= RAMS_getvar('CO2P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,44.)
   cdname='CO2 Concentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'HONO') then
   ivar_type=3
   ierr= RAMS_getvar('HONOP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,47.)
   cdname='HONO Concentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'HNO3') then
   ivar_type=3
   ierr= RAMS_getvar('HNO3P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,63.)
   cdname='HNO3 Concentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'O3') then
   ivar_type=3
   ierr= RAMS_getvar('O3P',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,48.)
   cdname='O3P mix ratio'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'O3mt') then
   ivar_type=2
   ierr= RAMS_getvar('O3P',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,48.)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd) ! d=dens_ar
   ierr= RAMS_getvar('GLAT',idim_type,ngrd,g,b,flnm)
   call RAMS_comp_vertmean_O3(n1,n2,n3,a,d,e,g,ngrd,00000.)
   cdname='O3P tropospheric mean mix ratio'
   cdunits='ppbv'


elseif(cvar(1:lv).eq.'TCO3') then

   fxx=1e-9*1000.*1./48.*1e-4  !*6.022e23
   ivar_type=2
   ierr= RAMS_getvar('O3P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_comp_mults(n1,n2,n3,a,1e-9) ! kg/kg

! add background 100 ppb
!   call RAMS_comp_add(n1,n2,n3,a,100.*1e-9*28./28.96)

   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd) ! d=dens_ar
   call RAMS_comp_mult(n1,n2,n3,a,d)         ! aqui a=O3*dens_ar
   ierr= RAMS_getvar('GLAT',idim_type,ngrd,g,b,flnm)
   call RAMS_comp_vertint_O3(n1,n2,n3,a,e,g,ngrd,00000.)


!   call RAMS_comp_mults(n1,n2,n3,a,1000.) ! O3 em g/m2
!   call RAMS_comp_mults(n1,n2,n3,a,1./48.) ! numero de moles de O3 por m2
!   call RAMS_comp_mults(n1,n2,n3,a,6.022e23) !numero de moleculas de O3 por m2
!   call RAMS_comp_mults(n1,n2,n3,a,1e-4) !numero de moleculas de O3 por cm2
   call RAMS_comp_mults(n1,n2,n3,a,fxx)
   cdname='O3 tropos  total column '
!   cdunits='molec/cm^2'
   cdunits='moles/cm^2'

elseif(cvar(1:lv).eq.'TCCO') then

   fxx=1e-9*1000.*1./28.*1e-4  !*6.022e23
   ivar_type=2
   ierr= RAMS_getvar('COP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_comp_mults(n1,n2,n3,a,1e-9) ! kg/kg

! add background 100 ppb
!   call RAMS_comp_add(n1,n2,n3,a,100.*1e-9*28./28.96)

   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd) ! d=dens_ar
   call RAMS_comp_mult(n1,n2,n3,a,d)         ! aqui a=O3*dens_ar
   ierr= RAMS_getvar('GLAT',idim_type,ngrd,g,b,flnm)
   call RAMS_comp_vertint_O3(n1,n2,n3,a,e,g,ngrd,00000.)

!   call RAMS_comp_mults(n1,n2,n3,a,1000.) ! O3 em g/m2
!   call RAMS_comp_mults(n1,n2,n3,a,1./48.) ! numero de moles de O3 por m2
!   call RAMS_comp_mults(n1,n2,n3,a,6.022e23) !numero de moleculas de O3 por m2
!   call RAMS_comp_mults(n1,n2,n3,a,1e-4) !numero de moleculas de O3 por cm2
   call RAMS_comp_mults(n1,n2,n3,a,fxx)
   cdname='CO tropos total column '
!   cdunits='molec/cm^2'
   cdunits='moles/cm^2'


elseif(cvar(1:lv).eq.'TCNO2') then

   fxx=1e-9*1000.*1./46.*1e-4  !*6.022e23
   ivar_type=2
   ierr= RAMS_getvar('NO2P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_comp_mults(n1,n2,n3,a,1e-9) ! kg/kg

! add background 100 ppb
!   call RAMS_comp_add(n1,n2,n3,a,100.*1e-9*28./28.96)

   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd) ! d=dens_ar
   call RAMS_comp_mult(n1,n2,n3,a,d)         ! aqui a=O3*dens_ar
   ierr= RAMS_getvar('GLAT',idim_type,ngrd,g,b,flnm)
   call RAMS_comp_vertint_O3(n1,n2,n3,a,e,g,ngrd,00000.)

!   call RAMS_comp_mults(n1,n2,n3,a,1000.) ! O3 em g/m2
!   call RAMS_comp_mults(n1,n2,n3,a,1./48.) ! numero de moles de O3 por m2
!   call RAMS_comp_mults(n1,n2,n3,a,6.022e23) !numero de moleculas de O3 por m2
!   call RAMS_comp_mults(n1,n2,n3,a,1e-4) !numero de moleculas de O3 por cm2
   call RAMS_comp_mults(n1,n2,n3,a,fxx)
   cdname='NO2 tropos  total column'
!   cdunits='molec/cm^2'
   cdunits='moles/cm^2'


elseif(cvar(1:lv).eq.'J1') then
   ivar_type=3
   ierr= RAMS_getvar('d3010',idim_type,ngrd,a,b,flnm)
   cdname='J'
   cdunits='1/s'

elseif(cvar(1:lv).eq.'J2') then
   ivar_type=3
   ierr= RAMS_getvar('d3011',idim_type,ngrd,a,b,flnm)
   cdname='J '
   cdunits='1/s'

elseif(cvar(1:lv).eq.'J3') then
   ivar_type=3
   ierr= RAMS_getvar('d3012',idim_type,ngrd,a,b,flnm)
   cdname='J '
   cdunits='1/s'

elseif(cvar(1:lv).eq.'J4') then
   ivar_type=3
   ierr= RAMS_getvar('d3013',idim_type,ngrd,a,b,flnm)
   cdname='J 1 '
   cdunits='1/s'
elseif(cvar(1:lv).eq.'J5') then
   ivar_type=3
   ierr= RAMS_getvar('d3014',idim_type,ngrd,a,b,flnm)
   cdname='J 1 '
   cdunits='1/s'
elseif(cvar(1:lv).eq.'J6') then
   ivar_type=3
   ierr= RAMS_getvar('d3015',idim_type,ngrd,a,b,flnm)
   cdname='J 1 '
   cdunits='1/s'
elseif(cvar(1:lv).eq.'J7') then
   ivar_type=3
   ierr= RAMS_getvar('d3016',idim_type,ngrd,a,b,flnm)
   cdname='J 1 '
   cdunits='1/s'
elseif(cvar(1:lv).eq.'J8') then
   ivar_type=3
   ierr= RAMS_getvar('d3017',idim_type,ngrd,a,b,flnm)
   cdname='J 1 '
   cdunits='1/s'

elseif(cvar(1:lv).eq.'J9') then
   ivar_type=3
   ierr= RAMS_getvar('d3018',idim_type,ngrd,a,b,flnm)
   cdname='J 1 '
   cdunits='1/s'

elseif(cvar(1:lv).eq.'J10') then
   ivar_type=3
   ierr= RAMS_getvar('d3019',idim_type,ngrd,a,b,flnm)
   cdname='J 1 '
   cdunits='1/s'

elseif(cvar(1:lv).eq.'J11') then
   ivar_type=3
   ierr= RAMS_getvar('d3021',idim_type,ngrd,a,b,flnm)
   cdname='J 1 '
   cdunits='1/s'

elseif(cvar(1:lv).eq.'J12') then
   ivar_type=3
   ierr= RAMS_getvar('d3022',idim_type,ngrd,a,b,flnm)
   cdname='J 1 '
   cdunits='1/s'

elseif(cvar(1:lv).eq.'J13') then
   ivar_type=3
   ierr= RAMS_getvar('d3023',idim_type,ngrd,a,b,flnm)
   cdname='J 1 '
   cdunits='1/s'

elseif(cvar(1:lv).eq.'J14') then
   ivar_type=3
   ierr= RAMS_getvar('d3024',idim_type,ngrd,a,b,flnm)
   cdname='J 1 '
   cdunits='1/s'

elseif(cvar(1:lv).eq.'J15') then
   ivar_type=3
   ierr= RAMS_getvar('d3025',idim_type,ngrd,a,b,flnm)
   cdname='J 1 '
   cdunits='1/s'

elseif(cvar(1:lv).eq.'co3') then
   ivar_type=3
   ierr= RAMS_getvar('d3022',idim_type,ngrd,a,b,flnm)
   cdname='CO3'
   cdunits=' ?? '

elseif(cvar(1:lv).eq.'cair') then
   ivar_type=3
   ierr= RAMS_getvar('d3023',idim_type,ngrd,a,b,flnm)
   cdname='cair'
   cdunits=' ?? '
elseif(cvar(1:lv).eq.'tco3') then
   ivar_type=3
   ierr= RAMS_getvar('d3025',idim_type,ngrd,a,b,flnm)
   cdname='TCO3'
   cdunits=' ?? '

elseif(cvar(1:lv).eq.'airden') then
   ivar_type=3
   ierr= RAMS_getvar('d3024',idim_type,ngrd,a,b,flnm)
   cdname='airden'
   cdunits=' ?? '

elseif(cvar(1:lv).eq.'JNO2') then
   ivar_type=3
   ierr= RAMS_getvar('d3007',idim_type,ngrd,a,b,flnm)
   cdname='J NO2 '
   cdunits='1/s'

elseif(cvar(1:lv).eq.'JO3') then
   ivar_type=3
   ierr= RAMS_getvar('d3008',idim_type,ngrd,a,b,flnm)
   cdname='J O3 (rk(2) + rk(3)) '
   cdunits='1/s'

elseif(cvar(1:lv).eq.'NO2') then
   ivar_type=3
   ierr= RAMS_getvar('NO2P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,46.)
   cdname='NO2 mixing ratio'
   cdunits='ppbv'
elseif(cvar(1:lv).eq.'NO3') then
   ivar_type=3
   ierr= RAMS_getvar('NO3P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,62.)
   cdname='NO3 mixing ratio'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'H2O') then
   ivar_type=3
   ierr= RAMS_getvar('H2OP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,18.)
   cdname='H2O mixing ratio'
   cdunits='ppbv.1e6'

 elseif(cvar(1:lv).eq.'OH') then
   ivar_type=3
   ierr= RAMS_getvar('OHP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,6.02E23)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-15)
   call RAMS_comp_mults(n1,n2,n3,a,28.96)
   call RAMS_comp_mults(n1,n2,n3,a,1./17.)
   ierr= RAMS_getvar('THETA',idim_type,ngrd,d,b,flnm)
   ierr= RAMS_getvar('PI',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_tempK(n1,n2,n3,d,c)
   call RAMS_comp_press(n1,n2,n3,c)
   call RAMS_comp_mults(n1,n2,n3,c,100.)
   call convert_from_ppbm_to_molec_by_cm3(n1,n2,n3,a,d,c)

   cdname='OH density'
   cdunits='molec/cm^3'

elseif(cvar(1:lv).eq.'H2O2') then
   ivar_type=3
   ierr= RAMS_getvar('H2O2P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,34.)
   cdname='H2O2 mixing ratio'
   cdunits='ppbv '


elseif(cvar(1:lv).eq.'ETH') then
   ivar_type=3
   ierr= RAMS_getvar('ETHP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,16.)
   cdname='ETH mixing ratio'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'ETE') then
   ivar_type=3
   ierr= RAMS_getvar('ETEP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname='ETE mixing ratio'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'MACR') then
   ivar_type=3
   ierr= RAMS_getvar('MACRP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,70.)
   cdname='MACR mixing ratio'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'HCHO') then
   ivar_type=3
   ierr= RAMS_getvar('HCHOP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,30.)
   cdname='HCHO mixing ratio'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'PAN') then
   ivar_type=3
   ierr= RAMS_getvar('PANP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,121.)
   cdname='PAN mixing ratio'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'OLE') then
   ivar_type=3
   ierr= RAMS_getvar('OLEP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,121.)
   cdname='OLE mixing ratio'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'FORM') then
   ivar_type=3
   ierr= RAMS_getvar('FORMP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,121.)
   cdname='FORM mixing ratio'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'ALD2') then
   ivar_type=3
   ierr= RAMS_getvar('ALD2P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,121.)
   cdname='ALD2 mixing ratio'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'PNA') then
   ivar_type=3
   ierr= RAMS_getvar('PNAP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a,121.)
   cdname='PNA mixing ratio'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'NOSRCBB') then
   ivar_type=3
   ierr= RAMS_getvar('NO_bburn_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='NO_bburn_SRC '
   cdunits=' '


elseif(cvar(1:lv).eq.'NO_src') then
   ivar_type=3
   ierr= RAMS_getvar('NO_bburn_SRC',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('NO_antro_SRC',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('NO_bioge_SRC',idim_type,ngrd,e,b,flnm)
   call get_total_src(n1,n2,n3,a,c,e)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='NO src'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'CO_src') then
   ivar_type=3
   ierr= RAMS_getvar('CO_bburn_SRC',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CO_antro_SRC',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('CO_bioge_SRC',idim_type,ngrd,e,b,flnm)
   call get_total_src(n1,n2,n3,a,c,e)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='CO src'
   cdunits='kg/kg/day'
elseif(cvar(1:lv).eq.'CO_asrc') then
   ivar_type=3
   ierr= RAMS_getvar('CO_antro_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='CO src anthro'
   cdunits='kg/kg/day'
elseif(cvar(1:lv).eq.'CO2_src') then
   ivar_type=3
   ierr= RAMS_getvar('CO2_bburn_SRC',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('CO2_antro_SRC',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('CO2_bioge_SRC',idim_type,ngrd,e,b,flnm)
   call get_total_src(n1,n2,n3,a,c,e)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='CO2 src'
   cdunits='kg/kg/day'
elseif(cvar(1:lv).eq.'CO_asrc') then
   ivar_type=3
   ierr= RAMS_getvar('CO_antro_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='CO src anthro'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'ISO_src') then
   ivar_type=3
   ierr= RAMS_getvar('ISO_bburn_SRC',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('ISO_antro_SRC',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('ISO_bioge_SRC',idim_type,ngrd,e,b,flnm)
   call get_total_src(n1,n2,n3,a,c,e)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='ISO src'
   cdunits='kg/kg/day'
elseif(cvar(1:lv).eq.'ETH_src') then
   ivar_type=3
   ierr= RAMS_getvar('ETH_bburn_SRC',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('ETH_antro_SRC',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('ETH_bioge_SRC',idim_type,ngrd,e,b,flnm)
   call get_total_src(n1,n2,n3,a,c,e)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='ETH src'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'SO2_src') then
   ivar_type=3
   ierr= RAMS_getvar('SO2_bburn_SRC',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('SO2_antro_SRC',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('SO2_bioge_SRC',idim_type,ngrd,e,b,flnm)
   ierr= RAMS_getvar('SO2_geoge_SRC',idim_type,ngrd,d,b,flnm)
!   call get_total_src(n1,n2,n3,a,c,e)
   call get_total_src_volc(n1,n2,n3,a,c,e,d)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='SO2 src'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'COGF_src') then
   ivar_type=3
   ierr= RAMS_getvar('COGF_bburn_SRC',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('COGF_antro_SRC',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('COGF_bioge_SRC',idim_type,ngrd,e,b,flnm)
   call get_total_src(n1,n2,n3,a,c,e)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='COGF src'
   cdunits='kg/kg/day'
 elseif(cvar(1:lv).eq.'COSP_src') then
   ivar_type=3
   ierr= RAMS_getvar('COSP_bburn_SRC',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('COSP_antro_SRC',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('COSP_bioge_SRC',idim_type,ngrd,e,b,flnm)
   call get_total_src(n1,n2,n3,a,c,e)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='COSP src'
   cdunits='kg/kg/day'
 elseif(cvar(1:lv).eq.'COAB_src') then
   ivar_type=3
   ierr= RAMS_getvar('COAB_bburn_SRC',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('COAB_antro_SRC',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('COAB_bioge_SRC',idim_type,ngrd,e,b,flnm)
   call get_total_src(n1,n2,n3,a,c,e)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='COAB src'
   cdunits='kg/kg/day'
 elseif(cvar(1:lv).eq.'COAB_xsrc') then
   ivar_type=3
   !ierr= RAMS_getvar('COAB_antro_SRC',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('COAB_bioge_SRC',idim_type,ngrd,a,b,flnm)
   call get_total_srcx(n1,n2,n3,a,a,e)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='COAB src'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'PM25_src') then
   ivar_type=3
   ierr= RAMS_getvar('bburn2_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='PM25 src'
   cdunits='kg/kg/day'
elseif(cvar(1:lv).eq.'SO4_src') then
   ivar_type=3
   ierr= RAMS_getvar('urban2_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='SO4 src'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'PM10_src') then
   ivar_type=3
   ierr= RAMS_getvar('bburn3_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='PM10 src'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'ash1_src') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash1_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='ASH1 src'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'ash2_src') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash2_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='ASH2 src'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'ash3_src') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash3_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='ASH3 src'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'ash4_src') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash4_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='ASH4 src'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'ash5_src') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash5_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='ASH5 src'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'ash6_src') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash6_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='ASH6 src'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'ash7_src') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash7_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='ASH7 src'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'ash8_src') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash8_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='ASH8 src'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'ash9_src') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash9_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='ASH9 src'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'ash10_src') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash10_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='ASH10 src'
   cdunits='kg/kg/day'

! AQUEOUS CHEMISTRY

elseif(cvar(1:lv).eq.'O3aqR') then

   ivar_type=3
   ierr= RAMS_getvar('O3aqPR',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_transf_ppb(n1,n2,n3,a,48.)
   cdname='O3 aq Rain Concentration'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'O3aqC') then

   ivar_type=3
   ierr= RAMS_getvar('O3aqPC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_transf_ppb(n1,n2,n3,a,48.)
   cdname='O3 aq Cloud Concentration'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'H2O2aqR') then

   ivar_type=3
   ierr= RAMS_getvar('H2O2aqPR',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_transf_ppb(n1,n2,n3,a,34.)
   cdname='H2O2 aq Rain Concentration'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'H2O2aqC') then

   ivar_type=3
   ierr= RAMS_getvar('H2O2aqPC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_transf_ppb(n1,n2,n3,a,34.)
   cdname='H2O2 aq Cloud Concentration'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'HNO3aqR') then

   ivar_type=3
   ierr= RAMS_getvar('HNO3aqPR',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_transf_ppb(n1,n2,n3,a,63.)
   cdname='HNO3 aq Rain Concentration'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'HNO3aqC') then

   ivar_type=3
   ierr= RAMS_getvar('HNO3aqPC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_transf_ppb(n1,n2,n3,a,63.)
   cdname='HNO3 aq Cloud Concentration'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'HO2aqR') then

   ivar_type=3
   ierr= RAMS_getvar('HO2aqPR',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_transf_ppb(n1,n2,n3,a,33.)
   cdname='HO2 aq Rain Concentration'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'HO2aqC') then

   ivar_type=3
   ierr= RAMS_getvar('HO2aqPC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_transf_ppb(n1,n2,n3,a,33.)
   cdname='HO2 aq Cloud Concentration'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'ORA2aqR') then

   ivar_type=3
   ierr= RAMS_getvar('ORA2aqPR',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_transf_ppb(n1,n2,n3,a,48.)
   cdname='ORA2 aq Rain Concentration'
   cdunits='ppbv '

elseif(cvar(1:lv).eq.'ORA2aqC') then

   ivar_type=3
   ierr= RAMS_getvar('ORA2aqPC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_transf_ppb(n1,n2,n3,a,48.)
   cdname='ORA2 aq Cloud Concentration'
   cdunits='ppbv '


! End CHEMISTRY






! CO com bound condition
!elseif(cvar(1:lv).eq.'CO') then
!   ivar_type=3
!   ierr= RAMS_getvar('SCLP001',idim_type,ngrd,a,b,flnm)
!   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
!   call RAMS_transf_ppb(n1,n2,n3,a)
!   if(ngrd.eq.1) then
!   ierr= RAMS_getvar('UP',idim_type,ngrd,d,b,flnm)
!   ierr= RAMS_getvar('VP',idim_type,ngrd,e,b,flnm)
!
!   call RAMS_set_bnd(n1,n2,n3,a,d,e)
!   endif
!   cdname='CO Concentration'
!   cdunits='ppb'

elseif(cvar(1:lv).eq.'COstc_sbc') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP002',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='CO Conc - without conv transp and bound condition'
   cdunits='ppb'

! CO com bound condition
!elseif(cvar(1:lv).eq.'COstc') then
!   ivar_type=3
!   ierr= RAMS_getvar('SCLR002',idim_type,ngrd,a,b,flnm)
!   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
!   call RAMS_transf_ppb(n1,n2,n3,a)
!   if(ngrd.eq.1) then
!   ierr= RAMS_getvar('UP',idim_type,ngrd,d,b,flnm)
!   ierr= RAMS_getvar('VP',idim_type,ngrd,e,b,flnm)
!
!   call RAMS_set_bnd(n1,n2,n3,a,d,e)
!   endif
!   cdname='CO Conc - without convective transp'
!   cdunits='ppb'

elseif(cvar(1:lv).eq.'CO_sbc') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP001',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='CO Concentration without bound condition'
   cdunits='ppb'

elseif(cvar(1:lv).eq.'COD') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP005',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='Duncan - CO Concentration'
   cdunits='ppb'

elseif(cvar(1:lv).eq.'COE') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP006',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='EDGAR - CO Concentration'
   cdunits='ppb'

elseif(cvar(1:lv).eq.'COSH') then
   ivar_type=3
   ierr= RAMS_getvar('ETHP',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname='only shallow transp - CO Concentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'CODP') then
   ivar_type=3
   ierr= RAMS_getvar('HC3P',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname='only deep transp - CO Concentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'COPR') then
   ivar_type=3
   ierr= RAMS_getvar('CH4P',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_noneg(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname='only plumerise transp - CO Concentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'COAT') then
   ivar_type=3
   ierr= RAMS_getvar('NOP',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_none(n1,n2,n3,a)
   !call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a,28.)
   cdname='only adv+turb transp - CO Concentration'
   cdunits='ppbv'

elseif(cvar(1:lv).eq.'CODPSH') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP004',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='only deep-shallow transp - CO Conc'
   cdunits='ppb'

elseif(cvar(1:lv).eq.'COPL') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP005',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='only plumerise - CO Conc'
   cdunits='ppb'

   elseif(cvar(1:lv).eq.'src1') then
   ivar_type=3
   ierr= RAMS_getvar('scrsc001',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/day para kg/day
   cdname='emission 1'
   cdunits='kg/kg/day'

   elseif(cvar(1:lv).eq.'src2') then
   ivar_type=3
   ierr= RAMS_getvar('scrsc002',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/day para kg/day
   cdname='emission 2'
   cdunits='kg/kg/day'



elseif(cvar(1:lv).eq.'src1INT') then
   ivar_type=2
   ierr= RAMS_getvar('scrsc001',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d) !Unit: kg[pm25]/m3
   call RAMS_comp_vertint(n1,n2,n3,a,e,ngrd) ! Unit: kg[pm25]/m2
   call RAMS_comp_mults(n1,n2,n3,a,1.e+9)  ! converte de kg/m2 para ug/m2

   cdname='SRC1 vert int'
   cdunits='ug/m2'

elseif(cvar(1:lv).eq.'src2INT') then
   ivar_type=2
   ierr= RAMS_getvar('scrsc002',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d) !Unit: kg[pm25]/m3
   call RAMS_comp_vertint(n1,n2,n3,a,e,ngrd) ! Unit: kg[pm25]/m2
   call RAMS_comp_mults(n1,n2,n3,a,1.e+9)  ! converte de kg/m2 para ug/m2

   cdname='SRC2 vert int'
   cdunits='ug/m2'

   elseif(cvar(1:lv).eq.'src3') then
   ivar_type=3
   ierr= RAMS_getvar('scrsc003',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/day para kg/day
   cdname='emission 3'
   cdunits='kg/kg/day'

   elseif(cvar(1:lv).eq.'src4') then
   ivar_type=3
   ierr= RAMS_getvar('scrsc004',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/day para kg/day
   cdname='emission 4'
   cdunits='kg/kg/day'

   elseif(cvar(1:lv).eq.'src5') then
   ivar_type=3
   ierr= RAMS_getvar('scrsc005',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/day para kg/day
   cdname='emission 5'
   cdunits='kg/kg/day'

   elseif(cvar(1:lv).eq.'src6') then
   ivar_type=3
   ierr= RAMS_getvar('scrsc006',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/day para kg/day
   cdname='emission 6'
   cdunits='kg/kg/day'

    elseif(cvar(1:lv).eq.'src7') then
   ivar_type=3
   ierr= RAMS_getvar('scrsc007',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/day para kg/day
   cdname='emission 7'
   cdunits='kg/m2/day'

   elseif(cvar(1:lv).eq.'src8') then
   ivar_type=3
   ierr= RAMS_getvar('scrsc008',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/day para kg/day
   cdname='emission 5'
   cdunits='kg/m2/day'

elseif(cvar(1:lv).eq.'COstc') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP002',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='CO Conc. without conv. transp'
   cdunits='ppb'

elseif(cvar(1:lv).eq.'COANT') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP004',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='CO Concentration ANTRO'
   cdunits='ppb'

elseif(cvar(1:lv).eq.'COTOT') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP005',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
  call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='CO Conc ANTRO+BB'
   cdunits='ppb'

elseif(cvar(1:lv).eq.'PM25old') then
   ivar_type=3
   ierr= RAMS_getvar('SCLP003',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='PM25 Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'ash_src') then
   ivar_type=2
   call RAMS_comp_zero(n1,n2,n3,a)
   ierr= RAMS_getvar('v_ash1_SRC',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash2_SRC',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash3_SRC',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash4_SRC',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash5_SRC',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash6_SRC',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash7_SRC',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash8_SRC',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash9_SRC',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash10_SRC',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de ug/kg para kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   !call RAMS_comp_mult(n1,n2,n3,a,d) !Unit: kg[pm25]/m3
   call RAMS_comp_vertint(n1,n2,n3,a,e,ngrd) ! Unit: kg[pm25]/m2
   call RAMS_comp_mults(n1,n2,n3,a,1.e+6)  ! converte de kg/m2 para mg/m2

   cdname='total ash vert integrated'
   cdunits='mg/m2'

elseif(cvar(1:lv).eq.'ashint') then
   ivar_type=2
   call RAMS_comp_zero(n1,n2,n3,a)
   ierr= RAMS_getvar('v_ash1P',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash2P',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash3P',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash4P',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash5P',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash6P',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash7P',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash8P',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash9P',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash10P',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de ug/kg para kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d) !Unit: kg[pm25]/m3
   call RAMS_comp_vertint(n1,n2,n3,a,e,ngrd) ! Unit: kg[pm25]/m2
   call RAMS_comp_mults(n1,n2,n3,a,1.e+6)  ! converte de kg/m2 para mg/m2

   cdname='total ash vert integrated'
   cdunits='mg/m2'


elseif(cvar(1:lv).eq.'ashdd_tot') then
   ivar_type=2
   call RAMS_comp_zero(n1,n2,n3,a)
   ierr= RAMS_getvar('v_ash1DD',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash2DD',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash3DD',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash4DD',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash5DD',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash6DD',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash7DD',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash8DD',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash9DD',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('v_ash10DD',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='Dry deposition total mass ash'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'ash1dd') then
   ivar_type=2
   ierr= RAMS_getvar('v_ash1DD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='Dry deposition mass ash 1'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'ash2dd') then
   ivar_type=2
   ierr= RAMS_getvar('v_ash2DD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='Dry deposition mass ash 2'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'ash3dd') then
   ivar_type=2
   ierr= RAMS_getvar('v_ash3DD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='Dry deposition mass ash 3'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'ash4dd') then
   ivar_type=2
   ierr= RAMS_getvar('v_ash4DD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='Dry deposition mass ash 4'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'ash5dd') then
   ivar_type=2
   ierr= RAMS_getvar('v_ash5DD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='Dry deposition mass ash 5'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'ash6dd') then
   ivar_type=2
   ierr= RAMS_getvar('v_ash6DD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='Dry deposition mass ash 6'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'ash7dd') then
   ivar_type=2
   ierr= RAMS_getvar('v_ash7DD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='Dry deposition mass ash 7'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'ash8dd') then
   ivar_type=2
   ierr= RAMS_getvar('v_ash8DD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='Dry deposition mass ash 8'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'ash9dd') then
   ivar_type=2
   ierr= RAMS_getvar('v_ash9DD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='Dry deposition mass ash 9'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'ash10dd') then
   ivar_type=2
   ierr= RAMS_getvar('v_ash10DD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='Dry deposition mass ash 10'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'ash1') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash1P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='ASH bin 1 Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'ash2') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash2P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='ASH bin 2 Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'ash3') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash3P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='ASH bin 3 Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'ash4') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash4P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='ASH bin 4 Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'ash5') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash5P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='ASH bin 5 Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'ash6') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash6P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='ASH bin 6 Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'ash7') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash7P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='ASH bin 7 Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'ash8') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash8P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='ASH bin 8 Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'ash9') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash9P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='ASH bin 9 Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'ash10') then
   ivar_type=3
   ierr= RAMS_getvar('v_ash10P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='ASH bin 10 Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'PM25bb') then
   ivar_type=3
   ierr= RAMS_getvar('bburn2P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='PM25 bburn Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'marin1') then
   ivar_type=3
   ierr= RAMS_getvar('marin1P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='marin 1 Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'PM25urb') then
   ivar_type=3
   ierr= RAMS_getvar('urban2P',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('urban3P',idim_type,ngrd,c,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='PM25 urban +backg Concentration'
   cdunits='ug/m3'
elseif(cvar(1:lv).eq.'PM25con') then
   ivar_type=3
   ierr= RAMS_getvar('urban3P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='PM25 backg Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'PM25u_src') then
   ivar_type=2
   ierr= RAMS_getvar('urban2_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   call RAMS_comp_noneg(n1,n2,n3,a)
!   call RAMS_transf_ppb(n1,n2,n3,a)
   cdname='PM25 urb src'
   cdunits='kg/kg/day'


elseif(cvar(1:lv).eq.'PM25bbdd') then
   ivar_type=2
   ierr= RAMS_getvar('bburn2DD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='Dry deposition mass tracer PM25'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'PM25bbwd') then
   ivar_type=2
   ierr= RAMS_getvar('bburn2WD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='WET deposition mass tracer PM25'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'PM10dd') then
   ivar_type=2
   ierr= RAMS_getvar('bburn3DD',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
   cdname='Dry deposition mass tracer PM10'
   cdunits='kg/m2'


elseif(cvar(1:lv).eq.'PM10') then
   ivar_type=3
   ierr= RAMS_getvar('bburn3P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='PM10 bburn Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'SO4') then
   ivar_type=3
   ierr= RAMS_getvar('urban2P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de mg/kg para kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='SO4 Concentration'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'SO2INT') then
   ivar_type=2
   ierr= RAMS_getvar('SO2P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de ug/kg para kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d) !Unit: kg[pm25]/m3
   call RAMS_comp_vertint(n1,n2,n3,a,e,ngrd) ! Unit: kg[pm25]/m2
   call RAMS_comp_mults(n1,n2,n3,a,1.e+6)  ! converte de kg/m2 para mg/m2

   cdname='SO2 vert int'
   cdunits='mg/m2'

elseif(cvar(1:lv).eq.'PMBBINT') then
   ivar_type=2
   ierr= RAMS_getvar('bburn2P',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de ug/kg para kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d) !Unit: kg[pm25]/m3
   call RAMS_comp_vertint(n1,n2,n3,a,e,ngrd) ! Unit: kg[pm25]/m2
   call RAMS_comp_mults(n1,n2,n3,a,1.e+6)  ! converte de kg/m2 para mg/m2

   cdname='PM25 bburn vert int'
   cdunits='mg/m2'

elseif(cvar(1:lv).eq.'PMINT') then
   ivar_type=2
   ierr= RAMS_getvar('bburn2P',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('urban2P',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('urban3P',idim_type,ngrd,e,b,flnm)

   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,e)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de ug/kg para kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d) !Unit: kg[pm25]/m3
   call RAMS_comp_vertint(n1,n2,n3,a,e,ngrd) ! Unit: kg[pm25]/m2
   call RAMS_comp_mults(n1,n2,n3,a,1.e+6)  ! converte de kg/m2 para mg/m2

   cdname='PM2.5 bburn+antro+backg vert int'
   cdunits='mg/m2'

elseif(cvar(1:lv).eq.'PM25dd') then
   ivar_type=2
   ierr= RAMS_getvar('bburn2DD',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('urban2DD',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_accum(n1,n2,n3,a,c)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='Dry deposition mass tracer PM25'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'PM25wd') then
   ivar_type=2
   ierr= RAMS_getvar('bburn2WD',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('urban2WD',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_accum(n1,n2,n3,a,c)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='WET deposition mass tracer PM25'
   cdunits='kg/m2'
elseif(cvar(1:lv).eq.'PM25') then
   ivar_type=3
   ierr= RAMS_getvar('bburn2P',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('urban2P',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('urban3P',idim_type,ngrd,e,b,flnm)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,c)
   if(ierr==0)call RAMS_comp_accum(n1,n2,n3,a,e)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='PM25 bburn+antro+backg Concentration'
   cdunits='ug/m3'

!----------------------------matrix aerosols ----------------
elseif(cvar(1:lv).eq.'ssc_seas') then
   ivar_type=3
   ierr= RAMS_getvar('ssc_seasP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
  !air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='aer marin accum'
   cdunits='ug/m3'
elseif(cvar(1:lv).eq.'ssa_seas') then
   ivar_type=3
   ierr= RAMS_getvar('ssa_seasP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
  !air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='aer marin coarse'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'bc1_bcar') then
   ivar_type=3
   ierr= RAMS_getvar('bc1_bcarP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
  !air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='aer bc1_bcar'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'bc1_bcarSRC') then
   ivar_type=3
   ierr= RAMS_getvar('bc1_bcar_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='aer SRC bc1_bcar'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'boc_bcarSRC') then
   ivar_type=3
   ierr= RAMS_getvar('boc_bcar_SRC',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
   cdname='aer SRC boc_bcar'
   cdunits='kg/kg/day'

elseif(cvar(1:lv).eq.'boc_bcar') then
   ivar_type=3
   ierr= RAMS_getvar('boc_bcarP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
  !air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='aer boc_bcar'
   cdunits='ug/m3'

elseif(cvar(1:lv).eq.'occ_ocar') then
   ivar_type=3
   ierr= RAMS_getvar('occ_ocarP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
  !air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='aer occ_ocar'
   cdunits='ug/m3'
elseif(cvar(1:lv).eq.'boc_ocar') then
   ivar_type=3
   ierr= RAMS_getvar('boc_ocarP',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-9)  ! converte de 1e-9 kg/kg para 1 kg/kg
  !air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)

   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='aer boc_ocar'
   cdunits='ug/m3'


elseif(cvar(1:lv).eq.'nakk') then
   ivar_type=3
   ierr= RAMS_getvar('numb_akkP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)
   cdname='# Sulfate Aitken mode'
   cdunits='#/cm3'

elseif(cvar(1:lv).eq.'nacc') then
   ivar_type=3
   ierr= RAMS_getvar('numb_accP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)
   cdname='# Sulfate accum. mode'
   cdunits='#/cm3'

elseif(cvar(1:lv).eq.'nboc') then
   ivar_type=3
   ierr= RAMS_getvar('numb_bocP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)
   cdname='# Black Carbon + Organic Carbon+ Sulfate'
   cdunits='#/cm3'

elseif(cvar(1:lv).eq.'nocc') then
   ivar_type=3
   ierr= RAMS_getvar('numb_occP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)
   cdname='# Organic carbon + Sulfate'
   cdunits='#/cm3'

elseif(cvar(1:lv).eq.'nmxx') then
   ivar_type=3
   ierr= RAMS_getvar('numb_mxxP',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)
   cdname='# Black + Organic Carbon+ Sulf+Dust+SeaSalt'
   cdunits='#/cm3'




! ------------------ AOT ------------------
! WAVE / 0.256, 0.280, 0.296, 0.319, 0.335, 0.365, 0.420, 0.482,
!	 0.598, 0.690, 0.762, 0.719, 0.813, 0.862, 0.926, 1.005,
!	 1.111, 1.333, 1.562, 1.770, 2.051, 2.210, 2.584, 3.284,
!	 3.809, 4.292,
!	 4.546, 4.878, 5.128, 5.405, 5.714, 6.061, 6.452, 6.897,
!	 7.407, 8.333, 9.009, 10.309,12.500,13.889,16.667,
!	 20.000, 26.316, 35.714, 62.50
elseif(cvar(1:lv).eq.'aot256') then
   ivar_type=2
   ierr= RAMS_getvar('AOT',idim_type,ngrd,aot,b,flnm)
   call D3toD2(n1,n2,nwave,1,a,aot)
   cdname='AOT 256nm'
   cdunits=' '

elseif(cvar(1:lv).eq.'aot296') then
   ivar_type=2
   ierr= RAMS_getvar('AOT',idim_type,ngrd,aot,b,flnm)
   call D3toD2(n1,n2,nwave,3,a,aot)
   cdname='AOT 296nm'
   cdunits=' '

elseif(cvar(1:lv).eq.'aot335') then
   ivar_type=2
   ierr= RAMS_getvar('AOT',idim_type,ngrd,aot,b,flnm)
   call D3toD2(n1,n2,nwave,5,a,aot)
   cdname='AOT 335nm'
   cdunits=' '

elseif(cvar(1:lv).eq.'aot420') then
   ivar_type=2
   ierr= RAMS_getvar('AOT',idim_type,ngrd,aot,b,flnm)
   call D3toD2(n1,n2,nwave,7,a,aot)
   cdname='AOT 420nm'
   cdunits=' '

elseif(cvar(1:lv).eq.'aot440') then
   ivar_type=2
   ierr= RAMS_getvar('AOT',idim_type,ngrd,aot,b,flnm)
   call D3toD2(n1,n2,nwave,8,a,aot)
   cdname='AOT 440nm'
   cdunits=' '

elseif(cvar(1:lv).eq.'aot482') then
   ivar_type=2
   ierr= RAMS_getvar('AOT',idim_type,ngrd,aot,b,flnm)
   call D3toD2(n1,n2,nwave,8,a,aot)
   cdname='AOT 482nm'
   cdunits=' '


elseif(cvar(1:lv).eq.'aot598') then
   ivar_type=2
   ierr= RAMS_getvar('AOT',idim_type,ngrd,aot,b,flnm)
   call D3toD2(n1,n2,nwave,9,a,aot)
   cdname='AOT 598nm'
   cdunits=' '

elseif(cvar(1:lv).eq.'aot690') then
   ivar_type=2
   ierr= RAMS_getvar('AOT',idim_type,ngrd,aot,b,flnm)
   call D3toD2(n1,n2,nwave,10,a,aot)
   cdname='AOT 690nm'
   cdunits=' '

elseif(cvar(1:lv).eq.'aot500') then
   ivar_type=2
!   ierr= RAMS_getvar('AOT',idim_type,ngrd,c,b,flnm)
   ierr= RAMS_getvar('AOT',idim_type,ngrd,aot,b,flnm)
   call D3toD2(n1,n2,nwave,11,a,aot)
   cdname='AOT 500nm'
   cdunits=' '

elseif(cvar(1:lv).eq.'aot550') then
   ivar_type=2
   ierr= RAMS_getvar('AOT',idim_type,ngrd,aot,b,flnm)
   call D3toD2(n1,n2,nwave,12,a,aot)
   cdname='AOT 550nm'
   cdunits=' '

elseif(cvar(1:lv).eq.'aot660') then
   ivar_type=2
   ierr= RAMS_getvar('AOT',idim_type,ngrd,aot,b,flnm)
   call D3toD2(n1,n2,nwave,14,a,aot)
   cdname='AOT 660nm'
   cdunits=' '


elseif(cvar(1:lv).eq.'aot862') then
   ivar_type=2
   ierr= RAMS_getvar('AOT',idim_type,ngrd,aot,b,flnm)
   call D3toD2(n1,n2,nwave,20,a,aot)
   cdname='AOT 862nm'
   cdunits=' '


elseif(cvar(1:lv).eq.'secog') then
   ivar_type=2
   ierr= RAMS_getvar('DUM1',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,2,a,c)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/day para kg/day
   cdname='GOES-8 ABBA CO emission'
   cdunits='kg/m2/day'


elseif(cvar(1:lv).eq.'secod') then
   ivar_type=2
   ierr= RAMS_getvar('DUM1',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,11,a,c)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/day para kg/day
   cdname='Duncan CO emission'
   cdunits='kg/m2/day'

elseif(cvar(1:lv).eq.'secoant') then
   ivar_type=2
   ierr= RAMS_getvar('DUM1',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,11,a,c)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/day para kg/day
   cdname='Antropogenic CO emission'
   cdunits='kg/m2/day'

elseif(cvar(1:lv).eq.'secoe') then
   ivar_type=2
   ierr= RAMS_getvar('DUM1',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,14,a,c)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/day para kg/day
   cdname='EDGAR CO emission'
   cdunits='kg/m2/day'


elseif(cvar(1:lv).eq.'scco') then
   ivar_type=2
   ierr= RAMS_getvar('scrsc001',idim_type,ngrd,a,b,flnm)
   cdname='Massa de CO emitida'
   cdunits='kg/(m2 day)'

elseif(cvar(1:lv).eq.'scpm25') then
   ivar_type=2
   ierr= RAMS_getvar('scrsc003',idim_type,ngrd,a,b,flnm)
   cdname='Massa de PM25 emitida'
   cdunits='kg/(m2 day)'

elseif(cvar(1:lv).eq.'sccofe') then
   ivar_type=2
   ierr= RAMS_getvar('QSC3',idim_type,ngrd,a,b,flnm)
   cdname='Massa de CO FWB - EDGAR emitida'
   cdunits='kg/(m2 day)'

elseif(cvar(1:lv).eq.'sccoae') then
   ivar_type=2
   ierr= RAMS_getvar('QSC4',idim_type,ngrd,a,b,flnm)
   cdname='Massa de CO AWB - EDGAR emitida'
   cdunits='kg/(m2 day)'

elseif(cvar(1:lv).eq.'sccobbe') then
   ivar_type=2
   ierr= RAMS_getvar('QSC5',idim_type,ngrd,a,b,flnm)
   cdname='Massa de CO BB - EDGAR emitida'
   cdunits='kg/(m2 day)'

elseif(cvar(1:lv).eq.'sccod') then
   ivar_type=2
   ierr= RAMS_getvar('QSC9',idim_type,ngrd,a,b,flnm)
   cdname='Massa de CO Duncan emitida'
   cdunits='kg/(m2 day)'

elseif(cvar(1:lv).eq.'sccol') then
   ivar_type=2
   ierr= RAMS_getvar('QSC3',idim_type,ngrd,a,b,flnm)
   cdname='Massa de CO emitida -logan'
   cdunits='kg/(m2 day)'

elseif(cvar(1:lv).eq.'sccoant') then
   ivar_type=2
   ierr= RAMS_getvar('scrsc004',idim_type,ngrd,a,b,flnm)
   cdname='Massa de CO emitida -ANTROPO'
   cdunits='kg/(m2 day)'

elseif(cvar(1:lv).eq.'pwv') then
   ivar_type=2
   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd) ! d=dens_ar
   call RAMS_comp_mult(n1,n2,n3,a,d)         ! aqui a=rv*dens_ar
   call RAMS_comp_vertint(n1,n2,n3,a,e,ngrd) ! agua em kg/m^2
   call RAMS_comp_mults(n1,n2,n3,a,0.1) !converte para cm = 1 kg/m^2 * 100 cm/m / (1000 kg/m^3 dens_agua)
   cdname='precipitable water vapor'
   cdunits='cm'

elseif(cvar(1:lv).eq.'pwt') then
   ivar_type=2

   ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)
   ierr= RAMS_getvar('RCP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RRP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RPP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RSP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RAP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RGP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)
   ierr= RAMS_getvar('RHP',idim_type,ngrd,c,b,flnm)
   if(ierr.eq.0) call RAMS_comp_accum(n1,n2,n3,a,c)

   call RAMS_comp_noneg(n1,n2,n3,a)

!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd) ! d=dens_ar
   call RAMS_comp_mult(n1,n2,n3,a,d)         ! aqui a=rv*dens_ar
   call RAMS_comp_vertint(n1,n2,n3,a,e,ngrd) ! agua em kg/m^2
   call RAMS_comp_mults(n1,n2,n3,a,0.1) !converte para cm = 1 kg/m^2 * 100 cm/m / (1000 kg/m^3 dens_agua)
   cdname='precipitable total water'
   cdunits='cm'


elseif(cvar(1:lv).eq.'pmyfx') then
   ivar_type=2
   ierr= RAMS_getvar('SCLP003',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
!air density
   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,b,c,d,e,ngrd)
   call RAMS_transf_ugm3(n1,n2,n3,a,d)
   call RAMS_comp_noneg(n1,n2,n3,a) ! a= dens (pm2.5)

! meridional wind
   ierr=RAMS_getvar('VP',idim_type,ngrd,e,b,flnm)
   ierr=RAMS_getvar('UP',idim_type,ngrd,c,b,flnm)
   call RAMS_comp_rotate(n1,n2,n3,c,e,ngrd)
   call RAMS_comp_avgv(n1,n2,n3,e)

! get flux  dens * ve
   call RAMS_comp_mult(n1,n2,n3,a,e)

   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
! a= integral( dens * dz )
   call RAMS_comp_vertint(n1,n2,n3,a,e,ngrd) ! unit:

   call get_flux_x(n1,n2,n3,a,ngrd)


   cdname='PM25 meridional flux vert integ'
   cdunits='ug/s'

  elseif(cvar(1:lv).eq.'akmin') then
   ivar_type=2
   ierr= RAMS_getvar('AKMIN2D',idim_type,ngrd,a,b,flnm)
   cdname='akmin 2d'
   cdunits=' #'

!  elseif(cvar(1:lv).eq.'CO2') then
!   ivar_type=3
!   ierr= RAMS_getvar('SCLP004',idim_type,ngrd,a,b,flnm)
!  call RAMS_comp_noneg(n1,n2,n3,a)
!!!!   call RAMS_transf_ppm(n1,n2,n3,a)
!   cdname='CO2 Concentration'
!   cdunits='ppm'

elseif(cvar(1:lv).eq.'TKUO') then
   ivar_type=3
   ierr= RAMS_getvar('DUM3',idim_type,ngrd,a,b,flnm)
   call RAMS_transf_ppb_day(n1,n2,n3,a)
   cdname='CO tend conc due conv trans'
   cdunits='ppb/day'

elseif(cvar(1:lv).eq.'TKUOSH') then
   ivar_type=3
   ierr= RAMS_getvar('DUM8',idim_type,ngrd,a,b,flnm)
   call RAMS_transf_ppb_day(n1,n2,n3,a)
   cdname='CO tend conc due Shallow conv trans'
   cdunits='ppb/day'



! ------------------------ Stilt-RAMS coupling------------
elseif(cvar(1:lv).eq.'afxu') then
   ivar_type=3
   ierr= RAMS_getvar('AFXU',idim_type,ngrd,a,b,flnm)
   cdname='adv u flux'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'afxub') then
   ivar_type=3
   ierr= RAMS_getvar('AFXUB',idim_type,ngrd,a,b,flnm)
   cdname='averaged adv u flux'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'afxv') then
   ivar_type=3
   ierr= RAMS_getvar('AFXV',idim_type,ngrd,a,b,flnm)
   cdname='adv v flux'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'afxvb') then
   ivar_type=3
   ierr= RAMS_getvar('AFXVB',idim_type,ngrd,a,b,flnm)
   cdname='averaged adv v flux'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'afxw') then
   ivar_type=3
   ierr= RAMS_getvar('AFXW',idim_type,ngrd,a,b,flnm)
   cdname='adv w flux'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'afxwb') then
   ivar_type=3
   ierr= RAMS_getvar('AFXWB',idim_type,ngrd,a,b,flnm)
   cdname='averaged adv W flux'
   cdunits='kg/m^2s'


elseif(cvar(1:lv).eq.'sigw') then
   ivar_type=3
   ierr= RAMS_getvar('SIGW',idim_type,ngrd,a,b,flnm)
   cdname=' sigma W'
   cdunits=''

elseif(cvar(1:lv).eq.'sigwb') then
   ivar_type=3
   ierr= RAMS_getvar('SIGWB',idim_type,ngrd,a,b,flnm)
   cdname='averaged sigma W'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'tlb') then
   ivar_type=3
   ierr= RAMS_getvar('TLB',idim_type,ngrd,a,b,flnm)
   cdname='averaged Lagr timescale'
   cdunits='s'

elseif(cvar(1:lv).eq.'tl') then
   ivar_type=3
   ierr= RAMS_getvar('TL',idim_type,ngrd,a,b,flnm)
   cdname='Lagr timescale'
   cdunits='s'

elseif(cvar(1:lv).eq.'lmo') then
   ivar_type=2
   ierr= RAMS_getvar('LMO',idim_type,ngrd,a,b,flnm)
   cdname=' '
   cdunits=' '
elseif(cvar(1:lv).eq.'pblhgt') then
   ivar_type=2
   ierr= RAMS_getvar('PBLHGT',idim_type,ngrd,a,b,flnm)
   cdname=' PBL height '
   cdunits=' m'

elseif(cvar(1:lv).eq.'tkeb') then
   ivar_type=3
   ierr= RAMS_getvar('TKEPB',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_noneg(n1,n2,n3,a)
   cdname='average turb kinetic energy'
   cdunits='m2/s2'

elseif(cvar(1:lv).eq.'facup1') then
   ivar_type=3
   ierr= RAMS_getvar('FACUP1',idim_type,ngrd,a,b,flnm)
   cdname='frac area cov up -deep'
   cdunits=''

elseif(cvar(1:lv).eq.'facup2') then
   ivar_type=3
   ierr= RAMS_getvar('FACUP2',idim_type,ngrd,a,b,flnm)
   cdname='frac area cov up -shal'
   cdunits=''

elseif(cvar(1:lv).eq.'facdn1') then
   ivar_type=3
   ierr= RAMS_getvar('FACDN1',idim_type,ngrd,a,b,flnm)
   cdname='frac area cov down -deep'
   cdunits=''


elseif(cvar(1:lv).eq.'cfxup1') then
   ivar_type=3
   ierr= RAMS_getvar('CFXUP1',idim_type,ngrd,a,b,flnm)
   cdname='conv up flux deep'
   cdunits='kg/m^2s'

 elseif(cvar(1:lv).eq.'cfxup2') then
   ivar_type=3
   ierr= RAMS_getvar('CFXUP2',idim_type,ngrd,a,b,flnm)
   cdname='conv up flux shallow'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'cfxdn1') then
   ivar_type=3
   ierr= RAMS_getvar('CFXDN1',idim_type,ngrd,a,b,flnm)
   cdname='conv down flux deep'
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'dfxup1') then
   ivar_type=3
   ierr= RAMS_getvar('DFXUP1',idim_type,ngrd,a,b,flnm)
   cdname='deep conv flx up->env '
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'efxup1') then
   ivar_type=3
   ierr= RAMS_getvar('EFXUP1',idim_type,ngrd,a,b,flnm)
   cdname='deep conv flx env->up '
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'dfxdn1') then
   ivar_type=3
   ierr= RAMS_getvar('DFXDN1',idim_type,ngrd,a,b,flnm)
   cdname='deep conv flx down->env '
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'efxdn1') then
   ivar_type=3
   ierr= RAMS_getvar('EFXDN1',idim_type,ngrd,a,b,flnm)
   cdname='deep conv flx env->down'
   cdunits='kg/m^2s'


elseif(cvar(1:lv).eq.'dfxup2') then
   ivar_type=3
   ierr= RAMS_getvar('DFXUP2',idim_type,ngrd,a,b,flnm)
   cdname='shallow conv flx up->env '
   cdunits='kg/m^2s'

elseif(cvar(1:lv).eq.'efxup2') then
   ivar_type=3
   ierr= RAMS_getvar('EFXUP2',idim_type,ngrd,a,b,flnm)
   cdname='shallow conv flx env -> up'
   cdunits='kg/m^2s'



!------------Grell cumulus scheme --------------------------

elseif(cvar(1:lv).eq.'wdm1') then
   ivar_type=2
   ierr= RAMS_getvar('wetdep001',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   cdname='Wet deposition mass tracer 1'
   cdunits='kg/m2'


elseif(cvar(1:lv).eq.'wdm3') then
   ivar_type=2
   ierr= RAMS_getvar('wetdep003',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   cdname='Wet deposition mass tracer 3'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'wdmass') then
   ivar_type=2
   ierr= RAMS_getvar('wetdep003',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   cdname='Wet deposition mass tracer 3'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'ddm1') then
   ivar_type=2
   ierr= RAMS_getvar('SCDD001',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   cdname='Dry deposition mass tracer 1'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'ddm3') then
   ivar_type=2
   ierr= RAMS_getvar('SCDD003',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,1.e-6)  ! converte de mg/kg para kg/kg
   cdname='Dry deposition mass tracer 3'
   cdunits='kg/m2'

elseif(cvar(1:lv).eq.'vdep3') then
   ivar_type=2
   ierr= RAMS_getvar('d2004',idim_type,ngrd,a,b,flnm)
   cdname='dry dep velocity tracer 3'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'ierr') then
   ivar_type=2
   ierr= RAMS_getvar('XIERR',idim_type,ngrd,c,b,flnm)
   call set_undef(n1,n2,n3,a,c)
   cdname='ierr'
   cdunits=' '
elseif(cvar(1:lv).eq.'err') then
   ivar_type=2
   ierr= RAMS_getvar('d2001',idim_type,ngrd,c,b,flnm)
   call set_undef(n1,n2,n3,a,c)
   cdname='ierr'
   cdunits=' '

elseif(cvar(1:lv).eq.'ierrsh') then
   ivar_type=2
   ierr= RAMS_getvar('XIERRSH',idim_type,ngrd,c,b,flnm)
   call set_undef(n1,n2,n3,a,c)
   cdname='ierr'
   cdunits=' '

elseif(cvar(1:lv).eq.'upmf') then
   ivar_type=2
   ierr= RAMS_getvar('UPMF',idim_type,ngrd,c,b,flnm)
   call set_undef(n1,n2,n3,a,c)
   cdname='updraft mass flux'
   cdunits='kg/(m^2 s)'

elseif(cvar(1:lv).eq.'dnmf') then
   ivar_type=2
   ierr= RAMS_getvar('DNMF',idim_type,ngrd,c,b,flnm)
   call set_undef(n1,n2,n3,a,c)
   cdname='downdraft mass flux'
   cdunits='kg/(m^2 s)'


elseif(cvar(1:lv).eq.'shmf') then
   ivar_type=2
   ierr= RAMS_getvar('UPMFSH',idim_type,ngrd,c,b,flnm)
   call set_undef(n1,n2,n3,a,c)
   cdname='shallow cum mass flux'
   cdunits='kg/(m^2 s)'

elseif(cvar(1:lv).eq.'shmf1') then
   ivar_type=2
   ierr= RAMS_getvar('SHMF',idim_type,ngrd,c,b,flnm)
   call set_undef(n1,n2,n3,a,c)
   cdname='shallow cum mass flux for nnshcu=1'
   cdunits='kg/(m^2 s)'

elseif(cvar(1:lv).eq.'shmf3') then
   ivar_type=2
   ierr= RAMS_getvar('UPMFSH3',idim_type,ngrd,c,b,flnm)
   call set_undef(n1,n2,n3,a,c)
   cdname='shallow cum mass flux - gf scheme'
   cdunits='kg/(m^2 s)'

elseif(cvar(1:lv).eq.'lsfth') then
   ivar_type=3
   ierr= RAMS_getvar('lsfth',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
    cdname='DEEP forcing theta'
   cdunits='K/day'

elseif(cvar(1:lv).eq.'lsfrt') then
   ivar_type=3
   ierr= RAMS_getvar('lsfrt',idim_type,ngrd,a,b,flnm)
!   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   call RAMS_comp_mults(n1,n2,n3,a,86400.*1000.)
    cdname=' DEEP forcing water vapor'
    cdunits='g/kg/day'

elseif(cvar(1:lv).eq.'lsfthsh') then
   ivar_type=3
   ierr= RAMS_getvar('lsfthSH',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_mults(n1,n2,n3,a,86400.)
    cdname='Shallow forcing theta'
   cdunits='K/day'

elseif(cvar(1:lv).eq.'lsfrtsh') then
   ivar_type=3
   ierr= RAMS_getvar('lsfrtSH',idim_type,ngrd,a,b,flnm)
!   call RAMS_comp_mults(n1,n2,n3,a,86400.)
   call RAMS_comp_mults(n1,n2,n3,a,86400.*1000.)
    cdname='Shallow forcing water vapor'
    cdunits='g/kg/day'


elseif(cvar(1:lv).eq.'topcl') then
   ivar_type=2
   ierr= RAMS_getvar('XKTOP',idim_type,ngrd,c,b,flnm)
   call set_undef(n1,n2,n3,a,c)
   cdname='Cloud top'
   cdunits=' '


elseif(cvar(1:lv).eq.'jmin') then
   ivar_type=2
   ierr= RAMS_getvar('XJMIN',idim_type,ngrd,c,b,flnm)
   call set_undef(n1,n2,n3,a,c)
   cdname='Down starts level'
   cdunits=' '

elseif(cvar(1:lv).eq.'rtgt') then
   ivar_type=2

   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_rtgt(n1,n2,n3,a,e,ngrd)

   cdname='RTGT met factor'
   cdunits='#'


elseif(cvar(1:lv).eq.'cprtint') then
   ivar_type=2

   ierr= RAMS_getvar('TOPT',idim_type,ngrd,e,b,flnm)
   call RAMS_comp_dn0(n1,n2,n3,c,b,d,e,ngrd)
   ierr= RAMS_getvar('RTSRC',idim_type,ngrd,a,b,flnm)

   call RAMS_comp_mult(n1,n2,n3,a,d)
   call RAMS_comp_vertint(n1,n2,n3,a,e,ngrd)

   cdname='vertint cp rt'
   cdunits='kg/m2*s'

elseif(cvar(1:lv).eq.'xave') then
   ivar_type=2
   ierr= RAMS_getvar('DUM5',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,2,a,c)
   cdname='X_AVE'
   cdunits=' '

elseif(cvar(1:lv).eq.'xavec1') then
   ivar_type=2
   ierr= RAMS_getvar('DUM5',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,16,a,c)
   cdname='X_AVE Capmax'
   cdunits=' '

elseif(cvar(1:lv).eq.'xavec2') then
   ivar_type=2
   ierr= RAMS_getvar('DUM5',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,17,a,c)
   cdname='X_AVE Capmax'
   cdunits=' '

elseif(cvar(1:lv).eq.'xavec3') then
   ivar_type=2
   ierr= RAMS_getvar('DUM5',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,18,a,c)
   cdname='X_AVE Capmax'
   cdunits=' '

!****************

elseif(cvar(1:lv).eq.'xff0') then
   ivar_type=2
   ierr= RAMS_getvar('d2003',idim_type,ngrd,a,b,flnm)
   cdname='XFF0 for deep'
   cdunits=' '
elseif(cvar(1:lv).eq.'xff0sh') then
   ivar_type=2
   ierr= RAMS_getvar('d2002',idim_type,ngrd,a,b,flnm)
   cdname='XFF0 for shallow'
   cdunits=' '

elseif(cvar(1:lv).eq.'prgr1') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,16,a,c)
   cdname=' precip closure 1 large cap'
   cdunits=' mm/h'

elseif(cvar(1:lv).eq.'prgr2') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,17,a,c)
   cdname=' precip closure 1 medium cap'
   cdunits=' mm/h'

elseif(cvar(1:lv).eq.'prgr3') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,18,a,c)
   cdname=' precip closure 1 low cap'
   cdunits=' mm/h'

elseif(cvar(1:lv).eq.'prw1') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,19,a,c)
   cdname=' precip closure 2 large cap'
   cdunits=' mm/h'

elseif(cvar(1:lv).eq.'prw2') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,20,a,c)
   cdname=' precip closure 2 medium cap'
   cdunits=' mm/h'

elseif(cvar(1:lv).eq.'prw3') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,21,a,c)
   cdname=' precip closure 2 low cap'
   cdunits=' mm/h'

elseif(cvar(1:lv).eq.'prmc1') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,22,a,c)
   cdname=' precip closure 3 large cap'
   cdunits=' mm/h'

elseif(cvar(1:lv).eq.'prmc2') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,23,a,c)
   cdname=' precip closure 3 medium cap'
   cdunits=' mm/h'

elseif(cvar(1:lv).eq.'prmc3') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,24,a,c)
   cdname=' precip closure 3 low cap'
   cdunits=' mm/h'


elseif(cvar(1:lv).eq.'prst1') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,25,a,c)
   cdname=' precip closure 4 large cap'
   cdunits=' mm/h'

elseif(cvar(1:lv).eq.'prst2') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,26,a,c)
   cdname=' precip closure 4 medium cap'
   cdunits=' mm/h'

elseif(cvar(1:lv).eq.'prst3') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,27,a,c)
   cdname=' precip closure 4 low cap'
   cdunits=' mm/h'


elseif(cvar(1:lv).eq.'pras1') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,28,a,c)
   cdname=' precip closure 5 large cap'
   cdunits=' mm/h'

elseif(cvar(1:lv).eq.'pras2') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,29,a,c)
   cdname=' precip closure 5 medium cap'
   cdunits=' mm/h'

elseif(cvar(1:lv).eq.'pras3') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,30,a,c)
   cdname=' precip closure 5 low cap'
   cdunits=' mm/h'

elseif(cvar(1:lv).eq.'prasm') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2m(n1,n2,n3,28,a,c)
   cdname=' precip closure AS medium'
   cdunits=' mm/h'

elseif(cvar(1:lv).eq.'prgrm') then
   ivar_type=2
   ierr= RAMS_getvar('d3004',idim_type,ngrd,c,b,flnm)
   call D3toD2m(n1,n2,n3,16,a,c)
   cdname=' precip closure GR medium'
   cdunits=' mm/h'

elseif(cvar(1:lv).eq.'xstd') then
   ivar_type=2
   ierr= RAMS_getvar('DUM5',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,3,a,c)
   cdname='X_STD'
   cdunits=' '

elseif(cvar(1:lv).eq.'xske') then
   ivar_type=2
   ierr= RAMS_getvar('DUM5',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,4,a,c)
   cdname='x_ske'
   cdunits=' '

elseif(cvar(1:lv).eq.'xcur') then
   ivar_type=2
   ierr= RAMS_getvar('DUM5',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,5,a,c)
   cdname='x_cur'
   cdunits=' '



elseif(cvar(1:lv).eq.'xmbgr') then
   ivar_type=2
   ierr= RAMS_getvar('DUM5',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,6,a,c)
   cdname='xmbgr'
   cdunits=' '


elseif(cvar(1:lv).eq.'xmbw') then
   ivar_type=2
   ierr= RAMS_getvar('DUM5',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,7,a,c)
   cdname='xmbw'
   cdunits=' '

elseif(cvar(1:lv).eq.'xmbmc') then
   ivar_type=2
   ierr= RAMS_getvar('DUM5',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,8,a,c)
   cdname='xmbmc'
   cdunits=' '

elseif(cvar(1:lv).eq.'xmbst') then
   ivar_type=2
   ierr= RAMS_getvar('DUM5',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,9,a,c)
   cdname='xmbst'
   cdunits=' '

elseif(cvar(1:lv).eq.'xmbas') then
   ivar_type=2
   ierr= RAMS_getvar('DUM5',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,10,a,c)
   cdname='xmbas'
   cdunits=' '




elseif(cvar(1:lv).eq.'prgr') then
   ivar_type=2
   ierr= RAMS_getvar('d3010',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,11,a,c)
   cdname='prgr'
   cdunits=' '


elseif(cvar(1:lv).eq.'prw') then
   ivar_type=2
   ierr= RAMS_getvar('d3010',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,12,a,c)
   cdname='prw'
   cdunits=' '

elseif(cvar(1:lv).eq.'prmc') then
   ivar_type=2
   ierr= RAMS_getvar('d3010',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,13,a,c)
   cdname='prmc'
   cdunits=' '

elseif(cvar(1:lv).eq.'prst') then
   ivar_type=2
   ierr= RAMS_getvar('d3010',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,14,a,c)
   cdname='prst'
   cdunits=' '

elseif(cvar(1:lv).eq.'pras') then
   ivar_type=2
   ierr= RAMS_getvar('d3010',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,15,a,c)
   cdname='pras'
   cdunits=' '

elseif(cvar(1:lv).eq.'um') then
   ivar_type=2
   ierr= RAMS_getvar('DUM5',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,24,a,c)
   cdname='u mean'
   cdunits='m/s '

elseif(cvar(1:lv).eq.'vm') then
   ivar_type=2
   ierr= RAMS_getvar('DUM5',idim_type,ngrd,c,b,flnm)
   call D3toD2(n1,n2,n3,25,a,c)
   cdname='v mean'
   cdunits='m/s '

!srf- grell's stuff------------------------------------------------

!-srf Health Indexes in collaboration with Luis Estela (CESAM-Cuba)
elseif(cvar(1:lv).eq.'poda') then
   ivar_type=2

   !- compute the Partial Oxygen Density in the air (PODA) index
   !
   !
   !compute  dew point temperature at 1.5m height
      !--- rv_2.0m ---
      ierr= RAMS_getvar('RV2MJ',idim_type,ngrd,a,b,flnm)
      call RAMS_comp_mults(n1,n2,n3,a,1.e3)
      !--- tempk2m ---
      ierr= RAMS_getvar('T2MJ',idim_type,ngrd,c,b,flnm)
      !--- PI ---
      ierr=RAMS_getvar('PI',idim_type,ngrd,f,b,flnm)
      call RAMS_comp_dewK_2m(n1,n2,n3,a,f,c)
      !call undef (n1,n2,n3,a,1,-80.0+273,80.0+273.)
      !cdname='dewpoint temp at 2.0m height'
      !cdunits='K'
      call copy_x_to_y(n1,n2,n3,a,g)
      ! - td2m var  -> g array

   !-compute slp (hPa)
      ierr= RAMS_getvar('TOPT',idim_type,ngrd,c,b,flnm)
      ierr= RAMS_getvar('PI',idim_type,ngrd,d,b,flnm)

      ierr= RAMS_getvar('THETA',idim_type,ngrd,e,b,flnm)
      ierr= RAMS_getvar('RV',idim_type,ngrd,a,b,flnm)

      call RAMS_comp_thetv(n1,n2,n3,e,a)

      call RAMS_comp_slpmm5(n1,n2,n3,e,d,c,a)
      !cdname='sea level pressure;'
      !cdunits='hPa'
      call copy_x_to_y(n1,n2,n3,a,f)
      !- slp var -> f array

    !- compute  air temperature at 1.5m height
      !cdunits='K'
      ierr= RAMS_getvar('T2MJ',idim_type,ngrd,c,b,flnm)
      !call undef (n1,n2,n3,c,1,193.0,353.0)
      !- t2m  var -> c array

    !- compute the poda index
      call calc_poda_index(n1,n2,n3,c,f,g,a)
      cdname='PODA index'
      cdunits='g/m^3'

!-srf Health Indexes in collaboration with Luis Estela (CESAM-Cuba)

else

   print*,'Variable name not found in hvlib.f - ',cvar(1:lv)
   ivar_type=0

endif

if(ierr_getvar.eq.1) ivar_type=0
!if(ierr_getvar.eq.1.or.ifound.eq.0) ivar_type=0
!print*,'IERR_GETVAR=',ierr_getvar,ivar_type
return
end

!*******************************************************************************
!############################# Change Log ##################################
! 2.3.0.1
!
! 000830 CJT rams_comp ##
!            Corrected reference to "cpr" and "r" to "cpor" and "rgas" ##
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!  Mission Research Corporation / *ASTeR Division
!###########################################################################

subroutine RAMS_comp(n1,n2,n3,n4,n5)
  use modrconfig
dimension a(n1,n2,n3),b(n1,n2,n3),c(n1,n2,n3),e(n1,n2,n3),topt(n1,n2),d(n1,n2,n3)
dimension a2(n1,n2,n4,n5),x1lat(n1,n2)
real f1(n1,n2,n3),f2(n1,n2,n3)
dimension theta(n1,n2,n3),pp(n1,n2,n3),slp(n1,n2),z(n1,n2,n3)
include 'rconstants.h'
dimension slmsts0(12)
data slmsts0/0.395, 0.410, 0.435, 0.485, 0.451, 0.420  &
            ,0.477, 0.476, 0.426, 0.492, 0.482, 0.863/

!SRF
!  PMAR, PMCO, PMC02 = pesos moleculares do ar, CO, CO2
data PMAR/28.96/
data PMCO/28./
data PMCO2/44./
real  altmax,massx,xx1
entry RAMS_transf_ppb_day(n1,n2,n3,a)
!  PMAR e PMCO = pesos moleculares do ar e do CO
!  TRANSFORMACAO DE kg[CO]/kg[AR] para ppb (PARTE POR BILHAO)
     do k=1,n3
 	do j=1,n2
 	   do i=1,n1

     a(i,j,k)=a(i,j,k)*(PMAR/PMCO)*1.E+9*1.e-6*86400.

 	   enddo
 	enddo
     enddo
return


entry convert_from_ppbm_to_molec_by_cm3(n1,n2,n3,a,d,c)
     do k=1,n3
 	do j=1,n2
 	   do i=1,n1
     a(i,j,k)=a(i,j,k)*c(i,j,k)/(8.314*d(i,j,k))
 	   enddo
 	enddo
     enddo
return

entry copy_x_to_y(n1,n2,n3,c,a)
     do k=1,n3
 	do j=1,n2
 	   do i=1,n1
              a(i,j,k)=c(i,j,k)
 	   enddo
 	enddo
     enddo
return



entry RAMS_transf_ppb(n1,n2,n3,a,PMX)
!  transform from ppbm to ppbv
     do k=1,n3
 	do j=1,n2
 	   do i=1,n1

     a(i,j,k)=a(i,j,k)*(PMAR/PMX)

 	   enddo
 	enddo
     enddo
return

entry RAMS_transf_ppm(n1,n2,n3,a)
!  TRANSFORMACAO DE kg[CO2]/kg[AR] para ppm (PARTE POR MILHAO)
     do k=1,n3
 	do j=1,n2
 	   do i=1,n1
 	      a(i,j,k)=a(i,j,k)*(PMAR/PMCO2)*1.E+6
 	   enddo
 	enddo
     enddo
return

entry RAMS_transf_ugm3(n1,n2,n3,a,b)
!   Transformacao de conc  de kg/kg para ug/m3
    do k=1,n3
      do j=1,n2
    	do i=1,n1
        !print*,a(i,j,k),b(i,j,k)
	a(i,j,k)=a(i,j,k)*b(i,j,k)*1.e+9

    	enddo
      enddo
    enddo
return

entry get_ZI(n1,n2,n3,a,c,ngrd)
tkemin =1.E-2

      do i=1,n1
         do j=1,n2
          a(i,j,1) = (ztn(2,ngrd)+ ztn(1,ngrd))
         enddo
      enddo

      do i=1,n1
        do j=1,n2
         do k = 2,n3-1
!
!         print*,k,c(i,j,k),ztn(k,ngrd)
         if(c(i,j,k).lt.tkemin) then
            kzi = k
            a(i,j,1)=0.5*(ztn(kzi,ngrd)+ ztn(kzi-1,ngrd))
	    go to 500
	 endif
	 enddo
 500     continue
!         print*,i,j,a(i,j,1)
        enddo
      enddo
return
entry get_ZI_RELHUM(n1,n2,n3,a,c,d,ngrd)
      do i=1,n1
         do j=1,n2
          a(i,j,1) = 0.
         enddo
      enddo
!
      do i=1,n1
         do j=1,n2
! get ZI
            do k=2,n3
      if(d(i,j,k).gt.d(i,j,k+1)) go to 1778
            enddo
 1778 continue
        kzi = k
        if(abs(d(i,j,k)).gt. 0.99) then

        a(i,j,1) =0.5*(ztn(kzi,ngrd)+ ztn(kzi-1,ngrd))  &
                     *(1.-c(i,j,1)/zmn(nnzp(1)-1,1))
        else

        a(i,j,1) =0.5*(ztn(kzi,ngrd)+ ztn(kzi+1,ngrd))  &
                     *(1.-c(i,j,1)/zmn(nnzp(1)-1,1))
!        a(i,j,1) =ztn(kzi,ngrd)

        endif

        if(a(i,j,1).lt.0.) a(i,j,1)=0.
         enddo
      enddo
return

entry RAMS_comp_nebulosidade(n1,n2,n3,a,b,c,e,ngrd)
!
! b=cloud
! c=dn0
! e=topo
       do j=1,n2
           do i=1,n1
         rnebu=0.
         rodzint=0
         do k=2,n3-1
          dz=(ztn(k,ngrd)-ztn(k-1,ngrd))*(1.-e(i,j,1)/zmn(nnzp(1)-1,1))
          rnebu   = rnebu   + b(i,j,k)*c(i,j,k)*dz
          rodzint = rodzint +	       c(i,j,k)*dz
         enddo
              a(i,j,1) = 1000.*rnebu/(rodzint+1.e-5)
	 enddo
       enddo
return

entry get_grid_size(n1,n2,n3,a,ngrd)

    c1= (2. * erad) ** 2
       do j=2,n2
	  do i=2,n1
	   xt2 = xtn(i,ngrd)**2
	   yt2 = ytn(j,ngrd)**2
	 fmapt=1.+(xt2 + yt2) / c1
	 DXT=(XMn(I,ngrd)-XMn(I-1,ngrd))/fmapt
	 DYT=(YMn(J,ngrd)-YMn(J-1,ngrd))/fmapt
	 a(i,j,1) =  sqrt(DXT*DYT)
	  enddo
       enddo
return
entry get_grid_sizeX(n1,n2,n3,a,ngrd)

    c1= (2. * erad) ** 2
       do j=2,n2
	  do i=2,n1
	   xt2 = xtn(i,ngrd)**2
	   yt2 = ytn(j,ngrd)**2
	 fmapt=1.+(xt2 + yt2) / c1
	 DXT=(XMn(I,ngrd)-XMn(I-1,ngrd))/fmapt
	 !DYT=(YMn(J,ngrd)-YMn(J-1,ngrd))/fmapt
	 a(i,j,1) =  DXT
	  enddo
       enddo
return
entry get_grid_sizeY(n1,n2,n3,a,ngrd)

    c1= (2. * erad) ** 2
       do j=2,n2
	  do i=2,n1
	   xt2 = xtn(i,ngrd)**2
	   yt2 = ytn(j,ngrd)**2
	 fmapt=1.+(xt2 + yt2) / c1
	 !DXT=(XMn(I,ngrd)-XMn(I-1,ngrd))/fmapt
	 DYT=(YMn(J,ngrd)-YMn(J-1,ngrd))/fmapt
	 a(i,j,1) =  DYT
	  enddo
       enddo
return

!entry RAMS_get_leaf(n1,n2,n5,a)
!       do j=1,n2
!        do i=1,n1
!         do ip=1,n5
!            a(i,j,ip)=a(i,j,ip)
!         enddo
!        enddo
!       enddo
!return

entry RAMS_comp_vegclass2(n1,n2,n3,a)
      do i=1,n1
      a(i,1,1) = a(i,1,1) + .5
      enddo
return

!entry RAMS_comp_vegclass(n1,n2,n5,a)
!       do j=1,n2
!        do i=1,n1
!         do ip=1,n5
!!            print*,i,j,ip,' veg=',a(i,j,ip)
!            a(i,j,ip)=float(int(a(i,j,ip)+0.5))
!!            print*,i,j,ip,' veg=',a(i,j,ip)
!         enddo
!        enddo
!       enddo
!return

entry get_leaf_soil(n1,n2,n3,n4,n5,a,a2)
       kip=0
      do ip=1,n5
       do k=1,n4
          kip=kip+1
          do i=1,n1
           do j=1,n2

             a2(i,j,k,ip)=a(i,j,kip)
!!
!             if(a(i,j,kip) > 0. .and. ip >1) &
!	     print*,i,j,k,' SOIL=',a2(i,j,k,ip),a(i,j,kip)
!!
	   enddo
          enddo
       enddo
      enddo
!      stop
return

entry D3toD2(n1,n2,n3,klevel,a,c)
  do j=1,n2
      do i=1,n1
	a(i,j,1)=c(i,j,klevel)
!	print*,i,j,klevel,c(i,j,klevel)
      enddo
  enddo
return


entry up_to_tp(n1,n2,n3,a,b)
         do k=1,n3
            do j=1,n2
               a(1,j,k)=b(1,j,k)
               do i=2,n1
               a(i,j,k)=0.5* (b(i,j,k) + b(i-1,j,k))
               enddo
            enddo
         enddo
return

entry vp_to_tp(n1,n2,n3,a,b)
         do k=1,n3
            do i=1,n1
               a(i,1,k)=b(i,1,k)
               do j=2,n2
               a(i,j,k)=0.5* (b(i,j,k) + b(i,j-1,k))
               enddo
            enddo
         enddo
return

entry wp_to_tp(n1,n2,n3,a,b)
         do j=1,n2
           do i=1,n1
              a(i,j,1)=b(i,j,1)
              do k=2,n3
               a(i,j,k)=0.5* (b(i,j,k) + b(i,j,k-1))
              enddo
            enddo
         enddo
return

entry set_undef(n1,n2,n3,a,c)
  do j=1,n2
      do i=1,n1
	a(i,j,1)=c(i,j,1)
!        if(a(i,j,1) .lt. 0.0001) a(i,j,1) = -9.99e33
!	print*,i,j,klevel,c(i,j,klevel)
      enddo
  enddo
return
!SRF
!***********************************************************

entry RAMS_comp_vals(n1,n2,n3,a)
   print*,'==================== values =========================='
   do k=1,n3
      do j=1,n2
         do i=1,n1
            if(a(i,j,k).ne.0.) write(*,'(3i3,e14.6)') i,j,k  &
                                                     ,a(i,j,k)
         enddo
      enddo
   enddo
   print*,'======================================================'
return

entry RAMS_comp_tot(n1,n2,n3,a)
   tot=0.
   do k=1,n3
      do j=1,n2
         do i=1,n1
            tot=tot+a(i,j,k)
         enddo
      enddo
   enddo
   write(*,'(a,e12.6)') '-> total- ',tot
return
entry get_total_src(n1,n2,n3,a,c,d)
      a(:,:,2)=a(:,:,2)+c(:,:,1)+d(:,:,1)
return
entry get_total_src_volc(n1,n2,n3,a,c,e,d)
      a(:,:,2)=a(:,:,2)+c(:,:,1)+e(:,:,1)+d(:,:,2)
      do k=3,n3
        a(:,:,k)=a(:,:,k)+d(:,:,k)
      enddo
return

entry get_total_srcx(n1,n2,n3,a,c,d)
      a(:,:,2)=c(:,:,1)!+d(:,:,1)
return

entry RAMS_comp_maxval(n1,n2,n3,a)
   zmax=-1.0e30
   do k=1,n3
      do j=1,n2
         do i=1,n1
            if(a(i,j,k).gt.zmax) then
               zmax=a(i,j,k)
               maxx=i
               maxy=j
               maxz=k
            endif
         enddo
      enddo
   enddo
   write(*,'(a,e12.6,a,3i3)') '-> max- ',zmax,' at i,j,k-',maxx  &
                              ,maxy,maxz
return

entry RAMS_comp_minval(n1,n2,n3,a)
   zmin=1.0e30
   do k=1,n3
      do j=1,n2
         do i=1,n1
            if(a(i,j,k).lt.zmin) then
               zmin=a(i,j,k)
               minx=i
               miny=j
               minz=k
            endif
         enddo
      enddo
   enddo
   write(*,'(a,e12.6,a,3i3)') '-> min- ',zmin,' at i,j,k-',minx  &
                              ,miny,minz
return

entry D3toD2m(n1,n2,n3,klevel,a,c)
  do j=1,n2
      do i=1,n1
	a(i,j,1)=1./3.*( c(i,j,klevel)+c(i,j,klevel+1)+c(i,j,klevel+2) )
!	print*,i,j,klevel,c(i,j,klevel)
      enddo
  enddo
return

entry RAMS_comp_zero(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=0.
         enddo
      enddo
   enddo
return

entry RAMS_comp_1minus(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=1.-a(i,j,k)
         enddo
      enddo
   enddo
return

entry RAMS_comp_mults(n1,n2,n3,a,s)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k) * s
         enddo
      enddo
   enddo
return
entry RAMS_comp_add(n1,n2,n3,a,s)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k) + s
         enddo
      enddo
   enddo
return


entry RAMS_comp_accum(n1,n2,n3,a,b)
   do k=1,n3
      do j=1,n2
         do i=1,n1
         !if(b(i,j,k) > 1.e5) stop 333
            a(i,j,k)=a(i,j,k)+b(i,j,k)
         !if(a(i,j,k) < 0.) stop 332
         enddo
      enddo
   enddo
return

entry RAMS_comp_noneg(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=max(a(i,j,k),0.)
         enddo
      enddo
   enddo
return

entry RAMS_comp_subt(n1,n2,n3,a,b)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)-b(i,j,k)
         enddo
      enddo
   enddo
return

entry RAMS_comp_mult(n1,n2,n3,a,b)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)*b(i,j,k)
         enddo
      enddo
   enddo
return

!Demerval [
entry RAMS_comp_pw(n1,n2,n3,a,e,c,ngrd)

  do j=1,n2
    do i=1,n1
       a(i,j,1)=0.
    enddo
  enddo
  do j=1,n2
     do i=1,n1
       do k=2,n3
            a(i,j,1)=a(i,j,1) + e(i,j,k)*c(i,j,k)*    &
             (ztn(k,ngrd)-ztn(k-1,ngrd))*             &
             (1.-c(i,j,k)/zmn(nnzp(1)-1,1))*0.0001
       enddo
     enddo
  enddo

return
!Demerval ]

entry RAMS_comp_z(n1,n2,n3,a,c,ngrd)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=c(i,j,1)  &
                 +ztn(k,ngrd)*(1.-c(i,j,1)/zmn(nnzp(1)-1,1))
         enddo
      enddo
   enddo
return

entry RAMS_comp_rotate(n1,n2,n3,a,b,ngrd)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            call xy_ll(qlat,qlon,platn(ngrd),plonn(ngrd)  &
               ,xtn(i,ngrd),ytn(j,ngrd))
            u=a(i,j,k)
            v=b(i,j,k)
            call uvtoueve(u,v,a(i,j,k),b(i,j,k)  &
                         ,qlat,qlon,platn(ngrd),plonn(ngrd))
         enddo
      enddo
   enddo
return

entry RAMS_comp_tempK(n1,n2,n3,a,b)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)*b(i,j,k)/cp
         enddo
      enddo
   enddo
return

entry RAMS_comp_press(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=(a(i,j,k)/cp)**cpor*p00*.01
         enddo
      enddo
   enddo
return
entry RAMS_comp_press_sfc(n1,n2,n3,c,a)
      do j=1,n2
         do i=1,n1
            a(i,j,1)=(0.5* ( c(i,j,1)+c(i,j,2) ) / cp)**cpor*p00*.01
	    !print*,c(i,j,1),c(i,j,2),  a(i,j,1)
         enddo
      enddo
return

entry RAMS_comp_tempC(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)-273.16
         enddo
      enddo
   enddo
return

entry RAMS_comp_tempF(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=(a(i,j,k)-273.16)*1.8+32.
         enddo
      enddo
   enddo
return

entry RAMS_comp_wcms(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)*100.
         enddo
      enddo
   enddo
return

entry RAMS_comp_avgw(n1,n2,n3,a)
   do k=n3,2,-1
      do j=1,n2
         do i=1,n1
            a(i,j,k)=0.5*(a(i,j,k)+a(i,j,k-1))
         enddo
      enddo
   enddo
return

entry RAMS_comp_avgu(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=n1,2,-1
            a(i,j,k)=0.5*(a(i,j,k)+a(i-1,j,k))
         enddo
      enddo
   enddo
return

entry RAMS_comp_avgv(n1,n2,n3,a)
   do k=1,n3
      do j=n2,2,-1
         do i=1,n1
            a(i,j,k)=0.5*(a(i,j,k)+a(i,j-1,k))
         enddo
      enddo
   enddo
return

entry RAMS_comp_sfcdiv(n1,n2,n3,a,ngrd)
   do j=1,n2
      do i=1,n1
         a(i,j,1)=-(a(i,j,2)-a(i,j,1))*dztn(2,ngrd)
      enddo
   enddo
return

entry RAMS_comp_rt(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=max(0.,a(i,j,k))*1000.
         enddo
      enddo
   enddo
return

entry RAMS_comp_hr_pcprate(n1,n2,n3,a,b,c)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=(b(i,j,k)-c(i,j,k))
         enddo
      enddo
   enddo
return

entry RAMS_comp_speed(n1,n2,n3,a,b)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=sqrt(a(i,j,k)**2+b(i,j,k)**2)
         enddo
      enddo
   enddo
return

entry RAMS_comp_dir(n1,n2,n3,a,b,ngrd)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            call xy_ll(qlat,qlon,platn(ngrd),plonn(ngrd)  &
               ,xtn(i,ngrd),ytn(j,ngrd))
            u=a(i,j,k)
            v=b(i,j,k)
            call uvtoueve(u,v,a(i,j,k),b(i,j,k)  &
                         ,qlat,qlon,platn(ngrd),plonn(ngrd))
            call winddf(a(i,j,k),ff,a(i,j,k),b(i,j,k))
         enddo
      enddo
   enddo
return

entry RAMS_comp_dewK(n1,n2,n3,a,b,c)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            xpress=(b(i,j,k)/cp)**cpor*p00
            xtemp=c(i,j,k)*b(i,j,k)/cp
            xwatsat=rs(xpress,xtemp)
            a(i,j,k)=td(xpress,min(a(i,j,k),xwatsat) )
         enddo
      enddo
   enddo
return

!<Demerval
entry RAMS_comp_thete(n1,n2,n3,a,e,f1,f2)
   do k=1,n3
      do j=1,n2
         do i=1,n1
         TL=55.+1./( 1./(f1(i,j,k)-55.) -  &
            log(f2(i,j,k)/100.)/2840.)
            a(i,j,k)=a(i,j,k)*exp((alvl*e(i,j,k))/(cp*TL))
         enddo
      enddo
   enddo
return

!entry RAMS_comp_thete(n1,n2,n3,a,b,c)
!   do k=1,n3
!      do j=1,n2
!         do i=1,n1
!            xpress=(b(i,j,k)/cp)**cpor*p00
!            xtemp=c(i,j,k)*b(i,j,k)/cp
!            xwatsat=rs(xpress,xtemp)
!            a(i,j,k)=c(i,j,k)*exp( alvl*xwatsat  &
!                 /(cp*td(xpress,min(a(i,j,k),xwatsat) )) )
!         enddo
!      enddo
!   enddo
!return

!Demerval>

entry RAMS_comp_thetv(n1,n2,n3,a,b)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)*(1. + .61 * b(i,j,k))
         enddo
      enddo
   enddo
return

entry RAMS_comp_bowen(n1,n2,n3,a,b)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)/max(1.e-12,b(i,j,k))*1004./2.5e6
         enddo
      enddo
   enddo
return

entry RAMS_comp_rh(n1,n2,n3,a,b,c)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            xtemp=c(i,j,k)*b(i,j,k)/cp
            xpress=(b(i,j,k)/cp)**cpor*p00
            a(i,j,k)=100.*min(1.  &
                 ,max(0.,a(i,j,k)/rs(xpress,xtemp)))
         enddo
      enddo
   enddo
return

entry RAMS_comp_watsat(n1,n2,n3,a,b,c)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            c(i,j,k)=c(i,j,k)*a(i,j,k)/cp
            b(i,j,k)=(b(i,j,k)/cp)**cpor*p00
            a(i,j,k)=rs(b(i,j,k),c(i,j,k))
         enddo
      enddo
   enddo
return

entry get_flux_x(n1,n2,n3,a,ngrd)

   c1= (2. * erad) ** 2
   do k=1,1
      do j=2,n2-2
         do i=2,n1-1
          xt2 = xtn(i,ngrd)**2
          yt2 = ytn(j,ngrd)**2
	  fmapt=1.+(xt2 + yt2) / c1
	  DXT=(XMn(I,ngrd)-XMn(I-1,ngrd))/fmapt
	  a(i,j,1) = a(i,j,1) * DXT
         enddo
      enddo
   enddo
return

entry RAMS_comp_relvortx(n1,n2,n3,a,b,c,topt,ngrd)

   factor = ztn(1,ngrd) / ztn(2,ngrd)
   do j=1,n2
      do i=1,n1
         a(i,j,1) = a(i,j,2) * factor
      enddo
   enddo

   call gradr(n1,n2,n3,2,n1-1,1,n2-1,b,c,'ydir','wpnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))
   call gradr(n1,n2,n3,2,n1-1,1,n2-1,a,b,'zdir','vpnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))

   do k=1,n3
      do j=1,n2
         do i=1,n1
            b(i,j,k) = c(i,j,k) - b(i,j,k)
         enddo
      enddo
   enddo

   do j = 1,n2
      j1 = max(j-1,1)
      j2 = min(j,n2-1)
      do i = 1,n1
         do k = 1,n3
            k1 = max(k-1,1)
            k2 = min(k,n3-1)
            a(i,j,k) =0.25 * (b(i,j1,k1) + b(i,j1,k2)  &
                            + b(i,j2,k1) + b(i,j2,k2))
         enddo
      enddo
   enddo
  return

entry RAMS_comp_relvorty(n1,n2,n3,a,b,c,topt,ngrd)

   factor = ztn(1,ngrd) / ztn(2,ngrd)
   do j=1,n2
      do i=1,n1
         a(i,j,1) = a(i,j,2) * factor
      enddo
   enddo

   call gradr(n1,n2,n3,1,n1-1,2,n2-1,b,c,'xdir','wpnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))
   call gradr(n1,n2,n3,1,n1-1,2,n2-1,a,b,'zdir','upnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))

   do k=1,n3
      do j=1,n2
         do i=1,n1
            b(i,j,k) = b(i,j,k) - c(i,j,k)
         enddo
      enddo
   enddo

   do j = 1,n2
      do i = 1,n1
         i1 = max(i-1,1)
         i2 = min(i,n1-1)
         do k = 1,n3
            k1 = max(k-1,1)
            k2 = min(k,n3-1)
            a(i,j,k) = 0.25 * (b(i1,j,k1) + b(i1,j,k2)  &
                             + b(i2,j,k1) + b(i2,j,k2))
         enddo
      enddo
   enddo


   return

entry RAMS_comp_relvortz(n1,n2,n3,a,b,c,topt,ngrd)

   factor = ztn(1,ngrd) / ztn(2,ngrd)
   do j=1,n2
      do i=1,n1
         a(i,j,1) = a(i,j,2) * factor
         b(i,j,1) = b(i,j,2) * factor
      enddo
   enddo

   call gradr(n1,n2,n3,1,n1-1,1,n2-1,b,c,'xdir','vpnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))

   call gradr(n1,n2,n3,1,n1-1,1,n2-1,a,b,'ydir','upnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))

   do k=1,n3
      do j=1,n2
         do i=1,n1
            b(i,j,k) = c(i,j,k) - b(i,j,k)
         enddo
      enddo
   enddo

   do j = 1,n2
      j1 = max(j-1,1)
      j2 = min(j,n2-1)
      do i = 1,n1
         i1 = max(i-1,1)
         i2 = min(i,n1-1)
         do k = 1,n3
            a(i,j,k) = 0.25 * (b(i1,j1,k) + b(i1,j2,k)  &
                             + b(i2,j1,k) + b(i2,j2,k))
         enddo
      enddo
   enddo
   return

entry RAMS_comp_totvortz(n1,n2,n3,a,b,c,topt,ngrd)

   factor = ztn(1,ngrd) / ztn(2,ngrd)
   do j=1,n2
      do i=1,n1
         a(i,j,1) = a(i,j,2) * factor
         b(i,j,1) = b(i,j,2) * factor
      enddo
   enddo

   call gradr(n1,n2,n3,1,n1-1,1,n2-1,b,c,'xdir','vpnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))
   call gradr(n1,n2,n3,1,n1-1,1,n2-1,a,b,'ydir','upnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))

   do k=1,n3
      do j=1,n2
         do i=1,n1
            b(i,j,k) = c(i,j,k) - b(i,j,k)
         enddo
      enddo
   enddo

   do j = 1,n2
      j1 = max(j-1,1)
      j2 = min(j,n2-1)
      do i = 1,n1
         i1 = max(i-1,1)
         i2 = min(i,n1-1)
         do k = 1,n3
            a(i,j,k) = 0.25 * (b(i1,j1,k) + b(i1,j2,k)  &
                             + b(i2,j1,k) + b(i2,j2,k))
         enddo
      enddo
   enddo

   omega2 = 2. * 7.292e-5
   do j = 1,n2
      do i = 1,n1
         call xy_ll(xlat,xlon,platn(ngrd),plonn(ngrd)  &
              ,xtn(i,ngrd),ytn(j,ngrd))
         fcor = omega2 * sin(xlat * pi180)
         do k = 1,n3
            a(i,j,k) = a(i,j,k) + fcor
         enddo
      enddo
   enddo
   return

entry RAMS_comp_potvortz(n1,n2,n3,a,b,c,e,topt,ngrd)

   call gradr(n1,n2,n3,1,n1-1,1,n2-1,b,e,'zdir','tpnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))

   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k) = a(i,j,k) * e(i,j,k) / (9.8 * c(i,j,k))
         enddo
      enddo
   enddo
   return

entry RAMS_comp_vegclass(n1,n2,n3,a)
   do i=1,n1
      a(i,1,1) = float(int(a(i,1,1) + .1))
!      print*,i,int(a(i,1,1))
   enddo
   return

entry RAMS_comp_horizdiv(n1,n2,n3,a)
   do k=2,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=-(a(i,j,k)-a(i,j,k-1))*dztn(k,ngrd)
         enddo
      enddo
   enddo
return

entry RAMS_comp_vertint(n1,n2,n3,a,topt,ngrd)
   !altmax=100000.
   ztop = zmn(nnzp(1)-1,1)
   do j = 1,n2
      do i = 1,n1
         rtgt = 1. - topt(i,j) / ztop
         a(i,j,1) = 0.
	 do k = 2,n3-1
	    if(zmn(k,ngrd) > 100000.) cycle
            a(i,j,1) = a(i,j,1) + a(i,j,k) * (zmn(k,ngrd)-zmn(k-1,ngrd)) * rtgt
         enddo
      enddo
   enddo
return
entry RAMS_comp_rtgt(n1,n2,n3,a,topt,ngrd)
   !altmax=100000.
   ztop = zmn(nnzp(1)-1,1)
   do j = 1,n2
      do i = 1,n1
            rtgt = 1. - topt(i,j) / ztop
            a(i,j,1) = rtgt
      enddo
   enddo
return

entry RAMS_comp_vertint_O3(n1,n2,n3,a,topt,x1lat,ngrd,altmax)
   !altmax=100000.
   ztop = zmn(nnzp(1)-1,1)
   do j = 1,n2
    xx1=abs(x1lat(int(0.5*n1),j))
    if(xx1>= 0. .and. xx1<= 10.) altmax=15000
    if(xx1> 10. .and. xx1<= 20.) altmax=14500
    if(xx1> 20. .and. xx1<= 30.) altmax=14000
    if(xx1> 30. .and. xx1<= 40.) altmax=13000
    if(xx1> 40. .and. xx1<= 50.) altmax=12000
    if(xx1> 50. .and. xx1<= 60.) altmax=11000
    if(xx1> 60.                ) altmax=10000
      do i = 1,n1
         rtgt = 1. - topt(i,j) / ztop
         a(i,j,1) = 0.
	 do k = 2,n3-1
	    if(zmn(k,ngrd)*rtgt+ topt(i,j)> altmax) cycle
	    !print*,'k=',k,zmn(k,ngrd),altmax
            a(i,j,1) = a(i,j,1) + a(i,j,k) * (zmn(k,ngrd)-zmn(k-1,ngrd)) * rtgt
         enddo
      enddo
   enddo
return

entry RAMS_comp_vertmean_O3(n1,n2,n3,a,d,topt,x1lat,ngrd,altmax)
   !d=air density
   ztop = zmn(nnzp(1)-1,1)
   do j = 1,n2
    xx1=abs(x1lat(int(0.5*n1),j))
    if(xx1>= 0. .and. xx1<= 10.) altmax=15000
    if(xx1> 10. .and. xx1<= 20.) altmax=14500
    if(xx1> 20. .and. xx1<= 30.) altmax=14000
    if(xx1> 30. .and. xx1<= 40.) altmax=13000
    if(xx1> 40. .and. xx1<= 50.) altmax=12000
    if(xx1> 50. .and. xx1<= 60.) altmax=11000
    if(xx1> 60.                ) altmax=10000

      do i = 1,n1
         rtgt = 1. - topt(i,j) / ztop
         a(i,j,1) = 0.
	 massx=0.
	 do k = 2,n3-1
	    if(zmn(k,ngrd)*rtgt+ topt(i,j)> altmax) cycle
	    !print*,'k=',k,zmn(k,ngrd),altmax
	    massx=massx+d(i,j,k) * (zmn(k,ngrd)-zmn(k-1,ngrd)) * rtgt
            a(i,j,1) = a(i,j,1) + a(i,j,k)*d(i,j,k) * (zmn(k,ngrd)-zmn(k-1,ngrd)) * rtgt
         enddo
	 a(i,j,1)=a(i,j,1)/(1.E-10+MASSX) != MEAN O3 COLUMN

      enddo
   enddo
return



entry RAMS_comp_dn0(n1,n2,n3,a,b,c,topt,ngrd)
   ztop = zmn(nnzp(1)-1,1)
   do j=1,n2
      do i=1,n1
         do k=1,n3
            vctr2(k)=ztn(k,ngrd)*(1.-topt(i,j)/ztop)+topt(i,j)
         enddo
         call htint(n3,pi01dn(1,ngrd),ztn(1,ngrd),n3,vctr11,vctr2)
         call htint(n3,th01dn(1,ngrd),ztn(1,ngrd),n3,vctr12,vctr2)
         do k=1,n3
            b(i,j,k)=vctr12(k)
         enddo
         a(i,j,n3) = vctr11(n3)

         c1=g*2.*(1.-topt(i,j)/ztop)
         c2=(1-cpor)
         c3=cp**c2
         do k=n3-1,1,-1
            a(i,j,k)=a(i,j,k+1)  &
                +c1/((b(i,j,k)+b(i,j,k+1))*dzmn(k,ngrd))
         enddo

         do k=1,n3
            c(i,j,k)=(c3*p00)/(rgas*b(i,j,k)*a(i,j,k)**c2)
	    !print*,c(i,j,k)
         enddo

      enddo
   enddo
return

entry RAMS_comp_ppress(n1,n2,n3,a,c)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k) = 1000. * (a(i,j,k)/cp) ** cpor  &
                     - 1000. * (c(i,j,k)/cp) ** cpor
         enddo
      enddo
   enddo
return

entry RAMS_comp_raintemp(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k) = (a(i,j,k) - 334000.) / 4186.
         enddo
      enddo
   enddo
return

entry RAMS_comp_qwtc(n1,n2,n3,a,b,c)

! Unlike subroutine qwtk in rsurf.f which is receives qw in J/m^3, this
! subroutine inputs 'a' in units of J/cm^3.  Thus, we first convert back
! to J/m^3.  Also, 'b' is input as wgp, and is thus multiplied first by
! 1.e3 to convert to kg/m^3.

   do k=1,n3
      do j=1,n2
         do i=1,n1

            qwliq0 = b(i,j,k) * 3.34e8
            nsoil = nint(c(i,j,k))
            dryhcap = slcpd(nsoil)

            a(i,j,k) = a(i,j,k) * 1.e6
            b(i,j,k) = b(i,j,k) * 1.e3

            if (a(i,j,k) .le. 0.) then
               a(i,j,k) = a(i,j,k)  &
                  / (2093. * b(i,j,k) + dryhcap)
               b(i,j,k) = 0.
            elseif (a(i,j,k) .ge. qwliq0) then
               a(i,j,k) = (a(i,j,k) - qwliq0)  &
                  / (4186. * b(i,j,k) + dryhcap)
               b(i,j,k) = 1.
            else
               b(i,j,k) = a(i,j,k) / qwliq0
               a(i,j,k) = 0.
            endif

         enddo
      enddo
   enddo
return

entry RAMS_comp_copysst(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k) = a(i,j,n3) - 273.15
         enddo
      enddo
   enddo
return

entry RAMS_comp_qtcpcp(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            if (a(i,j,k) .le. 0.) then
               a(i,j,k) = a(i,j,k) / 2093.
            elseif (a(i,j,k) .le. 80.) then
               a(i,j,k) = 0.
            else
               a(i,j,k) = (a(i,j,k) - 334000.) / 4186.
            endif
         enddo
      enddo
   enddo
return

entry RAMS_comp_fracliq(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            if (a(i,j,k) .le. 0.) then
               a(i,j,k) = 0.
            elseif (a(i,j,k) .ge. 334000.) then
               a(i,j,k) = 1.
            else
               a(i,j,k) = a(i,j,k) / 334000.
            endif
         enddo
      enddo
   enddo
return

entry RAMS_comp_fracice(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            if (a(i,j,k) .le. 0.) then
               a(i,j,k) = 1.
            elseif (a(i,j,k) .ge. 334000.) then
               a(i,j,k) = 0.
            else
               a(i,j,k) = 1. - a(i,j,k) / 334000.
            endif
         enddo
      enddo
   enddo
return

entry RAMS_comp_hydrodiam(n1,n2,n3,a,c,ccfmas,ppwmas)
   rpwmas = 1. / ppwmas
   do k=1,n3
      do j=1,n2
         do i=1,n1
            if(a(i,j,k) .gt. 1.e-10 .and. c(i,j,k).gt.1.e-10)then
               a(i,j,k) = (a(i,j,k) / (c(i,j,k) * ccfmas))**rpwmas
            else
               a(i,j,k) = 0.
            endif
         enddo
      enddo
   enddo
return

entry RAMS_comp_slmstf(n1,n2,n3,a,c)
   do i=1,n1
      a(i,1,1) = a(i,1,1) / max(1.e-6,slmsts0(nint(c(i,1,1))))
   enddo
return

entry rams_sum_snowlayers(n1,n2,n3,a)
   do ip=1,n3
      do k=2,n2
         do ij=1,n1
            a(ij,1,ip) = a(ij,1,ip) + a(ij,k,ip)
         enddo
      enddo
   enddo
return

entry rams_fill_sst(n1,n2,n3,kp,a,c)
   do j=1,n2
      do i = 1,n1
         a(i,j,1) = (c(i,j,kp)-334000)/4186
      enddo
   enddo
return

entry rams_comp_pcpgnorm(n1,n2,n3,a,c)
return

entry rams_comp_vapnorm(n1,n2,n3,a,c)
return

entry rams_comp_snownorm(n1,n2,n3,a,c)
return

entry rams_comp_vegnorm(n1,n2,n3,a,c)
return

entry rams_comp_cannorm(n1,n2,n3,a,c)
return


entry get_ZItheta(n1,n2,n3,a,c,e,ngrd)
      do i=1,n1
        do j=1,n2
          a(i,j,1) = 0.
        enddo
      enddo

      do i=1,n1
        do j=1,n2

          do k=3,n3-5
            dtheta=c(i,j,k)-c(i,j,k-1)
            x=0.6
            if(dtheta.gt.x.or.abs(e(i,j,k)).gt.1.e-5) go to 1881
          enddo
 1881     continue
          kzi = k
          if(abs(e(i,j,k)).gt.1.e-5) then

            a(i,j,1) =0.5*(ztn(kzi,ngrd)+ ztn(kzi-1,ngrd))

          else

            a(i,j,1) =0.5*(ztn(kzi,ngrd)+ ztn(kzi-1,ngrd))
          endif

          if(a(i,j,1).lt.0..or.kzi.le.2) a(i,j,1)=0.
       enddo
     enddo

   return

!DSM {
entry calc_u10m(n1,n2,n3,a,c)
   do j=1,n2
      do i=1,n1
         a(i,j,1)=a(i,j,1)*cos((270.0-c(i,j,2))*3.1415/180.0)
      enddo
   enddo
return

entry calc_v10m(n1,n2,n3,a,c)
   do j=1,n2
      do i=1,n1
         a(i,j,1)=a(i,j,1)*sin((270.0-c(i,j,2))*3.1415/180.0)
      enddo
   enddo
return

entry calc_omega(n1,n2,n3,a,c)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=-a(i,j,k)*9.8*c(i,j,k)
         enddo
      enddo
   enddo
return
!DSM }


entry RAMS_comp_pbl(n1,n2,n3,a,c,ngrd)
   tkethrsh=0.01   !tke threshold for PBL height in m2/s2
   do j=1,n2
      do i=1,n1
         pblht=0.
         do k=2,n3
            pblht=ztn(k-1,ngrd)*(1.-c(i,j,1)/zmn(nnzp(1)-1,1))
!                  if(i.ge.10.and.i.le.25.and.j.ge.13.and.j.le.25)
!     &               print*,'i,j,k,z,pbl=',i,j,k,ztn(k,ngrd),pblht
            if(a(i,j,k).le.tkethrsh)goto 10
         enddo
 10      continue
         do k=1,n3
           a(i,j,k)=pblht
         enddo
      enddo
   enddo
!         call cpezct(a(i,j,2),n1,n2)
return
entry RAMS_comp_zlcl(n1,n2,n3,a,b,c)
    k=2
      do j=1,n2
         do i=1,n1
            plll=(b(i,j,k)/cp)**cpor*p00
            tlll=c(i,j,k)*b(i,j,k)/cp
            rlll=a(i,j,k)
	    CALL LCL(TLLL,PLLL,RLLL,TLCL,PLCL,DZLCL)

            a(i,j,1)=DZLCL
         enddo
      enddo
return

entry RAMS_comp_etrans(n1,n2,n3,a,b,a2d)
   do j=1,n2
      do i=1,n1
         temp1=a(i,j,1)*b(i,j,1)/cp
         press1=(b(i,j,1)/cp)**cpor*p00
         dens=press1/(rgas*temp1)
         if(i.eq.5.and.j.eq.5) then
            print*,'============++++++'
            print*,temp1,press1,dens,a2d(i,j)
         endif
         a(i,j,1)=a2d(i,j)*dens*1.e-3*39.37*3600.
         do k=2,n3
            a(i,j,k)=a(i,j,1)
         enddo
      enddo
   enddo
return

entry RAMS_comp_2d(n1,n2,n3,a)
   do j=1,n2
      do i=1,n1
        a(i,j,1)=a(i,j,2)
      enddo
   enddo
return

!<Demerval
entry RAMS_comp_dewK_2m(n1,n2,n3,a,b,c)
      do j=1,n2
         do i=1,n1
            xpress=(0.5*(b(i,j,1)+b(i,j,2))/cp)**cpor*p00
            xtemp=c(i,j,1)
            xwatsat=rs(xpress,xtemp)
            a(i,j,1)=td(xpress,min(a(i,j,1)/1000.,xwatsat) )
         enddo
      enddo
return

entry RAMS_comp_slpress(n1,n2,n3,theta,pp,z,slp)
!
!     This subroutine calculates the pressure at level zlev. it
!       is hardwired here to calculate mean sea level pressure,
!       but can be easily changed to calculate pressure at any level
!       by just changing zlev.
!     a standard atmosphere lapse rate of 6.5 c/km is used to
!       interpolate temperature down from level 2 in the model.
!

   sl_p00=1000.
   sl_g=9.8
   sl_cp=1004.
   sl_r=287.
   sl_cpor=sl_cp/sl_r
!c      rlap=-.0065  ! standard temp lapse rate
   rlap=.0025     ! approx standard theta lapse rate
   zlev=0.

do j=1,n2
   do i=1,n1
      do k=2,n3
         if(z(i,j,k).ge.zlev) then
            ktop=k
            kbot=k-1
	    !print*,'loop1:',ktop,z(i,j,k),zlev
            go to 56
         endif
      enddo
56    continue
!print*,'loop1F:',ktop,z(i,j,kbot),zlev
      !if(i.eq.1.and.j.eq.1)  &
         !print*,'kbot:',kbot,ktop,z(i,j,kbot),z(i,j,ktop)  &
         !   ,pp(i,j,kbot),theta(i,j,kbot)
         ddz=zlev-z(i,j,kbot)
         if(zlev.lt.z(i,j,kbot))then
            thbar=(theta(i,j,kbot)-.5*ddz*rlap)
         else
            thbar=.5*(theta(i,j,kbot)+theta(i,j,ktop))
         endif
         slp(i,j)=pp(i,j,kbot)-ddz*sl_g/thbar
         slp(i,j)=(slp(i,j)/sl_cp)**sl_cpor*sl_p00
	 !PRINT*,'SLP=' , SLP(I,J)
      enddo
   enddo
!stop 111
return
entry RAMS_comp_slpress2(n1,n2,n3,theta,pp,z,slp)
!
!     This subroutine calculates the pressure at level zlev. it
!       is hardwired here to calculate mean sea level pressure,
!       but can be easily changed to calculate pressure at any level
!       by just changing zlev.
!     a standard atmosphere lapse rate of 6.5 c/km is used to
!       interpolate temperature down from level 2 in the model.
!

   sl_p00=1000.
   sl_g=9.8
   sl_cp=1004.
   sl_r=287.
   sl_cpor=sl_cp/sl_r
!c      rlap=-.0065  ! standard temp lapse rate
   rlap=.0025     ! approx standard theta lapse rate
   zlev=0.

do j=1,n2
   do i=1,n1
      do k=2,n3
         if(z(i,j,k).ge.zlev) then
            ktop=k
            kbot=k-1
	    !print*,'loop1:',ktop,z(i,j,k),zlev
            go to 31
         endif
      enddo
31    continue
!print*,'loop1F:',ktop,z(i,j,kbot),zlev
      !if(i.eq.1.and.j.eq.1)  &
         !print*,'kbot:',kbot,ktop,z(i,j,kbot),z(i,j,ktop)  &
         !   ,pp(i,j,kbot),theta(i,j,kbot)
         ddz=zlev-z(i,j,kbot)
         if(zlev.lt.z(i,j,kbot))then
            thbar=(theta(i,j,kbot)-.5*ddz*rlap)
         else
            thbar=.5*(theta(i,j,kbot)+theta(i,j,ktop))
         endif
         slp(i,j)=pp(i,j,kbot)-ddz*sl_g/thbar
         slp(i,j)=(slp(i,j)/sl_cp)**sl_cpor*sl_p00
	 !PRINT*,'SLP=' , SLP(I,J)
      enddo
   enddo
!stop 111
return
entry RAMS_comp_ctprof(n1,n2,n3,a,b,ngrd)
   do i=1,n1
      do j=1,n2
         kmax=0
         do k=1,n3
            if(a(i,j,k).ge.0.0001.and.b(i,j,k).ge.0.99)kmax=k
         enddo
         if(kmax.gt.2)then
            a(i,j,1)=ztn(kmax,ngrd)
         else
            a(i,j,1)=0.0
         endif
      enddo
   enddo
return

end


subroutine RAMS_comp_multap(n1,n2,n3,n4,a,b)
dimension a(n1,n2,n4),b(n1,n2,n3)
   do k=1,n4
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)*b(i,j,1)
         enddo
      enddo
   enddo
return
end



subroutine RAMS_comp_patchsum(n1,n2,n3,n4,a,f,psum)
real a(n1,n2,n3,n4),f(n1,n2,n4),psum(n1,n2,n3)

! This routine is for quantities such as net roughness that are defined
! for all patches

do k = 1,n3
   do j = 1,n2
      do i = 1,n1
         psum(i,j,k) = 0.
         do ip = 1,n4
            psum(i,j,k) = psum(i,j,k) + f(i,j,ip) * a(i,j,k,ip)
         enddo
      enddo
   enddo
enddo

! Copy psum into f, which was passed in as a(1).  n3 may exceed n4 but this
! should be ok.

do k = 1,n3
   do j = 1,n2
      do i = 1,n1
         f(i,j,k) = psum(i,j,k)
      enddo
   enddo
enddo

return
end



subroutine RAMS_comp_patchsum_l(n1,n2,n3,n4,a,f,psum)
real a(n1,n2,n3,n4),f(n1,n2,n4),psum(n1,n2,n3)

! This routine is for quantities such as veg roughness that are not
! defined for water patches

do k = 1,n3
   do j = 1,n2
      do i = 1,n1
         if (f(i,j,1) .lt. .991) then
            psum(i,j,k) = 0.
            do ip = 2,n4
               psum(i,j,k) = psum(i,j,k) + f(i,j,ip) * a(i,j,k,ip)  &
                           / (1. - f(i,j,1))
            enddo
         else
            psum(i,j,k) = a(i,j,k,2)
         endif
      enddo
   enddo
enddo

! Copy psum into f, which was passed in as a(1).  n3 may exceed n4 but this
! should be ok.

do k = 1,n3
   do j = 1,n2
      do i = 1,n1
         f(i,j,k) = psum(i,j,k)
      enddo
   enddo
enddo

return
end

subroutine RAMS_comp_bigpatch(n1,n2,n3,n4,a,f,b)
real a(n1,n2,n3,n4),f(n1,n2,n4),b(n1,n2,n3)

! Extract LSP value from largest patch

do k = 1,n3
   do j = 1,n2
      do i = 1,n1
         if (f(i,j,2) .ge. f(i,j,1)) then
            b(i,j,k) = a(i,j,k,2)
         else
            b(i,j,k) = a(i,j,k,1)
         endif
      enddo
   enddo
enddo

! Copy b into f, which was passed in as a(1).  n3 may exceed n4 but this
! should be ok.

do k = 1,n3
   do j = 1,n2
      do i = 1,n1
         f(i,j,k) = b(i,j,k)
      enddo
   enddo
enddo

return
end


subroutine RAMS_comp_5050(n1,n2,n3,a,d)
real a(n1,n2),d(n1,n2,n3)

do j = 1,n2
   do i = 1,n1
      a(i,j) = .5 * (a(i,j) + d(i,j,2))
   enddo
enddo

return
end


!------------------subroutine to calculate cloud fraction
subroutine cldfraction(n1,n2,n3,frac,pi,rh)
implicit none
include 'rconstants.h'

integer :: i,j,k,kmax,n1,n2,n3
real :: frac(n1,n2),pi(n1,n2,n3),rh(n1,n2,n3)
real, allocatable::rhc(:), cs(:)
real :: kappai,c_1,c_2,c_junk,pop2,csmax

c_1     = 2.
c_junk  = 3.
c_2     = c_junk**0.5
kappai = (1./.286)


allocate (rhc(n3),cs(n3) )
      print*,'+++++++:',n1,n2,n3

do j=1,n2
   do i=1,n1
      frac(i,j) = 0.
      csmax = 0.
      kmax  = 0
      do k = 1, n3
         rhc(k)= 0.
         cs(k)= 0.
      enddo

      do k = 1, n3
         pop2 = (pi(i,j,k)/pi(i,j,2))**kappai

         rhc(k) = 100. - (100.*c_1*pop2)*  &
               (1.-pop2)*(1.+c_2*(pop2-0.5))

         if(rh(i,j,k) .ge. rhc(k))then
            if(rhc(k).eq.100.)rhc(k)=rhc(k)+0.0000001
            cs(k) = ( (rh(i,j,k)-rhc(k))/(100.-rhc(k)) ) **2.
         else
            cs(k) = 0.
         endif
         if(cs(k).gt.csmax)then
            csmax=cs(k)
            kmax = k
         endif
         frac(i,j) = frac(i,j) + cs(k)*(1./float(k))
      if(i==20.and.j==20) print*,'+++++++:',k,pi(i,j,k),rh(i,j,k),frac(i,j)
      enddo

      csmax=max(csmax,0.)

!      frac(i,j) = 1.-min(1.,max(0.,csmax))
      frac(i,j) = 1.-min(1.,max(0.,frac(i,j)))  ! actually returns
                                                ! clear sky fraction

   enddo
enddo

deallocate (rhc,cs)

return
end
!***************************************************************************

subroutine RAMS_reduced_temp (n1,n2,n3,n4,tempnew,speed,ustar  &
                             ,tstar,znew,zold,zrough,patfrac  &
                             ,cantemp,theta,pi,topo,ztop)

implicit none

integer :: n1,n2,n3,n4,i,j,np
real :: tempnew(n1,n2),speed(n1,n2,n3),ustar(n1,n2,n4),znew,zold  &
       ,zrough(n1,n2,n4),patfrac(n1,n2,n4),cantemp(n1,n2,n4)  &
       ,theta(n1,n2,n3),pi(n1,n2,n3),topo(n1,n2),ztop,tstar(n1,n2,n4)
include 'rconstants.h'

real :: richno,rtgt,zagl,rtemp,rtempw,z0,a2,spd,cantheta,sfcpi,fh
!srf: consistent with Louis 1981
fh = 1.

do j=1,n2
   do i=1,n1

      rtgt=1.-topo(i,j)/ztop
      zagl=zold*rtgt
      sfcpi=.5*(pi(i,j,1)+pi(i,j,2))


      rtempw=0.

      do np=1,n4

         z0=zrough(i,j,np)
         if(np==1) z0=.001
         spd=max(speed(i,j,2),.25)
         cantheta=cantemp(i,j,np)*cp/sfcpi

!-srf
!         richno=g*zagl*(theta(i,j,2)-cantheta)  &
!                     /(theta(i,j,2)*spd**2)
         richno=g*zagl*(theta(i,j,2)-cantheta)  &
                     /(.5*(theta(i,j,2)+cantheta)*spd**2)

         a2 = (vonk / log(znew / z0)) ** 2

        !print*,richno,spd,theta(i,j,2),zagl


         if(richno.gt.0.) then
            rtemp=cantheta                            &
!srf         +(ustar(i,j,np)*tstar(i,j,np)*0.74)/(a2*spd)  &
             +(ustar(i,j,np)*tstar(i,j,np)*fh  )/(a2*spd)  &
                    *(1.+15.*richno*sqrt(1+5*richno))


            !print*,rtemp, cantheta,theta(i,j,2)
	    rtemp=min(max(rtemp, cantheta),theta(i,j,2))

        else
            rtemp=cantheta                              &
!srf         +((ustar(i,j,np)*tstar(i,j,np)*0.74)/(a2*spd))  &
             +((ustar(i,j,np)*tstar(i,j,np)*fh  )/(a2*spd))  &
                              / (1.- 15.*richno/(1.+75.*a2   &
                              * sqrt(-znew*richno/z0)))


            !print*,rtemp, cantheta,theta(i,j,2)
	    rtemp=max(min(rtemp, cantheta),theta(i,j,2))


         endif

         !if((i==50.and.j==25)) then
         !   print*,'====tempf2m:',i,j
         !   print*,np,patfrac(i,j,np),cantheta
         !   print*,np,ustar(i,j,np),zrough(i,j,np),tstar(i,j,np)
         !  print*,np,theta(i,j,2),speed(i,j,2),rtemp
         !endif

         rtempw=rtempw+rtemp*patfrac(i,j,np)


      enddo

     tempnew(i,j)=rtempw ! temperatura potencial


   enddo
enddo

return
end
!-----------------------------------------------------------------------

subroutine RAMS_reduced_wind(n1,n2,n3,n4,velnew,speed,ustar &
         ,znew,zold,zrough,patfrac,cantemp,theta,pi,topo,ztop)
implicit none
integer :: n1,n2,n3,n4,i,j,np
real :: velnew(n1,n2),speed(n1,n2,n3),ustar(n1,n2,n4),znew,zold  &
          ,zrough(n1,n2,n4),patfrac(n1,n2,n4),cantemp(n1,n2,n4)  &
          ,theta(n1,n2,n3),pi(n1,n2,n3),topo(n1,n2),ztop
include 'rconstants.h'

real:: richno,rtgt,zagl,rwind,rwindw,z0,a2,spd,cantheta,sfcpi



do j=1,n2
   do i=1,n1

      rtgt=1.-topo(i,j)/ztop
      zagl=zold*rtgt
      sfcpi=.5*(pi(i,j,1)+pi(i,j,2))

      rwindw=0.

      do np=1,n4

         z0=zrough(i,j,np)
         if(np==1) z0=.001
         spd=max(speed(i,j,2),.25)
         cantheta=cantemp(i,j,np)*cp/sfcpi

         richno=g*zagl*(theta(i,j,2)-cantheta)  &
                      /(theta(i,j,2)*spd**2)
         a2 = (vonk / log(znew / z0)) ** 2

         if(richno.gt.0.) then
            rwind=sqrt(ustar(i,j,np)**2/a2   &
                     *(1.+10.*richno/sqrt(1+5*richno)) )
         else
            rwind=sqrt( ustar(i,j,np)**2/a2  &
                / (1.- 10.*richno/(1.+75.*a2  &
                              * sqrt(-znew*richno/z0))))
         endif

         rwind=max(min(rwind,speed(i,j,2)),0.)

         !if(i==50.and.j==25) then
         !   print*,'====speed10m'
         !   print*,np,patfrac(i,j,np),cantemp(i,j,np)
         !   print*,np,ustar(i,j,np),zrough(i,j,np)
         !   print*,np,theta(i,j,2),speed(i,j,2),rwind
         !endif

         rwindw=rwindw+rwind*patfrac(i,j,np)

      enddo

      velnew(i,j)=rwindw


   enddo
enddo

return
end

!***************************************************************************
!-------------------------------------------------------------------------
!*rmc Will Cheng's code for calculating slp with mm5's GRAPH method
! ------- added for calculating SLP from MM5 algorithm ------

      subroutine RAMS_comp_slpmm5(n1,n2,n3,theta,pp,z,slp)

!    The subroutine calculates SLP from an algorithm taken from
!    GRAPH, a post-processing packing of MM5 V3.3
!
!    Input: theta - potential temperature (K)         3D
!           pp    - Exner function        (J/kg K)    3D
!           z     - terrain               (m)         2D
!
!    Ouput: SLP   - sea-level pressure    (hPa)       2D

! ------ define dimension of arrays ----------

      dimension theta(n1,n2,n3), pp(n1,n2,n3), z(n1,n2),slp(n1,n2)

! ------ input variables to GRAPH subroutine ----------

      dimension sfp(n1,n2), ts(n1,n2), t_mm5(n1,n2,n3-1), p_mm5(n1,n2,n3-1)

! -----------------------------------------------------

      cp = 1004
      rgas = 287
      cpor = cp / rgas
      p00 = 1.e5

      do j = 1,n2
        do i = 1,n1
!! calculate surface pressure
        sfp(i,j) = (0.5*(pp(i,j,1)+pp(i,j,2))/cp)**cpor*p00*.01
!! calculate surface temp
        ts(i,j) = (0.5/cp)*(theta(i,j,1)*pp(i,j,1)+&
                            theta(i,j,2)*pp(i,j,2))
        enddo
      enddo

      do k = 2,n3
        kk = n3-k+1
        do j = 1,n2
!! flip array upside down for input to GRAPH subroutine
          do i = 1,n1
            t_mm5(i,j,kk) = theta(i,j,k)*pp(i,j,k)/cp
            p_mm5(i,j,kk) = (pp(i,j,k)/cp)**cpor*p00*.01
          enddo
        enddo
      enddo

      call SEAPRS_0(t_mm5,p_mm5,z,sfp,ts,n1,n2,n3-1,slp)

      return
      end


!------------------------------------------------------------------------
      SUBROUTINE SEAPRS_0(T,PP,TER,SFP,TS,IMX,JMX,KX,SLP)
!
!     SECTION  DIAGNOSTIC
!     PURPOSE  COMPUTES SEA LEVEL PRESSURE FROM THE RULE
!              T1/T2=(P1/P2)**(GAMMA*R/G).
!
!     *** LEVELS GO FROM TOP-DOWN ***
!
!     INPUT       T        TEMPERATURE (Kelvin)                3D
!                 TER      TERRAIN     (m)                     2D
!                 SFP      SURFACE PRESSURE (hPa)              2D
!                 IMX      DOT POINT DIMENSION N-S
!                 JMX      DOT POINT DIMENSION E-W
!                 KX       NUMBER OF VERTICAL LEVELS
!
!     OUTPUT      SLP      SEA LEVEL PRESSURE (hPa)            2D
!
      DIMENSION T(IMX,JMX,KX), PP(IMX,JMX,KX),&
                PS(IMX,JMX)  ,SFP(IMX,JMX) , &
                TER(IMX,JMX)
      DIMENSION PL(IMX,JMX),T0(IMX,JMX),TS(IMX,JMX),&
                XKLEV(IMX,JMX)
      DIMENSION SLP(IMX,JMX)
      PARAMETER (R=287.04,G=9.8,GAMMA=6.5E-3)
      PARAMETER (TC=273.16+17.5) ! T CRITICAL IN PSFC/PSLV
      PARAMETER (PCONST=100.)
!
      LOGICAL L1,L2,L3,L4
!
!
!
!
!     ... SEA LEVEL PRESSURE
!
      XTERM=GAMMA*R/G
!
!     ... COMPUTE PRESSURE AT PCONST MB ABOVE SURFACE (PL)
!
      KUPTO=KX/2
99    CONTINUE
      DO 100 J=1,JMX
      DO 100 I=1,IMX
         PL(I,J)=SFP(I,J)-PCONST
         XKLEV(I,J)=0.
100   CONTINUE
!
!     ... FIND 2 LEVELS ON SIGMA SURFACES SURROUNDING PL AT EACH I,J
!
      DO 150 J=1,JMX
      DO 150 I=1,IMX
         DO 125 K=KX-1,KUPTO,-1
            XK=FLOAT(K)
            XKHOLD=XKLEV(I,J)
!srf            XKLEV(I,J)=CVMGT(XK,XKHOLD,   &
            XKLEV(I,J)=merge(XK,XKHOLD,   &
              (((PP(I,J,K)).LT.PL(I,J)) .AND.  &
               ((PP(I,J,K+1)).GE.PL(I,J))))
125      CONTINUE
         IF(XKLEV(I,J).LT.1.) THEN
            PRINT *,'ERROR FINDING PRESSURE LEVEL ',PCONST,' MB ',&
                   'ABOVE THE SURFACE'
            PRINT *,'LAST K LEVEL =',KUPTO
            IF(KUPTO.NE.1) THEN
               PRINT *,'TRYING AGAIN WITH KUPTO=1'
               KUPTO=1
               GOTO 99
            ELSE
               PRINT *,'I,J=',I,J
               PRINT *,'PL=',PL(I,J)
               PRINT *,'PSFC=',SFP(I,J)
               STOP
            END IF
         END IF
150   CONTINUE
!
!     ... GET TEMPERATURE AT PL (TL), EXTRAPOLATE T AT SURFACE (TS)
!         AND T AT SEA LEVEL (T0) WITH 6.5 K/KM LAPSE RATE
!
      DO 200 J=1,JMX
      DO 200 I=1,IMX
         KLO=NINT(XKLEV(I,J))+1
         KHI=NINT(XKLEV(I,J))
         PLO=PP(I,J,KLO)
         PHI=PP(I,J,KHI)
         TLO=T(I,J,KLO)
         THI=T(I,J,KHI)
         TL=THI-(THI-TLO)*ALOG(PL(I,J)/PHI)/ALOG(PLO/PHI)
         TS(I,J)=TL*(SFP(I,J)/PL(I,J))**XTERM
         TBAR=(TS(I,J)+TL)*0.5
         HL=TER(I,J)-R/G*ALOG(PL(I,J)/SFP(I,J))*TBAR
         T0(I,J)=TL+GAMMA*HL
200   CONTINUE
!
!     ... CORRECT SEA LEVEL TEMPERATURE IF TOO HOT
!
      DO 400 J=1,JMX
      DO 400 I=1,IMX
         L1=T0(I,J).LT.TC
         L2=TS(I,J).LE.TC
         L3=.NOT.L1
         T0HOLD=T0(I,J)
!srf         T0(I,J)=CVMGT(T0HOLD,&
!srf           CVMGT(TC,TC-0.005*(TS(I,J)-TC)**2,L2.AND.L3),L1.AND.L2)
         T0(I,J)=merge(T0HOLD,&
           merge(TC,TC-0.005*(TS(I,J)-TC)**2,L2.AND.L3),L1.AND.L2)
400   CONTINUE
!
!     ... COMPUTE SEA LEVEL PRESSURE
!
      DO 600 J=1,JMX
      DO 600 I=1,IMX
         SLP(I,J)=SFP(I,J)*EXP(2.*G*TER(I,J)/(R*(TS(I,J)+T0(I,J))))
600   CONTINUE
      RETURN
      END

!-------------------------------------------------------------------------

!--------------------------------------------------------
subroutine BRAMS_comp_slpress(n1,n2,n3,theta,pp,z,slp,topo,ztarget)
!
!     This subroutine calculates the pressure at level zlev. it
!       is hardwired here to calculate mean sea level pressure,
!       but can be easily changed to calculate pressure at any level
!       by just changing zlev.
!     a standard atmosphere lapse rate of 6.5 c/km is used to
!       interpolate temperature down from level 2 in the model.
!

use modrconfig

implicit none

integer :: n1,n2,n3
real, dimension(n3) :: ztarget
real, dimension(n1,n2) :: topo,slp
real, dimension(n1,n2,n3) ::theta,pp,z
real b(nzpmax,6)
integer :: i,j,k,ktop,kbot,ii,jj,iinc,jinc,icount,inc,ICX
real ddz, thbar,phbar,xte

real ::sl_p00,sl_g,sl_cp, sl_r,sl_cpor,rlap,zlev


   sl_p00=1000.
   sl_g=9.8
   sl_cp=1004.
   sl_r=287.
   sl_cpor=sl_cp/sl_r
!c      rlap=-.0065  ! standard temp lapse rate
   rlap=.0025     ! approx standard theta lapse rate
   zlev=0.1

! niveis cartesianos onde serao interpolados os valores em sigma-z
   do k=1,n3
        b(k,4)=ztarget(k)
	!print*,k,b(k,4)
   enddo
   !b(1,4) = 0.
! interpola theta e pp para n\EDveis cartesianos e undefine valores n\E3o existentes
do j=1,n2
   do i=1,n1
      do k=1,n3
           b(k,1)=theta(i,j,k)
           b(k,5)=pp(i,j,k)
           b(k,2)=z(i,j,k)
      enddo
      call htint(n3,b(1,1),b(1,2),n3,b(1,3),b(1,4))
      call htint(n3,b(1,5),b(1,2),n3,b(1,6),b(1,4))
      !print*,' ij=' ,i,j
      do k=1,n3
       if( b(k,4).lt.topo(i,j) .and. topo(i,j) > zlev) then
         theta(i,j,k)= -1.
         pp(i,j,k)   = -1.
         !print*,'1',b(k,4),topo(i,j),theta(i,j,k)
!        stop
       else
         xte=theta(i,j,k)
         theta(i,j,k)=b(k,3)
         pp(i,j,k)   =b(k,6)
         !print*,'2',b(k,4),topo(i,j),theta(i,j,k)
       endif
      enddo
   enddo
 enddo
print*,'theta=' , maxval(theta(:,:,1) ),minval(theta(:,:,1) ) !, theta(:,:,1)
call flush(6)

!do j=1,n2
!   do i=1,n1
do j=367,367
   do i=70,70
!   if (topo(i,j)>1000.) then
!     print*,i,j,topo(i,j)
!     stop 33
!   endif
      do k=n3-1,1,-1

         if((topo(i,j)-b(k,4)).ge.0.1) then

            ktop=k+1
            kbot=k
	    print*,' w',ktop,kbot,b(k,4),topo(i,j)
            exit
          else
	   kbot=1
	   ktop=2
	  endif
      enddo
         !ddz=zlev-b(kbot,4)
         ddz=zlev-z(i,j,kbot)


!        if(zlev.lt.z(i,j,kbot))then
         if(zlev.lt.b(kbot,4))then

	  slp(i,j) = pp(i,j,ktop)
	  do k=ktop-1,1,-1
	    ICX    = 0
	    inc    = 1
	    icount = 0
	    !print*,' ============= ijk = ',i,j,k,maxval(theta)
	    do while (icount == 0)
	    icount = 0; thbar  = 0.; phbar  = 0.
            !print*,'icx=',icx,thbar
	    !  face - norte
	    jj = MIN(N2,j + inc)
	    do ii=max(1,i-inc),min(n1,i+inc),1
	      !print*, ' 1YN' ,jj,ii,theta(ii,jj,k)
	      if(theta(ii,jj,k) > 0. ) then
	      print*, ' 2YN' ,jj,ii,theta(ii,jj,k)

	         thbar = thbar + theta(ii,jj,k)
	         phbar = phbar +    pp(ii,jj,k)
	         icount= icount+ 1
	      endif
	    enddo

	    !  face - sul
	    jj = MAX(1,j - inc)
	    do ii=max(1,i-inc),min(n1,i+inc),1
	      !print*, ' 1YS' ,jj,ii,theta(ii,jj,k)
	      if(theta(ii,jj,k) > 0. ) then
	      print*, ' 2YS' ,jj,ii,theta(ii,jj,k)
	         thbar = thbar + theta(ii,jj,k)
	         phbar = phbar +    pp(ii,jj,k)
	         icount= icount+ 1
	      endif
	    enddo

	    !  face - oeste
	    ii = Max(1,i - inc)
	    do jj=max(1,j-inc),min(n2,j+inc),1
	      !print*, ' 1xO' ,jj,ii,theta(ii,jj,k)
	      if(theta(ii,jj,k) > 0. ) then
	         thbar = thbar + theta(ii,jj,k)
	      print*, ' 2xO' ,jj,ii,theta(ii,jj,k)
	         phbar = phbar +    pp(ii,jj,k)
	         icount= icount+ 1
	      endif
	    enddo

	    !  face - leste
	    ii = min(N1,i + inc)
	    do jj=max(1,j-inc),min(n2,j+inc),1
	      !print*, ' 1xL' ,jj,ii,theta(ii,jj,k)
	      if(theta(ii,jj,k) > 0. ) then
	         thbar = thbar + theta(ii,jj,k)
	      print*, ' 2xL' ,jj,ii,theta(ii,jj,k)
	         phbar = phbar +    pp(ii,jj,k)
	         icount= icount+ 1
	      endif
	    enddo

	     inc = inc + 1
	     ICX = ICX + 1
	     !PRINT*,' ICX = ', ICX,INC,icount,thbar
	     if( icx > 1000) exit
	    enddo
	    IF(ICOUNT > 0) THEN
	      theta(i,j,k) = thbar/float(icount)
	      pp(i,j,k)    = phbar/float(icount)
	      print*,'X',theta(i,j,k),icount,i,j,k
	      !print*,'max=' ,maxval(theta)
	    ELSE
	      print*,'no conv for' ,i,j,k

	      !STOP 'NO CONVERGENCE - slp_BRAMS'
	    ENDIF

	   if(k>1) then
	    ddz=b(k+1,4)-b(k,4)
            slp(i,j)=slp(i,j)-ddz*sl_g/(0.5*(theta(i,j,k+1)+theta(i,j,k)))
	   else
	    ddz=b(k,4)
	    slp(i,j)=slp(i,j)-ddz*sl_g/(0.5*(theta(i,j,k+1)+theta(i,j,k)))
	   endif

           !print*,'thbar= ',i,j,K,thbar
         ENDDO

         else
            thbar=.5*(theta(i,j,kbot)+theta(i,j,ktop))
	    phbar= 	pp(i,j,kbot)! checar
            slp(i,j)=phbar-ddz*sl_g/thbar
	    print*,' slpx=',slp(i,j),ddz,thbar,phbar
	 endif


         slp(i,j)=(slp(i,j)/sl_cp)**sl_cpor*sl_p00
         !slp(i,j)=phbar-ddz*sl_g/thbar
         !slp(i,j)=(slp(i,j)/sl_cp)**sl_cpor*sl_p00
	 PRINT*,'SLP=' , SLP(I,J),i,j; call flush(6)
      enddo
   enddo
stop 111
return
end subroutine BRAMS_comp_slpress
!----------------------------------------------------------------
!DSM{
subroutine RAMS_comp_slp_metar(n1,n2,n3,press,z,a,ztn)
!--- Esta subrotina reduz a pressao ao NMM utilizando a mesma
!--- formulacao empregada na reducao das medidas do METAR. Eh
!--- util para avaliar o modelo em relacao ao METAR.
!--- O procedimento de como eh realizada esta reducao foi fornecido
!--- por Rodrigo Gevaerd (vide email: avalia\E7ao da pressao)
!--- Demerval S. Moreira - 12/Mar/2012
integer :: n1,n2,n3,i,j
dimension a(n1,n2,n3),z(n1,n2),press(n1,n2,n3)
real ::g=9.8,R=287.04, Tm, To, Te

   do j=1,n2
      do i=1,n1
         altura=ztn+z(i,j)
         !Te=a(i,j,2)
         !To=Te + 0.0065*altura
         !Tm=(To+Te)/2
         Tm=288.15-0.0065*altura/2
         a(i,j,1)=press(i,j,2)/exp( -(g*altura)/(R*Tm) )
      enddo
   enddo
return
end
!DSM}
!DSM{
subroutine undef (n1,n2,n3,a,lev,lim_Donw,lim_Up)
real a(n1,n2,n3),lim_Donw,lim_Up
integer lev
do k = 1,n3
   do j = 1,n2
      do i = 1,n1
         if (k==lev .and. (a(i,j,k)<lim_Donw .or. a(i,j,k)>lim_Up .or. a(i,j,k)==-3.7158504e-26)) a(i,j,k)=-9.99e33
      enddo
   enddo
enddo

return
end
subroutine RAMS_comp_patch2(n1,n2,n3,n4,a,f,p2)
real a(n1,n2,n3,n4),f(n1,n2,n4),p2(n1,n2,n3)

! This routine is for quantities such as veg roughness that are not
! defined for water patches

do k = 1,n3
   do j = 1,n2
      do i = 1,n1
            p2(i,j,k) = a(i,j,k,2)
      enddo
   enddo
enddo

! Copy psum into f, which was passed in as a(1).  n3 may exceed n4 but this
! should be ok.

do k = 1,n3
   do j = 1,n2
      do i = 1,n1
         f(i,j,k) = p2(i,j,k)
      enddo
   enddo
enddo

return
end
!DSM}
subroutine calc_poda_index(n1,n2,n3,c,f,g,a)
implicit none
integer :: n1,n2,n3,i,j
real ::a(n1,n2,n3),c(n1,n2,n3),f(n1,n2,n3),g(n1,n2,n3)

real x,e
! g= dew point T in K
! f= sl pressure in hPa
! c=  temp in K

   !print*,"max.minf",maxval(f),minval(f)
   !print*,"max.ming",maxval(g),minval(g)
   !print*,"max.minc",maxval(c),minval(c)

   do j=2,n2-1
      do i=2,n1-1
         g(i,j,1)=g(i,j,1)-273.15
         x= 235.*log(f(i,j,1)/1.333224) - 1936.4 - g(i,j,1)*25.34 + &
	    g(i,j,1)*log(f(i,j,1)/1.333224)

	 e=exp( - x/(g(i,j,1)+235.) + log(f(i,j,1)/1.333224) - log(622.0) )

         a(i,j,1)=80.51 * f(i,j,1)/c(i,j,1) * (1. - e/f(i,j,1))
         a(i,j,1)=max(0.,a(i,j,1))
      enddo
   enddo
   a(1, :,1)=a(2   ,:,1)
   a(n1,:,1)=a(n1-1,:,1)
   a(: ,1,1)=a(:   ,2,1)
   a(:,n2,1)=a(:,n2-1,1)

   return
end

