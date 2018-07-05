! Author: Simon Mason
Module get_input_file
!
! Modules
  Use data_numbers, Only: rp
!
! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: get_xfile, get_yfile, get_zfile
!
Contains
!
!
 Function get_xfile()
!
! Accesses an X file, and determines its structure
!
! Error indicators:
!    ifail =  0 Successful
!    ifail =  1 Problem allocating memory
!
! Modules
  Use time,     Only: Operator(+), Operator(<), &
                      nss, &
                      date_diff
  Use gui,      Only: set_viewflags, window_update
  Use settings, Only: iel, igcms, iretro, istd, isynch, nf, nt, nv, nx, nz, xfd_old
  Use iofiles,  Only: bkfile, idir, mfile, xfile, yfile, zfile, &
                      file_refresh, init_ifile, set_mfile
  Use space,    Only: cstndx, cstnx, idomx, rlatdx, rlatx, rlngdx, rlngx
  Use fields,   Only: larea, xfield, yfield, zfield, &
                      init_field
  Use missing,  Only: kax, kaz, xmiss
  Use season,   Only: icsv
  Use analysis, Only: ianal
!
! Function type
  Integer :: get_xfile
!
! Locals
!
! Local scalars
  Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Associated, Len_Trim
!
! Executable Statements
!
! Get X input file
  get_xfile = 2
  nv => nx
  If (Len_Trim(xfile%cfile%desc) == 0) xfile%cfile%desc = 'X Input File'
  Call get_ifile ('X', xfile, xfield, mfile,yfield, nv, xmiss, rlatx, rlngx, rlatdx, rlngdx, cstnx, cstndx, idomx, kax, ifail)
!
! Reset forecast file
  If (ifail == 0) Then
     If (xfile%cfile%ffile /= bkfile%cfile%ffile) Then ! - if X file is the same as previous, update the settings in case the file was modified -
        xfd_old = xfile%fdate%iyr
        If (.not.larea) Then
           get_xfile = 0
           Return
        End If
        zfile = xfile
        zfile%lmax = xfile%lmax
        If ((nt > 0) .and. (xfile%fdate+(nt-1) < xfile%prdn%sdate)) Then ! - set default first forecast date to after training period - 
           zfile%fdate = xfile%fdate + nt
           nf = date_diff(zfile%fdate, zfile%prdn%sdate, zfile%iseq) + 1
        Else
           zfile%fdate = zfile%prdn%sdate
           nf = 1
        End If
        Call init_field (zfield, zfile%nfl, zfield(:)%rmiss, ifail) ! - reset Z fields -
        If (ifail == 0) Then
           zfield(:) = xfield(:)
           nz = nx
        Else
           ifail = 1
           Call init_ifile (zfile, &
                dir=idir)
           Call init_field (zfield, 1, (/xmiss/), ifail)
           nz = 0
        End If
     End If
     If (ifail == 0) Then
! - update forecast file settings in case of modified xfile contents -
        If (zfile%cfile%ffile == xfile%cfile%ffile) Then
           zfile%cprd1 = xfile%cprd1
           zfile%cprdn = xfile%cprdn
           zfile%cssn = xfile%cssn
           zfile%prd1 = xfile%prd1
           zfile%prdn = xfile%prdn
           If (Associated(kaz)) Then
              Deallocate (kaz)
              Nullify (kaz)
           End If
           Allocate (kaz(xfile%nt,xfile%nfl/nss), Stat=ifail)
           If (ifail == 0) Then
              kaz(:,:) = kax(:,:)
           Else
              ifail = 1
              Call init_ifile (zfile, &
                   dir=idir)
              Call init_field (zfield, 1, (/xmiss/), ifail)
              nz = 0
           End If
           get_xfile = 0
        End If
     End If
     Call file_refresh (zfile, &
          nv=nz)
     Call window_update (nf)
! - update Y-file in case of modified start date -
     Call file_refresh (yfile, &
          nv=nv)
!
! Disable any invalid analyses
     Call set_viewflags (ianal, istd, xfile%idstr, igcms, icsv, iel, isynch, &
          iretro=iretro)
!
! Check for calculation of seasonal values
     If (icsv > 0) yfile%iseq=xfile%iseq
     Call set_mfile (icsv)
!
! Reset fields if file is opened unsuccessfully
  Else
     If (.not.Associated(xfield)) Call init_field (xfield, 1, (/xmiss/), ifail)
     get_xfile = 1
  End If
!
  Return
 End Function get_xfile
!
!
!
 Function get_yfile()
!
! Accesses a Y file, and determines its structure
!
! Modules
  Use data_time_constants, Only: isq_yr
  Use time,                Only: Operator(>=), Operator(<=)
  Use labels,              Only: ca_dstruct
  Use gui,                 Only: set_viewflags
  Use settings,            Only: iel, igcms, iretro, istd, isynch, izero, nv, ny, yfd_old
  Use iofiles,             Only: xfile, yfile, &
                                 file_refresh, set_mfile
  Use space,               Only: cstndy, cstny, idomy, rlatdy, rlaty, rlngdy, rlngy
  Use fields,              Only: dsdy, xfield, yfield, &
                                 init_field, set_yfields
  Use missing,             Only: kay, ymiss
  Use climate,             Only: iclim, climate_bak, climate_per, &
                                 init_climatep
  Use season,              Only: icsv, lb
  Use analysis,            Only: ianal
!
! Function type
  Integer :: get_yfile
!
! Locals
!
! Local scalars
  Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Associated, Len_Trim
!
! Executable Statements
!
! Get Y input file
  get_yfile = 2
  nv => ny
  If (Len_Trim(yfile%cfile%desc) == 0) yfile%cfile%desc = 'Y Input File'
  Call get_ifile ('Y', yfile, yfield, xfile, xfield, nv, ymiss, rlaty, rlngy, rlatdy, rlngdy, cstny, cstndy, idomy, kay, ifail)
!
! Update field settings
  If (ifail == 0) Then
     Call set_yfields (lb)
!
! Get data structure description, and set default zero-bound setting 
     dsdy = ca_dstruct(yfile%idstr)
     yfd_old = yfile%fdate%iyr
     If (yfile%nfs == 1) Then
        Select Case (yfield(1)%var)
         Case ('precipitation', 'prcp', 'prec', 'pre', 'rain', 'rainfall')
           izero = 1
         Case Default
           izero = 0
        End Select
     Else
        izero = 0
     End If
!
! Reset climatology
     climate_bak = climate_per
     If ((climate_bak%d1 >= yfile%prd1%sdate) .and. (climate_bak%d2 <= yfile%prdn%sdate)) Then
! - set unused month and day for yearly sequencing -
        If (yfile%iseq == isq_yr) Then
           climate_per%d1%imn = yfile%prd1%sdate%imn
           climate_per%d1%idy = 0
           climate_per%d2%imn = yfile%prd1%sdate%imn
           climate_per%d2%idy = 0
        End If
     Else
        Call init_climatep ()
     End If
     iclim = 1
! - update X-file in case of modified start date -
     Call file_refresh (xfile, &
          nv=nv)
!
! Disable any invalid analyses
     Call set_viewflags (ianal, istd, xfile%idstr, igcms, icsv, iel, isynch, &
          iretro=iretro)
     get_yfile = 0
!
! Check for calculation of seasonal values
     If (icsv > 0) yfile%iseq=xfile%iseq
     Call set_mfile (icsv)
!
! Reset fields if file is opened unsuccessfully
  Else
     If (.not.Associated(yfield)) Call init_field (yfield, 1, (/ymiss/), ifail)
     get_yfile=1
  End If
!
  Return
 End Function get_yfile
!
!
!
 Function get_zfile()
!
! Accesses a forecast file
!
! Modules
  Use iofiles, Only: zfile, bkfile, &
                     get_old_file
!
! Function type
  Integer :: get_zfile
!
! Locals
!
! Local scalars
  Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Select input file
  bkfile = zfile
  If (Len_Trim(zfile%cfile%desc) == 0) zfile%cfile%desc = 'Forecast Data File'
  Call get_old_file (Trim(zfile%cfile%desc), zfile%cfile%ffile, zfile%cfile%fdir, (/'Forecast Data Files'/), (/'*.*'/), 1, &
       zfile%cfile%fname, ifail)
  If (ifail == 0) Then
     get_zfile = check_zfile()
  Else
     zfile = bkfile
     get_zfile = 2
  End If
!
  Return
 End Function get_zfile
!
!
!
 Function check_zfile() &
          Result (check)
!
! Opens a forecast file, and checks for consistency with the X file
!
! Error indicators:
!    ifail =  0 Successful
!    ifail =  1 Problem allocating memory
!    ifail =  2 File version is inconsistent with corresponding X file
!    ifail =  3 File structure is inconsistent with corresponding X file
!    4  <=  ifail  <=  8 Fields are inconsistent with corresponding X file
!
! Modules
  Use data_cpt_constants, Only: ia_pfv
  Use data_io_constants,  Only: iin, lfil
  Use time,               Only: Operator(+), Operator(<), &
                                date_diff, get_cdate
  Use errors,             Only: cproc, &
                                cpt_error
  Use settings,           Only: mza, nf, nt, nx, nz, &
                                record_change
  Use iofiles,            Only: bkfile, xfile, zfile, &
                                file_mdate, file_version, open_iofile
  Use space,              Only: cstndz, cstnz, idomz, rlatdz, rlatx, rlatz, rlngdz, rlngx, rlngz, &
                                reset_grids
  Use fields,             Only: xfield, zfield, &
                                init_field
  Use missing,            Only: kaz, xmiss
  Use season,             Only: lsn, nsn, &
                                get_cssn
  Use analysis,           Only: ianal
!
! Function type
  Integer :: check
!
! Locals
!
! Local scalars
  Integer :: l     ! - field / lagged-field index -
  Integer :: iarg1 ! - error optional argument -
  Integer :: iarg2 ! - error optional argument -
  Integer :: ifd   ! - current field -
  Integer :: ilf   ! - current lagged-field -
  Integer :: mdate ! - file modification date -
  Integer :: ifail ! - error indicator -
!
  Character(Len=lfil) :: carg2 ! - error optional argument -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Any, Trim
!
! Executable Statements
!
! Initialise routine
  cproc = 'check_zfile'
  carg2 = ' '
  iarg1 = 1
  iarg2 = 1
!
! Open file
  Call open_iofile (iin, zfile%cfile%ffile, .true., .true., .true., ifail)
  If (ifail /= 0) Then
     cproc = 'open_iofile'
     GoTo 1
  End If
!
! Check modification date
  mdate = file_mdate(zfile%cfile%ffile)
!
! Copy field information from X
  If ((zfile%cfile%ffile == bkfile%cfile%ffile) .and. (mdate == bkfile%mdate)) Then
     check = 2
     Return
  Else If (zfile%cfile%ffile == xfile%cfile%ffile) Then
     Call init_field (zfield, zfile%nfl, zfield(:)%rmiss, ifail)
     If (ifail /= 0) Then
        ifail = 1
        GoTo 1
     End If
     zfile = xfile
     zfield(:) = xfield(:)
!
! Check dataset structure
  Else
     zfile%cfile%ffmt%iver = file_version(zfile%cfile%ffile)
     If (zfile%cfile%ffmt%iver /= xfile%cfile%ffmt%iver) Then
        ifail = 2
        carg2 = xfile%cfile%ffile
        GoTo 1
     End If
     Call get_structure ((ianal == ia_pfv), zfile, zfield, xmiss, rlatz, rlngz, cstnz, rlatdz, rlngdz, cstndz, idomz, kaz, ifail)
     If (ifail /= 0) GoTo 2
! - check for consistency with xfile -
     If (zfile%idstr /= xfile%idstr) Then
        ifail = 3
        iarg1 = zfile%idstr
        iarg2 = xfile%idstr
        GoTo 1
     End If
     iarg1 = 0
     iarg2 = 0
!
! Check for consistency of field settings
     cproc = 'check_zfile'
     Do ifd = 1, zfile%nfs
        If (zfile%nfs > 1) iarg1 = ifd
        Do ilf = 1, zfile%nls
           If (zfile%nls > 1) iarg2 = ilf
           l = (ifd - 1)*zfile%nls + ilf
! - permit inconsistent precipitation abbreviations -
           If (zfield(l)%var /= xfield(l)%var) Then
              If (Trim(zfield(l)%var) == 'prec' .and. Trim(xfield(l)%var) == 'prcp') Then
                 zfield(l)%var = 'prcp'
              Else If (Trim(zfield(l)%var) == 'prcp' .and. Trim(xfield(l)%var) == 'prec') Then
                 zfield(l)%var = 'prec'
              Else
                 ifail = 4
                 carg2 = 'field'
                 GoTo 1
              End If
           End If
! - check for consistent units -
           If (zfield(l)%unit /= xfield(l)%unit) Then
              ifail = 4
              carg2 = 'unit'
              GoTo 1
           End If
! - check for consistent domains -
           If (zfield(l)%nlt /= xfield(l)%nlt) Then
              ifail = 5
              carg2 = 'latitudes'
              GoTo 1
           End If
           If (zfield(l)%nlg /= xfield(l)%nlg) Then
              ifail = 5
              carg2 = 'longitudes'
              GoTo 1
           End If
           If (zfield(l)%ln2s.NEQV.xfield(l)%ln2s) Then
              ifail = 6
              GoTo 1
           End If
           If (Any(rlatz(:,l) /= rlatx(:,l))) Then
              ifail = 7
              carg2 = 'Latitudes'
              GoTo 1
           End If
           If (Any(rlngz(:,l) /= rlngx(:,l))) Then
              ifail = 7
              carg2 = 'Longitudes'
              GoTo 1
           End If
        End Do
     End Do
! - free unnecessary array memory -
     Call reset_grids (rlatz,rlngz,cstnz,rlatdz,rlngdz,cstndz,idomz, ifail)
!
! Set date settings
     zfile%cprd1 = get_cdate(zfile%prd1, lsn(1), 2)
     zfile%cprdn = get_cdate(zfile%prdn, lsn(1), 2)
     If (zfile%iseq == 0) zfile%iseq = xfile%iseq
     zfile%cssn = get_cssn(zfile%prd1, zfile%iseq, nsn)
     If (zfile%cssn /= xfile%cssn) Then
        ifail = 8
        GoTo 1
     End If
     If (zfile%idstr == 3) zfield(:)%nv = zfield(:)%nlt
  End If
!
! Set modification date
  zfile%mdate = mdate
!
! Set default first date of interest and numbers of forecasts
  If (zfile%cfile%ffile == xfile%cfile%ffile) Then
     If (xfile%fdate+(nt-1) < xfile%prdn%sdate) Then
        zfile%fdate = xfile%fdate + nt
        nf = date_diff(zfile%fdate, zfile%prdn%sdate, zfile%iseq) + 1
     Else
        zfile%fdate = zfile%prdn%sdate
        nf = 1
     End If
  Else
     zfile%fdate = zfile%prd1%sdate
     nf = zfile%nt
     zfield(:)%region = xfield(:)%region
  End If
  nz = nx
  mza = 0
!
! Set if successful
  zfile%cssn = xfile%cssn
  zfile%cfile%lset = .true.
  check = record_change()
  check = 0
  ifail = 0
!
! Reset if error
1 Close (Unit=iin)
  If (ifail == 0) Return
  Call cpt_error (Trim(cproc), ifail, .false., &
       c_arg1=Trim(zfile%cfile%ffile), c_arg2=Trim(carg2), i_arg1=iarg1, i_arg2=iarg2)
2 check = 1
!
  Return
 End Function check_zfile
!
!
!
 Subroutine get_ifile (cxy, afile, afield, bfile, bfield, nv, rmiss, rlat, rlng, rlatd, rlngd, cstn, cstnd, idom, ka, ifail)
!
! Accesses an input file, and determines its structure
!
! On exit (if cproc = 'get_ifile'):
!    ifail =  0 Successful
!    ifail =  1 Gridded and station X input files not permitted for MLR
!    ifail =  2 Station and unreferenced X input files not permitted for GCM
!    ifail =  3 Multi-field X input files not permitted for GCM
!    ifail =  4 Insufficient number of cases
!    ifail =  5 Date sequencing is inconsistent with currently opened input file
!
! Modules
  Use data_numbers,        Only: ihuge
  Use data_cpt_constants,  Only: ia_cca, ia_mlr, ia_gcm, ia_pfv
  Use data_io_constants,   Only: lfil, lstn
  Use data_time_constants, Only: isq_mn, isq_sn, isq_so, isq_yr
  Use time,                Only: pdate,pprd, &
                                 Operator(<), Operator(<=), Operator(>), Operator(>=), Operator(==), Operator(+), Operator(-), &
                                 iseq, lss, &
                                 date_diff, get_cdate, reset_iseq, reset_time
  Use labels,              Only: ca_atypes_a, ca_dstruct_l
  Use gui,                 Only: ih_xfd, ih_yfd, &
                                 set_cursor_waiting, window_update
  Use errors,              Only: cproc, &
                                 cpt_error, query
  Use settings,            Only: iel, mnu, nmt, nt, &
                                 record_change
  Use iofiles,             Only: ifile, &
                                 bkfile, mfile, yfile, &
                                 file_mdate, file_refresh, file_version, get_maxnt, get_old_file, set_fdate
  Use space,               Only: full_domain, reset_grids
  Use fields,              Only: field, &
                                 bkfield, mfield, tfield, &
                                 get_area, init_field
  Use season,              Only: iafter, icsv, lprst, lsn, lensn, nsn, &
                                 get_cssn, get_seasons, set_icsv
  Use analysis,            Only: iaction, ianal, ianaln
  Use cpt_view,            Only: change_view
!
! Arguments
!
! Input scalars
  Real(Kind=rp), Intent(In) :: rmiss ! - default missing value flag -
!
  Character(Len=1), Intent(In) :: cxy ! - X / Y variables flag -
!
! Pointer scalars
  Integer, Pointer :: nv ! - number of variables -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: afile ! - input file -
  Type(ifile), Intent(InOut) :: bfile ! - second input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Pointer arrays
  Integer, Dimension(:,:), Pointer :: idom ! - variable is within domain -
!
  Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - used latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - realigned/used longitudes -
!
  Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - names of stations -
  Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - names of stations within domains -
!
  Logical, Dimension(:,:), Pointer :: ka ! - cases available? -
!
  Type(field), Dimension(:), Pointer :: afield ! - first field -
  Type(field), Dimension(:), Pointer :: bfield ! - second field -
!
! Locals
!
! Local scalars
  Integer :: iarg1   ! - error optional argument -
  Integer :: iarg2   ! - error optional argument -
  Integer :: l       ! - current field / lagged-field -
  Integer :: lag     ! - lag -
  Integer :: lagspia ! - afile lag-adjustment for persistence component of SPI -
  Integer :: lagspib ! - bfile lag-adjustment for persistence component of SPI -
  Integer :: nv_old  ! - backup number of variables -
  Integer :: n       ! - number of cases -
!
  Character(Len=lfil) :: carg1 ! - first character error message argument -
  Character(Len=lfil) :: carg2 ! - second character error message argument -
  Character(Len=lfil) :: carg3 ! - third character error message argument -
!
  Type(pdate) :: abdate ! - A beginning date -
  Type(pdate) :: afdate ! - A start date -
  Type(pdate) :: bbdate ! - B beginning date -
  Type(pdate) :: bfdate ! - B start date -
!
  Type(pprd) :: apprd ! - period -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic MaxVal, Min, Size, Sum, Trim
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_ifile'
  carg2 = ' '
  carg3 = ' '
  iarg1 = 0
  iarg2 = 0
!
! Backup old file
  bkfile = afile
  nv_old = nv
  If (.not.bfile%cfile%lset) Call reset_time ()
  Allocate (bkfield(Size(afield)))
  bkfield = afield
!
! Select input file
  Call get_old_file (Trim(afile%cfile%desc), afile%cfile%ffile, afile%cfile%fdir, (/cxy//' Input Files'/), (/'*.*'/), 1, &
       afile%cfile%fname, ifail)
  If (ifail /= 0) Then
     afile = bkfile
     nv = nv_old
     afield = bkfield
     Deallocate (bkfield)
     Return
  End If
  Call set_cursor_waiting (1)
!
! Check for version 10+ format
  afile%cfile%ffmt%iver = file_version(afile%cfile%ffile)
!
! Check modification date
  afile%mdate = file_mdate(afile%cfile%ffile)
!
! Determine dataset structure
  Select Case (cxy)
   Case ('X', 'x')
     lss=(ianal == ia_pfv)
     If (lss .and. bfile%cfile%lset) Then ! - reset mfields if Y file is set -
        Call init_field (bfield, 1, mfield(1:1)%rmiss, ifail)
        If (ifail /= 0) Return  
        bfield(1) = mfield(1)
     End If
   Case ('Y', 'y')
     lss = .false.
  End Select
  Call get_structure (lss, afile, afield, rmiss, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ka, ifail)
  If (ifail /= 0) GoTo 2
  cproc = 'get_ifile'
  If (afile%nt < mnu) Then
     iarg1 = afile%iseq
     ifail = 1
     GoTo 1
  Else If (afile%nt == 1) Then
     Select Case (cxy)
      Case ('Y', 'y')
        iarg1 = afile%iseq
        ifail = 10
        GoTo 1
     End Select
  End If
!
! Check for consistent file sequences
  If (bfile%cfile%lset) Then
! - permit single forecast PFV -
     If (afile%iseq == 0 .Or. bfile%iseq == 0) Then
        Select Case (cxy)
         Case ('X', 'x')
           Select Case (bfile%iseq)
            Case (:-1)
              afile%iseq = isq_yr
            Case Default
              afile%iseq = bfile%iseq
           End Select
         Case ('Y', 'y')
           Select Case (afile%iseq)
            Case (:-1)
              bfile%iseq = isq_yr
            Case Default
              bfile%iseq = afile%iseq
           End Select
        End Select
! - permit monthly sequenced Y-file -
     Else If (afile%iseq /= bfile%iseq) Then
        Select Case (cxy) 
         Case ('X', 'x')
           If (bfile%iseq == isq_mn) Call set_icsv (icsv,bfield(1)%unit)
           If (icsv /= 0) iseq=afile%iseq
         Case ('Y', 'y')
           If (afile%iseq == isq_mn) Call set_icsv (icsv,afield(1)%unit)
           iseq = bfile%iseq
        End Select
        If (icsv == 0) Then
           carg2 = bfile%cfile%ffile
           iarg1 = afile%iseq
           iarg2 = bfile%iseq
           ifail = 2
           GoTo 1
        End If
     Else
        icsv = 0
     End If
  Else
     iseq = afile%iseq
     icsv = 0
  End If
!
! Check for valid input file formats
  Select Case (cxy)
   Case ('X')
     If ((afile%idstr <= 2) .and. (ianal == ia_mlr)) Then ! - disable gridded and station X input files for MLR -
        Select Case (iel)
         Case (1)
           carg1 = ca_atypes_a(ianal)
           carg2 = ca_atypes_a(ia_cca)
           ifail = 3
           If (query(cproc, ifail, .true., c_arg1=Trim(carg1), c_arg2=Trim(carg2)) /= 1) GoTo 2
           ifail = 0
        End Select
        ianaln = ia_cca
        iarg1 = change_view()
     Else If ((afile%ngs == 1) .and. (ianal == ia_pfv)) Then ! - force probabilistic X files for PFV -
        Select Case (iel)
         Case (1)
           carg1 = ca_atypes_a(ianal)
           carg2 = ca_atypes_a(ia_cca)
           ifail = 4
           If (query(cproc, ifail, .true., c_arg1=Trim(carg1), c_arg2=Trim(carg2)) /= 1) GoTo 2
           ifail = 0
        End Select
        ianaln = ia_cca
        iarg1 = change_view()
     Else If ((afile%ngs > 1) .and. (ianal /= ia_pfv)) Then ! - enable probabilistic X files for PFV only -
        Select Case (iel)
         Case (1)
           carg1 = ca_atypes_a(ianal)
           carg2 = ca_atypes_a(ia_pfv)
           ifail = 5
           If (query(cproc, ifail, .true., c_arg1=Trim(carg1), c_arg2=Trim(carg2)) /= 1) GoTo 2
           ifail = 0
        End Select
        ianaln = ia_pfv
        iarg1 = change_view()
     End If
   Case ('Y')
     If ((afile%nms > 1) .and. (ianal == ia_gcm)) Then ! - disable multi-model Y input files for GCM -
        carg3 = ca_atypes_a(ianal)
        ifail = 6
        GoTo 1
     End If
     If (afile%ngs > 1) Then ! - disable probabilistic Y files -
        ifail = 7
        GoTo 1
     End If
  End Select
  If ((afile%nfl /= afile%nms) .and. (ianal == ia_gcm)) Then ! - disable multi-field input files for GCM -
     carg3 = ca_atypes_a(ianal)
     ifail = 8
     GoTo 1
  End If
!
! Check for compatibility of X and Y files for PFV
  If ((ianal == ia_pfv) .and. (bfile%cfile%lset)) Then
     Select Case (afile%idstr)
      Case (1)
        Select Case (cxy)
         Case ('X') ! - gridded forecasts -
           If (bfile%idstr == 3) ifail = 9
         Case ('Y') ! - gridded observations -
           If (bfile%idstr /= 1) ifail = 9
        End Select
      Case (2)
        Select Case (cxy)
         Case ('X') ! - station forecasts -
           If (bfile%idstr /= 2) ifail = 9
         Case ('Y') ! - station observations -
           If (bfile%idstr == 3) ifail = 9
        End Select
      Case (3) ! - gridded forecasts or observations -
        If (bfile%idstr /= 3) ifail = 9
     End Select      
  End If
  If (ifail /= 0) Then
     carg2 = cxy
     Select Case (cxy)
      Case ('X')
        carg3 = 'Y'
      Case ('Y')
        carg3 = 'X'
     End Select
     GoTo 1
  End If
!
! Identify number of ensemble members
  If (afile%lensemble) Then
     afile%nem = MaxVal(afield(:)%member)
  Else
     afile%nem = 0
  End If
!
! Prompt for season if necessary
  Call set_cursor_waiting (0)
  Select Case (cxy)
   Case ('X')
     Call get_seasons (ianal, afield, bfield, ifail)
   Case ('Y')
     mfile = afile
     Call get_seasons (ianal, bfield, afield, ifail)
  End Select
  Call set_cursor_waiting (1)
  If (ifail /= 0) GoTo 2
!
! Prompt for domain if relevant
  Select Case (afile%idstr)
! - gridded and station data -
   Case (1, 2)
     If (Size(afield) == Size(bkfield)) Then
        afield(:)%region%alim = bkfield(:)%region%alim
     Else
        afield(:)%region%alim = bkfield(1)%region%alim
     End If
     Call set_cursor_waiting (0)
     lss = (ianal == ia_pfv)
     tfield => afield(1:)
     Call get_area (cxy, afile%idstr, afile%nfs, afile%nls, afile%nms, afile%nem, lss, tfield, rlat, rlng, cstn, nv, rlatd, rlngd, &
          cstnd, idom, ifail)
     Nullify (tfield)
     Call set_cursor_waiting (1)
     If (ifail /= 0) GoTo 2
     If (lprst .and. cxy == 'Y') Then
        afield(0)%region = afield(1)%region
        afield(0)%nlt = afield(1)%nlt
        afield(0)%nlg = afield(1)%nlg
        afield(0)%nv = afield(1)%nv
        afield(0)%nva = afield(1)%nva
     End If
! - unreferenced data -
   Case (3)
     If (.not.lss) Then
        nv = Sum(afield(1:)%nlt)
     Else
        nv = MaxVal(afield(:)%nlt)
     End If
     afield(:)%nv = afield(:)%nlt
     cstnd(:,:) = cstn(:,:)
     Do l = 1, afile%nfl
        Call full_domain (afield(l)%nv, afield(l)%nlt, afield(l)%nlg, afield(l)%region, idom(:,l))
     End Do
     ifail = 0
! - cancel -
   Case Default
     nv = nv_old
     afile = bkfile
     afield = bkfield
     Deallocate (bkfield)
     Call set_cursor_waiting (0)
     Return
  End Select
!
! Update file settings
  afile%cfile%lset = .true.
  afile%cgss = Trim(ca_dstruct_l(afile%idstr))
  If (icsv == 0) lsn(1)=1
  Select Case (iseq)
   Case Default
     If (lprst .and. cxy == 'Y') Then
        apprd = afile%prd1
        Call reset_iseq (isq_mn)
        apprd%sdate = apprd%sdate - lsn(0)
        Call reset_iseq ()
        afile%cprd1 = get_cdate(apprd, lsn(1), 2)
     Else
        afile%cprd1 = get_cdate(afile%prd1, lsn(1), 2)
     End If
     afile%cprdn = get_cdate(afile%prdn, lsn(1), 2)
   Case (isq_sn, isq_so)
     afile%cprd1 = get_cdate(afield(1)%tprd, 1, 2)
     afile%cprdn = get_cdate(afile%prdn, 1 ,2)
  End Select
! - set season indicator if file version is CPT10+ -
  If (afile%cfile%ffmt%iver >= 10) Then
     Do l = 1, afile%nfl
        afield(l)%cssn = get_cssn(afield(l)%tprd, afile%iseq, 1)
     End Do
  Else
     afield(:)%cssn = 'N/A'
  End If
! - delete any old first date extraneous information -
  Select Case (afile%iseq)
   Case (isq_yr)
     afile%fdate%idy = afile%prd1%sdate%idy
     afile%fdate%imn = afile%prd1%sdate%imn
   Case (isq_mn, isq_sn, isq_so)
     afile%fdate%idy = afile%prd1%sdate%idy
  End Select
!
! Set default first date of interest
! - retain prior start date unless it is invalid -
  If ((afile%fdate < afile%prd1%sdate) .or. (afile%fdate > afile%prdn%sdate)) afile%fdate = afile%prd1%sdate
! - set appropriate lag if the second input file is open -
  If (bfile%cfile%lset) Then
     afdate = afile%fdate
     bfdate = bfile%fdate
     If (lprst) Then ! - calculate adjustment for persistence component -
        If (iseq < 0 .and. iseq /= isq_sn) Then
           Call reset_iseq (isq_new=isq_mn)
        Else
           Call reset_iseq (isq_new=iseq)
        End If
        Select Case (cxy)
         Case ('X', 'x')
           lagspia = 0
           lagspib = lsn(0)
           abdate = afdate
           bbdate = bfdate - lagspib
           Call reset_iseq ()
           Do
              If (bbdate >= mfile%prd1%sdate) Exit
              bbdate = bbdate + 1
           End Do
           If (iseq < 0 .and. iseq /= isq_sn) Then
              Call reset_iseq (isq_new=isq_mn)
           Else
              Call reset_iseq (isq_new=iseq)
           End If
           bfdate = bbdate + lagspib
         Case ('Y', 'y')
           lagspia = lsn(0)
           lagspib = 0
           bbdate = bfdate
           abdate = afdate - lagspia
           Call reset_iseq ()
           Do
              If (abdate >= mfile%prd1%sdate) Exit
              abdate = abdate + 1
           End Do
           If (iseq < 0 .and. iseq /= isq_sn) Then
              Call reset_iseq (isq_new=isq_mn)
           Else
              Call reset_iseq (isq_new=iseq)
           End If
           afdate = abdate + lagspia
        End Select
        Call reset_iseq ()
     Else
        lagspia = 0
        lagspib = 0
        abdate = afdate
        bbdate = bfdate
     End If
     Select Case (cxy)
      Case ('X')
        If ((date_diff(afile%prd1%sdate, bfdate, iseq) >= 0) .and. (bfdate <= afile%prdn%sdate)) Then ! - Y's start date is within X's data range -
           Call set_fdate (-iafter, afile%iseq, bfdate, afile%prd1%sdate, abdate, afdate, ifail)
           If (ifail /= 0) Call set_fdate (iafter, afile%iseq, afdate, mfile%prd1%sdate, bbdate, bfdate, ifail)
        Else If ((afile%prd1%sdate <= bfile%prdn%sdate) .and. (afile%prdn%sdate >= bfile%prd1%sdate)) Then ! - set start dates to earliest commonly available if there is overlap -
           If (iseq < 0 .and. iseq /= isq_sn) Then
              Call reset_iseq (isq_new=isq_mn)
           Else
              Call reset_iseq (isq_new=iseq)
           End If
           lag = date_diff(bfile%prd1%sdate, afile%prd1%sdate, iseq)
           Call reset_iseq ()
           If (lag >= 0) Then ! - Y file starts before X file -
              Call set_fdate (iafter, afile%iseq, afdate, bfile%prd1%sdate, bbdate, bfdate, ifail)
           Else ! - X file starts before Y file -
              bfdate = bfile%prd1%sdate
              bbdate = bfdate
              If (lagspib > 0) Then
                 If (iseq < 0 .and. iseq /= isq_sn) Then
                    Call reset_iseq (isq_new=isq_mn)
                 Else
                    Call reset_iseq (isq_new=iseq)
                 End If
                 bbdate = bfdate - lagspib
                 Call reset_iseq ()
              End If
              Call set_fdate (-iafter, afile%iseq, bfdate, afile%prd1%sdate, abdate, afdate, ifail)
           End If
        Else ! - no overlap -
           afdate = afile%prd1%sdate
           abdate = afdate
        End If
      Case ('Y')
        If ((date_diff(afile%prd1%sdate, bfdate, iseq) >= -1) .and. (bfdate <= afile%prdn%sdate)) Then ! - X start date is within Y's data range -
           Call set_fdate (iafter, bfile%iseq, bfdate, mfile%prd1%sdate, abdate, afdate, ifail)
           If (ifail /= 0) Call set_fdate (-iafter, bfile%iseq, afdate, bfile%prd1%sdate, bbdate, bfdate, ifail)
        Else If ((afile%prd1%sdate <= bfile%prdn%sdate) .and. (afile%prdn%sdate >= bfile%prd1%sdate)) Then ! - set start dates to earliest commonly available if there is overlap -
           If (iseq<0 .and. iseq /= isq_sn) Then
              Call reset_iseq (isq_new=isq_mn)
           Else
              Call reset_iseq (isq_new=iseq)
           End If
           lag = date_diff(bfile%prd1%sdate, afile%prd1%sdate, iseq)
           Call reset_iseq ()
           If (lag > 0) Then ! - X file starts before Y file -
              Call set_fdate (-iafter, bfile%iseq, afdate, bfile%prd1%sdate, bbdate, bfdate, ifail)
           Else ! - X file starts before Y file -
              bfdate = bfile%prd1%sdate
              bbdate = bfdate
              Call set_fdate (iafter, bfile%iseq, bfdate, afile%prd1%sdate, abdate, afdate, ifail)
           End If
        Else ! - no overlap -
           afdate = afile%prd1%sdate
           abdate = afdate
           If (lagspia > 0) Then
              If (iseq < 0 .and. iseq /= isq_sn) Then
                 Call reset_iseq (isq_new=isq_mn)
              Else
                 Call reset_iseq (isq_new=iseq)
              End If
              abdate = afdate - lagspia
              Call reset_iseq ()
           End If
        End If
     End Select
     afile%bdate = abdate
     afile%fdate = afdate
     bfile%bdate = bbdate
     bfile%fdate = bfdate
! - make sure any changes to Y file start date are applied if current file is X file -
     Select Case (cxy)
      Case ('X')
        yfile%fdate%iyr = bfile%fdate%iyr
        yfile%bdate%iyr = bfile%bdate%iyr
        If (ianal == ia_pfv) Then
           yfile%fdate%imn = bfile%fdate%imn
           yfile%bdate%imn = bfile%bdate%imn
        End If
      Case ('Y')
        mfile%fdate = afile%fdate
        mfile%bdate = afile%bdate
     End Select
!
! Set default length of training period
     If (nt == 0) nt = ihuge
     If (.not.lprst) Then
        Call get_maxnt (icsv, lensn, n, nmt)
     Else If (iseq /= isq_so) Then
        Call get_maxnt (icsv, lensn-lsn(0), n, nmt)
     Else
        Call get_maxnt (icsv, lsn(nsn), n, nmt)
     End If
     nt = Min(n, nt)
     Call window_update (nt)
  End If
! - check for file change -
  If ((afile%cfile%ffile /= bkfile%cfile%ffile) .or. (afile%mdate /= bkfile%mdate)) Then
     ifail = record_change()
! - check for both files open -
     If (bfile%cfile%lset) iaction = 1
  End If
  Deallocate (bkfield)
!
! Update screen
  iarg1 = afile%fdate%iyr
  Select Case (cxy)
   Case ('X', 'x')
     Call file_refresh (afile, &
          ih=ih_xfd, nv=nv)
   Case ('Y', 'y')
     Call file_refresh (afile, &
          ih=ih_yfd, nv=nv)
  End Select
  afile%fdate%iyr = iarg1 ! FTN95 bug - window_update@ changes afile%fdate%iyr -
! - switch off cursor waiting -
  Call set_cursor_waiting (0)
  ifail = 0
!
  Return
!
! Identify errors
1 Call cpt_error (Trim(cproc), ifail, .false., &
       c_arg1=Trim(afile%cfile%ffile), c_arg2=Trim(carg2), c_arg3=Trim(carg3), i_arg1=iarg1, i_arg2=iarg2)
!
! Reset old file
2 afile = bkfile
  nv = nv_old
  If (afile%cfile%lset) Then
     Call get_structure ((ianal == ia_pfv), afile, afield, rmiss, rlat, rlng, cstn, rlatd, rlngd, cstnd,idom, ka, ifail)
     If (ifail == 0) afield = bkfield
  Else
     Call reset_grids (rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ifail)
  End If
  Deallocate (bkfield)
  ifail = 1
! - switch off cursor waiting -
  Call set_cursor_waiting (0)
!
  Return
 End Subroutine get_ifile
!
!
!
 Subroutine get_structure (lss, afile, afield, rmiss, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ka, ifail)
!
! Determines structure of an input file, including its size, field settings, and missing cases
!
! On exit:
!    ifail =  0 Successful
!    ifail  /=  1 Depends on value of cproc
!
! Arguments
!
! Modules
  Use data_io_constants, Only: iin, lprd, lstn
  Use errors,            Only: cproc, &
                               cpt_error
  Use iofiles,           Only: ifile, &
                               open_iofile
  Use fields,            Only: field
!
! Input scalars
  Real(Kind=rp), Intent(In) :: rmiss ! - missing value flag -
!
  Logical, Intent(In) :: lss ! - permit seasonally-sequenced files? -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Pointer arrays
  Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
  Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - realigned used latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - realigned used longitudes -
!
  Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - names of stations -
  Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - names of stations within domain -
!
  Logical, Dimension(:,:), Pointer :: ka ! - cases available? -
!
  Type(field), Dimension(:), Pointer :: afield ! - field -
!
! Locals
!
! Local scalars
  Integer :: iarg1 ! - error optional argument -
  Integer :: iarg2 ! - error optional argument -
  Integer :: iarg3 ! - error optional argument -
  Integer :: ifd   ! - current field -
  Integer :: ilf   ! - current lagged-field -
  Integer :: igf   ! - current category field -
!
  Character(Len=lprd) :: cldate ! - last date read successfully -
  Character(Len=lprd) :: cfail  ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Open file
  Call open_iofile (iin,afile%cfile%ffile, .true., .true., .true., ifail)
  If (ifail /= 0) Then
     cproc = 'open_iofile'
     GoTo 1
  End If
!
! Determine file structure
  Select Case (afile%cfile%ffmt%iver)
   Case (9)
     Call get_structure_v9 (afile, afield, rmiss, rlat, rlng, cstn, rlatd, rlngd, cstnd, &
          idom, ka, ifail, cproc, cldate, cfail)
     iarg1 = 0
     iarg2 = 0
     iarg3 = 0
   Case (10)
     Call get_structure_v10 (lss, afile, afield, rmiss, rlat, rlng, cstn, rlatd, rlngd, cstnd, &
          idom, ka, ifd, ilf, igf, ifail, cproc, cldate, cfail)
     iarg1 = ifd
     iarg2 = ilf
     iarg3 = igf
   Case Default
     cproc = 'file_version'
     ifail = 5
     iarg1 = afile%cfile%ffmt%iver
     iarg2 = 0
     iarg3 = 0
     cldate = ' '
     cfail = ' '
  End Select
!
! Errors
1 If (ifail /= 0) Then
     Call cpt_error (Trim(cproc), ifail, .false., &
          c_arg1=Trim(afile%cfile%ffile), c_arg2=Trim(cldate), c_arg3=Trim(cfail), i_arg1=iarg1, i_arg2=iarg2, i_arg3=iarg3)
  Else
     afield(:)%unitc = afield%unit
  End If
  Close (Unit=iin)
!
  Return
 End Subroutine get_structure
!
!
!
 Subroutine get_structure_v9 (afile, afield, rmiss, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ka, ifail, cproc, cldate, cfail)
!
! Determines structure of a version-9 input file, including its size, field settings, and missing cases
!
! On exit (if cproc = 'get_structure_v9'):
!    ifail =  0 Successful
!    ifail =  1 Problem allocating memory
!    ifail =  2 Problem reading file. Data up to cldate read successfully
!    ifail =  3 Premature end of file reached. Data up to cldate read successfully
!    ifail =  4 File is not in a valid CPT format
!
! Modules
  Use data_io_constants,   Only: iin, lprd, lstn
  Use data_time_constants, Only: lmon
  Use time,                Only: pprd, &
                                 Assignment(=), Operator(==), &
                                 get_cdate, get_month
  Use labels,              Only: upcase
  Use iofiles,             Only: ifile
  Use fields,              Only: field, &
                                 init_field
!
! Arguments
!
! Input scalars
  Real(Kind=rp), Intent(In) :: rmiss ! - missing value flag -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
  Character(Len=*), Intent(Out) :: cproc  ! - procedure in error -
  Character(Len=*), Intent(Out) :: cldate ! - last date read successfully -
  Character(Len=*), Intent(Out) :: cfail  ! - error indicator -
!
! Pointer arrays
  Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
  Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - realigned used latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - realigned used longitudes -
!
  Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - names of stations -
  Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - names of stations within domain -
!
  Logical, Dimension(:,:), Pointer :: ka ! - cases available? -
!
  Type(field), Dimension(:), Pointer :: afield ! - field -
!
! Locals
!
! Local scalars
  Integer :: idy ! - current day -
!
  Character(Len=lmon) :: ccmon ! - current month -
  Character(Len=lprd) :: ctag  ! - tags -
!
  Type(pprd) :: pdlast ! - last period read successfully -
  Type(pprd) :: prd0   ! - null period -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_structure_v9'
  cldate = ' '
  cfail = ' '
  pdlast = 0
!
! Clear field information
  afile%nms = 1
  afile%nfs = 1
  afile%nls = 1
  afile%nfl = 1
  afile%ngs = 1
  Call init_field (afield, 1, (/rmiss/), ifail)
  If (ifail /= 0) Then
     ifail = 1
     GoTo 3
  End If
!
! Determine structure of dataset
  Read (Unit=iin, Fmt=*, Err=1, End=2) ctag
  Call upcase (ctag)
  Select Case (Trim(ctag))
   Case ('STATION', 'STATIONS', 'STN') ! - station dataset -
     afile%idstr = 2
     afile%igeog = 1
   Case ('YEARS', 'YEAR', 'NAME') ! - unreferenced -
     afile%idstr = 3
     afile%igeog = 0
   Case Default ! - gridded -
     Rewind (Unit=iin)
     Read (Unit=iin, Fmt=*, Err=1, End=2) idy, ccmon
     If (get_month(ccmon) == 0) Then
        ifail = 4
        GoTo 3
     End If
     afile%idstr = 1
     afile%igeog = 1
  End Select
!
! Determine structure of gridded dataset
  Select Case (afile%idstr)
   Case (1)
! - determine numbers of latitudes and longitudes -
     Call get_gridded_v9 (iin, afile, afield(1), rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ka, ifail, cproc, cfail, pdlast)
     afile%lmax = 0
!
! Determine structure of station and unreferenced dataset
   Case (2, 3)
     Call get_nongridded_v9 (iin, afile, afield(1), rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ka, ifail, cproc, cfail, pdlast)
  End Select
  If (ifail /= 0) GoTo 3
!
! Set field information and number of tag lines
  afield(1)%tprd = afile%prd1
  afield(1)%mdate = 0
  afile%ntag = 0
!
  Close (Unit=iin)
  ifail = 0
  Return
!
! Errors
! - problem reading file -
1 Close (Unit=iin)
  ifail = 2
  Return
! - problem reading file -
2 Close (Unit=iin)
  ifail = 3
  Return
!
! - other errors -
3 Close (Unit=iin)
!
! Identify last date read
  prd0 = 0
  If (.not.pdlast == prd0) cldate = get_cdate(pdlast, 1, 1)
!
  Return
 End Subroutine get_structure_v9
!
!
!
 Subroutine get_gridded_v9 (iin, afile, afield, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ka, ifail, cproc, cfail, pdlast)
!
! Determines structure of a version-9 gridded input file, including its size, field settings, and missing cases
!
! On exit (if cproc = 'get_gridded_v9'):
!    ifail =  0 Successful
!    ifail =  1 Problem allocating memory
!
! Modules
  Use data_io_constants, Only: lstn
  Use time,              Only: pprd
  Use iofiles,           Only: ifile
  Use space,             Only: reset_grids
  Use fields,            Only: field
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input unit number -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
  Type(field), Intent(InOut) :: afield ! - field -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
  Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
  Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
  Type(pprd), Intent(Out) :: pdlast ! - last date read successfully -
!
! Pointer arrays
  Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
  Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - realigned used latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - realigned used longitudes -
!
  Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - names of stations -
  Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - names of stations within domain -
!
  Logical, Dimension(:,:), Pointer :: ka ! - cases available? -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Size
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_gridded_v9'
!
! Determine numbers of latitudes and longitudes
  Call get_gridded_dimensions_v9 (iin, afield%nlg, afield%nlt, afield%ln2s, ifail, cfail)
  If (ifail /= 0) Then
     cproc = 'get_gridded_dimensions_v9'
     Return
  End If
!
! Read latitudes and longitudes
  Call reset_grids (rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ifail, &
       idstr=afile%idstr, ndom=1, nlt=(/afield%nlt/), nlg=(/afield%nlg/))
  If (ifail /= 0) Then
     ifail = 1
     Return
  End If
  Call get_gridded_latlons_v9 (iin, afield%nlg, afield%nlt, afield%ln2s, rlng(:,1), rlat(:,1), ifail)
  If (ifail /= 0) Then
     cproc = 'get_gridded_latlons_v9'
     Return
  End If
!
! Determine number of time steps and sequencing
  If (Size(rlatd, Dim=1) > Size(rlngd, Dim=1)) Then
     Call get_gridded_nt_v9 (iin, afile, afield%ln2s, afield%nlt, afield%nlg, &
          rlat(:,1), rlng(:,1), ka, rlatd(:,1), ifail, cproc, cfail, pdlast)
  Else
     Call get_gridded_nt_v9 (iin, afile, afield%ln2s, afield%nlt, afield%nlg, &
          rlat(:,1), rlng(:,1), ka, rlngd(:,1), ifail, cproc, cfail, pdlast)
  End If
!
  Return
!
 Contains
!
!
  Subroutine get_gridded_dimensions_v9 (iin, nlg, nlt, ln2s, ifail, cfail)
!
! Determines numbers of latitudes and longitudes in a version-9 gridded dataset
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 First line of data is too long. Data are probably truncated
!    ifail =  2 Unable to determine number of latitudes/longitudes (as indicated by cfail)
!    ifail =  3 Problem reading file
!
! Modules
  Use data_io_constants,   Only: lfli
  Use data_time_constants, Only: lmon
  Use time,                Only: get_month
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input unit number -
!
! Output scalars
  Integer, Intent(Out) :: nlg   ! - number of longitudes -
  Integer, Intent(Out) :: nlt   ! - number of latitudes -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
  Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
  Logical, Intent(Out) :: ln2s ! - north to south latitudes? -
!
! Locals
!
! Local scalars
  Integer :: j   ! - longitude index -
  Integer :: idy ! - day -
  Integer :: iyr ! - year -
  Integer :: inc ! - increment -
  Integer :: ll  ! - line length -
!
  Real(Kind=rp) :: dum  ! - dummy argument -
  Real(Kind=rp) :: rlt1 ! - first latitude -
  Real(Kind=rp) :: rlt2 ! - last latitude -
!
  Character(Len=lfli) :: fli   ! - first line of input -
  Character(Len=lmon) :: ccmon ! - current month -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Len_Trim
!
! Executable Statements
!
! Initialise routine
  cfail = ' '
!
! Determine length of first line of data
  Rewind (Unit=iin)
  ll=lfli
  Read (Unit=iin, Fmt='(A)', ERR=4, End=4) fli(1:ll)
  ll = Len_Trim(fli(1:ll))
! - return if line is too long -
  If (ll == lfli) Then
     ifail = 1
     Return
  End If
!
! Determine number of longitudes
  nlg = 1
  inc = 64
  Do
     Read (Unit=fli(1:ll), Fmt=*, Err=1, End=1) idy, ccmon, iyr, (dum, j=1,nlg)
     nlg = nlg + inc
     Cycle
1    nlg = nlg - inc
     If (inc == 1) Exit
     inc = inc/8
  End Do
! - return if unsuccessful -
  If (nlg < 1) Then
     ifail = 2
     cfail = 'longitudes'
     Return
  End If
!
! Determine number of latitudes
  nlt = 0
  Read (Unit=iin, Fmt=*, Err=3, End=2) rlt1, ccmon
  nlt = 1
! - return if unsuccessful -
2 If (nlt == 0) Then
     ifail = 2
     cfail = 'latitudes'
     Return
  End If
! - return if next line indicates a new date, implying no data -
  If (get_month(ccmon) > 0) Then
     ifail = 2
     cfail = 'latitudes'
     Return
  End If
! - read new lines until next date is reached -
  Do
     Read (Unit=iin, Fmt=*, Err=4, End=3) dum, ccmon
     If (get_month(ccmon) > 0) Exit
     rlt2 = dum
     nlt = nlt + 1
  End Do
!
! Check latitude ordering
3 If (nlt > 1) Then
     If (rlt1 > rlt2) Then
        ln2s = .true.
     Else
        ln2s = .false.
     End If
  Else
     ln2s = .true.
  End If
  ifail = 0
  Return
!
! Errors
4 ifail = 3
  Return
  End Subroutine get_gridded_dimensions_v9
!
!
!
  Subroutine get_gridded_latlons_v9 (iin, nlg, nlt, ln2s, rlng, rlat, ifail)
!
! Determines latitudes and longitudes in a version-9 gridded dataset
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file reached
!    ifail =  3 Latitudes are not consecutive
!    ifail =  4 Duplicate latitudes
!
! Modules
  Use data_time_constants, Only: lmon
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input unit number -
  Integer, Intent(In) :: nlg ! - number of longitudes -
  Integer, Intent(In) :: nlt ! - number of latitudes -
!
  Logical, Intent(In) :: ln2s ! - north to south latitudes? -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Output arrays
  Real(Kind=rp), Dimension(:), Intent(Out) :: rlng ! - longitudes -
  Real(Kind=rp), Dimension(:), Intent(Out) :: rlat ! - latitudes -
!
! Locals
!
! Local scalars
  Integer :: i    ! - latitude index -
  Integer :: i1   ! - first latitude index -
  Integer :: i2   ! - last latitude index -
  Integer :: j    ! - longitude index -
  Integer :: idy  ! - day -
  Integer :: iyr  ! - year -
  Integer :: iinc ! - latitude increment -
!
  Character(Len=lmon) :: ccmon ! - current month -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Any
!
! Executable Statements
!
! Read longitudes
  Rewind (Unit=iin)
  Read (Unit=iin, Fmt=*, Err=1, End=2) idy, ccmon, iyr, (rlng(j), j=1,nlg)
!
! Read latitudes
! - set latitude ordering -
  If (ln2s) Then
     i1 = 1
     i2 = nlt
     iinc = 1
  Else
     i1 = nlt
     i2 = 1
     iinc = -1
  End If
! - read latitudes -
  Do i = i1, i2, iinc
     Read (Unit=iin, Fmt=*, Err=1, End=2) rlat(i)
  End Do
! - check latitudes -
  ifail = 0
  If (Any(rlat(1:nlt-1)<rlat(2:nlt))) ifail = 3
  If (ifail == 3) Then
     If (Any(rlat(1:nlt-1) == rlat(2:nlt))) ifail = 4
     Return
  End If
  Return
!
! Errors
! - problem reading file -
1 ifail = 1
  Return
!
! - end of file -
2 ifail = 2
  Return
!
  End Subroutine get_gridded_latlons_v9
!
!
!
  Subroutine get_gridded_nt_v9 (iin, afile, ln2s, nlt, nlg, rlat, rlng, ka, dwk, ifail, cproc, cfail, pdlast)
!
! Determines numbers of cases in a gridded dataset
!
! On exit (if cproc = 'get_gridded_nt_v9'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to pdlast read successfully
!    ifail =  2 Premature end of file reached. Data up to pdlast read successfully
!    ifail =  3 Inconsistent latitudes or longitudes (depending on cfail). Data up to pdlast read successfully
!    ifail =  4 Invalid date sequencing (as indicated by cfail)
!    ifail =  5 Problem reading date (possible incorrect number of latitudes). Data up to pdlast read successfully
!    ifail =  6 Problem allocating memory
!
! Modules
  Use data_time_constants, Only: lmon
  Use time,                Only: Assignment(=), &
                                 date_diff, get_month, get_sequence, valid_pdate
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input unit number -
  Integer, Intent(In) :: nlt ! - number of latitudes -
  Integer, Intent(In) :: nlg ! - number of longitudes -
!
  Logical, Intent(In) :: ln2s ! - north to south latitudes? -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
  Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
  Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
  Type(pprd), Intent(Out) :: pdlast ! - last date read successfully -
!
! Input arrays
  Real(Kind=rp), Dimension(:), Intent(In) :: rlat ! - latitudes -
  Real(Kind=rp), Dimension(:), Intent(In) :: rlng ! - longitudes -
!
! Pointer arrays
  Logical, Dimension(:,:), Pointer :: ka ! - cases available? -
!
! Workspace arrays
  Real(Kind=rp), Dimension(:), Intent(InOut) :: dwk ! - workspace -
!
! Locals
!
! Local scalars
  Integer :: i    ! - latitude index -
  Integer :: j    ! - longitude index -
  Integer :: k    ! - case index -
  Integer :: dd   ! - date difference -
  Integer :: i1   ! - first latitude index -
  Integer :: i2   ! - last latitude index -
  Integer :: iinc ! - latitude increment -
!
  Character(Len=lmon) :: ccmon ! - current month -
!
  Type(pprd) :: prdp ! - previous date -
  Type(pprd) :: prdc ! - current date -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Any, Associated
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_gridded_nt_v9'
  cfail = ' '
  pdlast = 0
!
! Rewind to first date
  Rewind (Unit=iin)
!
! Read first date
  Read (Unit=iin, Fmt=*, Err=2, End=3) afile%prd1%sdate%idy, ccmon, afile%prd1%sdate%iyr
  afile%prd1%sdate%imn = get_month(ccmon)
  ifail = valid_pdate(afile%prd1%sdate)
  If (ifail /= 0) Then
     cproc = 'valid_pdate'
     Return
  End If
  afile%prd1%edate = afile%prd1%sdate
! - skip data -
  Do i = 1, nlt
     Read (Unit=iin, Fmt=*, Err=2, End=3)
  End Do
  pdlast=afile%prd1
  afile%nt=1
!
! Read second date
  Read (Unit=iin, Fmt=*, Err=2, End=1) prdc%sdate%idy, ccmon, prdc%sdate%iyr, (dwk(j), j=1,nlg)
  prdc%sdate%imn = get_month(ccmon)
  ifail = valid_pdate(prdc%sdate)
  If (ifail /= 0) Then
     cproc = 'valid_pdate'
     GoTo 4
  End If
  prdc%edate=prdc%sdate
! - check for consistency of longitudes -
  If (Any(dwk(1:nlg) /= rlng(1:nlg))) Then
     ifail = 3
     cfail = 'Longitudes'
     GoTo 4
  End If
! - read latitudes -
  Do i = 1, nlt
     Read (Unit=iin, Fmt=*, Err=2, End=3) dwk(i)
  End Do
! - check for consistency of latitudes -
  If (ln2s) Then
     i1 = 1
     i2 = nlt
     iinc = 1
  Else
     i1 = nlt
     i2 = 1
     iinc = -1
  End If
  If (Any(dwk(i1:i2:iinc) /= rlat(1:nlt))) Then
     ifail = 3
     cfail = 'Latitudes'
     GoTo 4
  End If
  afile%nt = afile%nt + 1
!
! Identify date sequencing
  Call get_sequence (afile%prd1, prdc, afile%iseq, ifail)
  Select Case (ifail)
   Case (0)
     pdlast = prdc
   Case Default
     cproc = 'get_sequence'
     GoTo 4
  End Select
!
! Read subsequent dates
  get_nt: Do
     prdp = pdlast
     Read (Unit=iin, Fmt=*, Err=2, End=1) prdc%sdate%idy, ccmon, prdc%sdate%iyr, (dwk(j), j=1,nlg)
     prdc%sdate%imn = get_month(ccmon)
     If (prdc%sdate%imn == 0) Then
        ifail = 5
        GoTo 4
     End If
     prdc%edate=prdc%sdate
! - check for consistency of longitudes -
     If (Any(dwk(1:nlg) /= rlng(1:nlg))) Then
        ifail = 3
        cfail = 'Longitudes'
        GoTo 4
     End If
! - read latitudes -
     Do i = 1, nlt
        Read (Unit=iin, Fmt=*, Err=2, End=3) dwk(i)
     End Do
! - check for consistency of latitudes -
     If (Any(dwk(i1:i2:iinc) /= rlat(1:nlt))) Then
        ifail = 3
        cfail = 'Latitudes'
        GoTo 4
     End If
     pdlast = prdc
  End Do get_nt
! - determine total number of cases from difference between start and end dates -
1 afile%nt = date_diff(afile%prd1%sdate, pdlast%sdate, afile%iseq) + 1
!
! Assign memory for available cases flags
  If (Associated(ka)) Then
     Deallocate (ka)
     Nullify (ka)
  End If
  Allocate (ka(afile%nt,1), Stat=ifail)
  If (ifail /= 0) Then
     ifail = 6
     GoTo 4
  End If
  ka(1,:) = .true.
!
! Return if there is only one case
  afile%prdn = pdlast
  If (afile%nt == 1) Return
!
! Rewind to second date
  Rewind (Unit=iin)
  Do k = 1, nlt+1
     Read (Unit=iin, Fmt=*, Err=2, End=3)
  End Do
!
! Check sequencing
  pdlast = afile%prd1
  dd = 1
  Do k = 2, afile%nt
     Select Case (dd) ! - identify whether to account for missing records -
! - read next record -
      Case (1)
        prdp = pdlast
        Read (Unit=iin, Fmt=*, Err=2, End=3) prdc%sdate%idy, ccmon, prdc%sdate%iyr
        Do i = 1, nlt
           Read (Unit=iin, Fmt=*, Err=2, End=3)
        End Do
        prdc%sdate%imn = get_month(ccmon)
        If (prdc%sdate%imn == 0) Then
           ifail = 5
           GoTo 4
        End If
        prdc%edate = prdc%sdate
        dd = date_diff(prdp%sdate, prdc%sdate, afile%iseq) ! - check whether next record is in sequence -
        If (dd == 1) Then
           ka(k,1) = .true.
           pdlast = prdc
        Else If (dd > 1) Then
           ka(k,1) = .false.
           Cycle
        Else
           ifail = 7
           GoTo 4
        End If
! - current record is missing, but prepare to read next record -
      Case (2)
        ka(k,1) = .true.
        pdlast = prdc
        dd = dd - 1
! - current record is missing -
      Case (3:)
        ka(k,1) = .false.
        pdlast = prdc
        dd = dd - 1
     End Select
  End Do
  ifail = 0
  Return
!
! Errors
! - problem reading file -
2 ifail = 1
  GoTo 4
!
! - end of file -
3 ifail = 2
!
! Free workspace
4 If (Associated(ka)) Then
     Deallocate (ka)
     Nullify (ka)
  End If
!
  Return
  End Subroutine get_gridded_nt_v9
 End Subroutine get_gridded_v9
!
!
!
 Subroutine get_nongridded_v9 (iin, afile, afield, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom,ka, ifail, cproc, cfail, pdlast)
!
! Determines structure of a version-9 gridded input file, including its size, field settings, and missing cases
!
! On exit (if cproc = 'get_nongridded_v9'):
!    ifail =  0 Successful
!    ifail =  1 Problem allocating memory
!
! Modules
  Use data_io_constants, Only: lstn
  Use time,              Only: pprd, &
                               Assignment(=)
  Use iofiles,           Only: ifile
  Use space,             Only: reset_grids
  Use fields,            Only: field
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input unit number -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
  Type(field), Intent(InOut) :: afield ! - field -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
  Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
  Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
  Type(pprd), Intent(Out) :: pdlast ! - last date read successfully -
!
! Pointer arrays
  Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
  Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - realigned used latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - realigned used longitudes -
!
  Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - names of stations -
  Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - names of stations within domain -
!
  Logical, Dimension(:,:), Pointer :: ka ! - cases available? -
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_nongridded_v9'
  cfail = ' '
  pdlast = 0
!
! Determine numbers of stations
  If (afile%idstr == 2) Then
     Call get_nongridded_dimensions_v9 (iin, afield%nlt, 4, afile%lmax, ifail)
  Else
     Call get_nongridded_dimensions_v9 (iin, afield%nlt, 2, afile%lmax, ifail)
  End If
  If (ifail /= 0) Then
     cproc = 'get_nongridded_dimensions_v9'
     If (afile%idstr == 2) Then
        cfail = 'stations'
     Else
        cfail = 'indices'
     End If
     Return
  End If
  afield%nlg = afield%nlt
!
! Read coordinates
  Call reset_grids (rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ifail, &
       idstr=afile%idstr, ndom=1, nlt=(/afield%nlt/), nlg=(/afield%nlg/))
  If (ifail /= 0) Then
     ifail = 1
     Return
  End If
  If (afile%idstr == 2) Then
     Call get_nongridded_latlons_v9 (iin, afield%nlt, cstn(:,1), ifail, &
          rlng=rlng(:,1), rlat=rlat(:,1))
  Else
     Call get_nongridded_latlons_v9 (iin, afield%nlt, cstn(:,1), ifail)
  End If
  If (ifail /= 0) Then
     cproc = 'get_nongridded_latlons_v9'
     Return
  End If
!
! Determine number of time steps and sequencing
! - station data -
  If (afile%idstr == 2) Then
     Call get_nongridded_nt_v9 (iin, 3, afile, ka, ifail, cproc, cfail, pdlast)
! - unreferenced data -
  Else
     Call get_nongridded_nt_v9 (iin, 1, afile, ka, ifail, cproc, cfail, pdlast)
  End If
  Return
!
 Contains
!
!
  Subroutine get_nongridded_dimensions_v9 (iin, nst, idat, lmax, ifail)
!
! Determines numbers of stations in a version-9 nongridded dataset
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 File is too wide; data are probably truncated
!    ifail =  3 Unable to determine number of stations
!
! Modules
  Use data_numbers,      Only: cdigits
  Use data_io_constants, Only: lfli
  Use iofiles,           Only: max_width
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin  ! - input unit number -
  Integer, Intent(In) :: idat ! - first line of data -
!
! Output scalars
  Integer, Intent(Out) :: nst   ! - number of stations -
  Integer, Intent(Out) :: lmax  ! - maximum length of line -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  Integer :: i   ! - station index -
  Integer :: inc ! - increment -
  Integer :: i1  ! - locator -
!
  Real(Kind=rp) :: dum ! - dummy argument -
!
  Character(Len=lfli) :: cline ! - line of input -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Scan, Verify
!
! Executable Statements
!
! Determine maximum length of line of data
  Rewind (Unit=iin)
  Call max_width (iin, lmax, ifail, &
       idat=idat, cline=cline)
  If (ifail == 1) Return
!
! Determine number of stations
  nst = 1
  inc = 64
  i1 = Scan(cline(1:lmax), cdigits)
  i1 = i1 + Verify(cline(i1+1:lmax), cdigits//'-/T:')
  Do
     Read (Unit=cline(i1:lmax), Fmt=*, Err=1, End=1) (dum, i=1,nst)
     nst = nst + inc
     Cycle
1    nst = nst - inc
     If (inc == 1) Exit
     inc = inc/8
  End Do
  If (nst == 0) ifail = 3
!
  Return
  End Subroutine get_nongridded_dimensions_v9
!
!
!
  Subroutine get_nongridded_latlons_v9 (iin, nst, cstn, ifail, rlng, rlat)
!
! Determines latitudes and longitudes in a version-9 nongridded dataset
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file reached
!
! Modules
  Use data_io_constants, Only: ldat
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input unit number -
  Integer, Intent(In) :: nst ! - number of stations -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Output arrays
  Character(Len=*), Dimension(:), Intent(Out) :: cstn ! - names of stations / indices -
! - optional output arrays -
  Real(Kind=rp), Dimension(:), Intent(Out), Optional :: rlng ! - longitudes -
  Real(Kind=rp), Dimension(:), Intent(Out), Optional :: rlat ! - latitudes -
!
! Locals
!
! Local scalars
  Integer :: i ! - station index -
!
  Character(Len=ldat) :: ctag ! - tags -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Present
!
! Executable Statements
!
! Read stations
  Rewind (Unit=iin)
  Read (Unit=iin, Fmt=*, Err=1, End=2) ctag, (cstn(i), i=1,nst)
!
! Read latitudes and longitudes
  If (Present(rlat) .and. Present(rlng)) Then
     Read (Unit=iin, Fmt=*, Err=1, End=2) ctag, (rlat(i), i=1,nst)
     Select Case (ctag(1:3))
      Case ('LAT', 'Lat', 'lat')
        Read (Unit=iin, Fmt=*, Err=1, End=2) ctag, (rlng(i), i=1,nst)
      Case ('LON', 'Lon', 'lon', 'LNG', 'Lng', 'lng')
        rlng(:) = rlat(:)
        Read (Unit=iin, Fmt=*, Err=1, End=2) ctag, (rlat(i), i=1,nst)
      Case Default
        ifail = 3
        Return
     End Select
  End If
  ifail = 0
  Return
!
! Errors
! - problem reading file -
1 ifail = 1
  Return
!
! - end of file -
2 ifail = 2
  Return
!
  End Subroutine get_nongridded_latlons_v9
!
!
!
  Subroutine get_nongridded_nt_v9 (iin, iskip, afile, ka, ifail, cproc, cfail, pdlast)
!
! Determines numbers of cases in a nongridded dataset
!
! On exit (if cproc = 'get_nongridded_nt_v9'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to pdlast read successfully
!    ifail =  2 Premature end of file reached. Data up to pdlast read successfully
!    ifail =  3 Inconsistent latitudes or longitudes (depending on cfail). Data up to pdlast read successfully
!    ifail =  4 Invalid date sequencing (as indicated by cfail)
!    ifail =  5 Problem reading date (possible incorrect number of latitudes). Data up to pdlast read successfully
!    ifail =  6 Problem allocating memory
!
! Modules
  Use data_io_constants,   Only: lprd
  Use data_time_constants, Only: isq_mn, isq_sn
  Use time,                Only: date_diff, get_pdate, get_sequence
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin   ! - input unit number -
  Integer, Intent(In) :: iskip ! - number of lines to skip -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
  Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
  Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
  Type(pprd), Intent(Out) :: pdlast ! - last date read successfully -
!
! Pointer arrays
  Logical, Dimension(:,:), Pointer :: ka ! - cases available? -
!
! Locals
!
! Local scalars
  Integer :: k  ! - case index -
  Integer :: dd ! - date difference -
!
  Character(Len=lprd) :: ctag1 ! - date tags -
  Character(Len=lprd) :: ctag2 ! - date tags -
!
  Type(pprd) :: prdp ! - previous date -
  Type(pprd) :: prdc ! - current date -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic AdjustL, Associated, Len_Trim, Trim
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_nongridded_nt_v9'
  cfail = ' '
  pdlast = 0
!
! Rewind to first date
  Rewind (Unit=iin)
  If (iskip > 0) Then
     Do k = 1, iskip
        Read (Unit=iin, Fmt=*, Err=3, End=3)
     End Do
  End If
!
! Read first date
  afile%nt = 0
  Read (Unit=iin, Fmt='(A)', Err=3, End=3) ctag1
  ctag1 = AdjustL(ctag1)
  Call get_pdate (' ',Trim(ctag1),afile%prd1%sdate, ifail, &
       edate=afile%prd1%edate)
  If (ifail /= 0) Then
     cproc = 'get_pdate'
     Return
  End If
  pdlast = afile%prd1
  afile%nt = afile%nt + 1
!
! Read second date
  Read (Unit=iin, Fmt='(A)', Err=3, End=2) ctag1
  ctag1 = AdjustL(ctag1)
  Call get_pdate (' ', Trim(ctag1), prdc%sdate, ifail, &
       edate=prdc%edate)
  If (ifail /= 0) Then
     cproc = 'get_pdate'
     GoTo 4
  End If
  afile%nt = afile%nt + 1
!
! Identify date sequencing
  Call get_sequence (afile%prd1, prdc, afile%iseq, ifail)
  Select Case (ifail)
   Case (0)
     If (afile%iseq == isq_mn) Then ! - block monthly sequencing in version 9 files -
        ifail = 2
        cfail = 'Monthly'
        Return
     Else If (afile%iseq == isq_sn) Then ! - block seasonal sequencing in version 9 files -
        ifail = 2
        cfail = 'Seasonal'
        Return
     End If
   Case Default
     cproc = 'get_sequence'
     GoTo 4
  End Select
!
! Skip to last date
  pdlast = prdc
  Do
     Read (Unit=iin, Fmt='(A)', Err=1, End=1) ctag2
     If (Len_Trim(ctag2) == 0) Exit
     ctag1 = ctag2
  End Do
1 ctag1 = AdjustL(ctag1)
  Call get_pdate (' ', Trim(ctag1), prdc%sdate, ifail, &
       edate=prdc%edate)
  If (ifail /= 0) GoTo 4
  pdlast = prdc
  afile%prdn = prdc
! - determine total number of cases from difference between start and end dates -
  afile%nt = date_diff(afile%prd1%sdate, afile%prdn%sdate, afile%iseq) + 1
!
! Assign memory for available cases flags
  If (Associated(ka)) Then
     Deallocate (ka)
     Nullify (ka)
  End If
2 Allocate (ka(afile%nt,1), Stat=ifail)
  If (ifail /= 0) Then
     ifail = 3
     GoTo 4
  End If
  ka(1,1) = .true.
! - return if there is only one case -
  pdlast = afile%prd1
  If (afile%nt == 1) Then
     afile%prdn = afile%prd1
     Return
  End If
!
! Rewind to second date
  Rewind (Unit=iin)
  If (iskip > 0) Then
     Do k = 1, iskip+1
        Read (Unit=iin, Fmt=*, Err=3, End=3)
     End Do
  End If
!
! Check sequencing
  dd = 1
  Do k = 2, afile%nt
     Select Case (dd) ! - identify whether to account for missing records -
! - read next record -
      Case (1)
        prdp = pdlast
        Read (Unit=iin, Fmt='(A)', Err=3, End=3) ctag1
        ctag1 = AdjustL(ctag1)
        Call get_pdate (' ', Trim(ctag1), prdc%sdate, ifail, &
             edate=prdc%edate)
        If (ifail /= 0) Then
           cproc = 'get_pdate'
           GoTo 4
        End If
        dd = date_diff(prdp%sdate, prdc%sdate, afile%iseq) ! - check whether next record is in sequence -
        If (dd == 1) Then
           ka(k,1) = .true.
           pdlast = prdc
        Else If (dd > 1) Then
           ka(k,1) = .false.
           Cycle
        Else
           ifail = 4
           GoTo 4
        End If
! - current record is missing, but prepare to read next record -
      Case (2)
        ka(k,1) = .true.
        pdlast = prdc
        dd = dd - 1
! - current record is missing -
      Case (3:)
        ka(k,1) = .false.
        pdlast = prdc
        dd = dd - 1
     End Select
  End Do
  afile%prdn = pdlast
  ifail = 0
  Return
!
! Errors
! - problem reading file -
3 ifail = 1
!
! Free workspace
4 If (Associated(ka)) Then
     Deallocate (ka)
     Nullify (ka)
  End If
  Return
!
  End Subroutine get_nongridded_nt_v9
 End Subroutine get_nongridded_v9
!
!
!
 Subroutine get_structure_v10 (lss, afile, afield, rmiss, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, &
            ka, ifd, ilf, igf, ifail, cproc, cldate, &
            cfail)
!
! Determines structure of a version 10 input file, including its size, field settings, and missing cases
!
! On exit (if cproc = 'get_structure_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!    ifail =  3 Unrecognized header line
!
! Modules
  Use data_io_constants, Only: cxmlns, iin, lstn, ltag
  Use time,              Only: pprd, &
                               Assignment(=), Operator(==), &
                               get_cdate
  Use iofiles,           Only: ifile
  Use fields,            Only: field
!
! Arguments
!
! Input scalars
  Real(Kind=rp), Intent(In) :: rmiss ! - missing value flag -
!
  Logical, Intent(In) :: lss ! - permit seasonally-sequenced files? -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifd   ! - field at error -
  Integer, Intent(Out) :: ilf   ! - lagged-field at error -
  Integer, Intent(Out) :: igf   ! - category field at error -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
  Character(Len=*), Intent(Out) :: cproc  ! - procedure in error -
  Character(Len=*), Intent(Out) :: cldate ! - last date read -
  Character(Len=*), Intent(Out) :: cfail  ! - error indicator -
!
! Pointer arrays
  Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
  Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - used longitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - realigned used longitudes -
!
  Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - names of stations -
  Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - names of stations within domain -
!
  Logical, Dimension(:,:), Pointer :: ka ! - cases available? -
!
  Type(field), Dimension(:), Pointer :: afield ! - field -
!
! Locals
!
! Local scalars
  Integer :: i1   ! - locator -
  Integer :: igrd ! - data structure indicator -
  Integer :: nt   ! - number of time steps -
  Integer :: nlh  ! - number of header lines -
!
  Character(Len=ltag) :: cline ! - line -
!
  Type(field) :: tfield ! - field information -
!
  Type(pprd) :: pdlast ! - last period read successfully -
  Type(pprd) :: prd0   ! - null period -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Associated, Index, Len, Trim
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_structure_v10'
  cldate=' '
  cfail = ' '
  pdlast = 0
  ifd = 0
  ilf = 0
  ifail = 1
!
! Initialise
  If (Associated(afield)) Nullify (afield)
  afile%nfs = 1
  nlh = 0
  afile%ntag = 0
  afile%nfs = 1
  afile%ngs = 1
!
! Read header lines
  headers: Do
     Read (Unit=iin, Fmt='(A)', Err=1, End=2) cline
     nlh=nlh+1
!
! Read XML namespace headers
     If (Index(cline,cxmlns//':') > 0) Then
        afile%ntag=afile%ntag+1
!
! Read number of fields
     Else If (Index(cline, 'cpt:nfields=') > 0) Then
        i1 = Index(cline, 'cpt:nfields=') + Len('cpt:nfields=')
        Read (Unit=cline(i1:), Fmt=*, Err=3) afile%nfs
        afile%ntag=afile%ntag+1
!
! Read number of categories
     Else If (Index(cline, 'cpt:ncats=') > 0) Then
        i1 = Index(cline, 'cpt:ncats=') + Len('cpt:ncats=')
        Read (Unit=cline(i1:), Fmt=*, Err=3) afile%ngs
        afile%ntag = afile%ntag + 1
!
! First field
     Else If (Index(cline, 'cpt:field=') > 0) Then
        Exit headers
!
! Skip unused tags
     Else If (Index(cline, 'cpt:') > 0) Then
        afile%ntag = afile%ntag + 1
!
! Unrecognised lines
     Else
        ifail = 4
        GoTo 3
     End If
  End Do headers
!
! Determine structure of dataset
  Call read_tags (Trim(cline)//' ', 4, (/'col    ', 'row    ', 'ncol   ', 'nrow   '/), .false., tfield, ifail, cfail, &
       idstr=igrd, nt=nt)
  If (ifail /= 0) Then
     cproc = 'read_tags'
     GoTo 3
  End If
! - disable multiple fields for unreferenced data -
  afile%idstr = igrd
  If (afile%idstr /= 3) Then
     afile%igeog = 1
  Else
     afile%igeog = 0
  End If
!
! Determine structure of gridded dataset
  Select Case (afile%idstr)
   Case (1)
     Call get_gridded_v10 (iin, afile, lss, afield, rmiss, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, &
          ka, ifail, ifd, ilf, igf, cproc, cfail, pdlast)
     afile%lmax = 0
!
! Determine structure of station and unreferenced datasets
   Case (2,3)
     Call get_nongridded_v10 (iin, nt, afile, lss, afield, rmiss, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, &
          ka, ifail, ifd, ilf, igf, cproc, cfail, pdlast)
  End Select
  If (ifail == 0) Return
!
  GoTo 3
!
! Errors
! - problem reading file -
1 ifail = 1
  GoTo 3
!
! - end of file -
2 ifail = 2
!
! - other errors -
3 If (afile%nfs == 1) ifd = 0
  If (afile%nls <= 1) ilf = 0
  If (afile%ngs == 1) igf = 0
!
! Identify last date read
  prd0 = 0
  If (.not.pdlast == prd0) cldate = get_cdate(pdlast, 1, 1)
!
  Return
 End Subroutine get_structure_v10
!
!
!
 Subroutine get_gridded_v10 (iin, afile, lss, afield, rmiss, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ka, ifail, &
            ifd, ilf, igf, cproc, cfail, pdlast)
!
! Determines structure of a version-10 gridded input file, including its size, field settings, and missing cases
!
! Modules
  Use data_io_constants, Only: lstn
  Use time,              Only: pprd, &
                               iseq, nss
  Use iofiles,           Only: ifile
  Use space,             Only: reset_grids
  Use fields,            Only: field, &
                               init_field
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input unit number -
!
  Logical, Intent(In) :: lss ! - permit seasonally-sequenced files? -
!
! Input scalars
  Real(Kind=rp), Intent(In) :: rmiss ! - missing value flag -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
  Integer, Intent(Out) :: ifd   ! - field at error -
  Integer, Intent(Out) :: ilf   ! - lagged-field at error -
  Integer, Intent(Out) :: igf   ! - category field at error -
!
  Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
  Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
  Type(pprd), Intent(Out) :: pdlast ! - last date read successfully -
!
! Pointer arrays
  Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
  Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - used latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - realigned/used longitudes -
!
  Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - names of stations -
  Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - names of stations within domain -
!
  Logical, Dimension(:,:), Pointer :: ka ! - cases available? -
!
  Type(field), Dimension(:), Pointer :: afield ! - field information -
!
! Locals
!
! Local scalars
  Integer :: iseqbk ! - backup time sequence indicator -
!
  Logical :: lfend ! - end of file? -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Associated,Max
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_gridded_v10'
  cfail = ' '
!
! Check for stacked fields and lagged-fields
  Call get_gridded_nls_v10 (iin, lss, afile, ifail, ifd, ilf, igf, cproc, cfail, pdlast)
  If (ifail /= 0) Return
!
! Initialise fields
  Call init_field (afield, afile%nfl, (/rmiss/), ifail)
  If (ifail /= 0) Then
     ifail = 1
     Return
  End If
!
! Get fields and maximum number of cases
  Call get_gridded_fields_v10 (iin, afile, afield, rmiss, lfend, ifail, ifd, ilf, igf, cproc, cfail, pdlast)
  iseqbk = iseq
  iseq = afile%iseq
  If (ifail /= 0) GoTo 1
!
! Initialise latitudes and longitudes
  Call reset_grids (rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ifail, &
       idstr=afile%idstr, ndom=afile%nfl, nlt=afield(:)%nlt, nlg=afield(:)%nlg)
  If (ifail /= 0) Then
     ifail = 1
     GoTo 1
  End If
!
! Assign memory for available cases flags
  If (Associated(ka)) Then
     Deallocate (ka)
     Nullify (ka)
  End If
  If (lss) Then
     Allocate (ka(afile%nt,afile%nfl/nss), Stat=ifail)
  Else
     Allocate (ka(afile%nt,afile%nfl), Stat=ifail)
  End If
  If (ifail /= 0) Then
     ifail = 1
     GoTo 1
  End If                                                                             
  ka(1:Max(nss,1),:) = .true.
  If (afile%nt > 1) ka(nss+1:,:)=.false.
!
! Get latitudes and longitudes, and identify available cases
  Call get_gridded_info_v10 (iin, lss, afile, afield, ka, rlat, rlng, rlatd, rlngd, lfend, ifail, &
       ifd, ilf, igf, cproc, cfail, pdlast)
!
! Free workspace
  If (ifail /= 0) Then
     If (Associated(ka)) Then
        Deallocate (ka)
        Nullify (ka)
     End If
  End If
!
! Restore sequence
1 iseq = iseqbk
  Return
!
 Contains
!
!
  Subroutine get_gridded_nls_v10 (iin, lss, afile, ifail, ifd, ilf, igf, cproc, cfail, pdlast)
!
! Determines stacking of any multiple fields, and number of lagged-fields in a version-10 gridded input file
!
! On exit (if cproc = 'get_gridded_nls_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!    ifail =  3 Category fields are not consecutive
!
! Modules
  Use data_time_constants, Only: isq_mn, isq_sn, isq_so, nmn
  Use data_numbers,        Only: zero
  Use time,                Only: Assignment(=), Operator(==), &
                                 ims, lms, nss, &
                                 date_diff
  Use fields,              Only: Operator(/=), &
                                 set_field
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input unit number -
!
  Logical, Intent(In) :: lss ! - permit seasonally-sequenced files? -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
  Integer, Intent(Out) :: ifd   ! - field at error -
  Integer, Intent(Out) :: ilf   ! - lagged-field at error -
  Integer, Intent(Out) :: igf   ! - category field at error -
!
  Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
  Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
  Type(pprd), Intent(Out) :: pdlast ! - last date read successfully -
!
! Locals
!
! Local scalars
  Integer :: k    ! - time index -
  Integer :: isq1 ! - first time sequencing -
!
  Type(field) :: fieldc ! - current field -
  Type(field) :: fieldp ! - previous field -
  Type(field) :: fieldn ! - new field -
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_gridded_nls_v10'
  ifd = 0
  ilf = 0
  igf = 0
  cfail = ' '
  pdlast = 0
  afile%nls = 0
  Call set_field (fieldc, zero)
  Call set_field (fieldp, zero)
!
! Rewind to first date
  Rewind (Unit=iin)
  If (afile%ntag > 0) Then
     Do k = 1, afile%ntag
        Read (Unit=iin, Fmt=*, Err=1, End=2)
     End Do
  End If
!
! Read first blocks
  ifd = 1
  ilf = 1
  igf = 1
  Call get_gridded_block_v10 (iin, igf, fieldc, .true., ifail, cproc, cfail)
  If (ifail /= 0) GoTo 3
  fieldp = fieldc
  afile%prd1 = fieldc%tprd
  pdlast = fieldc%tprd
!
! Read additional categories
  If (afile%ngs > 1) Then
     Do igf = 2, afile%ngs
        Call get_gridded_block_v10 (iin, igf, fieldc,.false., ifail, cproc, cfail, &
             bfield=fieldp)
        Select Case (ifail)
         Case (0)
           Continue
         Case (2)
           If (igf < afile%ngs) GoTo 3
         Case Default
           GoTo 3
        End Select
! - check for category ordering -
        If (fieldc%icat(igf) /= igf) Then
           cproc = 'get_gridded_nls_v10'
           ifail = 5
           GoTo 3
! - check for consistency -
        Else If ((fieldc /= fieldp) .or. (.not.(fieldc%tprd == fieldp%tprd))) Then ! - check for consistency of fields -
           cproc = 'get_gridded_nls_v10'
           ifail = 4
           GoTo 3
        Else
           fieldp%icat(igf) = igf
           fieldp%prob(igf) = fieldc%prob(igf)
        End If
     End Do
  End If
  igf = 1
  If (lss) Then
     ims(ilf) =fieldc%tprd%sdate%imn
     lms(ilf) = date_diff(fieldc%tprd%sdate, fieldc%tprd%edate, isq_mn)
  End If
!
! Read subsequent blocks
  isq1 = 0
  afile%nt = 0
  afile%nls = 1
  get_nls: Do
     Call get_gridded_block_v10 (iin, igf, fieldc, .false., ifail, cproc, cfail, &
          bfield=fieldp)
     Select Case (ifail)
      Case (0)
        Continue
      Case (-1)
        ifail = 0
      Case (2) ! - check for end of file -
        If (afile%nfs == 1) Then
           fieldc = fieldp
           afile%nt = 1
           afile%lensemble = .false.
           afile%lstack = .true.
           cproc = 'get_gridded_nls_v10'
           Exit get_nls
        Else
           Return
        End If
      Case Default
        If (afile%nfs > 1) ifd = ifd + 1
        If (afile%nls > 1) ilf = ilf + 1
        If (afile%ngs > 1) igf = igf + 1
        Return
     End Select
     pdlast = fieldc%tprd
!
! Check for stacked fields
     If (afile%nls == 1) Then
        If (afile%nfs > 1) Then
           If (fieldp%member == fieldc%member) Then
              afile%lensemble = .false.
              If ((fieldp%var /= fieldc%var) .or. (fieldp%z%hght /= fieldc%z%hght)) Then
                 afile%lstack = .false.
              Else
                 afile%lstack = .true.
              End If
           Else
              afile%lensemble = .true.
              afile%lstack = .false.
           End If
        Else
           afile%lensemble = .false.
           afile%lstack = .true.
        End If
     End If
!
! Skip remaining categories
     If (afile%ngs > 1) Then
        fieldn = fieldc
        Do igf = 2, afile%ngs
           Call get_gridded_block_v10 (iin, igf, fieldc, .false., ifail, cproc, cfail, &
                bfield=fieldn)
           If (ifail /= 0) Return
           pdlast = fieldc%tprd
        End Do
        igf = 1
     End If
!
! Skip remaining unstacked fields
     If (.not.afile%lstack) Then
        If (afile%nfs > 2) Then
           Do ifd = 3, afile%nfs
              Do igf = 1, afile%ngs
                 Call get_gridded_block_v10 (iin, igf, fieldc,.false., ifail, cproc, cfail, &
                      bfield=fieldp)
                 If (ifail /= 0) Return
                 pdlast = fieldc%tprd
              End Do
           End Do
           igf = 1
        End If
! - read next time step if necessary -
        Call get_gridded_block_v10 (iin, igf, fieldc,.true., ifail, cproc, cfail, &
             bfield=fieldp)
        Select Case (ifail)
         Case (0)
           Continue
         Case (2) ! - check for end of file -
           fieldc = fieldp
           afile%nt = 1
           cproc = 'get_gridded_nls_v10'
           Exit get_nls
         Case Default
           Return
        End Select
     End If
!
! Identify date sequencing
     Call determine_sequence (fieldp%tprd, fieldc%tprd, lss, isq1, afile%iseq, afile%nls, pdlast, ifail, &
          ilf=ilf)
     If (isq1 == 0) isq1 = afile%iseq
     Select Case (ifail)
      Case (0)
        Exit get_nls
      Case (1)
        If (lss) Then
           ims(ilf) = fieldc%tprd%sdate%imn
           lms(ilf) = date_diff(fieldc%tprd%sdate, fieldc%tprd%edate, isq_mn)
        End If
        Cycle get_nls
      Case Default
        cproc = 'get_sequence'
        Return
     End Select
  End Do get_nls
!
! Reassign seasonal sequencing
  If (isq1 == isq_mn .and. afile%nls == nmn) Then
     afile%iseq = isq_mn
     afile%nls=1
  Else If (isq1 == isq_so .and. afile%nls <= nmn) Then
     afile%iseq = isq_so
     afile%nls = 1
  Else If (isq1 == isq_sn .and. afile%nls <= nmn) Then
     afile%iseq = isq_sn
  End If
  If (lss) nss = afile%nls
  afile%nfl = afile%nfs*afile%nls
!
  ifail = 0
  Return
!
! Errors
! - problem reading file -
1 ifail = 1
  Return
!
! - end of file -
2 ifail = 2
  Return
!
! - other errors -
3 If (afile%ngs > 1) igf = igf - 1
!
  Return
  End Subroutine get_gridded_nls_v10
!
!
!
  Subroutine get_gridded_fields_v10 (iin, afile, afield, rmiss, lfend, ifail, ifd, ilf, igf, cproc, cfail, pdlast)
!
! Determines field settings and number of cases
!
! On exit (if cproc = 'get_gridded_fields_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field
!    ifail =  2 Premature end of file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field
!    ifail =  3 Season for last date of ifd field, ilf lagged-field is inconsistent with earlier dates
!    ifail =  4 Climatological probability for the igf category, ifd field, ilf lagged-field is inconsistent with earlier dates
!    ifail =  5 Information for last date of ifd field, ilf lagged-field is inconsistent with earlier information
!    ifail =  6 Last date for ifd field, ilf lagged-field is not in sequence
!
! Modules
  Use data_time_constants, Only: isq_sn, isq_yr
  Use time,                Only: Assignment(=), Operator(==), &
                                 check_pprd, date_diff
  Use fields,              Only: set_field
  Use season,              Only: get_cssn
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input unit number -
!
  Real(Kind=rp), Intent(In) :: rmiss ! - missing value flag -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
  Integer, Intent(Out) :: ifd   ! - field at error -
  Integer, Intent(Out) :: ilf   ! - lagged-field at error -
  Integer, Intent(Out) :: igf   ! - category field at error -
!
  Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
  Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
  Logical, Intent(Out) :: lfend ! - end of file? -
!
  Type(pprd), Intent(Out) :: pdlast ! - last date read successfully -
!
! Input/output arrays
  Type(field), Dimension(:), Intent(InOut) :: afield ! - field information -
!
! Locals
!
! Local scalars
  Integer :: k   ! - case index -
  Integer :: l   ! - field/lagged-field index -
  Integer :: n   ! - number of read time steps -
  Integer :: ntn ! - number of time steps -
!
  Type(field) :: tfield ! - temporary field information -
  Type(field) :: pfield ! - previous field information -
  Type(field) :: nfield ! - new information -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Max
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_gridded_fields_v10'
  ifd = 0
  ilf = 0
  igf = 0
  cfail = ' '
  pdlast = 0
  Call set_field (tfield, rmiss)
  Call set_field (pfield, rmiss)
!
! Rewind to first date
  Rewind (Unit=iin)
  Do k = 1, afile%ntag
     Read (Unit=iin, Fmt=*, Err=1, End=2)
  End Do
!
! Read first lagged-fields, stacked fields, and category fields
  If (afile%lstack) Then
     afile%nt = 1
     Do ifd = 1, afile%nfs
        Select Case (afile%iseq)
         Case Default
           n = 1
         Case (isq_sn)
           n = afile%nls
        End Select
        Do ilf = 1, afile%nls
           l = (ifd - 1)*afile%nls + ilf
           If ((ifd == 1) .or. (ilf /= 1)) Then
              If (l == 1) Then
                 Call get_gridded_block_v10 (iin, 1, afield(l), .true., ifail, cproc, cfail)
              Else
                 Call get_gridded_block_v10 (iin, 1, afield(l), .false., ifail, cproc, cfail, &
                      bfield=afield(l-1))
              End If
              If (ifail /= 0) Return
              afield(l)%cssn = get_cssn(afield(l)%tprd, afile%iseq, 1)
              pdlast = afield(l)%tprd
              If (afile%ngs > 1) Then ! - read additional categories -
                 Do igf = 2, afile%ngs
                    Call get_gridded_block_v10 (iin, igf, tfield, .false., ifail, cproc, cfail, &
                         bfield=afield(l))
                    If (ifail /= 0) Return
                    afield(l)%icat(igf) = tfield%icat(igf)
                    afield(l)%prob(igf) = tfield%prob(igf)
                 End Do
              End If
           End If
        End Do
        pfield = afield(l)
!
! Continue to read blocks until new stacked field is found
        new_stack: Do
           Do ilf = 1, afile%nls
              l = (ifd - 1)*afile%nls + ilf
              igf = 1
              Call get_gridded_block_v10 (iin, igf, tfield, .true., ifail, cproc, cfail, &
                   bfield=afield(l))
              Select Case (ifail)
               Case (0)
                 Continue
               Case (2)
                 If ((ifd<afile%nfs) .or. (Trim(cproc) /= 'get_gridded_block_v10')) Then
                    Return
                 Else
                    cproc = 'get_gridded_fields_v10'
                    ifail = 1
                    Exit new_stack
                 End If
               Case Default
                 Return
              End Select
              tfield%cssn = get_cssn(tfield%tprd, afile%iseq, 1)
! - check for changes in field variable -
              If ((tfield%member /= afield(l)%member) .or. (tfield%var /= afield(l)%var) .or.  &
                  (tfield%z%hght /= afield(l)%z%hght) .or. (tfield%model /= afield(l)%model)) Then
                 If (ifd < afile%nfs) Then
                    l = ifd*afile%nls + 1
                    afield(l) = tfield
                    Exit new_stack
                 Else ! - error if already at last field -
                    cproc = 'get_gridded_fields_v10'
                    ifail = 2
                    Return
                 End If
! - check for lagged fields as separate fields -
              Else If ((afile%iseq == isq_yr) .and. (tfield%cssn /= afield(l)%cssn)) Then
                 If (l < afile%nls) Then
                    l = l + 1
                    Cycle
                 Else If (ifd < afile%nfs) Then
                    l = ifd*afile%nls + 1
                    afield(l) = tfield
                    Exit new_stack
                 Else ! - error if season is inconsistent for last field -
                    cproc = 'get_gridded_fields_v10'
                    ifail = 3
                    Return
                 End If
! - check for identical fields -
              Else If ((tfield%var == afield(l)%var) .and. (tfield%tprd == afield(l)%tprd)) Then
                 If (ifd < afile%nfs) Then
                    l = ifd*afile%nls + 1
                    afield(l) = tfield
                    Exit new_stack
                 End If
              End If
! - check for consistency of climatological probabilities -
              If (afile%ngs > 1) Then
                 If (tfield%prob(igf) /= afield(l)%prob(igf)) Then
                    cproc = 'get_gridded_fields_v10'
                    igf = afile%ngs
                    ifail = 4
                    Return
                 End If
              End If
! - check for consistency of periods -
              ifail = check_pprd(afield(l)%cssn, tfield%cssn, afield(l)%tprd, tfield%tprd, afile%iseq)
              Select Case (ifail)
               Case (0)
                 Continue
               Case (5)
                 ifail = check_pprd(afield(l)%cssn, tfield%cssn, afield(1)%tprd, tfield%tprd, afile%iseq)
                 If (ifail /= 0) Then
                    cproc = 'check_pprd'
                    Return
                 End If
               Case Default
                 cproc = 'check_pprd'
                 Return
              End Select
              pdlast = tfield%tprd
              pfield = tfield
              n = n + 1
! - read additional categories -
              If (afile%ngs > 1) Then
                 Do igf = 2, afile%ngs
                    Call get_gridded_block_v10 (iin, igf, tfield, .false., ifail, cproc, cfail, &
                         bfield=afield(l))
                    If (ifail /= 0) Return
! - check for consistency of climatological probabilities -
                    If (tfield%prob(igf) /= afield(l)%prob(igf)) Then
                       cproc = 'get_gridded_fields_v10'
                       ifail = 4
                       Return
                    End If
                 End Do
              End If
           End Do
        End Do new_stack
        pdlast = tfield%tprd
        igf = 1
!
! Check whether last case is for last lagged-field
        If (n > 1) Then
           lfend = .false.
           tfield = pfield
           If (afile%iseq == isq_yr) Then
              Do ilf = afile%nls, 1, -1
                 l = (ifd - 1)*afile%nls + ilf
                 If ((tfield%tprd%sdate%imn == afield(l)%tprd%sdate%imn) &
               .and. (tfield%tprd%sdate%idy == afield(l)%tprd%sdate%idy) &
               .and. (tfield%tprd%edate%imn == afield(l)%tprd%edate%imn) &
               .and. (tfield%tprd%edate%idy == afield(l)%tprd%edate%idy)) Then
                    Exit
                 Else If (ilf > 1) Then
                    lfend = .true.
                 Else ! - is this possible? -
                    cproc = 'get_gridded_fields_v10'
                    ifail = 5
                    Return
                 End If
              End Do
           End If
!
! Calculate number of cases
           Select Case (afile%iseq)
            Case Default
              ntn = date_diff(afield(l)%tprd%sdate, tfield%tprd%sdate, afile%iseq) + 1
              If (ntn < 1) Then
                 cproc = 'get_gridded_fields_v10'
                 ifail = 6
                 Return
              End If
            Case (isq_sn)
              ntn = n
           End Select
           afile%nt = Max(afile%nt, ntn)
        Else
           ifail = 0
           lfend = .false.
        End If
     End Do
!
! Read first unstacked fields, lagged-fields, and category fields
  Else
     Do ilf = 1, afile%nls
        Do ifd = 1, afile%nfs
           l = (ifd - 1)*afile%nls + ilf
           igf = 1
           If (l == 1) Then
              Call get_gridded_block_v10 (iin, igf, afield(l), .true., ifail, cproc, cfail)
           Else
              Call get_gridded_block_v10 (iin, igf, afield(l), .false., ifail, cproc, cfail, &
                   bfield=afield(l-1))
           End If
           If (ifail /= 0) Return
           afield(l)%cssn = get_cssn(afield(l)%tprd, afile%iseq, 1)
           pdlast = afield(l)%tprd
           If (afile%ngs > 1) Then
              Do igf = 2, afile%ngs
                 Call get_gridded_block_v10 (iin, igf, tfield, .false., ifail, cproc, cfail, &
                      bfield=afield(l))
                 If (ifail /= 0) Return
                 afield(l)%icat(igf) = tfield%icat(igf)
                 afield(l)%prob(igf) = tfield%prob(igf)
              End Do
           End If
        End Do
     End Do
!
! Read to end of file
     n = 0
     file_end: Do
        Do ilf = 1, afile%nls
           Do ifd = 1, afile%nfs
              l = (ifd - 1)*afile%nls + ilf
              Do igf = 1, afile%ngs
                 If (l == 1) Then
                    Call get_gridded_block_v10 (iin, igf, tfield, .true., ifail, cproc, cfail, &
                         bfield=afield(l))
                 Else
                    If (.not.(afield(l)%tprd == afield(l-1)%tprd)) Then
                       Call get_gridded_block_v10 (iin, igf, tfield, .false., ifail, cproc, cfail, &
                            bfield=afield(l))
                    Else
                       nfield = afield(l)
                       nfield%tprd = tfield%tprd
                       Call get_gridded_block_v10 (iin, igf, tfield, .false., ifail, cproc, cfail, &
                            bfield=nfield)
                    End If
                 End If
                 Select Case (ifail)
                  Case (0)
                    pdlast = tfield%tprd
                    pfield = tfield
                    n = n + 1
                  Case (2)
                    cproc = 'get_gridded_fields_v10'
                    If (igf == afile%ngs) Then
                       ifail = 0
                       Exit file_end
                    Else
                       ifail = 1
                       Return
                    End If
                  Case Default
                    Return
                 End Select
                 If (afile%ngs > 1) Then ! - read additional categories -
                    If (tfield%prob(igf) /= afield(l)%prob(igf)) Then ! - check for consistency of climatological probabilities -
                       cproc = 'get_gridded_fields_v10'
                       ifail = 4
                       Return
                    End If
                 End If
              End Do
           End Do
        End Do
     End Do file_end
!
! Check whether last case is for last unstacked field
     igf = 1
     tfield = pfield
     lfend = .false.
     Do ifd = afile%nfs, 1, -1
        l = ifd*afile%nls
        If ((tfield%member == afield(l)%member) .or. (tfield%var /= afield(l)%var) .or. (tfield%z%hght /= afield(l)%z%hght)) Then
           Exit
        Else If (ifd > 1) Then
           lfend = .true.
        Else
           cproc = 'get_gridded_fields_v10'
           ifail = 5
           Return
        End If
     End Do
!
! Check whether last case is for last lagged-field
     If (.not.lfend) Then
        If ((afile%iseq == isq_yr) .and. (n > 0)) Then
           Do ilf = afile%nls, 1, -1
              If ((tfield%tprd%sdate%imn == afield(ilf)%tprd%sdate%imn) .and.  &
                  (tfield%tprd%sdate%idy == afield(ilf)%tprd%sdate%idy) .and.  &
                  (tfield%tprd%edate%imn == afield(ilf)%tprd%edate%imn) .and.  &
                  (tfield%tprd%edate%idy == afield(ilf)%tprd%edate%idy)) Then
                 Exit
              Else If (ilf > 1) Then
                 lfend = .true.
              Else
                 cproc = 'get_gridded_fields_v10'
                 ifail = 5
                 Return
              End If
           End Do
        End If
!
! Calculate number of cases
        If (n > 0) Then
           afile%nt = date_diff(afield(ilf)%tprd%sdate, tfield%tprd%sdate, afile%iseq) + 1
           If (afile%nt < 1) Then
              cproc = 'get_gridded_fields_v10'
              ifail = 6
              Return
           End If
        Else
           afile%nt = 1
        End If
     End If
  End If
!
! Identify number of models
  afile%nms = 1
  If (afile%nfl > 1) Then
     Do l = 2, afile%nfl
        If (Any(afield(1:l-1)%model == afield(l)%model)) Cycle
        afile%nms = afile%nms + 1
     End Do
  End If
!
! Set number of cases
  ifail = 0
  Return
!
! Errors
! - problem reading file -
1 ifail = 1
  Return
!
! - problem reading file -
2 ifail = 2
!
  Return
  End Subroutine get_gridded_fields_v10
!
!
!
  Subroutine get_gridded_info_v10 (iin, lss, afile, afield, ka, rlat, rlng, rlatd, rlngd, lfend, ifail, &
             ifd, ilf, igf, cproc, cfail, pdlast)
!
! Reads latitudes and longitudes, checking for consistency of fields. Identifies available cases
!
! On exit (if cproc = 'get_gridded_info_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field
!    ifail =  2 Premature end of file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field
!    ifail =  3 Unable to read latitudes and/or longitudes (as indicated by cfail) for ifd field, ilf lagged-field
!    ifail =  4 Dates are not in sequence. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field
!
! Modules
  Use data_time_constants, Only: isq_sn, isq_yr
  Use time,                Only: Assignment(=), Operator(+), Operator(==)
  Use space,               Only: check_latitude_ordering
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input unit number -
!
  Logical, Intent(In) :: lss   ! - permit seasonally-sequenced files? -
  Logical, Intent(In) :: lfend ! - end of file? -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
  Integer, Intent(Out) :: ifd   ! - field at error -
  Integer, Intent(Out) :: ilf   ! - lagged-field at error -
  Integer, Intent(Out) :: igf   ! - category field at error -
!
  Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
  Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
  Type(pprd), Intent(Out) :: pdlast ! - last date read successfully -
!
! Input/output arrays
  Type(field), Dimension(:), Pointer :: afield ! - field information -
!
  Logical, Dimension(:,:), Pointer :: ka ! - cases available? -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:), Pointer :: rlat ! - latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlng ! - longitudes -
!
! Workspace arrays
  Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - test latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - test longitudes -
!
! Locals
!
! Local scalars
  Integer :: k      ! - case index -
  Integer :: l      ! - field/lagged-field index -
  Integer :: ifd_bk ! - backup field index -
  Integer :: ilf_bk ! - backup lagged-field index -
!
  Logical :: lread ! - read current case? -
!
  Type(pprd) :: pdlastt ! - last period read successfully -
!
  Type(field) :: nfield ! - new field information -
!
! Local arrays
  Type(pprd), Dimension(afile%nfl) :: prdps ! - last successfully read periods -
!
  Type(field), Dimension(:), Pointer :: tfield ! - temporary field information -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Ceiling, Real
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_gridded_info_v10'
  ifd = 0
  ilf = 0
  igf = 0
  l = 0
  cfail = ' '
  pdlast = 0
  pdlastt = 0
!
! Set initial dates
  prdps(:) = afield(:)%tprd
!
! Initialise temporary field
  Nullify (tfield)
  Call init_field (tfield, 1, afield(:)%rmiss, ifail)
!
! Rewind
  Rewind (Unit=iin)
  Do k = 1, afile%ntag
     Read (Unit=iin, Fmt=*, Err=2, End=3)
  End Do
!
! Read latitudes and longitudes for stacked fields
  If (afile%lstack) Then
     Do ifd = 1, afile%nfs
        afile%prdn = afile%prd1
        lread = .true.
        Do k = 1, afile%nt
           Do ilf = 1, afile%nls
              l = (ifd - 1)*afile%nls + ilf
              If (lread) Then
                 Do igf = 1, afile%ngs
                    If (igf == 1) Then
                       Call get_gridded_block_v10 (iin, igf, tfield(1), .false., ifail, cproc, cfail, &
                            bfield=afield(l), rlat=rlatd(:,l), rlng=rlngd(:,l))
                    Else
                       Call get_gridded_block_v10 (iin, igf, tfield(1), .false., ifail, cproc, cfail, &
                            bfield=nfield, rlat=rlatd(:,l), rlng=rlngd(:,l))
                    End If
                    Select Case (ifail)
                     Case (0)
                       Continue
                     Case (2)
                       If ((lss) .and. (k >= Ceiling(Real(afile%nt)/Real(afile%nls))) .and. (igf == 1)) Then
                          ifail = 0
                          GoTo 1
                       Else If ((afile%iseq > 0) .and. (igf == 1)) Then ! - adjust number of cases if data are subseasonal and are for part of the year only -
                          afile%nt = k 
                          ifail = 0
                          GoTo 1
                       Else
                          GoTo 4
                       End If
                     Case Default
                       GoTo 4
                    End Select
!
! Check latitude ordering
                    If ((k == 1) .and. (igf == 1)) Then
                       Call check_latitude_ordering (afield(l)%nlt, rlatd(:,l), afield(l)%ln2s, ifail)
                       rlat(1:afield(l)%nlt,l) = rlatd(1:afield(l)%nlt,l)
                       rlng(1:afield(l)%nlg,l) = rlngd(1:afield(l)%nlg,l)
!
! Check for consistency of fields
                    Else
                       Call check_gridded_block_v10 (tfield(1), afield(l), rlatd(1:afield(l)%nlt,l), rlat(1:afield(l)%nlt,l), &
                            rlngd(1:afield(l)%nlg,l), rlng(1:afield(l)%nlg,l), cfail, cproc, ifail)
                       If (ifail /= 0) GoTo 4
                    End If
                    If ((igf == 1) .and. (afile%ngs > 1)) nfield = tfield(1)
                    pdlast = tfield(1)%tprd
                 End Do
              End If
              igf = 1
              pdlast = pdlastt
              cproc = 'get_gridded_info_v10'
!
! Check for missing dates
              If (k > 1) Then
                 If (lss .or. afile%iseq > 0) Then
                    If (afile%iseq /= isq_sn) Then
                       Call check_missing_dates (prdps(l), tfield(1)%tprd, afile%iseq, afile%nls, afile%prdn, &
                            ka(k,l), lread, ifail)
                    Else
                       Call check_missing_dates (prdps(l), tfield(1)%tprd, isq_yr, 1, afile%prdn, &
                            ka((k-1)*afile%nls+l,1), lread, ifail)
                    End If
                 Else
                    Call check_missing_dates (prdps(l), tfield(1)%tprd, afile%iseq, 1, afile%prdn, &
                         ka(k,l), lread, ifail)
                 End If
                 If (ifail /= 0) Then
                    cproc = 'check_missing_dates'
                    GoTo 4
                 End If
                 If (.not.lread) Then
                    ifd_bk = ifd
                    ilf_bk = ilf
                 End If
              End If
              pdlastt = tfield(1)%tprd
              pdlast = pdlastt
           End Do
        End Do
     End Do
!
! Read latitudes and longitudes for unstacked fields
  Else
     afile%prdn = afile%prd1
     lread = .true.
     Do k = 1, afile%nt
        Do ilf = 1, afile%nls
           Do ifd = 1, afile%nfs
              l = (ifd - 1)*afile%nls + ilf
              If (lread) Then
                 Do igf = 1, afile%ngs
                   If (l == 1) Then
                       Call get_gridded_block_v10 (iin, igf, tfield(1), .false., ifail, cproc, cfail, &
                            bfield=afield(l), rlat=rlatd(:,l), rlng=rlngd(:,l))
                    Else
                       If (.not.(afield(l)%tprd == afield(l-1)%tprd)) Then
                          Call get_gridded_block_v10 (iin, igf, tfield(1), .false., ifail, cproc, cfail, &
                               bfield=afield(l), rlat=rlatd(:,l), rlng=rlngd(:,l))
                       Else
                          nfield = afield(l)
                          nfield%tprd = tfield(1)%tprd
                          Call get_gridded_block_v10 (iin, igf, tfield(1), .false., ifail, cproc, cfail, &
                               bfield=nfield, rlat=rlatd(:,l), rlng=rlngd(:,l))
                       End If
                    End If
                    Select Case (ifail)
                     Case (0)
                       Continue
                     Case (2)
                       If ((lss) .and. (k >= Ceiling(Real(afile%nt)/Real(afile%nls))) .and. (igf == 1)) Then
                          ifail = 0
                          GoTo 1
                       Else If ((afile%iseq > 0) .and. (igf == 1)) Then ! - adjust number of cases if data are subseasonal and are for part of the year only -
                          afile%nt = k 
                          ifail = 0
                          GoTo 1
                       Else
                          GoTo 4
                       End If
                     Case Default
                       GoTo 4
                    End Select
!
! Check latitude ordering
                    If ((k == 1) .and. (igf == 1)) Then
                       Call check_latitude_ordering (afield(l)%nlt, rlatd(:,l), afield(l)%ln2s, ifail)
                       rlat(1:afield(l)%nlt,l) = rlatd(1:afield(l)%nlt,l)
                       rlng(1:afield(l)%nlg,l) = rlngd(1:afield(l)%nlg,l)
!
! Check for consistency of fields
                    Else
                       Call check_gridded_block_v10 (tfield(1), afield(l), rlatd(1:afield(l)%nlt,l), rlat(1:afield(l)%nlt,l), &
                            rlngd(1:afield(l)%nlg,l), rlng(1:afield(l)%nlg,l), cfail, cproc, ifail)
                       If (ifail /= 0) GoTo 4
                    End If
                    pdlast = tfield(1)%tprd
                 End Do
              End If
              igf = 1
              pdlast = pdlastt
              cproc = 'get_gridded_info_v10'
!
! Check for missing dates
              If (k > 1) Then
                 If (lss) Then
                    If (afile%iseq /= isq_sn) Then
                       Call check_missing_dates (prdps(l), tfield(1)%tprd, afile%iseq, afile%nls, afile%prdn, &
                            ka(k,l), lread, ifail)
                    Else
                       Call check_missing_dates (prdps(l), tfield(1)%tprd, isq_yr, 1, afile%prdn, &
                            ka((k-1)*afile%nls+l,1), lread, ifail)
                    End If
                 Else
                    Call check_missing_dates (prdps(l), tfield(1)%tprd, afile%iseq, 1, afile%prdn, &
                         ka(k,l), lread, ifail)
                 End If
                 If (ifail /= 0) Then
                    cproc = 'check_missing_dates'
                    GoTo 4
                 End If
                 If (.not.lread) Then
                    ifd_bk = ifd
                    ilf_bk = ilf
                 End If
              End If
              pdlastt = tfield(1)%tprd
              pdlast = pdlastt
           End Do
        End Do
     End Do
  End If
!
! Invert latitudes of south to north fields
1 Do l = 1, afile%nfl
     If (.not.afield(l)%ln2s) Then
        rlatd(afield(l)%nlt:1:-1,l) = rlat(1:afield(l)%nlt,l)
        rlat(1:afield(l)%nlt,l) = rlatd(1:afield(l)%nlt,l)
     End If
  End Do
  If (lread) Then
     ifail = 0
  Else
     ifail = 4
     GoTo 4
  End If
  Return
!
! Errors
! - problem reading file -
2 ifail = 1
  GoTo 4
!
! - problem reading file -
3 If ((lfend) .and. (l == afile%nfl)) Then
     ifail = 0
     Return
  Else
     ifail = 2
  End If
!
! Identify last date read
4 If (.not.lread) Then
     ifd = ifd_bk
     ilf = ilf_bk
  End If
  igf = igf - 1
  If (igf == 0) Then
     igf = afile%ngs
     If (afile%lstack) Then
        ilf = ilf - 1
        If (ilf == 0) Then
           ilf = afile%nls
           If (k == 1) Then
              ifd = ifd - 1
              If (ifd == 0) Then
                 ilf = 0
                 igf = 0
             End If
           End If
        End If
     Else
        ifd = ifd - 1
        If (ifd == 0) Then
           ifd = afile%nfs
           ilf = ilf - 1
           If (ilf == 0) Then
              If (k > 1) Then
                 ilf = afile%nls
              Else
                 ifd = 0
                 ilf = 0
                 igf = 0
              End If
           End If
        End If
     End If
  End If
!
  Return
  End Subroutine get_gridded_info_v10
!
!
!
  Subroutine get_gridded_block_v10 (iin, igf, afield, lctagt, ifail, cproc, cfail, &
             bfield, rlat, rlng)
!
! Reads field information for a block of data
!
! On exit (if cproc = 'get_gridded_block_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!    ifail =  3 Unable to identify tag line
!    ifail =  4 Field structure is not gridded
!    ifail =  5 Category numbering is incorrect
!
! Modules
  Use data_io_constants, Only: ltag
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input unit number -
  Integer, Intent(In) :: igf ! - current gridded field -
!
  Logical, Intent(In) :: lctagt ! - cpt:T tag required? -
!
! - optional input scalars -
  Type(field), Intent(In), Optional :: bfield ! - second field -
!
! Input/output scalars
  Type(field), Intent(InOut) :: afield ! - field information -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
  Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
  Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
! Output arrays
! - optional output arrays -
  Real(Kind=rp), Dimension(:), Intent(Out), Optional :: rlat ! - latitudes -
  Real(Kind=rp), Dimension(:), Intent(Out), Optional :: rlng ! - longitudes -
!
! Locals
!
! Local scalars
  Integer :: i    ! - latitude index -
  Integer :: j    ! - longitude index -
  Integer :: igrd ! - structure -
!
  Character(Len=ltag) :: ctag ! - tag line -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Present, Trim
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_gridded_block_v10'
  cfail = ' '
!
! Read tag line
  Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag
  If (Len_Trim(ctag) == 0) GoTo 2
  If (Index(ctag, 'cpt:') == 0) Then
     ifail = 3
     Return
  End If
!
! Parse tag line
  Call read_tags (Trim(ctag)//' ',15,                                                                     &
       (/'T        ', 'S        ', 'F        ', 'Z        ', 'C        ', 'clim_prob', 'M        ', 'model    ', &
         'col      ', 'row      ', 'ncol     ', 'nrow     ', 'field    ', 'units    ', 'missing  '/),           &
       lctagt, afield, ifail, cfail,                                                                         &
       bfield=bfield, icat=igf, idstr=igrd)
  If (ifail /= 0) Then
     cproc = 'read_tags'
     Return
  End If
!
! Return if field is not gridded
  If (.not.Present(bfield)) Then
     If (igrd /= 1) Then
        ifail = 4
        Return
     End If
!
! Return if category is incorrect
     If ((afield%icat(igf) /= igf) .and. (afield%icat(igf) > 0)) Then
        ifail = 5
        Return
     End If
  End If
!
! Read longitudes
  If (Present(rlng)) Then
     Read (Unit=iin, Fmt=*, Err=1, End=2) (rlng(j), j=1,afield%nlg)
  Else
     Read (Unit=iin, Fmt=*, Err=1, End=2)
  End If
!
! Read latitudes
  If (Present(rlat)) Then
     Do i = 1, afield%nlt
        Read (Unit=iin, Fmt=*, Err=1, End=2) rlat(i)
     End Do
  Else
     Do i = 1, afield%nlt
        Read (Unit=iin, Fmt=*, Err=1, End=2)
     End Do
  End If
  Return
!
! Errors
! - problem reading file -
1 ifail = 1
  Return
!
! - end of file -
2 ifail = 2
  Return
!
  End Subroutine get_gridded_block_v10
!
!
!
  Subroutine check_gridded_block_v10 (tfield, rfield, tlat, rlat, tlng, rlng, cfail, cproc, ifail)
!
! Checks for consistency of field information
!
! On exit (if cproc = 'check_gridded_block_v10'):
!    ifail =  0 Successful
!    ifail =  1 Inconsistent tag as indicated by cfail
!    ifail =  2 Inconsistent latitudes or longitudes as indicated by cfail
!
! Arguments
!
! Input scalars
  Type(field), Intent(In) :: tfield ! - test field -
  Type(field), Intent(In) :: rfield ! - reference field -
!
! Input arrays
  Real(Kind=rp), Dimension(:), Intent(In) :: tlat ! - test latitudes -
  Real(Kind=rp), Dimension(:), Intent(In) :: rlat ! - reference longitudes -
  Real(Kind=rp), Dimension(:), Intent(In) :: tlng ! - test latitudes -
  Real(Kind=rp), Dimension(:), Intent(In) :: rlng ! - reference longitudes -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
  Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
  Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Any
!
! Executable Statements
!
! Initialise routine
  cproc = 'check_gridded_block_v10'
  cfail = ' '
!
! Check for consistent field settings
! - number of latitudes -
  If (tfield%nlt /= rfield%nlt) Then
     cfail = 'ncol'
     ifail = 1
     Return
  End If
! - number of longitudes -
  If (tfield%nlg /= rfield%nlg) Then
     cfail = 'nrow'
     ifail = 1
     Return
  End If
! - ensemble member -
  If (tfield%member /= rfield%member) Then
     cfail = 'member'
     ifail = 1
     Return
  End If
! - missing values -
  If (tfield%rmiss /= rfield%rmiss) Then
     cfail = 'missing'
     ifail = 1
     Return
  End If
! - variable -
  If (tfield%var /= rfield%var) Then
     cfail = 'var'
     ifail = 1
     Return
  End If
! - units -
  If (tfield%unit /= rfield%unit) Then
     cfail = 'units'
     ifail = 1
     Return
  End If
! - geopotential height -
  If (tfield%z%hght /= rfield%z%hght) Then
     cfail = 'z'
     ifail = 1
     Return
  End If
!
! Compare latitudes
  If (Any(tlat(:) /= rlat(:))) Then
     cfail = 'latitudes'
     ifail = 2
     Return
  End If
!
! Compare longitudes
  If (Any(tlng(:) /= rlng(:))) Then
     cfail = 'longitudes'
     ifail = 2
     Return
  End If
  ifail = 0
!
  Return
  End Subroutine check_gridded_block_v10
 End Subroutine get_gridded_v10
!
!
!
 Subroutine get_nongridded_v10 (iin, nt, afile, lss, afield, rmiss, rlat,  rlng, cstn, rlatd, rlngd, cstnd, idom, ka, ifail, &
            ifd, ilf, igf, cproc, cfail, pdlast)
!
! Determines structure of a version-10 station or unreferenced input file, including its size, field settings, and missing cases
!
! On exit (if cproc = 'get_nongridded_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem allocating memory
!
! Modules
  Use data_io_constants, Only: lstn
  Use time,              Only: pprd, &
                               iseq, nss
  Use iofiles,           Only: ifile
  Use space,             Only: reset_grids
  Use fields,            Only: field, &
                               init_field
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input unit number -
!
  Real(Kind=rp), Intent(In) :: rmiss ! - missing value flag -
!
  Logical, Intent(In) :: lss ! - permit seasonally-sequenced files? -
!
! Input/output scalars
  Integer, Intent(InOut) :: nt ! - number of time steps -
!
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
  Integer, Intent(Out) :: ifd   ! - field at error -
  Integer, Intent(Out) :: ilf   ! - lagged-field at error -
  Integer, Intent(Out) :: igf   ! - category field at error -
!
  Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
  Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
  Type(pprd), Intent(Out) :: pdlast ! - last period read successfully -
!
! Pointer arrays
  Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
  Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - used latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - realigned/used longitudes -
!
  Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - names of stations/indices -
  Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - names of stations within domains -
!
  Logical, Dimension(:,:), Pointer :: ka ! - cases available? -
!
  Type(field), Dimension(:), Pointer :: afield ! - field information -
!
! Locals
!
! Local scalars
  Integer :: ntag   ! - number of tag lines -
  Integer :: iseqbk ! - backup time sequence indicator -
!
  Logical :: lfend ! - end of file? -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Associated, Max
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_nongridded_v10'
  cfail = ' '
  ifd = 0
  ilf = 0
  igf = 0
!
! Check for lagged-fields
  Call get_nongridded_nls_v10 (iin, lss, nt, afile, ntag, ifail, cproc, pdlast)
  If (ifail /= 0) Return
!
! Initialise fields
  Call init_field (afield, afile%nfl, (/rmiss/), ifail)
  If (ifail /= 0) Then
     ifail = 1
     Return
  End If
!
! Get fields and maximum number of cases
  Call get_nongridded_fields_v10 (iin, lss, ntag, afile, afield, rmiss, lfend, ifail, ifd, ilf, igf, cproc, cfail, pdlast)
  iseqbk = iseq
  iseq = afile%iseq
  If (ifail /= 0) GoTo 1
!
! Initialise latitudes and longitudes
  Call reset_grids (rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ifail, &
       idstr=afile%idstr, ndom=afile%nfl, nlt=afield(:)%nlt, nlg=afield(:)%nlg)
  If (ifail /= 0) Then
     ifail = 1
     GoTo 1
  End If
!
! Assign memory for available cases flags
  If (Associated(ka)) Then
     Deallocate (ka)
     Nullify (ka)
  End If
  If (lss) Then
     Allocate (ka(afile%nt,afile%nfl/nss), Stat=ifail)
  Else
     Allocate (ka(afile%nt,afile%nfl), Stat=ifail)
  End If
  If (ifail /= 0) Then
     ifail = 1
     GoTo 1
  End If
  ka(1:Max(nss,1),:) = .true.
  If (afile%nt > nss) ka(nss+1:,:) = .false.
!
! Get station/index names and coordinates, and identify available cases
  Call get_nongridded_info_v10 (iin, ntag, lss, afile, afield, ka, rlat, rlng, cstn, rlatd, lfend, ifail, &
       ifd, ilf, igf, cproc, cfail, pdlast)
!
! Free workspace
  If (ifail /= 0) Then
     If (Associated(ka)) Then
        Deallocate (ka)
        Nullify (ka)
     End If
  End If
!
! Restore sequencing
1 iseq = iseqbk
  afile%ntag = ntag
  ilf = 0
  Return
!
 Contains
!
!
  Subroutine get_nongridded_nls_v10 (iin, lss, nt, afile, ntag, ifail, cproc, pdlast)
!
! Determines number of lagged-fields in a version-10 station or unreferenced input file
!
! On exit (if cproc = 'get_nongridded_nls_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!    ifail =  4 File is too wide
!
! Modules
  Use data_io_constants,   Only: lfli
  Use data_time_constants, Only: isq_mn, isq_sn, isq_so, nmn
  Use time,                Only: Assignment(=), &
                                 ims, lms, nss, &
                                 date_diff, get_pdate
!  Use iofiles,             Only: max_width
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input unit number -
!
  Logical, Intent(In) :: lss ! - permit seasonally-sequenced files? -
!
! Input/output scalars
  Integer, Intent(InOut) :: nt ! - number of time steps -
!
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ntag  ! - number of tag lines -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
  Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
!
  Type(pprd), Intent(Out) :: pdlast ! - last period read successfully -
!
! Locals
!
! Local scalars
  Integer :: k     ! - case index -
  Integer :: emax  ! - maximum length of line -
  Integer :: llen  ! - length of line -
  Integer :: ilf   ! - lagged field index -
  Integer :: isq1  ! - first time sequencing -
!
  Character(Len=lfli) :: ctag ! - tag line -
!
  Type(pprd) :: prdc ! - current date -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic AdjustL, Index, Trim
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_nongridded_nls_v10'
  pdlast = 0
!
! Skip station names and tags
  Read (Unit=iin, Fmt=*, Err=1, End=2)
  ntag = 1
  Do
     Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag
     If (Index(ctag, 'cpt:') == 0) Exit
     ntag = ntag + 1
  End Do
!
! Read first date
  ctag = AdjustL(ctag)
  Call get_pdate (' ',Trim(ctag),afile%prd1%sdate, ifail, &
       edate=afile%prd1%edate)
  If (ifail /= 0) Then
     cproc = 'get_pdate'
     Return
  End If
  pdlast = afile%prd1
  ilf = 1
  If (lss) Then
     ims(ilf) = afile%prd1%sdate%imn
     lms(ilf) = date_diff(afile%prd1%sdate, afile%prd1%edate, isq_mn)
  End If
!
! Check for lagged-fields
! - read subsequent dates -
  afile%nls = 1
  If (nt > 1) Then
     get_nls: Do k = 1, nt-1
        Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag
        ctag=AdjustL(ctag)
        Call get_pdate (' ', Trim(ctag), prdc%sdate, ifail, &
             edate=prdc%edate)
        If (ifail /= 0) Then
           cproc = 'get_pdate'
           Return
        End If
! - identify date sequencing -
        Call determine_sequence (afile%prd1, prdc, lss, isq1, afile%iseq, afile%nls, pdlast, ifail)
        If (k == 1) isq1 = afile%iseq
        Select Case (ifail)
         Case (0)
           Exit get_nls
         Case (1)
           ilf = ilf + 1
           If (lss) Then
              ims(ilf) = prdc%sdate%imn
              lms(ilf) = date_diff(prdc%sdate, prdc%edate, isq_mn)
           End If
         Case Default
           cproc = 'get_sequence'
           Return
        End Select
     End Do get_nls
  End If
  If (isq1 == isq_mn .and. afile%nls == nmn) Then
     afile%iseq = isq_mn
     afile%nls = 1
  Else If (isq1 == isq_so .and. afile%nls <= nmn) Then
     afile%iseq = isq_so
     afile%nls = 1
  Else If (isq1 == isq_sn .and. afile%nls <= nmn) Then
     afile%iseq = isq_sn
  End If
  If (lss) nss = afile%nls
  afile%nfl = afile%nfs*afile%nls
!
! Identify longest line
  Rewind (Unit=iin)
!  Call max_width (iin,afile%lmax, ifail) ! - bug in FTN95 -
  afile%lmax = 0
  emax = 100
  Do
     Read (Unit=iin, Fmt='(A)', Err=3, End=3) ctag(1:emax)
     llen = Len_Trim(ctag(1:emax))
     If (llen <= afile%lmax) Then ! - current line is shorter - 
        Cycle
     Else ! - extend if line is too short -
        afile%lmax = llen
        If (afile%lmax >= lfli-100) Then ! - return if line is too long -
           ifail = 4
           Return
        End If
        emax = Min(afile%lmax+100, lfli)
        If (emax < 2**13) Then
           Backspace (Unit=iin)
        Else
           Rewind (Unit=iin)
        End If
     End If
  End Do
!
3 ifail = 0
  Return
!
! Errors
! - problem reading file -
1 ifail = 1
  Return
! - end of file -
2 ifail = 2
!
  Return
  End Subroutine get_nongridded_nls_v10
!
!
!
  Subroutine get_nongridded_fields_v10 (iin, lss, ntag, afile, afield, rmiss, lfend, ifail, ifd, ilf, igf, cproc, cfail, pdlast)
!
! Determines field settings and maximum number of cases
!
! On exit (if cproc = 'get_nongridded_fields_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field, igf category
!    ifail =  2 Premature end of file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field, igf category
!    ifail =  3 Season for last date of ifd field, ilf lagged-field, igf category is inconsistent with earlier dates
!    ifail =  4 Last date for ifd field, ilf lagged-field, igf category is not in sequence
!    ifail =  5 Categories are not in order
!    ifail =  6 Field information for different categories is inconsistent
!    ifail =  7 Starting date for different categories is inconsistent
!
! Modules
  Use data_io_constants,   Only: ltag
  Use data_time_constants, Only: isq_sn, isq_yr
  Use time,                Only: Assignment(=), Operator(==), &
                                 date_diff, get_pdate
  Use fields,              Only: Operator(/=), &
                                 set_field
  Use season,              Only: get_cssn
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin  ! - input unit number -
  Integer, Intent(In) :: ntag ! - number of tag lines -
!
  Real(Kind=rp), Intent(In) :: rmiss ! - missing value flag -
!
  Logical, Intent(In) :: lss ! - permit seasonally-sequenced files? -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
  Integer, Intent(Out) :: ifd   ! - field at error -
  Integer, Intent(Out) :: ilf   ! - lagged-field at error -
  Integer, Intent(Out) :: igf   ! - category field at error -
!
  Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
  Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
  Logical, Intent(Out) :: lfend ! - end of file? -
!
  Type(pprd), Intent(Out) :: pdlast ! - last period read successfully -
!
! Input/output arrays
  Type(field), Dimension(:), Intent(InOut) :: afield ! - field information -
!
! Locals
!
! Local scalars
  Integer :: k   ! - case index -
  Integer :: l   ! - field/lagged-field index -
  Integer :: ntn ! - number of time steps -
  Integer :: ntt ! - number of time steps -
!
  Character(Len=ltag) :: ctag ! - tag line -
!
  Type(pprd) :: prdc ! - current date -
!
  Type(field) :: tfield ! - field information -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic AdjustL, Max, Trim
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_nongridded_fields_v10'
  ifd = 0
  ilf = 0
  igf = 0
  cfail = ' '
  pdlast = 0
  Call set_field (tfield, rmiss)
!
! Rewind to first field
  Rewind (Unit=iin)
  Do k = 1, afile%ntag
     Read (Unit=iin, Fmt=*, Err=1, End=2)
  End Do
!
! Read fields
  afile%nt = 1
  Do ifd = 1, afile%nfs
     l = (ifd - 1)*afile%nls + 1
     Do igf = 1, afile%ngs
        Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag
        ctag = AdjustL(ctag)
        If (igf == 1) Then
           If (l == 1) Then
              Call read_tags (Trim(ctag)//' ', 9,                                     &
                   (/'C        ', 'clim_prob', 'col      ', 'row      ', 'ncol     ', &
                     'nrow     ', 'field    ', 'units    ', 'missing  '/),            &
                   .false., tfield, ifail, cfail,                                     &
                   icat=igf, nt=ntn)
           Else
              Call read_tags (Trim(ctag)//' ', 9,                                     &
                   (/'C        ', 'clim_prob', 'col      ', 'row      ', 'ncol     ', &
                     'nrow     ', 'field    ', 'units    ', 'missing  '/),            &
                   .false.,tfield, ifail,cfail,                                       &
                   bfield=afield(l-1),icat=igf,nt=ntn)
           End If
        Else
           Call read_tags (Trim(ctag)//' ', 9,                                     &
                (/'C        ', 'clim_prob', 'col      ', 'row      ', 'ncol     ', &
                  'nrow     ', 'field    ', 'units    ', 'missing  '/),            &
                .false., tfield, ifail, cfail,                                     &
                bfield=afield(l), icat=igf,nt=ntn)
        End If
        If (ifail /= 0) Then
           cproc = 'read_tags'
           Return
        End If
        If ((tfield%icat(igf) /= igf) .and. (tfield%icat(igf) > 0)) Then ! - check for correct ordering of categories -
           ifail = 5
           GoTo 3
        End If
        If (igf == 1) Then
           afield(l) = tfield
           afield(l)%mdate = 0
        Else
           If (tfield /= afield(l)) Then ! - check for consistency of fields -
              ifail = 6
              GoTo 3
           End If
           afield(l)%icat(igf) = igf
           afield(l)%prob(igf) = tfield%prob(igf)
        End If
!
! Skip additional tag lines
        Do k = 1, ntag
           Read (Unit=iin, Fmt=*, Err=1, End=2)
        End Do
!
! Set lagged-fields
        Do ilf = 1, afile%nls
           l = (ifd - 1)*afile%nls + ilf
           If ((ilf > 1) .and. (igf == 1)) afield(l) = afield(l-1)
           Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag
           ctag = AdjustL(ctag)
           Call get_pdate (' ', Trim(ctag), prdc%sdate, ifail, &
                edate=prdc%edate)
           If (ifail /= 0) Then
              cproc = 'get_pdate'
              Return
           End If
           If (igf == 1) Then
              afield(l)%tprd = prdc
              afield(l)%cssn = get_cssn(prdc, afile%iseq, 1)
           Else
              If (.not.(prdc == afield(l)%tprd)) Then
                 ifail = 7
                 GoTo 3
              End If
           End If
           pdlast = prdc
        End Do
!
! Read last case
        If (ntn>afile%nls) Then
           If (ntn-afile%nls-1 > 0) Then
              Do k = afile%nls+1, ntn-1
                 Read (Unit=iin, Fmt=*, Err=1, End=2)
              End Do
           End If
           l = ifd*afile%nls
           Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag
           ctag = AdjustL(ctag)
           Call get_pdate (' ',Trim(ctag), prdc%sdate, ifail, &
                edate=prdc%edate)
           If (ifail /= 0) Then
              cproc = 'get_pdate'
              Return
           End If
           pdlast = prdc
!
! Check whether last case is for last lagged-field
           lfend = .false.
           If (afile%iseq == isq_yr) Then
              Do ilf = afile%nls, 1, -1
                 If ((prdc%sdate%imn == afield(l)%tprd%sdate%imn) .and. (prdc%sdate%idy == afield(l)%tprd%sdate%idy) .and.  &
                     (prdc%edate%imn == afield(l)%tprd%edate%imn) .and. (prdc%edate%idy == afield(l)%tprd%edate%idy)) Then
                    Exit
                 Else If (ilf > 1) Then
                    lfend = .true.
                    l = l - 1
                 Else
                    ifail = 3
                    Return
                 End If
              End Do
           End If
!
! Calculate number of cases
           If ((ifd == 1) .and. (igf == 1)) Then
              If (afile%iseq /= 0) Then
                 ntt = date_diff(afield(l)%tprd%sdate, prdc%sdate, afile%iseq) + 1
                 If (afile%iseq == isq_sn) ntt = ntt + l - 1
                 afile%nt = Max(afile%nt, ntt)
                 If (ntt < 1) Then
                    ifail = 4
                    Return
                 End If
                 afile%prdn = prdc
              End If
           End If
        Else
           afile%prdn = prdc
        End If
     End Do
  End Do
  If (afile%nt == 1 .and. afile%iseq<0 .and. .not.lss) afile%iseq = isq_yr
  afile%nms = 1
!
! Set number of cases
  ifail = 0
  Return
!
! Errors
! - problem reading file -
1 ifail = 1
  GoTo 3
!
! - end of file -
2 Backspace (Unit=iin)
  Backspace (Unit=iin)
  Read (Unit=iin, Fmt='(A)', Err=3) ctag
  ctag = AdjustL(ctag)
  Call get_pdate (' ',Trim(ctag), pdlast%sdate, ifail, &
       edate=pdlast%edate)
  If (ifail == 0) ifail = 2
!
! Category errors
3 igf = igf - 1
  If (afile%ngs > 1) Then
     If (igf == 0) Then
        If (ifd > 1) Then
           ifd = ifd - 1
           igf = afile%ngs
        Else
           pdlast = 0
        End If
     End If
  Else
     ifd = ifd - 1
  End If
!
  Return
  End Subroutine get_nongridded_fields_v10
!
!
!
  Subroutine get_nongridded_info_v10 (iin, ntag, lss, afile, afield, ka, rlat, rlng, cstn, rwk, lfend, ifail, &
             ifd, ilf, igf, cproc, cfail, pdlast)
!
! Determines variable names and coordinates of station of stations. Identifies available cases.
!
! On exit (if cproc = 'get_nongridded_info_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field, igf category
!    ifail =  2 Premature end of file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field, igf category
!    ifail =  3 Unable to read latitudes and/or longitudes (as indicated by cfail) for ifd field, ilf lagged-field, igf category
!    ifail =  4 Dates for ifd field, ilf lagged-field, igf category are not in sequence. Data up to pdlast read successfully
!    ifail =  5 Inconsistent omitted values for ifd field, ilf lagged-field, igf category
!
! Modules
  Use data_io_constants,   Only: ltag
  Use data_time_constants, Only: isq_sn, isq_yr
  Use time,                Only: Assignment(=), Operator(+), &
                                 check_pprd, get_pdate, &
                                 nss
  Use season,              Only: get_cssn
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin  ! - input unit number -
  Integer, Intent(In) :: ntag ! - number of tag lines -
!
  Logical, Intent(In) :: lss   ! - permit seasonally-sequenced files? -
  Logical, Intent(In) :: lfend ! - end of file? -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
  Integer, Intent(Out) :: ifd   ! - field at error -
  Integer, Intent(Out) :: ilf   ! - lagged-field at error -
  Integer, Intent(Out) :: igf   ! - category field at error -
!
  Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
  Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
  Type(pprd), Intent(Out) :: pdlast ! - last period read successfully -
!
! Input arrays
  Type(field), Dimension(:), Intent(In) :: afield ! - field information -
!
! Input/output arrays
  Logical, Dimension(:,:), Intent(InOut) :: ka ! - cases available? -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:), Pointer :: rlat ! - latitudes -
  Real(Kind=rp), Dimension(:,:), Pointer :: rlng ! - longitudes -
!
  Character(Len=lstn), Dimension(:,:), Intent(Out) :: cstn ! - names of stations/indices -
!
! Workspace arrays
  Real(Kind=rp), Dimension(:,:), Pointer :: rwk ! - workspace -
!
! Locals
!
! Local scalars
  Integer :: i     ! - station index -
  Integer :: k     ! - case index -
  Integer :: l     ! - field/lagged-field index -
  Integer :: nl    ! - number of lags -
  Integer :: nskip ! - number of lines to skip -
!
  Character(Len=ltag) :: ctag ! - tag line -
!
  Logical :: llx   ! - longitudes read? -
  Logical :: lly   ! - longitudes read? -
  Logical :: lread ! - read current case? -
!
  Type(pprd) :: prdc ! - current date -
!
! Local arrays
  Type(pprd), Dimension(afile%nfl) :: prdps ! - last successfully read periods -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic AdjustL, Ceiling, Index, Mod, Real, Trim
!
! Executable Statements
!
! Initialise routine
  cproc = 'get_nongridded_info_v10'
  ifd = 0
  ilf = 0
  igf = 0
  cfail = ' '
  pdlast = 0
  nl = afile%nls
  If (lss) nl = nl/nss
!
! Rewind
  Rewind (Unit=iin)
  Do k = 1, afile%ntag
     Read (Unit=iin, Fmt=*, Err=1, End=2)
  End Do
!
! Set initial dates
  lread = .true.
  read_fields: Do ifd = 1, afile%nfs
     Do igf = 1, afile%ngs
        l = (ifd - 1)*afile%nls + 1
        prdps(:) = afield(:)%tprd
!
! Read station names
        If (lread) Read (Unit=iin, Fmt=*, Err=1, End=2)
        Read (Unit=iin, Fmt=*, Err=1, End=2) (cstn(i,l), i=1,afield(l)%nlt)
!
! Read latitudes and longitudes
        nskip = 1
        If (afile%igeog == 1) Then
           lly = .false.
           llx = .false.
           Do
              Read (Unit=iin, Fmt=*, Err=1, End=2) ctag, (rwk(i,l), i=1,afield(l)%nlt)
              If (Index(ctag, 'cpt:Y') > 0) Then
                 lly = .true.
                 nskip = nskip + 1
                 If (igf == 1) Then
                    rlat(1:afield(l)%nlt,l) = rwk(1:afield(l)%nlt,l)
                 Else If (Any(rwk(1:afield(l)%nlt,l) /= rlat(1:afield(l)%nlt,l))) Then ! - check for consistency -
                    cfail = 'latitudes'
                    GoTo 3
                 End If
                 If (llx) Exit
              Else If (Index(ctag, 'cpt:X') > 0) Then
                 llx = .true.
                 nskip = nskip + 1
                 If (igf == 1) Then
                    rlng(1:afield(l)%nlt,l) = rwk(1:afield(l)%nlt,l)
                 Else If (Any(rwk(1:afield(l)%nlt,l) /= rlng(1:afield(l)%nlt,l))) Then ! - check for consistency -
                    cfail = 'longitudes'
                    GoTo 3
                 End If
                 If (lly) Exit
              Else If (Index(ctag, 'cpt:') > 0) Then
                 nskip = nskip + 1
              Else
                 Exit
              End If
           End Do
!
! Check for missing coordinates
           If (llx .and. lly) Then
              Continue
           Else If (.not.(llx .and. lly)) Then
              cfail = 'latitudes and longitudes'
              ifail = 3
              Return
           Else If (.not.lly) Then
              cfail = 'latitudes'
              ifail = 3
              Return
           Else If (.not.llx) Then
              cfail = 'longitudes'
              ifail = 3
              Return
           End If
!
! Skip tag lines in unreferenced files
        Else
           nskip = nskip + 1
           Do
              Read (Unit=iin, Fmt=*, Err=1, End=2) ctag
              If (Index(ctag, 'cpt:') > 0) Then
                 nskip = nskip + 1
              Else
                 Exit
              End If
           End Do
        End If
!
! Skip past first dates
        If (afile%nt*nl > 1) Then
           Do k = 1, afile%nls+ntag-nskip
              Read (Unit=iin, Fmt=*, Err=1, End=2) ctag
           End Do
           l = ifd*nl
           pdlast = afield(l)%tprd
!
! Check for missing dates
           If (afile%nt > nss) Then
              lread = .true.
              Do k = afile%nls+1, afile%nt
                 Do ilf = 1, nl
                    l = (ifd - 1)*afile%nls + ilf
                    If (lss .and. nss > 1) l = l + Mod(k-1, nss)
                    If (lread) Then
                       Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag
                       ctag = AdjustL(ctag)
! - check for next field in case of missing last values -
                       If (Index(ctag, 'cpt:') > 0) Then
                          lread = .false.
                          Cycle read_fields
                       End If
                       Call get_pdate (' ', Trim(ctag), prdc%sdate, ifail, &
                            edate=prdc%edate)
                       Select Case (ifail)
                        Case (0)
                          Continue
                        Case (2)
                          If ((lss) .and. (k >= Ceiling(Real(afile%nt)/Real(afile%nls))) .and. (igf == 1)) Then
                             ifail = 0
                             Exit read_fields
                          Else
                             cproc = 'get_pdate'
                             Return
                          End If
                        Case Default
                          cproc = 'get_pdate'
                          Return
                       End Select
                    End If
                    ctag = get_cssn(prdc, afile%iseq, 1)
! - check for consistency of periods -
                    ifail = check_pprd(afield(l)%cssn, ctag, afield(l)%tprd, prdc, afile%iseq)
                    If (ifail /= 0) Then
                       cproc = 'check_pprd'
                       Return
                    End If
! - check for advancing dates -
                    If (lss) Then
                       If (afile%iseq /= isq_sn) Then
                          Call check_missing_dates (prdps(l), prdc, afile%iseq, afile%nls, afile%prdn, ka(k,l), lread, ifail)
                       Else
                          Call check_missing_dates (prdps(l), prdc, isq_yr, 1, afile%prdn, ka(k,1), lread, ifail)
                       End If
                    Else
                       Call check_missing_dates (prdps(l), prdc, afile%iseq, 1, afile%prdn, ka(k,l), lread, ifail)
                    End If
                    Select Case (ifail)
                     Case (0)
                       pdlast = prdc
                     Case (1)  
                       cproc = 'check_missing_dates'
                       Return
                     Case (2)
                       cfail = 'omitted values'
                       ifail = 5
                    End Select
                 End Do
              End Do
           End If
        Else If (igf < afile%ngs) Then ! - read single forecasts -
           Read (Unit=iin, Fmt=*, End=2)
        End If
     End Do
!
! Copy field information for lagged variables
     If (afile%nls > 1) Then
        Do ilf = 2, afile%nls
           l = (ifd - 1)*afile%nls + ilf
           If (ilf > 1) Then
              If (afile%igeog == 1) Then
                 rlat(:,l) = rlat(:,l+1-ilf)
                 rlng(:,l) = rlng(:,l+1-ilf)
              End If
              cstn(:,l) = cstn(:,l+1-ilf)
           End If
        End Do
     End If
  End Do read_fields
!
! Errors
  If (lread) Then
     ifail = 0
  Else
     ifail = 4
  End If
  Return
!
! - problem reading file -
1 ifail = 1
  Return
!
! - problem reading file -
2 If ((lfend) .and. (l == afile%nfl)) Then
     ifail = 0
  Else
     ifail = 2
  End If
  Return
!
! Trap inconsistency errors
3 ifail = 5
  If (afile%ngs > 1) Then
     igf = igf - 1
     If (igf == 0) Then
        If (ifd > 1) Then
           ifd = ifd - 1
           igf = afile%ngs
        Else
           pdlast = 0
        End If
     End If
  Else
     ifd = ifd - 1
  End If
!
  Return
  End Subroutine get_nongridded_info_v10
 End Subroutine get_nongridded_v10
!
!
!
 Subroutine check_missing_dates (p1, p2, isq, nss, prdn, ka, lread, ifail, &
                                 igf)
!
! Checks whether dates are missing in a sequence
!
! Modules
  Use time, Only: Operator(+), &
                  pprd, &
                  date_diff
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: isq ! - sequence indicator -
  Integer, Intent(In) :: nss ! - number of seasons -
!
  Type(pprd), Intent(In) :: p2 ! - second period -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: igf ! - current category-field -
!
! Input/output scalars
  Logical, Intent(InOut) :: ka    ! - case available? -
  Logical, Intent(InOut) :: lread ! - read current case? -
!
!
  Type(pprd), Intent(InOut) :: p1   ! - first period -
  Type(pprd), Intent(InOut) :: prdn ! - period of last data -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  Integer :: dd ! - date difference -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Present
!
! Executable Statements
!
! Check whether dates are advancing
  dd=date_diff(p1%sdate,p2%sdate,isq)
  If (dd == nss) Then ! - dates are consecutive -
     ka = .true.
     p1 = p2
     prdn = p2
     lread = .true.
     ifail = 0
  Else If (dd > nss) Then ! - dates have skipped -
     If (Present(igf)) Then
        If ((igf > 1) .and. (ka)) Then ! - check whether dates were available in other categories -
           ifail = 2
           Return
        End If
     End If
     p1 = p1 + 1
     lread = .false.
     ifail = 0
  Else ! - second date is before first -
     ifail = 1
  End If
!
  Return
 End Subroutine check_missing_dates
!
!
!
 Subroutine determine_sequence (prd1, prd2, lss, isq1, isq, nls, pdlast, ifail, &
            ilf)
!
! Determines date sequencing, and counts lagged fields
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Lagged fields may be present; test next sequence
!    ifail =  2 Error
!
! Modules
  Use data_time_constants, Only: isq_mn, isq_sn, isq_so, isq_yr, nmn
  Use time,                Only: pprd, &
                                 Operator(==), &
                                 get_sequence
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: isq1 ! - initial time sequencing -
!
  Logical, Intent(In) :: lss ! - permit seasonally-sequenced files? -
!
  Type(pprd), Intent(In) :: prd1 ! - period 1 -
  Type(pprd), Intent(In) :: prd2 ! - period 2 -
!
! Input/output scalars
  Integer, Intent(InOut) :: isq ! - time sequencing -
  Integer, Intent(InOut) :: nls ! - number of lagged fields -
!
! - optional input/output scalars -
  Integer, Intent(InOut), Optional :: ilf ! - lagged field -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
  Type(pprd), Intent(Out) :: pdlast ! - last period read successfully -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Present
!
! Executable Statements
!
! Determine sequencing
  Call get_sequence (prd1, prd2, isq, ifail)
  Select Case (ifail)
   Case (0)
     pdlast = prd2
! - assume a lagged-field if sequencing is monthly or seasonal, unless all months / seasons are present -
     Select Case (isq)
      Case (isq_mn, isq_sn, isq_so)
        nls = nls + 1
        If (Present(ilf)) ilf = ilf + 1
        ifail = 1
! - check for yearly sequencing -
      Case (isq_yr)
        If (lss) Then
           If (prd2%sdate == prd2%edate) Then
              If (prd1%sdate%imn == prd2%sdate%imn) Then
                 isq = isq_yr
              Else
                 isq = isq_mn
              End If
! - allow monthly sequencing if all months are present -
           Else If (nls == nmn) Then
              isq = isq_so
           Else
              If (prd1%sdate%imn == prd2%sdate%imn) Then
                 isq = isq_yr
              Else
                 isq = isq_sn
              End If
           End If
           If (isq1 /= isq_sn) nls = 1
        End If
        ifail = 0
     End Select
   Case Default
     ifail = 2
  End Select
!
  Return
 End Subroutine determine_sequence
!
!
!
 Subroutine read_tags (ctag, ntags, ctags, lctagt, afield, ifail, cfail, &
                      bfield, icat, idstr, nt)
!
! Reads CPT file tag values
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Invalid combination of tags
!    ifail = -2 Missing optional argument nt for nrow and/or ncol tags
!    ifail = -3 Missing optional argument icat for C and/or clim_prob tags
!    ifail =  1 Unable to read cfail tag
!    ifail =  2 Unable to find cfail tag (if tag is a required field, otherwise ifail = 0 and default value is specified)
!    ifail =  3 Invalid value for cfail tag
!    ifail =  4 Value for crow is icompatible with ccol='X' or ccol='lon'
!    ifail =  5 Value for crow is icompatible with ccol='station'
!    ifail =  6 Value for crow is icompatible with ccol='index'
!    ifail =  7 Unknown value for ccol
!
! Modules
  Use data_io_constants, Only: ldat
  Use data_numbers,      Only: zero
  Use time,              Only: get_pdate
  Use fields,            Only: field
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: ntags ! - number of desired tags -
!
  Character(Len=*), Intent(In) :: ctag ! - tag line -
!
  Logical, Intent(In) :: lctagt ! - cpt:T tag required? -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: icat ! - current category (category 1 is used if unspecified) -
!
  Type(field), Intent(In), Optional :: bfield ! - default field -
!
! Input arrays
  Character(Len=*), Dimension(:), Intent(In) :: ctags ! - desired tags -
!
! Input/output scalars
  Type(field), Intent(InOut) :: afield ! - new field -
!
! - optional input/output scalars -
  Integer, Intent(InOut), Optional :: nt ! - number of time steps -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
! - optional output scalars -
  Integer, Intent(Out), Optional :: idstr ! - dataset structure indicator -
!
  Character(Len=*), Intent(Out) :: cfail ! - problem tag -
!
! Locals
  Integer :: i    ! - tag index -
  Integer :: i1   ! - locator -
  Integer :: i2   ! - locator -
  Integer :: i3   ! - trimmed length -
  Integer :: ic   ! - category -
  Integer :: ncol ! - number of columns -
  Integer :: nrow ! - number of rows -
  Integer :: ierr ! - error indicator -
!
  Integer, SAVE :: igrd ! - data structure indicator -
!
  Real(Kind=rp) :: r ! - real input value -
!
  Character(Len=ldat) :: ccol ! - columns -
  Character(Len=ldat) :: crow ! - rows -
!
  Logical :: lcol   ! - columns set? -
  Logical :: lrow   ! - rows set? -
  Logical :: lncol  ! - number of columns set? -
  Logical :: lnrow  ! - number of rows set? -
  Logical :: lsdate ! - start date set? -
  Logical :: lfdate ! - forecast date set? -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Iachar, Index, Len, Len_Trim, MaxVal, Nint, Present, Scan, Trim
!
! Executable Statements
!
! Initialise
  lcol = .false.
  lrow = .false.
  lncol = .false.
  lnrow = .false.
  lsdate = .false.
  lfdate = .false.
  i3 = Len_Trim(ctag)
  Do
     If ((Iachar(ctag(i3:i3)) == 9) .or. (ctag(i3:i3) == '"')) Then
        i3 = i3 - 1
     Else
        Exit
     End If
  End Do
  If (Present(icat)) Then
     ic = icat
  Else
     ic = 1
  End If
  If (Present(bfield)) afield = bfield
!
! Repeat for each tag
  ifail = 1
  Do i = 1, ntags
     cfail = ctags(i)
     Select Case (Trim(ctags(i)))
!
! Current date
      Case ('T')
        Call get_pdate ('T', ctag(1:i3), afield%tprd%sdate, ierr, &
             edate=afield%tprd%edate)
        Select Case (ierr)
         Case (0)
           Continue
         Case (1)
           If (lctagt) Then
              ifail = 2
              Return
           Else
              If (Present(bfield)) afield%tprd = bfield%tprd
           End If
         Case (2)
           ifail = 1
           Return
         Case (3)
           ifail = ierr
           Return
        End Select
!
! Start date for model data
      Case ('S')
        If (lfdate) Cycle
        Call get_pdate ('S', ctag(1:i3), afield%mdate, ierr)
        Select Case (ierr)
         Case (0)
           lsdate = .true.
         Case (1)
           If (Present(bfield)) afield%mdate = bfield%mdate
           lsdate = .true.
         Case (2)
           ifail = 1
           Return
         Case (3)
           ifail = ierr
           Return
        End Select
!
! Forecast date
      Case ('F')
        If (lsdate) Cycle
        Call get_pdate ('F', ctag(1:i3), afield%mdate, ierr)
        Select Case (ierr)
         Case (0)
           lfdate = .true.
         Case (1)
           If (Present(bfield)) afield%mdate = bfield%mdate
           lfdate = .true.
         Case (2)
           ifail = 1
           Return
         Case (3)
           ifail = ierr
           Return
        End Select
!
! Category
      Case ('C')
        If (.not.Present(icat)) Then
           ifail = -3
           Return
        End If
        i1=Index(ctag(1:), 'cpt:C=')
        If (i1 > 0) Then
           i1 = i1 + Len('cpt:C=')
           i2 = i1 + Scan(ctag(i1:), ', ') - 2
           If (i2 < i1) i2 = i3
           Read (Unit=ctag(i1:i2), Fmt=*, Err=1) afield%icat(ic)
        Else
           If (Present(bfield)) Then
              afield%icat(ic) = bfield%icat(ic)
           Else
              afield%icat(ic) = 0
           End If
        End If
!
! Climatological probability
      Case ('clim_prob')
        If (.not.Present(icat)) Then
           ifail = -3
           Return
        End If
        i1=Index(ctag(1:), 'cpt:clim_prob=')
        If (i1 > 0) Then
           i1 = i1 + Len('cpt:clim_prob=')
           i2 = i1 + Scan(ctag(i1:), ', ') - 2
           If (i2 < i1) i2 = i3
           Read (Unit=ctag(i1:i2), Fmt=*, Err=1) afield%prob(ic)
        Else
           If (Present(bfield)) Then
              afield%prob(ic) = bfield%prob(ic)
           Else
              afield%prob(ic) = zero
           End If
        End If
!
! Level
      Case ('Z')
        i1 = MaxVal((/Index(ctag(1:), 'cpt:Z='), Index(ctag(1:), 'cpt:zlev='), Index(ctag(1:), 'cpt:P=')/))
        If (i1 > 0) Then
           i1 = i1 + Index(ctag(i1:), '=')
           i2 = i1 + Scan(ctag(i1:), ', ') - 1
           If (i2 < i1) i2 = i3
           Read (Unit=ctag(i1:i2), Fmt=*, IOstat=ifail) afield%z%hght, afield%z%unit
           If (ifail /= 0) Then
              Read (Unit=ctag(i1:i2), Fmt=*, Err=1) afield%z%hght
              afield%z%unit = 'none'
           End If
           ifail = 1
        Else
           If (Present(bfield)) Then
              afield%z%hght = bfield%z%hght
              afield%z%unit = bfield%z%unit
           Else
              afield%z%hght = zero
              afield%z%unit = 'none'
           End If
        End If
!
! Ensemble member
      Case ('M')
        i1 = Index(ctag(1:), 'cpt:M=')
        If (i1 > 0) Then
           i1 = i1 + Len('cpt:M=')
           i2 = i1 + Scan(ctag(i1:), ', ')-2
           If (i2 < i1) i2 = i3
           Read (Unit=ctag(i1:i2), Fmt=*, Err=1) r
           afield%member = Nint(r)
        Else
           If (Present(bfield)) Then
              afield%member = bfield%member
           Else
              afield%member = 0
           End If
        End If
!
! Model
      Case ('model')
        i1 = Index(ctag(1:), 'cpt:model=')
        If (i1 > 0) Then
           i1 = i1 + Len('cpt:model=')
           i2 = i1 + Scan(ctag(i1:), ', ') - 2
           If (i2 < i1) i2 = i3
           Read (Unit=ctag(i1:i2), Fmt='(A)', Err=1) afield%model
        Else
           If (Present(bfield)) Then
              afield%model = bfield%model
           Else
              afield%model = 'N/A'
           End If
        End If
!
! Data structure
! - columns -
      Case ('col')
        i1 = Index(ctag(1:), 'cpt:col=')
        If (i1 > 0) Then
           i1 = i1 + Len('cpt:col=')
           i2 = i1 + Scan(ctag(i1:), ', ') - 2
           If (i2 < i1) i2 = i3
           Read (Unit=ctag(i1:i2), Fmt='(A)', Err=1) ccol
           lcol = .true.
        Else
           If (.not.Present(bfield)) ifail = -2
        End If
! - rows -
      Case ('row')
        i1=Index(ctag(1:), 'cpt:row=')
        If (i1 > 0) Then
           i1 =i1 + Len('cpt:row=')
           i2 =i1 + Scan(ctag(i1:), ', ') - 2
           If (i2 < i1) i2 = i3
           Read (Unit=ctag(i1:i2), Fmt='(A)', Err=1) crow
           lrow = .true.
        Else
           If (.not.Present(bfield)) ifail = -2
        End If
!
! Identify number of latitudes and longitudes / number of stations / indices
! - number of columns -
      Case ('ncol')
        i1 = Index(ctag(1:), 'cpt:ncol=')
        If (i1 > 0) Then
           i1 = i1 + Len('cpt:ncol=')
           Read (Unit=ctag(i1:), Fmt=*, Err=1) ncol
           lncol = .true.
        Else
           If (.not.Present(bfield)) ifail = 2
        End If
! - number of rows -
      Case ('nrow')
        i1 = Index(ctag(1:), 'cpt:nrow=')
        If (i1 > 0) Then
           i1 = i1 + Len('cpt:nrow=')
           Read (Unit=ctag(i1:), Fmt=*, Err=1) nrow
           lnrow = .true.
        Else
           If (.not.Present(bfield)) ifail = 2
        End If
!
! Field variable
      Case ('field')
        i1 = Index(ctag(1:), 'cpt:field=')
        If (i1 > 0) Then
           i1 = i1 + Len('cpt:field=')
           i2 = i1 + Scan(ctag(i1:), ', ')-2
           If (i2 < i1) i2=i3
           Read (Unit=ctag(i1:i2), Fmt='(A)', Err=1) afield%var
        Else
           If (Present(bfield)) Then
              afield%var = bfield%var
           Else
              afield%var = 'undefined'
           End If
        End If
!
! Variable units
      Case ('units')
        i1 = Index(ctag(1:), 'cpt:units=')
        If (i1 > 0) Then
           i1 = i1 + Len('cpt:units=')
           i2 = i1 + Scan(ctag(i1:), ', ')-2
           If (i2 < i1) i2 = i3
           Read (Unit=ctag(i1:i2), Fmt='(A)', Err=1) afield%unit
        Else
           If (Present(bfield)) Then
              afield%unit = bfield%unit
           Else
              afield%unit = 'undefined'
           End If
        End If
!
! Missing value flag
      Case ('missing')
        i1 = Index(ctag(1:), 'cpt:missing=')
        If (i1 > 0) Then
           i1 = i1 + Len('cpt:missing=')
           i2 = i1 + Scan(ctag(i1:), ', ') - 2
           If (i2 < i1) i2 = i3
           Read (Unit=ctag(i1:i3), Fmt=*, Err=1) afield%rmiss
        End If
     End Select
  End Do
  If (ifail /= 1) Return
  cfail = ' '
!
! Check for valid combination of requested tags
  If ((lrow.EQV.lcol) .and. (lnrow.EQV.lncol)) Then
     If (lcol) Then
        Call get_idstr (ccol, crow, igrd, ifail)
        If (ifail /= 0) Then
           ifail = ifail + 3
           Return
        End If
        If (Present(idstr)) idstr = igrd
     End If
     If (lcol .or. Present(bfield)) Then
        Select Case (igrd)
         Case (1) ! - gridded data -
           If (lncol) afield%nlg =ncol
           If (lnrow) afield%nlt = nrow
         Case (2) ! - station data -
           If (lncol) Then
              afield%nlt = ncol
              afield%nlg = ncol
           End If
           If (lnrow) nt = nrow
         Case (3) ! - station and unreferenced data -
           If (lncol) Then
              afield%nlt = ncol
              afield%nlg = ncol
           End If
           If (lnrow) Then
              If (.not.Present(nt)) Then
                 ifail = -2
                 Return
              End If
              nt = nrow
           End If
        End Select
        ifail = 0
     Else If (lncol .and. .not.Present(bfield)) Then
        ifail = -1
     Else
        ifail = 0
     End If
  Else If (lnrow .or. lncol) Then
     If (Present(bfield)) Then
        Select Case (igrd)
         Case (1) ! - gridded data -
           If (lncol) afield%nlg = ncol
           If (lnrow) afield%nlt = nrow
         Case (2) ! - station data -
           If (lncol) Then
              afield%nlt = ncol
              afield%nlg = ncol
           End If
           If (lnrow) nt = nrow
         Case (3) ! - station and unreferenced data -
           If (lncol) Then
              afield%nlt = ncol
              afield%nlg = ncol
           End If
           If (lnrow) Then
              If (.not.Present(nt)) Then
                 ifail = -2
                 Return
              End If
              nt = nrow
           End If
        End Select
     Else
        ifail = -1
     End If
  Else
     ifail = -1
  End If
!
! Errors
1 Return
!
 Contains
!
!
 Subroutine get_idstr (ccol, crow, idstr, ifail)
!
! Determines file structure
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Value for crow is icompatible with ccol='X' or ccol='lon'
!    ifail =  2 Value for crow is icompatible with ccol='station'
!    ifail =  3 Value for crow is icompatible with ccol='index'
!    ifail =  4 Unknown value for ccol
!
! Arguments
!
! Input scalars
  Character(Len=ldat), Intent(In) :: ccol ! - columns -
  Character(Len=ldat), Intent(In) :: crow ! - rows -
!
! Output scalars
  Integer, Intent(Out) :: idstr ! - file structure -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Determine structure
  idstr = 0
  Select Case (Trim(ccol))
   Case ('X', 'lon')
     Select Case (Trim(crow))
      Case ('Y', 'lat') ! - gridded file -
        idstr = 1
      Case Default
        ifail = 1
        Return
     End Select
   Case ('station')
     Select Case (Trim(crow))
      Case ('T') ! - station file -
        idstr = 2
      Case Default
        ifail = 2
        Return
     End Select
   Case ('index')
     Select Case (Trim(crow))
      Case ('T') ! - unreferenced file -
        idstr = 3
      Case Default
        ifail = 3
        Return
     End Select
   Case Default
     ifail = 4
     Return
  End Select
  ifail = 0
!
  Return
  End Subroutine get_idstr
 End Subroutine read_tags
End Module get_input_file
