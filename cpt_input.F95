! Author: Simon Mason
Module cpt_input
!
! Modules
  Use data_numbers, Only: rp
!
! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: get_data, init_read, num_read, proj_read_data, proj_write_data, read_data
!
Contains
!
!
 Function get_data(icsv, lcex, lfail)
!
! Gets X and Y data and sets up default forecast file
!
! On exit returns:
!    0 if no errors
!   -1 if cancelled
!    1 if insufficient memory to read data
!    2 if first year of X training period is before first year in file
!    3 if first year of Y training period is before first year in file
!
! Modules
  Use data_numbers,        Only: one
  Use data_cpt_constants,  Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv
  Use data_time_constants, Only: isq_mn, isq_sn, isq_so, isq_yr, nmn
  Use time,                Only: Operator(+), Operator(-), Operator(<), &
                                 iseq, nss, &
                                 date_diff
  Use progress_meter,      Only: lcalc, &
                                 set_progress_increment
  Use errors,              Only: cpt_error
#if GUI == 1
  Use errors,              Only: query
#endif
  Use arrays,              Only: x, y
  Use settings,            Only: nct, nmt, nt, nx, ny
  Use iofiles,             Only: mfile, tfile=>bkfile, xfile, yfile
  Use space,               Only: idomx, idomy
  Use fields,              Only: mfield, xfield, yfield
  Use missing,             Only: kax, kay
  Use climate,             Only: clim, climate_per
  Use season,              Only: lensn, lsn
  Use analysis,            Only: ianal
!
! Function type
  Integer :: get_data
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: icsv ! - calculate seasonal values flag -
!
  Logical, Intent(In) :: lcex  ! - climatological period extends beyond the range of the training period? -
  Logical, Intent(In) :: lfail ! - return if error reading Y data? -
!
! Locals
!
! Local scalars
  Integer :: l     ! - current field -
  Integer :: il    ! - field offset -
  Integer :: isq   ! - sequence flag -
  Integer :: ifail ! - error indicator -
  Integer :: nact  ! - number of actions -
  Integer :: ntx   ! - number of remaining cases in X file -
  Integer :: m1    ! - number of grids / ensemble members -
!
  Character(Len=10) :: cproc ! - procedure -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Max, Mod, Real, Sum
!
! Executable Statements
!
! Initialise reading
  get_data = -1
  Select Case (ianal)
   Case (ia_cca, ia_pcr, ia_mlr, ia_pfv)
     m1 = xfile%ngs
   Case (ia_gcm)
     m1 = Max(1,xfile%nem)
  End Select
  If (icsv == 0) Then
     nmt = nt
     tfile = yfile
  Else
     tfile=mfile
     Select Case (iseq)
      Case (isq_yr)
        nmt = (nt - 1)*nmn + lensn
      Case (isq_sn)
        nmt = date_diff(yfile%fdate, yfile%fdate+(nt-2), isq_mn) + lensn
      Case (isq_so)
        nmt = nt + lsn(nmn) - 1
      Case Default
        ntx = date_diff(xfile%bdate, xfile%fdate+(nt-1), xfile%iseq) + 1
        nmt = date_diff(tfile%bdate, tfile%fdate+(nt-1), tfile%iseq) + 1
        If (ntx > nmt) Then
           nmt = nmt - Mod(lensn, nmn)
        Else
           nmt = Min(ntx + Mod(lensn, nmn), nmt)
        End If
     End Select
  End If
  If (lcex) Then
     If (climate_per%d1 < tfile%bdate) tfile%bdate = climate_per%d1
  End If
!
! Check read settings
  Call check_read (ianal, isq, ifail)
  Select Case (ifail)
   Case (0)
     isq = 1
   Case (1:2)
     cproc = 'check_read'
     GoTo 1
   Case Default
     If (lcalc) Then
#if GUI == 1
        If (1-query('check_read', -ifail, .true.) /= 0) Return
#endif
     Else ! - force cancellation for file merging -
        cproc = 'check_read'
        ifail = 2 - ifail
        GoTo 1
     End If
  End Select
!
! Initialise progress increment for reading data
  nact = num_read(yfile%idstr, yfile%lstack, yfile%nt, nct, Sum(yfield(1:)%nlt), yfile%nfs, yfile%nls, yfile%ngs, yfile%it1) + &
         num_read(xfile%idstr, xfile%lstack, xfile%nt, nt, Sum(xfield(:)%nlt), xfile%nfs, xfile%nls, xfile%ngs, xfile%it1)
  Call set_progress_increment (one/Real(nact+1, Kind=rp), .false.)
!
! Initialise memory
  ifail = init_read(nx, ny, m1, nt, nct, nmt)
  If (ifail /= 0) Then
     cproc = 'init_read'
     GoTo 1
  End If
!
! Read climatology if necessary
  If (.not.lfail) Then
     il = 0
     Do l = 1, tfile%nfl
        y(il+1:il+yfield(l)%nva,:,:) = yfield(l)%rmiss
        il = il + yfield(l)%nva
     End Do
  End If
  If (lcex) Then
     If (icsv == 0) Then
        Call read_data (tfile, yfield(1:), nct, idomy, kay, clim, ifail)
     Else
        Call read_data (tfile, mfield, nct, idomy, kay, clim, ifail)
     End If
!
! Read Y data
  Else
     If (icsv == 0) Then
        Call read_data (tfile, yfield(1:), nmt, idomy, kay, y, ifail)
     Else
        Call read_data (tfile, mfield, nmt, idomy, kay, y, ifail)
     End If
  End If
  If (ifail /= 0 .and. lfail) Return
!
! Read X data
  If (.not.lfail) Then
     il = 0
     Do l = 1, xfile%nfl
        x(il+1:il+xfield(l)%nva,:,:) = xfield(l)%rmiss
        il = il + xfield(l)%nva
     End Do
  End If
  xfile%nls = xfile%nls/nss
  Call read_data (xfile, xfield, nt, idomx, kax, x, ifail)
  xfile%nls = xfile%nls*nss
  If (ifail /= 0) Return
!
  get_data = 0
  Return
!
! Errors
1 get_data = ifail
  Call cpt_error (cproc, ifail, .false., &
       i_arg1=isq)
!
  Return
!
 Contains
!
!
  Subroutine check_read (ianal, nn, ifail)
!
! Checks reading settings
!
! On exit:
!    ifail =  0 if all checks passed
!    ifail =  1 if Insufficient X data
!    ifail =  2 if Insufficient Y data
!    ifail = -1 if negative lag
!    ifail = -2 if synchronous predictors and predictands with synchronous predictors switched off
!    ifail = -3 if large lag
!    ifail = -4 if inconsistent target period
!
! Modules
  Use data_cpt_constants, Only: ia_pfv
  Use time,               Only: check_lead
  Use settings,           Only: isynch, &
                                check_nt, set_it1
!
! Arguments
!
! Inpur scalars
  Integer, Intent(In) :: ianal ! - analysis flag -
!
! Output scalars
  Integer, Intent(Out) :: nn    ! - number of cases available -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Check date availability
  xfile%it1 = set_it1(xfile%iseq, xfile%prd1%sdate, xfile%bdate)
  tfile%it1 = set_it1(tfile%iseq, tfile%prd1%sdate, tfile%bdate)
  yfile%it1 = set_it1(yfile%iseq, yfile%prd1%sdate, yfile%fdate)
!
! Check that data for training period are available
  ifail = check_nt(nt, xfile%it1, xfile%nt)
  If (ifail /= 0) Then
     nn = ifail
     ifail = 1
     Return
  End If
  ifail = check_nt(nmt, tfile%it1, tfile%nt)
  If (ifail /= 0) Then
     If (nmt /= nt) Then
        nmt = ifail
        nn = ifail/nmn
     Else
        nn = ifail
     End If
     ifail = 2
     Return
  Else
     mfile%it1 = tfile%it1
  End If
!
! Check for negative leads
  Select Case (ianal)
   Case Default
     Call check_lead (xfile%fdate, yfile%fdate, xfield(1)%mdate, xfile%it1, xfile%iseq, isynch, ifail)
   Case (ia_pfv)
     Call check_lead (xfile%fdate, yfile%fdate, xfield(1)%mdate, xfile%it1, xfile%iseq,      1, ifail)
  End Select
  ifail = -ifail
!
  Return
  End Subroutine check_read
 End Function get_data
!
!
!
 Function init_read(nx, ny, ngs, nt, nct, nty, &
          lb, ns1, ns2) &
          Result (init)
!
! Initialises memory and settings for reading data
!
! On exit, returns:
!    0 if no errors
!    1 if problem allocating memory
!
! Modules
  Use arrays,  Only: rwk, x, y
  Use fields,  Only: xfield, yfield
  Use climate, Only: clim, lcex
!
! Function type
  Integer :: init
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nx  ! - number of X variables -
  Integer, Intent(In) :: ny  ! - number of Y variables -
  Integer, Intent(In) :: ngs ! - number of X categories -
  Integer, Intent(In) :: nt  ! - length of training period -
  Integer, Intent(In) :: nct ! - length of training period with climatology -
  Integer, Intent(In) :: nty ! - number of Y cases to read -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: lb  ! - lower bound -
  Integer, Intent(In), Optional :: ns1 ! - number of seasons -
  Integer, Intent(In), Optional :: ns2 ! - number of seasons -
!
! Locals
!
! Local scalars
  Integer :: mlgx  ! - maximum number of X longitudes -
  Integer :: mlgy  ! - maximum number of Y longitudes -
  Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Max, MaxVal, Present
!
! Executable Statements
!
! Allocate data space
  init = 1
! - X data -
  Allocate (x(nx, nt, ngs), Stat=ifail)
  If (ifail /= 0) Return
! - Y data -
  If (ny > 0) Then
     If (.not.(Present(lb) .and. Present(ns1))) Then
        Allocate (y(ny, nty, 1), Stat=ifail)
     Else
        Allocate (y(ny, nty, lb:ns1), Stat=ifail)
     End If
     If (ifail /= 0) Return
! - climatology data -
     If (lcex) Then
        If (.not.(Present(lb) .and. Present(ns2))) Then
           Allocate (clim(ny, nct, 1), Stat=ifail)
        Else
           Allocate (clim(ny, nct, lb:ns2), Stat=ifail)
        End If
        If (ifail /= 0) Return
     End If
! - workspace -
     mlgy = MaxVal(yfield(:)%nlg)
  Else
     mlgy = 0
  End If
  mlgx = MaxVal(xfield(:)%nlg)
  Allocate (rwk(Max(nty, Max(mlgx, mlgy))), Stat=ifail)
  If (ifail /= 0) Return
!
  init = 0
!
  Return
 End Function init_read
!
!
!
 Function num_read(idstr, lstack, n, nt, nlt, nfs, nls, ngs, it1)
!
! Estimates number of steps to read data for progress meter
!
! Function type
  Integer :: num_read
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: idstr ! - grid identifier -
  Integer, Intent(In) :: n     ! - total number of cases in file -
  Integer, Intent(In) :: nt    ! - number of cases in training period -
  Integer, Intent(In) :: nlt   ! - number of latitudes -
  Integer, Intent(In) :: nfs   ! - number of fields -
  Integer, Intent(In) :: nls   ! - number of lagged fields -
  Integer, Intent(In) :: ngs   ! - number of categories -
  Integer, Intent(In) :: it1   ! - index of first year of interest in file -
!
  Logical, Intent(In) :: lstack ! - stacked fields? -
!
! Input arrays
!
! Locals
!
! Local scalars
  Integer :: nn ! - number of time steps -
!
! Executable Statements
!
! Calculate number of input steps
  nn = it1 - 1 + nt
  Select Case (idstr)
   Case (1)
     num_read = 1 + nfs*nls*(it1 - 1)
     num_read = num_read + ngs*(nfs*nls + nlt)*nt
     If (lstack) num_read = num_read + (nfs - 1)*nls*(n - nn)
   Case (2)
     num_read = nfs*it1 + ngs*nlt*nt + (nfs - 1)*(n - nt - it1 + 1)
   Case (3)
     num_read = nfs*it1 + ngs*nlt*nt + (nfs - 1)*(n - nt - it1 + 1)
  End Select
!
  Return  
 End Function num_read
!
!
!
 Subroutine read_data (afile, afield, n, idom, ka, v, ifail)
!
! Reads an input file
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  Use settings, Only: isem
  Use iofiles,  Only: ifile
  Use fields,   Only: field
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: n ! - number of cases -
!
  Type(ifile), Intent(In) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Input arrays
  Integer, Dimension(:,:), Intent(In) :: idom ! - used variables -
!
  Logical, Dimension(:,:), Intent(In) :: ka ! - cases available? -
!
  Type(field), Dimension(:), Intent(In) :: afield ! - fields -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: v ! - data -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Read data
  Print *, 'Reading '//Trim(afile%cfile%ffile)//' ...'
  Select Case (afile%idstr)
   Case (1)
     Call read_grid (afile, afield, n, isem, idom, ka, v, ifail)
   Case (2)
     Call read_stns (afile, afield, n, idom, ka, v, ifail)
   Case (3)
     Call read_unrf (afile, afield, n, idom, ka, v, ifail)
  End Select
!
  Return
 End Subroutine read_data
!
!
!
 Subroutine read_grid (afile, afield, n, isem, idom, ka, v, ifail)
!
! Reads gridded data
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  Use data_cpt_constants, Only: iuseq
  Use data_io_constants,  Only: iin, lprd
  Use maths,              Only: get_cordn, magnitude
  Use time,               Only: pprd, &
                                Assignment(=), &
                                get_cdate
  Use labels,             Only: ca_seqs_l
  Use errors,             Only: cpt_error
  Use arrays,             Only: rwk, &
                                insertion_sort
  Use iofiles,            Only: ifile, &
                                open_iofile
  Use iofiles,            Only: file_mdate
  Use fields,             Only: field
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: n    ! - number of cases -
  Integer, Intent(In) :: isem ! - sort ensemble members flag -
!
  Type(ifile), Intent(In) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Input arrays
  Integer, Dimension(:,:), Intent(In) :: idom ! - used gridpoints -
!
  Logical, Dimension(:,:), Intent(In) :: ka ! - cases available? -
!
  Type(field), Dimension(:), Intent(In) :: afield ! - fields -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: v ! - data -
!
! Locals
!
! Local scalars
  Integer :: i     ! - variable index -
  Integer :: ie    ! - ensemble member index -
  Integer :: ifd   ! - field index -
  Integer :: ilf   ! - lagged field index -
  Integer :: iv    ! - current variable -
  Integer :: iv1   ! - variable offset -
  Integer :: k     ! - time index -
  Integer :: l     ! - field/lagged field index -
  Integer :: nread ! - number of records read from training period -
  Integer :: im    ! - order of magnitude -
  Integer :: imi   ! - order of magnitude of latitude -
  Integer :: ilat0 ! - last successfully read latitude -
!
  Character(Len=  10) :: cfmt  ! - format statement -
  Character(Len= 128) :: cprog ! - progress -
  Character(Len=lprd) :: cdate ! - date -
!
  Type(pprd) :: prd0 ! - date of last successfully read period -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Open file
  Call open_iofile (iin, afile%cfile%ffile, .true., .true., .true., ifail)
  If (ifail /= 0) Then
     Call cpt_error ('open_iofile', ifail, .true., &
          c_arg1=Trim(afile%cfile%ffile))
     Return
  End If
!
! Check whether file has been modified
  If (file_mdate(afile%cfile%ffile) > afile%mdate) Then
     ifail = 4
     Call cpt_error ('open_iofile', ifail, .true., &
          c_arg1=Trim(afile%cfile%ffile))
     Return
  End If
!
! Read gridded data
  prd0 = 0
  Select Case (afile%cfile%ffmt%iver)
   Case (9)
     Call read_grid_v9 (afile, afield, n, idom, ka, v(:,:,1), nread, prd0, ilat0, ifail)
   Case (10)
     If (.not.afile%lstack) Then
        Call read_grid_v10_nostack (afile, afield, n, idom, ka, v, nread, prd0, ilat0, ifail)
     Else
        Call read_grid_v10_stack (afile, afield, n, idom, ka, v, nread, prd0, ilat0, ifail)
     End If
  End Select
  Close (Unit=iin)
!
! Errors
  Select Case (ifail)
   Case  (0)
     Continue
   Case (-1)
     Return
   Case Default
     If (prd0%sdate%iyr > 0) Then
        cdate = get_cdate(prd0, 1, 2)
        If (nread > 0) Then
           im = magnitude(nread)
           If (ilat0 > 0) Then
              imi = magnitude(ilat0)
              Write (Unit=cfmt, Fmt='(A,I1,A)') '(A,I', imi, ',4A,I', im, ',3A)'
              Write (Unit=cprog, Fmt=cfmt) &
                 'Data up to the ', ilat0, get_cordn(ilat0), ' latitude for', Trim(cdate), &
                 ' (', nread, ' ', Trim(ca_seqs_l(iuseq(afile%iseq))), ' of training period) read successfully.'
           Else
              Write (Unit=cfmt, Fmt='(A,I1,A)') '(3A,I', im, ',3A)'
              Write (Unit=cprog, Fmt=cfmt) &
                 'Data up to ', Trim(cdate), &
                 ' (', nread, ' ', Trim(ca_seqs_l(iuseq(afile%iseq))), ' of training period) read successfully.'
           End If
        Else
           If (ilat0 > 0) Then
              imi = magnitude(ilat0)
              Write (Unit=cfmt, Fmt='(A,I1,A)') '(A,I', imi, ',4A)'
              Write (Unit=cprog, Fmt=cfmt) &
                 'Data up to the ', ilat0, get_cordn(ilat0), ' latitude for ', Trim(cdate), ' read successfully.'
           Else
              Write (Unit=cprog, Fmt='(3A)') &
                 'Data up to ', Trim(cdate), ' read successfully.'
           End If
        End If
        Call cpt_error ('read_grid', ifail, .true., &
             i_arg1=nread, c_arg1=Trim(afile%cfile%ffile), c_arg2=Trim(cprog))
     Else
        Call cpt_error ('read_grid', ifail, .true., &
             c_arg1=Trim(afile%cfile%ffile))
     End If
     Return
  End Select
!
! Sort ensemble members
  If (afile%lensemble .and. (isem == 1)) Then
     iv1 = 0
     Do ifd = 1, afile%nfs/afile%nem
        Do ilf = 1, afile%nls
           l = (ifd - 1)*afile%nem*afile%nls + ilf
           Do i = 1, afield(l)%nv
              Do k = 1, n
                 Do ie = 1, afile%nem
                    iv = iv1 + ((ie - 1)*afile%nls + (ilf - 1))*afield(l)%nv + i
                    rwk(ie) = v(iv,k,1)
                 End Do
                 Call insertion_sort (afile%nem, rwk(:), 'a')
                 Do ie = 1, afile%nem
                    iv = iv1 + ((ie - 1)*afile%nls + (ilf - 1))*afield(l)%nv + i
                    v(iv,k,1) = rwk(ie)
                 End Do
              End Do
           End Do
        End Do
        l = (ifd - 1)*afile%nem*afile%nls
        iv1 = iv1 + afile%nem*afile%nls*afield(l)%nv
     End Do
  End If
!
  Return
!
 Contains
!
!
  Subroutine read_grid_v9 (afile, afield, n, idom, ka, v, nread, prd0, ilat0, ifail)
!
! Reads formatted gridded data
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Aborted
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  Use data_time_constants, Only: lmon
  Use time,                Only: pdate, &
                                 get_month
  Use progress_meter,      Only: update_progress_meter
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: n ! - number of cases -
!
  Type(ifile), Intent(In) :: afile ! - file -
!
! Input/output scalars
  Type(pprd), Intent(InOut) :: prd0 ! - last successfully read period -
!
! Input arrays
  Integer, Dimension(:,:), Intent(In) :: idom ! - used gridpoints -
!
  Logical, Dimension(:,:), Intent(In) :: ka ! - cases available? -
!
  Type(field), Dimension(:), Intent(In) :: afield ! - fields -
!
! Output scalars
  Integer, Intent(Out) :: nread ! - number of cases in training period read successfully -
  Integer, Intent(Out) :: ilat0 ! - last successfully read latitude -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: v ! - data -
!
! Locals
!
! Local scalars
  Integer :: i      ! - latitude index -
  Integer :: i1     ! - first latitude index -
  Integer :: i2     ! - last latitude index -
  Integer :: iinc   ! - latitude index increment -
  Integer :: j      ! - longitude index -
  Integer :: j0     ! - initial longitude offset -
  Integer :: jj     ! - grid index -
  Integer :: k      ! - time index -
  Integer :: irskip ! - number of records to skip -
!
  Real(Kind=rp) :: rlat ! - latitudes -
!
  Character(Len=lmon) :: cmon ! - current month -
!
  Type(pdate) :: date1 ! - current date -
!
! Executable Statements
!
! Skip first few records if required
  ifail = -1
  ilat0 = 0
  nread = 0
  irskip = afile%it1 - 1
  If (update_progress_meter(.false.) /= 0) Return
  If (irskip > 0) Then
     Do k = 1, irskip
        If (.not.ka(k,1)) Cycle
        Read (Unit=iin, Fmt=*, Err=1, End=2) date1%idy, cmon, date1%iyr
        date1%imn = get_month(cmon)
        Do i = 1, afield(1)%nlt
           Read (Unit=iin, Fmt=*, Err=1, End=2)
        End Do
        If (update_progress_meter(.false.) /= 0) Return
        prd0%sdate = date1
     End Do
  End If
!
! Determine direction
  If (afield(1)%ln2s) Then
     i1 = 1
     i2 = afield(1)%nlt
     iinc = 1
     j0 = 0
  Else
     i1 = afield(1)%nlt
     i2 = 1
     iinc = -1
     j0 = afield(1)%region%nlgs*(afield(1)%region%nlts - 1)
  End If
!
! Read data
  Do k = 1, n
     If (ka(irskip+k,1)) Then
        Read (Unit=iin, Fmt=*, Err=1, End=2) date1%idy, cmon, date1%iyr
        date1%imn = get_month(cmon)
        If (update_progress_meter(.false.) /= 0) Return
        jj = j0
        Do i = i1, i2, iinc
           If ((i >= afield(1)%region%nlt1) .and. (i <= afield(1)%region%nlt2)) Then
              Read (Unit=iin, Fmt=*, Err=1, End=2) rlat, (rwk(j), j=1,afield(1)%nlg)
              v(jj+1:jj+afield(1)%region%nlgs,k) = rwk(idom(1:afield(1)%region%nlgs,1))
              jj = jj + afield(1)%region%nlgs*iinc
           Else
              Read (Unit=iin, Fmt=*, Err=1, End=2)
           End If
           If (update_progress_meter(.false.) /= 0) Return
           ilat0 = i
        End Do
        ilat0 = 0
        prd0%sdate = date1
        nread = nread + 1
     Else
        v(1:afield(1)%nv,k) = afield(1)%rmiss
     End If
  End Do
!
! No errors
  ifail = 0
  Return
!
! Error
1 ifail = 1
  GoTo 3
!
! End of file
2 ifail = 2
!
3 If (ilat0 > 0) prd0%sdate = date1
  prd0%edate = prd0%sdate
  Return
!
  End Subroutine read_grid_v9
!
!
!
  Subroutine read_grid_v10_nostack (afile, afield, n, idom, ka, v, nread, prd0, ilat0, ifail)
!
! Reads formatted gridded data with unstacked fields
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Aborted
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  Use data_io_constants, Only: ltag
  Use time,              Only: get_pdate
  Use progress_meter,    Only: update_progress_meter
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: n ! - number of cases -
!
  Type(ifile), Intent(In) :: afile ! - file -
!
! Input/output scalars
  Type(pprd), Intent(InOut) :: prd0 ! - last successfully read period -
!
! Output scalars
  Integer, Intent(Out) :: nread ! - number of cases read successfully -
  Integer, Intent(Out) :: ilat0 ! - last successfully read latitude -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Input arrays
  Integer, Dimension(:,:), Intent(In) :: idom ! - used gridpoints -
!
  Logical, Dimension(:,:), Intent(In) :: ka ! - cases available? -
!
  Type(field), Dimension(:), Intent(In) :: afield ! - fields -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: v ! - data -
!
! Locals
!
! Local scalars
  Integer :: i      ! - latitude index -
  Integer :: i1     ! - first latitude index -
  Integer :: i2     ! - last latitude index -
  Integer :: iinc   ! - latitude index increment -
  Integer :: j      ! - longitude index -
  Integer :: jj     ! - grid index -
  Integer :: k      ! - time index -
  Integer :: l      ! - field / lag index -
  Integer :: igf    ! - category index -
  Integer :: irskip ! - number of records to skip -
  Integer :: ierr   ! - error indicator -
!
  Real(Kind=rp) :: rlat ! - latitudes -
!
  Character(Len=ltag) :: ctag0 ! - previous tag line -
  Character(Len=ltag) :: ctag1 ! - current tag line -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Sum
!
! Executable Statements
!
! Skip v10 XML namespace headers and tags
  ifail = -1
  xml: Do k = 1, afile%ntag
     Read (Unit=iin, Fmt=*, Err=1, End=2)
  End Do xml
  If (update_progress_meter(.false.) /= 0) Return
!
! Skip first few records if required
  nread = 0
  ilat0 = 0
  ctag0 = 'None'
  irskip = afile%it1 - 1
  If (irskip > 0) Then
     Do k = 1, irskip
        Do l = 1, afile%nfl
           If (.not.ka(k,l)) Cycle
           Do igf = 1, afile%ngs
              Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag1
              Read (Unit=iin, Fmt=*, Err=1, End=2)
              Do i = 1, afield(l)%nlt
                 Read (Unit=iin, Fmt=*, Err=1, End=2)
              End Do
           End Do
           ctag0 = ctag1
           If (update_progress_meter(.false.) /= 0) Return
        End Do
     End Do
  End If
!
! Read data
  Do k = 1, n
     Do ilf = 1, afile%nls
        Do ifd = 1, afile%nfs
           l = (ifd - 1)*afile%nls + ilf
           If (ka(irskip+k,1)) Then
              Do igf = 1, afile%ngs
                 Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag1
                 Read (Unit=iin, Fmt=*, Err=1, End=2)
                 If (update_progress_meter(.false.) /= 0) Return
                 If (afield(l)%ln2s) Then
                    i1 = 1
                    i2 = afield(l)%nlt
                    iinc = 1
                    jj = 0
                    If (l > 1) jj = jj + Sum(afield(1:l-1)%region%nlts*afield(1:l-1)%region%nlgs)
                 Else
                    i1 = afield(l)%nlt
                    i2 = 1
                    iinc = -1
                    jj = afield(l)%region%nlgs*(afield(l)%region%nlts - 1)
                    If (l > 1) jj = jj + Sum(afield(1:l-1)%region%nlts*afield(1:l-1)%region%nlgs)
                 End If
                 Do i = i1, i2, iinc
                    If ((i >= afield(l)%region%nlt1) .and. (i <= afield(l)%region%nlt2)) Then
                       Read (Unit=iin, Fmt=*, Err=1, End=2) rlat, (rwk(j), j=1,afield(l)%nlg)
                       v(jj+1:jj+afield(l)%region%nlgs,k,igf) = rwk(idom(1:afield(l)%region%nlgs,l))
                       jj = jj + afield(l)%region%nlgs*iinc
                    Else
                       Read (Unit=iin, Fmt=*, Err=1, End=2)
                    End If
                    If (update_progress_meter(.false.) /= 0) Return
                    ilat0 = i
                 End Do
                 ilat0 = 0
              End Do
              ctag0 = ctag1
           Else
              jj = 0
              If (l > 1) jj = jj + Sum(afield(1:l-1)%region%nlts*afield(1:l-1)%region%nlgs)
              v(jj+1:jj+afield(l)%region%nlts*afield(l)%region%nlgs,k,:) = afield(l)%rmiss
              Do igf = 1, afile%ngs
                 Do i = 1, afield(l)%nlt+1
                    If (update_progress_meter(.false.) /= 0) Return
                 End Do
              End Do
           End If
        End Do
        nread = nread + 1
     End Do
  End Do
!
! No errors
  ifail = 0
  Return
!
! Error reading file
1 ifail = 1
  GoTo 3
!
! End of file
2 ifail = 2
!
! Get latest date
3 If (ilat0 > 0) ctag0=ctag1
  If (ctag0(1:4) /= 'None') Then
     Call get_pdate ('T', ctag0, prd0%sdate, ierr, &
          edate=prd0%edate)
  End If
!
  Return
  End Subroutine read_grid_v10_nostack
!
!
!
  Subroutine read_grid_v10_stack (afile, afield, n, idom, ka, v, nread, prd0, ilat0, ifail)
!
! Reads formatted gridded data with stacked fields
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Aborted
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  Use data_io_constants, Only: ltag
  Use time,              Only: get_pdate
  Use progress_meter,    Only: update_progress_meter
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: n ! - number of cases -
!
  Type(ifile), Intent(In) :: afile ! - file -
!
! Input/output scalars
  Type(pprd), Intent(InOut) :: prd0 ! - last successfully read period -
!
! Output scalars
  Integer, Intent(Out) :: nread ! - number of cases read successfully -
  Integer, Intent(Out) :: ilat0 ! - last successfully read latitude -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Input arrays
  Integer, Dimension(:,:), Intent(In) :: idom ! - used gridpoints -
!
  Logical, Dimension(:,:), Intent(In) :: ka ! - cases available? -
!
  Type(field), Dimension(:), Intent(In) :: afield ! - fields -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: v ! - data -
!
! Locals
!
! Local scalars
  Integer :: i      ! - latitude index -
  Integer :: i1     ! - first latitude index -
  Integer :: i2     ! - last latitude index -
  Integer :: iinc   ! - latitude index increment -
  Integer :: j      ! - longitude index -
  Integer :: jj     ! - grid index -
  Integer :: k      ! - time index -
  Integer :: l      ! - field / lag index -
  Integer :: igf    ! - category index -
  Integer :: irskip ! - number of records to skip -
  Integer :: ierr   ! - error indicator -
!
  Real(Kind=rp) :: rlat ! - latitudes -
!
  Character(Len=ltag) :: ctag0 ! - previous tag line -
  Character(Len=ltag) :: ctag1 ! - current tag line -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Sum
!
! Executable Statements
!
! Skip v10 XML namespace headers and tags
  ifail = -1
  xml: Do k = 1, afile%ntag
     Read (Unit=iin, Fmt=*, Err=1, End=2)
  End Do xml
  If (update_progress_meter(.false.) /= 0) Return
!
! Skip first few records if required
  nread = 0
  ilat0 = 0
  ctag0 = 'None'
  irskip = afile%it1 - 1
  Do ifd = 1, afile%nfs
     If (irskip > 0) Then
        Do k = 1, irskip
           Do ilf = 1, afile%nls
              l = (ifd - 1)*afile%nls + ilf
              If (ka(k,l)) Then
                 Do igf = 1, afile%ngs
                    Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag1
                    Read (Unit=iin, Fmt=*, Err=1, End=2)
                    Do i = 1, afield(l)%nlt
                       Read (Unit=iin, Fmt=*, Err=1, End=2)
                    End Do
                 End Do
                 ctag0 = ctag1
              End If
              If (update_progress_meter(.false.) /= 0) Return
           End Do
        End Do
     End If
!
! Read data
     Do k = 1, n
        Do ilf = 1, afile%nls
           l = (ifd - 1)*afile%nls + ilf
           If (ka(irskip+k,1)) Then
              Do igf = 1, afile%ngs
                 Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag1
                 Read (Unit=iin, Fmt=*, Err=1, End=2)
                 If (update_progress_meter(.false.) /= 0) Return
                 If (afield(l)%ln2s) Then
                    i1 = 1
                    i2 = afield(l)%nlt
                    iinc = 1
                    jj = 0
                    If (l > 1) jj = jj + Sum(afield(1:l-1)%region%nlts*afield(1:l-1)%region%nlgs)
                 Else
                    i1 = afield(l)%nlt
                    i2 = 1
                    iinc = -1
                    jj = afield(l)%region%nlgs*(afield(l)%region%nlts - 1)
                    If (l > 1) jj = jj + Sum(afield(1:l-1)%region%nlts*afield(1:l-1)%region%nlgs)
                 End If
                 Do i = i1, i2, iinc
                    If ((i >= afield(l)%region%nlt1) .and. (i <= afield(l)%region%nlt2)) Then
                       Read (Unit=iin, Fmt=*, Err=1, End=2) rlat, (rwk(j), j=1,afield(l)%nlg)
                       v(jj+1:jj+afield(l)%region%nlgs,k,igf) = rwk(idom(1:afield(l)%region%nlgs,l))
                       jj = jj + afield(l)%region%nlgs*iinc
                    Else
                       Read (Unit=iin, Fmt=*, Err=1, End=2)
                    End If
                    If (update_progress_meter(.false.) /= 0) Return
                    ilat0 = i
                 End Do
                 ilat0 = 0
              End Do
              ctag0 = ctag1
           Else
              jj = 0
              If (l > 1) jj = jj + Sum(afield(1:l-1)%region%nlts*afield(1:l-1)%region%nlgs)
              v(jj+1:jj+afield(l)%region%nlts*afield(l)%region%nlgs,k,:) = afield(l)%rmiss
              Do igf = 1, afile%ngs
                 Do i = 1, afield(l)%nlt+1
                    If (update_progress_meter(.false.) /= 0) Return
                 End Do
              End Do
           End If
        End Do
        nread = nread + 1
     End Do
!
! Skip any additional data
     If ((ifd < afile%nfs) .and. (irskip+n < afile%nt)) Then
        Do k = irskip+n+1, afile%nt
           Do ilf = 1, afile%nls
              l = (ifd - 1)*afile%nls + ilf
              If (ka(k,l)) Then
                 Do igf = 1, afile%ngs
                    Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag1
                    Read (Unit=iin, Fmt=*, Err=1, End=2)
                    Do i = 1, afield(l)%nlt
                       Read (Unit=iin, Fmt=*, Err=1, End=2)
                    End Do
                 End Do
                 ctag0 = ctag1
              End If
              If (update_progress_meter(.false.) /= 0) Return
           End Do
        End Do
     End If
  End Do
!
! No errors
  ifail = 0
  Return
!
! Error reading file
1 ifail = 1
  GoTo 3
!
! End of file
2 ifail = 2
!
! Get latest date
3 If (ilat0 > 0) ctag0 = ctag1
  If (ctag0(1:4) /= 'None') Then
     Call get_pdate ('T', ctag0, prd0%sdate, ierr, &
          edate=prd0%edate)
  End If
!
  Return
  End Subroutine read_grid_v10_stack
 End Subroutine read_grid
!
!
!
 Subroutine read_stns (afile, afield, n, idom, ka, v, ifail)
!
! Selects appropriate reading routine based on file format
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  Use data_cpt_constants, Only: iuseq
  Use data_io_constants,  Only: iin, lprd
  Use maths,              Only: magnitude
  Use time,               Only: pprd, &
                                Assignment(=), &
                                get_cdate
  Use labels,             Only: ca_seqs_l
  Use errors,             Only: cpt_error
  Use arrays,             Only: rwk
  Use iofiles,            Only: ifile, &
                                file_mdate, open_iofile
  Use fields,             Only: field
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: n ! - number of cases -
!
  Type(ifile), Intent(In) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Input arrays
  Integer, Dimension(:,:), Intent(In) :: idom ! - used gridpoints -
!
  Logical, Dimension(:,:), Intent(In) :: ka ! - cases available? -
!
  Type(field), Dimension(:), Intent(In) :: afield ! - fields -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: v ! - data -
!
! Locals
!
! Local scalars
  Integer :: nread ! - number of records read from training period -
  Integer :: im    ! - order of magnitude -
!
  Character(Len=  10) :: cfmt  ! - format statement -
  Character(Len= 128) :: cprog ! - progress -
  Character(Len=lprd) :: cdate ! - date -
!
  Type(pprd) :: prd0 ! - date of last successfully read data -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Open file
  Call open_iofile (iin, afile%cfile%ffile, .true., .true., .true., ifail)
  If (ifail /= 0) Then
     Call cpt_error ('open_iofile', ifail, .true., &
          c_arg1=Trim(afile%cfile%ffile))
     Return
  End If
!
! Check whether file has been modified
  If (file_mdate(afile%cfile%ffile) > afile%mdate) Then
     ifail = 4
     Call cpt_error ('open_iofile', ifail, .true., &
          c_arg1=Trim(afile%cfile%ffile))
     Return
  End If
!
! Read station data
  prd0 = 0
  Select Case (afile%cfile%ffmt%iver)
   Case (9)
     Call read_stns_v9 (afile, afield, n, idom, ka, v(:,:,1), nread, prd0, ifail)
   Case (10)
     Call read_nongrid_v10 (afile, afield, n, idom, ka, v, nread, prd0, ifail)
  End Select
  Close (iin)
!
! Errors
  Select Case (ifail)
   Case  (0)
     Continue
   Case (-1)
     Return
   Case Default
     If (prd0%sdate%iyr > 0) Then
        cdate = get_cdate(prd0, 1, 2)
        If (nread > 0) Then
           im = magnitude(nread)
           Write (Unit=cfmt, Fmt='(A,I1,A)') '(3A,I', im, ',3A)'
           Write (Unit=cprog, Fmt=cfmt) &
              'Data up to ', Trim(cdate), ' (', nread, ' ', Trim(ca_seqs_l(iuseq(afile%iseq))), &
              ' of training period) read successfully.'
        Else
           Write (Unit=cprog, Fmt='(3A)') &
              'Data up to ', Trim(cdate), ' read successfully.'
        End If
        Call cpt_error ('read_stns', ifail, .true., &
             i_arg1=nread, c_arg1=Trim(afile%cfile%ffile), c_arg2=cprog)
     Else
        Call cpt_error ('read_stns', ifail, .true., &
             c_arg1=Trim(afile%cfile%ffile))
     End If
  End Select
!
  Return
!
 Contains
!
!
  Subroutine read_stns_v9 (afile, afield, n, idom, ka, v, nread, prd0, ifail)
!
! Reads formatted station data
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Aborted
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  Use data_numbers,        Only: one
  Use data_io_constants,   Only: ltag
  Use data_time_constants, Only: isq_yr
  Use time,                Only: get_pdate
  Use progress_meter,      Only: set_progress_increment, update_progress_meter
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: n ! - number of cases -
!
  Type(ifile), Intent(In) :: afile ! - input file -
!
! Input/output scalars
  Type(pprd), Intent(InOut) :: prd0 ! - last successfully read period -
!
! Output scalars
  Integer, Intent(Out) :: nread ! - number of cases read successfully -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Input arrays
  Integer, Dimension(:,:), Intent(In) :: idom ! - used gridpoints -
!
  Logical, Dimension(:,:), Intent(In) :: ka ! - cases available? -
!
  Type(field), Dimension(:), Intent(In) :: afield ! - fields -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: v ! - data -
!
! Locals
!
! Local scalars
  Integer :: i      ! - station index -
  Integer :: k      ! - time index -
  Integer :: irskip ! - number of records to skip -
  Integer :: icskip ! - number of initial spaces to pick -
  Integer :: lcmin  ! - minimum of length of ctag and cline -
!
  Character(Len=      ltag) :: ctag0 ! - previous tag line -
  Character(Len=      ltag) :: ctag1 ! - current tag line -
  Character(Len=afile%lmax) :: cline ! - line -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Min, Real, Trim
!
! Executable Statements
!
! Read station labels and coordinates
  ifail = -1
  nread = 0
  ctag0 = 'None'
  ctag1 = ' '
  lcmin = Min(ltag, afile%lmax)
  Read (Unit=iin, Fmt=*, Err=1, End=2)
  Read (Unit=iin, Fmt=*, Err=1, End=2)
  Read (Unit=iin, Fmt=*, Err=1, End=2)
  If (update_progress_meter(.false.) /= 0) Return
!
! Skip first few records if required
  irskip = afile%it1 - 1
  If (irskip > 0) Then
     Do k = 1, irskip
        If (.not.ka(k,1)) Cycle
        Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag1
        ctag0 = ctag1
        If (update_progress_meter(.false.) /= 0) Return
     End Do
  End If
!
! Read data
  Call set_progress_increment (Real(afield(1)%nlt, Kind=rp), .true.)
  Do k = 1, n
     If (ka(irskip+k,1)) Then
        If ((nread == 0) .or. ((afile%iseq /= isq_yr) .and. (afile%iseq /= 10))) Then
            Call parse_line (iin, cline, ifail, &
                 icskip)
            If (ifail /= 0) GoTo 3
        Else
            Call parse_line (iin, cline, ifail)
        End If
        Read (Unit=cline(icskip+1:), Fmt=*, Err=1) (rwk(i),i = 1, afield(1)%nlt)
        Do i = 1, afield(1)%region%nlts
           v(i,k) = rwk(idom(i,1))
        End Do
        ctag0(1:lcmin) = cline(1:lcmin)
        nread = nread + 1
     Else
        v(1:afield(1)%region%nlts,k) = afield(1)%rmiss
     End If
     If (update_progress_meter(.false.) /= 0) Return
  End Do
  Call set_progress_increment (one/Real(afield(1)%nlt, Kind=rp), .true.)
!
! No errors
  ifail = 0
  Return
!
! Error reading file
1 ifail = 1
  GoTo 3
!
! End of file
2 ifail = 2
!
! Get latest date
3 If (ctag0(1:4) /= 'None') Then
     Call get_pdate (' ',Trim(ctag0), prd0%sdate, ifail, &
          edate=prd0%edate)
  End If
!
  Return
  End Subroutine read_stns_v9
 End Subroutine read_stns
!
!
!
 Subroutine read_unrf (afile, afield, n, idom, ka, v, ifail)
!
! Selects appropriate reading routine based on file format
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  Use data_cpt_constants, Only: iuseq
  Use data_io_constants,  Only: iin, lprd
  Use maths,              Only: magnitude
  Use time,               Only: pprd, &
                                Assignment(=), &
                                get_cdate
  Use labels,             Only: ca_seqs_l
  Use errors,             Only: cpt_error
  Use iofiles,            Only: ifile, &
                                open_iofile
  Use iofiles,            Only: file_mdate
  Use fields,             Only: field
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: n ! - number of cases -
!
  Type(ifile), Intent(In) :: afile ! - input file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Input arrays
  Integer, Dimension(:,:), Intent(In) :: idom ! - used gridpoints -
!
  Logical, Dimension(:,:), Intent(In) :: ka ! - cases available? -
!
  Type(field), Dimension(:), Intent(In) :: afield ! - fields -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: v ! - data -
!
! Locals
!
! Local scalars
  Integer :: nread ! - number of records read from training period -
  Integer :: im    ! - order of magnitude -
!
  Character(Len=  10) :: cfmt  ! - format statement -
  Character(Len= 128) :: cprog ! - progress -
  Character(Len=lprd) :: cdate ! - date -
!
  Type(pprd) :: prd0 ! - date of last successfully read data -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Open file
  Call open_iofile (iin, afile%cfile%ffile, .true., .true., .true., ifail)
  If (ifail /= 0) Then
     Call cpt_error ('open_iofile', ifail, .true., &
          c_arg1=Trim(afile%cfile%ffile))
     Return
  End If
!
! Check whether file has been modified
  If (file_mdate(afile%cfile%ffile) > afile%mdate) Then
     ifail = 4
     Call cpt_error ('open_iofile', ifail, .true., &
          c_arg1=Trim(afile%cfile%ffile))
     Return
  End If
!
! Read ungridded data
  prd0 = 0
  Select Case (afile%cfile%ffmt%iver)
   Case (9)
     Call read_unrf_v9 (afile, afield(1)%nv, n, afield(1)%rmiss, ka, v(:,:,1), nread, prd0, ifail)
   Case (10)
     Call read_nongrid_v10 (afile, afield, n, idom, ka, v, nread, prd0, ifail)
  End Select
  Close (iin)
!
! Errors
  Select Case (ifail)
   Case (:0)
     Return
   Case Default
     If (prd0%sdate%iyr > 0) Then
        cdate=get_cdate(prd0, 1, 2)
        If (nread > 0) Then
           im = magnitude(nread)
           Write (Unit=cfmt, Fmt='(A,I1,A)') '(3A,I', im, ',3A)'
           Write (Unit=cprog, Fmt=cfmt) &
              'Data up to ', Trim(cdate) ,' (', nread, ' ', Trim(ca_seqs_l(iuseq(afile%iseq))), &
              ' of training period) read successfully.'
        Else
           Write (Unit=cprog, Fmt='(3A)') &
              'Data up to ', Trim(cdate), ' read successfully.'
        End If
        Call cpt_error ('read_unrf', ifail, .true., &
             i_arg1=nread, c_arg1=Trim(afile%cfile%ffile), c_arg2=cprog)
     Else
        Call cpt_error ('read_unrf', ifail, .true., &
             c_arg1=Trim(afile%cfile%ffile))
     End If
  End Select
!
  Return
!
 Contains
!
!
  Subroutine read_unrf_v9 (afile, nv, n, rmiss, ka, v, nread, prd0, ifail)
!
! Reads formatted unreferenced data
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Aborted
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  Use data_numbers,        Only: one
  Use data_io_constants,   Only: ltag
  Use data_time_constants, Only: isq_yr
  Use time,                Only: get_pdate
  Use progress_meter,      Only: set_progress_increment, update_progress_meter
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nv ! - number of variables -
  Integer, Intent(In) :: n  ! - number of cases -
!
  Real(Kind=rp), Intent(In) :: rmiss ! - missing values -
!
  Type(ifile), Intent(In) :: afile ! - input file -
!
! Input/output scalars
  Type(pprd), Intent(InOut) :: prd0 ! - last successfully read period -
!
! Output scalars
  Integer, Intent(Out) :: nread ! - number of cases read successfully -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Input arrays
  Logical, Dimension(:,:), Intent(In) :: ka ! - cases available? -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: v ! - data -
!
! Locals
!
! Local scalars
  Integer :: i      ! - station index -
  Integer :: k      ! - time index -
  Integer :: irskip ! - number of records to skip -
  Integer :: icskip ! - number of initial spaces to pick -
!
  Character(Len=      ltag) :: ctag0 ! - previous tag line -
  Character(Len=      ltag) :: ctag1 ! - current tag line -
  Character(Len=afile%lmax) :: cline ! - line -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Real
  Intrinsic Trim
!
! Executable Statements
!
! Read variable names
  ifail = -1
  nread = 0
  ctag0 = 'None'
  Read (Unit=iin, Fmt=*, Err=1, End=2)
  If (update_progress_meter(.false.) /= 0) Return
!
! Skip first few records if required
  irskip = afile%it1 - 1
  If (irskip > 0) Then
     Do k = 1, irskip
        If (.not.ka(k,1)) Cycle
        Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag1
        ctag0 = ctag1
        If (update_progress_meter(.false.) /= 0) Return
     End Do
  End If
!
! Read data
  Call set_progress_increment (Real(nv, Kind=rp),.true.)
  Do k = 1, n
     If (ka(k,1)) Then
        If ((nread == 0) .or. ((afile%iseq /= isq_yr) .and. (afile%iseq /= 10))) Then
            Call parse_line (iin, cline, ifail, &
                 icskip)
            If (ifail /= 0) GoTo 3
        Else
            Call parse_line (iin, cline, ifail)
        End If
        Read (Unit=cline(icskip+1:), Fmt=*, Err=1) (v(i,k),i = 1, nv)
        ctag0 = ctag1
        nread = nread + 1
     Else
        v(1:nv,k) = rmiss
     End If
     If (update_progress_meter(.false.) /= 0) Return
  End Do
  Call set_progress_increment (one/Real(nv, Kind=rp), .true.)
!
! No errors
  ifail = 0
  Return
!
! Error reading file
1 ifail = 1
  GoTo 3
!
! End of file
2 ifail = 2
!
! Get latest date
3 If (ctag0(1:4) /= 'None') Then
     Call get_pdate (' ', Trim(ctag0), prd0%sdate, ifail, &
          edate=prd0%edate)
  End If
!
  Return
  End Subroutine read_unrf_v9
 End Subroutine read_unrf
!
!
!
 Subroutine read_nongrid_v10 (afile, afield, n, idom, ka, v, nread, prd0, ifail)
!
! Reads formatted station or unreferenced data
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Aborted
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  Use data_numbers,        Only: one
  Use data_io_constants,   Only: iin, ltag
  Use data_time_constants, Only: isq_yr
  Use time,                Only: pprd, &
                                 get_pdate
  Use progress_meter,      Only: set_progress_increment, update_progress_meter
  Use arrays,              Only: rwk
  Use iofiles,             Only: ifile
  Use fields,              Only: field
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: n ! - number of cases -
!
  Type(ifile), Intent(In) :: afile ! - input file -
!
! Input/output scalars
  Type(pprd), Intent(InOut) :: prd0 ! - last successfully read period -
!
! Output scalars
  Integer, Intent(Out) :: nread ! - number of cases read successfully -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Input arrays
  Integer, Dimension(:,:), Intent(In) :: idom ! - used gridpoints -
!
  Logical, Dimension(:,:), Intent(In) :: ka ! - cases available? -
!
  Type(field), Dimension(:), Intent(In) :: afield ! - fields -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: v ! - data -
!
! Locals
!
! Local scalars
  Integer :: i      ! - station/variable index -
  Integer :: k      ! - time index -
  Integer :: l      ! - field / lagged field index -
  Integer :: ifd    ! - field index -
  Integer :: ilf    ! - lagged-field index -
  Integer :: igf    ! - category field index -
  Integer :: ij     ! - available station index -
  Integer :: irskip ! - number of records to skip -
  Integer :: icskip ! - number of initial spaces to pick -
  Integer :: ierr   ! - error indicator -
!
  Character(Len=      ltag) :: ctag0 ! - previous tag line -
  Character(Len=      ltag) :: ctag1 ! - current tag line -
  Character(Len=afile%lmax) :: cline ! - line -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Index, Real
!
! Executable Statements
!
! Read v10 XML namespace headers and tags
  ifail = -1
  xml: Do
     Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag1
     If (Index(ctag1,'cpt:field=') > 0) Then
        Exit xml
     Else
        Cycle xml
     End If
  End Do xml
!
! Skip station/index names and tags
  nread = 0
  ctag0 = 'None'
  irskip = afile%it1 - 1
  Do ifd = 1, afile%nfs
     Do igf = 1, afile%ngs
        If ((ifd == 1) .and. (igf == 1)) Then
           Do k = 1, afile%ntag
              Read (Unit=iin, Fmt=*, Err=1, End=2) ctag1
           End Do
        Else
           Do k = 1, afile%ntag+1
              Read (Unit=iin, Fmt=*, Err=1, End=2) ctag1
           End Do
        End If
        ctag0 = ctag1
        If (update_progress_meter(.false.) /= 0) Return
!
! Skip first few records if required
        If (irskip > 0) Then
           Do k = 1, irskip
              Do ilf = 1, afile%nls
                 l = (ifd - 1)*afile%nls + ilf
                 If (.not.ka(k,l)) Cycle
                 Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag1
                 ctag0 = ctag1
              End Do
              If (update_progress_meter(.false.) /= 0) Return
           End Do
        End If
!
! Read data
        nread = 0
        Do k = 1, n
           ij = 0
           Do ilf = 1, afile%nls
              l = (ifd - 1)*afile%nls + ilf
!              If (l > 1) ij = Sum(afield(1:l-1)%region%nlts) $$$$ FTN95 BUG
If (l > 1) Then
   ij = 0
   Do i = 1, l-1
      ij = ij + afield(i)%region%nlts
   End Do
End If
              If (ka(irskip+k,l)) Then
                 If ((nread == 0) .or. (afile%nls>1) .or. ((afile%iseq /= isq_yr) .and. (afile%iseq /= 10))) Then
                     Call parse_line (iin, cline, ifail, &
                          icskip)
                     If (ifail /= 0) GoTo 3
                 Else
                     Call parse_line (iin, cline, ifail)
                 End If
                 Read (Unit=cline(icskip+1:), Fmt=*, Err=1) (rwk(i),i = 1, afield(l)%nlt)
!
! Extract stations within domain
                 Do i = 1, afield(l)%region%nlts
                    v(ij+i,k,igf) = rwk(idom(i,l))
                 End Do
                 ctag0 = cline
              Else
                 v(ij+1:ij+afield(l)%region%nlts,k,igf) = afield(l)%rmiss
              End If
              Call set_progress_increment (Real(afield(l)%nlt, Kind=rp), .true.)
              If (update_progress_meter(.false.) /= 0) Return
              Call set_progress_increment (Real(one/afield(l)%nlt, Kind=rp), .true.)
           End Do
           nread = nread + 1
        End Do
!
! Skip any additional data
        If (((ifd < afile%nfs) .or. (igf < afile%ngs)) .and. (irskip + n < afile%nt)) Then
           Do k = irskip+n+1, afile%nt
              Do ilf = 1, afile%nls
                 l = (ifd - 1)*afile%nls + ilf
                 If (.not.ka(k,l)) Cycle
                 Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag1
                 ctag0 = ctag1
              End Do
              If (update_progress_meter(.false.) /= 0) Return
           End Do
        End If
     End Do
  End Do
!
! No errors
  ifail = 0
  Return
!
! Error reading file
1 ifail = 1
  GoTo 3
!
! End of file
2 ifail = 2
!
! Get latest date
3 If (ctag0(1:4) /= 'None') Then
     Call get_pdate (' ', Trim(ctag0), prd0%sdate, ierr, &
          edate=prd0%edate)
  End If
!
  Return
 End Subroutine read_nongrid_v10
!
!
!
 Subroutine parse_line (iin, cline, ifail, &
            icskip, irskip, ka)
!
! Reads a line from an input file
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  Use data_numbers, Only: cdigits
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input unit number -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: irskip ! - number of lines to skip -
!
! Output scalars
  Integer, Intent(Out) :: ifail  ! - error indicator -
!
  Character(Len=*), Intent(Out) :: cline ! - line -
!
! - optional output scalars -
  Integer, Intent(Out), Optional :: icskip ! - number of initial spaces to skip -
!
! Input arrays
! - optional input arrays -
  Logical, Dimension(:), Intent(In), Optional :: ka ! - cases available? -
!
! Locals
!
! Local scalars
  Integer :: i1 ! - locator -
  Integer :: k  ! - case index -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Present, Scan, Verify
!
! Executable Statements
!
! Read first case
  ifail = 0
  Read (Unit=iin, Fmt='(A)', Err=1, End=2) cline
  If (.not.Present(icskip)) Return
  i1 = Scan(cline, cdigits//'/-T:')
  icskip = i1 + Verify(cline(i1:), cdigits//'/-T:') - 2
  If (Present(irskip)) Then
     Rewind (Unit=iin)
     Do k = 1, irskip+1
        If (ka(k)) Read (Unit=iin, Fmt=*)
     End Do
  End If
  Return
!
! Error reading file
1 ifail = 1
  Return
!
! End of file
2 ifail = 2
!
  Return
 End Subroutine parse_line
!
!
!
 Subroutine proj_read_data (iin, ianal, lb, nsn, ifail)
!
! Reads input data from project file 
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!    ifail =  4 Problem allocating memory
!
! Modules
  Use data_cpt_constants,  Only: ia_cca, ia_gcm, ia_pcr, ia_mlr, ia_pfv
  Use data_time_constants, Only: isq_sn, isq_so
  Use time,                Only: iseq
  Use arrays,              Only: x, y
  Use settings,            Only: mya, nct, nt, nx, ny
  Use iofiles,             Only: xfile
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin   ! - input file unit number -
  Integer, Intent(In) :: ianal ! - analysis identifier -
  Integer, Intent(In) :: lb    ! - lower bound -
  Integer, Intent(In) :: nsn   ! - number of seasons -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Read input data
  Select Case (ianal)
   Case (ia_cca, ia_pcr, ia_mlr)
     If (init_read(nx, ny, xfile%ngs, nt, nct, nt, &
                   lb=lb, ns1=nsn, ns2=nsn) /= 0) GoTo 4
   Case (ia_gcm)
     If (init_read(nx, ny, xfile%nem, nt, nct, nt, &
                   lb=lb, ns1=nsn, ns2=nsn) /= 0) GoTo 4
   Case (ia_pfv)
     Select Case (iseq)
      Case Default
        If (init_read(nx, ny, xfile%ngs, nt, nct, nt, &
                      lb=lb, ns1=1, ns2=nsn) /= 0) GoTo 4
      Case (isq_sn, isq_so)
        If (init_read(nx, mya, xfile%ngs, nt, nct, nt, &
                      lb=lb, ns1 = 1, ns2=nsn) /= 0) GoTo 4
     End Select
  End Select
  Read (Unit=iin, Err=1, End=2) x
  Read (Unit=iin, Err=1, End=2) y
!
  ifail = 0
  Return
!
! Errors in project file
! - problem reading file -
1 ifail = 1
  Return
!
! - end of file -
2 ifail = 2
  Return
!
! - memory allocation problem -
4 ifail = 4
  Return
!
 End Subroutine proj_read_data
!
!
!
 Subroutine proj_write_data (iout, ifail)
!
! Writes input data to project file
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem writing to file
!
! Modules
  Use arrays, Only: x, y
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iout ! - output file unit number -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write input data
  Write (Unit=iout, Err=1) x
  Write (Unit=iout, Err=1) y
!
  ifail = 0
  Return
!
! Errors in project file
! - problem writing to file -
1 ifail = 1
  Return
!
 End Subroutine proj_write_data
End Module cpt_input
