! Author: Simon Mason
Module cpt_file
!
! Declarations
!
! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: close_file, merge_files, input_files, update_file
#if GUI == 0
  Public :: get_file
#endif
!
! Scalars
!
! Integer scalars
  Integer, Public :: iclose ! - close file indicator -

!
Contains
!
!
 Function get_file()
!
! Opens an input file
!
! Modules
  Use data_cpt_constants, Only: c_file
#if GUI == 1
  Use gui,            Only: set_colour_direction
#endif 
  Use settings,       Only: iel
  Use iofiles,        Only: i_file
#if GUI == 1
  Use iofiles,        Only: zfile, &
                            file_refresh
  Use fields,         Only: yfield
#endif
  Use pcs,            Only: get_eofx_settings, get_eofy_settings
  Use get_input_file, Only: get_xfile, get_yfile, get_zfile
  Use forecast,       Only: set_fcst
!
! Function type
  Integer :: get_file
!
! Executable Statements
!
! Open input file
  Select Case (c_file(i_file))
! - X file -
   Case ('X')
     Select Case (iel)
      Case (1) ! - expert mode -
        get_file = get_infile('X', get_xfile, &
                   get_eof_settings=get_eofx_settings)
      Case Default ! - beginner mode -
        get_file = get_infile('X', get_xfile)
     End Select
! - Y file -
   Case ('Y')
     Select Case (iel)
      Case (1) ! - expert mode -
        get_file = get_infile('Y', get_yfile, &
                   get_eof_settings=get_eofy_settings)
      Case Default ! - beginner mode -
        get_file = get_infile('Y', get_yfile)
     End Select
#if GUI == 1
     Call set_colour_direction (yfield(1)%var) ! Set default colour direction
#endif
! - Z file -
   Case ('Z')
     get_file = get_zfile()
     get_file = set_fcst(get_file)
! - first file -
   Case ('1')
     get_file = get_infile('X', get_xfile)
! - second file -
   Case ('2')
     get_file = get_infile('Y', get_yfile)
! - update file -
   Case ('3')
     get_file = get_zfile()
#if GUI == 1
     If (get_file == 0) Call file_refresh (zfile)
#endif
     get_file = 2
  End Select
!
  Return
 End Function get_file
!
!
!
 Function get_infile(cxy, get_afile, &
          get_eof_settings)
!
! Opens an input file, and prompts for numbers of modes if appropriate
!
! Modules
#if GUI == 1
  Use gui,         Only: nclr
#endif
  Use gui,         Only: inew
  Use iofiles,     Only: xfile, yfile
  Use pcs,         Only: get_eofs_settings
  Use analysis,    Only: ianal
  Use cpt_actions, Only: reset
!
! Function type
  Integer :: get_infile
!
! Arguments
!
! Input scalars
  Character(Len=*), Intent(In) :: cxy ! - X/Y flag -
!
! Procedure arguments
  Integer, External :: get_afile
!
! - optional procedure arguments -
  Integer, External, Optional :: get_eof_settings
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Present
!
! Executable Statements
!
! Check for completed calculations
  get_infile = 2
#if GUI == 1
  nclr = 3
#endif
  If (reset('Opening a new '//cxy//' file', .true.) == 1) Return
!
! Open file
  If (get_afile() == 0) Then
     inew = 0 ! - indicate that a file is open -
!
! Prompt for mode and CCA mode settings
     If (Present(get_eof_settings)) Then
        If (xfile%cfile%lset .and. yfile%cfile%lset) Then
           get_infile = get_eofs_settings(cxy, ianal, .true., get_eof_settings)
!
! Prompt for mode settings
        Else
           get_infile = get_eofs_settings(cxy, ianal, .false., get_eof_settings)
        End If
     End If
  End If
!
  Return
 End Function get_infile
!
!
!
 Function merge_files()
!
! Merges two input files to produce a multi-field file
!
! Modules
  Use progress_meter, Only: lcalc, lstop, &
                            end_progress_meter, start_progress_meter
  Use gui,            Only: ih_xfd, ih_yfd, &
                            window_update
  Use errors,         Only: cpt_error
  Use settings,       Only: isynch, nmt, nt, nx, ny, &
                            set_options
  Use arrays,         Only: iusex, iusey
  Use iofiles,        Only: xfile, x2file, yfile, &
                            get_maxnt, get_new_file
  Use fields,         Only: xfield, yfield
  Use missing,        Only: kuse, &
                            init_missing
  Use climate,        Only: lcex
  Use cpt_input,      Only: get_data
  Use cpt_output,     Only: write_results
  Use cpt_actions,    Only: reset_mem
!
! Function type
  Integer :: merge_files
!
! Locals
!
! Local scalars
  Integer :: i     ! - index -
  Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Backup settings
  ifail = set_options(i1=nt, i2=isynch, l1=lcex) ! - backup settings -
  isynch = 1                                     ! - enable synchronous files -
!
! Prompt for files to merge
  xfile%cfile%desc = 'First file'
  yfile%cfile%desc = 'Second file'
1 Call get_2files ('Merge files', ih_xfd, xfile, '1', ih_yfd, yfile, '2', ifail)
  If (ifail /= 0) GoTo 2
!
! Check for consistency
! - data structure -
  If (xfile%idstr /= yfile%idstr) Then
     ifail = 1
     Call cpt_error ('merge_files', ifail, .false., &
          c_arg1=xfile%cgss, c_arg2=yfile%cgss)
     GoTo 1
  End If
! - sequencing -
  If (xfile%iseq /= yfile%iseq) Then
     ifail = 2
     Call cpt_error ('merge_files', ifail, .false., &
          i_arg1=xfile%iseq, i_arg2=yfile%iseq)
     GoTo 1
  End If
!
! Read data
  Call start_progress_meter ('Reading data', .false.)
  Call get_maxnt (0, 0, nt, nmt)        ! - check number of cases -
  lcex = .false.                        ! - switch off climatology extension -
  lcalc = .false.                       ! - switch off calculation flag -
  ifail = get_data(0, .false., .false.) ! - read data -
  Call end_progress_meter (.false., .false.)
  If ((ifail /= 0) .or. (lstop)) GoTo 2
!
! Prompt for output file
  x2file%cfile%fdir = xfile%cfile%fdir
  x2file%cfile%fname = ' '
  x2file%cfile%fext = xfile%cfile%fext
  x2file%cfile%ffmt = xfile%cfile%ffmt
  Call get_new_file ('Combined file', .true., x2file%cfile%ffile, x2file%cfile%fdir, x2file%cfile%fext, &
       (/'Data Files'/), (/'*.*'/), 1, x2file%cfile%fname, ifail)
  If (ifail /= 0) Then
     merge_files = reset_mem()
     GoTo 1
  End If
  x2file%cfile%lset = .true.
!
! Reset lagged fields to fields
  xfile%nls = 1
  xfile%nfs = xfile%nfl
  yfile%nls = 1
  yfile%nfs = yfile%nfl
!
! Save data
  merge_files = init_missing(nmt,nmt, .false.)
  xfield(:)%nva = xfield(:)%nv
  yfield(:)%nva = yfield(:)%nv
  iusex(:) = (/(i,i=1,nx)/)
  iusey(:) = (/(i,i=1,ny)/)
  kuse(:) = .true.
  Call write_results ()
!
! Restore settings
2 merge_files = reset_mem()
  iclose = 1
  merge_files = close_file()
  iclose = 2
  merge_files = close_file()
  merge_files = set_options(iw=0, i1=nt, i2=isynch, l1=lcex)
  Call window_update (nt)
!
  Return
 End Function merge_files
!
!
!
 Function update_file()
!
! Updates a file using data in a second file
!
! Modules
  Use data_numbers,   Only: rp, one
  Use time,           Only: Operator(<), Operator(>), &
                            date_diff
  Use progress_meter, Only: lstop, &
                            end_progress_meter, set_progress_increment, start_progress_meter
  Use gui,            Only: ih_xfd, ih_yfd, &
                            window_update
  Use errors,         Only: cpt_error
  Use settings,       Only: nmt, nf, nt, nx, &
                            set_it1, set_options
  Use arrays,         Only: iusex, x
  Use iofiles,        Only: xfile, xofile, zfile, &
                            get_new_file
  Use space,          Only: idomx
  Use fields,         Only: larea, xfield, zfield
  Use missing,        Only: kax, kaz, kuse, &
                            init_missing
  Use cpt_input,      Only: init_read, num_read, read_data
  Use cpt_output,     Only: write_results
  Use cpt_actions,    Only: reset_mem
!
! Function type
  Integer :: update_file
!
! Locals
!
! Local scalars
  Integer :: i     ! - index -
  Integer :: ifail ! - error indicator -
  Integer :: lgap  ! - gap between primary and update file -
  Integer :: nact  ! - number of lines to read -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Real,Sum
!
! Executable Statements
!
! Backup settings
  ifail = set_options(i1=nt, i2=nf) ! - backup settings -
!
! Prompt for files to update
  larea = .false.
  xfile%cfile%desc = 'Primary file'
  zfile%cfile%desc = 'Update file'
1 Call get_2files ('Update files', ih_xfd, xfile, '1', ih_yfd, zfile, '3', ifail, &
       iopen=xfile%igeog)
  If (ifail /= 0) GoTo 3
!
! Read data
! - check start-at dates -
  xfile%it1 = set_it1(xfile%iseq, xfile%prd1%sdate, xfile%fdate)
  zfile%it1 = set_it1(zfile%iseq, zfile%prd1%sdate, zfile%fdate)
  If (zfile%fdate < xfile%fdate) Then
     ifail = 1
     Call cpt_error ('update_file', ifail, .false.)
     GoTo 1
  End If
! - determine number of cases -
  lgap = date_diff(xfile%prdn%sdate, zfile%fdate, xfile%iseq) ! - identify any gaps -
  Select Case(lgap)
   Case (1) ! - update file immediately follows primary file -
     nt = date_diff(xfile%fdate, xfile%prdn%sdate, xfile%iseq) + 1
     nmt = nt + nf
     lgap = 0
   Case (:0) ! - overlap -
     nt = date_diff(xfile%fdate, zfile%fdate, xfile%iseq)
     nmt = nt + nf
     lgap = 0
   Case Default ! - gap -
     nt = date_diff(xfile%fdate, xfile%prdn%sdate, xfile%iseq) + 1
     nmt = nt + lgap + nf - 1
     x(:,nt+1:nt+lgap,:) = xfield(1)%rmiss
  End Select
  nf = date_diff(zfile%fdate, zfile%prdn%sdate, zfile%iseq) + 1
! - read primary data -
  nact=num_read(xfile%idstr, xfile%lstack, xfile%nt, nt, Sum(xfield(:)%nlt), xfile%nfs, xfile%nls, xfile%ngs, xfile%it1) + &
       num_read(zfile%idstr, zfile%lstack, zfile%nt, nf, Sum(zfield(:)%nlt), zfile%nfs, zfile%nls, zfile%ngs, zfile%it1)
  Call set_progress_increment (one/Real(nact+1, Kind=rp), .false.)
! - read primary data -
  Call start_progress_meter ('Reading data', .false.)
  If (init_read(nx, 0, 1, nmt, 0, 0) /= 0) Then
     ifail = 2
     Call cpt_error ('update_file', ifail, .false.)
     GoTo 3
  End If
  Call read_data (xfile, xfield, nt, idomx, kax, x, ifail)
  If (ifail>0) Then
     ifail = 3
     Call cpt_error ('update_file', ifail, .false., &
          c_arg1 = Trim(xfile%cfile%ffile))
     GoTo 3
  End If
! - read update data -
  nt = nt + lgap
  Call read_data (zfile, zfield, nf, idomx, kaz, x(:,nt+1:nt+nf,:), ifail)
  If (ifail > 0) Then
     ifail = 3
     Call cpt_error ('update_file', ifail, .false., &
          c_arg1=Trim(zfile%cfile%ffile))
     GoTo 3
  End If
  Call end_progress_meter (.false., .false.)
  If ((ifail /= 0) .or. (lstop)) GoTo 3
!
! Update X file
  If (zfile%prdn%sdate > xfile%prdn%sdate) Then
     xfile%prdn = zfile%prdn
     xfile%nt = nmt
  End If
!
! Prompt for output file
  xofile%cfile%ffile = xfile%cfile%ffile
  xofile%cfile%fdir = xfile%cfile%fdir
  xofile%cfile%fname = xfile%cfile%fname
  xofile%cfile%fext = xfile%cfile%fext
  xofile%cfile%ffmt = xfile%cfile%ffmt
  nt = nmt
2 Call get_new_file ('Updated file', .true., xofile%cfile%ffile, xofile%cfile%fdir, xofile%cfile%fext, &
       (/'Data Files'/), (/'*.*'/), 1, xofile%cfile%fname,ifail)
  If (ifail /= 0) GoTo 2
  xofile%cfile%lset = .true.
!
! Save data
  update_file=init_missing(nmt ,nmt, .false.)
  iusex(:) = (/(i,i=1,nx)/)
  kuse(:) = .true.
  Call write_results ()
!
! Restore settings
3 update_file = reset_mem()
  iclose = 1
  update_file = close_file()
  iclose = 3
  update_file = close_file()
  update_file = set_options(iw=0, i1=nt, i2=nf)
  Call window_update (nt)
  Call window_update (nf)
  larea = .true.
!
  Return
 End Function update_file
!
!
!
 Subroutine get_2files (ctitle, ih1, file1, cfile1, ih2, file2, cfile2, ifail, &
            iopen)
!
! Prompts for two input files
!
! Modules
  Use data_gui_constants, Only: i_handle
  Use labels,             Only: cg_infiles_t
#if GUI == 1
  Use gui,                Only: iw, &
                                win_prompt
#endif
  Use gui,                Only: box_close, box_open, init_win
  Use iofiles,            Only: ifile
!
! Arguments
!
! Input scalars
  Character(Len=*), Intent(In) :: ctitle ! - title -
  Character(Len=*), Intent(In) :: cfile1 ! - first file -
  Character(Len=*), Intent(In) :: cfile2 ! - second file -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: iopen ! - ready for opening flag -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: file1 ! - first file -
  Type(ifile), Intent(InOut) :: file2 ! - second file -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
  Integer(Kind=i_handle), Intent(Out) :: ih1 ! - handle for first file -
  Integer(Kind=i_handle), Intent(Out) :: ih2 ! - handle for second file -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Prompt for input files
1 Call init_win (title=ctitle)
  Call box_open (.true., .false., &
       title=Trim(cg_infiles_t), c_opts='bottom_exit')
! - first input file -
  Call box_open (.false., .false.)
  Call input_files (cfile1, Trim(file1%cfile%desc), file1, &
       ih=ih1)
  Call box_close (.false.)
! - second input file -
  Call box_open (.false., .false.)
  Call input_files (cfile2, Trim(file2%cfile%desc), file2, &
       ih=ih2, iopen=iopen)
  Call box_close (.false.)
! - close input box -
  Call box_close (.true.)
! - prompt -
#if GUI == 1
  iw = win_prompt(.true.)
  If (iw == 3) Then
     If ((.not.file1%cfile%lset) .or. (.not.file2%cfile%lset)) GoTo 1
#endif
     file2%bdate=file2%fdate
     ifail = 0
#if GUI == 1
  Else
     ifail = 1
  End If 
#endif
!
  Return
 End Subroutine get_2files
!
!
!
 Subroutine input_files (cxyz, cdesc, afile, &
            iopen, ih, nv, nva, cprd1, cprdn, fcb1, fcb2, fcb3)
!
! Displays CPT input file information
!
! Modules
#if GUI == 1
  Use data_numbers,       Only: one
#endif
  Use data_cpt_constants, Only: c_file, m_file
  Use data_gui_constants, Only: i_handle
  Use time,               Only: iseq
  Use labels,             Only: cg_data1, cg_datan, cg_startat
#if GUI == 1
  Use labels,             Only: cg_browse_l, cg_fields_l, cg_filename, cg_nof, cg_nused, &
                                cls_l
  Use screen,             Only: dts_adj
  Use gui,                Only: iwdth, &
                                add_button, adjust_y, box_open, gui_creturn, print_ivalue
#endif
  Use gui,                Only: box_close, print_cvalue, prompt_pdate
  Use iofiles,            Only: ifile, &
                                adjust_mfdate
#if GUI == 1
  Use iofiles,            Only: i_file
#endif
!
! Arguments
!
! Input scalars
  Character(Len=*), Intent(In) :: cxyz  ! - X, Y, or Z file / first or second input file -
  Character(Len=*), Intent(In) :: cdesc ! - file decription -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: iopen ! - ready for opening flag -
  Integer, Intent(In), Optional :: nv    ! - total number of variables -
  Integer, Intent(In), Optional :: nva   ! - number of used variables -
!
  Character(Len=*), Intent(In), Optional :: cprd1 ! - first period available -
  Character(Len=*), Intent(In), Optional :: cprdn ! - last period available -
!
! Input/output scalars
  Type(ifile), Intent(InOut) :: afile ! - input file -
!
! - optional input/output scalars -
  Integer(Kind=i_handle), Intent(InOut), Optional :: ih ! - control handle -
!
! Procedure arguments
! - optional procedure arguments -
  Integer, External, Optional :: fcb1
  Integer, External, Optional :: fcb2
  Integer, External, Optional :: fcb3
!
! Locals
!
! Local scalars
  Integer :: itab  ! - tab position -
  Integer :: i_fid ! - input file ID -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Present, Trim
#if GUI == 1
  Intrinsic Min, Nint
#endif
!
! Executable Statements
!
! Identify input file type
  Do i_fid = 1, m_file
     If (cxyz == c_file(i_fid)) Exit
  End Do
!
#if GUI == 1
! Prompt for input file settings
  Call box_open (.true., .false., &
       title=cdesc)
! - prompt for file -
  If (dts_adj > one) Call adjust_y (dts_adj-one)
  Call add_button ('tt', 1, Trim(cg_browse_l), &
       iwdth=5, igrey=iopen, iset1=i_file, ival1=i_fid, cbf=get_file) 
  Call gui_creturn (.false.)
  itab=12
  Call print_cvalue (Trim(cg_filename), afile%cfile%fname, &
       itab=itab, iwid=20, ldepth=.false.)
#else
  ih = get_file()
  itab = 20
#endif
! - print date of first data -
  If (Present(cprd1)) Then
     Call print_cvalue (Trim(cg_data1), cprd1, &
          itab=itab, iwid=20)
  Else
     Call print_cvalue (Trim(cg_data1), afile%cprd1, &
          itab=itab, iwid=20)
  End If
! - print date of last data -
  If (Present(cprdn)) Then
     Call print_cvalue (Trim(cg_datan), cprdn, &
          itab=itab, iwid=20)
  Else
     Call print_cvalue (Trim(cg_datan), afile%cprdn, &
          itab=itab, iwid=20)
  End If
! - prompt for start date -
  Select Case (cxyz)
   Case ('X', 'x')
     Call prompt_pdate (Trim(cg_startat), iseq, afile%fdate, &
          itab=itab, lbold=.true., modify1=fcb1, freset=fcb2, ih=ih)
   Case ('Y', 'y')
     Call prompt_pdate (Trim(cg_startat), iseq, afile%fdate, &
          itab=itab, lbold=.true., modify1=fcb1, modify2=adjust_mfdate, freset=fcb2, ih=ih)
   Case ('Z', 'z')
     Call prompt_pdate (Trim(cg_startat), iseq, afile%fdate, &
          itab=itab, lbold=.true., modify1=fcb3)
   Case ('1', '2', '3')
#if GUI == 1
     Call prompt_pdate (Trim(cg_startat), afile%iseq, afile%fdate, &
          itab=itab, lbold=.true., modify1=fcb1)
#else
     Call prompt_pdate (Trim(cg_startat), afile%iseq, afile%fdate, &
          itab=itab, lbold=.true., dlimits=(/afile%prd1%sdate, afile%prdn%sdate/))
#endif
  End Select
#if GUI == 1
! - print numbers of fields, lagged fields, variables, and used variables -
  itab = Min(Nint(0.03*Real(iwdth)), 35)
  Call print_ivalue (Trim(cg_nof)//' '//Trim(cg_fields_l), afile%nfs, &
       itab=itab)
  Call print_ivalue (Trim(cg_nof), afile%nfs, &
       cqual=cls_l, itab=itab)
  If (Present(nv)) Call print_ivalue (Trim(cg_nof), nv, &
                        cqual=afile%cgss, itab=itab)
  If (Present(nva)) Call print_ivalue (Trim(cg_nused), nva, &
                         itab=itab)
#endif
  Call box_close (.true.)
!
  Return
 End Subroutine input_files
!
!
!
 Function close_file()
!
! Closes an input file
!
! Modules
  Use time,        Only: iseq, &
                         reset_time
  Use gui,         Only: ih_xfd, ih_yfd, ih_zfd, inew, &
                         set_viewflags, window_update
  Use settings,    Only: iel, igcms, istd, isynch, mxa, mya, mza, nf, nx, ny, nz
  Use iofiles,     Only: mfile, xfile, yfile, zfile, &
                         close_ifile
  Use fields,      Only: nmfd
  Use season,      Only: icsv, &
                         close_season
  Use analysis,    Only: ianal, iaction, icalc
  Use forecast,    Only: ifc, ifcast, &
                         close_fcast
  Use cpt_actions, Only: reset
!
! Function type
  Integer :: close_file
!
! Locals
!
! Local scalars
  Integer :: ireset ! - reset flag -
!
! Executable Statements
!
! Close input file
  close_file = 1
1 Select Case (iclose)
! - X file -
   Case (1)
     If (icalc == 1) Then
        ireset = reset('Closing the X file', .true.)
        If (ireset == 1) Return
     End If
     Call reset_time ()
     Call close_season ()
     Call close_ifile (xfile, nx, &
          ih=ih_xfd, nva=mxa)
     iaction = 0
     iclose = 3
     If (yfile%cfile%lset) Then ! - restore monthly file settings -
        yfile%cprd1 = mfile%cprd1
        yfile%cprdn = mfile%cprdn
        yfile%cssn = mfile%cssn
        yfile%iseq = mfile%iseq
        yfile%prd1 = mfile%prd1
        yfile%prdn = mfile%prdn
        iseq = mfile%iseq
     End If
     Call window_update (nx)
     Call window_update (mxa)
     GoTo 1
! - Y file -
   Case (2)
     If (icalc == 1) Then
        ireset = reset('Closing the Y file', .true.)
        If (ireset == 1) Return
     End If
     Call close_season ()
     Call close_ifile (yfile, ny, &
          ih=ih_yfd, nva=mya)
     Call close_ifile (mfile, ny)
     iaction = 0
     Call window_update (ny)
     Call window_update (mya)
     icsv = 0
     nmfd = 1
! - forecast file -
   Case (3)
     ifc = close_fcast()
     ifcast = 0
     Call close_ifile (zfile, nz, &
          ih=ih_zfd, nva=mza)
     nf = 0
     Call window_update (nf)
     Call window_update (mza)
  End Select
  iclose = 0
!
! Enable analyses
  Call set_viewflags (ianal, istd, xfile%idstr, igcms, icsv, iel, isynch)
!
! Check whether all files are closed
  If (xfile%cfile%lset .or. yfile%cfile%lset) Then
     inew = 0
  Else
     inew = 1
  End If
!
  Return
 End Function close_file
End Module cpt_file
