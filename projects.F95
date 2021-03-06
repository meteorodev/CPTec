! Author: Simon Mason
Module projects
!
! Modules
  Use data_cpt_constants, Only: na
!
! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: close_project, get_project_startup, new_project, open_project, save_project, saveas_project, &
            reset_project
!
! Derived type definitions
!
! - project files -
  Public :: proj
  Type proj
     Sequence
     Character(Len=186) :: pfile ! - file -
     Character(Len=150) :: pdir  ! - file directory -
     Character(Len= 36) :: pname ! - file name -
!
     Logical :: lproj            ! - file set? -
  End Type proj
!
! Arrays
!
! Character arrays
  Character(Len= 21) :: pdescs(na) ! - project file descriptions -
  Character(Len=  5) :: pfilts(na) ! - project filters -
!
! Scalars
!
! Character scalars
  Integer, Public :: ianalp ! - project analysis type -
!
! Character scalars
  Character(Len=150), Public :: p_dir    ! - project file directory -
  Character(Len=150), Public :: pdir     ! - default project file directory -
  Character(Len=150), Public :: pdir_old ! - old project file directory -
!
! Derived type scalars
  Type(proj), Public :: this_proj ! - current project name -
  Type(proj), Public :: new_proj  ! - new project file -
!
Contains
!
!
 Subroutine init_projects (lnew)
!
! Initialises project file
!
! Modules
  Use labels, Only: ca_atypes_a
!
! Arguments
!
! Input scalars
  Logical, Intent(In) :: lnew ! - reset new project? -
!
! Locals
!
! Local scalars
  Integer :: i ! - project type index -
!
! Executable Statements
!
! Initialise project file settings
! - generic project -
  pdescs(1) = 'CPT Project File'
  pfilts(1) = '*.CPT'
! - other projects -
  Do i = 1, na-1
     pdescs(i+1) = 'Old '//ca_atypes_a(i)//' Project Files'
     pfilts(i+1) = '*.'//ca_atypes_a(i)
  End Do
!
! Initialise current project
  Call reset_project (this_proj)
  this_proj%pdir = pdir
  If (lnew) Then
     Call reset_project (new_proj)
     new_proj%pdir = pdir
  End If
!
  Return
 End Subroutine init_projects
!
!
!
 Subroutine get_project_startup ()
!
! Identifiers project as a command-line argument
!
! Modules
  Use iofiles, Only: parse_file
!
! Locals
!
! Local scalars
  Integer :: ifail ! - error indicator -
!
  Character(Len=8) :: cdum ! - dummy argument to remove a possible project file extension -
!
  Logical :: le ! - file exists? -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Get_Command_Argument, Len_Trim
!
! Executable Statements
!
! Check for comman-line argument
  new_proj%pfile = ' '
  Call Get_Command_Argument (1, &
       value=new_proj%pfile)
  If (Len_Trim(new_proj%pfile) > 0) Then
!
! Identifier whether argument is an existing project file
     Inquire (File=new_proj%pfile, Exist=le)
     If (.not.le) Goto 1
     Call parse_file (new_proj%pfile, new_proj%pdir, new_proj%pname, ifail, &
          fext_new=cdum)
     If (ifail /= 0) Goto 1
!
! Attempt to read project file
     If (get_project() /= 0) GoTo 1
  Endif
  Call init_projects (.false.)
  Return
!
! Errors
1 Call init_projects (.true.)
  Return
 End Subroutine get_project_startup
!
!
!
 Function open_project()
!
! Opens a project
!
! Modules
#if GUI == 1
  Use clrwin$,             Only: clear_window$
#endif
  Use data_numbers,        Only: one
  Use data_cpt_constants,  Only: na, na10
#if GUI == 1
  Use gui,                 Only: ih_con
#endif
  Use gui,                 Only: set_cursor_waiting, set_win_title, window_update
  Use progress_meter,      Only: set_progress_meter
  Use settings,            Only: isave, &
                                 nx, ny, &
                                 training_refresh
  Use iofiles,             Only: fext, xfile, yfile, &
                                 files_refresh, get_old_file
  Use analysis,            Only: ianal, icalc
  Use cpt_actions,         Only: reset
!
! Function type
  Integer :: open_project
!
! Locals
!
! Local scalars
  Integer :: ifail    ! - error indicator -
  Integer :: icalc_bk ! - backup calculation flag -
!
  Logical :: lts ! - title screen? -
!
! Executable Statements
!
! Copy settings
  open_project = 1
  ianalp = ianal
  Select Case (ianal)
   Case (1:na)
     lts = .false.
     If (isave >= 2) isave = isave + 2
     icalc_bk = icalc
     If (close_project() == 1) Return
     icalc = icalc_bk
     open_project = reset(' ', .true.)
     icalc = 0
   Case Default
     lts = .true.
     open_project = 0
  End Select
  Call reset_project (new_proj)
!
! Select project file
  open_project = 2
  Select Case (ianal)
   Case (1:na10)
     Call get_old_file (pdescs(1), new_proj%pfile, new_proj%pdir, pdescs(1:ianal+1:ianal), pfilts(1:ianal+1:ianal), 2, &
          new_proj%pname, ifail, fext=fext)
   Case (na10+1:na)
     Call get_old_file (pdescs(1), new_proj%pfile, new_proj%pdir, (/pdescs(1)/), (/pfilts(1)/), 1, &
          new_proj%pname, ifail, fext=fext)
   Case Default
     Call get_old_file (pdescs(1), new_proj%pfile, new_proj%pdir, pdescs, pfilts, na10+1, &
          new_proj%pname, ifail, fext=fext)
  End Select
  If (ifail /= 0) Return
!
! Open project file
  If (get_project() /= 0) Return
!
! Update window
  Call files_refresh ()
  Call training_refresh ()
Call files_refresh () ! - duplicate call for FTN95 bug -
  If (icalc == 1) Call set_progress_meter (one)
  If (lts) Then
     open_project = 0
  Else
#if GUI == 1
!     Call clear_window$ (ih_con)
#endif
     Print *, ' '
!
! Indicate numbers of variables
     If (xfile%cfile%lset) Call window_update (nx)
     If (yfile%cfile%lset) Call window_update (ny)
     open_project = 1
  End If
  Call window_update (this_proj%pname)
!
! Update CPT window title
  Call set_win_title (ianal, &
       cproj=this_proj%pname)
  Call set_cursor_waiting (0)
!
  Return
 End Function open_project
!
!
!
 Function get_project()
!
! Opens a project
!
! Modules
  Use data_numbers,        Only: sp
  Use data_cpt_constants,  Only: ia_mlr, ia_gcm, ia_pfv
  Use data_io_constants,   Only: iin, lfli
  Use data_version,        Only: ver
  Use time,                Only: iseq
  Use labels,              Only: ca_dstruct
  Use gui,                 Only: inew, &
                                 set_cursor_waiting, set_viewflags, set_win_title
  Use errors,              Only: cpt_error
  Use settings,            Only: iel, igauss, igauss_bk, igcms, igcms_bk, iretro, isave, istd, istd_bk, isynch, &
                                 nt, nt_old, nx, nz, xfd_old, yfd_old
  Use iofiles,             Only: xfile, yfile, zfile, &
                                 open_iofile
  Use fields,              Only: dsdy
  Use pcs,                 Only: ieofx, ieofx_bk, &
                                 set_scree
  Use categories,          Only: ithr, ithr_bk
  Use climate,             Only: iclim
  Use season,              Only: icsv
  Use analysis,            Only: iaction, ianal, icalc, &
                                 analysis_flags
  Use cpt_actions,         Only: reset_mem
!
! Function type
  Integer :: get_project
!
! Locals
!
! Local scalars
  Integer :: ifail ! - error indicator -
!
  Real(Kind=sp) :: pver ! - project version number -
!
  Character(Len=24) :: cfail ! - project file read location error -
!
! Executable Statements
!
! Open project file
  get_project = 1
  Call set_cursor_waiting (1)
  Call open_iofile (iin, new_proj%pfile, .true., .false., .false., ifail)
  If (ifail /= 0) Then
     Call cpt_error ('open_iofile', ifail, .false., &
          c_arg1=new_proj%pfile)
     Return
  End If
!
! Read version number
  pver = 0.0_sp
  cfail = ' '
  Read (Unit=iin, Err=1, End=2) pver
  If (pver > ver) Then           ! - project files are newer than CPT -
     ifail = 2
     GoTo 3
  Else If (pver >= 13.0_sp) Then ! - version 13+ files -
     isave = 0
     ifail = read_project_v13(pver,cfail)
  Else                           ! - old files -
     ifail = 7
     cfail = ' '
     GoTo 3
  End If
  Select Case (ifail)
   Case (0)
     If (icalc == 1) Then
        Call analysis_flags ('end')
     Else
        Call analysis_flags ('off')
     End If
   Case (3)
     Call analysis_flags ('off')
     get_project = reset_mem()
     GoTo 3
   Case (4)
     Call analysis_flags ('off')
     get_project = reset_mem()
     ifail = 0
   Case (5)
     Call analysis_flags ('off')
     get_project = reset_mem()
     ifail = 8
     GoTo 3
   Case Default
     Call analysis_flags ('off')
     get_project = reset_mem()
     ifail = ifail + 2
     GoTo 3
  End Select
  Close (Unit=iin)
!
! Force file width to be non-zero
  If (pver < 14.0_sp) Then
     If (xfile%cfile%lset) xfile%lmax = lfli
     If (yfile%cfile%lset) yfile%lmax = lfli
  End If
!
! Reset analysis type if necessary
  ianal = ianalp
!
! Set backups
  If (zfile%cfile%lset) Then
     nz = nx
  Else
     nz = 0
  End If
  nt_old = nt
  xfd_old = xfile%fdate%iyr
  yfd_old = yfile%fdate%iyr
  Select Case (ianal)
   Case (ia_mlr)
     ieofx_bk = ieofx
   Case (ia_gcm)
     igauss_bk = igauss
     igcms_bk = igcms
   Case (ia_pfv)
     igauss_bk = igauss
     istd_bk = istd
     ithr_bk = ithr
  End Select
!
! Update settings
  Call set_viewflags (ianal, istd, xfile%idstr, igcms, icsv, iel, isynch, &
                      iretro=iretro, f_scree=set_scree)
  new_proj%lproj = .true.
  this_proj = new_proj
  If (yfile%cfile%lset) Then
     dsdy = ca_dstruct(yfile%idstr)
     iclim = 1
     If (xfile%cfile%lset) Then
        iaction = 1
     Else
        iaction = 0
     End If
     iseq = yfile%iseq
     inew = 0
  Else
     iaction = 0
     If (xfile%cfile%lset) Then
        iseq = xfile%iseq
        inew = 0
     Else
        inew = 1
     End If
  End If
!
! Update CPT window title
  Call set_win_title (ianal, &
       cproj=this_proj%pname)
  Call set_cursor_waiting (0)
  get_project = 0
!
  Return
!
! Errors in project file
1 ifail = 3
  GoTo 3
!
2 ifail = 4
!
3 Close (Unit=iin)
  Call cpt_error ('get_project', ifail, .false., &
       r_arg1=pver, c_arg1=cfail)
!
! Reset
  If (pver < 11.0_sp) Then
     xfile%cfile%lset = .false.
     yfile%cfile%lset = .false.
     zfile%cfile%lset = .false.
     iaction = 0
     iclim = 0
  End If
  get_project = new_project()
!
  Return
 End Function get_project
!
!
!
 Function read_project_v13(pver, cfail) &
          Result (read_project)
!
! Reads a version 13 project
!
! Modules
  Use data_numbers,       Only: sp
  Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv, ng
  Use data_io_constants,  Only: iin
  Use time,               Only: nss
  Use gui,                Only: ihind
#if GUI == 1
  Use gui,                Only: set_colour_direction
#endif
  Use screen,             Only: proj_read_screen
  Use arrays,             Only: proj_read_results
  Use settings,           Only: igauss, iretro, nret, nt, nur, &
                                proj_read_settings
  Use distribs,           Only: proj_read_distribs
  Use iofiles,            Only: xfile, yfile, zfile, &
                                proj_read_iofiles
  Use space,              Only: proj_read_interp, proj_read_space
  Use fields,             Only: xfield, yfield, &
                                proj_read_fields
  Use pcs,                Only: nxo, nyo, mcc, &
                                proj_read_pcs
  Use missing,            Only: xmiss, ymiss, &
                                proj_read_missing1, proj_read_missing2
  Use categories,         Only: proj_read_categories
  Use climate,            Only: lcex, &
                                init_climate, proj_read_climate
  Use season,             Only: lb, lprst, lsn, nsn, &
                                proj_read_season
  Use analysis,           Only: icalc, nopt, &
                                init_analysis1, init_analysis2, init_analysis_pfv, init_results, proj_read_analysis
  Use markers,            Only: proj_read_markers
  Use cpt_input,          Only: proj_read_data
  Use bootstrap,          Only: proj_read_bootstrap
  Use forecast,           Only: proj_read_forecasts
  Use verif,              Only: proj_read_verif
!
! Function type
  Integer :: read_project
!
! Arguments
!
! Input scalars
  Real(Kind=sp), Intent(In) :: pver ! - project version number -
!
! Output scalars
  Character(Len=*), Intent(Out) :: cfail ! - project file read location error -
!
! Locals
!
! Local scalars
  Integer :: mlt    ! - maximum number of latitudes -
  Integer :: mlg    ! - maximum number of longitudes -
  Integer :: nur_bk ! - backup number -
  Integer :: ifail  ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Max, MaxVal
!
! Executable Statements
!
! Read analysis type
  cfail = ' '
  Call proj_read_analysis (iin, ianalp, pver, .false., ifail)
  If (ifail /= 0) Then
     cfail = 'analysis settings'
     GoTo 1
  End If
!
! Read program settings
  Call proj_read_settings (iin, icalc, pver, ifail)
  If (ifail /= 0) Then
     cfail = 'program settings'
     GoTo 1
  End If

!
! Read bootstrap settings
  Call proj_read_bootstrap (iin, ifail)
  If (ifail /= 0) Then
     cfail = 'bootstrap settings'
     GoTo 1
  End If
!
! Read input file settings
  Call proj_read_iofiles (iin, pver, ifail)
  If (ifail /= 0) Then
     cfail = 'input file settings'
     GoTo 1
  End If
!
! Read season settings
  Call proj_read_season (iin, pver, ifail)
  If (ifail /= 0) Then
     cfail = 'season settings'
     GoTo 1
  End If
!
! Initialise and read field settings
  Call proj_read_fields (iin, (ianalp==ia_pfv), xfile%nfl, yfile%nfl*nsn, zfile%nfl, xmiss, ymiss, xmiss, pver, ifail)
  If (ifail /= 0) Then
     cfail = 'field settings'
     GoTo 1
  End If
#if GUI == 1
! - restore colour direction -
  If (yfile%cfile%lset) Call set_colour_direction (yfield(1)%var)
#endif
!
! Initialise and read domain settings
  Call proj_read_space (iin, xfile%cfile%lset, xfile%idstr, xfile%igeog, xfile%nfl, xfield(:)%nlt, xfield(:)%nlg, &
                             yfile%cfile%lset, yfile%idstr, yfile%igeog, yfile%nfl, yfield(:)%nlt, yfield(:)%nlg, &
       ifail)
  If (ifail /= 0) Then
     cfail = 'domain settings'
     GoTo 1
  End If
!
! Initialise and read missing value settings
  Call proj_read_missing1 (iin, nss, xfile%cfile%lset, xfile%nt, xfile%nfl, &
                                     yfile%cfile%lset, yfile%nt, yfile%nfl, &
                                     zfile%cfile%lset, zfile%nt, zfile%nfl, ifail)
  If (ifail /= 0) Then
     cfail = 'missing value settings'
     GoTo 1
  End If
!
! Read climatology settings
  Call proj_read_climate (iin, nt, lsn(0), .false., ifail)
  If (ifail /= 0) Then
     cfail='climatology settings'
     GoTo 1
  End If
!
! Read category settings
  Call proj_read_categories (iin, 0, 0, nsn, .false., ifail)
  If (ifail /= 0) Then
     cfail = 'category settings'
     GoTo 1
  End If
!
! Read GUI settings
  Call proj_read_pcs (iin, ianalp, icalc, .false., ifail)
  If (ifail /= 0) Then
     cfail = 'GUI settings'
     GoTo 1
  End If
!
! Read verification settings
  Call proj_read_verif (iin, ifail)
  If (ifail /= 0) Then
     cfail = 'verification settings'
     GoTo 1
  End If
!
! Read screen settings
  Call proj_read_screen (iin, pver, ifail)
  If (ifail /= 0) Then
     cfail = 'screen settings'
     GoTo 1
  End If
!
! Read marker settings
  Call proj_read_markers (iin, pver, ifail)
  If (ifail /= 0) Then
     cfail = 'marker settings'
     GoTo 1
  End If
!
! Read input data
  If (icalc == 1) Then
     Call proj_read_data (iin, ianalp, lb, nss, ifail)
     If (ifail /= 0) Then
        cfail = 'input data'
        GoTo 4
     End If
!
! Read missing value indicators
     Call proj_read_missing2 (iin, lcex, ifail)
     If (ifail /= 0) Then
        cfail = 'missing value indicators'
        GoTo 4
     End If
!
! Allocate memory
     If (.not.lprst) Then
        Call init_climate (1, nsn, ifail)
     Else
        Call init_climate (0, nsn, ifail)
     End If
     If (ifail /= 0) GoTo 4
     Select Case (ianalp)
      Case (ia_cca) ! - CCA -
        Call init_analysis1 (ianalp, .true., ng, nret, nopt, ifail)
        If (ifail /= 0) GoTo 4
        Call init_analysis2 (ifail, &
             nco=Min(nxo, nyo, mcc))
        If (ifail /= 0) GoTo 4
        ihind = 1
      Case (ia_pcr, ia_mlr) ! - GUIR, MLR -
        Call init_analysis1 (ianalp, .true., ng, nret, nopt, ifail)
        If (ifail /= 0) GoTo 4
        Call init_analysis2 (ifail)
        If (ifail /= 0) GoTo 4
        ihind = 1
      Case (ia_gcm) ! - GCM -
        Select Case (yfile%idstr)
         Case (1)
           mlt = MaxVal(yfield(:)%region%nlts)
           mlg = MaxVal(yfield(:)%region%nlgs)
         Case (ia_pfv)
           mlt = MaxVal(yfield(:)%nva)
           mlg = MaxVal(yfield(:)%nva)
        End Select
        Call init_analysis1 (ianalp, .true., ng, 0, nopt, ifail, &
             nlt=mlt, nlg=mlg, ndom=xfile%nms)
        If (ifail /= 0) GoTo 4
        Call init_analysis2 (ifail)
        If (ifail /= 0) GoTo 4
        ihind = 1
      Case (ia_pfv) ! - PFV -
        nur_bk = nur
        Select Case (yfile%idstr)
         Case (1)
           Call init_analysis_pfv (ng, nt, MaxVal(yfield(:)%region%nlts), MaxVal(yfield(:)%region%nlgs), yfile%nfl, ifail)
         Case Default
           Call init_analysis_pfv (ng, nt, MaxVal(yfield(:)%nva), MaxVal(yfield(:)%nva), yfile%nfl, ifail)
        End Select
        If (ifail /= 0) GoTo 4
        ihind = 0
     End Select
     Call init_results (ianalp, Max(MaxVal(xfield(:)%nlt), MaxVal(yfield(:)%nlt)), &
          Max(MaxVal(xfield(:)%nlg), MaxVal(yfield(:)%nlg)), ifail)
     If (ifail /= 0) GoTo 4
!
! Read climatological data
     Call proj_read_climate (iin, nt, lsn(0), .true., ifail)
     If (ifail /= 0) Then
        cfail = 'climatological data'
        GoTo 4
     End If
!
! Read category data
     Call proj_read_categories (iin, ihind, iretro, nsn, .true., ifail)
     If (ifail /= 0) Then
        cfail = 'categories'
        GoTo 4
     End If
!
! Read distribution parameters
     If (igauss == 2) Then
        Call proj_read_distribs (iin, pver, ifail)
        If (ifail /= 0) Then
           cfail = 'distribution parameters'
           GoTo 4
        End If
     End If
!
! Read GUI results
     Select Case (ianalp)
      Case (ia_cca, ia_pcr, ia_mlr)
        Call proj_read_pcs (iin, ianalp, icalc, .true., ifail)
        If (ifail /= 0) Then
           cfail = 'GUI results'
           GoTo 4
        End If
!
! Read interpolation settings
      Case (ia_gcm, ia_pfv)
        If (pver < 14.03_sp) Then
           Read (Unit=iin, Fmt=*, Err=1, End=2)
           Read (Unit=iin, Fmt=*, Err=1, End=2)
        Else If (pver > 15.06_sp) Then
           Call proj_read_interp (iin, ifail)
           If (ifail /= 0) Then
              cfail = 'interpolation settings'
              GoTo 1
           End If
        End If
     End Select
!
! Read optimization results
     If (ianalp /= ia_pfv) Then
        Call proj_read_analysis (iin, ianalp, pver, .true., ifail)
        If (ifail /= 0) Then
           cfail = 'optimization results'
           GoTo 1
        End If
     End If
!
! Read results
     Call proj_read_results (iin, ianalp, igauss, ihind, iretro, ifail)
     If (ifail /= 0) Then
        cfail = 'results'
        GoTo 4
     End If
     Call proj_read_forecasts (iin, ianalp, igauss, ihind, pver, ifail)
     If (ifail /= 0) Then
        cfail = 'forecasts'
        GoTo 5
     End If
     If (ianalp == ia_pfv) nur = nur_bk
  End If
!
  read_project = 0
  Return
!
! Errors in project file
! - problems initializing memory -
1 read_project = ifail
  Return
!
! - end of file -
2 read_project = 2
  Return
!
! - problem initializing memory for results -
4 read_project = 4
  Return
!
! - problem initializing memory for forecasts -
5 read_project = 0
  Return
!
 End Function read_project_v13
!
!
!
 Function save_project()
!
! Determines whether to save or save-as a project
!
! Modules
  Use settings, Only: isave
!
! Function type
  Integer :: save_project
!
! Executable Statements
!
! Check whether project is active
  If (this_proj%lproj) Then
     If (isave > 0) save_project=psave()
  Else If (isave == -1) Then
     save_project = psave()
  Else
     save_project = saveas_project()
  End If
  save_project = 2
!
  Return
 End Function save_project
!
!
!
 Function saveas_project()
!
! Prompts for a project name and saves the project (i.e., save as)
!
! Modules
  Use gui,      Only: set_win_title
  Use iofiles,  Only: get_new_file
  Use analysis, Only: ianal
!
! Function type
  Integer :: saveas_project
!
! Locals
!
! Local scalars
  Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Select project file
  Call get_new_file ('CPT Project Files', .true., this_proj%pfile, this_proj%pdir, pfilts(1), (/'CPT Project Files'/), &
       (/pfilts(1)/), 1, this_proj%pname, ifail)
!
! Update CPT window title
  If (ifail == 0) Then
     this_proj%lproj = .true.
     Call set_win_title (ianal, &
          cproj=this_proj%pname)
     saveas_project = psave()
  End If
  saveas_project = 2
!
  Return
 End Function saveas_project
!
!
!
 Function psave()
!
! Saves a project
!
! Modules
  Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv
  Use data_io_constants,  Only: iout
  Use data_version,       Only: ver
  Use gui,                Only: ihind
  Use screen,             Only: proj_write_screen
  Use errors,             Only: cpt_error
  Use arrays,             Only: proj_write_results
  Use settings,           Only: igauss, iretro, isave, &
                                proj_write_settings
  Use distribs,           Only: proj_write_distribs
  Use iofiles,            Only: xfile, yfile, zfile, &
                                open_iofile, proj_write_iofiles
  Use space,              Only: proj_write_interp, proj_write_space
  Use fields,             Only: proj_write_fields
  Use pcs,                Only: proj_write_pcs
  Use missing,            Only: proj_write_missing1, proj_write_missing2
  Use categories,         Only: proj_write_categories
  Use climate,            Only: lcex, &
                                proj_write_climate
  Use season,             Only: nsn, &
                                proj_write_season
  Use analysis,           Only: ianal, icalc, &
                                proj_write_analysis
  Use markers,            Only: proj_write_markers
  Use cpt_input,          Only: proj_write_data
  Use bootstrap,          Only: proj_write_bootstrap
  Use forecast,           Only: proj_write_forecasts
  Use verif,              Only: proj_write_verif
!
! Function type
  Integer :: psave
!
! Locals
!
! Local scalars
  Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Open project file
  Call open_iofile (iout, this_proj%pfile, .false., .false., .false., ifail)
  If (ifail /= 0) Then
     Call cpt_error ('open_project', ifail, .false., &
          c_arg1=this_proj%pfile)
     psave = 2
     Return
  End If
!
! Save version number
  Write (Unit=iout, Err=1) ver
  Call proj_write_analysis (iout, .false., ifail)
  If (ifail /= 0) GoTo 1
!
! Save settings
! - program settings -
  Call proj_write_settings (iout, icalc, ifail)
  If (ifail /= 0) GoTo 1
! - bootstrap settings -
  Call proj_write_bootstrap (iout, ifail)
  If (ifail /= 0) GoTo 1
! - input files -
  Call proj_write_iofiles (iout, ifail)
  If (ifail /= 0) GoTo 1
! - season settings -
  Call proj_write_season (iout, ifail)
  If (ifail /= 0) GoTo 1
! - field settings -
  Call proj_write_fields (iout, xfile%nfl, yfile%nfl*nsn, zfile%nfl, ifail)
  If (ifail /= 0) GoTo 1
! - domain settings -
  Call proj_write_space (iout, xfile%cfile%lset, xfile%idstr, xfile%igeog, yfile%cfile%lset, yfile%idstr, yfile%igeog, ifail)
  If (ifail /= 0) GoTo 1
! - missing value settings -
  Call proj_write_missing1 (iout, xfile%cfile%lset, yfile%cfile%lset, zfile%cfile%lset, ifail)
  If (ifail /= 0) GoTo 1
! - climatology settings -
  Call proj_write_climate (iout, .false., ifail)
  If (ifail /= 0) GoTo 1
! - category settings -
  Call proj_write_categories (iout, 0, 0, .false., ifail)
  If (ifail /= 0) GoTo 1
! - GUI settings -
  Call proj_write_pcs (iout, ianal, icalc, .false., ifail)
  If (ifail /= 0) GoTo 1
! - verification settings -
  Call proj_write_verif (iout, ifail)
  If (ifail /= 0) GoTo 1
! - screen settings -
  Call proj_write_screen (iout, ifail)
  If (ifail /= 0) GoTo 1
! - marker settings -
  Call proj_write_markers (iout, ifail)
  If (ifail /= 0) GoTo 1
! - input data -
  If (icalc == 1) Then
     Call proj_write_data (iout, ifail)
     If (ifail /= 0) GoTo 1
! - missing value indicators -
     Call proj_write_missing2 (iout, lcex, ifail)
     If (ifail /= 0) GoTo 1
! - climatological data -
     Call proj_write_climate (iout, .true., ifail)
     If (ifail /= 0) GoTo 1
! - category data -
     Call proj_write_categories (iout, ihind, iretro, .true., ifail)
     If (ifail /= 0) GoTo 1
! - distribution parameters -
     If (igauss == 2) Then
        Call proj_write_distribs (iout, ifail)
        If (ifail /= 0) GoTo 1
     End If
! - GUI results -
     Select Case (ianal)
      Case (ia_cca, ia_pcr, ia_mlr)
        Call proj_write_pcs (iout, ianal, icalc, .true., ifail)
! - interpolation settings -
      Case (ia_gcm)
        Call proj_write_interp (iout, ifail)
     End Select
     If (ifail /= 0) GoTo 1
! - optimization esults -
     If (ianal /= ia_pfv) Then
        Call proj_write_analysis (iout, .true., ifail)
        If (ifail /= 0) GoTo 1
     End If
! - results -
     Call proj_write_results (iout, ianal, igauss, ihind, iretro, ifail)
     If (ifail /= 0) GoTo 1
! - forecasts -
     Call proj_write_forecasts (iout, ianal, igauss, ihind, ifail)
     If (ifail /= 0) GoTo 1
  End If
  Close (Unit=iout)
!
! Confirm
  psave = 5
  Call cpt_error ('psave', psave, .false., &
       c_arg1=Trim(this_proj%pname)//pfilts(1)(2:), c_arg2=Trim(this_proj%pdir))
  isave = 0
!
  psave = 2
  Return
!
! Error saving project file
1 Close (Unit=iout)
  ifail = 4
  Call cpt_error ('psave', ifail, .false.)
  psave = 2
!
  Return
 End Function psave
!
!
!
 Function new_project()
!
! Clears existing project settings
!
! Modules
  Use gui,         Only: inew
  Use settings,    Only: isave
  Use analysis,    Only: ianal, icalc
  Use cpt_actions, Only: reset
!
! Function type
  Integer :: new_project
!
! Locals
!
! Local scalars
  Integer :: icalc_bk ! - backup calculation flag -
!
! Functions and Subroutines
!
! External routines
!  External init_sets
  Interface
    Subroutine init_sets ()
    End Subroutine init_sets
  End Interface
!
! Executable Statements
!
! Check for completed calculations
  new_project = 2
  If (ianal > 0) Then
     If (isave >= 2) isave = isave + 4
  End If
!
! Check for changes
  icalc_bk = icalc
  new_project = close_project()
  If (new_project == 1) Return 
!
! Free memory
  icalc = icalc_bk
  new_project = reset(' ', .false.)
!
! Reset all fields to defaults
  Call init_sets ()
!
! Indicate that all files are closed
  inew = 1
!
! Reset project settings
  new_project = clear_project()
!
  Return
 End Function new_project
!
!
!
 Function clear_project()
!
! Removes project name from window
!
! Modules
#if GUI == 1
  Use clrwin$,  Only: clear_window$
  Use gui,      Only: ih_con
#endif
  Use gui,      Only: set_win_title
  Use analysis, Only: ianal
!
! Function type
  Integer :: clear_project
!
! Executable Statements
!
! Reset project settings
  clear_project = 2
  Call reset_project (this_proj)
!
! Update CPT window title
  If (ianal > 0) Then
     Call set_win_title (ianal)
#if GUI == 1
!     Call clear_window$ (ih_con)
#endif
     Print *, ' '
  End If
!
  clear_project = 2
!
  Return
 End Function clear_project
!
!
!
 Subroutine reset_project (proj_file)
!
! Resets project name
!
! Modules
  Use settings, Only: isave
!
! Arguments
!
! Input/output scalars
  Type(proj), Intent(InOut) :: proj_file ! - project file -
!
! Executable Statements
!
! Reset project file
  proj_file%pfile = ' '
  proj_file%pname = ' '
  proj_file%lproj = .false.
  isave = 0
!
  Return
 End Subroutine reset_project
!
!
!
 Function close_project()
!
! Closes a project
!
! Modules
#if GUI == 1
  Use clrwin$,        Only: clear_window$
#endif
  Use data_numbers,   Only: zero
  Use time,           Only: iseq
#if GUI == 1
  Use gui,            Only: ih_con, &
                            prompt_yesno
#endif
  Use gui,            Only: iw
  Use progress_meter, Only: set_progress_meter
  Use settings,       Only: isave
  Use iofiles,        Only: xfile, yfile
  Use analysis,       Only: icalc, &
                            analysis_flags
  Use cpt_actions,    Only: reset_mem
!
! Function type
  Integer :: close_project
!
! Executable Statements
!
! Confirm loss of changes
  If (this_proj%lproj) Then
     If (isave /= 0) Then
#if GUI == 1
        Select Case (isave)
         Case (1)
           iw = prompt_yesno(.false., .true., &
                'Project File settings have been changed. Do you want to save the changes?')
         Case (2)
           iw = prompt_yesno(.false., .true., &
                'Do you want to save the results in the Project File?')
         Case (3)
           iw = prompt_yesno(.false., .true., &
                'Project File settings have been changed. Do you want to save the changes and the results?')
         Case (4)
           iw = prompt_yesno(.false., .true., &
                'Opening another project will clear current results.', &
                msg2='Do you want to save these results in the current Project File?')
         Case (5)
           iw = prompt_yesno(.false., .true., &
                'Opening another project will clear current results.', &
                msg2='Do you want to save these results and the changes to the settings in the current Project File?')
         Case (6)
           iw = prompt_yesno(.false., .true., &
                'Starting a new project will clear current results.', &
                msg2='Do you want to save the results in the current Project File?')
         Case (7)
           iw = prompt_yesno(.false., .true., &
                'Starting a new project will clear current results.', &
                msg2='Do you want to save the results and the changes to the settings in the current Project File?&')
         Case (-1)
           iw = prompt_yesno(.false., .true., &
                'Project File is old. Do you want to update the file?')
         Case Default
           iw = 2
        End Select
#else
        iw = 1
#endif
        Select Case (iw)
         Case (1)
           close_project = psave()
           close_project = 0
         Case (2)
           isave = 0
           close_project = 0
         Case Default
           If (icalc == 0) Then
              Call set_progress_meter (zero)
#if GUI == 1
!              Call clear_window$ (ih_con)
#endif
              Print *, ' '
           Else
              Select Case (isave)
               Case (4, 5)
                 isave = isave - 2
               Case (6, 7)
                 isave = isave - 4
              End Select
           End If
           close_project = 1
        End Select
        If (close_project == 0) Then
           this_proj%lproj = .false.
           If (.not.(xfile%cfile%lset .and. yfile%cfile%lset)) iseq = 0
           If (icalc == 1) Then
              close_project = reset_mem()
              Call analysis_flags ('off')
              close_project = clear_project()
              close_project = 0
           End If
        End If
     Else
        close_project = 0
     End If
  Else
     close_project = 0
  End If
!
  Return
 End Function close_project
End Module projects
