! Author: Simon Mason
Module cpt_interface
!
! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: cpt_window, title_page
!
Contains
!
!
 Subroutine title_page ()
!
! Creates CPT title page
!
! Modules
#if GUI == 0
  Use data_cpt_constants, Only: ia_cca, ia_pfv
#endif
  Use data_cpt_constants, Only: na
#if GUI == 1
  Use data_text,          Only: ilang
  Use data_io_constants,  Only: ddir
  Use data_version,       Only: cyr1, cyr2
  Use data_numbers,       Only: rp, one, two
  Use labels,             Only: init_labels
  Use errors,             Only: cpt_error
  Use gui,                Only: cwtitle, jel, &
                                add_bitmap, init_win, set_text_size, window_end
#else
  Use data_version,       Only: cyr1, cyr2, ver
  Use gui,                Only: iprompt, &
                                prompt_integer
#endif
  Use gui,                Only: inew, &
                                print_text, set_win_title
#if GUI == 1
  Use screen,             Only: init_fonts
  Use settings,           Only: iel
#endif
  Use analysis,           Only: ianal
  Use cpt_view,           Only: set_view
  Use projects,           Only: this_proj, &
                                reset_project
  Use user,               Only: cpt_ini, &
                                save_ini
!
! Locals
!
! Local scalars
  Integer :: ioc   ! - option changed flag -
  Integer :: iopt  ! - option -
#if GUI == 1
  Integer :: ifail ! - error indicator -
!
  Logical :: lnl ! - new language? -
#else
!
  Character(Len=13) :: ctxt ! - text -
#endif
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Check whether a project has been opened as a command-level argument
  If (ianal /= 0) Return
!
#if GUI == 1
! Initialise new language check
  lnl = (ilang /= 1)
!
! Initialise labels
1 If (lnl) Then
     Call init_labels (ilang, Trim(ddir), ifail)
     If (ifail /= 0) Then
        Call cpt_error ('init_labels', ifail, .false.)
        Stop
     End If
  Else
     lnl = .true.
  End If
!
! Create window
! - initialise window -
  ioc = 0
  jel = 1 - iel
  Call init_win (title=Trim(cwtitle), &
       lresize=.true.)
!
! Print title page
  Call set_text_size (3.5_rp)
#else
  Call print_text (' ')
#endif
  Call print_text (' ')
#if GUI == 1
  Call print_text ('CLIMATE PREDICTABILITY', &
       lbold=.true., lcentre=.true., lpad=.true.)
  Call print_text ('TOOL', &
       lbold=.true., lcentre=.true.)
  Call set_text_size (two)
#else
  Call print_text ('C L I M A T E   P R E D I C T A B I L I T Y   T O O L ', &
       lcentre=.true.)
  Write (Unit=ctxt, Fmt='(A,F5.2)') 'Version ',ver
  Call print_text (ctxt, &
       lcentre=.true.)
#endif
  Call print_text (' ')
  Call print_text ('Evaluating seasonal climate predictability', &
       lcentre=.true.)
  Call print_text ('Modificacion Ecuador !!!', &
       lcentre=.true.)
  Call print_text (' ')
#if GUI == 1
  Call set_text_size (one)
#endif
  Call print_text (' ')
  Call print_text ('Copyright '//cyr1//'-'//cyr2//' International Research Institute for Climate and Society', &
       lcentre=.true.)
  Call print_text (' ')
  Call print_text (' ')
#if GUI == 1
  Call add_bitmap ('IRI_logo')
#else
  Call print_text ('III RRRR  III                                                                 ', &
       lcentre=.true.)
  Call print_text (' I  R   R  I                                                                  ', &
       lcentre=.true.)
  Call print_text (' I  R   R  I                                                                  ', &
       lcentre=.true.)
  Call print_text (' I  RRRR   I                                                                  ', &
       lcentre=.true.)
  Call print_text (' I  R   R  I   I N T E R N A T I O N A L   R E S E A R C H   I N S T I T U T E', &
       lcentre=.true.)
  Call print_text ('III R   R III  F O R         C L I M A T E         A N D         S O C I E T Y', &
       lcentre=.true.)
1 Call print_text (' ')
  Call print_text (' ')
#endif
!
! Add menus
  iopt = 0
  Call add_menus (iopt, &
                  ioc=ioc)
!
! Prompt for option
#if GUI == 1
  Call window_end ()
#else
  Call prompt_integer (' ', iopt)
  Select Case (iopt)
! - open project -
   Case (142)
     Call menu_file (iopt)
     If (iopt /= 0) Then
        Select Case (iprompt)
         Case (0)
           GoTo 1
         Case (1)
           ianal = ia_cca
         Case (2)
           Stop
        End Select
     End If
! - data download -
   Case (811)
     Call menu_data (iopt)
     GoTo 1
! - set analysis type -
   Case (0:na)
     ianal = iopt
   Case (611:614)
     ianal = iopt - 610
   Case (621)
     ianal = ia_pfv
! - change language -
   Case (511, 11)
     Call menu_opts (iopt)
     GoTo 1
! - error options -
   Case (571)
     Call menu_opts (iopt)
     GoTo 1
! - help_about -
   Case (700:799)
     Call menu_help (iopt)
     GoTo 1
! - error -
   Case Default
     Select Case (iprompt)
      Case (0)
        GoTo 1
      Case (1)
        ianal=ia_cca
      Case (2)
        Stop
     End Select
  End Select
#endif
!
! Check for changed options
  If (ioc == 1) Then
     Call save_ini (Trim(cpt_ini))
#if GUI ==1
     Call init_fonts ()
#endif
     GoTo 1
  End If
  Select Case (ianal)
! - quit -
   Case (0)
     Stop
! - set analysis type -
   Case (1:na)
     If (.not.this_proj%lproj) Then
        Call reset_project (this_proj) ! - apply project settings -
        inew = 1
     End If
     If (set_view() /= 0) Stop
  End Select
!
! Reset Window title
  If (.not.this_proj%lproj) Call set_win_title (ianal)
!
  Return
 End Subroutine title_page
!
!
!
#if GUI == 1
 Subroutine cpt_window ()
!
! Creates CPT window
!
! Modules
  Use data_gui_constants, Only: i_handle
  Use screen,             Only: ihght, iwdth
  Use gui,                Only: cwtitle, ic_par, &
                                add_mdi_frame, init_win, leave_window, set_viewflags
  Use settings,           Only: iel, igcms, iretro, istd, isynch
  Use iofiles,            Only: xfile
  Use season,             Only: icsv
  Use analysis,           Only: ianal, &
                                close_analysis
  Use forecast,           Only: close_fcast
  Use projects,           Only: close_project
!
! Locals
!
! Local scalars
  Integer :: iopt ! - option -
!
  Integer(i_handle) :: ih_host ! - host window handle -
!
! Executable Statements
!
! Set grey menu items
  Call set_viewflags (ianal, istd, xfile%idstr, igcms, icsv, iel, isynch, &
                      iretro=iretro)
!
! Open results window
  Call init_win (title=cwtitle, &
       lresize=.true., ihandle=ih_host, c_opts='maximise,no_border', &
       fclose=close_fcast, fclose2=close_analysis, fclose3=close_project)
! - add menus -
  iopt = 1
  Call add_menus (iopt)
! - create MDI frame -
  Call add_mdi_frame (iwdth-10, ihght-700)
  Call leave_window (ic_par)
!
! Prepare console window
  Call console_init (400)
!
  Return
!
Contains
!
!
 Subroutine console_init (nlines)
!
! Initialises console for runtime advisories
!
! Modules
  Use clrwin$,      Only: add_cursor_monitor$, set_max_lines$
  Use data_numbers, Only: one
  Use labels,       Only: cg_infiles_t, cg_ltp, cg_nof, cg_xs_t, cg_ys_t, cg_zs_t, &
                          c_lab, &
                          l_actions, l_fcasts, &
                          make_label
  Use screen,       Only: dts
  Use gui,          Only: ic_con, icon_h, icon_v, ih_con, ih_nt, ih_xfd, ih_yfd, &
                          box_close, box_open, gui_creturn, ic_current, init_win, open_clearwin, print_text, prompt_integer, &
                          set_tabs, set_text_size
  Use settings,     Only: mxa, mya, mza, nf, nt, nx, ny, nz, &
                          record_change
  Use iofiles,      Only: mfile, yfile, zfile
  Use analysis,     Only: iaction
  Use forecast,     Only: change_fcast
  Use cpt_actions,  Only: check_reset
  Use cpt_file,     Only: input_files
  Use user,         Only: iwin
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nlines ! - number of lines -
!
! Locals
!
! Local scalars
  Integer :: ioutc ! - output unit number of console -
  Integer :: ncol  ! - number of columns -
  Integer :: nrow  ! - number of rows -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Max, Nint, Trim
!
! Executable Statements
!
! Add control monitor
  Call add_cursor_monitor$ (ic_current)
!
! Create console window
  Call init_win (ipar=ic_par, c_opts='no_frame,no_caption')
! - X input file -
  Call box_open (.false., .false.)
  Call print_text (Trim(cg_infiles_t), &
       lbold=.true., lcentre=.true., lcolour=.true.)
  Call box_open (.false., .false.)

  Call input_files ('X', Trim(cg_xs_t), xfile, &
       ih=ih_xfd, nv=nx, nva=mxa, fcb1=record_change, fcb2=check_reset)
  Call gui_creturn (.false.)
  Call gui_creturn (.true.)
  Call print_text (' ', &
       lcentre=.true., ladvance=.false.)
  Call prompt_integer (' '//Trim(cg_ltp), nt, &
       ilow=0, fcb1=check_reset, lbold=.true., lfocus=.false., ihandle=ih_nt)
  Call box_close (.false.)
! - Y input file -
  Call box_open (.false., .false.)
  Call input_files ('Y', Trim(cg_ys_t), yfile, &
       ih=ih_yfd, nv=ny, nva=mya, cprd1=mfile%cprd1, cprdn=mfile%cprdn, fcb1=record_change, fcb2=check_reset)
  Call gui_creturn (.false.)
  Call box_close (.false.)
! - Z input file -
  Call box_open (.false., .false.)
  Call input_files ('Z', Trim(cg_zs_t), zfile, &
       iopen=iaction, nv=nz, nva=mza, fcb3=change_fcast)
  Call gui_creturn (.false.)
  Call gui_creturn (.true.)
  Call print_text (' ', &
       lcentre=.true., ladvance=.false.)
  c_lab = make_label('L', l_fcasts)
  Call prompt_integer (' '//Trim(cg_nof)//' '//Trim(c_lab), nf, &
       ilow=0, fcb1=change_fcast, lbold=.true., lfocus=.false.)
  Call box_close (.false.)
! - close input box -
  Call box_close (.false.)
! - set text size -
  Call set_text_size (1.5d0)
! - tab setting -
  Call set_tabs (2, (/3, 15/))
! - set actions list -
  Call gui_creturn (.false.)
  Call print_text (' '//Trim(l_actions%c), &
       ladvance=.false.)
  Call set_text_size (one)
! - open Clearwin+ window -
  Call gui_creturn (.true.)
  Call gui_creturn (.true.)
  If (iwin == 10) Then
     ncol = Nint(Real(iwdth)*0.1) - 3
     nrow = Nint(-14.5+0.0555*Real(ihght)+dts*Real(-17))
  Else
     ncol = Nint(Real(iwdth - 33)/7.95)
     nrow = Max(1, Nint(Real(ihght - 465)/16.5))
  End If
  Call open_clearwin (ioutc, ncol+icon_h, nrow+icon_v, .false., &
       i_out=0, ih=ih_con, lhs=.true., lvs=.true.)
! - leave open  -
  Call leave_window (ic_con)
!
  Call set_max_lines$ (ih_con, nlines)
  Print *, ' '
!
  Return
 End Subroutine console_init
 End Subroutine cpt_window
#else
!
!
!
 Subroutine cpt_window ()
!
! Indicates CPT prompt options
!
! Modules
  Use time,               Only: pdate
  Use labels,             Only: cg_ltp, cg_nof, cg_traindata, &
                                c_lab, &
                                l_fcast, l_fcasts, &
                                make_label
  Use screen,             Only: iwdth
  Use gui,                Only: cwtitle, lwu, &
                                add_menu, print_text, prompt_integer, prompt_pdate, set_viewflags
  Use settings,           Only: iel, igcms, iretro, istd, isynch, mxa, mya, nf, nt, nx, ny
  Use iofiles,            Only: i_file, mfile, xfile, yfile, zfile, &
                                adjust_mfdate, set_bdate
  Use season,             Only: icsv, lsn
  Use analysis,           Only: ianal, icalc, jcalc, &
                                close_analysis, get_nt
  Use forecast,           Only: change_fcast, close_fcast, get_nf
  Use cpt_actions,        Only: reset
  Use cpt_file,           Only: get_file
!
! Locals
!
! Local arrays
  Type(pdate) :: dlimits(2) ! - date limits -
!
! Locals scalars
  Integer :: iopt ! - analysis option -
!
  Character(Len=iwdth-1) :: ctxt ! - text -
!
  Logical :: lsubm ! - sub-menu? -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Mod, Repeat, Trim
!
! Executable Statements
!
! Set grey menu items
  Call set_viewflags (ianal, istd, xfile%idstr, igcms, icsv, iel, isynch, &
       iretro=iretro)
!
! Print title
  Do
     If (lwu) Then
        Call print_text (' ')
        ctxt = Repeat('_', iwdth-1)
        Call print_text (ctxt, &
             lcentre=.true.)
        Call print_text (' ')
        Call print_text (Trim(cwtitle))
!
! Indicate current settings
        Call file_settings (           'X', xfile, nx, mxa)
        Call file_settings (           'Y', yfile, ny, mya, &
             cprd1=mfile%cprd1, cprdn=mfile%cprdn)
        Call file_settings (Trim(l_fcast%c), zfile, nx, mxa)
!
        Print *, ' '
        Call print_text (Trim(cg_traindata))                                    ! 'Training Data'
        Write (Unit=*, Fmt='(1X,A,T45,I10)') Trim(cg_ltp), nt                   ! 'Length of training period: '
        c_lab = make_label('L', l_fcasts)
        Write (Unit=*, Fmt='(1X,A,T45,I10)') Trim(cg_nof)//' '//Trim(c_lab), nf ! 'Number of forecasts: '
     End If
     Call print_text (ctxt, & 
          lcentre=.true.)
     lwu=.false.
!
! Indicate options
     Call print_text (' ')
     Call print_text ('Select option:')
! - data files -
     Call add_menu ('mn', 1,   0, ' ')
     Call add_menu ('mn', 2,   1, 'Open X input file', &
          iset1=i_file, ival1=1, cbf=get_file)
     Call add_menu ('mn', 2,   2, 'Open Y input file', &
          iset1=i_file, ival1=2, cbf=get_file)
     Call add_menu ('mn', 2,   3, 'Open forecast file', &
          igrey=xfile%idstr, iset1=i_file, ival1=3, cbf=get_file)
     Call add_menu ('mn', 2,   4, 'X training period settings', &
          igrey=xfile%idstr)
     Call add_menu ('mn', 2,   5, 'Y training period settings', &
          igrey=yfile%idstr)
     Call add_menu ('mn', 2,   6, 'Forecast period settings', &
          igrey=zfile%idstr, cbf=change_fcast)
     Call add_menu ('mn', 2,   7, cg_ltp, &
          igrey=jcalc, cbf=get_nt)
! Option 8 (length of cross-validation window) replaced by 534
     c_lab = make_label('L', l_fcasts)
     Call add_menu ('mn', 2,   9, Trim(cg_nof)//' '//Trim(c_lab), &
          igrey=zfile%idstr, cbf=get_nf)
!
! Add menus
     iopt = 1
     Call add_menus (iopt)
     Call prompt_integer (' ', iopt)
1    lsubm = (Mod(iopt, 10) == 0) ! - check whether option is a sub-menu -
!
! Perform action
     Select Case (iopt)
      Case (1) ! - Open X input file -
        i_file = iopt
        iopt = get_file()
      Case (2) ! - Open Y input file -
        i_file = iopt
        iopt = get_file()
      Case (3) ! - Open forecast file -
        If (.not.xfile%cfile%lset) Cycle
        i_file = iopt
        iopt = get_file()
      Case (4) ! - X training period settings -
        If (.not.xfile%cfile%lset) Cycle
        If (reset('Modifying start date of training period', .true.) == 1) Cycle
        Write (Unit=*, Fmt='(2A)') 'First date of data in X file: ', xfile%cprd1
        dlimits(1) = xfile%prd1%sdate
        dlimits(2) = xfile%prdn%sdate
        Call prompt_pdate ('First date of X training period', xfile%iseq, xfile%fdate, &
             dlimits=dlimits, lfocus=.true.)
        xfile%bdate = xfile%fdate
      Case (5) ! - Y training period settings -
        If (.not.yfile%cfile%lset) Cycle
        If (reset('Modifying start date of training period', .true.) == 1) Cycle
        Write (Unit=*, Fmt='(2A)') 'First date of data in Y file: ', mfile%cprd1
        dlimits(1) = yfile%prd1%sdate
        dlimits(2) = yfile%prdn%sdate
        Call prompt_pdate ('First date of Y training period', yfile%iseq, yfile%fdate, &
             dlimits=dlimits, modify2=adjust_mfdate, lfocus=.true.)
        Call set_bdate (lsn(0))
        mfile%fdate = yfile%fdate
        mfile%bdate = yfile%bdate
      Case (6) ! - Forecast period settings -
        If (.not.zfile%cfile%lset) Cycle
        Write (Unit=*, Fmt='(2A)') 'First date of data in forecast file: ', zfile%cprd1
        dlimits(1) = zfile%prd1%sdate
        dlimits(2) = zfile%prdn%sdate
        Call prompt_pdate ('First date from which to forecast', zfile%iseq, zfile%fdate, &
             dlimits=dlimits, lfocus=.true.)
        iopt = change_fcast()
      Case (7) ! - Length of training period -
        iopt = get_nt()
      Case (9) ! - Number of forecasts -
        If (.not.zfile%cfile%lset) Cycle
        iopt = get_nf()
      Case (100:199)
        Call menu_file (iopt)
      Case (800:899) ! - data menu -
        Call menu_data (iopt)
      Case (200:299)
        Call menu_edit (iopt)
      Case (300:399)
        Call menu_acts (iopt)
      Case (400:499)
        Call menu_tool (iopt)
      Case (500:599, 8)
        Call menu_opts (iopt)
      Case (600:699)
        Call menu_view (iopt)
      Case (700:799)
        Call menu_help (iopt)
      Case (0) ! - Exit -
        If (icalc == 1) Then
           iopt = close_fcast()
           iopt = close_analysis()
        End If
        Exit
      Case Default
        Cycle
     End Select
     If (lsubm) GoTo 1 ! - repeat actions search when responding to a sub-menu -
  End Do

  Return
!
Contains
!
!
 Subroutine file_settings (cxyz, afile, nv, mva, &
            cprd1, cprdn)
!
! Prints CPT input file settings
!
! Modules
  Use data_io_constants, Only: lprd
  Use maths,             Only: magnitude
  Use time,              Only: get_cdate
  Use labels,            Only: cg_data1, cg_datan, cg_fields_l, cg_indata_t, cg_nof, cg_nused, cg_startat, &
                               cls_l, &
                               l_file
  Use iofiles,           Only: ifile
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nv  ! - number of variables -
  Integer, Intent(In) :: mva ! - number of variables -
!
  Character(Len=*), Intent(In) :: cxyz ! - X/Y/Z flag -
!
  Type(ifile), Intent(In) :: afile ! - input file -
!
! - optional input scalars -
  Character(Len=*), Intent(In), Optional :: cprd1 ! - first period available -
  Character(Len=*), Intent(In), Optional :: cprdn ! - last period available -
!
! Locals
!
! Local scalars
  Character(Len=lprd) :: fsdate ! - file start date -
  Character(Len=  24) :: cfmt   ! - format statement -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic AdjustL, Present, Trim
!
! Executable Statements
!
! Print file settings
  Print *, ' '
  Write (Unit=*, Fmt='(  T45,A)') cxyz//' '//Trim(l_file%c)
  Print *, ' '
  Write (Unit=*, Fmt='(1X,A,T45,A)') cg_indata_t, Trim(afile%cfile%fname)
  If (Present(cprd1)) Then
     Write (Unit=*, Fmt='(1X,A,T45,A)') Trim(cg_data1), Trim(cprd1)
  Else
     Write (Unit=*, Fmt='(1X,A,T45,A)') Trim(cg_data1), Trim(afile%cprd1)
  End If
  If (Present(cprd1)) Then
     Write (Unit=*, Fmt='(1X,A,T45,A)') Trim(cg_datan), Trim(cprdn)
  Else
     Write (Unit=*, Fmt='(1X,A,T45,A)') Trim(cg_datan), Trim(afile%cprdn)
  End If
  fsdate=get_cdate(afile%fdate,2)
  Write (Unit=*, Fmt='(1X,A,T45,A)') Trim(cg_startat), AdjustL(fsdate)
  Write (Unit=cfmt, Fmt='(A,I1,A)') '(1X,A,T45,I', magnitude(afile%nfs), ')'
  Write (Unit=*, Fmt=cfmt) Trim(cg_nof)//' '//Trim(cg_fields_l), afile%nfs
  Write (Unit=cfmt, Fmt='(A,I1,A)') '(1X,A,T45,I', magnitude(afile%nls), ')'
  Write (Unit=*, Fmt=cfmt) Trim(cg_nof)//' '//Trim(cls_l), afile%nls
  Write (Unit=cfmt, Fmt='(A,I1,A)') '(1X,A,T45,I', magnitude(nv), ')'
  Write (Unit=*, Fmt=cfmt) Trim(cg_nof)//' '//Trim(afile%cgss), nv
  Write (Unit=cfmt, Fmt='(A,I1,A)') '(1X,A,T45,I', magnitude(mva), ')'
  Write (Unit=*, Fmt=cfmt) Trim(cg_nused)//' variables: ', mva
!
  Return
 End Subroutine file_settings
 End Subroutine cpt_window
#endif
!
!
!
 Subroutine add_menus (iopt, &
            ioc)
!
! Adds windows menus
!
#if GUI == 0
! Modules
  Use labels, Only: l_exit
  Use gui,    Only: print_text
!
#endif
! Arguments
!
! Input/output scalars
  Integer, Intent(InOut) :: iopt ! - selected option -
!
! Output scalars
! - optional output scalars -
  Integer, Intent(Out), Optional :: ioc ! - changed option -
!
#if GUI == 0
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
#endif
! Executable Statements
!
! Add shortened windows menu
! - file menu -
  Select Case (iopt)
   Case (0)
     iopt = -100
     Call menu_file (iopt)
! - data menu -
     iopt = 800
     Call menu_data (iopt)
! - options menu -
     iopt = -500
     Call menu_opts (iopt, &
          ioc=ioc)
! - view menu -
     iopt = -600
     Call menu_view (iopt)
! - help menu -
     iopt = 700
     Call menu_help (iopt)
!
! Add extended windows menus
! - file -
   Case (1)
     iopt = 100
     Call menu_file (iopt)
! - data -
     iopt = 800
     Call menu_data (iopt)
! - edit -
     iopt = 200
     Call menu_edit (iopt)
! - actions -
     iopt = 300
     Call menu_acts (iopt)
! - tools -
     iopt = 400
     Call menu_tool (iopt)
! - options -
     iopt = 500
     Call menu_opts (iopt)
! - view -
     iopt = 600
     Call menu_view (iopt)
! - help -
     iopt = 700
     Call menu_help (iopt)
  End Select
!
#if GUI == 0
! Add exit prompt
  Call print_text (' ')
  Call print_text (' ')
  Call print_text ('  0.  '//Trim(l_exit%c))
  Call print_text (' ')
!
#endif
  Return
 End Subroutine add_menus
!
!
!
 Subroutine menu_file (iopt)
!
! Constructs File menu
!
! Modules
  Use labels,     Only: l_lab, &
                        l_close, l_file, l_new, l_open, l_outres, l_save, l_saveas, l_savegi, l_viewgi
#if GUI == 1
  Use labels,     Only: l_exit, l_resdefs
#endif
  Use gui,        Only: add_menu
  Use settings,   Only: isave
  Use iofiles,    Only: xfile, yfile, zfile
#if GUI == 0
  Use iofiles,    Only: cformat, itags, &
                        output_format, output_precision
#endif
  Use analysis,   Only: icalc, jcalc
  Use cpt_output, Only: save_good
  Use validate,   Only: view_good
  Use cpt_file,   Only: iclose, &
                        close_file
  Use projects,   Only: new_project, open_project, save_project, saveas_project
#if GUI == 1
  Use user,       Only: restore_defs
#endif
!
! Arguments
!
! Input/output scalars
  Integer, Intent(InOut) :: iopt ! - selected option -
!
! Locals
!
! Local scalars
#if GUI == 0
  Integer :: igrey ! - greyed menu flag -
#endif
  Integer :: ilev  ! - current menu-level -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Construct File menu
  Select Case (iopt)
   Case (-100) ! - shortened menu -
     Call add_menu ('mn', 1, 100, l_file)
     Call add_menu ('mn', 2, 142, l_open, &
          cbf=open_project, acc_key='Ctrl+O')
#if GUI == 1
     Call add_menu ('mn', 2,  -2, l_resdefs, &
          cbf=restore_defs)
     Call add_menu ('mn', 2,   0, l_exit, &
          scbf='STOP', acc_key='Ctrl+Q')
#endif
   Case (100) ! - extended menu -
     Call add_menu ('mn', 1, 100, l_file)
     Call add_menu ('mn', 2, 141, l_new, &
          cbf=new_project, acc_key='Ctrl+N')
     Call add_menu ('mn', 2, 142, l_open, &
          cbf=open_project, acc_key='Ctrl+O')
     Call add_menu ('mn', 2, 143, l_save, &
          igrey=isave, cbf=save_project, acc_key='Ctrl+S')
     Call add_menu ('mn', 2, 144, l_saveas, &
          cbf=saveas_project, acc_key='Ctrl+Shift+S')
     l_lab%c = Trim(l_close%c)//' X'
     l_lab%iak = Len_Trim(l_close%c) + 2
     Call add_menu ('mn', 2, 121, l_lab, &
          lsep=.true., igrey=xfile%nfs, iset1=iclose, ival1=1, cbf=close_file)
     l_lab%c = Trim(l_close%c)//' Y'
     Call add_menu ('mn', 2, 122, l_lab, &
          igrey=yfile%nfs, iset1=iclose, ival1=2, cbf=close_file)
     l_lab%c = Trim(l_close%c)//' Forecast'
     Call add_menu ('mn', 2, 123, l_lab, &
          igrey=zfile%nfs, iset1=iclose, ival1=3, cbf=close_file)
     ilev = 2
     Call menu_file_outres ()
#if GUI == 1
     Call add_menu ('mn', 2,   0, l_exit, &
          lsep=.true., scbf='STOP', acc_key='Ctrl+Q')
#else
     l_lab%c = 'Output file format'
     Call add_menu ('mn', 2, 131, l_lab, &
          cbf=output_format)
     If (cformat%ifmt /= 3) Then
        igrey = 1
     Else
        igrey = 0
     End If
     l_lab%c = 'Output data precision'
     Call add_menu ('mn', 2, 132, l_lab, &
          igrey=igrey, cbf=output_precision)
     Select Case (itags)
      Case (0)
        l_lab%c = 'Include CPT output tags'
      Case (1)
        l_lab%c = 'Exclude CPT output tags'
     End Select
     Call add_menu ('mn', 2, 133, l_lab, &
          itoggle=itags)
   Case (110) ! - output results -
     ilev = 1
     Call menu_file_outres ()
   Case (111) ! - data output -
     If (icalc == 1) iopt = output_results()
   Case (112) ! - save goodness index -
     If (jcalc == 1) iopt = save_good()
   Case (113) ! - save goodness index -
     If (icalc == 1) iopt = view_good()
   Case (121) ! - close X file -
     If (xfile%cfile%lset) Then
        iclose = 1
        iopt = close_file()
     End If
   Case (122) ! - close Y file -
     If (yfile%cfile%lset) Then
        iclose = 2
        iopt = close_file()
     End If
   Case (123) ! - close forecast file -
     If (.not.zfile%cfile%lset) Then
        iclose = 3
        iopt = close_file()
     End If
   Case (131) ! - file output format -
     iopt = output_format()
   Case (132) ! - data precision -
     If (cformat%ifmt /= 3) iopt = output_precision()
   Case (133) ! - CPT output tags -
     itags = 1 - itags
   Case (141) ! - new project -
     iopt = new_project()
   Case (142) ! - open project -
     iopt = open_project()
   Case (143) ! - open project -
     If (isave>0) iopt = save_project()
   Case (144) ! - open project -
     iopt = saveas_project()
#endif
  End Select
!
  Return
!
 Contains
!
!
  Subroutine menu_file_outres ()
!
! Output Results sub-menu
!
#if GUI == 0
! Modules
  Use gui, Only: prompt_integer
!
#endif
! Executable Statements
!
! Construct File ~ Output Results menu
  Call add_menu ('mn', ilev, 110, l_outres)
  Call add_menu ('mn', ilev+1, 112, l_savegi, &
       igrey=jcalc, cbf=save_good)
  Call add_menu ('mn', ilev+1, 113, l_viewgi, &
       igrey=icalc, cbf=view_good)
  Call add_menu ('mn', ilev+1, 111, l_outres, &
       lsep=.true., igrey=icalc, cbf=output_results)
!
#if GUI == 0
! Prompt for option
  If (iopt == 110) Call prompt_integer (' ',iopt)
#endif
!
  Return
  End Subroutine menu_file_outres
 End Subroutine menu_file
!
!
!
 Subroutine menu_data (iopt)
!
! Constructs Data menu
!
! Modules
  Use data_iridl, Only: id_gcm, id_obs, id_rea, &
                        ig_enso, ig_gphs, ig_mslp, ig_ssts, ig_tave, ig_tmax, ig_tmin, ig_prcp, ig_uvwd, ig_uwnd, ig_vwnd, ig_wetd
  Use labels,     Only: l_data, l_download, l_enso, l_gcmouts, l_gph, l_merge, l_mslp, l_obs, l_plevs, l_prcp, l_reanalyses, &
                        l_sst, l_tmax, l_tmean, l_tmin, l_u, l_update, l_uv, l_v, l_wetdays
#if GUI == 0
  Use labels,     Only: l_cancel
  Use gui,        Only: print_item, print_text, prompt_integer
#endif
  Use gui,        Only: inew, &
                        add_menu
  Use cpt_file,   Only: merge_files, update_file
  Use downloads,  Only: iddt, igrib, &
                        download_data
!
! Arguments
!
! Input/output scalars
  Integer, Intent(InOut) :: iopt ! - selected option -
!
#if GUI == 0
! Locals
!
! Local scalars
  Integer :: idown ! - download -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
#endif
! Executable Statements
!
! Construct Data menu
#if GUI == 0
  idown = 0
  Select Case (iopt)
   Case (800)
#endif
     Call add_menu ('mn', 1, 800, l_data)
     Call add_menu ('mn', 2, 801, l_merge, &
          igrey=inew, cbf=merge_files)
     Call add_menu ('mn', 2, 802, l_update, &
          igrey=inew, cbf=update_file)
#if GUI == 1
     Call add_menu ('mn', 2, 811, l_download)
#else
     Call add_menu ('mn', 2, 811, l_download, &
          scbf='CONTINUE')
#endif
! - observations -
#if GUI == 1
  Call add_menu ('mn', 3,  id_obs, l_obs)
  Call add_menu ('mn', 4, ig_enso, l_enso, &
       iset1=iddt, ival1=1, iset2=igrib, ival2=ig_enso, cbf=download_data)
  Call add_menu ('mn', 4, ig_ssts, l_sst, &
       iset1=iddt, ival1=1, iset2=igrib, ival2=ig_ssts, cbf=download_data)
  Call add_menu ('mn', 4, ig_tmax, l_tmax, &
       iset1=iddt, ival1=1, iset2=igrib, ival2=ig_tmax, cbf=download_data)
  Call add_menu ('mn', 4, ig_tmin, l_tmin, &
       iset1=iddt, ival1=1, iset2=igrib, ival2=ig_tmin, cbf=download_data)
  Call add_menu ('mn', 4, ig_tave, l_tmean, &
       iset1=iddt, ival1=1, iset2=igrib, ival2=ig_tave, cbf=download_data)
  Call add_menu ('mn', 4, ig_prcp, l_prcp, &
       iset1=iddt, ival1=1, iset2=igrib, ival2=ig_prcp, cbf=download_data)
  Call add_menu ('mn', 4, ig_wetd, l_wetdays, &
       iset1=iddt, ival1=1, iset2=igrib, ival2=ig_wetd, cbf=download_data)
! - reanalysys -
  Call add_menu ('mn', 3,  id_rea, l_reanalyses)
  Call add_menu ('mn', 4, ig_mslp, l_mslp, &
       iset1=iddt, ival1=2, iset2=igrib, ival2=ig_mslp, cbf=download_data)
  Call add_menu ('mn', 4, ig_gphs, l_gph, &
       iset1=iddt, ival1=2, iset2=igrib, ival2=ig_gphs, cbf=download_data)
  Call add_menu ('mn', 4, ig_uwnd, l_u, &
       iset1=iddt, ival1=2, iset2=igrib, ival2=ig_uwnd, cbf=download_data)
  Call add_menu ('mn', 4, ig_vwnd, l_v, &
       iset1=iddt, ival1=2, iset2=igrib, ival2=ig_vwnd, cbf=download_data)
  Call add_menu ('mn', 4, ig_uvwd, l_uv, &
       iset1=iddt, ival1=2, iset2=igrib, ival2=ig_uvwd, cbf=download_data)
! - GCM outputs -
  Call add_menu ('mn', 3,  id_gcm, l_gcmouts)
  Call add_menu ('mn', 4, ig_ssts, l_sst, &
       iset1=iddt, ival1=3, iset2=igrib, ival2=ig_ssts, cbf=download_data)
  Call add_menu ('mn', 4, ig_tmax, l_tmax, &
       iset1=iddt, ival1=3, iset2=igrib, ival2=ig_tmax, cbf=download_data)
  Call add_menu ('mn', 4, ig_tmin, l_tmin, &
       iset1=iddt, ival1=3, iset2=igrib, ival2=ig_tmin, cbf=download_data)
  Call add_menu ('mn', 4, ig_tave, l_tmean, &
       iset1=iddt, ival1=3, iset2=igrib, ival2=ig_tave, cbf=download_data)
  Call add_menu ('mn', 4, ig_prcp, l_prcp, &
       iset1=iddt, ival1=3, iset2=igrib, ival2=ig_prcp, cbf=download_data)
  Call add_menu ('mn', 4,      -1, l_plevs)
  Call add_menu ('mn', 5, ig_gphs, l_gph, &
       iset1=iddt, ival1=3, iset2=igrib, ival2=ig_gphs, cbf=download_data)
  Call add_menu ('mn', 5, ig_uwnd, l_u, &
       iset1=iddt, ival1=3, iset2=igrib, ival2=ig_uwnd, cbf=download_data)
  Call add_menu ('mn', 5, ig_vwnd, l_v, &
       iset1=iddt, ival1=3, iset2=igrib, ival2=ig_vwnd, cbf=download_data)
#else
!
! Actions
! - merge files -
   Case (801)
     If (inew == 1) iopt = merge_files()
! - update files -
   Case (802)
     If (inew == 1) iopt = update_file()
! - data download -
   Case (811)
     Call add_menu ('mn', 2, 811, l_download)
     Call print_item (id_obs, Trim(l_obs%c), &
          itab=6, iwid=1)
     Call print_item (id_rea, Trim(l_reanalyses%c), &
          itab=6, iwid=1)
     Call print_item (id_gcm, Trim(l_gcmouts%c), &
          itab=6, iwid=1)
     Call print_item (     0, Trim(l_cancel%c), &
          itab=6, iwid=1)
     Call prompt_integer (' ', iddt, &
          ilow=0, ihgh=3)
     Select Case (iddt)
      Case (id_obs) ! - observational data -
        getgrib1: Do
           Call print_text (Trim(l_obs%c)//':')
           Call print_item (ig_enso, Trim(l_enso%c), &
                itab=9, iwid=4)
           Call print_item (ig_ssts, Trim(l_sst%c), &
                itab=9, iwid=4)
           Call print_item (ig_tmax, Trim(l_tmax%c), &
                itab=9, iwid=4)
           Call print_item (ig_tmin, Trim(l_tmin%c), &
                itab=9, iwid=4)
           Call print_item (ig_tave, Trim(l_tmean%c), &
                itab=9, iwid=4)
           Call print_item (ig_prcp, Trim(l_prcp%c), &
                itab=9, iwid=4)
           Call print_item (ig_wetd, Trim(l_wetdays%c), &
                itab=9, iwid=4)
           Call print_item (      0, Trim(l_cancel%c), &
                itab=9, iwid=4)
           Call prompt_integer (' ', igrib)
           Select Case (igrib)
            Case (ig_enso, ig_ssts, ig_tmax, ig_tmin, ig_tave, ig_prcp, ig_wetd)
              idown = 1
              Exit getgrib1
            Case (0)
              idown = 0
              Exit getgrib1
            Case Default
              Cycle getgrib1
           End Select
        End Do getgrib1
      Case (id_rea) ! - reanalysis data -
        getgrib2: Do
           Call print_text (Trim(l_reanalyses%c)//':')
           Call print_item (ig_mslp, Trim(l_mslp%c), &
                itab=9, iwid=4)
           Call print_item (ig_gphs, Trim(l_gph%c), &
                itab=9, iwid=4)
           Call print_item (ig_uwnd, Trim(l_u%c), &
                itab=9, iwid=4)
           Call print_item (ig_vwnd, Trim(l_v%c), &
                itab=9, iwid=4)
           Call print_item (ig_uvwd, Trim(l_uv%c), &
                itab=9, iwid=4)
           Call print_item (      0, Trim(l_cancel%c), &
                itab=9, iwid=4)
           Call prompt_integer (' ', igrib)
           Select Case (igrib)
            Case (ig_uwnd, ig_vwnd, ig_uvwd, ig_mslp, ig_gphs)
              idown = 1
              Exit getgrib2
            Case (0)
              idown = 0
              Exit getgrib2
            Case Default
              Cycle getgrib2
           End Select
        End Do getgrib2
      Case (id_gcm) ! - GCM outputs -
        getgrib3: Do
           Call print_text (Trim(l_gcmouts%c)//':')
           Call print_item (ig_ssts, Trim(l_sst%c), &
                itab=9, iwid=4)
           Call print_item (ig_tave, Trim(l_tmean%c), &
                itab=9, iwid=4)
           Call print_item (ig_tmax, Trim(l_tmax%c), &
                itab=9, iwid=4)
           Call print_item (ig_tmin, Trim(l_tmin%c), &
                itab=9, iwid=4)
           Call print_item (ig_prcp, Trim(l_prcp%c), &
                itab=9, iwid=4)
           Call print_item (ig_gphs, Trim(l_plevs%c)//': '//Trim(l_gph%c), &
                itab=9, iwid=4)
           Call print_item (ig_uwnd, Trim(l_plevs%c)//': '//Trim(l_u%c), &
                itab=9, iwid=4)
           Call print_item (ig_vwnd, Trim(l_plevs%c)//': '//Trim(l_v%c), &
                itab=9, iwid=4)
           Call print_item (ig_uvwd, Trim(l_plevs%c)//': '//Trim(l_uv%c), &
                itab=9, iwid=4)
           Call print_item (      0, Trim(l_cancel%c), &
                itab=9, iwid=4)
           Call prompt_integer (' ', igrib)
           Select Case (igrib)
            Case (ig_ssts, ig_tave, ig_tmax, ig_tmin, ig_prcp, ig_uvwd, ig_uwnd, ig_vwnd, ig_gphs)
              idown = 1
              Exit getgrib3
            Case (0)
              idown = 0
              Exit getgrib3
            Case Default
              Cycle getgrib3
           End Select
        End Do getgrib3
     End Select
     If (idown == 1) idown = download_data() ! - download data -
  End Select
#endif
!
  Return
 End Subroutine menu_data
!
!
!
 Subroutine menu_edit (iopt)
!
! Constructs Edit menu
!
! Modules
  Use labels,      Only: l_datdomx, l_datdomy, l_edit, l_tseason
  Use gui,         Only: iseas, &
                         add_menu
  Use iofiles,     Only: xfile, yfile
  Use fields,      Only: get_xarea, get_yarea
  Use analysis,    Only: jcalc
  Use cpt_options, Only: get_season_opts
!
! Arguments
!
! Input/output scalars
  Integer, Intent(InOut) :: iopt ! - selected option -
!
! Executable Statements
!
! Construct Edit menu
#if GUI == 0
  Select Case (iopt)
   Case (200)
#endif
     Call add_menu ('mn', 1, 200, l_edit, &
          igrey=jcalc)
     Call add_menu ('mn', 2, 211, l_datdomx, &
          igrey=xfile%igeog, cbf=get_xarea)
     Call add_menu ('mn', 2, 212, l_datdomy, &
          cbf=get_yarea, igrey=yfile%igeog)
     Call add_menu ('mn', 2, 213, l_tseason, &
          lsep=.true., igrey=iseas, cbf=get_season_opts)
#if GUI == 0
   Case (211) ! - Change X data domain -
     If ((jcalc /= 0) .and. (xfile%igeog /= 0)) iopt = get_xarea()
   Case (212) ! - Change Y data domain -
     If ((jcalc /= 0) .and. (yfile%igeog /= 0)) iopt = get_yarea()
   Case (213) ! - Target season -
     If ((jcalc /= 0) .and. (iseas /= 0)) iopt = get_season_opts()
  End Select
#endif
!
  Return
 End Subroutine menu_edit
!
!
!
 Subroutine menu_acts (iopt)
!
! Constructs Actions menu
!
! Modules
  Use labels,      Only: l_actions, l_calculate, l_cvn, l_retro, l_reset, l_verify
  Use gui,         Only: ipfv, &
                         add_menu
  Use settings,    Only: iretro
  Use analysis,    Only: iaction, icalc, jcalc
  Use cpt_actions, Only: cpt_reset, run_analysis
!
! Arguments
!
! Input/output scalars
  Integer, Intent(InOut) :: iopt ! - selected option -
!
! Locals
!
! Local scalars
  Integer :: ilev ! - current menu-level -
!
! Executable Statements
!
! Construct Actions menu
#if GUI == 0
  Select Case (iopt)
   Case (300)
#endif
     Call add_menu ('mn', 1, 300, l_actions, &
          igrey=iaction)
     ilev = 2
     Call menu_acts_calculate ()
     Call add_menu ('mn', 2, 321, l_reset, &
          igrey=icalc, cbf=cpt_reset)
#if GUI == 0
   Case (310)
   Case (311) ! - Perform cross-validated analysis -
     If ((iaction /= 0) .and. (jcalc /= 0) .and. (ipfv%ioff /= 0)) Then
        iretro = 0
        iopt = run_analysis()
     End If
   Case (312) ! - Perform retroactive analysis -
     If ((iaction /= 0) .and. (jcalc /= 0) .and. (ipfv%ioff /= 0)) Then
        iretro = 1
        iopt = run_analysis()
     End If
   Case (313) ! - Verify -
     If ((iaction /= 0) .and. (jcalc /= 0) .and. (ipfv%ion /= 0)) iopt = run_analysis()
   Case (321) ! - Reset -
     If ((iaction /= 0) .and. (jcalc /= 1)) iopt = cpt_reset()
  End Select
#endif
!
  Return
!
 Contains
!
!
  Subroutine menu_acts_calculate ()
!
! Output Results sub-menu
!
#if GUI == 0
! Modules
  Use gui, Only: prompt_integer
!
#endif
! Executable Statements
!
! Construct File ~ Output Results menu
  Call add_menu ('mn', ilev, 310, l_calculate, &
       igrey=jcalc)
  Call add_menu ('mn', ilev+1, 311, l_cvn, &
       igrey=ipfv%ioff, iset1=iretro, ival1=0, cbf=run_analysis)
  Call add_menu ('mn', ilev+1, 312, l_retro, &
       igrey=ipfv%ioff, iset1=iretro, ival1=1, cbf=run_analysis)
  Call add_menu ('mn', ilev+1, 313, l_verify, &
       lsep=.true., igrey=ipfv%ion, cbf=run_analysis)
!
#if GUI == 0
! Prompt for option
  If (iopt == 310) Call prompt_integer (' ',iopt)
#endif
!
  Return
  End Subroutine menu_acts_calculate
 End Subroutine menu_acts
!
!
!
 Subroutine menu_tool (iopt)
!
! Constructs Tools menu
!
! Modules
#if GUI == 1
  Use data_cpt_constants, Only: nwr
#endif
  Use labels,             OnlY: l_attribs, l_averages, l_boot, l_climmaps, l_correls, l_ctables, l_cvn, l_ensembles, l_exceedps, &
                                l_fcasts, l_maps, l_modes, l_odds, l_performs, l_probs, l_regress, l_retro, l_rhitsdiag,         &
                                l_rocdiag, l_scoresp, l_screes, l_series, l_skillmaps, l_tenddiag, l_threshs, l_tools, l_valid,  &
                                l_values, l_verifn, l_wrlt
#if GUI == 1
  Use labels,             Only: l_ccamaps, l_loadsx, l_loadsy, l_probc, l_scatters, &
                                ls_wrlts
  Use gui,                Only: iyeof
#else
  Use labels,             Only: l_pvalues
#endif
  Use gui,                Only: ihind, im_modes, imlr, iscree, &
                                add_menu
  Use settings,           Only: iretro
#if GUI == 1
  Use iofiles,            Only: yfile
#endif
  Use analysis,           Only: icalc
#if GUI == 0
  Use validate,           Only: roc_cv, roc_ra
#endif
  Use validate,           Only: validate_cv, validate_ra
  Use bootstrap,          Only: bootstrap_cv, bootstrap_ra
#if GUI == 0
  Use bootstrap,          Only: ipval
#endif
  Use maps,               Only: map_averages, map_correls, map_skill_cv, map_skill_ra, map_thresholds
#if GUI == 1
  Use maps,               Only: map_pscores
  Use forecast,           Only: fcst_hps
#endif
  Use forecast,           Only: ifcast, &
                                fcst_fens, fcst_fps, fcst_fser, fcst_fval, fcst_odds, fcst_pexc
  Use eofs,               Only: scree_plots
#if GUI == 1
  Use eofs,               Only: map_ccas, map_xeofs, map_yeofs
  Use scatter,            Only: scatter_cv, scatter_ra
#endif
  Use scatter,            Only: regress
  Use tables,             Only: table_cv, table_ra
#if GUI == 1
  Use verif,              Only: iwrlt
#endif
  Use verif,              Only: irv, &
                                attrib_diagram, rhit_diagram, prob_scores, roc_diagram, tendency_diagram, wrlt_diagram
!
! Arguments
!
! Input/output scalars
  Integer, Intent(InOut) :: iopt ! - selected option -
!
! Locals
!
! Local scalars
  Integer :: ilev ! - current menu-level -
!
! Executable Statements
!
! Construct Tools menu
#if GUI == 0
  Select Case (iopt)
   Case (400)
#endif
! - Tools -
     Call add_menu ('mn', 1, 400, l_tools, &
          igrey=icalc)
! - Validation
     ilev = 2
     Call menu_tool_validn ()
! - Verification -
     ilev = 2
     Call menu_tool_verifn ()
! - Modes -
     ilev = 2
     Call menu_tool_modes ()
! - Regression -
     Call add_menu ('mn', 2, 471, l_regress, &
          igrey=imlr%ion, cbf=regress)
! - Climatological Maps -
     ilev = 2
     Call menu_tool_clim ()
! - Forecasts -
     ilev = 2
     Call menu_tool_fcast ()
#if GUI == 0
   Case (410, 420) ! - Validation -
     ilev = 1
     Call menu_tool_validn ()
   Case (411) ! - Cross-validated performance measures -
     If ((icalc /= 0) .and. (ihind /= 0)) iopt = validate_cv()
   Case (412) ! - Cross-validated bootstrapping and significance testing -
     If ((icalc /= 0) .and. (ihind /= 0)) iopt = bootstrap_cv()
   Case (413) ! - Cross-validated skill maps -
     If ((icalc /= 0) .and. (ihind /= 0)) iopt = map_skill_cv()
   Case (414) ! - Cross-validated ROC -
     If ((icalc /= 0) .and. (ihind /= 0)) iopt = roc_cv()
   Case (415) ! - Cross-validated contingency tables -
     If ((icalc /= 0) .and. (ihind /= 0)) iopt = table_cv()
   Case (421) ! - Retroactive performance measures -
     If (icalc == 0) Return
     If (ihind == 0) Return
     If ((icalc /= 0) .and. (ihind /= 0) .and. (iretro == 1)) iopt = validate_ra()
   Case (422) ! - Retroactive bootstrapping and significance testing -
     If ((icalc /= 0) .and. (ihind /= 0) .and. (iretro == 1)) iopt = bootstrap_ra()
   Case (423) ! - Retroactive skill maps -
     If ((icalc /= 0) .and. (ihind /= 0) .and. (iretro == 1)) iopt = map_skill_ra()
   Case (424) ! - Retroactive ROC -
     If ((icalc /= 0) .and. (ihind /= 0) .and. (iretro == 1)) iopt = roc_ra()
   Case (425) ! - Retroactive contingency tables -
     If ((icalc /= 0) .and. (ihind /= 0) .and. (iretro == 1)) iopt = table_ra()
   Case (430) ! - Verification -
     ilev = 1
     Call menu_tool_verifn ()
   Case (431) ! - Attributes diagram -
     If ((icalc /= 0) .and. (iretro == 1)) iopt = attrib_diagram()
   Case (432) ! - ROC diagram -
     If ((icalc /= 0) .and. (irv == 1)) iopt = roc_diagram()
   Case (433) ! - Probabilistic verification scores -
     If ((icalc /= 0) .and. (irv == 1)) iopt = prob_scores()
   Case (434) ! - Weather roulette -
     If ((icalc /= 0) .and. (irv == 1)) iopt = wrlt_diagram()
   Case (435) ! - tendency diagram -
     If ((icalc /= 0) .and. (irv == 1)) iopt = tendency_diagram()
   Case (436) ! - ranked hits diagram -
     If ((icalc /= 0) .and. (irv == 1)) iopt = rhit_diagram()
   Case (440) ! - Modes -
     ilev = 1
     Call menu_tool_modes ()
   Case (441) ! - Scree plots -
     If ((icalc /= 0) .and. (iscree == 1)) iopt = scree_plots()
   Case (450) ! - Forecasts -
     ilev = 1
     Call menu_tool_fcast ()
   Case (451) ! - Forecast series -
     If ((icalc /= 0) .and. (ifcast == 1)) iopt = fcst_fser()
   Case (452) ! - Forecast ensembles -
     If ((icalc /= 0) .and. (ifcast == 1)) iopt = fcst_fens()
   Case (453) ! - Exceedance probabilities -
     If ((icalc /= 0) .and. (ifcast == 1)) iopt = fcst_pexc()
   Case (454) ! - Forecast values -
     If ((icalc /= 0) .and. (ifcast == 1)) iopt = fcst_fval()
   Case (455) ! - Forecast probabilities -
     If ((icalc /= 0) .and. (ifcast == 1)) iopt = fcst_fps()
   Case (456) ! - Forecast odds -
     If ((icalc /= 0) .and. (ifcast == 1)) iopt = fcst_odds()
   Case (460) ! - Climatological Maps -
     ilev = 1
     Call menu_tool_verifn ()
   Case (461) ! - Climatological thresholds maps -
     If (icalc /= 0) iopt = map_thresholds()
   Case (462) ! - Climatological averages maps -
     If (icalc /= 0) iopt = map_averages()
   Case (463) ! - Correlation maps -
     If ((icalc /= 0) .and. (ihind /= 0)) iopt = map_correls()
   Case (471) ! - Regression -
     If ((icalc /= 0) .and. (imlr%ion /= 0)) iopt = regress()
  End Select
#endif
!
  Return
!
 Contains
!
!
  Subroutine menu_tool_validn ()
!
! Tools ~ Validation sub-menu
!
#if GUI == 0
! Modules
  Use gui, Only: prompt_integer
!
#endif
! Executable Statements
!
! Construct Tools ~ Validation menu
  Call add_menu ('mn',   ilev, 410, l_valid, &
       igrey=ihind)
! - cross-validated -
  Call add_menu ('mn', ilev+1, 410, l_cvn)
  Call add_menu ('mn', ilev+2, 411, l_performs, &
       cbf=validate_cv)
  Call add_menu ('mn', ilev+2, 412, l_boot, &
       cbf=bootstrap_cv)
#if GUI == 0
  Select Case (ipval)
   Case (0)
#endif
     Call add_menu ('mn', ilev+2, 413, l_skillmaps, &
          cbf=map_skill_cv)
#if GUI == 0
   Case (1)
     Call add_menu ('mn', ilev+2, 413, l_pvalues, &
          cbf=map_skill_cv)
  End Select
  Call add_menu ('mn', ilev+2, 414, l_rocdiag, &
       cbf=roc_cv)
#else
  Call add_menu ('mn', ilev+2, 416, l_scatters, &
       cbf=scatter_cv)
#endif
  Call add_menu ('mn', ilev+2, 415, l_ctables, &
       cbf=table_cv)
! - retroactive -
  Call add_menu ('mn', ilev+1, 420, l_retro, &
       igrey=iretro)
  Call add_menu ('mn', ilev+2, 421, l_performs, &
       cbf=validate_ra)
  Call add_menu ('mn', ilev+2, 422, l_boot, &
       cbf=bootstrap_ra)
#if GUI == 0
  Select Case (ipval)
   Case (0)
#endif
     Call add_menu ('mn', ilev+2, 423, l_skillmaps, &
          cbf=map_skill_ra)
#if GUI == 0
   Case (1)
     Call add_menu ('mn', ilev+2, 423, l_pvalues, &
          cbf=map_skill_ra)
  End Select
  Call add_menu ('mn', ilev+2, 424, l_rocdiag, &
       cbf=roc_ra)
#else
  Call add_menu ('mn', ilev+2, 426, l_scatters, &
       cbf=scatter_ra)
#endif
  Call add_menu ('mn', ilev+2, 425, l_ctables, &
       cbf=table_ra)
!
#if GUI == 0
! Prompt for option
  If (iopt == 430) Call prompt_integer (' ',iopt)
#endif
!
  Return
  End Subroutine menu_tool_validn
!
!
!
  Subroutine menu_tool_verifn ()
!
! Tools ~ Verification sub-menu
!
#if GUI == 0
! Modules
  Use gui, Only: prompt_integer
!
#else
! Locals
!
! Local scalars
  Integer :: i ! - weather roulette index -
!
#endif
! Executable Statements
!
! Construct Tools ~ Verification menu
  Call add_menu ('mn',   ilev, 430, l_verifn, &
       igrey=irv)
  Call add_menu ('mn', ilev+1, 431, l_attribs, &
       cbf=attrib_diagram)
  Call add_menu ('mn', ilev+1, 432, l_rocdiag, &
       cbf=roc_diagram)
  Call add_menu ('mn', ilev+1, 433, l_scoresp, &
       cbf=prob_scores)
#if GUI == 1
  Call add_menu ('mn', ilev+1,   3, l_skillmaps, &
       cbf=map_pscores)
  Call add_menu ('mn', ilev+1, 434, l_wrlt)
  Do i = 1, nwr
     Call add_menu ('mn', ilev+2,   i, ls_wrlts(i), &
          iset1=iwrlt, ival1=i, cbf=wrlt_diagram)
  End Do
#else
  Call add_menu ('mn', ilev+1, 434, l_wrlt, &
       cbf=wrlt_diagram)
#endif
  Call add_menu ('mn', ilev+1, 435, l_tenddiag, &
       cbf=tendency_diagram)
  Call add_menu ('mn', ilev+1, 436, l_rhitsdiag, &
       cbf=rhit_diagram)
!
#if GUI == 0
! Prompt for option
  If (iopt == 430) Call prompt_integer (' ',iopt)
#endif
!
  Return
  End Subroutine menu_tool_verifn
!
!
!
  Subroutine menu_tool_modes ()
!
! Tools ~ Modes sub-menu
!
#if GUI == 0
! Modules
  Use gui, Only: prompt_integer
!
#endif
! Executable Statements
!
! Construct Tools ~ Modes menu
  Call add_menu ('mn', ilev,   440, l_modes, &
       igrey=im_modes)
  Call add_menu ('mn', ilev+1, 441, l_screes, &
       igrey=iscree, cbf=scree_plots)
#if GUI == 1
  Call add_menu ('mn', ilev+1, 442, l_loadsx, &
       cbf=map_xeofs)
  Call add_menu ('mn', ilev+1, 443, l_loadsy, &
       igrey=iyeof, cbf=map_yeofs)
  Call add_menu ('mn', ilev+1, 444, l_ccamaps, &
       igrey=iyeof, cbf=map_ccas)
!
#else
! Prompt for option
  If (iopt == 440) Call prompt_integer (' ',iopt)
#endif
!
  Return
  End Subroutine menu_tool_modes
!
!
!
  Subroutine menu_tool_fcast ()
!
! Tools ~ Forecasts sub-menu
!
#if GUI == 0
! Modules
  Use gui, Only: prompt_integer
!
#endif
! Executable Statements
!
! Construct Tools ~ Forecasts menu
  Call add_menu ('mn', 2, 450, l_fcasts, &
       igrey=ifcast)
  Call add_menu ('mn', 3, 451, l_series, &
       igrey=ihind, cbf=fcst_fser)
  Call add_menu ('mn', 3, 452, l_ensembles, &
       igrey=ihind, cbf=fcst_fens)
  Call add_menu ('mn', 3, 453, l_exceedps, &
       igrey=ihind, cbf=fcst_pexc)
  Call add_menu ('mn', 3, 450, l_maps)
  Call add_menu ('mn', 4, 454, l_values, &
       igrey=ihind, cbf=fcst_fval)
  Call add_menu ('mn', 4, 455, l_probs, &
       cbf=fcst_fps)
#if GUI == 1
  Call add_menu ('mn', 4, 457, l_probc, &
       igrey=yfile%igeog, cbf=fcst_hps)
#endif
  Call add_menu ('mn', 4, 456, l_odds, &
       cbf=fcst_odds)
!
#if GUI == 0
! Prompt for option
  If (iopt == 450) Call prompt_integer (' ',iopt)
#endif
!
  Return
  End Subroutine menu_tool_fcast
!
!
!
  Subroutine menu_tool_clim ()
!
! Tools ~ Climatological Maps sub-menu
!
#if GUI == 0
! Modules
  Use gui, Only: prompt_integer
!
#endif
! Executable Statements
!
! Construct Tools ~ Climatological Maps menu
  Call add_menu ('mn',   ilev, 460, l_climmaps)
  Call add_menu ('mn', ilev+1, 461, l_threshs, &
       cbf=map_thresholds)
  Call add_menu ('mn', ilev+1, 462, l_averages, &
       cbf=map_averages)
  Call add_menu ('mn', ilev+1, 463, l_correls, &
       igrey=ihind, cbf=map_correls)
!
#if GUI == 0
! Prompt for option
  If (iopt == 460) Call prompt_integer (' ',iopt)
#endif
!
  Return
  End Subroutine menu_tool_clim
 End Subroutine menu_tool
!
!
!
 Subroutine menu_opts (iopt, &
            ioc)
!
! Constructs Options menu
!
! Modules
#if GUI == 1
  Use data_text,          Only: cs_langlist, c_tab, ilang, nlang
  Use data_cpt_constants, Only: ia_mlr
#endif
  Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_gcm
#if GUI == 1
  Use time,               Only: iseq
#endif
  Use labels,             Only: l_ccaopts, l_climper, l_cvopts, l_data, l_fcastsets, l_gcmopts, l_goodness, l_lang, l_missings, &
                                l_modeoptsx, l_modeoptsy, l_options, l_resamples, l_sortens, l_synchps, l_tailor, l_timesets,   &
                                l_transforms, l_transformy, l_verifsets, l_zerob
#if GUI == 1
  Use labels,             Only: l_beginner, l_bw, l_catshade, l_config, l_console, l_crosses, l_expert, l_expertlev, l_fntscal,    &
                                l_graphics, l_graphscal, l_masklake, l_maskland, l_models, l_reversecols, l_scorgrphs, l_showdays, &
                                l_stnmrks, l_txtscal, l_vlines, &
                                ls_analyses, &
                                ca_atypes_a
  Use gui,                Only: icbw, icca, icrev, igcm, imlr, ipcr, jel
#endif
  Use gui,                Only: igcmo, ihind, itrns, ixeof, iyeof, jgauss, &
                                add_menu
  Use settings,           Only: igauss, isem, isynch, izero, &
                                change_igauss, record_change, set_zero
#if GUI == 1
  Use settings,           Only: iel
#else
  Use iofiles,            Only: cxy
  Use pcs,                Only: ieofp, ieofx, ieofy, &
                                advanced_cca, advanced_eof
#endif
  Use climate,            Only: iclim
  Use analysis,           Only: jcalc
#if GUI == 1
  Use analysis,           Only: ianaln
  Use graphics,           Only: icross, ilake, iland, iscore, itsh, ivl
  Use cpt_view,           Only: change_view
  Use cpt_options,        Only: get_console_opts, get_gfont_opts, get_gsize_opts, get_marker_opts, get_text_opts
#else
  Use analysis,           Only: ianal
  Use bootstrap,          Only: ipval
  Use cpt_options,        Only: change_language, get_error_opts
#endif
  Use cpt_options,        Only: get_boot_opts, get_cca_opts, get_climatology_opts, get_cv_opts, get_eofx_opts, &
                                get_eofy_opts, get_fcast_opts, get_gcm_opts, get_goodness_opts, get_missing_opts,                &
                                get_tailoring_opts, get_time_opts, get_transform_opts, get_verif_opts
!
! Arguments
!
! Input/output scalars
  Integer, Intent(InOut) :: iopt ! - selected option -
!
! Output scalars
! - optional output scalars -
  Integer, Intent(Out), Optional :: ioc ! - changed option -
!
! Locals
!
! Local scalars
  Integer :: ilev ! - current menu-level -
#if GUI == 1
  Integer :: i    ! - index -
!
  Character(Len=8) :: c_anala ! - analysis abbreviations -
!
#endif
! Executable Statements
!
! Construct Options menu
! - short menu -
  Select Case (iopt)
   Case (-500)
     Call add_menu ('mn', 1,   5, l_options)
#if GUI == 1
     Call add_menu ('mn', 2, 511, l_lang)
     Do i = 1, nlang
        Call add_menu ('mn', 3,   i, cs_langlist(i), &
             iset1=ioc, ival1=1, iset2=ilang, ival2=i, scbf='EXIT')
     End Do
     Call add_menu ('mn', 2, 581, l_expertlev)
     Call add_menu ('mn', 3,   1, l_beginner, &
          igrey=iel, iset1=ioc, ival1=1, itoggle=iel, scbf='EXIT')
     Call add_menu ('mn', 3,   2, l_expert, &
          igrey=jel, iset1=ioc, ival1=1, itoggle=iel, scbf='EXIT')
     ilev = 2
     Call menu_opts_config ()
#else
     Call add_menu ('mn', 2, 511, l_lang, &
          cbf=change_language)
     Call add_menu ('mn', 2, 571, 'Error settings', &
          cbf=get_error_opts)
     ioc = 0
#endif
   Case (500)
     Call add_menu ('mn', 1,   5, l_options)
#if GUI == 0
     Call add_menu ('mn', 2, 511, l_lang, &
          cbf=change_language)
#endif
#if GUI == 1
     Call add_menu ('mn', 2, 521, l_modeoptsx, &
          igrey=ixeof, cbf=get_eofx_opts)
     Call add_menu ('mn', 2, 523, l_modeoptsy, &
          igrey=iyeof, cbf=get_eofy_opts)
     Call add_menu ('mn', 2, 525, l_ccaopts, &
          igrey=iyeof, cbf=get_cca_opts)
#else
     Call add_menu ('mn', 2, 521, 'Number of X modes', &
          igrey=ixeof, cbf=get_eofx_opts)
     Call add_menu ('mn', 2, 522, l_modeoptsx, &
          igrey=ixeof, cbf=advanced_eof)
     Call add_menu ('mn', 2, 523, 'Number of Y modes', &
          igrey=iyeof, cbf=get_eofy_opts)
     Call add_menu ('mn', 2, 524, l_modeoptsy, &
          igrey=iyeof, cbf=advanced_eof)
     Call add_menu ('mn', 2, 525, 'Number of CCA modes', &
          igrey=iyeof, cbf=get_cca_opts)
     Call add_menu ('mn', 2, 526, l_ccaopts, &
          igrey=iyeof, cbf=advanced_cca)
#endif
     Call add_menu ('mn', 2, 527, l_gcmopts, &
          igrey=igcmo, cbf=get_gcm_opts)
#if GUI == 1
     Call add_menu ('mn', 2, 528, l_models, &
          igrey=jel)
     c_anala = c_tab//'('//ca_atypes_a(ia_cca)//')'
     Call add_menu ('mn', 3, ia_cca, ls_analyses(ia_cca), &
          igrey=icca%iacc, iset1=ianaln, ival1=ia_cca, iset2=iyeof, ival2=1, cbf=change_view, c_suf=c_anala)
     c_anala = c_tab//'('//ca_atypes_a(ia_pcr)//')'
     Call add_menu ('mn', 3, ia_pcr, ls_analyses(ia_pcr), &
          igrey=ipcr%iacc, iset1=ianaln, ival1=ia_pcr, iset2=iyeof, ival2=0, cbf=change_view, c_suf=c_anala)
     c_anala = c_tab//'('//ca_atypes_a(ia_mlr)//')'
     Call add_menu ('mn', 3, ia_mlr, ls_analyses(ia_mlr), &
          igrey=imlr%iacc, iset1=ianaln, ival1=ia_mlr, iset2=iyeof, ival2=0, cbf=change_view, c_suf=c_anala)
     c_anala = c_tab//'('//ca_atypes_a(ia_gcm)//')'
     Call add_menu ('mn', 3, ia_gcm, ls_analyses(ia_gcm), &
          igrey=igcm%iacc, iset1=ianaln, ival1=ia_gcm, iset2=iyeof, ival2=0, cbf=change_view, c_suf=c_anala)
#endif
     Call add_menu ('mn', 2, 531, l_goodness, &
          lsep=.true., igrey=jcalc, cbf=get_goodness_opts)
     Call add_menu ('mn', 2, 532, l_climper, &
          lsep=.true., igrey=iclim, cbf=get_climatology_opts)
     Call add_menu ('mn', 2, 533, l_tailor, &
          igrey=ihind, cbf=get_tailoring_opts)
     Call add_menu ('mn', 2, 534, l_cvopts, &
          igrey=ihind, cbf=get_cv_opts)
     Call add_menu ('mn', 2, 551, l_resamples, &
          cbf=get_boot_opts)
     Call add_menu ('mn', 2, 552, l_fcastsets, &
          cbf=get_fcast_opts)
     Call add_menu ('mn', 2, 553, l_verifsets, &
          cbf=get_verif_opts)
     Call add_menu ('mn', 2, 554, l_transforms, &
          igrey=itrns, cbf=get_transform_opts)
     Call add_menu ('mn', 2, 555, l_timesets, &
          cbf=get_time_opts)
     ilev = 2
     Call menu_opts_data ()
#if GUI == 1
     ilev = 2
     Call menu_opts_graph ()
#else
     Call add_menu ('mn', 2, 561, 'Skill map p-values', &
          icheck=ipval, itoggle=ipval)
     Call add_menu ('mn', 2, 571, 'Error settings', &
          cbf=get_error_opts)
   Case (511, 11) ! - Change language -
     iopt = change_language()
   Case (521) ! - Change numbers of X modes / GCM settings -
     Select Case (ianal)
      Case (ia_cca, ia_pcr)
        iopt = get_eofx_opts()
      Case Default
        Return
     End Select
   Case (522) ! - Change X modes options -
     If (ianal == ia_cca .or. ianal == ia_pcr) Then
        cxy = 'X'
        ieofp => ieofx
        iopt = advanced_eof()
     End If
   Case (523) ! - Change numbers of Y modes -
     If (ianal == ia_cca) iopt = get_eofy_opts()
   Case (524) ! - Change Y modes options -
     If (ianal == ia_cca) Then
        cxy = 'Y'
        ieofp => ieofy
        iopt = advanced_eof()
     End If
   Case (525) ! - Change numbers of CCA modes -
     If (ianal == ia_cca) iopt = get_cca_opts()
   Case (526) ! - Change CCA options -
     If (ianal == ia_cca) iopt = advanced_cca()
   Case (527) ! - Change GCM options -
     If (ianal == ia_gcm) iopt = get_gcm_opts()
   Case (531) ! - Goodness index options
     iopt = get_goodness_opts()
   Case (532) ! - Set climatological period -
     If (iclim /= 0) iopt = get_climatology_opts()
   Case (533) ! - Tailoring -
     iopt = get_tailoring_opts()
   Case (534, 8) ! - Length of cross-validation window -
     iopt = get_cv_opts()
   Case (540) ! - Data -
     ilev = 1
     Call menu_opts_data ()
   Case (541) ! - transform Y data -
     If (jcalc /= 0) iopt = change_igauss()
   Case (542) ! - zero-bound -
     If (jcalc /= 0) iopt = set_zero()
   Case (543) ! - set sort-ensemble members flag -
     If (jcalc /= 0) isem = 1 - isem
   Case (544) ! - missing value options -
     If (jcalc /= 0) iopt = get_missing_opts()
   Case (545) ! - allow synchronous predictors flag -
     If (jcalc /= 0) isynch = 1 - isynch
   Case (551) ! - bootstrap settings -
     iopt = get_boot_opts()
   Case (552) ! - forecast settings -
     iopt = get_fcast_opts()
   Case (553) ! - verification settings -
     iopt = get_verif_opts()
   Case (554) ! - transformation settings -
     If (itrns /= 0) iopt = get_transform_opts()
   Case (555) ! - time settings -
     iopt = get_time_opts()
   Case (561) ! - p-values -
     ipval = 1 - ipval
   Case (571) ! - error settings -
     iopt = get_error_opts()
#endif
  End Select
!
  Return
!
 Contains
!
!
  Subroutine menu_opts_data ()
!
! Options ~ Data sub-menu
!
#if GUI == 0
! Modules
  Use gui, Only: prompt_integer
!
#endif
! Executable Statements
!
! Construct Options ~ Data menu
  Call add_menu ('mn',   ilev, 540, l_data, &
       lsep=.true., igrey=jcalc)
  Call add_menu ('mn', ilev+1, 541, l_transformy, &
       icheck=igauss, igrey=jgauss, cbf=change_igauss)
  Call add_menu ('mn', ilev+1, 542, l_zerob, &
       icheck=izero, cbf=set_zero)
  Call add_menu ('mn', ilev+1, 543, l_sortens, &
       icheck=isem, itoggle=isem, cbf=record_change)
  Call add_menu ('mn', ilev+1, 545, l_synchps, &
       icheck=isynch, itoggle=isynch, cbf=record_change)
  Call add_menu ('mn', ilev+1, 544, l_missings, &
       cbf=get_missing_opts)
!
#if GUI == 0
! Prompt for option
  If (iopt == 540) Call prompt_integer (' ',iopt)
#endif
!
  Return
  End Subroutine menu_opts_data
#if GUI == 1
!
!
!
  Subroutine menu_opts_config ()
!
! Options ~ Configuration sub-menu
!
! Executable Statements
!
! Construct Options ~ Configuration menu
  Call add_menu ('mn', ilev, 590, l_config)
  Call add_menu ('mn', ilev+1, 591, l_showdays, &
       icheck=iseq, itoggle=iseq)
  Call add_menu ('mn', ilev+1, 592, l_txtscal, &
       iset1=ioc, ival1=1, cbf=get_text_opts, scbf='EXIT')
  Call add_menu ('mn', ilev+1, 593, l_console, &
       iset1=ioc, ival1=1, cbf=get_console_opts, scbf='EXIT')
!
  Return
  End Subroutine menu_opts_config
!
!
!
  Subroutine menu_opts_graph ()
!
! Options ~ Graphics sub-menu
!
! Executable Statements
!
! Construct Options ~ Graphics menu
  Call add_menu ('mn',   ilev, 580, l_graphics, &
       lsep=.true.)
  Call add_menu ('mn', ilev+1, 581, l_stnmrks, &
       cbf=get_marker_opts)
  Call add_menu ('mn', ilev+1, 582, l_fntscal, &
       cbf=get_gfont_opts)
  Call add_menu ('mn', ilev+1, 583, l_graphscal, &
       cbf=get_gsize_opts)
  Call add_menu ('mn', ilev+1, 584, l_crosses, &
       lsep=.true., icheck=icross, itoggle=icross)
  Call add_menu ('mn', ilev+1, 585, l_scorgrphs, &
       icheck=iscore, itoggle=iscore)
  Call add_menu ('mn', ilev+1, 586, l_maskland, &
       icheck=iland, itoggle=iland)
  Call add_menu ('mn', ilev+1, 587, l_masklake, &
       icheck=ilake, itoggle=ilake)
  Call add_menu ('mn', ilev+1, 588, l_bw, &
       icheck=icbw, itoggle=icbw)
  Call add_menu ('mn', ilev+1, 589, l_reversecols, &
       icheck=icrev, itoggle=icrev)
  Call add_menu ('mn', ilev+1, 590, l_vlines, &
       icheck=ivl, itoggle=ivl)
  Call add_menu ('mn', ilev+1, 591, l_catshade, &
       icheck=itsh, itoggle=itsh)
!
  Return
  End Subroutine menu_opts_graph
#endif
 End Subroutine menu_opts
!
!
!
 Subroutine menu_view (iopt)
!
! Constructs View menu
!
! Modules
  Use data_text,          Only: c_tab
  Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv, &
                                ia_dia, ia_for
  Use labels,             Only: ca_atypes_a, &
                                l_diagnostics, l_fcast, l_verifn, l_view, &
                                ls_analyses
  Use gui,                Only: icca, igcm, imlr, ipcr, ipfv, iyeof, jdia, jfor, &
                                add_menu
  Use settings,           Only: iel, isynch
  Use analysis,           Only: ianal, ianaln, jcalc
  Use cpt_view,           Only: change_view
!
! Arguments
!
! Input/output scalars
  Integer, Intent(InOut) :: iopt ! - selected option -
!
  Character(Len=8) :: c_anala ! - analysis abbreviations -
!
! Locals
!
! Local scalars
  Integer :: itab ! - tab position -
!
! Executable Statements
!
! Construct View menu
  Select Case (iopt)
! - short menu -
   Case (-600)
     Call add_menu ('mn', 1,   6, l_view)
     Select Case (iel)
      Case (1) ! - expert level -
        itab = MaxVal(Len_Trim(ls_analyses(:)%c)) + 1
        c_anala = c_tab//'('//ca_atypes_a(ia_cca)//')'
        Call add_menu ('mn', 2, 611, ls_analyses(ia_cca), &
             iset1=ianal, ival1=ia_cca, scbf='EXIT', c_suf=c_anala, itab=itab)
        c_anala = c_tab//'('//ca_atypes_a(ia_pcr)//')'
        Call add_menu ('mn', 2, 612, ls_analyses(ia_pcr), &
             iset1=ianal, ival1=ia_pcr, scbf='EXIT', c_suf=c_anala, itab=itab)
        c_anala = c_tab//'('//ca_atypes_a(ia_mlr)//')'
        Call add_menu ('mn', 2, 613, ls_analyses(ia_mlr), &
             iset1=ianal, ival1=ia_mlr, scbf='EXIT', c_suf=c_anala, itab=itab)
        c_anala = c_tab//'('//ca_atypes_a(ia_gcm)//')'
        Call add_menu ('mn', 2, 614, ls_analyses(ia_gcm), &
             iset1=ianal, ival1=ia_gcm, scbf='EXIT', c_suf=c_anala, itab=itab)
        c_anala = c_tab//'('//ca_atypes_a(ia_pfv)//')'
        Call add_menu ('mn', 2, 621, ls_analyses(ia_pfv), &
             iset1=ianal, ival1=ia_pfv, scbf='EXIT', c_suf=c_anala, itab=itab)
      Case (0) ! - beginner level -
        Call add_menu ('mn', 2, 631, l_fcast, &
             iset1=ianal, ival1=ia_cca, iset2=isynch, ival2=0, scbf='EXIT')
        Call add_menu ('mn', 2, 632, l_diagnostics, &
             iset1=ianal, ival1=ia_cca, iset2=isynch, ival2=1, scbf='EXIT')
        Call add_menu ('mn', 2, 621, l_verifn, &
             iset1=ianal, ival1=ia_pfv, scbf='EXIT')
     End Select
#if GUI == 0
     ianal = 0
#endif
! - extended menu -
   Case (600)
     Call add_menu ('mn', 1,   6, l_view, &
          igrey=jcalc)
     Select Case (iel)
      Case (1) ! - expert level -
        itab = MaxVal(Len_Trim(ls_analyses(:)%c)) + 1
        c_anala = c_tab//'('//ca_atypes_a(ia_cca)//')'
        Call add_menu ('mn', 2, 611, ls_analyses(ia_cca), &
             igrey=icca%iacc, iset1=ianaln, ival1=ia_cca, cbf=change_view, c_suf=c_anala, itab=itab)
        c_anala = c_tab//'('//ca_atypes_a(ia_pcr)//')'
        Call add_menu ('mn', 2, 612, ls_analyses(ia_pcr), &
             igrey=ipcr%iacc, iset1=ianaln, ival1=ia_pcr, cbf=change_view, c_suf=c_anala, itab=itab)
        c_anala = c_tab//'('//ca_atypes_a(ia_mlr)//')'
        Call add_menu ('mn', 2, 613, ls_analyses(ia_mlr), &
             igrey=imlr%iacc, iset1=ianaln, ival1=ia_mlr, cbf=change_view, c_suf=c_anala, itab=itab)
        c_anala = c_tab//'('//ca_atypes_a(ia_gcm)//')'
        Call add_menu ('mn', 2, 614, ls_analyses(ia_gcm), &
             igrey=igcm%iacc, iset1=ianaln, ival1=ia_gcm, cbf=change_view, c_suf=c_anala, itab=itab)
        c_anala = c_tab//'('//ca_atypes_a(ia_pfv)//')'
        Call add_menu ('mn', 2, 621, ls_analyses(ia_pfv), &
             igrey=ipfv%iacc, iset1=ianaln, ival1=ia_pfv, cbf=change_view, c_suf=c_anala, itab=itab)
      Case (0) ! - beginner level -
        Call add_menu ('mn', 2, 631, l_fcast, &
             igrey=jfor, iset1=ianaln, ival1=ia_for, iset2=jfor, ival2=0, iset3=jdia, ival3=1, cbf=change_view)
        Call add_menu ('mn', 2, 632, l_diagnostics, &
             igrey=jdia, iset1=ianaln, ival1=ia_dia, iset2=jfor, ival2=1, iset3=jdia, ival3=0, cbf=change_view)
        Call add_menu ('mn', 2, 621, l_verifn, &
             igrey=jdia, iset1=ianaln, ival1=ia_pfv, iset2=iyeof, ival2=0, cbf=change_view)
     End Select
#if GUI == 0
   Case (611) ! - CCA -
     If ((jcalc /= 0) .and. (icca%iacc /= 0)) Then
        ianaln = ia_cca
        iopt = change_view()
     End If
   Case (612) ! - PCR -
     If ((jcalc /= 0) .and. (ipcr%iacc /= 0)) Then
        ianaln = ia_pcr
        iopt = change_view()
     End If
   Case (613) ! - MLR -
     If ((jcalc /= 0) .and. (imlr%iacc /= 0)) Then
        ianaln = ia_mlr
        iopt = change_view()
     End If
   Case (614) ! - GCM -
     If ((jcalc /= 0) .and. (igcm%iacc /= 0)) Then
        ianaln = ia_gcm
        iopt = change_view()
     End If
   Case (621) ! - PFV -
     If ((jcalc /= 0) .and. (ipfv%iacc /= 0)) Then
        ianaln = ia_pfv
        iopt = change_view()
     End If
#endif
  End Select
!
  Return
 End Subroutine menu_view
!
!
!
 Subroutine menu_help (iopt)
!
! Modules
  Use labels, Only: l_about, l_help
  Use gui,    Only: add_menu
#if GUI == 1
  Use help,   Only: help_all
#endif
  Use help,   Only: help_about
!
! Arguments
!
! Input scalars
  Integer, Intent(InOut) :: iopt ! - selected option -
!
! Executable Statements
!
! Construct Help menu
#if GUI == 0
  Select Case (iopt)
   Case (700)
#endif
     Call add_menu ('mn', 1,   7, l_help)
#if GUI == 1
     Call add_menu ('mn', 2, 712, l_help, &
          cbf=help_all)
#endif
     Call add_menu ('mn', 2, 711, l_about, &
          cbf=help_about)
#if GUI == 0
   Case (711) ! - help about -
     iopt = help_about()
  End Select
#endif
!
  Return
 End Subroutine menu_help
!
!
!
 Function output_results()
!
! Outputs results
!
! Modules
  Use cpt_output, Only: save_data
  Use forecast,   Only: ifc
!
! Function type
  Integer :: output_results
!
! Executbale Statements
!
! Output results
  output_results = save_data(ifc)
!
  Return
 End Function output_results
End Module cpt_interface
