! Author: Simon Mason
Module eofs
!
! Modules
  Use data_numbers, Only: rp
#if GUI == 1
  Use graphics,     Only: image
#endif
!
! Declarations
!
! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: scree_plots
#if GUI == 1
  Public :: init_img_eofs, map_ccas, map_xeofs, map_yeofs
!
! Scalars
!
! Integer scalars
  Integer, Public :: icum   ! - cumulative % variance flag -
  Integer, Public :: ilogsp ! - log axis for scree plot flag -
  Integer, Public :: istick ! - broken-stick flag -
!
  Real(Kind=rp), Private :: xtvar ! - total x variance -
  Real(Kind=rp), Private :: ytvar ! - total y variance -
!
! Arrays
!
! Derived-type arrays
  Type(image), DIMenSION(2), Private :: img_scree ! - eigenvalues scree plots -
  Type(image), Dimension(2), Private :: img_xeof  ! - X EOF loadings and scores -
  Type(image), Dimension(2), Private :: img_yeof  ! - Y EOF loadings and scores -
!
  Type(image), Dimension(3), Private :: img_ccas  ! - CCA loadings and scores graphs -
!
#endif
Contains
!
!
#if GUI == 1
 Subroutine init_img_eofs (igsize, ihsize)
!
! Initialises EOF graphics
!
! Modules
  Use graphics, Only: init_image
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: igsize ! - graphics area dimension -
  Integer, Intent(In) :: ihsize ! - reduced graphics area dimension -
!
! Executable Statements
!
! Initialise images
  Call init_image (img_scree(1), (igsize*3)/2)
  Call init_image (img_scree(2), (igsize*3)/2)
  Call init_image (img_xeof(1), (igsize*3)/2)
  Call init_image (img_xeof(2), (igsize*3)/2)
  Call init_image (img_yeof(1), (igsize*3)/2)
  Call init_image (img_yeof(2), (igsize*3)/2)
  Call init_image (img_ccas(1), ihsize)
  Call init_image (img_ccas(2), ihsize)
  Call init_image (img_ccas(3), ihsize)
!
! Initialise options
! - no cumulative % variance -
  icum = 0
! - linear y-axis on scree plot -
  ilogsp = 0
! - no broken stick -
  istick = 0
!
  Return
 End Subroutine init_img_eofs
!
!
!
 Function scree_plots() &
          Result (plots)
!
! Opens window for scree plots
!
! Modules
  Use data_cpt_constants, Only: ia_cca
  Use labels,             Only: l_lab, &
                                l_screes, l_screex, l_screey
  Use pcs,                Only: npx, npy
  Use analysis,           Only: ianal
  Use windows_gui,        Only: imgid, &
                                init_window_graphic
!
! Function type
  Integer :: plots
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Set image title
  img_scree(1)%title = Trim(l_screex%c)
  img_scree(2)%title = Trim(l_screey%c)
!
! Plot graphs
  If (npx > 1) Then
     If ((ianal == ia_cca) .and. (npy > 1)) Then
        plots = init_window_graphic(2, img_scree(:), imgid, l_screes%c, (/l_screex, l_screey/), &
                draw_screes, title_eofs, export_eofs,                                           &
                add_win_h=show_screes, f_help=help_eofs)
     Else
        plots = init_window_graphic(1, img_scree(1:), imgid, l_screes%c, (/l_lab/), &
                draw_screes, title_eofs, export_eofs,                               &
                add_win_h=show_screes, f_help=help_eofs)
     End If
  Else If ((ianal == ia_cca) .and. (npy > 1)) Then
     plots = init_window_graphic(1, img_scree(2:), imgid, l_screes%c, (/l_lab/), &
             draw_screes, title_eofs, export_eofs,                               &
             add_win_h=show_screes, f_help=help_eofs)
  End If
  plots = 1
!
  Return
 End Function scree_plots
!
!
!
#else
 Function scree_plots() &
          Result (plots)
!
! Prints scree plot titles
!
! Modules
  Use data_cpt_constants, Only: ia_cca
  Use settings,           Only: nu
  Use analysis,           Only: ianal
  Use pcs,                Only: npx, npy, svx, svy
!
! Function type
  Integer :: plots
!
! Executable Statements
!
! Open results window
  plots = 0
  Write (Unit=*, Fmt=*)
  Write (Unit=*, Fmt='(A)') 'Scree plots'
!
! Print X scree
  If (npx > 1) Then
     Write (Unit=*, Fmt=*)
     Write (Unit=*, Fmt='(A)') 'X EOFs'
     Call scree_plot (npx, nu, svx)
  End If
!
! Print Y scree
  If ((ianal == ia_cca) .and. (npy > 1)) Then
     Write (Unit=*, Fmt=*)
     Write (Unit=*, Fmt='(A)') 'Y EOFs'
     Call scree_plot (npy, nu, svy)
  End If
!
  Return
 End Function scree_plots
!
!
!
 Subroutine scree_plot (m, n, sv)
!
! Prints scree plot results
!
! Modules
  Use data_numbers, Only: zero, oneh
  Use arrays,       Only: bstick=>rwk
!
! Aguments
!
! Input scalars
  Integer, Intent(In) :: m ! - number of modes -
  Integer, Intent(In) :: n ! - number of cases -
!
! Input arrays
  Real(Kind=rp), Dimension(:), Intent(In) :: sv ! - singular values -
!
! Locals
!
! Local scalars
  Integer :: i ! - mode index -
!
  Real(Kind=rp) :: df     ! - number of cases -
  Real(Kind=rp) :: evalue ! - eigenvalue -
  Real(Kind=rp) :: tvar   ! - total variance -
  Real(Kind=rp) :: pvar   ! - percentage variance -
  Real(Kind=rp) :: svar   ! - cumulative variance -
  Real(Kind=rp) :: spvar  ! - cumulative percentage variance -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Real, Sum
!
! Executable Statements
!
! Calculate broken stick
  Call broken_stick (m, bstick)
!
! Print results
  Write (Unit=*, Fmt='(A10,4A20)') 'Mode', 'Eigenvalue', '% variance', 'Cum. % variance', 'Broken-stick segment'
  df = Real(n-1, Kind=rp)
  tvar = Sum(sv(1:m)**2/df)
  svar = zero
  Do i = 1, m
     evalue = sv(i)**2/df
     svar = svar + evalue
     pvar = oneh*evalue/tvar
     spvar = oneh*svar/tvar
     Write (Unit=*, Fmt='(I10,4G20.12)') i, evalue, pvar, spvar, bstick(i)
  End Do
!
  Return
 End Subroutine scree_plot
!
!
!
#endif
 Subroutine broken_stick (m, bstick)
!
! Constructs a broken-stick
!
! Modules
  Use data_numbers, Only: zero, one, oneh
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: m ! - number of segments -
!
! Output arrays
  Real(Kind=rp), Dimension(:), Intent(Out) :: bstick ! - broken stick -
!
! Locals
!
! Local scalars
  Integer :: i ! - segment index -
!
  Real(Kind=rp) :: seg ! - stick segment -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Real
!
! Executable Statements
!
! Calculate broken stick
  bstick(:) = oneh/Real(m, Kind=rp)
  seg = zero
  Do i = m, 1, -1
     seg = seg + one/Real(i, Kind=rp)
     bstick(i) = bstick(i)*seg
  End Do
!
  Return
 End Subroutine broken_stick
!
!
!
#if GUI == 1
 Function show_screes()
!
! Adds pup-up menu to scree plot window
!
! Modules
  Use data_cpt_constants, Only: ia_cca
  Use labels,             Only: l_broken, l_cumulat, l_logaxis
  Use gui,                Only: add_menu, box_close
  Use pcs,                Only: npx, npy
  Use analysis,           Only: ianal
!
! Function type
  Integer :: show_screes
!
! Executable Statements
!
! Add additional options to pop-up menu
  If ((npx > 1) .or. ((ianal == ia_cca) .and. (npy > 1))) Then
     Call add_menu ('pm', 1, 1, l_cumulat, &
          lsep=.true., icheck=icum, itoggle=icum, cbf=draw_screes)
     Call add_menu ('pm', 1, 2, l_logaxis, &
          icheck=ilogsp, itoggle=ilogsp, cbf=draw_screes)
     Call add_menu ('pm', 1, 3, l_broken, &
          icheck=istick, itoggle=istick, cbf=draw_screes)
  End If
  If (ianal == ia_cca) Then
     If ((npx > 1) .and. (npy > 1)) Then
        Call box_close (.false.)
        Call box_close (.false.)
     End If
  End If
  show_screes = 1
!
  Return
 End Function show_screes
!
!
!
 Function draw_screes() &
          Result (draw)
!
! Draws scree plots
!
! Function type
  Integer :: draw
!
! Executable Statements
!
! Draw scree plots
  draw = draw_xscree()
  draw = draw_yscree()
!
  Return
 End Function draw_screes
!
!
!
 Function draw_xscree() &
          Result (draw)
!
! Draws X scree plot
!
! Modules
  Use pcs, Only: npx, svx
!
! Function type
  Integer :: draw
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Sum
!
! Executable Statements
!
! Draw X scree plot
  draw = 1
  If (npx > 1) Then
     xtvar = Sum(svx(1:npx)**2)
     Call draw_scree (img_scree(1), npx, svx(:), xtvar)
  End If
!
  Return
 End Function draw_xscree
!
!
!
 Function draw_yscree() &
          Result (draw)
!
! Draws scree plots
!
! Modules
  Use data_cpt_constants, Only: ia_cca
  Use pcs,                Only: npy, svy
  Use analysis,           Only: ianal
!
! Function type
  Integer :: draw
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Sum
!
! Executable Statements
!
! Draw scree plots
  draw = 1
  If (ianal == ia_cca) Then
     If (npy > 1) Then
        ytvar = Sum(svy(1:npy)**2)
        Call draw_scree (img_scree(2), npy, svy(:), ytvar)
     End If
  End If
!
  Return
 End Function draw_yscree
!
!
!
 Subroutine draw_scree (this_scree, n, sv, tvar)
!
! Draws a scree plot
!
! Modules
  Use data_numbers, Only: zero, one, oneh
  Use labels,       Only: ca_scores, &
                          cg_mode
  Use arrays,       Only: rwk2
  Use plotting,     Only: plot_line_graph
!
! Aguments
!
! Input scalars
  Integer, Intent(In) :: n ! - number of modes -
!
  Real(Kind=rp), Intent(In) :: tvar ! - total variance -
!
! Input/output scalars
  Type(image), Intent(InOut) :: this_scree ! - scree plot -
!
! Input arrays
  Real(Kind=rp), Dimension(:), Intent(In) :: sv ! - singular values; shape(n) -
!
! Locals
!
! Local scalars
  Integer :: i ! - mode index -
!
  Logical :: laxis ! - logarithmic axis? -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Calculate broken stick
  Allocate (rwk2(n,2))
  If (istick == 1) Call broken_stick (n, rwk2(:,2))
!
! Calculate cumulative %s
  Select Case (icum)
   Case (0)
     rwk2(:,1) = oneh*sv(1:n)**2/tvar
   Case (1)
     rwk2(n,1) = zero
     Do i = n-1, 1, -1
        rwk2(i,1) = rwk2(i+1,1) + sv(i+1)**2/tvar
     End Do
     rwk2(1:n,1) = oneh*(one - rwk2(1:n,1))
     If (istick == 1) Then
        rwk2(n,2) = zero
        Do i= n-1, 1, -1
           rwk2(i,2) = rwk2(i+1,2) + rwk2(i,2)/oneh
        End Do
        rwk2(:,2) = oneh*(one - rwk2(:,2))
     End If
  End Select
!
! Determine whether to use logarithmic axis
  laxis = (ilogsp == 1)!
! Draw scree plot
  If (istick == 1) Then
     If (laxis .and. icum == 1) Then
        i = plot_line_graph (this_scree, Trim(cg_mode), Trim(ca_scores(4)), laxis, n, 1, rwk2(:,1:1), &
            ix1=1, ix2=1, ilw2=1, n2=n, m2=1, y2=rwk2(:,2:2), yhgh=oneh)
     Else
        i = plot_line_graph (this_scree, Trim(cg_mode), Trim(ca_scores(4)), laxis, n, 1, rwk2(:,1:1), &
            ix1=1, ix2=1, ilw2=1, n2=n ,m2=1, y2=rwk2(:,2:2))
     End If
  Else
     If (laxis .and. icum == 1) Then
        i = plot_line_graph (this_scree, Trim(cg_mode), Trim(ca_scores(4)), laxis, n, 1, rwk2(:,1:1), &
            ix1=1, yhgh=oneh)
     Else
        i = plot_line_graph (this_scree, Trim(cg_mode), Trim(ca_scores(4)), laxis, n, 1, rwk2(:,1:1), &
            ix1=1)
     End If
  End If
  Deallocate (rwk2)
!
  Return
 End Subroutine draw_scree
!
!
!
 Function map_xeofs() &
          Result (maps)
!
! Opens window for plotting X EOF loadings and scores
!
! Function type
  Integer :: maps
!
! Executable Statements
!
! Plot X EOF loadings and scores
  maps=map_eofs('X', img_xeof(:), prompt_eofx, update_xeofs)
!
  Return
 End Function map_xeofs
!
!
!
 Function map_yeofs() &
          Result (maps)
!
! Opens window for plotting Y EOF loadings and scores
!
! Function type
  Integer :: maps
!
! Executable Statements
!
! Plot Y EOF loadings and scores
  maps=map_eofs('Y', img_yeof(:), prompt_eofy, update_yeofs)
!
  Return
 End Function map_yeofs
!
!
!
 Function map_eofs(cxy, img_eof, prompt_eof, update_eof) &
          Result (maps)
!
! Opens window for plotting EOF loadings and scores
!
! Modules
  Use labels,      Only: l_axislimsy, l_contourlims, l_loads, l_modes, l_scores
  Use windows_gui, Only: imgid, &
                         init_window_graphic
!
! Function type
  Integer :: maps
!
! Arguments
!
! Input scalars
  Character(Len=1), Intent(In) :: cxy ! - X/Y flag -
!
! Input/output arrays
  Type(image), Dimension(:), Intent(InOut) :: img_eof ! - loadings map and scores graph -
!
! Procedure arguments
  Integer, EXTERNAL :: prompt_eof
  Integer, EXTERNAL :: update_eof
!
! Executable Statements
!
! Open results window
  maps = init_window_graphic(2, img_eof(:), imgid, cxy//' '//l_modes%c, (/l_loads, l_scores/), &
         update_eof, title_eofs, export_eofs,                                                  &
         cxy=cxy, extra_prompt = prompt_eof, limits_grph=limits_eofs, llims=(/l_contourlims, l_axislimsy/), &
         f_help=help_eofs)
!
  Return
 End Function map_eofs
!
!
!
 Function map_ccas() &
          Result (maps)
!
! Creates window for CCA maps and graphs
!
! Modules
  Use labels,      Only: l_axislimsy, l_ccamaps, l_ccas, l_ccax, l_ccay, l_contourlims
  Use windows_gui, Only: imgid, &
                         init_window_graphic
!
! Function type
  Integer :: maps
!
! Executable Statements
!
! Open results window
  maps = init_window_graphic(3, img_ccas(:), imgid, l_ccamaps%c, (/l_ccax, l_ccas, l_ccay/), &
         update_ccas, title_eofs, export_eofs,                                               &
         extra_prompt = prompt_cca, add_win_v=add_cancor, lsq3=.false., &
         limits_grph=limits_eofs, llims=(/l_contourlims, l_axislimsy, l_contourlims/), f_help=help_eofs)
!
  Return
 End Function map_ccas
!
!
!
 Function add_cancor() &
          Result (adds)
!
! Adds canonical correlations to results window
!
! Modules
  Use labels,  Only: cg_cancor_t
  Use gui,     ONly: print_rvalue
  Use pcs,     Only: cancor, iec, mu
!
! Function type
  Integer :: adds
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Add canonical correlation
  cancor = mu(iec)
  Call print_rvalue (Trim(cg_cancor_t), cancor, &
       iwid=6, idec=3)
  adds = 1
!
  Return
 End Function add_cancor
!
!
!
 Function update_xeofs() &
          Result (update)
!
! Prepares for plotting X EOF loadings and scores
!
! Modules
  Use iofiles, Only: xfile
  Use fields,  Only: iffx, xfield
  Use pcs,     Only: iex
!
! Function type
  Integer :: update
!
! Executable Statements
!
! Plot X EOF loadings and scores
  update = update_eof(xfile, iex, xfield(iffx), 'X', 'Mode', img_xeof(1), plot_xeofs)
!
  Return
 End Function update_xeofs
!
!
!
 Function update_yeofs() &
          Result (update)
!
! Prepares for plotting Y EOF loadings and scores
!
! Modules
  Use iofiles, Only: yfile
  Use fields,  Only: iffy, yfield
  Use pcs,     Only: iey
!
! Function type
  Integer :: update
!
! Executable Statements
!
! Plot Y EOF loadings and scores
  update = update_eof(yfile,iey,yfield(iffy),'Y', 'Mode',img_yeof(1),plot_yeofs)
!
  Return
 End Function update_yeofs
!
!
!
 Function update_ccas() &
          Result (update)
!
! Plots CCA graphics
!
! Modules
  Use maths,   Only: magnitude
  Use iofiles, Only: xfile, yfile
  Use fields,  Only: iffx, iffy, xfield, yfield
  Use pcs,     Only: iec
!
! Function type
  Integer :: update
!
! Locals
!
! Local scalars
  Character(Len=8) :: cfmt ! - format statement -
!
! Executable Statements
!
! Plot X CCA loadings
  update = update_eof(xfile, iec, xfield(iffx), 'X', 'Mode', img_ccas(1), plot_xhmap)
!
! Plot Y CCA loadings
  update = update_eof(yfile, iec, yfield(iffy), 'Y', 'Mode', img_ccas(3), plot_yhmap)
!
! Plot time series
  Write (Unit=cfmt, Fmt='(A,I1,A)') '(A,I', magnitude(iec),',A)'
  Write (Unit=img_ccas(2)%title, Fmt=cfmt) 'Temporal Scores (Mode ', iec, ')'
  update = plot_cca()
!
  Return
 End Function update_ccas
!
!
!
 Function update_eof(afile, ie, afield, ceof, cmode, img, fplot) &
          Result (update)
!
! Prepares for plotting loadings or scores
!
! Modules
  Use gui,     Only: set_cursor_waiting
  Use iofiles, Only: ifile
  Use fields,  Only: field
  Use pcs,     Only: make_eof_title
!
! Function type
  Integer :: update
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: ie ! - mode -
!
  Character(Len=1), Intent(In) :: ceof  ! - EOF flag -
  Character(Len=*), Intent(In) :: cmode ! - mode flag -
!
  Type(ifile), Intent(In) :: afile ! - input file -
!
  Type(field), Intent(In) :: afield ! - field data -
!
! Input/output scalars
  Type(image), Intent(InOut) :: img ! - image -
!
! Procedure arguments
  Integer, EXTERNAL :: fplot
!
! Locals
!
! Local scalars
  Integer :: k ! - current contour -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Real
!
! Executable Statements
!
! Set cursor
  Call set_cursor_waiting (1)
!
! Construct image title
  Call make_eof_title (afile%nfs, afile%nls, ie, afield, ceof, cmode, img%title)
!
! Set labelbar intervals
  Select Case (afile%idstr)
   Case (1, 2)
     img%lcontour = .true.
     img%ncs = 13
     Do k = 1, img%ncs
        img%contour(k) = -0.90_rp + 0.15_rp*Real(k-1, Kind=rp)
     End Do
   Case (3)
     img%lcontour = .false.
  End Select
!
! Plot EOF
  update = fplot()
!
  Return
 End Function update_eof
!
!
!
 Function plot_xeofs() &
          Result (plot)
!
! Plots X EOF loadings and scores
!
! Modules
  Use pcs, Only: iex
!
! Function type
  Integer :: plot
!
! Executable Statements
!
! Plot X EOF loadings and scores
  plot = plot_eofs('X', img_xeof(2), iex, plot_xeof, plot_xpca)
!
  Return
 End Function plot_xeofs
!
!
!
 Function plot_yeofs() &
          Result (plot)
!
! Plots Y EOF loadings and scores
!
! Modules
  Use pcs, Only: iey
!
! Function type
  Integer :: plot
!
! Executable Statements
!
! Plot Y EOF loadings and scores
  plot = plot_eofs('Y',img_yeof(2),iey,plot_yeof,plot_ypca)
!
  Return
 End Function plot_yeofs
!
!
!
 Function plot_eofs(ceof, img_pca, ie, fmap, fplot) &
          Result (plot)
!
! Plots loadings and scores
!
! Modules
  Use maths,    Only: magnitude
  Use gui,      Only: set_cursor_waiting
  Use graphics, Only: image
!
! Function type
  Integer :: plot
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: ie    ! - mode -
!
  Character(Len=1), Intent(In) :: ceof ! - EOF flag -
!
! Input/output scalars
  Type(image), Intent(InOut) :: img_pca ! - scores graph -
!
! Procedure arguments
  Integer, EXTERNAL :: fmap  ! - PCA loadings plotting function -
  Integer, EXTERNAL :: fplot ! - PCA time scores plotting function -
!
! Locals
!
! Local scalars
  Character(Len=8) :: cfmt ! - format statement -
!
! Executable Statements
!
! Plot loadings
  plot = fmap()
!
! Plot scores
  Write (cfmt, Fmt='(A,I1,A)') '(A,I', magnitude(ie), ',A)'
  Write (img_pca%title, Fmt=cfmt) ceof//' Temporal Scores (Mode', ie, ')'
  plot = fplot()
!
! Reset cursor
  Call set_cursor_waiting (0)
!
  Return
 End Function plot_eofs
!
!
!
 Function plot_xeof() &
          Result (plot)
!
! Plots X EOF loadings and scores
!
! Modules
  Use arrays,   Only: x
  Use settings, Only: mxa, nu
  Use pcs,      Only: iex, tsx
  Use plotting, Only: plot_xdata
!
! Function type
  Integer :: plot
!
! Executable Statements
!
! Plot X EOF loadings
  plot = plot_loads(img_xeof(1), iex, mxa, nu, x(:,:,1), tsx(:,:), plot_xdata)
!
  Return
 End Function plot_xeof
!
!
!
 Function plot_yeof() &
          Result (plot)
!
! Plots Y EOF loadings and scores
!
! Modules
  Use arrays,   Only: y
  Use settings, Only: mya, nu
  Use pcs,      Only: iey, tsy
  Use season,   Only: lb
  Use plotting, Only: plot_ydata
!
! Function type
  Integer :: plot
!
! Executable Statements
!
! Plot Y EOF loadings
  plot = plot_loads(img_yeof(1), iey, mya, nu, y(:,:,lb), tsy(:,:), plot_ydata)
!
  Return
 End Function plot_yeof
!
!
!
 Function plot_xhmap() &
          Result (plot)
!
! Plots X CCA mode maps
!
! Modules
  Use arrays,   Only: x
  Use settings, Only: mxa, nu
  Use pcs,      Only: hx_ser, iec
  Use plotting, Only: plot_xdata
!
! Function type
  Integer :: plot
!
! Executable Statements
!
! Plot X CCA loadings
  plot = plot_loads(img_ccas(1), iec, mxa, nu, x(:,:,1), hx_ser(:,:), plot_xdata)
!
  Return
 End Function plot_xhmap
!
!
!
 Function plot_yhmap() &
          Result (plot)
!
! Plots Y CCA mode maps
!
! Modules
  Use arrays,   Only: y
  Use settings, Only: mya, nu
  Use pcs,      Only: hy_ser, iec
  Use season,   Only: lb
  Use plotting, Only: plot_ydata
!
! Function type
  Integer :: plot
!
! Executable Statements
!
! Plot X CCA loadings
  plot = plot_loads(img_ccas(3), iec, mya, nu, y(:,:,lb), hy_ser(:,:), plot_ydata)
!
  Return
 End Function plot_yhmap
!
!
!
 Function plot_loads(img_eof, ie, m, n, v, ts, fmap) &
          Result (plot)
!
! Plots loadings
!
! Modules
  Use gui,    Only: set_cursor_waiting
  Use arrays, Only: eof=>rwk
  Use scores, Only: get_corrs
!
! Function type
  Integer :: plot
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: ie ! - mode -
  Integer, Intent(In) :: m  ! - number of variables -
  Integer, Intent(In) :: n  ! - number of cases -
!
! Input/output scalars
  Type(image), Intent(InOut) :: img_eof ! - loadings map -
!
! Input arrays
  Real(Kind=rp), Dimension(:,:), Intent(In) :: ts ! - temporal scores -
  Real(Kind=rp), Dimension(:,:), Intent(In) :: v  ! - original variables -
!
! Procedure arguments
  Interface ! - PCA loadings plotting function -
   Integer Function fmap(img, icols, ioff, ncb, z, c, &
                    vlow, vhgh, vmin, vmax)
    Use data_numbers, Only: rp
    Use graphics,     Only: image
    Integer, Intent(In) :: icols, ioff, ncb
    Type(image), Intent(InOut) :: img
    Character(Len=*), Intent(In) :: c
    Real(Kind=rp), Intent(In), Optional :: vlow, vhgh, vmin, vmax
    Real(Kind=rp), Dimension(:), Intent(In) :: z
   End Function fmap
  End Interface
!
! Executable Statements
!
! Calculate correlations between original data and EOFs
  Call get_corrs (1, n, m, v, ts(ie,1:n), eof)
!
! Update map
  plot = fmap(img_eof, 4, 0, 1, eof(:), 'Loadings')
!
! Reset cursor
  Call set_cursor_waiting (0)
  plot = 1
!
  Return
 End Function plot_loads
!
!
!
 Function plot_xpca() &
          Result (plot)
!
! Plots X temporal scores
!
! Modules
  Use iofiles,  Only: xfile, zfile
  Use pcs,      Only: fsx, iex, mxe, tsx
  Use forecast, Only: ifc
!
! Function type
  Integer :: plot
!
! Functions and Subroutines
  Intrinsic Transpose
!
! Executable Statements
!
! Plot X temporal scores
  If ((ifc == 2) .and. (iex <= mxe)) Then
     plot = plot_pca(img_xeof(2), xfile%fdate, tsx(iex,:), &
          zfile%fdate, tsf=Transpose(fsx(iex:iex,:)))
  Else
     plot = plot_pca(img_xeof(2), xfile%fdate, tsx(iex,:))
  End If
!
  Return
 End Function plot_xpca
!
!
!
 Function plot_ypca() &
          Result (plot)
!
! Plots Y temporal scores
!
! Modules
  Use iofiles, Only: yfile
  Use pcs,     Only: iey, tsy
!
! Function type
  Integer :: plot
!
! Executable Statements
!
! Plot Y temporal scores
  plot = plot_pca(img_yeof(2), yfile%fdate, tsy(iey,:))
!
  Return
 End Function plot_ypca
!
!
!
 Function plot_pca(img_pca, fdate, ts, &
          zdate, tsf) &
          Result (plot)
!
! Plots temporal scores
!
! Modules
  Use data_cpt_constants, Only: iuseq
  Use time,               Only: pdate, &
                                iseq
  Use labels,             Only: ca_seq
  Use gui,                Only: set_cursor_waiting
  Use settings,           Only: nf, nt, nu
  Use missing,            Only: kuse, kfuse
  Use plotting,           Only: plot_line_graph
!
! Function type
  Integer :: plot
!
! Arguments
!
! Input scalars
  Type(pdate), Intent(In) :: fdate ! - first date -
!
! - optional input scalars -
  Type(pdate), Intent(In), Optional :: zdate ! - first date of new data -
!
! Input/output scalars
  Type(image), Intent(InOut) :: img_pca ! - scores graph -
!
! Input arrays
  Real(Kind=rp), Dimension(:), Intent(In) :: ts ! - temporal scores -
!
! - optional input arrays -
  Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: tsf ! - new temporal scores -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Present, Reshape, Trim
!
! Executable Statements
!
! Plot X temporal scores
  If (Present(zdate)) Then
     plot = plot_line_graph (img_pca, Trim(ca_seq(iuseq(iseq))), 'Score', .false., nt, 1, Reshape(ts(1:nu), (/nu, 1/)), &
            d1=fdate, kuse=kuse(:), df=zdate, nf=nf, mf=1, icolf=2, yf=tsf(:,:), kfuse=kfuse(:))
  Else
     plot = plot_line_graph (img_pca, Trim(ca_seq(iuseq(iseq))), 'Score', .false., nt, 1, Reshape(ts(1:nu), (/nu, 1/)), &
            d1=fdate, kuse=kuse(:))
  End If
!
! Reset cursor
  Call set_cursor_waiting (0)
  plot = 1
!
  Return
 End Function plot_pca
!
!
!
 Function plot_cca() &
          Result (plot)
!
! Plots CCA temporal scores
!
! Modules
  Use data_cpt_constants,  Only: iuseq
  Use data_time_constants, Only: isq_yr
  Use maths,               Only: magnitude
  Use time,                Only: iseq, &
                                 date_diff
  Use labels,              Only: ca_seq
  Use gui,                 Only: icbw, &
                                 set_cursor_waiting
  Use settings,            Only: nf, nt
  Use iofiles,             Only: xfile, yfile, zfile
  Use pcs,                 Only: fsxc, hx_ser, hy_ser, iec, nccu
  Use missing,             Only: kfuse, kuse
  Use plotting,            Only: plot_line_graph
  Use forecast,            Only: ifc
!
! Function type
  Integer :: plot
!
! Locals
!
! Local scalars
  Integer :: nlag ! - size of X lag -
!
  Character(Len=32) :: cxlab ! - x-axis label -
  Character(Len=32) :: cylab ! - y-axis label -
  Character(Len= 8) :: cfmt  ! - format statement -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Max, MaxVal, Min, MinVal, Transpose, Trim
!
! Executable Statements
!
! Plot CCA time series
  Select Case (iseq)
   Case (isq_yr)
     nlag = yfile%fdate%iyr - xfile%fdate%iyr
   Case Default
     nlag = date_diff(xfile%fdate, yfile%fdate, iseq)
  End Select
  Select Case (nlag)
   Case (0)
     Write (Unit=cxlab, Fmt='(A)') Trim(ca_seq(iuseq(iseq)))
   Case (1)
     Write (Unit=cxlab, Fmt='(A)') 'Year (X offset by 1 year)'
   Case (2:)
     Write (Unit=cfmt, Fmt='(A,I1,A)') '(A,I', magnitude(nlag), ',A)'
     Write (Unit=cxlab, Fmt=cfmt) 'Year (X offset by ', nlag, ' years)'
   Case (-1)
     Write (Unit=cxlab, Fmt='(A)') 'Year (X offset by -1 year)'
   Case (:-2)
     Write (Unit=cfmt, Fmt='(A,I1,A)') '(A,I', magnitude(nlag)+1, ',A)'
     Write (Unit=cxlab, Fmt=cfmt) 'Year (X offset by ', nlag, ' years)'
  End Select
  Select Case (icbw)
   Case (0)
     cylab = 'Scores (X red; Y green)'
   Case (1)
     cylab = 'Scores (X black; Y grey)'
  End Select
!
! Plot CCA graphic
  If ((ifc == 2) .and. (iec <= nccu)) Then
     plot = plot_line_graph (img_ccas(2), Trim(cxlab), Trim(cylab), .false., nt, 1, Transpose(hx_ser(iec:iec,:)), &
            kuse=kuse(:), ilw2=2, d1=xfile%fdate, d2=yfile%fdate, m2=1, n2=nt, y2=Transpose(hy_ser(iec:iec,:)), k2use=kuse(:), &
            df=zfile%fdate, nf=nf, mf=1, icolf=2, yf=Transpose(fsxc(iec:iec,:)), kfuse=kfuse(:))
  Else
     plot = plot_line_graph (img_ccas(2), Trim(cxlab), Trim(cylab), .false., nt, 1, Transpose(hx_ser(iec:iec,:)), &
            kuse=kuse(:), ilw2=2, d1=xfile%fdate, d2=yfile%fdate, m2=1, n2=nt, y2=Transpose(hy_ser(iec:iec,:)), k2use=kuse(:), &
            ylow=Min(MinVal(hx_ser(iec,:)), MinVal(hy_ser(iec,:))),                                                            &
            yhgh=Max(MaxVal(hx_ser(iec,:)), MaxVal(hy_ser(iec,:))))
  End If
!
! Reset cursor
  Call set_cursor_waiting (0)
  plot = 1
!
  Return
 End Function plot_cca
!
!
!
 Function prompt_eofx() &
          Result (prompt)
!
! Prompts for X mode number
!
! Modules
  Use pcs, Only: iex, &
                 change_eofx, prompt_eofs
!
! Function type
  Integer :: prompt
!
! Executable Statements
!
! Prompt for X mode
  prompt = prompt_eofs(iex, change_eofx, update_xeofs)
!
  Return
 End Function prompt_eofx
!
!
!
 Function prompt_eofy() &
          Result (prompt)
!
! Prompts for Y mode number
!
! Modules
  Use pcs, Only: iey, &
                 change_eofy, prompt_eofs
!
! Function type
  Integer :: prompt
!
! Executable Statements
!
! Prompt for Y mode
  prompt = prompt_eofs(iey, change_eofy, update_yeofs)
!
  Return
 End Function prompt_eofy
!
!
!
 Function prompt_cca() &
          Result (prompt)
!
! Prompts for CCA mode number
!
! Modules
  Use pcs, Only: iec, &
                 change_cca, prompt_eofs
!
! Function type
  Integer :: prompt
!
! Executable Statements
!
! Prompt for CCA mode
  prompt = prompt_eofs(iec, change_cca, update_ccas)
!
  Return
 End Function prompt_cca
!
!
!
 Function title_eofs() &
          Result (title)
!
! Modules
  Use plotting, Only: get_title
!
! Function type
  Integer :: title
!
! Executable Statements
!
! Prompt for title
  title = replot_eofs(get_title)
!
  Return
 End Function title_eofs
!
!
!
 Function limits_eofs() &
          Result (limits)
!
! Modules
  Use plotting, Only: get_limits
!
! Function type
  Integer :: limits
!
! Executable Statements
!
! Prompt for limits
  limits = replot_eofs(get_limits)
!
  Return
 End Function limits_eofs
!
!
!
 Function replot_eofs(get_custom) &
          Result (replot)
!
! Modules
  Use plotting,    Only: replot_graph
  Use windows_gui, Only: imgid
!
! Function type
  Integer :: replot
!
! Arguments
!
! Procedure arguments
  Interface
    Subroutine get_custom (img, lreset)
     Use graphics, Only: image
     Type(image), Intent(InOut) :: img ! - image -
     Logical, Intent(Out) :: lreset ! - reset? -
    End Subroutine get_custom
  End Interface
!
! Executable Statements
!
! Replot customised images
  If (img_scree(1)%id == imgid) Then
     replot = replot_graph(img_scree(1), get_custom, draw_xscree)
  Else If (img_scree(2)%id == imgid) Then
     replot = replot_graph(img_scree(2), get_custom, draw_yscree)
  Else If (img_xeof(1)%id == imgid) Then
     replot = replot_graph(img_xeof(1), get_custom, plot_xeof)
  Else If (img_xeof(2)%id == imgid) Then
     replot = replot_graph(img_xeof(2), get_custom, plot_xpca)
  Else If (img_yeof(1)%id == imgid) Then
     replot = replot_graph(img_yeof(1), get_custom, plot_yeof)
  Else If (img_yeof(2)%id == imgid) Then
     replot = replot_graph(img_yeof(2), get_custom, plot_ypca)
  Else If (img_ccas(1)%id == imgid) Then
     replot = replot_graph(img_ccas(1), get_custom, plot_xhmap)
  Else If (img_ccas(3)%id == imgid) Then
     replot = replot_graph(img_ccas(3), get_custom, plot_yhmap)
  Else If (img_ccas(2)%id == imgid) Then
     replot = replot_graph(img_ccas(2), get_custom, plot_cca)
  End If
!
  Return
 End Function replot_eofs
!
!
!
 Function export_eofs() &
          Result (export)
!
! Modules
  Use pcs,         Only: iec, iex, iey, nco, nxo, nyo
  Use graphics,    Only: save_graphic
  Use windows_gui, Only: imgid
!
! Function type
  Integer :: export
!
! Executable Statements
!
! Prompt for montage title
  If (imgid < 0) Then
     export = export_montage_eofs(-imgid)
     Return     
  End If
!
! Save graph
! - scree plots -
  If (img_scree(1)%id == imgid) Then
     export = save_graphic('Scree_X', img_scree(1))
  Else If (img_scree(2)%id == imgid) Then
     export = save_graphic('Scree_Y', img_scree(2))
! - EOFs -
  Else If (img_xeof(1)%id == imgid) Then
     export = save_graphic('Loadings_XEOF', img_xeof(1), &
            imap=iex, mmap=nxo)
  Else If (img_xeof(2)%id == imgid) Then
     export = save_graphic('Scores_XEOF', img_xeof(2), &
            imap=iex, mmap=nxo)
  Else If (img_yeof(1)%id == imgid) Then
     export = save_graphic('Loadings_YEOF', img_yeof(1), &
            imap=iey, mmap=nyo)
  Else If (img_yeof(2)%id == imgid) Then
     export = save_graphic('Scores_YEOF', img_yeof(2), &
            imap=iey, mmap=nyo)
  Else If (img_ccas(1)%id == imgid) Then
     export = save_graphic('Loadings_XCCA', img_ccas(1), &
            imap=iec, mmap=nco)
  Else If (img_ccas(3)%id == imgid) Then
     export = save_graphic('Loadings_YCCA', img_ccas(3), &
            imap=iec, mmap=nco)
  Else If (img_ccas(2)%id == imgid) Then
     export = save_graphic('Scores_CCA', img_ccas(2), &
            imap=iec, mmap=nco)
  End If
!
  Return
 End Function export_eofs
!
!
!
 Function export_montage_eofs(imgid) &
          Result (export)
!
! Modules
  Use graphics, Only: export_montage
!
! Function type
  Integer :: export
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: imgid ! - image ID -
!
! Executable Statements
!
! Save images
! - scree plots -
  If (img_scree(1)%id == imgid) Then
     export = export_montage('Scree', img_scree(1), img_scree(2))
! - EOFs -
  Else If (img_xeof(1)%id == imgid) Then
     export = export_montage('XEOFs', img_xeof(1), img_xeof(2))
  Else If (img_yeof(1)%id == imgid) Then
     export = export_montage('YEOFs', img_yeof(1), img_yeof(2))
  Else If (img_ccas(1)%id == imgid) Then
     export = export_montage('CCA', img_ccas(1), img_ccas(2), &
            lsquare=.false., img_3=img_ccas(3))
  End If
!
  Return
 End Function export_montage_eofs
!
!
!
 Function help_eofs()
!
! Modules
  Use help,        Only: help_file
  Use windows_gui, Only: imgid
!
! Function type
  Integer :: help_eofs
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Any
!
! Executable Statements
!
! Open context-specific help pages
  If (Any(img_scree(:)%id == imgid)) Then
     Call help_file ('Tools_Modes_Scree.html')
  Else If (Any(img_xeof(:)%id == imgid)) Then
     Call help_file ('Tools_Modes_XEOF.html')
  Else If (Any(img_yeof(:)%id == imgid)) Then
     Call help_file ('Tools_Modes_YEOF.html')
  Else If (Any(img_ccas(:)%id == imgid)) Then
     Call help_file ('Tools_Modes_CCA.html')
  End If
  help_eofs = 2
!
  Return
 End Function help_eofs
#endif
End Module eofs
