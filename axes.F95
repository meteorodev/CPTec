! Author: Simon Mason
Module axes
!
! Declarations
!
! Modules
  Use data_numbers, Only: rp
!
! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: calc_xaxis_limits, draw_xaxis, draw_yaxis, get_xaxis_limits, get_yaxis_limits, set_axis
!
! Derived type definitions
! - axis -
  Public :: axis
  Type axis
     Sequence
     Integer :: i_min ! - minimum plotting position -
     Integer :: i_max ! - maximum plotting position -
!
     Real(Kind=rp) :: amin   ! - axis minimum -
     Real(Kind=rp) :: amax   ! - axis maximum -
     Real(Kind=rp) :: length ! - axis length -
     Real(Kind=rp) :: aint   ! - intercept -
     Real(Kind=rp) :: tick   ! - tick-mark interval -
     Real(Kind=rp) :: scale  ! - scaling -
!
     Character(Len=64) :: label ! - label -
!
     Logical :: laxis ! - logaritihmic axis? -
     Logical :: ldate ! - date axis? -
  End Type axis
!
! Derived type scalars
  Type(axis), Public :: x_axis ! - x-axis -
  Type(axis), Public :: y_axis ! - y-axis -
!
Contains
!
!
 Subroutine set_axis (aaxis, clab, laxis, ldate, i_pixel)
!
! Initialises axis settings
!
! Modules
  Use data_numbers, Only: zero,one
  Use maths,        Only: magnitude
!
! Arguments
!
! Input scalars
  Character(Len=*), Intent(In) :: clab ! - axis label -
!
  Logical, Intent(In) :: laxis ! - logarithmic axis? -
  Logical, Intent(In) :: ldate ! - date axis? -
!
! Input/output scalars
  Type(axis), Intent(InOut) :: aaxis ! - axis -
!
! Procedure arguments
  Interface
   Integer Function i_pixel (z)
    Use data_numbers, Only: rp
    Real(Kind=rp), Intent(In) :: z
   End Function i_pixel
  End Interface
!
! Locals
!
! Local scalars
  Integer :: iom  ! - order of magnitude -
!
  Character(Len=11) :: cfmt   ! - format statement -
  Character(Len=15) :: cscale ! - scaling label -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Nint,Trim
!
! Executable Statements
!
! Set axis flags
  aaxis%laxis = laxis
  aaxis%ldate = ldate
!
! Set intercept
  If (.not.ldate) Then
! - extremes both negative -
     If ((aaxis%amin < zero) .and. (aaxis%amax < zero)) Then
        aaxis%aint = aaxis%amax
! - extremes are of opposite signs -
     Else If (aaxis%amin < zero) Then
        aaxis%aint = zero
! - extremes both positive -
     Else
        aaxis%aint = aaxis%amin
     End If
! - date on X-axis -
  Else
     aaxis%aint = aaxis%amin
  End If
!
! Define limits
  aaxis%i_min = i_pixel(aaxis%amin)
  aaxis%i_max = i_pixel(aaxis%amax)
!
! Construct scaling label
  If (aaxis%scale /= one) Then
     iom = magnitude(aaxis%scale)
     If (iom > 0) Then
        If (iom <= 3) Then
           Write (Unit=cfmt, FMT='(A,I1,A)') '(A,I',iom,',A)'
           Write (Unit=cscale, Fmt=cfmt) '(x ',Nint(aaxis%scale),')'
        Else
           Write (Unit=cfmt, Fmt='(A,I4,A)') '(A,I',magnitude(iom-1),',A)'
           Write (Unit=cscale, Fmt=cfmt) '(x 10^',iom-1,')'
        End If
        aaxis%label = clab//' '//Trim(cscale)
     Else If (iom < 0) Then
        If (iom >= -3) Then
           Write (Unit=cfmt, Fmt='(A,I1,A)') '(A,I',1-iom,',A)'
           Write (Unit=cscale, Fmt=cfmt) '(/ ',Nint(one/aaxis%scale),')'
        Else
           Write (Unit=cfmt, Fmt='(A,I4,A)') '(A,I',magnitude(-iom-1),',A)'
           Write (Unit=cscale, Fmt=cfmt) '(/ 10^',-iom-1,')'
        End If
        aaxis%label = clab//' '//Trim(cscale)
     Else
        aaxis%label = clab
     End If
  Else
     aaxis%label = clab
  End If
!
  Return
 End Subroutine set_axis
!
!
!
 Subroutine get_xaxis_limits (x_axis, n1, ldate, &
            ix1, ix2, ixf, d1, d2, df, n2, nf)
!
! Sets appropriate axis limits for dates or for sequential data
!
! Modules
  Use data_time_constants, Only: isq_yr
  Use time,                Only: pdate, &
                                 Operator(<), &
                                 iseq, &
                                 date_diff
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: n1  ! - number of cases -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: ix1 ! - first X value -
  Integer, Intent(In), Optional :: ix2 ! - first X value for overlay data -
  Integer, Intent(In), Optional :: ixf ! - first X value for new data -
  Integer, Intent(In), Optional :: n2  ! - number of values, overlay variables -
  Integer, Intent(In), Optional :: nf  ! - number of new data -
!
  Type(pdate), Intent(In), Optional :: d1 ! - first date on x-axis -
  Type(pdate), Intent(In), Optional :: d2 ! - first date for overlay data -
  Type(pdate), Intent(In), Optional :: df ! - first date for new data -
!
! Input/output scalars
  Type(axis), Intent(InOut) :: x_axis ! - axis -
!
! Output scalars
  Logical, Intent(Out) :: ldate ! - date axis? -
!
! Locals
!
! Local scalars
  Integer :: ix  ! - X index -
  Integer :: nmx ! - maximum number of forecasts -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Abs, Max, Min, Present
!
! Executable Statements
!
! Set appropriate limits for sequential data
  If (Present(ix1)) Then
     ldate = .false.
     If (Present(ix2)) Then
        If (Present(ixf)) Then
           ix = Min(ix1, ix2, ixf)
           Call calc_xaxis_limits (x_axis,Max(ix1+n1, ix2+n2, ixf+nf)-ix, &
                ix1=ix)
        Else
           Call calc_xaxis_limits (x_axis,Max(n1, n2, ix2+n2-ix1, ix1+n1-ix2), &
                ix1=Min(ix1, ix2))
        Endif
     Else
        If (Present(ixf)) Then
           Call calc_xaxis_limits (x_axis,Max(n1, nf, ixf+nf-ix1, ix1+n1-ixf), &
                ix1=Min(ix1, ixf))
        Else
           Call calc_xaxis_limits (x_axis, n1, &
                ix1=ix1)
        Endif
     End If
!
! Set appropriate limits for dates
  Else If (Present(d1)) Then
     ldate = .true.
     If (.not.Present(d2) .and. .not.Present(df)) Then
        Call calc_xaxis_limits (x_axis, n1, &
             d1=d1)
     Else If (Present(d2) .and. .not.Present(df)) Then
        Select Case (iseq)
         Case (isq_yr) ! - annual sequencing -
           ix = Min(d1%iyr, d2%iyr)
           nmx = Max(d1%iyr+n1-ix, d2%iyr+n2-ix)
           Call calc_xaxis_limits (x_axis, nmx, &
                ix1=ix)
         Case (1:) ! - daily sequencing -
           nmx = n1 + Abs(date_diff(d1, d2, iseq))
           If (d1 < d2) Then
              Call calc_xaxis_limits (x_axis, nmx, &
                   d1=d1)
           Else
              Call calc_xaxis_limits (x_axis, nmx, &
                   d1=d2)
           End If
        End Select
     Else If (Present(df) .and. .not.Present(d2)) Then
        Select Case (iseq)
         Case (isq_yr) ! - annual sequencing -
           ix = Min(d1%iyr, df%iyr)
           nmx = Max(d1%iyr+n1-ix, df%iyr+nf-ix)
           Call calc_xaxis_limits (x_axis, nmx, &
                ix1=ix)
         Case (1:) ! - daily sequencing -
           nmx = n1 + Abs(date_diff(d1, df, iseq))
           If (d1 < df) Then
              Call calc_xaxis_limits (x_axis, nmx, &
                   d1=d1)
           Else
              Call calc_xaxis_limits (x_axis, nmx, &
                   d1=df)
           End If
        End Select
     Else If (Present(d2) .and. Present(df)) Then
        Select Case (iseq)
         Case (isq_yr) ! - annual sequencing -
           ix = Min(d1%iyr, d2%iyr, df%iyr)
           nmx = Max(d1%iyr+n1-ix, d2%iyr+n2-ix, df%iyr+nf-ix)
           Call calc_xaxis_limits (x_axis, nmx, &
                ix1=ix)
         Case (1:) ! - daily sequencing -
           nmx = n1 + Abs(date_diff(d1, df, iseq))
           If (d1 < df) Then
              Call calc_xaxis_limits (x_axis, nmx, &
                   d1=d1)
           Else
              Call calc_xaxis_limits (x_axis, nmx, &
                   d1=df)
           End If
        End Select
     End If
  End If
!
  Return
 End Subroutine get_xaxis_limits
!
!
!
 Subroutine get_yaxis_limits (lpreset, laxis, y_axis, y, img_min, img_max, img_scale, &
            y2, yf, ylow, yhgh, ymin, ymax)
!
! Sets appropriate axis limits for continuous data
!
! Modules
  Use data_numbers,       Only: one
  Use data_gui_constants, Only: mnnt, mxnt
  Use maths,              Only: calc_limits, get_intervals
!
! Arguments
!
! Input scalars
  Logical, Intent(In) :: lpreset ! - preset limits? -
  Logical, Intent(In) :: laxis   ! - logarithmic axis? -
!
! - optional input scalars -
  Real(Kind=rp), Intent(In), Optional :: ylow ! - lowest y-value (soft limits) -
  Real(Kind=rp), Intent(In), Optional :: yhgh ! - highest y-value (soft limits) -
  Real(Kind=rp), Intent(In), Optional :: ymin ! - minimum value (hard limit) -
  Real(Kind=rp), Intent(In), Optional :: ymax ! - maximum value (hard limit) -
!
! Input/output scalars
  Real(Kind=rp), Intent(InOut) :: img_min   ! - image minimum -
  Real(Kind=rp), Intent(InOut) :: img_max   ! - image maximum -
  Real(Kind=rp), Intent(InOut) :: img_scale ! - image scaling -
!
  Type(axis), Intent(InOut) :: y_axis ! - axis -
!
! Input arrays
  Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - y-data; shape(n, m) -
!
! - optional input arrays -
  Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: y2 ! - overlay data -
  Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: yf ! - new data -
!
! Locals
!
! Local scalars
  Real(Kind=rp) :: ylw ! - lowest y-value -
  Real(Kind=rp) :: yhg ! - highest y-value -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Ceiling, Floor, Log10, Max, MaxVal, Min, MinVal, Present
!
! Executable Statements
!
! Set appropriate limits for continuous data
  If (lpreset) Then ! - preset limits -
     y_axis%amin = img_min
     y_axis%amax = img_max
     y_axis%scale = img_scale
     Call get_intervals (y_axis%amin, y_axis%amax, mnnt, mxnt, y_axis%tick)
  Else ! - calculate limits -
     If (Present(ymin)) Then
        ylw = ymin
     Else If (Present(ylow)) Then
        ylw = ylow
     Else
        ylw = MinVal(y(:,:))
        If (Present(y2)) ylw = Min(ylw, MinVal(y2(:,:)))
        If (Present(yf)) ylw = Min(ylw, MinVal(yf(:,:)))
     End If
     If (Present(ymax)) Then
        yhg = ymax
     Else If (Present(yhgh)) Then
        yhg = yhgh
     Else
        yhg = MaxVal(y(:,:))
        If (Present(y2)) yhg = Max(yhg, MaxVal(y2(:,:)))
        If (Present(yf)) yhg = Max(yhg, MaxVal(yf(:,:)))
     End If
     If (laxis) Then ! - log axis -
        y_axis%amin = Floor(Log10(ylw))
        y_axis%amax = Ceiling(Log10(yhg))
        If (y_axis%amin == y_axis%amax) y_axis%amin = y_axis%amin - 1
        y_axis%scale = one
        y_axis%tick = one
     Else ! - linear axis -
        Call calc_limits (ylw, yhg, mnnt, mxnt, y_axis%amin, y_axis%amax, y_axis%scale, y_axis%tick)
        If (Present(ymin)) y_axis%amin = ymin
        If (Present(ymax)) y_axis%amax = ymax
     End If
     img_min = y_axis%amin
     img_max = y_axis%amax
     img_scale = y_axis%scale
  End If
!
  Return
 End Subroutine get_yaxis_limits
!
!
!
 Subroutine calc_xaxis_limits (aaxis,n, &
            ix1,d1)
!
! Calculates appropriate axis limits when data are years
!
! Modules
  Use data_numbers,        Only: one,two,three,six
  Use data_time_constants, Only: isq_mn,isq_sn,isq_so,isq_yr,lfeb,mdm,mdy,nmn
  Use time,                Only: pdate, &
                                 Operator(+), &
                                 iseq, &
                                 ndays,pdate_to_idate
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: n ! - number of years -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: ix1 ! - first x value -
!
  Type(pdate), Intent(In), Optional :: d1 ! - first date -
!
! Input/output scalars
  Type(axis), Intent(InOut) :: aaxis ! - axis -
!
! Locals
!
! Local scalars
  Integer :: ixf ! - first X value -
  Integer :: ixn ! - last X value -
  Integer :: ixa ! - rounding -
  Integer :: ixm ! - maximum number of values -
  Integer :: iom ! - order of magnitude -
!
  Type(pdate) :: adate ! - parsed date -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Mod, Nint, Present, Real
!
! Executable Statements
!
! Round years to nearest 5
  If (Present(ix1)) Then
     ixf = ix1 - Mod(ix1, 5)
     ixn = ix1 + n
     Call rounding1 (5, 10)
!
! Identify appropriate day limits
  Else If (Present(d1)) Then
     Select Case (iseq)
      Case (isq_yr) ! - annual sequencing -
        ixf = d1%iyr - Mod(d1%iyr, 5)
        ixn = d1%iyr + n
        Call rounding1 (5, 10)
        aaxis%tick = Real(Nint(aaxis%tick), Kind=rp)
      Case (isq_sn) ! - seasonal sequencing -
        ixf = pdate_to_idate(iseq, d1)
        ixn = ixf + n
        Call rounding2 (nmn/2, nmn)
      Case (isq_mn, isq_so) ! - monthly sequencing -
        ixf = pdate_to_idate(iseq, d1)
        ixn = ixf + n
        Call rounding2 (nmn, nmn*2)
      Case (1,5,7) ! - daily, pentad, and weekly sequencing -
        If (n <= mdm) Then
           ixf = pdate_to_idate(1, d1)
           ixn = ixf + n
           If (n > mdm/2) Then
              aaxis%tick = two
           Else
              aaxis%tick = one
           End If
        Else If (n <= mdy/iseq) Then
           adate = d1
           adate%idy = 1
           ixf = pdate_to_idate(1, adate)
           adate = d1+n
           adate%idy = ndays(adate%iyr, adate%imn)
           ixn = pdate_to_idate(1, adate)
           aaxis%tick = Real(lfeb, Kind=rp)
        Else
           adate = d1
           adate%imn = 1
           adate%idy = 1
           ixf = pdate_to_idate(1, adate)
           adate = d1+n
           adate%imn = nmn
           adate%idy = ndays(adate%iyr, adate%imn)
           ixn = pdate_to_idate(1, adate)
           aaxis%tick = Real(mdy, Kind=rp)
        End If
      Case (10)
        If (n <= 10) Then
           ixf = pdate_to_idate(iseq, d1)
           ixn = ixf + n
           aaxis%tick = one
        Else If (n <= 3*nmn) Then
           adate%iyr = d1%iyr
           adate%imn = d1%imn
           adate%idy = 1
           ixf = pdate_to_idate(iseq, adate)
           ixn = Mod(n, 3)
           Select Case (ixn)
            Case (0)
              adate = d1 + n
              ixn = pdate_to_idate(iseq, adate)
            Case (1)
              adate=d1 + (n + 2)
            Case (2)
              adate=d1 + (n + 1)
           End Select
           ixn=pdate_to_idate(iseq,adate)
           If (n <= 3*nmn/2) Then
              aaxis%tick = three
           Else
              aaxis%tick = six
           End If
        Else
           adate%iyr = d1%iyr
           adate%imn = 1
           adate%idy = 1
           ixf = pdate_to_idate(iseq, adate)
           adate = d1 + n
           adate%imn = nmn
           adate%idy = 21
           ixn = pdate_to_idate(iseq, adate)
           Call rounding2 (3, nmn)
        End If
     End Select
  Else
     ixf = 0
     ixn = 1
     aaxis%tick = one
  End If
!
! Set axis limits and scaling
  aaxis%amin = Real(ixf, Kind=rp)
  aaxis%amax = Real(ixn, Kind=rp)
  aaxis%scale = one
!
  Return
!
 Contains
!
!
  Subroutine rounding1 (n1, n2)
!
! Modules
  Use data_numbers, Only: four, ten
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: n1 ! - smaller number -
  Integer, Intent(In) :: n2 ! - larger number -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Real
!
! Executable Statements
!
! Identify sensible tick mark interval
  ixa = Mod(ixn, 5)
  If (ixa > 0) ixn = ixn + 5 - ixa
  iom = 0
  Do
     ixa = n1*n2**iom
     ixm = n1*n2**(iom+1)
     If (ixn - ixf >= ixm) Then
        If (Mod(ixf, n2) == ixa) ixf = ixf - ixa
        If (Mod(ixn, n2) == ixa) ixn = ixn + ixa
     Else
        If (iom == 0) Then
           aaxis%tick = Real(n1, Kind=rp)
        Else
           aaxis%tick = Real(n2, Kind=rp)**iom
        End If
        Exit
     End If
     iom = iom + 1
  End Do
  If (Real(ixn - ixf, Kind=rp)/aaxis%tick > ten) aaxis%tick = aaxis%tick*two
  If (Real(ixn - ixf, Kind=rp)/aaxis%tick < four) aaxis%tick = aaxis%tick/two
!
  Return
  End Subroutine rounding1
!
!
!
  Subroutine rounding2 (n1, n2)
!
! Modules
  Use data_numbers, Only: four, ten
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: n1 ! - smaller number -
  Integer, Intent(In) :: n2 ! - larger number -
!
! Locals
!
! Local scalars
  Integer :: ir ! - range -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Real
!
! Executable Statements
!
! Identify sensible tick mark interval
  iom = 0
  Do
     ixa = n1*n2*iom
     ixm = n1*n2*(iom + 1)
     If (ixn - ixf >= ixm) Then
        If (Mod(ixf, n2) == ixa) ixf = ixf - ixa
        If (Mod(ixn, n2) == ixa) ixn = ixn + ixa
     Else
        If (iom == 0) Then
           aaxis%tick = Real(n1, Kind=rp)
        Else
           aaxis%tick = Real(ixa, Kind=rp)
        End If
        Exit
     End If
     iom = iom + 1
  End Do
  ir = ixn - ixf
  Do
     If (Real(ir, Kind=rp)/aaxis%tick > ten) Then
        aaxis%tick=aaxis%tick*two
     Else If (Real(ir, Kind=rp)/aaxis%tick < four) Then
        If (Mod(iom, 2) == 0) Then
           aaxis%tick = aaxis%tick/three
        Else
           aaxis%tick = aaxis%tick/two
        End If
     Else
        Exit
     End If
  End Do
!
  Return
  End Subroutine rounding2
 End Subroutine calc_xaxis_limits
!
!
!
 Subroutine draw_xaxis (x_axis, idir, llab, iy_font, ix_pixel, iy_pixel, inum, iano)
!
! Draws an x-axis
!
! Modules
  Use clrwin$,             Only: draw_characters$, draw_line_between$, get_text_size$
  Use data_numbers,        Only: zero, one, three, five
  Use data_text,           Only: ilang
  Use data_time_constants, Only: cmon, isq_mn, isq_sn, isq_so, isq_yr
  Use time,                Only: pdate, &
                                 Operator(+), &
                                 iseq, &
                                 idate_to_pdate
  Use gui,                 Only: icol
  Use screen,              Only: font1, font3, &
                                 gfont_size
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: idir    ! - tick-mark direction (<0 down, >0 up) -
  Integer, Intent(In) :: iy_font ! - vertical font size in pixels -
!
  Logical, Intent(In) :: llab ! - draw labels? -
!
  Type(axis), Intent(In) :: x_axis ! - x-axis -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: inum ! - numerical labels positioning indicator (<0 left, >0 right) -
  Integer, Intent(In), Optional :: iano ! - annotation positioning indicator (<0 left, >0 right) -
!
! Procedure arguments
  Interface
   Integer Function ix_pixel (x)
    Use data_numbers, Only: rp
    Real(Kind=rp), Intent(In) :: x
   End Function ix_pixel
  End Interface
  Interface
   Integer Function iy_pixel (y)
    Use data_numbers, Only: rp
    Real(Kind=rp), Intent(In) :: y
   End Function iy_pixel
  End Interface
!
! Locals
!
! Local parameters
  Integer, Parameter :: lxlab = 10 ! - length of xlab -
!
! Local scalars
  Integer :: ix1, ix2      ! - line X coordinates -
  Integer :: iy1, iy2, iy3 ! - line Y coordinates -
  Integer :: ix            ! - line coordinates -
  Integer :: isw           ! - string width -
  Integer :: isd           ! - string depth -
  Integer :: ntick         ! - tick increment -
!
  Real(Kind=rp) :: x     ! - x-value -
  Real(Kind=rp) :: xmin  ! - minimum x-value -
  Real(Kind=rp) :: tick  ! - tick-mark interval -
  Real(Kind=rp) :: atick ! - absolute tick-mark interval -
!
  Character(Len=lxlab) :: xlab ! - x-value label -
!
  Type(pdate) :: adate ! - current date -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Abs,Index,Max,Min,Mod,Nint,Present,Real,Repeat,Sign,Trim
!
! Executable Statements
!
! Plot axis
  xlab = Repeat(' ', lxlab)
  ix1 = x_axis%i_min
  ix2 = x_axis%i_max
  iy1 = iy_pixel(y_axis%aint)
  iy2 = iy1
  Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
!
! Add tick marks
  If (Present(inum)) Call gfont_size (font3)
  tick = x_axis%tick*Real(idir, Kind=rp)
  atick = Abs(tick)
  If (atick > zero .or. Present(inum)) Then
     iy2 = iy1 + Nint(Sign(three, tick))
     If (Present(inum)) Then
        iy3 = iy1 + 6*inum
        If (iy3 > iy1) iy3 = iy3 + iy_font
     Else
        iy3 = iy1
     End If
     xmin = Min(x_axis%amin, x_axis%amax)
! - dates -
     If (x_axis%ldate) Then
        Select Case (iseq)
         Case (isq_yr)
           ntick = Nint(x_axis%tick)
           adate%iyr = Nint(xmin)
           adate%imn = 0
           adate%idy = 0
         Case Default
           ntick = Nint(x_axis%tick)
           adate = idate_to_pdate(Nint(xmin))
        End Select
        x = xmin - atick
        Do
           x = x + atick
           If (x > Max(x_axis%amin, x_axis%amax)) Exit
           ix1 = ix_pixel(x)
           If (atick > zero) Then
              ix2 = ix1
              Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
           End If
           If (Present(inum)) Then ! - add date labels -
              Select Case (iseq)
               Case (isq_yr) ! - annual -
                 Write (Unit=xlab, Fmt='(I4)') adate%iyr
               Case (isq_sn) ! - seasonal -
                 If (ntick > 1) Then
                    If (adate%imn == 1) Then
                       Write (Unit=xlab, Fmt='(I4)') adate%iyr
                    Else
                       Write (Unit=xlab, Fmt='(A)') ' '
                    End If
                 Else
                    Write (Unit=xlab, Fmt='(A)') cmon(adate%imn:adate%imn+2, ilang)
                 End If
               Case (isq_mn, isq_so) ! - monthly -
                 If (ntick > 1) Then
                    If (adate%imn == 1) Then
                       Write (Unit=xlab, Fmt='(I4)') adate%iyr
                    Else
                       Write (Unit=xlab, Fmt='(A)') ' '
                    End If
                 Else
                    Write (Unit=xlab, Fmt='(A)') cmon(adate%imn, ilang)
                 End If
               Case (1, 5, 7) ! - daily, pentad, weekly -
                 Write (Unit=xlab, Fmt='(I2.2,1X,A)') adate%idy, cmon(adate%imn, ilang)
               Case (10) ! - dekadal -
                 Write (Unit=xlab, Fmt='(I2.2,1X,A)') adate%idy, cmon(adate%imn, ilang)
              End Select
              Call get_text_size$ (Trim(xlab), isw, isd)
              Call draw_characters$ (Trim(xlab), ix1-isw/2, iy3, icol(1))
           End If
           adate = adate + ntick
        End Do
! - numbers -
     Else If (atick > zero) Then
        x = xmin - Mod(xmin, tick)
        If (.not.x < xmin) x = x - atick
        Do
           x = x + atick
           If (x > Max(x_axis%amin, x_axis%amax)) Exit
           ix1 = ix_pixel(x)
           If (atick > zero) Then
              ix2 = ix1
              Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
           End If
           If (Present(inum)) Then ! - add numeric labels -
              If (atick < one) Then
                 Write (Unit=xlab, Fmt='(F10.2)') x
              Else If (atick < five) Then
                 Write (Unit=xlab, Fmt='(F10.1)') x
              Else
                 Write (Unit=xlab, Fmt='(I10)') Nint(x)
              End If
              ix = Index(xlab, ' ', BACK=.true.) + 1
              Call get_text_size$ (xlab(ix:lxlab), isw, isd)
              Call draw_characters$ (xlab(ix:lxlab), ix1-isw/2, iy3, icol(1))
           End If
        End Do
     End If
  End If
!
! Add label
  If (Present(inum)) Call gfont_size (font1)
  If (llab) Then
     Call get_text_size$ (Trim(x_axis%label), isw, isd)
     ix1 = (x_axis%i_min + x_axis%i_max - isw)/2
     iy1 = iy3 + iano*(isd + 1)
     Call draw_characters$ (Trim(x_axis%label), ix1, iy1, icol(1))
  End If
!
  Return
 End Subroutine draw_xaxis
!
!
!
 Subroutine draw_yaxis (y_axis,idir,llab,iy_font,ix_pixel,iy_pixel,inum,iano)
!
! Draws a y-axis
!
! Modules
  Use clrwin$,      Only: draw_characters$, draw_line_between$, get_text_size$, rotate_font$
  Use data_numbers, Only: zero, one, three, five, ten, r90, r270
  Use gui,          Only: icol
  Use screen,       Only: font1, font3, &
                          gfont_size
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: idir    ! - tick-mark direction (<0 down, >0 up) -
  Integer, Intent(In) :: iy_font ! - vertical font size in pixels -
!
  Logical, Intent(In) :: llab ! - draw labels? -
!
  Type(axis), Intent(In) :: y_axis ! - y-axis -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: inum ! - numerical labels positioning indicator (<0 left, >0 right) -
  Integer, Intent(In), Optional :: iano ! - annotation positioning indicator (<0 left, >0 right) -
!
! Procedure arguments
  Interface
   Integer Function ix_pixel (x)
    Use data_numbers, Only: rp
    Real(Kind=rp), Intent(In) :: x
   End Function ix_pixel
  End Interface
  Interface
   Integer Function iy_pixel (y)
    Use data_numbers, Only: rp
    Real(Kind=rp), Intent(In) :: y
   End Function iy_pixel
  End Interface
!
! Locals
!
! Local parameters
  Integer, Parameter :: lylab = 10 ! - length of ylab -
  Integer, Parameter :: lfmt  = 11 ! - length of fmt -
!
! Local scalars
  Integer :: ix1, ix2, ix3, ix4, ix5 ! - line X coordinates -
  Integer :: iy1, iy2, iy3           ! - line Y coordinates -
  Integer :: ix                      ! - line coordinates -
  Integer :: isw, isd                ! - string width and depth -
  Integer :: ntick                   ! - tick counter -
!
  Real(Kind=rp) :: y     ! - y-value -
  Real(Kind=rp) :: ymin  ! - minimum y-value -
  Real(Kind=rp) :: ymax  ! - maximum y-value -
  Real(Kind=rp) :: yy    ! - y-value for minor tick marks -
  Real(Kind=rp) :: tick  ! - tick-mark interval -
  Real(Kind=rp) :: atick ! - absolute tick mark interval -
!
  Character(Len=lylab) :: ylab ! - y-value label -
  Character(Len= lfmt) :: cfmt ! - format statement -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Abs, Index, Log10, Max, Min, Mod, Nint, Present, Real, Repeat, Sign, Trim
!
! Executable Statements
!
! Plot axis
  ylab = Repeat(' ', lylab)
  ix1 = ix_pixel(x_axis%aint)
  ix2 = ix1
  iy1 = y_axis%i_min
  iy2 = y_axis%i_max
  Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
!
! Add tick marks
  If (Present(inum)) Call gfont_size (font3)
  If (.not.y_axis%laxis) Then
     tick = y_axis%tick*Real(idir, Kind=rp)
     atick = Abs(tick)
  Else
     tick = one
     atick = one
  End If
  If (atick > zero .or. Present(inum)) Then
     ix2 = ix1 + Nint(Sign(three, tick))
     If (Present(inum)) Then
        ix3 = ix1 + 6*inum
        ix5 = ix3
     End If
     If (.not.y_axis%laxis) Then
        ymin = Min(y_axis%amin, y_axis%amax)
        y = ymin - Mod(ymin, tick)
        If (.not.y < ymin) y = y - atick
        ymax = Max(y_axis%amin, y_axis%amax)
     Else
        ymin = y_axis%amin
        ymax = y_axis%amax
        y = ymin - tick
     End If
     Do
        y = y + atick
        If (y > ymax) Exit
        iy1 = iy_pixel(y)
        If (atick > zero) Then
           iy2 = iy1
           Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
        End If
!
! Add numberic labels
        If (Present(inum)) Then
           iy3 = iy1 + iy_font/2
! - linear axes -
           If (.not.y_axis%laxis) Then
              If (atick < one) Then
                 Write (ylab, Fmt='(F10.2)') y
              Else If (atick < five) Then
                 Write (ylab, Fmt='(F10.1)') y
              Else
                 Write (ylab, Fmt='(I10)') Nint(y)
              End If
! - log axes -
           Else
              If (y < zero) Then
                 Write (cfmt, Fmt='(A,2(I2.2,A))') '(T', lylab+Nint(y)-2, ',A,I', 1-Nint(y), ')'
                 Write (ylab, Fmt=cfmt) '1/', Nint(ten**(-y))
              Else
                 Write (ylab, Fmt='(I10)') Nint(ten**y)
              End If
           End If
           ix = Index(ylab, ' ', BACK=.true.)+1
           Call get_text_size$ (ylab(ix:lylab), isw, isd)
           ix4 = ix3 - isw
           Call draw_characters$ (ylab(ix:lylab), ix4, iy3, icol(1))
           ix5 = Min(ix5, ix4)
        End If
!
! Add minor tick marks
        If ((y_axis%laxis) .and. (atick > zero)) Then
           Do ntick = 2, 9
              yy = y + Log10(Real(ntick, Kind=rp))
              If (yy > ymax) Exit
              iy1 = iy_pixel(yy)
              iy2 = iy1
              Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
           End Do
        End If
     End Do
  End If
!
! Add label
  If (Present(inum)) Call gfont_size (font1)
  If (llab) Then
     If (y_axis%amax > y_axis%amin) Then
        Call rotate_font$ (r90)
     Else
        Call rotate_font$ (r270)
     End If
     Call get_text_size$ (Trim(y_axis%label), isw, isd)
     ix1 = ix5 + iano*isd/4
     iy1 = (y_axis%i_min + y_axis%i_max + isw)/2
     Call draw_characters$ (Trim(y_axis%label), ix1, iy1, icol(1))
     Call rotate_font$ (zero)
  End If
!
  Return
 End Subroutine draw_yaxis
End Module axes
