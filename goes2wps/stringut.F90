!-----------------------------------------------
!   Yonggang G. Yu    last update: 3/25/2014
!   Institute of Geoscience
!   Uni-Frankfurt Riedberg
!   only one stringut.f90 on this machine
!   read/scan patterns and strings from file
!-----------------------------------------------


subroutine split_string (string, mark, length_mx, &
     mxseg, nseg, str_piece, jstatus)
  integer,           intent (in) :: length_mx
  character (len=length_mx), intent (in) :: string
  character (len=1), intent (in) :: mark
  integer,           intent (in) :: mxseg
  integer,           intent (out):: nseg
  character (len=length_mx), intent (out):: str_piece(mxseg)
  logical,           intent (out):: jstatus
  INTEGER                        :: len1, l
  integer                        :: iseg
  integer,           allocatable :: ipos(:)
  !
  !   xxxx_yy_zz_uu_vv
  !       ^  ^  ^  ^
  !       |  |  |  |
  !
  !       mark
  !
  
  len1 = LEN_TRIM( string )  
  allocate (ipos(len1))
  iseg=0
  do l=1, len1
     if (mark .eq. string(l:l)) then
        iseg=iseg+1
        ipos(iseg)=l
!        write(6,*) 'match!', l
!        write(6,*) 'ipos ', iseg, ' = ', l
     endif
  enddo
  if (iseg.eq.0 .or. iseg.gt.mxseg-1) then 
     call error ('split_string', 'find nseg .eq.0 or > 4', 1)
     jstatus=.false.
     return
  else
     jstatus=.true.
  endif
  nseg=iseg
  !
  !
  str_piece(:)=''
  lw=1    ! lw, lh: two index positions
  do l=1, nseg
     lh=ipos(l)-1
     do iseg=lw, lh
        str_piece(l)=trim(str_piece(l))//string(iseg:iseg)
     enddo
     lw=ipos(l)+1
  enddo
  if (lw.le.len1) then
     lh=len1
     do iseg=lw, lh
        str_piece(l)=trim(str_piece(l))//string(iseg:iseg)
     enddo
  endif
  nseg=nseg+1  ! must add one bc of eggs and '_'
  if (nseg.gt.mxseg) then
     call error ('split_string', 'nseg exceeds mx', 1)
  endif
  str_piece(nseg+1:mxseg)='xxx'
  return
end subroutine split_string



subroutine scan_begin (iunps, substring, rew, istatus)  
  !---------------------------------------------------------------------
  !
  implicit none
  ! unit of input
  integer, intent(in) :: iunps  
  ! Label to be matched
  character (len=*), intent(in) :: substring
  logical, intent(in) :: rew
  integer, intent(out) :: istatus
  ! String read from file
  character (len=100) :: line  
  ! Flag if .true. rewind the file
  logical, external :: matchbgn
  integer :: ios
  !
  ios = 0
  istatus = 0
  if (rew) rewind (iunps)  
  do while (ios==0)  
     read (iunps, '(a100)', iostat = ios, err = 300) line  
     if (matchbgn (line, substring) ) then
        istatus=ios
        return
     endif
  enddo
  istatus=ios; return
300 istatus=1
  call error_nonstop ('scan_begin', & 
       'No '//trim(substring)//' block', abs (ios) )  
end subroutine scan_begin



subroutine scan_contain (iunps, stop_string, rew)  
  !---------------------------------------------------------------------
  !
  implicit none
  integer, intent(in) :: iunps
  character (len=*), intent(in) :: stop_string  
  logical, intent(in) :: rew            ! if rewind
  character (len=100) :: line
  logical, external :: matches          ! function name
  integer :: ios
  !
  ios = 0
  if (rew) rewind (iunps)
  do while (ios==0)  
     read (iunps, '(a100)', iostat = ios, err = 300) line
     if (matches (line, stop_string) ) return  
  enddo
  return
300 call error_nonstop ('scan_contain', &
         'No '//trim(stop_string)//' block', abs (ios) )
end subroutine scan_contain



subroutine scan_io (iunps, ounps, stop_string, rew)  
  !---------------------------------------------------------------------
  !
  implicit none
  integer, intent(in) :: iunps, ounps
  character (len=*), intent(in) :: stop_string  
  logical, intent(in) :: rew            ! if rewind
  character (len=100) :: line
  logical, external :: matches          ! function name
  integer :: ios
  !
  ios = 0
  if (rew) rewind (iunps)
  do while (ios==0)  
     read (iunps, '(a100)', iostat = ios, err = 300) line
     if (ios.NE.0) then
        ! somehting wrong or end of file
        goto 300
     endif
     !
     if (matches (line, stop_string) ) return  
     if (ios .NE. 0) return  ! meaning something wrong or end of file
     write(ounps, '(a100)') line
  enddo
  return
300 call error_nonstop ('scan_io', &
         'No '//trim(stop_string)//' block', abs (ios) )
end subroutine scan_io



function matchbgn ( string, substring )
  ! only begin with
  ! string:  main-str
  ! substring:  sub-str
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(IN) :: string, substring
  LOGICAL                       :: matchbgn
  if (index(string, substring).eq.1) then
     matchbgn = .TRUE.
  else
     matchbgn = .FALSE.
  endif
  return
end function matchbgn



!-----------------------------------------------------------------------
function matches( string, substring )  
  !-----------------------------------------------------------------------
  !
  ! ... .TRUE. if string is contained in substring, .FALSE. otherwise
  !
  IMPLICIT NONE
  !
  CHARACTER (LEN=*), INTENT(IN) :: string, substring
  LOGICAL                       :: matches
  INTEGER                       :: l  

  l=index (string, substring)
  if (l.ge.1) then
     matches = .TRUE. 
  else
     matches = .FALSE.
  endif
  RETURN
end function matches


!------
!************************
!------ print string
!************************
!
!
!function mystring(i)
!  ! currently only for i <= 999
!  character (len=10) :: mystring
!  character (len=1 ) :: car1, car2, car3, car4, car5
!  integer            :: i, m1, m2, m3, m4, m5
!  m1 = mod( i, 10)
!  m2 = mod( i, 100   )/10
!  m3 = mod( i, 1000  )/100
!  m4 = mod( i, 10000 )/1000
!  m5 = i/10000
!  car1= char( 48+ m1 )
!  car2= char( 48+ m2 )
!  car3= char( 48+ m3 )
!  car4= char( 48+ m4 )
!  car5= char( 48+ m5 )
!
!  if ( i.ge. 10000 .and. i.le.99999 ) then        ! 10000 -- 99999
!     mystring = car5//car4//car3//car2//car1
!  elseif ( i.ge. 1000 .and. i.le.9999 ) then      ! 1000 -- 9999
!     mystring = car4//car3//car2//car1
!  elseif ( i.ge. 100  .and. i.le.999 ) then       ! 100 -- 999 
!     mystring = car3//car2//car1
!  elseif ( i.ge. 10 ) then                        ! 10 --  99       
!     mystring = car2//car1
!  elseif ( i.le. 9 .and. i .ge. 0 ) then                      
!     mystring = car1
!  else                                                        
!     stop ' wrong negative i '                                
!  endif 
!  return
!end function mystring


function mystring(i)
  character (len=20)  :: mystring
  integer, intent(in) :: i
  write (mystring, *) i
  mystring = trim(adjustl(mystring))
  if (i .gt. 10**5 )  STOP  'mystring exceeds 1.d5, stop.'
  return
end function mystring


function mystring_f(x)
  character (len=20)  :: mystring_f
  real*8, intent(in) :: x
  write (mystring_f, '(f4.1)') x
  mystring_f = trim(adjustl(mystring_f))
  !if (x .gt. int(10**20))  STOP  'mystring exceeds 1.d20, stop.'
  return
end function mystring_f




subroutine error(insubroutine, message, ierr )
  character (len=*), intent (in) :: insubroutine
  character (len=*), intent (in) :: message
  integer, intent (in) :: ierr
  !
  write (6, 11)   
  write (6, 12)  trim(insubroutine), trim(message), ierr
  write (6, 11)
  stop
11  format ('**====================**')
12  format (2x, a, 4x, a, 4x, "ierr =", i4)
  return
end subroutine error



subroutine error_nonstop( insubroutine, message, ierr )
  character (len=*), intent (in) :: insubroutine
  character (len=*), intent (in) :: message
  integer, intent (in) :: ierr
  !
  write (6, 11)   
  write (6, 12)  trim(insubroutine), trim(message), ierr
  write (6, 11)
11  format ('**====================**')
12  format (2x, a, 4x, a, 4x, "ierr =", i4)
  return
end subroutine error_nonstop



subroutine go_last_patn (iunps, substring, outline, rew)  
  !---------------------------------------------------------------------
  !
  implicit none
  integer, intent(in) :: iunps  
  logical, intent(in) :: rew  
  character (len=*), intent(in) :: substring  
  character (len=150), intent(out) :: outline   ! fixed 
  character (len=150) :: line
  integer :: ios, nr, mx
  !

  if (rew) rewind (iunps)
  ios=0
  nr=0
  do while (ios==0)  
     read (iunps, '(a150)', iostat = ios, err = 300) line
     if (index(line, substring).ge.1 ) then
        nr=nr+1
!        write (6,*) 'nr', nr
     endif
  enddo

  rewind (iunps)  
  ios=0
  mx=0
  do while (ios==0)  
     read (iunps, '(a150)', iostat = ios, err = 300) line
     if (index(line, substring).ge.1 ) then
        mx=mx+1
        if (mx.eq.nr) then
           outline=line
           return
        endif
     endif
  enddo
300 continue
end subroutine go_last_patn



!------
!************************

subroutine scan_test (iunps, substring, msg, rew)  
  implicit none
  integer, intent(in) :: iunps  
  character (len=*), intent(in) :: substring  
  logical, intent(in) :: rew
  integer, intent(out):: msg

  character (len=100) :: line  
  logical, external   :: matches
  integer :: ios
  !
  ios = 0
  msg=0
  if (rew) rewind (iunps)  
  do while (ios==0)  
     read (iunps, '(a100)', iostat = ios) line  
     if (ios.NE.0) then
        ! something wrong or end of file
        return
     endif
     !
     if (matches (line, substring) ) then
        !-- ck
        !write(6, '(/,a,a)')  'msg=1, mathced line= ', trim(line)
        msg=1
        return  
     endif
  enddo
  return 
end subroutine scan_test



subroutine getNwords (string, nwd, chartyp)
  CHARACTER (LEN=*), intent(in) :: string
  integer,           intent(out):: nwd
  CHARACTER (LEN=1), intent(out):: chartyp
  !
  CHARACTER (LEN=LEN(string))   :: substring
  integer                       :: key(LEN(string))  ! allocate array
  integer  LN
  logical  con, con1, con2, con3
  CHARACTER (LEN=1)    CH1
  !
  ! method
  !  C s         T l 1        T l    F
  ! [1 1 0 0 0   1 1 1 0 0    1 1 0  1]
  !      w            w          w
  !
  substring=trim(ADJUSTL(string))
  LN=LEN(trim(substring))
  !
  CH1=substring(1:1)
  con1= CH1>='0'.AND.CH1<='9'
  if (con1) then
     chartyp='I'
  else
     chartyp='A'
  endif
  !
  do i=1, LN
     CH1=substring(i:i)
     con1= CH1>='0'.AND.CH1<='9'
     con2= CH1>='a'.AND.CH1<='z'
     con3= CH1>='A'.AND.CH1<='Z'
     con = con1.OR.con2.OR.con3
     if (con) then
        key(i)=1
        !write(6,  101)  substring(i:i)
     else
        key(i)=0
        !write(6, *) 'sp'
     endif
  enddo
  nwd=0
  if (key(1).eq.0 .or. key(LN).eq.0) &
       STOP 'findwords fail in ADJUSTL and trim'
  do i=1, LN-1
     if(key(i).gt.key(i+1)) nwd=nwd+1
  enddo
  nwd=nwd+1
  !-- ck only
  !write(6, '(a,a)' ) 'chartyp=', chartyp 
  !write(6, '(a,i3)') 'LEN=', LN
  !write(6, '(a,i3)') 'nwd=', nwd
  return
101 format (10a)
111 format (10a200)
121 format (10i4)
end subroutine getNwords





subroutine combine_words (string, nwd, line)
  CHARACTER (LEN=*), intent(in) :: string
  integer,           intent(out):: nwd
  CHARACTER (LEN=100), intent(out):: line       ! fixed length 100
  !
  integer                       :: LN
  CHARACTER (LEN=1)             :: chartyp
  CHARACTER (LEN=LEN(string))   :: substring
  integer                       :: key(LEN(string))  ! allocate array

  logical  con, con1, con2, con3
  CHARACTER (LEN=1)    CH1
  !
  ! method
  !  C s         T l 1        T l    F
  ! [1 1 0 0 0   1 1 1 0 0    1 1 0  1]
  !      w            w          w
  !
  substring=trim(ADJUSTL(string))
  LN=LEN(trim(substring))
  !
  CH1=substring(1:1)
  con1= CH1>='0'.AND.CH1<='9'
  if (con1) then
     chartyp='I'
  else
     chartyp='A'
  endif
  !
  line=''
  do i=1, LN
     CH1=substring(i:i)
     con1= CH1>='0'.AND.CH1<='9'
     con2= CH1>='a'.AND.CH1<='z'
     con3= CH1>='A'.AND.CH1<='Z'
     con = con1.OR.con2.OR.con3
     if (con) then
        key(i)=1
        line=trim(line)//CH1
        !write(6,  101)  substring(i:i)
     else
        key(i)=0
        !write(6, *) 'sp'
     endif
  enddo
  nwd=0
  do i=1, LN-1
     if(key(i).gt.key(i+1)) nwd=nwd+1
  enddo
  nwd=nwd+1
!  write(6, '(a,a7)' ) 'chartyp =', chartyp 
!  write(6, '(a,i10)') 'LEN=', LN
!  write(6, '(a,i10)') 'nwd=', nwd
!  write(6, '(a,a20)') 'line=', trim(line)

  return
101 format (10a)
111 format (10a200)
121 format (10i4)
end subroutine combine_words




!-------------------------------------------
!
!  remove char inbetween '(' ')'  0.1257(8)  0.457(2)  0.835(7)
!
!-------------------------------------------
subroutine blackout_pair_symbols (Lmx, pairmx, line, line2, CHa, CHb, ncount, seq)
  implicit none
  integer,            intent(in) :: Lmx       ! string length max
  integer,            intent(in) :: pairmx    ! max number of pairs  
  character(len=1),   intent(in) :: CHa, CHb
  character(len=Lmx), intent(in) :: line
  character(len=Lmx), intent(out):: line2
  integer,            intent(out):: ncount
  integer,            intent(out):: seq(pairmx)

  character(len=Lmx)             :: string
  integer  ::  j, ja, jb, djab
  integer  ::  i, bg, length

  ! j  is length
  ! ja is position of CHa


  line2=line  
  string=trim(line)
  length=LEN(trim(line))
  ja=index(string, CHa)
  jb=index(string, CHb)
  djab=jb-ja

  if (jb.eq.0) then
     ncount=2
     seq(1:2)=1
     return
  else
     j=LEN(trim(string(jb:Lmx)))
  endif


  bg=0
  ncount=0
  do while (j.GT.0 .AND. djab.GT.0 )
     seq(ncount+1)=bg+ja
     seq(ncount+2)=bg+jb
     do i=seq(ncount+1), seq(ncount+2)
        line2(i:i)=''
     enddo
     !
     ! truncate the string
     bg=bg+jb
     if (bg.eq.length) exit
     string=trim(line(bg+1:Lmx))
     ja=index(string, CHa)
     jb=index(string, CHb)
     djab=jb-ja
     if (jb.eq.0) exit
     j=LEN(trim(string(jb:Lmx)))
     ncount=ncount+2
  enddo

  return
end subroutine blackout_pair_symbols


subroutine remove_number_in_string (string1, string2, length)
  implicit none
  integer, intent (in) :: length
  character (len=length), intent(in) :: string1
  character (len=length), intent(out):: string2
  
  integer  i
  integer  kk
  character*1   CH1
  logical       con1
  character (len=length) :: string3

  kk=LEN(string1)
  do i=1, kk
     CH1=string1(i:i)
     con1= CH1>='0'.AND.CH1<='9'
     if (con1) then
        string3(i:i)=''
     else
        string3(i:i)=string1(i:i)
     endif
  enddo
  string2=trim(ADJUSTL(string3))
  return
end subroutine remove_number_in_string



subroutine remove_quote_in_string (string1, string2, length)
  implicit none
  integer, intent (in) :: length
  character (len=length), intent(in) :: string1
  character (len=length), intent(out):: string2
  
  integer  i
  integer  kk
  character*1   CH1
  logical       con1
  character (len=length) :: string3

  kk=LEN(string1)
  string3=string1
  do i=1, kk
     CH1=string1(i:i)
     if (CH1==CHAR(39).OR.CH1==CHAR(34)) string3(i:i)=''
  enddo
  string2=trim(ADJUSTL(string3))
  return
end subroutine remove_quote_in_string
