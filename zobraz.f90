!> \file zobraz.f90
!> \brief vytvareni obrazku v *.eps formatu
!!
!! nastroje pro konstrukci grafickeho vystupu v *.eps formatu\n
!! pouziva jen zakladni konstrukce postscriptu \n
!! cely obrazek je opatren Bounding Boxem
!!
!<

!> \brief kreslitka
!!
!! nastroje pro konstrukci grafickeho vystupu v *.eps formatu\n
!! pouziva jen zakladni konstrukce postscriptu \n
!! cely obrazek je opatren Bounding Boxem
!!
!<
module zobraz

    !> kind pro realna cisla ploticich utilitek
    integer, public, parameter :: rpl = selected_real_kind(15,99)
    !>   typ pro dvojrozmerny bod
    !!
    !! Dalsi typy z ekvivalentniho ADA kodu:
    !! Type Point2d_Array is array (Integer range <>) of Point2d
    !!  type(Point2d), dimension(:)
    !<
    type, public :: Point2d
        !> X-ova souradnice
        real(kind=rpl) :: x
        !> Y-ova souradnice
        real(kind=rpl) :: y
    end type Point2d

    !>   typ pro trojrozmerny bod
    type, public :: Point3d
        !> X-ova souradnice
        real(kind=rpl) :: x
        !> Y-ova souradnice
        real(kind=rpl) :: y
        !> Z-ova souradnice
        real(kind=rpl) :: z
    end type Point3d


    !> vnitrni data obrazku
    type, public :: Picture_Type
        !> popisovac souboru s obrazkem
        integer :: Fil
        !> jmeno vytvoreneho souboru
        character(len=1024) :: Name
        !> skutecne rozmery obrazzku v milimetrech
        real(kind=rpl) :: Xmin, Xmax, Ymin, Ymax
        !> jeden mm v bodech
        real(kind=rpl) :: Onemm_X, Onemm_Y
        !> posuv v bodech
        real(kind=rpl) :: Shft_X, Shft_Y
        !> zvetseni ve smeru x
        real(kind=rpl) :: Scale_X
        !> zvetseni ve smeru y
        real(kind=rpl) :: Scale_Y
    end type Picture_Type

    !> typ pro slozky barvy
    !!
    !!  Dalsi typy z ekvivalentniho ADA kodu:
    !! Type Color_Map is array (Index range <>) of RGB_Color_Type
    !!  type(RGB_Color_Type), dimension(:)
    !! Type Palete is access Color_Map
    !!  type(RGB_Color_Type), dimension(:), pointer
    !! Type CMap_Type is (Default, Jet).
    !!   Default .... 0
    !!   Jet     .... 1
    !<
    type, public ::  RGB_Color_Type
        !> jednotlive barevne slozky
        real(kind=rpl) :: R, G, B
    end type RGB_Color_Type


    public :: graph_plot
    public :: Create
    public :: Close
    public :: Show
    public :: Print
    public :: Axis
    public :: Line2d
    public :: Fill2d
    public :: Create_Colormap

    private :: Namer_Get_Name
    private :: Abs_X
    private :: Abs_Y

contains

    !>  demo pro kreslici utilitky
    subroutine graph_plot()
        !> \cond TR
        ! v F-mode zakazano
        implicit none
        type(Picture_Type) :: P
        real(kind=rpl) :: Size = 17
        integer :: i,nc
        type(Point2d) :: kam, kam1
        type(Point2d), dimension(1:4) :: cesta
        type(RGB_Color_Type) :: barva
        real(kind=rpl) :: tloustka,xstart,xh
        real(kind=rpl), dimension(:), allocatable :: xx
        real(kind=rpl), dimension(2)              :: yy
        type(RGB_Color_Type), dimension(:),pointer :: Pl => null()

        print *, "demo pro grafiku"
        kam%X = 30
        kam%Y =  20
        kam1%X = -20
        kam1%Y = -50
        barva%R = 0
        barva%G = 0
        barva%B = 0.9
        tloustka =3
        !print *, kam
        call Create(P, 800.0_rpl,450.0_rpl,-100.0_rpl,100.0_rpl,-50.0_rpl,&
                    100.0_rpl,"pok1ftn.ps","Graficke demo")
        call Axis(P,10,10)
        call Print(P,"Tisk textu na zadane misto",kam,Size,barva)
        call Print(P,"defaultni barva a tloustka",kam1)
        !namaluju nakreslim barevnou mapu
        call Create_Colormap(Pl,7)
        yy = (/ 40, 50 /)
        allocate(xx(1:8))
        do i=1,8
            xx(i) = i*10+20
        end do
        do i=1,7
            print *, Pl(i)
            barva = Pl(i)
            cesta(1)%x=xx(i)
            cesta(1)%y=yy(1)
            cesta(2)%x=xx(i+1)
            cesta(2)%y=yy(1)
            cesta(3)%x=xx(i+1)
            cesta(3)%y=yy(2)
            cesta(4)%x=xx(i)
            cesta(4)%y=yy(2)
            call Fill2d(P,cesta,barva)
            call Line2d(P,cesta)
        end do
        ! ted nejakou podrobnejsi
        yy = yy + 12
        nc = 253
        deallocate(xx)
        allocate(xx(1:nc+1))
        xstart = 30
        xh =  70.0/nc
        do i=1,nc+1
            xx(i) = (i-1)*xh+xstart
        end do
        call Create_Colormap(Pl,nc)
        do i=1,nc
            print *, Pl(i)
            barva = Pl(i)
            cesta(1)%x=xx(i)
            cesta(1)%y=yy(1)
            cesta(2)%x=xx(i+1)
            cesta(2)%y=yy(1)
            cesta(3)%x=xx(i+1)
            cesta(3)%y=yy(2)
            cesta(4)%x=xx(i)
            cesta(4)%y=yy(2)
            call Fill2d(P,cesta,barva)
            !call Line2d(P,cesta)
        end do
        ! ted nejakou jinou
        yy = yy + 12
        nc = 253
        deallocate(xx)
        allocate(xx(1:nc+1))
        xstart = 30
        xh =  70.0/nc
        do i=1,nc+1
            xx(i) = (i-1)*xh+xstart
        end do
        call Create_Colormap(Pl,nc,1)
        do i=1,nc
            print *, Pl(i)
            barva = Pl(i)
            cesta(1)%x=xx(i)
            cesta(1)%y=yy(1)
            cesta(2)%x=xx(i+1)
            cesta(2)%y=yy(1)
            cesta(3)%x=xx(i+1)
            cesta(3)%y=yy(2)
            cesta(4)%x=xx(i)
            cesta(4)%y=yy(2)
            call Fill2d(P,cesta,barva)
            !call Line2d(P,cesta)
        end do
        call Close(P)
        call Show(P)
        !> \endcond TR
    end subroutine graph_plot

    !> vytvoreni - inicializace obrazku
    subroutine Create(P, Size_X,Size_Y,Xmin, Xmax, Ymin, Ymax, Name, Caption)
        use pmatypy
        use pmatools
        implicit none
        !v F-mode zakazano
        !> obrazek
        type(Picture_Type), intent(in out) :: P
        !> x-ovy rozmer obrazku v mm
        real(kind=rpl), intent(in)         :: Size_X
        !> y-ovy rozmer obrazku v mm
        real(kind=rpl), intent(in)         :: Size_Y
        !> minimalni x-ova souradnice
        real(kind=rpl), intent(in)         :: Xmin
        !> maximalni x-ova souradnice
        real(kind=rpl), intent(in)         :: Xmax
        !> minimalni y-ova souradnice
        real(kind=rpl), intent(in)         :: Ymin
        !> maximalni y-ova souradnice
        real(kind=rpl), intent(in)         :: Ymax
        !> jmeno vytvareneho souboru s obrazkem, nepovinne
        character(len=*), intent(in), optional :: Name
        !> popiska obrazku, nepovinne
        character(len=*), intent(in), optional :: Caption
        !>   -- Size_? je v milimetrech
        character(len=12) :: Local_Name

        if (.not. present(Name)) then
            call Namer_Get_Name(Local_Name)
            P%Name = Local_Name
        else
           P%Name = Name
        end if
        call get_free_unit(P%Fil)
        !open(unit=P%Fil, FILE=Name, STATUS="unknown", ACTION="write")
        call safe_open_write(P%Name,P%Fil,1)

    write(unit=P%Fil,fmt="(a23)") "%!PS-Adobe-3.0 EPSF-3.0"
    write(unit=P%Fil,fmt="(a19,i0,a,i0,a)") "%%BoundingBox: 0 0 ",int(72.0/25.4*Size_X)+1, &
    " ",int(72.0/25.4*Size_Y)+1," "
    write(unit=P%Fil,fmt=*) "gsave"
    P%Xmin = Xmin
    P%Xmax = Xmax
    P%Ymin = Ymin
    P%Ymax = Ymax
    P%Onemm_X = (Xmax-Xmin)/(72.0*Size_X/25.4)
    P%Onemm_Y = (Ymax-Ymin)/(72.0*Size_Y/25.4)
    if ( present(Caption) ) then
        write(unit=P%Fil,fmt=*) "/Times_Roman findfont 10 scalefont setfont"
        write(unit=P%Fil,fmt=*)" 13 13 moveto (" ,Caption , ") show"
    end if
    P%Shft_X = 10.0
    P%Shft_Y = 10.0
    P%Scale_X = (72.0*Size_X/25.4 - 20.0)/(Xmax - Xmin)
    P%Scale_Y = (72.0*Size_Y/25.4 - 20.0)/(Ymax - Ymin)
    ! nastaveni clipu
    write(unit=P%Fil,fmt=*) " newpath"
    write(unit=P%Fil,fmt=*) " 0 0  moveto"
    write(unit=P%Fil,fmt=*) (72.0/25.4*Size_X), " 0  lineto"
    write(unit=P%Fil,fmt=*) (72.0/25.4*Size_X), "  ",(72.0/25.4*Size_Y), " lineto"
    write(unit=P%Fil,fmt=*) " 0  ", (72.0/25.4*Size_Y), " lineto"
    write(unit=P%Fil,fmt=*) "closepath clip"
end subroutine Create



!> dokonci tvorbu obrazku a zavre soubor
subroutine Close (P)
    implicit none
    !> obrazek
    type(Picture_Type), intent(in out) :: P
    write(unit=P%Fil,fmt=*) "grestore"
    write(unit=P%Fil,fmt=*) "showpage"
    Close(P%Fil)
end subroutine Close




!> zobrazi obrazek
subroutine Show (P)
    implicit none
    !> obrazek
    type(Picture_Type), intent(in) :: P

    integer :: runconf = 0
    integer :: i
    ! behova konfigurace
    ! 0 ... neznamo
    ! 1 ... linux
    ! 2 ... windows
    print *, P%Name
    if (runconf==0) then
        print *, "vyber behove prostredi"
        print *, " 1 ... linux"
        print *, " 2 ... win"
        read(unit=*,fmt=*) runconf
    end if
    if (runconf==1) then
        i = system("gv " // P%Name // " &")
    else
        i = system("cmd /c start C:\Users\pmayer\AppData\Local\Apps\Evince-2.32.0\bin\evince " // P%Name // " &")
    end if
    print *,i
end subroutine Show



!   ----------------------------------------------------------------------------
!   --                                                                        --
!   --    konverzni funkce                                                    --
!   --                                                                        --
!   ----------------------------------------------------------------------------
function Abs_X( X, P) result(Ret_Val)
    implicit none
    real(kind=rpl),intent(in) :: X
    type(Picture_Type), intent(in) :: P
    real(kind=rpl) :: Ret_Val
    Ret_Val = (X-P%Xmin)*P%Scale_X + P%Shft_X
end function Abs_X

function Abs_Y( Y, P) result(Ret_Val)
    implicit none
    real(kind=rpl),intent(in) :: Y
    type(Picture_Type), intent(in) :: P
    real(kind=rpl) :: Ret_Val

    Ret_Val = (Y-P%Ymin)*P%Scale_Y + P%Shft_Y
end function Abs_Y



!   ----------------------------------------------------------------------------
!   --                                                                        --
!   -- management                                                             --
!   --                                                                        --
!   ----------------------------------------------------------------------------


!> vytvari posloupnost jmen
subroutine Namer_Get_Name(Name)
    implicit none
    !> vytvorene nove jmeno
    Character(len=*), intent(out) :: Name
    Character(len=12), save :: Name_Local = "pict00001.ps"
    Integer, save :: wrk = 1
    ! ted to opravdu udelat
    write(unit=Name_Local,fmt="(a4,i5.5,a3)") "pict",wrk,".ps"
    Name = Name_Local
    print *, "jsem v Namer_Local_Name",wrk
    wrk = wrk + 1
end subroutine Namer_Get_Name


!   -----------------------------------------------------------------------------
!   --                                                                         --
!   --      sprava barev                                                       --
!   --                                                                         --
!   -----------------------------------------------------------------------------

!> vytvori barevnou mapu
subroutine Create_Colormap     (Pl,NC,TC)
    ! v F-mode zakazano
    implicit none
    !> ukazatel na vytvarenou mapu
    type(RGB_Color_Type), dimension(:), pointer :: Pl
    !> pocet barev
    integer, intent(in) :: NC
    !> typ mapy
    integer, intent(in), optional :: TC
    real :: w
    integer :: i,ltc
    if ( .not. associated(Pl)) then
        allocate(Pl(1:NC))
    end if
    if ( ubound(Pl,1) < NC ) then
        deallocate (Pl)
        allocate(Pl(1:NC))
    end if

    if ( present(TC) ) then
        ltc = TC
    else
        ltc = 0
    end if
    if (ltc==0) then
        if ( NC < 9) then
            if ( NC > 0) then
                Pl (1) = RGB_Color_Type(0.0, 0.0, 0.0)
            end if
            if ( NC > 1) then
                Pl (2) = RGB_Color_Type(0.0, 0.0, 1.0)
            end if
            if ( NC > 2) then
                Pl (3) = RGB_Color_Type(0.0, 1.0, 0.0)
            end if
            if ( NC > 3) then
                Pl (4) = RGB_Color_Type(0.0, 1.0, 1.0)
            end if
            if ( NC > 4) then
                Pl (5) = RGB_Color_Type(1.0, 0.0, 0.0)
            end if
            if ( NC > 5) then
                Pl (6) = RGB_Color_Type(1.0, 0.0, 1.0)
            end if
            if ( NC > 6) then
                Pl (7) = RGB_Color_Type(1.0, 1.0, 0.0)
            end if
            if ( NC > 7) then
                Pl (8) = RGB_Color_Type(1.0, 1.0, 1.0)
            end if
        else
            !tady neco pruznejsiho
            do i=1,nc
                w = (1.0/nc)*i
                if ( w < 1.0/7) then
                    Pl(i)%R = 0
                    Pl(i)%G = 0
                    Pl(i)%B = w*7
                else if ( w < 2.0/7) then
                    Pl(i)%R = 0
                    Pl(i)%G = 7*w-1
                    Pl(i)%B = 2-7*w
                else  if ( w < 3.0/7) then
                    Pl(i)%R = 0
                    Pl(i)%G = 1
                    Pl(i)%B = 7*w-2
                else if ( w < 4.0/7) then
                    Pl(i)%R = 7*w-3
                    Pl(i)%G = 4-7*w
                    Pl(i)%B = 4-7*w
                else if ( w < 5.0/7) then
                    Pl(i)%R = 1
                    Pl(i)%G = 0
                    Pl(i)%B = 7*w - 4
                else if ( w < 6.0/7) then
                    Pl(i)%R = 1
                    Pl(i)%G = 7*w-5
                    Pl(i)%B = 6-7*w
                else
                    Pl(i)%R = 1
                    Pl(i)%G = 1
                    Pl(i)%B = 7*w-6
                end if
            end do
        end if
    ! ted jinou mapu
    else if (ltc==1) then
    !jet :
    ! (0,0,0.5) -> (0,0,1) -> (0,1,1)->(1,1,0) ->(1,0,0)->(0.5,0,0)
    !   0            0.2         0.4     0.6        0.8       1
    do i=1,nc
       w = (1.0/nc)*i
       if (w<0.2) then
          Pl(i)%R = 0
          Pl(i)%G = 0
          Pl(i)%B = 0.5+2.5*w
       else if (w<0.4) then
          Pl(i)%R = 0
          Pl(i)%G = 5*w-1
          Pl(i)%B = 1
       else if ( w < 0.6 ) then
          Pl(i)%R = 5*w-2
          Pl(i)%G = 1
          Pl(i)%B = -5*w+3
       else if ( w < 0.8 ) then
         Pl(i)%R = 1
         Pl(i)%G = -5*w+4
         Pl(i)%B = 0
       else
         Pl(i)%R = -2.5*w+3
         Pl(i)%G = 0
         Pl(i)%B = 0
       end if
    end do
    else
    end if

end subroutine Create_Colormap


!   ----------------------------------------------------------------------------
!   --                                                                        --
!   --  kreslici procedurky                                                   --
!   --                                                                        --
!   ----------------------------------------------------------------------------

subroutine Line2d(P, Path, Color,Thickness)
    !v F-mode zakazano
    implicit none
    type(Picture_Type), intent(in) :: P
    type(Point2d), dimension(:), intent(in) :: Path
    type(RGB_Color_Type), intent(in), optional :: Color
    real(kind=rpl), intent(in), optional :: Thickness
    integer :: I,i1,i2
    real(kind=rpl) :: LT
    type(RGB_Color_Type) :: Cl

    i1 = lbound(Path,1)
    i2 = ubound(Path,1)
    if(present(Thickness)) then
        LT = Thickness
    else
        LT = 1
    end if
    if (present(Color)) then
        Cl = Color
    else
        Cl%R = 0
        Cl%G = 0
        Cl%B = 0
    end if
    write(unit=P%Fil,fmt=*) "gsave newpath"
    write(unit=P%Fil,fmt=*) Cl%R, " ",Cl%G, " ",Cl%B," setrgbcolor"
    write(unit=P%Fil,fmt=*) LT*P%Onemm_X, " setlinewidth"
    write(unit=P%Fil,fmt=*) Abs_X(Path(i1)%X,P), "  ",Abs_Y(Path(i1)%Y,P), " moveto"
    do I=i1,i2
        write(unit=P%Fil,fmt=*) Abs_X(Path(I)%X,P), "  ",Abs_Y(Path(I)%Y,P), " lineto"
    end do
    write(unit=P%Fil,fmt=*) " stroke grestore"

end subroutine Line2d

subroutine Print(P,Text,Where,Size,Color)
    implicit none
    type(Picture_Type), intent(in) :: P
    character(len=*), intent(in) :: Text
    type(Point2d), intent(in) :: Where
    real(kind=rpl), intent(in), optional :: Size ! 6.0;
    type(RGB_Color_Type), intent(in), optional :: Color ! (0.0,0.0,0.0) )

    real(kind=rpl) :: Onemm
    real(kind=rpl) :: Size_loc
    type(RGB_Color_Type) :: CL1
    Onemm = P%Onemm_X
    if (Onemm < P%Onemm_Y) then
        Onemm = P%Onemm_Y
    end if
    if (present(Size)) then
        Size_loc = Size
    else
        Size_Loc = 6
    end if
    if (present(Color)) then
        CL1 = Color
    else
        CL1%R = 0
        CL1%G = 0
        CL1%B = 0
    end if
    write(unit=P%Fil,fmt=*) " gsave /Times-Roman findfont"
    write(unit=P%Fil,fmt=*) Size_loc," scalefont setfont"
    write(unit=P%Fil,fmt=*) " newpath"
    write(unit=P%Fil,fmt=*) CL1%R,CL1%G,CL1%B," setrgbcolor"
    write(unit=P%Fil,fmt=*) Abs_X(Where%X,P), "  ",Abs_Y(Where%Y,P)," moveto"
    write(unit=P%Fil,fmt=*) "(", Text, ") show"
    write(unit=P%Fil,fmt=*) "grestore"
end subroutine Print



subroutine Axis(P, X_Ticks, Y_Ticks)
    implicit none
    type(Picture_Type), intent(in) :: P
    Integer, intent(in) ::  X_Ticks,Y_Ticks

    Real(kind=rpl) :: X,Y
    type(Point2d), dimension(1:2)  :: Line
    Real(kind=rpl) :: Step
    Real(kind=rpl) :: Work
    Character(len=20) :: Sh_W
    Integer :: I

    if (P%Xmin*P%Xmax <= 0.0) then
        X = 0.0
    else
        X = P%Xmin
    end if
    if (P%Ymin*P%Ymax <= 0.0) then
        Y = 0.0
    else
        Y = P%Ymin
    end if
    Line = (/ Point2d(X,P%Ymin),Point2d(X,P%Ymax) /)
    call Line2d(P,Line,RGB_Color_Type(0.0,0.0,0.0),(P%Xmax-P%Xmin)/1000.0)
    Line = (/ Point2d(P%Xmin,Y),Point2d(P%Xmax,Y) /)
    call Line2d(P,Line,RGB_Color_Type(0.0,0.0,0.0),(P%Ymax-P%Ymin)/1000.0)
    Step = (P%Ymax - P%Ymin)/Y_Ticks
    do I = 0,Y_Ticks
        Work = P%Ymin + I*Step
        write(unit=Sh_W,fmt="(es14.6)") Work
        Line = (/ Point2d(X - P%Onemm_X,Work),Point2d(X + P%Onemm_X,Work) /)
        call Line2d(P,Line,RGB_Color_Type(0.0,0.0,0.0),(P%Ymax-P%Ymin)/1000.0)
        call Print(P,Sh_W,Point2d(X + 2.0*P%Onemm_X,Work))
    end do
    Step = (P%Xmax - P%Xmin)/X_Ticks
    do I = 0, X_Ticks
        Work = P%Xmin + I*Step
        write(unit=Sh_W, fmt="(es14.6)") Work
        Line = (/ Point2d(Work,Y - P%Onemm_Y),Point2d(Work,Y + P%Onemm_Y) /)
        call Line2d(P,Line,RGB_Color_Type(0.0,0.0,0.0),(P%Xmax-P%Xmin)/1000.0)
        call Print(P,Sh_W,Point2d(Work,Y + 2.0*P%Onemm_Y))
    end do
end subroutine Axis

subroutine Fill2d(P,Path,Color)
    implicit none
    type(Picture_Type), intent(in) :: P
    type(Point2d), dimension(:), intent(in) :: Path
    type(RGB_Color_Type), intent(in), optional :: Color

    type(RGB_Color_Type) :: Cl
    integer :: i,i1,i2

    if (present(Color)) then
        Cl=Color
    else
        Cl%R = 0
        Cl%G = 0
        Cl%B = 0
    end if
    i1 = lbound(Path,1)
    i2 = ubound(Path,1)
    write(unit=P%Fil,fmt=*) "gsave newpath"
    write(unit=P%Fil,fmt=*) Cl%R, " ", Cl%G, " ", Cl%B, " setrgbcolor"
    write(unit=P%Fil,fmt=*) Abs_x(Path(i1)%X,P), "  ", Abs_Y(Path(i1)%y,P), " moveto"
    do I = i1,i2
        write(unit=P%Fil,fmt=*) Abs_X(Path(I)%X,P), "  ", Abs_Y(Path(I)%y,P),  " lineto"
    end do
    write(unit=P%Fil,fmt=*) " closepath fill grestore"
end subroutine Fill2d

end module zobraz


