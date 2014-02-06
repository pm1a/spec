!> \file mtx.f90
!! \brief Objektova implementace matic
!<


!> Module pro matice
!! ==
!! \page implem Implementace matic
!! Jde o implementaci abstraktniho typu. Zde je jednak nekolik malo 
!! procedur, ktere je nutno pri konstrukci potomka implementovat. Dale 
!! je zde rada nastroju, ktera je pomoci techto prostredku 
!! implementovana, ale lze je v pripade vhodnosti reimplementovat v
!! potomkovi.
!! Zmeny v celkovem interface se nepredpokladaji.
!! Typ ma dve interni polozky - rozmery matice. Jsou zpristupneny 
!! pomoci pristupovych funkci. 
!! - \subpage plna "Plna matice"
!! - \subpage ridka "Ridka matice"
module mtx
    use pmatypy
    implicit none
    private

    !> obecna matice
    type, abstract, public :: matrix
    !> pocet radku
    integer(kind=ikind), private :: n = 0
    !> pocet sloupcu
    integer(kind=ikind), private :: m = 0
contains
    !> vrati pocet radku
    procedure, non_overridable :: getn => getnmatrix
    !> vrati pocet sloupcu
    procedure, non_overridable :: getm => getmmatrix
    !> inicializace
    procedure :: init => initmatrix
    !> vrati prvek matice
    procedure(geti), deferred :: get
    !> nastavi prvek matice
    procedure(seti), pass(a), deferred :: set
    !> pricte cislo k prvku matice
    procedure :: add => addmatrix
    !> vybere radek z matice
    procedure :: getrow => getrowmatrix
    !> vybere sloupec z matice
    procedure :: getcol => getcolmatrix
    !> vytiskne matici
    procedure :: print => printmatrix
    !> dumpne celou strukturu matice
    procedure :: dump => dumpmatrix
    !> da pocet nenul v matici
    procedure :: nonzero => nzmatrix
    !> textova varianta spy
    procedure, non_overridable :: spy => spymatrix
    !> nasobi vektorem
    procedure :: mul => mulmatrix
    !> znasobi dve matice
    procedure :: mulm => mulmatrixmatrix
    !> odecte argument od matice
    procedure :: subm => submatrixmatrix
    !> pricte argument k matici
    procedure :: addm => addmatrixmatrix
    !> udela kopii matice
    procedure :: copy => copymatrix
    !> udela prazdnou kopii matice
    procedure :: clone => clonematrix
    !> jen prenastavi velikost
    procedure :: resize => initmatrix
    !> spocita Frobeniovskou normu matice
    procedure :: normF => normFmatrix
end type matrix

!> interface pro ziskani prvku
abstract interface
    !> funkce pro ziskani prvku
    function geti(a,i,j) result(r)
        use pmatypy
        import :: matrix
        !> matice
        class(matrix), intent(in) :: a
        !> radkovy index
        integer(kind=ikind), intent(in) :: i
        !> sloupcovy index
        integer(kind=ikind), intent(in) :: j
        !> hodnota prvku
        real(kind=rkind) :: r
    end function
end interface

!> interface pro nastaveni prvku
abstract interface
    !> rutina pro nastaveni prvku
    subroutine seti(r,a,i,j)
        use pmatypy
        import :: matrix
        !> matice
        class(matrix), intent(in out) :: a
        !> souradnice
        integer(kind=ikind), intent(in) :: i,j
        !> hodnota
        real(kind=rkind), intent(in) :: r
    end subroutine
end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! veci pro matrix - zcela univerzalni
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> pricte k prvku - kandidat reimplementace v potomkovi
subroutine addmatrix(a,r,i,j)
    use pmatypy
    !> matice
    class(matrix), intent(in out) :: a
    !> souradnice
    integer(kind=ikind), intent(in) :: i,j
    !> hodnota
    real(kind=rkind), intent(in) :: r
    real(kind=rkind) :: v

    v=a%get(i,j)
    v = v + r
    call a%set(v,i,j)
end subroutine addmatrix

!> rutina pro inicializaci
subroutine initmatrix(a,n,m)
    use pmatypy
    implicit none
    !> matice
    class(matrix), intent(in out) :: a
    !> rozmery
    integer(kind=ikind), intent(in) :: n,m
    a%n = n
    a%m = m
    print *,"initmatrix: konci"
end subroutine



!> vrati pocet radku
pure function getnmatrix(a) result(res)
    use pmatypy
    implicit none
    !> matice
    class(matrix), intent(in) :: a
    integer(kind=ikind) :: res
    res = a%n
end function getnmatrix


!> vrati pocet sloupcu
pure function getmmatrix(a) result(res)
    use pmatypy
    implicit none
    !> matice
    class(matrix), intent(in) :: a
    integer(kind=ikind) :: res
    res = a%m
end function getmmatrix

!> ziska radek z matice - univerzal - kandidat reimplementace v potomkovi
!!
!! vrati jen nenulove prvky z radku, v pripade potreby prealokuje prostor
!<
subroutine getrowmatrix(a,i,v,jj,nelem,mask)
    use pmatypy
    implicit none
    !> matice
    class(matrix), intent(in) :: a
    !> pozadovany radek
    integer(kind=ikind) :: i
    !> nenulove hodnoty z radku
    real(kind=rkind), dimension(:), allocatable, intent(inout) :: v
    !> sloupcove indexy nenulovych prvku radku
    integer(kind=ikind), dimension(:), allocatable, intent(inout) :: jj
    !> pocet nenulovych prvku v radku
    integer(kind=ikind), intent(out) :: nelem
    !> seznam povolenych indexu
    logical, dimension(:), intent(in) :: mask

    integer(kind=ikind) :: j
    real(kind=rkind) :: w

    ! dvoufazove
    ! spocitat kolik jich bude
    nelem = 0
    do j = 1,a%m
        if ( a%get(i,j) /= 0 .and. mask(j)) then
            nelem = nelem + 1
        end if
    end do
    ! overit misto
    if ( .not. allocated(v)) then
        allocate(v(1:nelem))
    else
        if (nelem > ubound(v,1)) then
            deallocate(v)
            allocate(v(1:nelem))
        end if
    end if
    if ( .not. allocated(jj)) then
        allocate(jj(1:nelem))
    else
        if (nelem > ubound(jj,1)) then
            deallocate(jj)
            allocate(jj(1:nelem))
        end if
    end if
    ! opravdu naplnit
    nelem = 0
    do j = 1,a%m
        w = a%get(i,j)
        if ( w /= 0 .and. mask(j)) then
            nelem = nelem + 1
            v(nelem)  = w
            jj(nelem) = j
        end if
    end do
end subroutine getrowmatrix

!> ziska sloupec z matice  - kandidat reimplementace v potomkovi
subroutine getcolmatrix(a,j,v,ii,nelem,mask)
    use pmatypy
    implicit none
    !> matice
    class(matrix), intent(in) :: a
    !> identifikace sloupce
    integer(kind=ikind), intent(in) :: j
    !> hodnoty prvku
    real(kind=rkind), dimension(:), allocatable, intent(inout) :: v
    !> radkove indexy
    integer(kind=ikind), dimension(:), allocatable, intent(inout) :: ii
    !> pocet prvku vybraneho sloupce
    integer(kind=ikind), intent(out) :: nelem
    !> seznam povolenych indexu
    logical, dimension(:), intent(in) :: mask
    integer(kind=ikind) :: i
    real(kind=rkind) :: w
    ! dvoufazove
    ! spocitat kolik jich bude
    nelem = 0
    do i = 1,a%n
        if ( a%get(i,j) /= 0 .and. mask(i)) then
            nelem = nelem + 1
        end if
    end do
    ! overit misto
    if ( .not. allocated(v)) then
        allocate(v(1:nelem))
    else
        if (nelem > ubound(v,1)) then
            deallocate(v)
            allocate(v(1:nelem))
        end if
    end if
    if ( .not. allocated(ii)) then
        allocate(ii(1:nelem))
    else
        if (nelem > ubound(ii,1)) then
            deallocate(ii)
            allocate(ii(1:nelem))
        end if
    end if
    ! opravdu naplnit
    nelem = 0
    do i = 1,a%n
        w = a%get(i,j)
        if ( w /= 0 .and. mask(i)) then
            nelem = nelem + 1
            v(nelem)  = w
            ii(nelem) = i
        end if
    end do
end subroutine getcolmatrix


!> napodoba matlabovske spy
subroutine spymatrix(a)
    use pmatypy
    implicit none
    !> matice
    class(matrix), intent(in) :: a
    integer(kind=ikind) :: i,j, nz
    character(len=a%getm()) :: radek
    character(len=8) :: fmts

    nz = 0
    write(fmts,fmt="(a2,i5,a1)") "(a",a%m,")"
    do i=1,a%getn()
        do j=1,a%getm()
            if (a%get(i,j) == 0.0_rkind) then
                radek(j:j) = '.'
            else
                radek(j:j) = 'X'
                nz = nz + 1
            end if
        end do
        print fmts,radek
    end do
    print *, " pocet radek=", A%getn()
    print *, " pocet sloupcu",A%getm()
    print *,"celkovy pocet nenul=",nz
end subroutine spymatrix

!> vytiskne matici
subroutine printmatrix(a,ncol, width, caption)
    use pmatypy
    implicit none
    !> matice
    class(matrix), intent(in) :: a
    !> pozadovany pocet sloupcu tisku, nepovinne
    integer(kind=ikind), intent(in), optional :: ncol
    !> pozadovane sirka sloupce, nepovinne
    integer(kind=ikind), intent(in), optional :: width
    !> nadpis, nepovinne
    character(len=*), intent(in),    optional :: caption
    character(len=100) :: cpt,fmts
    integer(kind=ikind) :: nc
    integer(kind=ikind) :: wd
    integer(kind=ikind) :: i,j, col1,col2
    real(kind=rkind), dimension(:), allocatable :: data

    if ( present(ncol) ) then
        nc = ncol
    else
        nc = 5
    end if

    if ( present(width) ) then
        wd = width
    else
        wd = 15
    end if

    if ( present(caption) ) then
        cpt = adjustl(caption)
    else
        cpt ="matice"
    end if

    allocate(data(1:nc))
    print *, trim(cpt)
    print *, " pocet radku=",a%n, " pocet sloupcu=", a%m
    !write(fmts,fmt="(i3,a2,i3,a2,i3,a2)") ncol,"(es",width,".",width-8,",a)"
    write(unit=fmts,fmt="(A1,I0,A2,I0,A1,I0,A1)")   "(",nc,"Es",wd+8,".",wd,")"
    !print *,fmts
    col1 = 1
    col2 = min(nc,a%getm())
    do
        print *,"tisknu sloupce ",col1," az ",col2
        do i=1,a%getn()
            do j=col1,col2
                data(j-col1+1) = a%get(i,j)
            end do
            print fmts, data(1:col2-col1+1)
        end do
        col1 = col2+1
        col2 = min(col2+nc,a%getm())
        if (col1>a%getm()) exit
    end do

end subroutine printmatrix

!> zakladni dump
subroutine dumpmatrix(a)
    implicit none
    !> dumpovana matice
    class(matrix), intent(in) :: a
    print *, "dump matice pro matrix - v zasade k nicemu"
    print *, "n=",a%n," m=",a%m
end subroutine dumpmatrix

!> vrati pocet nenul v matici - tady n*m
function nzmatrix(a) result(res)
    use pmatypy
    implicit none
    !> matice
    class(matrix), intent(in) :: a
    integer(kind=ikind) :: res
    res = a%n*a%m
end function nzmatrix


!> nasobi matici vektorem !!!!!prepsat
function mulmatrix(a,x, count) result(y)
    !> matice
    class(matrix), intent(in) :: a
    !> vektor
    real(kind=rkind), dimension(:), intent(in) :: x
    !> vysledek
    real(kind=rkind), dimension(:), allocatable :: y
    !> pocet operaci
    type(tcount), intent(inout), optional :: count
    integer(kind=ikind) :: i,j

    allocate(y(1:a%n))
    y = 0
    do i=1,a%n
        do j=1,a%m
            y(i) = y(i) + a%get(i,j)*x(j)
        end do
    end do
    if (present(count)) then
        count%ad  = count%ad  + a%n*a%m
        count%mul = count%mul + a%n*a%m
    end if
end function mulmatrix


!> zkopiruje matici  - kandidat reimplementace v potomkovi
subroutine copymatrix(A,B)
    !> cil
    class(matrix), intent(inout) :: A
    !> zdroj
    class(matrix), intent(in) :: B

    integer(kind=ikind) :: i,j
    real(kind=rkind) :: wrk
    call A%init(B%getn(),B%getm())
    do i=1,B%getn()
        do j=1,B%getm()
            wrk = B%get(i,j)
            call A%set(wrk,i,j)
        end do
    end do
end subroutine copymatrix

!> vytvori prazdnou kopii
subroutine clonematrix(A,B)
    !> cil
    class(matrix), intent(inout) :: A
    !> zdroj
    class(matrix), intent(in) :: B

    integer(kind=ikind) :: i,j
    real(kind=rkind) :: wrk
    call A%init(B%getn(),B%getm())
end subroutine clonematrix

!> \brief  A = B*C  !!!!!prepsat
!!
!! \param A vysledna matice
!! \param B levy operand
!! \param C pravy operand
!!
subroutine mulmatrixmatrix(A,B,C,info)
    use pmatypy
    implicit none
    class(matrix), intent(in out) :: A
    class(matrix), intent(in) :: B
    class(matrix), intent(in) :: C
    type(tcount),intent(in out), optional :: info

    integer(kind=ikind) :: n,m,p,i,j,k
    real(kind=rkind) :: wrk,w1,w2

    n = B%getn()
    m = C%getm()
    p = B%getm()
    i = C%getn()
    j = A%getn()
    k = A%getm()
    if ((p/=i) .or. (j/=n) .or. (m/=k)) print *,"nesedi rozmery"
    do i=1,n
        do j=1,m
            call A%set(0.0_rkind,i,j)
            wrk = 0
            do k=1,p
                w1 = B%get(i,k)
                w2 = C%get(k,j)
                wrk = wrk + w1*w2
            end do
            call A%set(wrk,i,j)
        end do
    end do
end subroutine mulmatrixmatrix

subroutine addmatrixmatrix(A,B)
    use pmatypy
    implicit none
    class(matrix), intent(in out) :: A
    class(matrix), intent(in)      :: B
    integer(kind=ikind)            :: i,j,n,m
    real(kind=rkind)               :: w1,w2
    real(kind=rkind), dimension(:), allocatable :: rb
    integer(kind=ikind), dimension(:), allocatable :: rib
    integer(kind=ikind) :: nrb
    logical, dimension(:), allocatable :: mask

    n = A%getn()
    m = A%getm()
    allocate(mask(1:m))
    mask = .true.
    do i=1,n
        call B%getrow(i,rb,rib,nrb,mask)
        do j=1,nrb
            call A%add(rb(j),i,rib(j))
        end do
    end do

end subroutine addmatrixmatrix

subroutine submatrixmatrix(A,B)
    use pmatypy
    implicit none
    class(matrix), intent(in out) :: A
    class(matrix), intent(in)      :: B
    integer(kind=ikind)            :: i,j,n,m
    real(kind=rkind)               :: w1,w2
    real(kind=rkind), dimension(:), allocatable :: rb
    integer(kind=ikind), dimension(:), allocatable :: rib
    integer(kind=ikind) :: nrb
    logical, dimension(:), allocatable :: mask

    n = A%getn()
    m = A%getm()
    allocate(mask(1:m))
    mask = .true.
    do i=1,n
        call B%getrow(i,rb,rib,nrb,mask)
        do j=1,nrb
            call A%add(-rb(j),i,rib(j))
        end do
    end do
end subroutine submatrixmatrix

!> \brief  spocte Frobeniovu normu matice
!!
!! \param A zkoumana matice
!! \return  hledana norma
!!
function normFmatrix(A) result(y)
    use pmatypy
    implicit none
    class(matrix), intent(in) :: A
    real(kind=rkind) :: y
    real(kind=rkind), dimension(:), allocatable :: rb
    integer(kind=ikind), dimension(:), allocatable :: rib
    integer(kind=ikind) :: nrb,i,j
    logical, dimension(:), allocatable :: mask

    allocate(mask(1:A%getm()))
    mask = .true.
    y = 0
    do i=1,A%getn()
        call A%getrow(i,rb,rib,nrb,mask)
        do j=1,nrb
            y = y + rb(j)*rb(j)
        end do
    end do
    y = sqrt(y)
end function normFmatrix

end module mtx
