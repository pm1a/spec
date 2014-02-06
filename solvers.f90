!> \file solvers.f90
!! \brief ruzne resice soustav linearnich rovnic
!!
!<

!> resice soustav
module solvers
    implicit none
    public :: jacobi
    public :: LDU
    public :: LDUd
    public :: Split
contains
    !> Jacobiova metoda
    !!
    !! Jde o iteracni postup
    !!  \f$  x_{i+1} = x_i + M^{-1}*(b-A*x_i) \f$
    !! kde \f$ M \f$ je diagonala matice \f$ A \f$ . \n
    !! Oznacime-li residuum jako \f$ r_i = b-A*x_i \f$, pak \f$
    !! reps = || r_i || / || r_0 || \f$ .
    !<
    subroutine jacobi(a,b,x,maxit,reps, it, repsfin, l1, l2, countop, ilev)
        use pmatypy
        use mtx
        implicit none
        !> matice soustavy
        class(matrix), intent(in) :: a
        !> prava strana
        real(kind=rkind), dimension(:), intent(in) :: b
        !> pocitane reseni soustavy
        real(kind=rkind), dimension(:), intent(inout) :: x
        !> maximalni povoleny pocet iteraci
        integer(kind=ikind), intent(in) :: maxit
        !> pozadovane relativni residuum
        real(kind=rkind), intent(in) :: reps
        !> skutecny pocet iteraci
        integer(kind=ikind), intent(out) :: it
        !> skutecne relativni residuum
        real(kind=rkind), intent(out) :: repsfin
        !> odhad rychlosti konvergence pomoci mocninne metody
        real(kind=rkind), intent(out) :: l1
        !> odhad rychlosti konvergence pomoci odmocniny
        real(kind=rkind), intent(out) :: l2
        !> pocitadlo operaci
        type(tcount), intent(inout), optional :: countop
        !> podrobnost informaci
        integer, intent(in), optional :: ilev

        real(kind=rkind) :: r0,r1,r2
        real(kind=rkind), dimension(:), allocatable :: r,diag
        integer(kind=ikind) :: i,j,k
        type(tcount) :: countop1
        real(kind=rkind) :: t1, t2

        call cpu_time(t1)
        allocate(r(1:a%getn()))
        allocate(diag(1:a%getn()))
        r=b-a%mul(x,countop1)
        r0 = norm2(r)
        countop1%ad = countop1%ad + a%getn()
        do i=1,a%getn()
            diag(i) = a%get(i,i)
        end do
        it = 0
        do
            if ( it > maxit ) exit
            r1 = norm2(r)
            repsfin = r1/r0
            if (r1 /= 0) then
                l1 = r1/r2
                l2 = repsfin**(1.0_rkind/it)
            end if
            if ( ilev > 0 )  print"(i16,4ES38.30)", it, r0,r1,l1,l2
            if ( repsfin < reps) exit
            do i=1, a%getn()
                x(i) = x(i) + r(i)/diag(i)
            end do
            r=b-a%mul(x,countop1)
            countop1%ad  = countop1%ad  + 2*a%getn()
            countop1%div = countop1%div +   a%getn()
            it = it + 1
            r2 = r1
        end do
        deallocate(r,diag)
        call cpu_time(t2)
        if (present(countop)) then
            countop%ad  = countop%ad  + countop1%ad
            countop%mul = countop%mul + countop1%mul
            countop%div = countop%div + countop1%div
            countop%time = t2-t1
        end if
    end subroutine jacobi

    !> nedestruktivni LDU rozklad
    !!
    !! pro volbu pivota plati nasledujici pravidla\n
    !! 0 ... zadna\n
    !! 1 ... uplny vyber hlavniho prvku (nutne obe permutace )\n
    !! 2 ... sloupcovy vyber hlavniho prvku(nutno perm1)\n
    !! 3 ... radkovy vyber hlavniho prvku(nutno perm2)\n
    !! 4 ... diagonalni vyber hlavniho prvku(nutno aspon jedna = obe shodne)\n
    !! 5 ... diagonalni vyber hlavniho prvku pro minimal degree\n
    !!              (nutno aspon jedna = obe shodne) \n
    !! 6 ... rid se predepsanou permutaci\n
    !! Pro situace 1 az 6 musi byt pritomny permutacni vektory\n
    !! Pokud nejsou pozadavky splneny - zvoli se nejpodobnejsi volba\n
    !!
    !! pro podrobnost informaci plati\n
    !! 0 ... tichy chod\n
    !! 1 ... super podrobne\n
    !<
    subroutine LDU(A,LU,info,pivtype,ilev,perm1,perm2)
        use mtx
        use pmatypy
        implicit none
        !> rozkladana matice
        class(matrix), intent(in) :: A
        !> rozlozena matice
        class(matrix), intent(inout) :: LU
        !> pocty operaci
        type(tcount), intent(inout), optional :: info
        !> typ pivotaze
        integer, intent(in), optional :: pivtype
        !> podrobnost informaci
        integer, intent(in), optional :: ilev
        !> permutace pro i
        integer(kind=ikind), dimension(:), allocatable, &
            intent(inout), optional :: perm1
        !> permutace pro j
        integer(kind=ikind), dimension(:), allocatable,&
            intent(inout), optional :: perm2

        integer :: pt1,il1
        integer(kind=ikind),dimension(:),allocatable :: p1,p2
        type(tcount) :: inf1

        if (present(pivtype)) then
            pt1 = pivtype
        else
            pt1 = 0
        end if
        if (present(ilev)) then
            il1 = ilev
        else
            il1 = 0
        end if
        allocate(p1(1:A%getn()))
        allocate(p2(1:A%getm()))
        select case(pt1)
            case (0)
                print *, "nula"
            case (1)
            case (2)
            case (3)
            case (4)
            case (5)
            case (6)
                if (present(perm1)) then
                else
                end if
            case default
                print *,"chybna volba"
        end select

        call LU%copy(A)
        call LDUd(LU,inf1,pt1,il1,p1,p2)
        print *,"LDU konci"
    end subroutine LDU

    !> destruktivni LDU rozklad
    !!
    !! nahradi vstupni matici jejim LDU rozkladem\n
    !! A = L*D*U\n
    !! L ... dolni trojuhelnikova matice s jednickami na diagonale\n
    !! D ... diagonalni matice\n
    !! U ... horni trojuhelnikova matice s jednickami na diagonale\n
    !!
    !! pro volbu pivota plati nasledujici pravidla\n
    !! 0 ... zadna\n
    !! 1 ... uplny vyber hlavniho prvku (nutne obe permutace )\n
    !! 2 ... sloupcovy vyber hlavniho prvku(nutno perm1)\n
    !! 3 ... radkovy vyber hlavniho prvku(nutno perm2)\n
    !! 4 ... diagonalni vyber hlavniho prvku(nutno aspon jedna = obe shodne)\n
    !! 5 ... diagonalni vyber hlavniho prvku pro minimal degree\n
    !!              (nutno aspon jedna = obe shodne) \n
    !! 6 ... rid se predepsanou permutaci\n
    !! Pro situace 1 az 6 musi byt pritomny permutacni vektory\n
    !! Pokud nejsou pozadavky splneny - zvoli se nejpodobnejsi volba\n
    !!
    !! pro podrobnost informaci plati\n
    !! 0 ... tichy chod\n
    !! 1 ... super podrobne\n
    !<
    subroutine LDUd(A,info,pivtype,ilev,perm1,perm2)
        use mtx
        use pmatypy
        implicit none
        !> rozkladana matice, pote s rozkladem
        class(matrix), intent(inout) :: A
        !> pocty operaci
        type(tcount), intent(inout), optional :: info
        !> typ pivotaze
        integer, intent(in), optional :: pivtype
        !> podrobnost informaci
        integer, intent(in), optional :: ilev
        !> permutace pro i
        integer(kind=ikind), dimension(:), allocatable, &
            intent(inout), optional :: perm1
        !> permutace pro j
        integer(kind=ikind), dimension(:), allocatable,&
            intent(inout), optional :: perm2


        real(kind=rkind) :: t1,t2 !meze pro cas
        integer :: pt1
        integer :: il1
        type(tcount) :: inf1
        integer(kind=ikind) :: i,q,j,k
        integer(kind=ikind) :: piv1, piv2 ! souradnice pivota
        integer(kind=ikind), dimension(:), allocatable :: p1
        integer(kind=ikind), dimension(:), allocatable :: p2
        integer(kind=ikind),dimension(:), allocatable :: ri1
        integer(kind=ikind),dimension(:), allocatable :: ri2
        real(kind=rkind),dimension(:), allocatable :: r1
        real(kind=rkind),dimension(:), allocatable :: r2
        integer(kind=ikind) :: sr1
        integer(kind=ikind) :: sr2
        real(kind=rkind) :: wrk
        logical, dimension(:), allocatable :: mp1
        logical, dimension(:), allocatable :: mp2


        call cpu_time(t1)
        allocate(p1(1:A%getn()))
        allocate(p2(1:A%getm()))
        allocate(mp1(1:A%getn()))
        allocate(mp2(1:A%getm()))
        do i=1,A%getn()
            p1(i)=i
        end do
        do i=1,A%getm()
            p2(i)=i
        end do
        mp1 = .true.
        mp2 = .true.
        if ( present(pivtype)) then
            pt1 = pivtype
        else
            pt1 = 0
        end if
        if (present(ilev)) then
            il1 = ilev
        else
            il1 = 0
        end if
        select case(pt1)
            case(0)
                ! nic resit nemusim
                if (il1>0) print *,"bez vyberu hlavniho prvku"
            case (1)
                if (present(perm1)) then
                    if (present(perm2)) then
                        ! tak tohle je ok
                        if (il1>0) print *,"s uplnym vyberem hlavniho prvku"
                    else
                        ! mam k dispozici je radkove permutace
                        ! menim na sloupcovy vyber
                        pt1 = 2
                        if (il1>0) print *,&
                            "menim uplnym vyber hlavniho prvku na sloupcovy"
                    end if
                else
                    if (present(perm2)) then
                        ! mam jen sloupcove permuatace
                        ! menim na radkovy vyber
                        pt1 = 3
                        if (il1>0) print *,&
                            "menim uplnym vyber hlavniho prvku na radkovy"
                    else
                        ! nemam zadne permutace
                        ! menim na zadny vyber
                        pt1 = 0
                        if (il1>0) print *,&
                            "menim uplnym vyber hlavniho prvku na bez vyberu"
                    end if
                end if
            case (2)
                if (present(perm1)) then
                    ! OK
                else
                    ! menim na bez vyberu
                    pt1 = 0
                end if
            case (3)
                if (present(perm2)) then
                    ! OK
                else
                    ! menim na bez vyberu
                    pt1 = 0
                end if
            case (4)
                if (present(perm1) .or. present(perm2)) then
                    ! OK
                else
                    ! menim na bez vyberu
                    pt1 = 0
                end if
            case (5)
                if (present(perm1) .or. present(perm2)) then
                    ! OK
                else
                    ! menim na bez vyberu
                    pt1 = 0
                end if
            case (6)
                if (present(perm1)) p1=perm1
                if (present(perm2)) p2=perm2
            case default
                print *, "chybna volba pivota, menim na zadna pivotaz"
                pt1 = 0
        end select
        i = A%getn()
        q = A%getm()
        if (i < q) q = i
        do i=1,q-1
            ! napred najdi pivota
            print *,"hledam pivoata"
            call findpivot
            print *, "mam pivota"
            mp1(piv1) = .false.
            mp2(piv2) = .false.
            call a%getrow(piv1,r1,ri1,sr1,mp1)
            call a%getcol(piv2,r2,ri2,sr2,mp2)
            wrk = a%get(piv1,piv2)
            print *, "mam radky"
            !call a%spy
            !call pockej("mam pivota")
            do j=1,sr1
                r1(j) = r1(j)/wrk
            end do
            inf1%div = inf1%div+sr1-1
            print *,"jdu eliminovat"
            do j=1,sr1
                do k=1,sr2
                    print *,j,k,sr1,sr2
                    wrk = a%get(ri1(j),ri2(k))
                    wrk = wrk - r1(j)*r2(k)
                    call a%set(wrk,ri1(j),ri2(k))
                end do
            end do
            print *, "krok hotov"
            wrk = a%get(piv1,piv2)
            do j=1,sr1
                call a%set(r1(j),piv1,ri1(j))
            end do
            do j=1,sr2
                call a%set(r2(j)/wrk,ri2(j),piv2)
            end do
            print *," radek a sloupec ulozeny"
            inf1%ad  = inf1%ad  + sr1*sr2
            inf1%mul = inf1%mul + sr1*sr2
            inf1%div = inf1%div+sr2
        end do
        call cpu_time(t2)
        if (present(info)) then
            info%ad   = info%ad  + inf1%ad
            info%mul  = info%mul + inf1%mul
            info%div  = info%div + inf1%div
            info%time = t2 - t1
        end if
        print *,"LDUd konci"
    contains
        !> vybere pivota a zaznemena to v permutacich
        !!
        !! orientuje se podle promenne pt1 - ta se nastavuje podle pivtype
        !<
        subroutine findpivot
            implicit none
            integer(kind=ikind) :: wrk, i1, j1, mi, mj
            real(kind=rkind) :: w1, mx
            select case(pt1)
                case (0)
                    piv1 = i
                    piv2 = i
                case (1)
                    print *, "piv1 i=",i
                    mx = abs(a%get(p1(i),p2(i)))
                    mi = i
                    mj = i
                    do i1 = i, a%getn()
                        do j1 = i, a%getm()
                            !print *,i1,j1
                            w1 = abs(a%get(p1(i1),p2(j1)))
                            if (w1>mx) then
                                mx = w1
                                mi = i1
                                mj = j1
                            end if
                        end do
                    end do
                    piv1   = p1(mi)
                    p1(mi) = p1(i)
                    p1(i)  = piv1
                    piv2   = p2(mj)
                    p2(mj) = p2(i)
                    p2(i)  = piv2
                case (2)
                    continue
                case (3)
                    continue
                case (4)
                    continue
                case (5)
                    continue
                case (6)
                    piv1 = p1(i)
                    piv2 = p2(i)
                case default
                    stop "chybna volba pivotaze - tady bych nemel nikdy byt"
            end select
            wrk = p1(i); p1(i) = p1(piv1); p1(piv1) = wrk
            wrk = p2(i); p2(i) = p2(piv2); p2(piv2) = wrk
        end subroutine findpivot
    end subroutine LDUd

    !> rozdeli matici s LDU rozkladem na jednotlive komponenty
    !!
    !! pokud jsou dodany permutace, tak plati
    !<
    subroutine Split(LDU,L,D,U,p1,p2)
        use pmatypy
        use mtx
        !> LDU dekompozice
        class(matrix), intent(in) :: LDU
        !> L cast rozkladu
        class(matrix), intent(out) :: L
        !> D cast rozkladu
        class(matrix), intent(out) :: D
        !> U cast rozkladu
        class(matrix), intent(out) :: U
        !> Leva permutace
        integer(Kind=ikind), dimension(:), intent(in) :: p1
        !> Prava permutace
        integer(Kind=ikind), dimension(:), intent(in) :: p2
        integer(kind=ikind) :: n,m,i,j

        !print *,"split zacina"
        call L%clone(LDU)
        call D%clone(LDU)
        call U%clone(LDU)
        !print *,"clonovani hotov0"
        n=LDU%getn()
        m=LDU%getm()
        do i=1,min(n,m)
            call L%set(1.0_rkind,i,i)
            call U%set(1.0_rkind,i,i)
            call D%set(LDU%get(i,i),i,i)
            do j=i+1,m
                call U%set(LDU%get(i,j),i,j)
            end do
            do j=i+1,n
                call L%set(LDU%get(j,i),j,i)
            end do
        end do
    end subroutine Split


end module solvers
