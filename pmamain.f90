!> \file pmamain.f90
!! \brief hlavni program
!<

!> hlavni program
program pokus
    use typy
    use mtx
    use fullmatrix
    use mtxtester
    use sparsematrix
    use datasetup
    use solvers
    use pmatools
    use mtxiotools
    use iso_fortran_env
    !$ use omp_lib
    implicit none

    type(fullmtx) :: af
    type(smtx)    :: as
    type(smtx)    :: aw
    real(kind=rkind), dimension(:), allocatable :: b,x,xright
    real(kind=rkind) :: reps, repsfin, l1,l2
    integer(kind=ikind) :: itmax,it
    integer(kind=ikind),dimension(:),allocatable :: perm1, perm2
    type(tcount) :: fc,sc
    real(kind=rkind), dimension(1:20) :: xv

    print *," prekladac:          ",compiler_version()
    print *," parametry prekladu: ",compiler_options()
    call pockej("Zaciname.")
    call matrixtester(af)
    call pockej("plna - hotovo")
    call matrixtester(as)
    call pockej("ridka - hotovo")
    call read_matrix(aw)
    call pockej()
    call test1
    call Laplace2D(af,10_ikind,10_ikind)
    call Laplace2D(as,10_ikind,10_ikind)

    print *, "matice pocet radku:",af%getn(), " sloupcu:",af%getm()
    call af%print()
    call pockej()
    call af%print(caption="plna maticka")
    call pockej()
    call as%print()
    call pockej()
    call af%spy()
    call pockej()

    print *, "chvile hry s radky matice"
    do
        print *,"vyber radek"
        read(*,*) it
        if ((0<it) .and. (it <= af%getn())) then
            block
                integer(kind=ikind) :: i, nelem
                integer(kind=ikind), dimension(:), allocatable :: ind
                real(kind=rkind), dimension(:), allocatable :: val
                logical, dimension(1:af%getm()) :: msk
                msk = .true.
                print *, "plna matice"
                call af%getrow(it,val,ind,nelem,msk)
                print *,"v radku ",it," je ",nelem," nenulovych prvku"
                do i=1,nelem
                    print *,"prvek cislo ",i," je ",val(i), " s indexem ",ind(i)
                end do
                print *, "ridka matice"
                call as%getrow(it,val,ind,nelem,msk)
                print *,"v radku ",it," je ",nelem," nenulovych prvku"
                do i=1,nelem
                    print *,"prvek cislo ",i," je ",val(i), " s indexem ",ind(i)
                end do

            end block
        else
            exit
        end if
    end do

    print *, "chvile hry se sloupci matice"
    do
        print *,"vyber sloupec"
        read(*,*) it
        if ((0<it) .and. (it <= af%getm())) then
            block
                integer(kind=ikind) :: i, nelem
                integer(kind=ikind), dimension(:), allocatable :: ind
                real(kind=rkind), dimension(:), allocatable :: val
                logical, dimension(1:af%getn()) :: msk
                msk = .true.
                print *, "plna matice"
                call af%getcol(it,val,ind,nelem,msk)
                print *,"ve sloupci ",it," je ",nelem," nenulovych prvku"
                do i=1,nelem
                    print *,"prvek cislo ",i," je ",val(i), " s indexem ",ind(i)
                end do
                print *, "ridka matice"
                call as%getcol(it,val,ind,nelem,msk)
                print *,"ve sloupci ",it," je ",nelem," nenulovych prvku"
                do i=1,nelem
                    print *,"prvek cislo ",i," je ",val(i), " s indexem ",ind(i)
                end do

            end block
        else
            exit
        end if
    end do

    call as%spy()
    allocate(b(1:af%getn()))
    allocate(x(1:af%getm()))
    allocate(xright(1:af%getm()))
    xright = 1
    b = af%mul(xright)
    x = 0
    reps=1.0e-18_rkind
    itmax=100000
    call jacobi(af,b,x,itmax,reps,it,repsfin, l1, l2,fc,0)
    print *, itmax, reps, it, repsfin, l1, l2, fc
    x = 0
    call pockej()
    call jacobi(as,b,x,itmax,reps,it,repsfin, l1, l2,sc,0)
    print *, itmax, reps, it, repsfin, l1, l2, sc

    print *," pocet operaci pro plnou matici ", fc
    print *," pocet operaci pro ridkou matici", sc
    call print_info(fc)
    call print_info(sc)

    call as%init(5_ikind,6_ikind)
    call as%dump()
    call as%set(2.3_rkind,1_ikind,3_ikind)
    call as%dump()
    call as%set(-3.3_rkind,3_ikind,3_ikind)
    call as%dump()
    call as%set(4.3_rkind,1_ikind,1_ikind)
    call as%dump()
    call as%set(0.0_rkind,1_ikind,3_ikind)
    call as%dump()
    call as%set(-3.7_rkind,2_ikind,3_ikind)
    call as%dump()
    call as%set(4.3_rkind,3_ikind,5_ikind)
    call as%dump()
    call as%set(2.3_rkind,1_ikind,3_ikind)
    call as%dump()
    call as%print()

    call pockej("male cekani")
    call print_info(sc)
    call Laplace2D(as,10_ikind,10_ikind)
    sc = tcount(0,0,0,0.0)
    call as%spy()
    call pockej("jdu eliminovat 0")
    call LDUd(as,sc,pivtype=0)
    call as%spy()
    call pockej("po eliminaci")
    call as%print()
    call print_info(sc)
    call pockej("jdu eliminovat 1")
    allocate(perm1(1:as%getn()))
    allocate(perm2(1:as%getm()))
    call Laplace2D(as,10_ikind,10_ikind)
    sc = tcount(0,0,0,0.0)
    call LDUd(as,sc,pivtype=1,perm1=perm1,perm2=perm2,ilev=1)
    call as%spy()
    call pockej("po eliminaci")
    call as%print()
    call print_info(sc)

    call read_vector(xv)

contains
    subroutine test1
        call af%init(30_ikind,30_ikind)
        call as%init(5_ikind,5_ikind)
        call as%dump()
        call pockej()
        print *, af%get(3_ikind,6_ikind)
        print *, as%get(3_ikind,6_ikind)
        call pockej()
    end subroutine test1
end program pokus
