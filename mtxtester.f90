!> \file mtxtester.f90
!! \brief testovadlo matic


module mtxtester_module
    public :: matrixtester
contains

    !> \brief  testovadlo matic
    !!
    !! \param a matice, nemusi byt alokovana. jde jen o moznost polymorfie
    !!
    subroutine matrixtester(a)
        use pmatypy
        use mtx
        use datasetup
        use solvers
        implicit none
        class(matrix), intent(in) :: a
        class(matrix), allocatable :: b
        class(matrix), allocatable :: c,c1
        class(matrix), allocatable :: L
        class(matrix), allocatable :: D
        class(matrix), allocatable :: U
        integer(kind=ikind), dimension(:), allocatable :: p1,p2
        integer(kind=ikind) :: i
        print *,"testy matic zacinaji"
        allocate(b,source=a)
        allocate(c,source=a)
        allocate(c1,source=a)
        allocate(l,source=a)
        allocate(d,source=a)
        allocate(u,source=a)
        !konstrukce
        call b%init(5_ikind,5_ikind)
        call b%print
        !manipulace s prvky
        call Hilbert(b,6_ikind)
        allocate(p1(1:6),p2(1:6))
        call b%print(ncol=6_ikind)
        call LDU(b,c)
        print *,"delam vektor"
        do i=1,6
            p1(i)=i
            p2(i)=i
        enddo
        print *,"volam split"
        call Split(c,L,D,U,p1,p2)
        call L%print(ncol=6_ikind,caption="L matice")
        call D%print(ncol=6_ikind,caption="D matice")
        call U%print(ncol=6_ikind,caption="U matice")

        call c1%clone(c)
        call c%mulm(L,D)
        call c1%mulm(c,U)
        call c1%print(ncol=6_ikind,caption="soucim LDU matice")
        call c1%subm(b)
        call c1%print(ncol=6_ikind,caption="rozdil LDU matice a originalu")
        print *, "norma rozdilu=",c1%normF()

    end subroutine matrixtester
end module mtxtester_module
