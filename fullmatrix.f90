!> \file fullmatrix.f90
!! \brief implementace plnych matic


!> \page plna 
!! Implementace plnych matic
!! --
!! Matice je ulozena jako dvourozmerne pole realnych cisel. 
!! Implementovany jsou jen nezbytne procedury, ostatni zdedeno.
!<
module fullmatrix
    use pmatypy
    use mtx
    !> plna matice
    type, public, extends(matrix) :: fullmtx
        !> hodnoty prvku
        real(kind=rkind), dimension(:,:), pointer, private :: val => null()
    contains
        procedure :: init => initfull
        procedure :: get => getfull
        procedure, pass(a) :: set => setfull
        !   final :: plnakonec
    end type fullmtx
contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! veci pro plnou matici
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> ziska prvek z plne matice
    function getfull(a,i,j) result (r)
        !> matice
        class(fullmtx), intent(in) :: a
        !> souradnice
        integer(kind=ikind), intent(in) :: i,j
        real(kind=rkind) :: r
        if ((i<1) .or. (j<1) .or. (i>a%getn()) .or. (j>a%getm())) &
            stop "chybne indexy v get plna matice"
        r = a%val(i,j)
    end function

    !> vlozi prvek do matice
    subroutine setfull(r,a,i,j)
        implicit none
        !> hodnota
        real(kind=rkind), intent(in) :: r
        !> matice
        class(fullmtx), intent(in out)  :: a
        !> souradnice
        integer(kind=ikind), intent(in) :: i,j
        if ((i<1) .or. (j<1) .or. (i>a%getn()) .or. (j>a%getm())) &
            stop "chybne indexy v set plna matice"
        a%val(i,j) = r
    end subroutine setfull

    !> inicializace plne matice
    subroutine initfull(a,n,m)
        use pmatypy
        implicit none
        !> matice
        class(fullmtx), intent(in out)  :: a
        !> rozmery matice
        integer(kind=ikind), intent(in) :: n,m
        call a%resize(n,m)
        print *,"po resize"
        if (associated(a%val)) deallocate(a%val)
        allocate(a%val(n,m))
        print *,"initfull: menim velikost"
        a%val = 0
    end subroutine initfull

    ! tohle je pripraveny, az bude fungovat destruktor
    !subroutine  plnakonec(a)
    !    implicit none
    !    type(fullmtx) :: a
    !
    !    print *, " destruktor pro plnou matici"
    !end subroutine plnakonec


end module fullmatrix
