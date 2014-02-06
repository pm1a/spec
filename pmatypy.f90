!> \file pmatypy.f90
!! \brief modul pro zakladni typy
!<

!> zakladni typy
!!
!! jsou zde definovany kind konstanty pro cisla a struktura pro
!! pocitani operaci
!<
module pmatypy
    !> kind pro realna cisla 18 cifer
    integer, parameter, public :: rkind = selected_real_kind(18,99)
    !> kind pro cela cisla 10 cifer
    integer, parameter, public :: ikind = selected_int_kind(10)
    !> velmi dlouha cela cisla
    integer, parameter, public :: likind = selected_int_kind(16)

    !> pocitadlo operaci
    type, public :: tcount
        !> pocet aditivnich operaci
        integer(kind=likind) :: ad = 0
        !> pocet nasobeni
        integer(kind=likind) :: mul = 0
        !> pocet deleni
        integer(kind=likind) :: div = 0
        !> doba behu
        real(kind=rkind) :: time = 0
    end type tcount

    public :: print_info
    public :: update_info
contains
    !> vytiskne udaje o pocitani
    subroutine print_info(info)
        implicit none
        !> data o spotrebe prace
        type(tcount), intent(in) :: info
        print *, "pocty operaci aditivni:",info%ad," nasobeni:",info%mul,&
            " deleni:",info%div, " cas:",info%time
    end subroutine print_info

    !> pricte info2 k info1
    subroutine update_info(info1,info2)
        implicit none
        type(tcount), intent(inout) :: info1
        type(tcount), intent(in) :: info2

        info1%ad = info1%ad + info2%ad
        info1%mul = info1%mul + info2%mul
        info1%div = info1%div + info2%div
        info1%time = info1%time + info2%time

    end subroutine update_info
end module pmatypy
