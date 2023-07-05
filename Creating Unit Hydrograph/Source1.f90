
program Unit_Hydrograph
    implicit none
    integer :: n,m,o,i,j
    real, dimension (:,:), allocatable :: R, Q, P, TP, PR, INV, ID, H
    !ID = Intermediate matrix
    interface
        subroutine Matrix_Trans(In1, In2,nrow,ncol)
        implicit none
        integer :: nrow, ncol, i, j
        real, dimension (:,:), allocatable :: In1, In2
        end subroutine Matrix_Trans      
    
        subroutine Matrix_Multi(In1,In2,Output, Row1, Col, Col2)
        implicit none
        integer :: Row1, Col, Col2
        real, dimension (:,:), allocatable :: In1, In2, Output
        end subroutine Matrix_Multi
    
        subroutine MatrixInverse(Input1, nrow, ncol, Output1)
        implicit none    
        integer :: nrow, ncol
            real, dimension(:,:), allocatable :: Input1, Output1
        end subroutine MatrixInverse    
    end interface

    open(1, file = "E:\Fortran\Exercies\Creating Unit Hydrograph\EFF P.txt")
    open(2,file = "E:\Fortran\Exercies\Creating Unit Hydrograph\DRH.txt")
    open(3,file = "E:\Fortran\Exercies\Creating Unit Hydrograph\Test.txt")
    print *, "Enter Number of DRH ordinates"
    read *, n
    print *, "Enter number of Precipitation data"
    read*,m
    o = n-m+1
    print *,o
    allocate (R(m,1),Q(n,1),P(n,o))
    do i = 1,m
        read(1,*) R(i,1)
    end do
    do i = 1,n
        read(2,*) Q(i,1)
    end do
!writing the P Matrix
    do i = 1,n
        do j = 1,o
            if (i-j+1 .le. 0) then
                P(i,j) = 0
            elseif (i-j+1 .gt. 6) then
                P(i,j) = 0
            else
                P(i,j) = R(i-j+1,1)
            end if 
        end do 
    end do
!Transverse of Matrix P
    call Matrix_Trans(P,TP,n,o)
    call Matrix_Multi(TP, P,PR, o,n,o)
    call MatrixInverse(PR,o,o,INV)
    call Matrix_Multi(TP,Q,ID,o,n,1)
    call Matrix_Multi(INV,ID,H,o,o,1)
    
    do i = 1,o
        write (3,'(f8.3)',advance = 'no'), H(i,1)        
        write (3,*)
    end do 
    

    
    end program Unit_Hydrograph
    
subroutine Matrix_Trans(In1, In2,nrow,ncol)
    implicit none
    integer :: nrow, ncol, i, j
    real, dimension (:,:), allocatable :: In1, In2
    allocate (In2(ncol,nrow))

    do j = 1, ncol
        do i = 1,nrow
            In2(j,i) = In1(i,j)
        end do
    end do
    end subroutine Matrix_Trans

subroutine Matrix_Multi(In1,In2,Output, Row1, Col, Col2)
    implicit none
    integer :: Row1, Col, Col2, i, j, k
    real, dimension (:,:), allocatable :: In1, In2, Output
    allocate (Output(Row1,Col2))

    do i = 1,Row1
        do j = 1,Col2
            Output(i,j) = 0.0
            do k = 1,Col
                Output(i,j) = Output(i,j) + In1(i,k)*In2(k,j)
            end do
        end do
    end do
    end subroutine Matrix_Multi
    
    
    
subroutine MatrixInverse(Input1, nrow, ncol, Output1)
    implicit none
    integer :: nrow, ncol
    real, dimension(:,:), allocatable :: Input1, Output1
    integer :: i, j, k
    real :: X11, Xik
    allocate(Output1(nrow, ncol))
    
    !writing an Identity matrix
    do i = 1, nrow
        do j = 1, ncol
            if (i == j) then
                Output1(i,j) = 1
            else
                Output1(i,j) = 0
            end if
        end do
    end do
    
    do k = 1, nrow
        X11 = Input1(k,k)
        do j = 1, ncol
            Input1(k,j) = Input1(k,j)/X11
            Output1(k,j) = Output1(k,j)/X11
        end do
          
        do i = 1, nrow
            if (i /= k) then
                Xik = Input1(i,k)
                do j = 1, ncol
                  Input1(i,j) = Input1(i,j) - Xik * Input1(k,j)
                  Output1(i,j) = Output1(i,j) - Xik * Output1(k,j)
                end do
            end if
        end do
    end do
    end subroutine MatrixInverse 
    

    

    
    