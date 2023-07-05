        !COMPILER-GENERATED INTERFACE MODULE: Mon Jul  3 01:05:26 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MATRIXINVERSE__genmod
          INTERFACE 
            SUBROUTINE MATRIXINVERSE(INPUT1,NROW,NCOL,OUTPUT1)
              REAL(KIND=4) ,ALLOCATABLE :: INPUT1(:,:)
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              REAL(KIND=4) ,ALLOCATABLE :: OUTPUT1(:,:)
            END SUBROUTINE MATRIXINVERSE
          END INTERFACE 
        END MODULE MATRIXINVERSE__genmod
