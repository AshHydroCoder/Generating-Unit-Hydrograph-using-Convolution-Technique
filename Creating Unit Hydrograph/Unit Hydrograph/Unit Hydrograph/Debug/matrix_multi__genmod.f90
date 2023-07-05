        !COMPILER-GENERATED INTERFACE MODULE: Mon Jul  3 01:05:26 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MATRIX_MULTI__genmod
          INTERFACE 
            SUBROUTINE MATRIX_MULTI(IN1,IN2,OUTPUT,ROW1,COL,COL2)
              REAL(KIND=4) ,ALLOCATABLE :: IN1(:,:)
              REAL(KIND=4) ,ALLOCATABLE :: IN2(:,:)
              REAL(KIND=4) ,ALLOCATABLE :: OUTPUT(:,:)
              INTEGER(KIND=4) :: ROW1
              INTEGER(KIND=4) :: COL
              INTEGER(KIND=4) :: COL2
            END SUBROUTINE MATRIX_MULTI
          END INTERFACE 
        END MODULE MATRIX_MULTI__genmod
