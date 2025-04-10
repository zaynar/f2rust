C     See GrammarPatcher::patch_swap

      SUBROUTINE SWAPC_ARRAY ()

      IMPLICIT NONE

      CHARACTER*(*) ARRAY (*)
      CHARACTER*1 TEMP
      INTEGER A
      INTEGER B
      INTEGER I, M

      RETURN

      ENTRY SWAPC_ARRAY_1 (ARRAY, A, B)

      M = MIN(LEN(ARRAY(A)), LEN(ARRAY(B)))

      DO I = 1, M
            TEMP = ARRAY(A)(I:I)
            ARRAY(A)(I:I) = ARRAY(B)(I:I)
            ARRAY(B)(I:I) = TEMP
      END DO

      IF (LEN(ARRAY(A)) > M) THEN
            ARRAY(A)(M+1:) = ' '
      END IF

      IF (LEN(ARRAY(B)) > M) THEN
            ARRAY(B)(M+1:) = ' '
      END IF

      RETURN

      END
