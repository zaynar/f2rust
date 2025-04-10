C     See GrammarPatcher::patch_swap

      SUBROUTINE SWAPC_ARRAY (A, B, ARRAY)

      IMPLICIT NONE

      CHARACTER*(*) ARRAY (*)
      CHARACTER*1 TEMP
      INTEGER A, B
      INTEGER I, M

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
