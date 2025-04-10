C     See GrammarPatcher::patch_swap

      SUBROUTINE SWAPD_ARRAY (A, B, ARRAY)

      IMPLICIT NONE

      DOUBLE PRECISION ARRAY (*)
      DOUBLE PRECISION TEMP
      INTEGER A, B

      TEMP = ARRAY(A)
      ARRAY(A) = ARRAY(B)
      ARRAY(B) = TEMP
      RETURN
      END
