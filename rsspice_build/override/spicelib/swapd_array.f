C     See GrammarPatcher::patch_swap

      SUBROUTINE SWAPD_ARRAY ()

      IMPLICIT NONE

      DOUBLE PRECISION ARRAY (*)
      DOUBLE PRECISION TEMP
      INTEGER A1, A2
      INTEGER B1, B2

      RETURN

      ENTRY SWAPD_ARRAY_1 (ARRAY, A, B)
      TEMP = ARRAY(A)
      ARRAY(A) = ARRAY(B)
      ARRAY(B) = TEMP
      RETURN

      ENTRY SWAPD_ARRAY_2 (ARRAY, A1, A2, B1, B2)
      TEMP = ARRAY(A1,A2)
      ARRAY(A1,A2) = ARRAY(B1,B2)
      ARRAY(B1,B2) = TEMP
      RETURN

      END
