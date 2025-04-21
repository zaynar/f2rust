      LOGICAL FUNCTION SETERR ( STATUS )

C     FAILED is called extremely frequently, and our standard
C     implementation of SAVE variables (using a hashmap) has a
C     noticeable performance cost, so we have some intrinsics
C     specifically to optimise this case.

      IMPLICIT NONE
      LOGICAL               STATUS
      LOGICAL FAILED
      CALL F2RUST_SET_FAILED(STATUS)
      SETERR = .TRUE.
      RETURN

      ENTRY FAILED ()
      FAILED = F2RUST_GET_FAILED()
      END
