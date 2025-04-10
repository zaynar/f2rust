      SUBROUTINE ZZCPUTIM ( TVEC )

C     SPICE's implementation uses RCLOCK(n) for DATE/TIME/ZONE,
C     which is an aliasing problem. Use separate variables instead.

      DOUBLE PRECISION      TVEC ( 6 )

      LOGICAL               RETURN

      INTEGER               DTIME (8)
      CHARACTER*(12)        DATE, TIME, ZONE

      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZCPUTIM' )
      END IF

      CALL DATE_AND_TIME( DATE, TIME, ZONE, DTIME )

      TVEC ( 1 ) = DBLE( DTIME(1) )
      TVEC ( 2 ) = DBLE( DTIME(2) )
      TVEC ( 3 ) = DBLE( DTIME(3) )
      TVEC ( 4 ) = DBLE( DTIME(5) )
      TVEC ( 5 ) = DBLE( DTIME(6) )
      TVEC ( 6 ) = DBLE( DTIME(7) ) + DBLE( DTIME(8) )/1000.D0

      CALL CHKOUT ( 'ZZCPUTIM' )
      RETURN

      END
