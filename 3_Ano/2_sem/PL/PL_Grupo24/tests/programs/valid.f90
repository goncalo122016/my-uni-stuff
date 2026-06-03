PROGRAM VALID
    INTEGER A, B, C
    A = 10
    B = 20
    C = A + B
    PRINT *, 'Sum: ', C
    IF (C .GT. 25) THEN
        PRINT *, 'Greater than 25'
    ELSE
        PRINT *, 'Not greater'
    ENDIF
END
