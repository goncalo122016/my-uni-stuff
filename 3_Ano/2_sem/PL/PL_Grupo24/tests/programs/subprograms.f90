PROGRAM TestSubprograms
    INTEGER A, B, RESULTADO
    
    A = 5
    B = 3
    
    ! Chamada da function
    RESULTADO = SOMA(A, B)
    
    ! Chamada da subroutine
    CALL IMPRIME(RESULTADO)
END

INTEGER FUNCTION SOMA(X, Y)
    INTEGER X, Y
    SOMA = X + Y
    RETURN
END

SUBROUTINE IMPRIME(VALOR)
    INTEGER VALOR
    PRINT *, VALOR
    RETURN
END
