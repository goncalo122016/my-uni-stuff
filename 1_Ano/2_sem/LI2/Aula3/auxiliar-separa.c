#include <stdio.h>
#include <locale.h>
#include <wchar.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>
#include <math.h>

// Função que testa se todos os elementos de um array são todos de um determinado dado valor
bool all_equal (char* tc, char c, int E) {
    for (int i = 0; i<E;  i++) {
        if (tc[i] != c) return false;
        return true;
    }
    return 0;
}

// Função que calcula a parte decimal do número presente na mantissa
double decimal_mantissa(char* Mant, int M) {
    double mantissa = 0.0;
    for (int i = 0; i < M; i++) {
        if (Mant[i] == '1') {
            mantissa += pow(2, -(i + 1));
        }
    }
    return mantissa;
}

// Função que calcula o resultado principal pretendido
void valor(char* Exp, char* Mant, int E, int M, char Sinal) {
    if (all_equal(Exp, '0', E)) {
        if (all_equal(Mant, '0', M))
            if  (Sinal == '0') printf ("%lg\n", 0.0); // Caso em que o número é 0 
            else  printf ("%lg\n", -0.0); // Caso em que o número é -0
        else {
            double resultado = pow(-1, Sinal - '0') * decimal_mantissa (Mant, M) * pow(2, 1 - E); // Caso em que o número é DESNORMALIZADO
            printf("%lg\n", resultado);
        }
    }
    else if (all_equal(Exp, '1', E)) {
            if (all_equal(Mant, '0', M)) {
                if  (Sinal == '0') printf ("+");
                else  printf("-");
                printf ("Infinity\n"); // Caso em que o número é ± Infinito
            }
            else printf ("NaN\n"); // Caso em que o número não existe (Not a Number)
    }
    else  {
        double result = pow(-1, Sinal - '0') * (1 + decimal_mantissa (Mant, M)) * pow(2, E); // Caso em que o número é NORMALIZADO (exceção aos outros casos todos)
        printf("%lg\n", result);
    }
}

int main()
{
    /* Mudar para UTF8 */
    setlocale(LC_CTYPE, "C.UTF-8");

    int primeiro_imput;
    char buf[BUFSIZ];
    char inputs[primeiro_imput];

    if (scanf ( "%d\n", &primeiro_imput) == 1) {
    for (int i=0; i<primeiro_imput; i++) {
        

        assert(fgets(buf, BUFSIZ, stdin) != NULL);
        assert(buf[strlen(buf) - 1] == '\n');
        buf[strlen(buf) - 1] = 0;
        int E, M;
        char bits[BUFSIZ] = {0};

        sscanf(buf, "%d %d %s", &E, &M, bits);

        char Sinal;
        char Exp[BUFSIZ] = {0};
        char Mant[BUFSIZ] = {0};

        char formato[BUFSIZ];

        sprintf(formato, "%%c%%%ds%%%ds", E, M);
        sscanf(bits, formato, &Sinal, Exp, Mant);
        
        valor (Exp, Mant, E, M, Sinal);
    }
    
}
    return 0;
}