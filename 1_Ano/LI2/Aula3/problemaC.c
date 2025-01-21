#include <stdio.h>
#include <locale.h>
#include <wchar.h>
#include <string.h>
#include <assert.h>
#include <math.h>

// Função que calcula o expoente da ultima parte da expressão «dos números normalizados e desnormalizados
double calc_exp(char* Exp, int E){
    int k = pow(2, (E - 1)) - 1;
    int m = 0;
    for (int i = 0; i < E; ++i) {
        if (Exp[i] == '1') {
            m += pow(2, (E - 1 - i));
        }
    }
    double r = m - k;
    return r;
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

// Função que testa se todos os elementos de um array são todos de um determinado dado valor
int todos_iguais (char* nbin, char c, int E) {
    for (int i = 0; i<E;  i++) {
        if (nbin[i] != c) return 0;
    }
    return 1;
}

// Função que calcula o resultado principal pretendido
void calcula_valor (char* Exp, char* Mant, int E, int M, char Sinal) {
    if (todos_iguais(Exp, '0', E)) {
        if (todos_iguais(Mant, '0', M))
            if  (Sinal == '0') printf ("%lg\n", 0.0); // Caso em que o número é 0 
            else  printf ("%lg\n", -0.0); // Caso em que o número é -0
        else {
            double result = pow(-1, Sinal - '0') * decimal_mantissa (Mant, M) * pow(2, (1 + calc_exp(Exp, E))); // Caso em que o número é DESNORMALIZADO
            printf("%lg\n", result);
        }
    }
    else if (todos_iguais(Exp, '1', E)) {
            if (todos_iguais(Mant, '0', M)) {
                if  (Sinal == '0') printf ("+");
                else  printf("-");
                printf ("Infinity\n"); // Caso em que o número é ± Infinito
            }
            else printf ("NaN\n"); // Caso em que o número não existe (Not a Number)
    }
    else  {
        double result = pow(-1, Sinal - '0') * (1 + decimal_mantissa (Mant, M)) * pow(2, calc_exp(Exp, E)); // Caso em que o número é NORMALIZADO (exceção aos outros casos todos)
        printf("%lg\n", result);
    }
}

int main()
{
    /* Mudar para UTF8 */
    setlocale(LC_CTYPE, "C.UTF-8");

    int n_testes;
    
    if (scanf ( "%d\n", &n_testes) == 1) {

        char inputs[n_testes] [BUFSIZ]; 
        for (int i=0; i<n_testes; i++) {
            char buf[BUFSIZ];

            assert(fgets(buf, BUFSIZ, stdin) != NULL);
            assert(buf[strlen(buf) - 1] == '\n');
            buf[strlen(buf) - 1] = 0;
            strcpy(inputs[i], buf);   
        }

        for (int j = 0; j<n_testes; j++) {   

            int E, M;
            char bits[BUFSIZ] = {0};

            sscanf(inputs[j], "%d %d %s", &E, &M, bits);

            char Sinal;
            char Exp[BUFSIZ] = {0};
            char Mant[BUFSIZ] = {0};
            char formato[BUFSIZ];

            sprintf(formato, "%%c%%%ds%%%ds", E, M);
            sscanf(bits, formato, &Sinal, Exp, Mant);

            calcula_valor (Exp, Mant, E, M, Sinal);
        }
    }   
    return 0;
}
