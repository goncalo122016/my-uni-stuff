#include <stdio.h>
#include <locale.h>
#include <wchar.h>
#include <ctype.h>
#include <math.h>

// Função que decifra (conhecido o deslocamento/chave) a mensagem substituindo as letras da mesagem pelas "letras deslocadas"
void decifrarMensagem(char msg[], int chave) {
    printf ("%d ",chave);
    for (int i = 0; msg[i] != '\0'; i++) {
        if (isalpha(msg[i])) {
            char letra = toupper(msg[i]);
            char decifrado = ((letra - 'A' + chave) % 26) + 'A'; // Aplicar o deslocamento inverso
            if (islower(msg[i])) decifrado = tolower(decifrado); // Manter a letra minúscula se era minúscula
            printf("%c", decifrado);
        } 
        else printf("%c", msg[i]); // Manter os outros caracteres como estão
    }
}

// Função que calcula a medida da pela diferença entre a Frequência esperada e a Frequência obtida (de cada letra)
double calcularMedida(char msg[], int chave) {
    double frequenciaEsperada[26] = {43.31, 10.56, 23.13, 17.25, 56.88, 9.24, 12.59,
                                     15.31, 38.45, 1.00, 5.61, 27.98, 15.36, 33.92,
                                     36.51, 16.14, 1.00, 38.64, 29.23, 35.43, 18.51,
                                     5.13, 6.57, 1.48, 9.06, 1.39}; // Frequência Esperada relativa das letras A-Z
    int frequenciaObtida [26] = {0};
    double medida = 0.0;

    for (int i = 0; msg[i] != '\0'; i++) {
        char letra = msg[i];

        if ((letra >= 'A' && letra <= 'Z') || (letra >= 'a' && letra <= 'z')) {
            letra = toupper(letra);
            letra = 'A' + ((letra - 'A' + chave) % 26);
            frequenciaObtida[letra - 'A']++;
        }
    }

    for (int j = 0; j < 26; j++) {
        medida += (pow((frequenciaEsperada[j] - frequenciaObtida[j]), 2)) / frequenciaEsperada[j];
    }
    return medida;
}



int main() {
    /* Mudar para UTF8 */
    setlocale(LC_CTYPE, "C.UTF-8");

    char encriptada[10000];

    if (fgets(encriptada, sizeof(encriptada), stdin) != NULL) {

        int chave = 0; 
        double aux = calcularMedida (encriptada, 0);
        
        for (int i = 1; i < 26; i++) {
            double medida = calcularMedida(encriptada, i);   // Calcular a medida para a substituição (iteração) atual

            if (medida<aux) {    // Escolher qual a menor medida e guardar a chave em que isso acontece
                aux = medida;
                chave = i;
            }
        }
    decifrarMensagem(encriptada, chave);  // Decifrar a mensagem usando a chave encontrada
    } 
    return 0;
}