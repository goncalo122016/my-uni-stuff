#include <stdio.h> 
#include <locale.h>
#include <wchar.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include "Guiao1.h"
#include "Guiao2.h"

int main() { 

    int num_testes; // recebe o número de expressões
     
    if (scanf("%d", &num_testes) != 1) {
        printf("Erro ao ler o número de expressões.\n");
        return 1;
    }

    while (getchar() != '\n'); // O loop continuará até que o utilizador clique na tecla "enter", posteriormente o buffer de entrada é limpo evitando, possíveis erros de leitura.

    char final[num_testes + 1][100]; // array que irá armazenar os "printf" relativos a cada linha de input (este array vai ser exibido no final)

    for (int i = 1; i < num_testes; i++) {
        char cartas[1000];
        assert (fgets(cartas, sizeof(cartas), stdin) != NULL);
        cartas[strlen(cartas) - 1] = '\0';
        
        for (int j = 0; j < strlen(cartas); j++) {

            cartas[j] = carta_para_codigo(cartas[j]);

        }
        
        // a função sprintf é uma função já definida no "<string.h>" que permite armazenar um dterminado "printf" num array.
        if (ncartas(cartas) == 1) { // Verifica se apenas uma carta foi recebida
            sprintf(final[i], "conjunto com 1 carta onde a carta é %s", cartas);
        } else if (sequencia(cartas)) {
            sprintf(final[i], "sequência com %d cartas onde a carta mais alta é %s", ncartas(cartas), carta_mais_alta_principal(cartas));
        } else if (mesmo_valor(cartas)) {
            sprintf(final[i], "conjunto com %d cartas onde a carta mais alta é %s", (ncartas(cartas)) , carta_mais_alta_principal(cartas));
        } else if (dupla_sequencia(cartas)) {
            sprintf(final[i], "dupla sequência com %d cartas onde a carta mais alta é %s", ncartas(cartas)/2 , carta_mais_alta_principal(cartas));
        } else {
            strcpy(final[i], "Nada!");
        }
    }

    for (int i = 1; i <= num_testes; i++) {
        printf("%s\n", final[i]);
    }

    return 0;
}