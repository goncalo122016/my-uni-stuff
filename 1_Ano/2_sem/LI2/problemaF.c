#include <stdio.h>

int main() {
    // Declarar variáveis para horas e minutos
    int horas, minutos;

    // Ler horas e minutos
    scanf("%d %d", &horas, &minutos);

    // Mapear os valores de horas e minutos para os códigos UTF-8 correspondentes
    int utf8_symbols[13][2] = {
        {1, 0x1f550}, {2, 0x1f551}, {3, 0x1f552}, {4, 0x1f553}, {5, 0x1f554},
        {6, 0x1f555}, {7, 0x1f556}, {8, 0x1f557}, {9, 0x1f558}, {10, 0x1f559},
        {11, 0x1f55a}, {12, 0x1f55b}, {0, 0}  // Marcar o final do array
    };

    // Encontrar o código UTF-8 correspondente
    int i = 0;
    while (utf8_symbols[i][0] != horas || utf8_symbols[i][1] != minutos) {
        i++;
    }

    // Verificar se a combinação foi encontrada e imprimir o resultado
    if (utf8_symbols[i][0] != 0) {
        printf("%lc\n", utf8_symbols[i][1]);
    } else {
        printf("Combinação inválida.\n");
    }

    return 0;
}
