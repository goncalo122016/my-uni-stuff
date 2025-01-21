#include <stdio.h>
#include <string.h>

// 1.Funções sobre strings

int contaVogais (char s[]) {
    int cont = 0;
    for (int i = 0; s[i]; i++) {
        if (s[i] == 'a' || s[i] == 'A' ||
            s[i] == 'e' || s[i] == 'E' ||
            s[i] == 'i' || s[i] == 'I' ||
            s[i] == 'o' || s[i] == 'O' ||
            s[i] == 'u' || s[i] == 'U') cont++;
    }
    return cont;
}

int retiraVogaisRep (char *s) {
    int i, j;
    for (i=j=0; s[i]; i++) {
        if ((s[i] != 'a' && s[i] != 'A' && s[i] != 'e' && s[i] != 'E' && s[i] != 'i' && s[i] != 'I' && s[i] != 'o' && s[i] != 'O' && s[i] != 'u' && s[i] != 'U') || s[i] != s[i+1])
            s[j++] = s[i];
    }
    s[j] = '\0';
    return i - j;
}

int duplicaVogais (char *s) {
    int i = strlen (s);
    int j = i + contaVogais(s);
    for (i; i>=0; i--) {
        s[j--] = s[i];
        if (s[i] == 'a' || s[i] == 'A' ||
            s[i] == 'e' || s[i] == 'E' ||
            s[i] == 'i' || s[i] == 'I' ||
            s[i] == 'o' || s[i] == 'O' ||
            s[i] == 'u' || s[i] == 'U') s[j--] = s[i];
    }
    return contaVogais (s);
}

// 2.Arrays ordenados

int ordenado (int v[], int N) {
    if (N == 0 || N == 1) return 1;
    for (int i = 1; i<N; i++) {
        if (v[i-1] > v[i]) return 0;
    }
    return 1;
}

int ordenado2 (int a[], int i, int j) {
    for (int k = i + 1; k <= j; k++) {
        if (a[k-1] > a[k]) return 0;
    }
    return 1;
}

void merge(int a[], int na, int b[], int nb, int r[]) {
    int i = 0; // Índice para percorrer o array 'a'
    int j = 0; // Índice para percorrer o array 'b'
    int k = 0; // Índice para preencher o array 'r'

    while (i < na && j < nb) {
        if (a[i] < b[j]) r[k++] = a[i++];
        else r[k++] = b[j++];
    }

    while (i < na) r[k++] = a[i++];
    while (j < nb) r[k++] = b[j++];
}


int partition (int v[], int N, int x) {
    int aux[N];
    int i, j = 0, k = 0;

    for (i = 0; i < N; i++) {
        if (v[i] <= x) {
            aux[j++] = v[i];
        }
    }

    for (i = N - 1; i >= 0; i--) {
        if (v[i] > x) {
            aux[N - 1 - k] = v[i];
            k++;
        }
    }

    for (i = 0; i < N; i++) v[i] = aux[i];
    return j;
}

int partition2(int v[], int N, int x) {
    int j, i = 0;

    for (j = 0; j < N; j++) {
        if (v[j] <= x) {
            int temp = v[i];
            v[i] = v[j];
            v[j] = temp;
            i++;
        }
    }
    
    return i;
}


int main() {
    //char s1[] = "Estaa e umaa string coom duuuplicadoos";
    //printf ("%d\n", retiraVogaisRep (s1));
    //for (int i = 0; s1[i]; i++) printf ("%c", s1[i]);
    //printf ("\n");
    
    //Exemplo de array para testar a função ordenado
    //int v[10] = {1,2,3,4,5,6,7,8,9,10};
    //printf("%d\n", ordenado(v, 10));
    
    int v[] = {3, 5, 8, 2, 9, 1, 6, 4};
    int N = sizeof(v) / sizeof(v[0]);
    int x = 5;

    printf("Array antes da partição: ");
    for (int i = 0; i < N; i++) {
        printf("%d ", v[i]);
    }
    printf("\n");

    int indice = partition2(v, N, x);

    printf("Array depois da partição: ");
    for (int i = 0; i < indice; i++) {
        printf("%d ", v[i]);
    }
    printf("\n");
    printf("Índice: %d\n", indice);

    return 0;
} 