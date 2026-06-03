#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct celula {
    char *palavra;
    int ocorr;
    struct celula * prox;
} *Palavras;

// Função para libertar a memória ocupada pela lista
void libertaLista(Palavras l) {
    while (l != NULL) {
        Palavras aux = l;
        l = l->prox;
        free(aux->palavra);
        free(aux);          
    }
}

int quantasP (Palavras l){
    Palavras atual;
    int pal = 0;
    for (atual = l; atual != NULL; atual = atual->prox) {
        pal++;
    }
    return pal;
}

void listaPal (Palavras l){
    Palavras atual;
    for (atual = l; atual != NULL; atual = atual->prox) {
        printf ("'%s' -> %d vezes\n", atual->palavra, atual->ocorr);
    }
    putchar ('\n');
}

char *ultima (Palavras l){
    if (l == NULL) return NULL;
    Palavras atual;
    for (atual = l; atual->prox != NULL; atual = atual->prox);
    return atual->palavra;
}

Palavras acrescentaInicio (Palavras l, char *p){
    Palavras nova = malloc (sizeof (struct celula));
    nova->palavra = strdup (p);
    nova->ocorr = 1;
    nova->prox = l;
    
    return nova;
}

Palavras acrescentaFim (Palavras l, char *p){
    Palavras nova = malloc(sizeof(struct celula));
    nova->palavra = strdup (p);
    nova->ocorr = 1;
    nova->prox = NULL;
    if (l == NULL) return nova;

    Palavras atual;
    for (atual = l; atual->prox != NULL; atual = atual->prox);
    atual->prox = nova;
    return l;
}

Palavras acrescenta (Palavras l, char *p){
    Palavras atual;

    for (atual = l; atual != NULL; atual = atual->prox) {
        if (strcmp(atual->palavra, p) == 0) {
            atual->ocorr++;
            return l;
        }
    }
    return acrescentaFim (l, p);
}

Palavras acrescenta1 (Palavras l, char *p){
    Palavras atual, novo, ant = NULL;

    for (atual = l; strcmp(atual->palavra, p) != 0 && atual != NULL; atual = atual->prox) ant = atual;

    if (strcmp(atual->palavra, p) == 0) {
        atual->ocorr++;
        return l;
    }

    Palavras nova = malloc(sizeof(struct celula));
    nova->palavra = strdup (p);
    nova->ocorr = 1;
    nova->prox = atual;

    if (ant == NULL) return nova;
    else ant->prox = nova;
    return l;
}

Palavras maisFreq (Palavras l){
    if (l == NULL) return NULL;
    Palavras freq = l, atual = l;

    while (atual != NULL) {
        if (atual->ocorr > freq->ocorr) {
            freq = atual;
        }
        atual = atual->prox;
    }
    return freq;
}

int main () {
    Palavras dic = NULL;

    char * canto1 [44] = {"as", "armas", "e", "os", "baroes", "assinalados",
                          "que", "da", "ocidental", "praia", "lusitana", 
                          "por", "mares", "nunca", "de", "antes", "navegados",
                          "passaram", "ainda", "alem", "da", "taprobana",
                          "em", "perigos", "e", "guerras", "esforcados",
                          "mais", "do", "que", "prometia", "a", "forca", "humana",
                          "e", "entre", "gente", "remota", "edificaram", 
                          "novo", "reino", "que", "tanto", "sublimaram"};

    printf ("\n_____________ Testes _____________\n\n");

    int i; struct celula *p;
    for (i=43;i>=0;i--)
        dic = acrescentaInicio (dic, canto1[i]);

    printf ("Foram inseridas %d palavras\n", quantasP (dic));
    printf ("palavras existentes:\n");
    listaPal (dic);
    printf ("última palavra inserida: %s\n", ultima (dic));

    libertaLista (dic);

    dic = NULL;

    srand(42);
    
    for (i=0; i<1000; i++)
        dic = acrescenta (dic, canto1 [rand() % 44]);
    
    printf ("Foram inseridas %d palavras\n", quantasP (dic));
    printf ("palavras existentes:\n");
    listaPal (dic);
    printf ("última palavra inserida: %s\n", ultima (dic));
    
    p = maisFreq (dic);
    printf ("Palavra mais frequente: %s (%d)\n", p->palavra, p->ocorr);

    libertaLista (dic);
    
    printf ("\n_________ Fim dos testes _________\n\n");

    return 0;
}

