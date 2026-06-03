#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct lligada {
    int valor;
    struct lligada *prox;
} *LInt;

//1
int length (LInt l) {
    LInt aux = l;
    int cont = 0;
    while (aux != NULL) {
        cont++;
        aux = aux->prox;
    }
    return cont;
}

//2
void freeL(LInt l) {
    while (l != NULL) {
        LInt aux = l;
        l = l->prox;
        free(aux);
    }
}

void freeL2(LInt l) {
    LInt aux;
    while (l != NULL) {
        aux = l->prox;
        free(l);
        l = aux;
    }
}

//3
void imprimeL (LInt l) {
    LInt aux = l;
    while (aux != NULL) {
        printf("%d\n", aux->valor);
        aux = aux->prox;
    }
}

//4
LInt reverseL (LInt l) {
    LInt rev = NULL, atual = l, prox = NULL;
    while (atual != NULL) {
        prox = atual->prox;
        atual->prox = rev;
        rev = atual;
        atual = prox;
    }
    return rev;
}

LInt reverseL2(LInt l) {
    LInt prox = l->prox;
    l->prox = NULL;
    while(prox) {
        LInt temp = prox->prox;
        prox->prox = l;
        l = prox;
        prox = temp;
    }
    return l;
}

//5
void insertOrd (LInt *l, int x) {
    LInt aux, nova, ant = NULL;
    nova = malloc (sizeof (struct lligada));
    nova->valor = x;
    nova->prox = NULL;

    if (*l == NULL || (*l)->valor >= x) {
        nova->prox = *l;
        *l = nova;
        return;
    }
    
    for (aux = *l; aux != NULL && aux->valor < x; aux = aux->prox) ant = aux;
    
    nova->prox = aux;
    ant->prox = nova;
}

//6
int removeOneOrd (LInt *l, int x) {
    LInt aux = *l, ant = NULL;

    for (aux; aux != NULL && aux->valor != x; aux = aux->prox) {
        ant = aux;
    }
    if (aux == NULL) {
        return 1;
    }
    if (ant == NULL) *l = aux->prox;
    else ant->prox = aux->prox;

    free(aux);
    return 0;
}

//7
void merge(LInt *r, LInt a, LInt b) {
    *r = NULL; // Inicializa a lista resultante como vazia

    // Caso especial: uma das listas é vazia
    if (a == NULL) {
        *r = b;
        return;
    }
    if (b == NULL) {
        *r = a;
        return;
    }

    // Escolhe o primeiro nó da lista resultante
    if (a->valor < b->valor) {
        *r = a;
        a = a->prox;
    } else {
        *r = b;
        b = b->prox;
    }

    LInt atual = *r; // Ponteiro para o último nó adicionado à lista resultante

    // Funde as duas listas até que uma delas se esgote
    while (a != NULL && b != NULL) {
        if (a->valor < b->valor) {
            atual->prox = a;
            a = a->prox;
        } else {
            atual->prox = b;
            b = b->prox;
        }
        atual = atual->prox;
    }

    // Se ainda houver elementos na lista 'a' ou na lista 'b', os adiciona à lista resultante
    if (a != NULL) {
        atual->prox = a;
    } else {
        atual->prox = b;
    }
}

void merge2 (LInt* r, LInt a, LInt b) {
    if(!a && !b) return;
    if(b == NULL || a != NULL && a->valor < b->valor) {
        (*r) = a;
        merge(&((*r)->prox),a->prox,b);
    }
    else {
        (*r) = b;
        merge(&((*r)->prox),a,b->prox);
    }
}

//8
void splitQS (LInt l, int x, LInt *mx, LInt *Mx) {
    LInt menor = NULL;
    LInt maior_igual = NULL;
    LInt atual;

    for (atual = l ; atual != NULL; atual = atual->prox) {
        if (atual->valor < x) {
            if (*mx == NULL) {
                *mx = atual;
                menor = *mx;
            } else {
                menor->prox = atual;
                menor = menor->prox;
            }
        } else {
            if (*Mx == NULL) {
                *Mx = atual;
                maior_igual = *Mx;
            } else {
                maior_igual->prox = atual;
                maior_igual = maior_igual->prox;
            }
        }
    }
    if (menor != NULL) menor->prox = NULL;
    if (maior_igual != NULL) maior_igual->prox = NULL;
}

void splitQS2(LInt l, int x, LInt *mx, LInt *Mx){
    LInt aux = l;
	while(aux != NULL){
		if(aux->valor < x){
			*mx = aux;
			mx = &((*mx)->prox);
		} else {
			*Mx = aux;
			Mx = &( (*Mx)->prox);
		}
		aux = aux->prox;
	}
	*mx = NULL;
	*Mx = NULL;
}

//9
LInt parteAmeio (LInt *l) {
    LInt lento = *l; // Ponteiro lento
    LInt rapido = *l; // Ponteiro rápido
    LInt anterior_lento = NULL; // Ponteiro para o nó anterior ao ponteiro lento

    while (rapido != NULL && rapido->prox != NULL) {
        anterior_lento = lento;
        lento = lento->prox;
        rapido = rapido->prox->prox;
    }
    if (anterior_lento != NULL) {
        anterior_lento->prox = NULL;
    }
    return lento;
}

//10
int removeAll (LInt *l, int x) {
    LInt atual = *l, ant = NULL;
    int r = 0;

    while (atual != NULL) {
        if (atual->valor == x) {
            r++;
            if (ant == NULL) {
                *l = atual->prox;
            }
            else {
                ant->prox = atual->prox;
            }
            LInt temp = atual;
            atual = atual->prox;
            free(temp);
        }
        else {
            ant = atual;
            atual = atual->prox;
        }
    }
    return r;
}

//11
int removeDups (LInt *l) {
    LInt atual = *l;
    int r = 0;
    while (atual != NULL && atual->prox != NULL) {
        LInt aux = atual;
        while (aux->prox != NULL) {
            if (atual->valor == aux->prox->valor) {
                LInt temp = aux->prox;
                aux->prox = aux->prox->prox;
                free(temp);
                r++;
            }
            else {
                aux = aux->prox;
            }
        }
        atual = atual->prox;
    } 
    return r;
}

int removeDups2(LInt *l) {
    int rem = 0;
    for(;*l; l = &((*l)->prox)) {
        LInt prevAux = (*l), aux = (*l)->prox;
        for(; aux; aux = prevAux->prox) {
            if(aux->valor == (*l)->valor) {
                prevAux->prox = aux->prox;
                rem++;
                free(aux);
            }
            else prevAux = aux;
        }
    }
    return rem;
}

//12
int removeMaiorL (LInt *l) {
    LInt atual = *l, ant = NULL;
    int maior = (*l)->valor;

    while (atual != NULL) {
        if (atual->valor > maior) {
            maior = atual->valor;
        }
        atual = atual->prox;
    }

    atual = *l;
    while (atual != NULL) {
        if (atual->valor == maior) {
            if (ant == NULL) {
                *l = atual->prox;
            } else {
                ant->prox = atual->prox;
            }
            free(atual);
            return maior;
        }
        ant = atual;
        atual = atual->prox;
    }

    return maior;
}

//13
void init (LInt *l) {
    if ((*l)->prox == NULL) {
        free(*l);
        *l = NULL; 
        return;   
    }
    
    LInt atual, ant = NULL;
    for (atual = *l; atual->prox != NULL; atual = atual->prox) ant = atual;
    ant->prox = NULL;
    free(atual);
}

//14
void appendL (LInt *l, int x) {
    LInt nova = malloc(sizeof(struct lligada));
    nova->valor = x;
    nova->prox = NULL;

    if (*l == NULL) {
        *l = nova;
        return;
    }

    LInt atual;
    for (atual = *l; atual->prox != NULL; atual = atual->prox);
    atual->prox = nova;
}

//15
void concatL (LInt *a, LInt b) {
    if (*a == NULL) {
        *a = b;
        return;
    }
    LInt atual;
    for (atual = *a; atual->prox != NULL; atual = atual->prox);
    atual->prox = b;
}

//16
LInt cloneL (LInt l) {
    LInt atual = l, dup = NULL, ultimo = NULL;

    while (atual != NULL) {
        LInt novo = malloc(sizeof(struct lligada));
        novo->valor = atual->valor;
        novo->prox = NULL;

        if (dup == NULL) {
            dup = novo;
        } else {
            ultimo->prox = novo;
        }
        ultimo = novo;
        atual = atual->prox;
    }

    return dup;
}

//17
LInt cloneRev(LInt l) {
    LInt clone = NULL;
    LInt atual;

    for (atual = l; atual != NULL; atual = atual->prox) {
        LInt novo = malloc(sizeof(struct lligada));
        novo->valor = atual->valor;

        novo->prox = clone;
        clone = novo;
    }

    return clone;
}

//18
int maximo (LInt l) {
    int maior = l->valor;
    LInt atual;
    for (atual = l->prox; atual != NULL; atual = atual->prox) {
        if (atual->valor > maior) 
            maior = atual->valor;
    }
    return maior;
}

//19
int take (int n, LInt *l) {
    LInt atual = *l;
    LInt anterior = NULL;
    int comprimento = 0;

    while (comprimento < n && atual != NULL) {
        comprimento++;
        anterior = atual;
        atual = atual->prox;
    }
    
    if (comprimento == n) {
        while (atual != NULL) {
            LInt proximo = atual->prox;
            free(anterior->prox);
            anterior->prox = proximo;
            atual = proximo;
        }
        if (anterior != NULL) {
            anterior->prox = NULL;
        } else {
            *l = NULL;
        }
    }
    return comprimento;
}

int freeLInt (LInt l) {
    int lib = 0;
    while (l != NULL) {
        LInt temp = l;
        l = l->prox;
        free(temp);
        lib++;
    }
    return lib;
}

int take2(int n, LInt *l) {
    if (*l == NULL) return 0;
    LInt atual, ant = NULL;
    for (atual = *l; atual != NULL && n > 0; atual = atual->prox) {
        ant = atual;
        n--;
    }
    if (ant != NULL) ant->prox = NULL;
    return freeLInt(atual);
}

//20
int drop (int n, LInt *l) {
    LInt atual = *l;
    int comprimento = 0;

    while (n > 0 && atual != NULL) {
        n--;
        LInt proximo = atual->prox;
        free(atual);
        atual = proximo;
        comprimento++;
    }
    *l = atual;

    return comprimento;
}

//21
LInt Nforward (LInt l, int N) {
    if (l == NULL || N <= 0) return l;

    LInt atual = l;
    while (N > 0) {
        atual = atual->prox;
        N--;
    }
    return atual;
}

//22
int listToArray (LInt l, int v[], int N) {
    LInt atual = l;
    int cont = 0;

    while (atual != NULL && cont < N) {
        v[cont++] = atual->valor;
        atual = atual->prox;
    }
    return cont;
}

//23
LInt arrayToList (int v[], int N) {
    LInt lista = NULL, atual = NULL;

    for (int i = 0; i < N; i++) {
        LInt novo = malloc(sizeof(struct lligada));
        novo->valor = v[i];
        novo->prox = NULL;
        if (lista == NULL) {
            lista = novo;
        } else {
            atual->prox = novo;
        }
        atual = novo;
    }
    return lista;
}

//24
LInt somasAcL(LInt l) {
    if (l == NULL) return NULL;

    LInt novaLista = malloc(sizeof(struct lligada));
    LInt atual = l; 
    LInt atualNova = novaLista;
    int soma = 0;
    
    while (atual != NULL) {
        soma += atual->valor;
        atualNova->valor = soma;

        if (atual->prox != NULL) {
            atualNova->prox = malloc(sizeof(struct lligada));
            atualNova = atualNova->prox;
        } else {
            atualNova->prox = NULL;
        }
        atual = atual->prox;
    }

    return novaLista;
}

//25
void remreps (LInt l) {
    if (l == NULL || l->prox == NULL) {
        return;
    }
    LInt atual = l;
    while (atual->prox != NULL) {
        if (atual->valor == atual->prox->valor) {
            LInt temp = atual->prox;
            atual->prox = atual->prox->prox;
            free(temp);
        } else {
            atual = atual->prox;
        }
    }
}

//26
LInt rotateL(LInt l) {
    if (l == NULL || l->prox == NULL) {
        return l;
    }

    LInt atual = l, primeiro = l;

    while (atual->prox->prox != NULL) atual = atual->prox;
    
    atual->prox->prox = primeiro;
    atual->prox->prox->prox = NULL;
    primeiro = primeiro->prox;

    return primeiro;
}

//27
LInt parte(LInt l) {
    if(!l || !l->prox) return NULL;
    LInt newL = l->prox;
    l->prox = l->prox->prox;
    newL->prox = parte(l->prox);
    return newL;
}

LInt parte2(LInt l) {
    if (l == NULL || l->prox == NULL) {
        return NULL;
    }

    LInt atual = l, par = NULL, impar = NULL, parAtual = NULL, imparAtual = NULL; 
    int pos = 1;
    while (atual != NULL) {
        if (pos % 2 == 0) {
            if (par == NULL) {
                par = atual;
                parAtual = par;
            } else {
                parAtual->prox = atual;
                parAtual = parAtual->prox;
            }
        } else {
            if (impar == NULL) {
                impar = atual;
                imparAtual = impar; 
            } else {
                imparAtual->prox = atual;
                imparAtual = imparAtual->prox; 
            }
        }
        atual = atual->prox;
        pos++; 
    }

    if (parAtual != NULL) parAtual->prox = NULL;
    if (imparAtual != NULL) imparAtual->prox = NULL;
    return par;
}

//28

typedef struct nodo {
    int valor;
    struct nodo *esq, *dir;
} *ABin;

int altura(ABin a) {
    if (a == NULL) return 0;
    int ae = altura(a->esq);
    int ad = altura(a->dir);
    if (ae > ad) return 1 + ae;
    else return 1 + ad;
}

//29
ABin cloneAB (ABin a) {
    ABin clone = malloc(sizeof(struct nodo));
    clone->valor = a->valor;
    clone->esq = cloneAB(a->esq);
    clone->dir = cloneAB(a->dir);

    return clone;
}

//30
void mirror(ABin a) {
    if (a == NULL) return;
    ABin aux = a->esq;
    a->esq = a->dir;
    a->dir = aux;
    mirror(a->esq);
    mirror(a->dir);
}

// Função para imprimir a árvore binária (pré-ordem)
void printTree(ABin a) {
    if (a == NULL) {
        printf("* ");
        return;
    }
    printf("%d ", a->valor);
    printTree(a->esq);
    printTree(a->dir);
}

// Função para imprimir a lista ligada
void printList(LInt l) {
    while (l != NULL) {
        printf("%d ", l->valor);
        l = l->prox;
    }
    printf("\n");
}

//31

void adiciona(LInt *l, int x) {
    LInt novo = (LInt) malloc(sizeof(struct nodo));
    novo->valor = x;
    novo->prox = NULL;
    if (*l == NULL) {
        *l = novo;
    } else {
        LInt atual = *l;
        while (atual->prox != NULL) atual = atual->prox;
        atual->prox = novo;
    }
}
void inorder (ABin a, LInt *l) {
    if (a == NULL) return;
    inorder(a->esq, l);
    adiciona(l, a->valor);
    inorder(a->dir, l);
}

//32
void preorder(ABin a, LInt *l) {
    if (a == NULL) return;
    adiciona(l, a->valor);
    preorder(a->dir, l);
    preorder(a->esq, l);
}

// 33
void posorder(ABin a, LInt *l) {
    if (a == NULL) return;
    posorder(a->esq, l);
    posorder(a->dir, l);
    adiciona(l, a->valor);
}

//34
int depth(ABin a, int x) {
    if (a == NULL) return -1;
    if (a->valor == x) return 1;
    int nivel_esq = depth(a->esq, x);
    int nivel_dir = depth(a->dir, x);
    if (nivel_esq == -1 && nivel_dir == -1) return -1;
    else if (nivel_esq == -1) return 1 + nivel_dir;
    else if (nivel_dir == -1) return 1 + nivel_esq;
    else return 1 + (nivel_esq < nivel_dir ? nivel_esq : nivel_dir);
}

//35
int freeAB(ABin a) {
    if (a == NULL) return 0;
    int liberados_esq = freeAB(a->esq);
    int liberados_dir = freeAB(a->dir);
    free(a);
    return 1 + liberados_esq + liberados_dir;
}

//36
int pruneAB(ABin *a, int l) {
    if (*a == NULL) return 0;
    if (l == 0) {
        int removidos_esq = pruneAB(&((*a)->esq), 0);
        int removidos_dir = pruneAB(&((*a)->dir), 0);
        free(*a);
        *a = NULL;
        return 1 + removidos_esq + removidos_dir;
    }
    int removidos_esq = pruneAB(&((*a)->esq), l - 1);
    int removidos_dir = pruneAB(&((*a)->dir), l - 1);
    return removidos_esq + removidos_dir;
}

//37
int saoIguais(ABin a, ABin b) {
    if (a == NULL && b == NULL) return 1;
    if (a == NULL || b == NULL || a->valor != b->valor) return 0;
    return saoIguais(a->esq, b->esq) && saoIguais(a->dir, b->dir);
}

int iguaisAB(ABin a, ABin b) {
    return saoIguais(a, b);
}

//38
void nivelAux(ABin a, int n, LInt *l) {
    if (a == NULL) return;
    if (n == 1) {
        LInt novo = malloc(sizeof(struct lint_nodo));
        novo->valor = a->valor;
        novo->prox = *l;
        *l = novo;
    } else {
        nivelAux(a->esq, n - 1, l);
        nivelAux(a->dir, n - 1, l);
    }
}

LInt nivelL(ABin a, int n) {
    LInt lista = NULL;
    nivelAux(a, n, &lista);
    return lista;
}

//39
int nivelV(ABin a, int n, int v[]) {
    if (a == NULL) return 0;
    if (n == 1) {
        v[0] = a->valor;
        return 1;
    }
    int esq = nivelV(a->esq, n-1, v);
    int dir = nivelV(a->dir, n-1, v+esq);
    return esq + dir;
}

//40
int dumpAbin(ABin a, int v[], int N) {
    if (a == NULL || N == 0) return 0;
    int esq = dumpAbin(a->esq, v, N);
    if (esq < N) {
        v[esq] = a->valor;
        esq++;
    }
    if (esq < N) {
        int dir = dumpAbin(a->dir, v + esq, N - esq);
        esq += dir;
    }
    return esq;
}

//41
int somaSubArvore(ABin a) {
    if (a == NULL) return 0;
    int somaEsq = somaSubArvore(a->esq);
    int somaDir = somaSubArvore(a->dir);
    return a->valor + somaEsq + somaDir;
}

ABin somasAcA(ABin a) {
    if (a == NULL) return NULL;
    ABin nova = malloc(sizeof(struct abin_nodo));
    nova->valor = somaSubArvore(a);
    nova->esq = somasAcA(a->esq);
    nova->dir = somasAcA(a->dir);
    return nova;
}

//42
int contaFolhas(ABin a) {
    if (a == NULL) return 0;
    if (a->esq == NULL && a->dir == NULL) return 1;
    return contaFolhas(a->esq) + contaFolhas(a->dir);
}

//43
ABin cloneMirror(ABin a) {
    if (a == NULL) return NULL;
    ABin nova = malloc(sizeof(struct abin_nodo));
    nova->valor = a->valor;
    nova->esq = cloneMirror(a->dir);
    nova->dir = cloneMirror(a->esq);
    return nova;
}

// Função de apoio para criar uma árvore binária para testar as funções
ABin newABin(int r, ABin e, ABin d) {
    ABin a = malloc(sizeof(struct abin_nodo));
    if (a != NULL) {
        a->valor = r;
        a->esq = e;
        a->dir = d;
    }
    return a;
}

// Função de apoio para imprimir uma árvore binária (inorder)
void inorderPrint(ABin a) {
    if (a == NULL) return;
    inorderPrint(a->esq);
    printf("%d ", a->valor);
    inorderPrint(a->dir);
}

int main() {
    //LInt lista = malloc(sizeof(struct lligada));
    //lista->valor = 1;
    //lista->prox = malloc(sizeof(struct lligada));
    //lista->prox->valor = 2;
    //lista->prox->prox = malloc(sizeof(struct lligada));
    //lista->prox->prox->valor = 3;
    //lista->prox->prox->prox = NULL;

    //int len = length(lista);
    //printf("Comprimento da lista: %d\n", len);

    //imprimeL(lista);
    //printf ("\n");

    //printf ("Lista Invertida:\n");
    //LInt rev = reverseL(lista);
    //imprimeL(rev);

    //insertOrd (&lista, 0);
    //imprimeL(lista);

    //removeOneOrd(&lista, 2);
    //imprimeL(lista);


    //LInt lista1 = NULL;
    //LInt lista2 = NULL;
    //
    //insertOrd(&lista1, 1);
    //insertOrd(&lista1, 3);
    //insertOrd(&lista1, 5);
    //
    //insertOrd(&lista2, 2);
    //insertOrd(&lista2, 4);
    //insertOrd(&lista2, 6);
//
    //printf("Lista 1:\n");
    //imprimeL(lista1);
    //printf("Lista 2:\n");
    //imprimeL(lista2);

    //LInt resultado = NULL;
    
    //merge(&resultado, lista1, lista2);

    //printf("Lista resultante:\n");
    //imprimeL(resultado);

    
    // Criação e inicialização da lista ligada l
    LInt l = malloc(sizeof(struct lligada));
    l->valor = 2;
    l->prox = malloc(sizeof(struct lligada));
    l->prox->valor = 2;
    l->prox->prox = malloc(sizeof(struct lligada));
    l->prox->prox->valor = 3;
    l->prox->prox->prox = malloc(sizeof(struct lligada));
    l->prox->prox->prox->valor = 4;
    l->prox->prox->prox->prox = malloc(sizeof(struct lligada));
    l->prox->prox->prox->prox->valor = 5;
    l->prox->prox->prox->prox->prox = NULL;
    printf ("Lista Original:\n");
    imprimeL(l);

    // Ponteiros para as listas menores e maiores ou iguais a x
    //LInt mx = NULL;
    //LInt Mx = NULL;

    //int x = 3;

    //splitQS(l, x, &mx, &Mx);

    //printf("Lista de elementos menores que %d:\n", x);
    //imprimeL(mx);
    //printf("Lista de elementos maiores ou iguais a %d:\n", x);
    //imprimeL(Mx);

    //LInt segunda_metade = parteAmeio(&l);

    //printf("Primeira metade da lista:\n");
    //imprimeL(l);
    //printf("Segunda metade da lista:\n");
    //imprimeL(segunda_metade);

    //printf ("Remove %d vezes o %d:\n", removeAll(&l, 5), 5);
    //imprimeL(l);

    //printf ("Remove %d duplicados:\n", removeDups(&l));
    //imprimeL(l);

    //printf ("Remove %d o maior:\n", removeMaiorL(&l));
    //imprimeL(l);

    //printf ("Retira o último elemento:\n");
    //init(&l);
    //imprimeL (l);

    //printf ("Acrescenta 7 no fim:\n");
    //appendL(&l, 7);
    //imprimeL (l);

    //LInt res;
    //res = cloneL(l);
    //printf ("Copia a lista:\n");
    //imprimeL (res);

    //printf ("Maior elemento: %d\n", maximo(l));

    //take (7, &l);
    //imprimeL(l);

    //drop (7, &l);
    //imprimeL(l);

    LInt inicio = malloc(sizeof(struct lligada));
    inicio->valor = 2;
    inicio->prox = malloc(sizeof(struct lligada));
    inicio->prox->valor = 2;
    inicio->prox->prox = malloc(sizeof(struct lligada));
    inicio->prox->prox->valor = 3;
    inicio->prox->prox->prox = malloc(sizeof(struct lligada));
    inicio->prox->prox->prox->valor = 4;
    inicio->prox->prox->prox->prox = inicio;

    //printf("Lista circular original:\n");

    //int posicao = 2; // Posição para avançar
    //LInt avancado = Nforward(inicio, posicao);

    //printf("O elemento %d posições à frente do início é: %d\n", posicao, avancado->valor);

    //int array[5];
//
    //int elementosPreenchidos = listToArray(l, array, 5);
//
    //printf("Elementos do array preenchidos:\n");
    //for (int i = 0; i < elementosPreenchidos; i++) {
    //    printf("%d ", array[i]);
    //}
    //printf("\n");

    int array[] = {1, 2, 2, 2, 3};
    int tamanhoArray = sizeof(array) / sizeof(array[0]); // Calcula o tamanho do array

    LInt lista = arrayToList(array, tamanhoArray);

    // Imprimindo os elementos da lista
    printf("Elementos da lista construída:\n");
    imprimeL(lista);

    //LInt listaSomasAcumuladas = somasAcL(l);

    //printf("Lista com as somas acumuladas:\n");
    //imprimeL(listaSomasAcumuladas);

    //remreps(lista);

    //printf("Lista sem repetições:\n");
    //imprimeL(lista);

    LInt nova_lista = rotateL(lista);

    printf("Lista após rotação:\n");
    imprimeL(nova_lista);

    // Liberar a memória alocada para l
    freeL(l); freeL(lista);
    free (inicio); //freeL(listaSomasAcumuladas);
    //freeL(segunda_metade);
    //freeL(lista);
    //freeL(resultado);

    // Criando uma árvore binária de teste
    ABin a = malloc(sizeof(struct nodo));
    a->valor = 10;
    a->esq = malloc(sizeof(struct nodo));
    a->esq->valor = 5;
    a->esq->esq = malloc(sizeof(struct nodo));
    a->esq->esq->valor = 2;
    a->esq->esq->esq = NULL;
    a->esq->esq->dir = NULL;
    a->esq->dir = NULL;
    a->dir = malloc(sizeof(struct nodo));
    a->dir->valor = 15;
    a->dir->esq = NULL;
    a->dir->dir = NULL;

    // Testando a função altura
    printf("Altura da árvore: %d\n", altura(a));

    // Testando a função cloneAB
    //ABin b = cloneAB(a);
    //printf("Árvore clonada (pré-ordem): ");
    //printTree(b);
    //printf("\n");

    // Testando a função mirror
    //mirror(&a);
    //printf("Árvore invertida (pré-ordem): ");
    //printTree(a);
    //printf("\n");

    // Testando a função inorder
    LInt l1 = NULL;
    inorder(a, &l1);
    printf("Lista ligada (inorder): ");
    printList(l1);

    // Liberando memória
    //free(a->esq->esq);
    //free(a->esq);
    //free(a->dir);
    //free(a);
    //free(b->esq->esq);
    //free(b->esq);
    //free(b->dir);
    //free(b);
    //while (lista != NULL) {
    //    LInt temp = lista;
    //    lista = lista->prox;
    //    free(temp);
    //}

    return 0;
}