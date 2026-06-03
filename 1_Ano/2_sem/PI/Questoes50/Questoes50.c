#include <stdio.h>	
#include <stdlib.h>
#include <string.h>
#include <math.h>

//1

int E1_maior() {
    int n, max = 0;
    do {
        scanf ("%d",&n);
        if (n > max) max = n;
    } while (n != 0);
    printf ("%d\n", max);
}

//2

double E2_media() {
    int n, soma = 0, i=0;
    do {
        scanf ("%d",&n);
        i++;
        soma += n;
    } while (n != 0);
    
    if (soma != 0) printf ("%f\n", (double)soma / (i-1));
    else printf("%f\n", 0.0);
}

//3

int E3_segmaior() {
    int n, max = 0, seg = 0;
    do {
        scanf ("%d",&n);
        if (n > max) {
            seg = max;
            max = n;
        }
        else if (n > seg) seg = n;
    } while (n != 0);
    printf ("%d\n", seg);
}

//4

int bitsUm (unsigned int n) {
    int nbits1 = 0;
    while (n > 0) {
        if (n % 2 == 1) nbits1++;
        n /= 2;
    }
    return nbits1;
}

int bitsUm2 (unsigned int n) {
    int nbits1 = 0;
    for (n; n > 0; n/=2) if (n % 2 == 1) nbits1++;
    return nbits1;
}

//5

int trailingZ (unsigned int n) {
    int cont = 0;
    while (n%2 == 0 && n > 0) {
        cont++;
        n/=2;
    }
    return cont;
}

//6

int qDig1 (unsigned int n) {
    int i;
    if (n<0) n= -n;
    for (i=1; n>=10; i++) n/=10;
    return i;
}

int qDig2 (unsigned int n) {
    int cont = 1;
    if (n<0) n= -n;
    while (n >= 10) {
        cont++;
        n/=10;
    }
    return cont;
}

//7

char *my_strcat (char s1[], char s2[]) {
    int i,j;
    for (i = 0; s1[i]; i++);
    for (j = 0; s2[j]; j++) s1[i++] = s2[j];
    s1[i] = '\0';
    return s1; 
}

//8

char *mystrcpy (char *dest, char source[]) {
    int i;
    for (i = 0; source[i]; i++) dest[i] = source[i];
    dest[i] = '\0';
    return dest;
}

//9

int my_strcmp (char s1[], char s2[]) {
    int i;
    for (i = 0; s1[i] == s2[i] && s1[i] && s2[i]; i++);
    
    if (s1[i] == s2[i]) return 0;
    else if (s1[i] > s2[i]) return 1;
    else return -1;
}

//10

char *my_strstr (char s1[], char s2[]) {
    int i,j;
    for (i = 0; s1[i]; i++) {
        for (j = 0; s2[j] && s1[i + j] == s2[j]; j++);
        if (s2[j] == '\0') return &s1[i];
    }
    return NULL;
}

//11

void my_strrev (char s[]) {
    int len, i;
    char aux [len];
    for (len = 0; s[len]; len++);
    for (i = 0; i<len; i++) aux[i] = s[len - i - 1];
    for (i = 0; i<len; i++) s[i] = aux[i];
}

//12

void my_strnoV (char s[]) {
    int i;
    for (i = 0; s[i]; i++) {
        if (s[i] == 'A' || s[i] == 'a' || s[i] == 'E' || s[i] == 'e' || s[i] == 'I' || s[i] == 'i' || s[i] == 'O' || s[i] == 'o' || s[i] == 'U' || s[i] == 'u') {
            for  (int j = i; s[j]; j++) s[j] = s[j+1];
        }
    }
}

//13

void my_truncW (char t[], int n) {
    int i, j;
    for (i = 0; t[i]; i++) {
        if (t[i] != ' ') {
            int len = 0;
            for (j = i; t[j] && t[j] != ' '; j++) len++;
            if (len > n) {
                for (j = i + n; t[j]; j++) t[j] = t[j + len - n];
            }
        }
    }
}

//14

char charMaisfreq (char s[]) {
    int freq = 0, maisfreq = 0, i, j;
    char chr = s[0];
    for (i = 0; s[i]; i++) {
        for (j = 0; s[j]; j++) if (s[j] == s[i]) freq++;
        if (freq > maisfreq) {
            maisfreq = freq;
            chr = s[i];
        }
        freq = 0;
    }
    if (chr != '\0') return chr;
    else return 0;
}

//15

int iguaisConsecutivos (char s[]) {
    int cont = 1, max_cont = 0;
    for (int i = 1; s[i]; i++) { 
        if (s[i] == s[i-1]) cont++;
        else {
            if (cont > max_cont) max_cont = cont;
            cont = 1;
        }
    }
    if (cont > max_cont) max_cont = cont;
    return max_cont; 
}

//16

int difConsecutivos (char s[]) {
    int max_cont = 0, cont = 1;
    for (int i = 1; s[i]; i++) {
        if (s[i] != s[i - 1]) cont++;
        else {
            if (cont > max_cont) max_cont = cont;
            cont = 1;
        }
    }
    if (cont > max_cont) max_cont = cont;
    return max_cont;
}

//17

int maiorPrefixo (char s1 [], char s2 []) {
    int len;
    for (len = 0; s1[len] && s2[len] && (s1[len] ==  s2[len]); len++);
    return len;
}

//18

int maiorSufixo (char s1 [], char s2 []) {
    int len1, len2, res = 0;
    for (len1 = 0; s1[len1]; len1++);
    for (len2 = 0; s2[len2]; len2++);
    for (len1, len2; len1>=0 && len2>=0 && s1[len1 - 1] == s2[len2 - 1]; len1--, len2--) res++;
    return res;
}

//19

int sufPref (char s1[], char s2[]) {
    int i, j = 0, res = 0;
    for (i = 0; s1[i]; i++) {
        if (s2[j] == s1 [i]) res++;
        else res = j = 0;
        j++;
    }
    return res;
}

//20

int contaPal (char s[]) {
    int cont = 0, inPalavra = 0;
    for (int i = 0; s[i]; i++) {
        if (s[i] == ' ' || s[i] == '\n') {
            if (inPalavra) cont++;
            inPalavra = 0;
        }
        else inPalavra = 1;
    }
    if(inPalavra) cont++;
    return cont;
}

//21

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

//22

int contida (char a[], char b[]) {
    int i, j;
    for (i = 0; a[i]; i++) {
        int contida = 0;
        for (j = 0; b[j]; j++) if (a[i] == b[j]) contida = 1;
        if (!contida) return 0;
    }
    return 1;
}

//23

int palindorome (char s[]) {
    int i, len;
    for (len = 0; s[len]; len++);
    for (i = 0; s[i]; i++) {
        if (s[i] != s[len - 1 - i]) return 0;
    }
    return 1;
}

//24

int remRep(char x[]) {
    if (x[0] == '\0') return 0;

    int j = 0;
    for (int i = 1; x[i]; i++) {
        if (x[i] != x[j]) {
            j++;
            x[j] = x[i]; 
        }
    }
    x[j + 1] = '\0';
    return j + 1; 
}

//25

int limpaEspacos(char t[]) {
    int i, espaco = 0;
    for (i = 0; t[i]; i++) {
        if (t[i] == ' ') {
            if (espaco) {
                for (int j = i; t[j]; j++) t[j] = t[j + 1];
                i--;
            } 
            else espaco = 1;
        } 
        else espaco = 0;
    }
    return i;
}

//26

void insere (int v[], int N, int x) {
    int i;
    for (i = N-1; i>=0 && v[i] > x; i--) v[i+1] = v[i];
    v[i+1] = x;
}

//27

void merge (int r [], int a[], int b[], int na, int nb) {
    int i, j, k;
    i = j = 0;
    for (k = 0; i<na && j<nb; k++) {
        if (a[i] < b[j]) r[k]=a[i++];
        else r[k]=b[j++];
    }
    while (i < na) r[k++] = a[i++];
    while (j < nb) r[k++] = b[j++];
}

//28

int crescente (int a[], int i, int j) {
    for (int k = i+1;k<=j;k++) {
        if (a[k-1] > a[k]) return 0;
    }
    return 1;
}

//29

int retiraNeg (int v[], int N) {
    int cont = 0;
    for (int i = 0; i < N; i++) {
        if (v[i] >= 0) v[cont++] = v[i];
    }
    return cont;
}

//30

int menosFreq (int v[], int N) {
    int freq = 1, minFreq = N, ans = v[0], i;
    for(i = 1; i < N; i++) {
        if (v[i] == v[i - 1]) freq++;
        if (v[i] != v[i - 1]) {
            if (freq < minFreq) {
                minFreq = freq;
                ans = v [i-1];
            }
            freq = 1;
        }
    }
    if (freq < minFreq) {
            minFreq = freq;
            ans = v [i-1];
    }
    return ans;
}

//31

int maisFreq (int v[], int N) {
    int freq = 1, maxFreq = 0, ans = v[0];
    for(int i = 1; i < N; i++) {
        if(v[i] == v[i - 1]) freq++;
        if (v[i] != v[i - 1]) {
            if (freq > maxFreq) {
                maxFreq = freq;
                ans = v [i-1];
            }
            freq = 1;
        }
    }
    return ans;
}

//32

int maxCresc (int v[], int N) {
    int cresAtual = 1, cresMax = 1;
    for (int i = 0;  i < N; i++) {
        if(v[i] > v[i-1]) cresAtual++;
        else {
            if(cresAtual > cresMax) cresMax = cresAtual;
            cresAtual = 1;
        } 
    }
    if(cresAtual > cresMax) cresMax = cresAtual;
    return cresMax;
} 

//33

int elimRep (int v[], int n) {
    int i = 1;
    while(i < n) {
        int existe = 0;
        for(int j = 0; j < i; j++) {
            if(v[i] == v[j]) existe = 1;
        }
        if(existe) {
            for(int j = i; j < n; j++) {
                v[j] = v[j + 1];
            }
            n--;
        } 
        else i++;
    }
    return n;
}

//34

int elimRepOrd (int v[], int n) {
    int j = 1; 
    for (int i = 1; i < n; i++) {
        if (v[i] != v[j- 1]) {
            v[j] = v[i];
            j++;
        }
    }
    return j;
}

//35

int comunsOrd (int a[], int na, int b[], int nb) {
    int i = 0, j = 0, res = 0;
    while(i < na && j < nb) {
        if(a[i] == b[j]) {
            res++;
            i++;
            j++;
        }
        else if (a[i] > b[j]) j++;
        else i++;
    }
    return res;
}

//36

int comuns (int a[], int na, int b[], int nb) {
    int res = 0;
    for (int i = 0; i < na; i++) {
        int existe = 0;
        for (int j = 0;  j < nb ; j++) {
            if (a[i] == b[j]) existe = 1;
        }
        if (existe) res++;
    }
    return res;
}

//37

int minInd (int v[], int n)  {
    if (n <= 0) return -1;
    int min_i = 0, i; 
    for (i = 1; i<n; i++) {
        if (v[i] < v[min_i]) min_i = i;
    }
    return min_i;
}

//38

void somasAc (int v[], int Ac [], int N) {
    for (int i = 0; i<N; i++) {
        Ac[i] = 0;
        for (int j = 0; j<=i; j++) Ac[i] += v[j];
    }
}

//39

int triSup (int N, float m [N][N]) {
    for(int i = 0; i < N; i++) {
        for(int j = 0; j < i; j++) {
            if (m[i][j]) return 0;
        }
    }
    return 1;
}

//40

void transposta (int N, float m [N][N]) {
    for (int i = 0; i<N; i++) {
        for (int j = 0; j < i; j++) {
            float aux = m[i][j];
            m[i][j] = m[j][i];
            m[j][i] = aux;
        }
    }
}

//41

void addTo (int N, int M, int a [N][M], int b[N][M]) {
    for (int i = 0; i<N; i++) {
        for (int j = 0; j<M; j++) {
            a[i][j] += b[i][j];
        }
    }
}

//42

int unionSet (int N, int v1[N], int v2[N], int r[N]) {
    int cont = 0;
    for(int i = 0; i < N; i++) {
        r[i] = 0;
        r[i] = v1[i] || v2[i];
        cont += r[i];
    }
    return cont;
}

//43

int intersectSet (int N, int v1[N], int v2[N], int r[N]) {
    int cont = 0;
    for(int i = 0; i < N; i++) {
        r[i] = 0;
        r[i] = v1[i] && v2[i];
        cont += r[i];
    }
    return cont;
}

//44

int intersectMSet (int N, int v1[N], int v2[N], int r[N]) {
    int cont = 0;
    for (int i = 0; i<N; i++) {
        r[i] = 0;
        if (v1[i] < v2[i]) r[i] = v1[i];
        else r[i] = v2[i];
        cont += r[i];
    }
    return cont;
}

//45

int unionMSet (int N, int v1[N], int v2[N], int r[N]) {
    int cont = 0;
    for (int i=0; i<N; i++) {
        r[i] = 0;
        r[i] = v1[i] + v2[i];
        cont += r[i];
    }
    return cont;
}

//46

int cardinalMSet (int N, int v[N]) {
    int card = 0;
    for (int i = 0; i<N; i++) card += v[i];
    return card;
}

//47

typedef enum movimento {Norte, Oeste, Sul, Este} Movimento;
typedef struct posicao {
    int x, y;
} Posicao;

Posicao posFinal (Posicao inicial, Movimento mov[], int N) {
    for (int i = 0; i<N; i++) {
        if (mov[i] == Norte) inicial.y++;
        if (mov[i] == Sul) inicial.y--;
        if (mov[i] == Este) inicial.x++;
        if (mov[i] == Oeste) inicial.x--;
    }
    return inicial;
}

//48

int caminho (Posicao inicial, Posicao final, Movimento mov[], int N) {
    int i;
    for (i = 0; i<N && (inicial.x != final.x); i++) {
        if (inicial.x < final.x) {
            mov[i] = Este;
            inicial.x++;
        }
        else {
            mov[i] = Oeste;
            inicial.x--;
        }
    }
    for (i; i<N && (inicial.y != final.y); i++) {
        if (inicial.y < final.y) {
            mov[i] = Norte;
            inicial.y++;
        }
        else {
            mov[i] = Sul;
            inicial.y--;
        }
    }
    if (inicial.x != final.x || inicial.y != final.y) return -1;
    else return i;
}

//49

int maisCentral (Posicao pos[], int N) {
    int iCentral = 0;
    double distMin = (pos[0].x*pos[0].x + pos[0].y*pos[0].y);
    for (int i = 1; i<N; i++) {
        double dist = (pos[i].x*pos[i].x + pos[i].y*pos[i].y);
        if (dist < distMin){
            distMin = dist;
            iCentral = i;
        }
    }
    return iCentral;
}

//50

int vizinhos (Posicao p, Posicao pos[], int N) {
    int cont = 0;
    for (int i = 0; i<N; i++) {
        if  (pos[i].y == p.y && ((pos[i].x + 1) == p.x || (pos[i].x - 1) == p.x) || 
             pos[i].x == p.x && ((pos[i].y + 1) == p.y || (pos[i].y - 1) == p.y) ) cont++;
    }
    return cont;
}

int main() {
    int result;
    unsigned int num;
    //result = E1_maior();
    //printf ("O maior dos Números inseridos é %d.\n", result);

    //result = E2_media();
    //printf ("A média dos Números inseridos é %f.\n", result);

    //result = E3_segmaior();
    //printf ("O segundo maior dos Números inseridos é %d.\n", result); 

    //scanf ("%u", &num);
    //printf("Quantidade de Bits '1': %d.\n", bits1(num));
    //printf("Bits a 0 no final da representação: %d.\n", trailingZ(num));
    //printf("Dígitos em base 10: %d.\n", qDig(num));

    char str1[50] = "asghaTESTEasjhajs";
    char str2[50] = "asghaTESTEs";
    char s3[100] =  "liberdade, igualdade e fraternidade";
    char s4[100] = "aaab  bba  aa";

    //printf ("%s\n",myStrcat (s1, s2));

    //printf ("%s\n", mystrcpy (s3,s1)); 

    //printf ("%d\n", mystrcmp (s1,s2));

    //printf ("%s\n", mystrstr1 (str1,str2));

    //mystrrev (str1);
    //printf ("%s\n", str1);

    //mystrnoV (str1);
    //printf("%s\n", str1);

    //truncW (s3, 4);
    //printf("%s\n", s3);

    //printf ("%c\n", charMaisfreq (str1));

    //printf("Maior sequência de char iguais: %d\n", iguaisConsecutivos ("aabcccaac"));
    
    //printf("Maior sequência de char diferentes: %d\n", difConsecutivos ("aabcccaac"));

    //printf ("%d\n", maiorPrefixo(str1, str2));
    
    //printf ("%d\n", maiorSufixo(str1, str2));

    //printf ("%d\n", sufPref("batota","totalidade"));

    //fgets (s4, 100, stdin);
    //printf ("%d\n", contaPal(s4));

    //printf ("Número de vogais: %d\n", contaVogais(s4));

    //printf ("%d\n", contida ("braga", "bracara augusta"));
    //printf ("%d\n", contida ("braga", "bracarense"));

    //printf ("%d\n", palindorome ("ana"));

    //printf ("%d\n", remRep (s4));
    //printf ("%s\n", s4);

    //printf ("%d\n", limpaEspacos (s4));
    //printf ("%s\n", s4);

    int v1[10] = {1,3,4,7,8,10,13,14,17,18};
    int v2[10] = {2,3,7,9,11,12,13,14,16,21};
    int v3[20] = {1,1,1,2,3,3,3,3,4};
    int v4[10] = {1, 2, 3, 2, 1, 4, 2, 4, 5, 4};
    int v5[10] = {0};

    //insere (v1, 10, 5);
    //for (int i = 0; i < 10; i++) printf ("%d ", v1[i]);
    //printf ("\n");

    //merge (v3, v1, v2, 10, 10);
    //for (int i = 0; i < 20; i++) printf ("%d ", v3[i]);
    //printf ("\n");

    //printf ("%d\n", crescente(v1,0,9));

    //int n = retiraNeg (v3, 7);
    //printf ("%d\n",n);
    //for (int i = 0; i < n; i++) printf ("%d ", v3[i]);
    //printf ("\n");

    //printf ("%d\n", menosFreq (v3,9));
    //printf ("%d\n", maisFreq (v3,9));

    //printf ("%d\n", maxCresc (v4,10));

    //printf ("%d\n", elimRep (v4, 10));
    //printf ("%d\n", elimRepOrd (v3, 9));
    
    //printf ("%d\n", comunsOrd (v1, 10, v2, 10));
    //printf ("%d\n", comuns (v1, 10, v2, 10));

    //printf ("%d\n", minInd (v2, 10));

    //somasAc (v1,v5, 10);
    //for (int i = 0; i < 10; i++) printf ("%d ", v5[i]);
    //printf ("\n");

    int m1 [4] [4] = {{1, 2, 3, 4},
                        {0, 5, 6, 7},
                        {0, 0, 8, 9},
                        {0, 0, 0, 10}};
    
    int m2 [4] [4] = {{1, 2, 3, 4},
                        {3, 5, 0, 2},
                        {5, 2, 8, 8},
                        {5, 1, 0, 1}};

    //printf ("%d\n", triSup(4,m1));

    //transposta (4, m1);
    //printf("Matriz Transposta:\n");
    //for (int i = 0; i < 4; i++) {
    //    for (int j = 0; j < 4; j++) {
    //        printf("%f ", m1[i][j]);
    //    }
    //    printf("\n");
    //}

    //addTo (4, 4, m1, m2);
    //for (int i = 0; i < 4; i++) {
    //    for (int j = 0; j < 4; j++) {
    //        printf("%d ", m1[i][j]);
    //    }
    //    printf("\n");
    //}

    int u1[8] = {0,1,0,0,1,0,0,1};   // representa {1,4,7}
    int u2[8] = {0,0,1,0,1,1,1,1};  // representa {2,5,6,8}
    int u3[8] = {0};

    int t1[8] = {0,2,0,0,1,0,0,3};
    int t2[8] = {0,1,0,2,1,0,1,1};

    //printf ("%d\n", unionSet(8, u1, u2, u3));
    //for (int i = 0; i<8; i++) printf("%d ",u3[i]);
    //putchar ('\n');

    //printf ("%d\n", intersectSet(8, u1, u2, u3));
    //for (int i = 0; i<8; i++) printf("%d ",u3[i]);
    //putchar ('\n');

    //printf ("%d\n", intersectMSet(8, u1, u2, u3));
    //for (int i = 0; i<8; i++) printf("%d ",u3[i]);
    //putchar ('\n');

    //printf ("%d\n", unionMSet(8, u1, u2, u3));
    //for (int i = 0; i<8; i++) printf("%d ",u3[i]);
    //putchar ('\n');

    //printf ("%d\n", cardinalMSet(8, t1));

    Movimento movs[5] = {Norte, Sul, Norte, Norte, Oeste};
    Movimento outmovs[10];
    Posicao pinicial = {2,1};
    Posicao pfinal = {-3,6};

    //pinicial = posFinal (pinicial, movs, 5);
    //printf("Posição Final: (%d, %d)\n", pinicial.x, pinicial.y);

    /*int tamanho = caminho (pinicial, pfinal, outmovs, 10);
    printf ("%d\n", tamanho);
    for (int i = 0; i < tamanho; i++) {
        switch (outmovs[i]) {
            case Norte:
                printf("Norte ");
                break;
            case Oeste:
                printf("Oeste ");
                break;
            case Sul:
                printf("Sul ");
                break;
            case Este:
                printf("Este ");
                break;
        }
    }
    printf("\n");*/

    //Posicao posicoes[5] = {{3, 4}, {-1, -2}, {0, 5}, {2, 2}, {10, 10}};
    //int ind = maisCentral(posicoes, 5);
    //printf("%d\n", ind);

    Posicao p = {2, 2};
    Posicao posicoes[] = {{1, 2}, {3, 2}, {2, 1}, {2, 3}, {1, 1}, {3, 3}}; 

    int tamanho = sizeof(posicoes) / sizeof(posicoes[0]); 
    int numVizinhos = vizinhos(p, posicoes, tamanho); 
    printf("Número de vizinhos da posição: %d\n", numVizinhos);


    return 0;
}