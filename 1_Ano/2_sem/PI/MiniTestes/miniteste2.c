#include <stdio.h>
#include <math.h>
#include <string.h>

int conta_frequencias(char *sequencia) {
    int contador = 0;
    for (int i = 0; sequencia[i] != '\0'; i++) {
        if (sequencia[i] == 'a' || sequencia[i] == 'g' || sequencia[i] == 's') {
            contador++;
        }
    }
    return contador;
}

char* menorSubstring(char *sequencia) {
    char *menor = sequencia;
    for (int i = 0; sequencia[i+2] != '\0'; i++) {
        if (strncmp(&sequencia[i], menor, 3) < 0) {
            menor = &sequencia[i];
        }
    }
    return menor;
}

int main() {

    //char *sequencia = "4K9fs8dk1QJnqZdStiKYnDx4HJmJWALfgbskJgn1R6htiU13fNno24vQiivnd2ThG6FVrLuMCopn630EhKiUMuXq23LoyX7e36in0ckowjRiuNtJvOb8VyuEtp524u3tO1GWdmQE9c6WPFwt3GCptlNhh9OfletSmGEbxheiaVfYHnO16Pyy1U8BywlvINASTrTaCtzxVqUPubmGi9B4duPZnApeJhNVd2ki3M28po5ySEDfLnXrXV4mBwAzTFkItGfQoZQdwhy0tCWI1HFfB3JGT1WKvL82H4cr3c4J5etGV4Md60wRac9Xv8B6aXfLH2sviwLqtBgXzvKQR1j8wDYJyDfc05EPYy3aPtj65j2psV4XGE8kwmGrEQANBP0T08ynbDGgELylTe7gJDafB7sR1LpJ260d0PVoADveuXJ2LgxqzzBdR39dCx5kjNshYm9uTdln4WRL4lIRn5pSTTUEUB7him6mAmvQR4nOrTsOhRvptsCtz9LHkU57ZNCA55RDGidH3uP7luKg34y0eDOVAh9ryK8zGzHTXEL7bZdtGCxPcmkPjq51gCyPJ7ldk035k2m7LB7YY1feSrW9ELMWNQv6WbrWZUNsNRE8QZITuqCA0W5CVtc9rotjgsoPmqREjsRz5NpVm5PVQLommekgdk52vMYeN9HUcYVGR1ft3nsb8XTWjuCrOIDAdC6rZajuDR5OyzmlJ6UVG2wX9zOshW2X352zK3VcavzNLKEjSo14uojuf7XSbiEaVnkgRfPQp0k6N1XWEChWMidv5n5HtOv1pidFcIf0KVIFP4YIMgMWQZV9rRjI4mE1GkL6Cy7UK8gpkd5Q4m4SfGO2Ddt4VAeC724qCegltC6oOdDUUnndELMyXWrNX6pVEhTIEo1YJIwJrzHHUeKFGJJonbykBzxx8YRrwPdymX6o6MEVoKoC2kYWaeE5KnvtfQ9SKfJ830zefoDlvpt3YTXjgkrKBoB46dtGVhybN1x8pCxQiRD8Lu6KncAqALju5BK9";    
    //printf ("%d\n", somaFrequencias(sequencia));

    char menor [3];
    char *sequencia = "awI6JZkGuIFvectQgWVe5H6G17jZj5jl1sYS00DbYmsOm1yvzGwuykEKBKMNxmpZdwKaff0OnRT73gNAkSuWUBitaOm0Kcc72l4Kd4uVxGJqDGkOQWw1EiIMhkfKDg8WLkhn41BbjeMoKhkkmoxTbW4DB7xdY4ncMf0JVf9lRdV2XGt7iaRNKkN5Pzut7hAl6k7DRGVGcPXRhA0vXGURzya3mqzE8z59Dp8HncDdCooQD1mSxMCYLiGl2oFtwadHmxJYpjeVpgv9pjcokC8VClASCCFhWAEla27WqmUPU6eWvzQNIinHyEWIja4QCbzdZWB7rx3uIqcD2nO2oK2F8Ioq7DYX5nMjVZuwPF5FRqnecbbG4kUZgu2uWVnORhKXDMvGdyYOsf6reBysdgG4PvT2fX2qCqjnVJT7T8fKAmAIKXtBFK3qOANzXqxVEGwt9KXJlQuLBNkjxKYgWJ2lTYfpq37cUecxXLXDwdT22YvHN44xT1sPfPKU8rpZ07Pgw0nAGvtNJ47QFLCFwEyUXSfLtZR2Lkxv4oDbcbm6eMPrALBnTQRdqwhRaRwEX6eILBoEyczCGuGHWpvUERBjAnTbdKrfdiNjMZGgLf9oRN76wgYePhevGI1tPN6nMd6Ye5POKoW2ULKBtBelQ9uK1Pk7Szr4pK7uhM1FLO706BUuxYeAx6rgPR6DyGFjEtnOzRgesd6RWvVEmWriwtsXrqHXh5WbMArt30vRoxl8iVCs9jKY0QT6DX5aGlxxW4TdMbaI3x7du6qivGmg2hRzOJ9lpIUTc57roCHg8ky8sBgX0TMFHIsEZXAyNfAvMfRKH8HHBifTslqU34p0Iu4PVbm8mZX1cidWMRChmdInqUaprJe6gbL8SvQ4VVoJWyY78PJK2BO4POXiwafjMeQPbn6BNdvterdPmujBmReQjxEalIUvDXkIe7wwOGN34UuOufSQFV52GKjsxQECb2REBWpKZ7tfYF49nRYQTse1DxY5yTCaSYf9H0Mcz63TZdcV";
    printf ("%.3s\n", menorSubstring(sequencia));

    //char *str = "dbacdcab";
    //char *resultado = menorSubstring(str);
    //if (resultado != NULL) {
    //    printf("Menor substring lexicograficamente: %.3s\n", resultado);
    //} else {
    //    printf("A string fornecida tem menos de 3 caracteres.\n");
    //}

    return 0;
}