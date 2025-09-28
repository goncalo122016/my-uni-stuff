#include <glib.h>
#include <stdio.h>
#include <ctype.h>

long calcular_diferenca_segundos(const char *data_hora) {
    struct tm base_tm = {0};
    struct tm entrada_tm = {0};

    base_tm.tm_year = 2021 - 1900;  
    base_tm.tm_mon = 6 - 1;         
    base_tm.tm_mday = 15;               
    base_tm.tm_hour = 12;               
    base_tm.tm_min = 0;            
    base_tm.tm_sec = 0;            

    time_t base_time = mktime(&base_tm);
    if (base_time == -1) {
        fprintf(stderr, "Erro ao converter base de tempo.\n");
        return -1;
    }

    if (sscanf(data_hora, "%4d/%2d/%2d %2d:%2d:%2d",
               &entrada_tm.tm_year,
               &entrada_tm.tm_mon,
               &entrada_tm.tm_mday,
               &entrada_tm.tm_hour,
               &entrada_tm.tm_min,
               &entrada_tm.tm_sec) != 6) {
        fprintf(stderr, "Erro: Formato de data/hora inválido.\n");
        return -1;
    }

    entrada_tm.tm_year -= 1900; 
    entrada_tm.tm_mon -= 1;     

    time_t entrada_time = mktime(&entrada_tm);
    if (entrada_time == -1) {\
        fprintf(stderr, "Erro ao converter data/hora de entrada.\n");
        return -1;
    }

    return difftime(entrada_time, base_time);
}


// Função para remover um conjunto de caracteres de uma string
gchar *remove_chars(const gchar *input, const gchar *chars_to_remove) {
    if (!input || !chars_to_remove) return NULL;  // Verificação de entrada nula

    GString *cleaned = g_string_new("");
    for (const gchar *p = input; *p; p++) {
        if (!strchr(chars_to_remove, *p)) {
            g_string_append_c(cleaned, *p);
        }
    }

    return g_string_free(cleaned, FALSE);  // Retorna a string limpa
}


// Função para converter uma string para um inteiro
gint time_string_to_seconds(const gchar *time_str) {
    if (time_str == NULL) {
        return -1;
    }

    gint hours = 0, minutes = 0, seconds = 0;

    // Utiliza sscanf para extrair as horas, minutos e segundos da string
    if (sscanf(time_str, "%d:%d:%d", &hours, &minutes, &seconds) != 3) {
        printf("Erro: Formato de tempo inválido em '%s'", time_str);
        return -1; 
    }

    // Converte tudo para segundos
    return hours * 3600 + minutes * 60 + seconds;
}

// Função para converter segundos em formato "hh:mm:ss"
gchar* time_seconds_to_hms(gint total_seconds) {
    gint hours = total_seconds / 3600;
    gint minutes = (total_seconds % 3600) / 60;
    gint seconds = total_seconds % 60;

    // Formatar a string "hh:mm:ss"
    gchar *time_str = g_strdup_printf("%02d:%02d:%02d", hours, minutes, seconds);

    return time_str;
}

// Função para obter o índice do maior valor de um array
gint get_max_index(const gint *array, gsize length) {
    if (length == 0) return -1;  // Caso o array esteja vazio

    gint max_value = array[0];
    gint max_index = 0;

    for (gsize i = 1; i < length; i++) {
        if (array[i] > max_value) {
            max_value = array[i];
            max_index = i;
        }
    }

    return max_index;
}

gboolean validate_key_in_hashtable(GHashTable *hashtable, gint key) {
    if (hashtable == NULL) {
        return FALSE;
    }

    if (key == 0) {
        return FALSE;
    }

    if (!g_hash_table_contains(hashtable, &key)) {
        return FALSE;
    }

    return TRUE;
}

void minusculo(gchar s1[], gchar s2[]){
    int i = 0;
    while(s1[i] != '\0'){
        s2[i] = tolower(s1[i]);
        i++;
    }
    s2[i] = '\0'; 
}

// Função para comparar datas no formato "yyyy/mm/dd"
int compare_dates(const char *date1, const char *date2) {
    int year1, month1, day1;
    int year2, month2, day2;

    sscanf(date1, "%d/%d/%d", &year1, &month1, &day1);
    sscanf(date2, "%d/%d/%d", &year2, &month2, &day2);

    // Comparar os anos
    if (year1 > year2) return 1;
    if (year1 < year2) return -1;

    // Se os anos forem iguais, comparar os meses
    if (month1 > month2) return 1;
    if (month1 < month2) return -1;

    // Se os meses também forem iguais, comparar os dias
    if (day1 > day2) return 1;
    if (day1 < day2) return -1;

    // Se dia, mês e ano forem iguais, as datas são iguais
    return 0;
}

int contar_digitos(int numero) {
    int contagem = 0;
    
    // Lidar com o caso em que o número é 0
    if (numero == 0) {
        return 1;
    }

    // Contar os dígitos
    while (numero > 0) {
        contagem++;
        numero /= 10;  // Remove o último dígito
    }

    return contagem;
}

char *createZerosString(int n) {
    if (n <= 0) {
        return NULL; // Retorna NULL se n for inválido
    }

    // Aloca memória para a string (n zeros + 1 para o terminador nulo)
    char *zeros = (char *)malloc((n + 1) * sizeof(char));
    if (!zeros) {
        perror("Erro ao alocar memória");
        return NULL; // Retorna NULL em caso de falha na alocação
    }

    // Preenche a string com '0' e termina com '\0'
    memset(zeros, '0', n);
    
    zeros[n] = '\0';

    return zeros;
}