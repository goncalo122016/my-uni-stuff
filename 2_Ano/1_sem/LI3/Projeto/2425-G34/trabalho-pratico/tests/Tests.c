#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/resource.h>
#include "../include/parsers/parser_artista.h"
#include "../include/parsers/parser_utilizador.h"
#include "../include/parsers/parser_musica.h"
#include "../include/parsers/parser_albuns.h"
#include "../include/parsers/parser_historico.h"
#include "../include/gestores/gestorUtilizador.h"
#include "../include/gestores/gestor_musica.h"
#include "../include/gestores/gestorArtista.h"
#include "../include/gestores/gestor_albums.h"
#include "../include/gestores/gestor_historico.h"
#include "../include/my_recomendador.h"
#include "../include/Queries.h"
#include "../include/Output.h"
#include "../include/utils.h"

#define MAX_SIZE 1024

double time_q1 = 0.0;
double time_q2 = 0.0;
double time_q3 = 0.0;
double time_q4 = 0.0;
double time_q5 = 0.0;
double time_q6 = 0.0;

int count_lines_in_file(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        printf("Erro ao abrir o arquivo: %s\n", filename);
        return -1;
    }

    int lines = 0;
    char buffer[MAX_SIZE];

    while (fgets(buffer, sizeof(buffer), file) != NULL) {
        lines++;
    }

    fclose(file);
    return lines;
}

gboolean compare_files_detailed(const gchar *expected_file, const gchar *output_file, int *line_of_discrepancy) {
    FILE *file1 = fopen(expected_file, "r");
    FILE *file2 = fopen(output_file, "r");

    if (file1 == NULL || file2 == NULL) {
        if (file1 != NULL) fclose(file1);
        if (file2 != NULL) fclose(file2);
        return FALSE;
    }

    gchar line1[MAX_SIZE], line2[MAX_SIZE];
    gboolean result = TRUE;
    int line_number = 0;

    while (fgets(line1, sizeof(line1), file1) != NULL && fgets(line2, sizeof(line2), file2) != NULL) {
        line_number++;
        line1[strcspn(line1, "\r\n")] = 0;
        line2[strcspn(line2, "\r\n")] = 0;

        if (g_strcmp0(line1, line2) != 0) {
            result = FALSE;
            *line_of_discrepancy = line_number;
            break;
        }
    }

    if (result && (fgets(line1, sizeof(line1), file1) != NULL || fgets(line2, sizeof(line2), file2) != NULL)) {
        result = FALSE;
        *line_of_discrepancy = ++line_number;
    }

    fclose(file1);
    fclose(file2);
    return result;
}

void process_query_line(gchar *line, int line_number, GHashTable *utilizador_table, GHashTable *musica_table, GHashTable *artistas_table, GHashTable *albums_table,  DiscoArtist **discografyArtists, GenreLikes ***gL, gint **matriz, gint *idsUtilizadores, GHashTable **cumulativeRanking, GHashTable *historicos_por_user_table) {
    gchar query_type = line[0];
    gchar *params = &line[2];
    gint min_age = 0;
    gint max_age = 0;
    gint has_S = 0;
    int nr_utilizadores;
    char* idUtilizadorAlvo;

    clock_t start_time, end_time;
    double elapsed_time;

    if(line[1] == 'S') {
        has_S = 1;
    }
    else {
        has_S = 0;
    }

    if (has_S){
        params = &line[3];
    }
    else{
        params = &line[2];
    }

    switch (query_type) {
        case '1': {  // Executa Querie1
            start_time = clock();  // Início do tempo

            gchar *username = g_strdup(params);

            gchar *output = Querie1(utilizador_table,artistas_table,musica_table, username, has_S);

            gchar output_filename[MAX_SIZE];
            snprintf(output_filename, sizeof(output_filename), "resultados/command%d_output.txt", line_number);

            write_output_file(output_filename, output ? output : "");

            if (output != NULL) {
                g_free(output);
            }
            g_free(username);

            end_time = clock();  // Fim do tempo
            elapsed_time = ((double)(end_time - start_time)) / CLOCKS_PER_SEC*1000;
            time_q1 += elapsed_time;  // Atualiza o tempo da query 1

            break;
        }

        case '2': {  // Executa Querie2
            start_time = clock();  // Início do tempo

            gint param1 = 0;
            gchar *param2 = NULL;

            char *token = strtok(params, " ");
            if (token) {
                param1 = atoi(token);  // Primeiro parâmetro é um número inteiro
                token = remove_chars(strtok(NULL, ""), "\"");
                if (token) {
                    param2 = token;  // Segundo parâmetro é uma string (opcional)
                }
            }

            gchar **output = Querie2(discografyArtists, artistas_table, param1, param2, has_S);
            gchar output_filename[MAX_SIZE];
            snprintf(output_filename, sizeof(output_filename), "resultados/command%d_output.txt", line_number);

            if (!output) {
                write_output_file(output_filename, "");  // Escreve vazio se não houver saída
            } else {
                for (int j = 0; j < param1 && output[j]; j++) {
                    write_output_file(output_filename, output[j] ? output[j] : "");
                    g_free(output[j]);  // Libera cada string individual após escrever
                }
                g_free(output);  // Libera o array de strings
            }

            end_time = clock();  // Fim do tempo
            elapsed_time = ((double)(end_time - start_time)) / CLOCKS_PER_SEC*1000;
            time_q2 += elapsed_time;  // Atualiza o tempo da query 2
            // Imprime o tempo da query
            break;
        }

        case '3': {  // Executa Querie3
            start_time = clock();  // Início do tempo

            if (sscanf(params, "%d %d", &min_age, &max_age) != 2) {
            fprintf(stderr, "Erro ao ler parâmetros: linha %d\n", line_number);
            return;
            }

            gchar **output = Querie3(gL, min_age, max_age, has_S);

            gchar output_filename[MAX_SIZE];
            snprintf(output_filename, sizeof(output_filename), "resultados/command%d_output.txt", line_number);

            if (!output) {
            write_output_file(output_filename, "");  // Escreve vazio se não houver saída
            } else {
            for (int j = 0; output[j]; j++) {
                write_output_file(output_filename, output[j] ? output[j] : "");
                g_free(output[j]);  // Libera cada string individual após escrever
            }
            g_free(output);  // Libera o array de strings
            }

            end_time = clock();  // Fim do tempo
            elapsed_time = ((double)(end_time - start_time)) / CLOCKS_PER_SEC*1000;
            time_q3 += elapsed_time;  // Atualiza o tempo da query 3

            break;
        }
        case '4': {
            start_time = clock();  // Início do tempo

            gchar *data_inicial = NULL;
            gchar *data_final = NULL;
            gchar **datas = g_strsplit(params, " ", 2);
            if (g_strv_length(datas) == 2) {
                data_inicial = datas[0];
                data_final = datas[1];
            }

            gchar *output = Querie4(data_inicial, data_final, cumulativeRanking, artistas_table, has_S);
            g_strfreev(datas);
            gchar output_filename[MAX_SIZE];
            snprintf(output_filename, sizeof(output_filename), "resultados/command%d_output.txt", line_number);

            write_output_file(output_filename, output ? output : "");

            if (output) {
                g_free(output);
            }

            end_time = clock();  // Fim do tempo
            elapsed_time = ((double)(end_time - start_time)) / CLOCKS_PER_SEC*1000;
            time_q4 += elapsed_time;
            // Imprime o tempo da query
            break;
        }
        case '5': {
            start_time = clock();  // Início do tempo

            idUtilizadorAlvo = malloc(20 * sizeof(char));
        if (sscanf(params, "%19s %d", idUtilizadorAlvo, &nr_utilizadores) !=
            2) {
          fprintf(stderr, "Erro ao ler parâmetros: linha %d\n", line_number);
          free(idUtilizadorAlvo);
          return;
        }

        gint idUtilizador = g_ascii_strtoll(idUtilizadorAlvo + 1, NULL, 10);

        gchar **output =
            Querie5_myrecomenda(idUtilizador, nr_utilizadores, matriz,
                                utilizador_table, idsUtilizadores);

        gchar output_filename[MAX_SIZE];

        snprintf(output_filename, sizeof(output_filename),
                 "resultados/command%d_output.txt", line_number);

        if (output) {
          for (int j = 0; j < nr_utilizadores && output[j]; j++) {
            write_output_file(output_filename, output[j]);
            write_output_file(output_filename, "\n");
          }
        } else {
          write_output_file(output_filename, "");
        }
        if (output) {
          for (int j = 0; j < nr_utilizadores && output[j]; j++) {
            g_free(output[j]);  // Free each string
          }
          g_free(output);
        }
          free(idUtilizadorAlvo);

            end_time = clock();  // Fim do tempo
            elapsed_time = ((double)(end_time - start_time)) / CLOCKS_PER_SEC*1000;
            time_q5 += elapsed_time;  // Atualiza o tempo da query 5

            break;
        }
        case '6': {
            start_time = clock();  // Início do tempo

        gint *user_id =  malloc(sizeof(gint));
        gchar *year = NULL;
        gint N = 0;

        // Separar os parâmetros do comando
        char *token = strtok(params, " ");
        if (token) {
          *user_id = g_ascii_strtoll(token + 1, NULL, 10); // Primeiro parâmetro: user_id
          token = strtok(NULL, " ");
          if (token) {
            year = g_strdup(token);  // Segundo parâmetro: year
            token = strtok(NULL, " ");
            if (token) {
              N = atoi(token);  // Terceiro parâmetro: N (se presente)
            }
          } else {
            fprintf(stderr, "Erro: Ano não fornecido, linha %d\n", line_number);
          }
        } else {
          fprintf(stderr, "Erro ao processar parâmetros na linha %d\n", line_number);
        }

        gchar **output = NULL;
        gint output_size = 0;
        Querie6(historicos_por_user_table, musica_table, artistas_table, albums_table, user_id, year, N, &output, &output_size, has_S);

            gchar output_filename[MAX_SIZE];
            snprintf(output_filename, sizeof(output_filename), "resultados/command%d_output.txt", line_number);

            if (!output) {
          write_output_file(
              output_filename,
              "");  // Escrever ficheiro vazio se não houver output
            } else {
          for (int j = 0; j < output_size; j++) {
            write_output_file(output_filename, output[j] ? output[j] : "");
            g_free(output[j]);
          }
          g_free(output);
        }

        g_free(user_id);
        g_free(year);


            end_time = clock();  // Fim do tempo
            elapsed_time = ((double)(end_time - start_time)) / CLOCKS_PER_SEC*1000;
            time_q6 += elapsed_time;
            // Imprime o tempo da query
            break;
        }

        default:
            printf("Comando desconhecido: %c\n", query_type);
            break;
    }
}


// Função para liberar recursos
void free_resources(GHashTable *utilizador_table, GHashTable *musica_table, GHashTable *artistas_table, GHashTable *albums_table, GHashTable *historico_table) {
    g_hash_table_destroy(utilizador_table);
    g_hash_table_destroy(musica_table);
    g_hash_table_destroy(artistas_table);
    g_hash_table_destroy(albums_table);
    g_hash_table_destroy(historico_table);
}

// Função para processar as queries a partir do arquivo
void process_queries_from_file_teste(const gchar *input_filename, const gchar *utilizadores_filename, const gchar *musica_filename, const gchar *artistas_filename, const gchar *albuns_filename, const gchar *historico_filename) {
    GHashTable *artistas_table = build_artist_hash(artistas_filename);
    if (artistas_table == NULL) {
        printf("Erro ao construir a tabela de artistas.\n");
        return;
    }

    GHashTable *albums_table = build_albums_hash(albuns_filename, artistas_table);
    if (albums_table == NULL) {
        printf("Erro ao construir a tabela de albuns.\n");
        return;
    }

    GHashTable *musica_table = build_musica_hash_table(musica_filename, artistas_table, albums_table);
    if (musica_table == NULL) {
        printf("Erro ao construir a tabela de musicas.\n");
        return;
    }
    
GenreLikes ***gL = createGenreLikesTotal();
  GHashTable *user_music_table = g_hash_table_new_full(
      g_int_hash, g_int_equal, free, (GDestroyNotify)destroy_user_musicas);

  GHashTable *utilizador_table =
      build_utilizador_hash_table(utilizadores_filename, musica_table, gL);
  if (utilizador_table == NULL) {
    printf("Erro ao construir a tabela de utilizadores.\n");
    return;
  }

  GHashTable *historicos_por_user_table = g_hash_table_new_full(
      g_int_hash, g_int_equal, free, (GDestroyNotify)g_ptr_array_unref);

  GHashTable *historico_table = build_historico_hash_table(
      historico_filename, utilizador_table, musica_table, user_music_table,
      historicos_por_user_table, artistas_table);
  if (historico_table == NULL) {
    printf("Erro ao construir a tabela de historico.\n");
  }

    gint **matriz = makeMatrix(utilizador_table, 10);

    const char *nomesGeneros[10] = {
      "Rock",   "Pop",   "Country", "Jazz",      "Blues",
      "Reggae", "Metal", "Hip Hop", "Classical", "Electronic"};

    int tamanho_hash_utilizador = g_hash_table_size(utilizador_table);

    gint *idsUtilizadores = malloc((tamanho_hash_utilizador + 1) * sizeof(gint));
    if (!idsUtilizadores) {
        printf("[ERRO] Falha ao alocar memória para idsUtilizadores\n");
    }

    char *nomesGenerosPtr[10];
    for (int i = 0; i < 10; i++) {
        nomesGenerosPtr[i] = (char *)nomesGeneros[i]; // Garantir compatibilidade com a função
    }

    fillMatrixWithUserLikes(user_music_table, musica_table, utilizador_table,
                             matriz, idsUtilizadores, tamanho_hash_utilizador,
                              nomesGenerosPtr, 10);

    FILE *file = fopen(input_filename, "r");
    if (file == NULL) {
        printf("Erro: Não foi possível abrir o ficheiro %s.\n", input_filename);
        free_resources(utilizador_table, musica_table, artistas_table, albums_table, historico_table);
        return;
    }

    gchar line[MAX_SIZE];
      DiscoArtist **discografyArtists =
      makeDiscografiaArtistas(artistas_table);

  DiscoArtist ***weeklyTop = makeWeeklyTop(artistas_table);
  GHashTable **cumulativeRanking = computeCumulativeRankings(weeklyTop);

    for (int i = 1; fgets(line, sizeof(line), file) != NULL; i++) {
        line[strcspn(line, "\n")] = 0;
        process_query_line(line, i, utilizador_table, musica_table, artistas_table, albums_table, discografyArtists, gL, matriz, idsUtilizadores, cumulativeRanking, historicos_por_user_table);
    }

    fclose(file);
    free_resources(utilizador_table, musica_table, artistas_table, albums_table, historico_table);
}

char determine_query_type(const gchar *input_line) {
    if (input_line[0] == '1') return '1';
    if (input_line[0] == '2') return '2';
    if (input_line[0] == '3') return '3';
    if (input_line[0] == '4') return '4';
    if (input_line[0] == '5') return '5';
    if (input_line[0] == '6') return '6';
    return '0';
}




int main(int argc, char *argv[]) {
    if (argc != 4) {
        printf("Wrong number of arguments given.\n");
        printf("Usage: %s <dataset_directory> <commands_file_path> <expected_output_directory>\n", argv[0]);
        return -1;
    }


    int total_tests_q1 = 0, correct_tests_q1 = 0;
    int total_tests_q2 = 0, correct_tests_q2 = 0;
    int total_tests_q3 = 0, correct_tests_q3 = 0;
    int total_tests_q4 = 0, correct_tests_q4 = 0;
    int total_tests_q5 = 0, correct_tests_q5 = 0;
    int total_tests_q6 = 0, correct_tests_q6 = 0;


    GString *discrepancies_q1 = g_string_new("");
    GString *discrepancies_q2 = g_string_new("");
    GString *discrepancies_q3 = g_string_new("");
    GString *discrepancies_q4 = g_string_new("");
    GString *discrepancies_q5 = g_string_new("");
    GString *discrepancies_q6 = g_string_new("");

    clock_t start_time_total = clock();

    char *input_filename = argv[2];
    char *dataset_directory = argv[1];
    char *expected_output_directory = argv[3];

    gchar *utilizadores_filename = g_strdup_printf("%s/users.csv", dataset_directory);
    gchar *musica_filename = g_strdup_printf("%s/musics.csv", dataset_directory);
    gchar *artistas_filename = g_strdup_printf("%s/artists.csv", dataset_directory);
    gchar *albuns_filename = g_strdup_printf("%s/albums.csv", dataset_directory);
    gchar *historico_filename = g_strdup_printf("%s/history.csv", dataset_directory);



    process_queries_from_file_teste(input_filename, utilizadores_filename, musica_filename, artistas_filename, albuns_filename, historico_filename);

    clock_t end_time_total = clock();
    double elapsed_time_total = ((double)(end_time_total - start_time_total)) / CLOCKS_PER_SEC;

     int total_queries = count_lines_in_file(input_filename);
    FILE *input_file = fopen(input_filename, "r");

    if (input_file == NULL) {
        printf("Erro ao abrir o arquivo de input.\n");
        return -1;
    }

    for (int i = 1; i <= total_queries; i++) {
        gchar expected_file[MAX_SIZE], output_file[MAX_SIZE], input_line[MAX_SIZE];
        snprintf(expected_file, sizeof(expected_file), "%scommand%d_output.txt", expected_output_directory, i);
        snprintf(output_file, sizeof(output_file), "resultados/command%d_output.txt", i);

        if (fgets(input_line, sizeof(input_line), input_file) == NULL) {
            printf("Erro ao ler a linha %d do arquivo de input.\n", i);
            break;
        }
        input_line[strcspn(input_line, "\r\n")] = 0;

        char query_type = determine_query_type(input_line);
        int line_of_discrepancy = 0;
        gboolean comparison_result;

        comparison_result = compare_files_detailed(expected_file, output_file, &line_of_discrepancy);

        switch (query_type) {
            case '1':
                total_tests_q1++;
                if (comparison_result) {
                    correct_tests_q1++;
                } else {
                    g_string_append_printf(discrepancies_q1, "Descrepância na query %d: linha %d de \"%s\"\n", i, line_of_discrepancy, output_file);
                }
                break;
            case '2':
                total_tests_q2++;
                if (comparison_result) {
                    correct_tests_q2++;
                } else {
                    g_string_append_printf(discrepancies_q2, "Descrepância na query %d: linha %d de \"%s\"\n", i, line_of_discrepancy, output_file);
                }
                break;
            case '3':
                total_tests_q3++;
                if (comparison_result) {
                    correct_tests_q3++;
                } else {
                    g_string_append_printf(discrepancies_q3, "Descrepância na query %d: linha %d de \"%s\"\n", i, line_of_discrepancy, output_file);
                }
                break;
            case '4':
                total_tests_q4++;
                if (comparison_result) {
                    correct_tests_q4++;
                } else {
                    g_string_append_printf(discrepancies_q4, "Descrepância na query %d: linha %d de \"%s\"\n", i, line_of_discrepancy, output_file);
                }
                break;
            case '5':
                total_tests_q5++;
                if (comparison_result) {
                    correct_tests_q5++;
                } else {
                    g_string_append_printf(discrepancies_q5, "Descrepância na query %d: linha %d de \"%s\"\n", i, line_of_discrepancy, output_file);
                }
                break;
            case '6':
                total_tests_q6++;
                if (comparison_result) {
                    correct_tests_q6++;
                } else {
                    g_string_append_printf(discrepancies_q6, "Descrepância na query %d: linha %d de \"%s\"\n", i, line_of_discrepancy, output_file);
                }
                break;  
            default:
                break;
                }
    }

    fclose(input_file);


    // Medir a utilização de memória
    struct rusage usage;
    getrusage(RUSAGE_SELF, &usage);


    printf("Q1: %d de %d testes ok!\n", correct_tests_q1, total_tests_q1);
    if (discrepancies_q1->len > 0) {
        printf("%s", discrepancies_q1->str);
    }

    printf("Q2: %d de %d testes ok!\n", correct_tests_q2, total_tests_q2);
    if (discrepancies_q2->len > 0) {
        printf("%s", discrepancies_q2->str);
    }

    printf("Q3: %d de %d testes ok!\n", correct_tests_q3, total_tests_q3);
    if (discrepancies_q3->len > 0) {
        printf("%s", discrepancies_q3->str);
    }

    printf("Q4: %d de %d testes ok!\n", correct_tests_q4, total_tests_q4);
    if (discrepancies_q4->len > 0) {
        printf("%s", discrepancies_q4->str);
    }

    printf("Q5: %d de %d testes ok!\n", correct_tests_q5, total_tests_q5);
    if (discrepancies_q5->len > 0) {
        printf("%s", discrepancies_q5->str);
    }

    printf("Q6: %d de %d testes ok!\n", correct_tests_q6, total_tests_q6);
    if (discrepancies_q6->len > 0) {
        printf("%s", discrepancies_q6->str);
    }

    g_string_free(discrepancies_q1, TRUE);
    g_string_free(discrepancies_q2, TRUE);
    g_string_free(discrepancies_q3, TRUE);
    g_string_free(discrepancies_q4, TRUE);
    g_string_free(discrepancies_q5, TRUE);
    g_string_free(discrepancies_q6, TRUE);
    printf("\nMemória utilizada: %ld MB\n", (usage.ru_maxrss)/1024);


    printf("\tQ1: %.2f ms\n", time_q1);
    printf("\tQ2: %.2f ms\n", time_q2);
    printf("\tQ3: %.2f ms\n", time_q3);
    printf("\tQ4: %.2f ms\n", time_q4);
    printf("\tQ5: %.2f ms\n", time_q5);
    printf("\tQ6: %.2f ms\n", time_q6);
    printf("Tempo total: %.2f s\n", elapsed_time_total);
}