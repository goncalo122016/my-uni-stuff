CC := gcc

EXEC := programa-principal
EXEC_INTERATIVO := programa-interativo
EXEC_TEST := programa-testes

FLAGS := -std=c11 -Wall -Wextra -Wdouble-promotion -Werror=pedantic -Werror=vla -pedantic-errors -Wfatal-errors -Ofast -g -ftree-vectorize -freorder-functions -freorder-blocks

LIBS := -lm -lncurses $(shell pkg-config --libs glib-2.0)
PKG_CONFIG := $(shell pkg-config --cflags glib-2.0)
INC := -I include/

# Adicione explicitamente os arquivos que contêm as funções ausentes
EXTRA_SRC := src/Queries.c  # Substitua pelo caminho correto se for diferente

SRC := $(filter-out src/main.c, $(shell find src/ -name "*.c")) $(EXTRA_SRC)
OBJ := $(SRC:src/%.c=build/%.o)

EXTERNAL_OBJ := recomendador-linux-x86_64.o

SRC_INTERATIVO := $(filter-out interativo/main_interativo.c, $(shell find interativo/ -name "*.c")) 
OBJ_INTERATIVO := $(SRC_INTERATIVO:interativo/%.c=build/interativo/%.o)

FLAGS_TEST := $(FLAGS) -Wno-unused-function
SRC_TEST := $(shell find tests/ -name "*.c")
OBJ_TEST := $(filter-out build/main.o, $(OBJ)) $(SRC_TEST:tests/%.c=build/%.o)

BUILD_DIR := $(shell mkdir -p build/entities build/gestores build/parsers build/tests build/interativo)
INCLUDE_FILES := $(shell find include/ -name "*.h")

.PHONY: all
all: build $(EXEC) $(EXEC_INTERATIVO) $(EXEC_TEST)

build:

# Programa Principal
$(EXEC): build/main.o $(OBJ) $(EXTERNAL_OBJ)
	@$(CC) $(FLAGS) $^ ${PKG_CONFIG} $(LIBS) -o $@ ; echo "[Compiling] $@"

build/main.o: src/main.c
	@$(CC) $(FLAGS) -c $< ${PKG_CONFIG} $(LIBS) $(INC) -o $@ ; echo "[Linking] $@"

# Programa Interativo
$(EXEC_INTERATIVO): build/interativo/main_interativo.o $(OBJ_INTERATIVO) $(OBJ) $(EXTERNAL_OBJ)
	@$(CC) $(FLAGS) $^ ${PKG_CONFIG} $(LIBS) -o $@ ; echo "[Compiling] $@"

build/interativo/main_interativo.o: interativo/main_interativo.c
	@$(CC) $(FLAGS) -c $< ${PKG_CONFIG} $(LIBS) $(INC) -o $@ ; echo "[Linking] $@"

$(EXEC_TEST): $(OBJ_TEST) $(EXTERNAL_OBJ)
	@$(CC) $(FLAGS_TEST) $^ ${PKG_CONFIG} $(LIBS) -o $@ ; echo "[Compiling] $@"

# Regras Genéricas
build/%.o: src/%.c
	@mkdir -p $(dir $@)
	@$(CC) $(FLAGS) -c $< ${PKG_CONFIG} $(LIBS) $(INC) -o $@ ; echo "[Linking] $@"

build/interativo/%.o: interativo/%.c
	@mkdir -p $(dir $@)
	@$(CC) $(FLAGS) -c $< ${PKG_CONFIG} $(LIBS) $(INC) -o $@ ; echo "[Linking] $@"

build/%.o: tests/%.c
	@mkdir -p $(dir $@)
	@$(CC) $(FLAGS_TEST) -c $< ${PKG_CONFIG} $(LIBS) $(INC) -o $@ ; echo "[Linking] $@"

.PHONY: clean
clean:
	rm -rf build/*
	rm -rf resultados/*
	rm -f $(EXEC) $(EXEC_INTERATIVO) $(EXEC_TEST)
	rm -rf $(shell find . -type f -executable -exec rm '{}' \;)

.PHONY: format
format:
	clang-format -i -style=file $(shell find src/ -name '*.c' -o -name '*.h') \
	$(shell find include/ -name '*.h') \
	$(shell find tests/ -name '*.c') \
	$(shell find interativo/ -name '*.c' -o -name '*.h')
