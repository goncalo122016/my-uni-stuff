package com.li4.cadeia

import com.li4.cadeia.domain.*
import com.li4.cadeia.repository.*
import com.li4.utils.Cargo
import org.springframework.boot.CommandLineRunner
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.context.annotation.Bean
import java.math.BigDecimal
import java.time.LocalDate

@SpringBootApplication
class CadeiaApplication {

    @Bean
    fun dataLoader(
        cadeiaRepository: CadeiaRepository,
        lojaRepository: LojaRepository,
        funcionarioRepository: FuncionarioRepository,
        categoriaRepository: CategoriaRepository,
        fornecedorRepository: FornecedorRepository,
        produtoRepository: ProdutoRepository,
        sincronizacaoRepository: SincronizacaoRepository
    ): CommandLineRunner = CommandLineRunner {

        val cadeia = cadeiaRepository.save(Cadeia(nome = "Grupo 9 Stores"))

        val loja1 = lojaRepository.save(Loja(nome = "Loja Lisboa Centro",
            localizacao = Localizacao(rua = "Rua da Liberdade, 100", cidade = "Lisboa", codigoPostal = "1250-140"),
            cadeia = cadeia))
        val loja2 = lojaRepository.save(Loja(nome = "Loja Porto",
            localizacao = Localizacao(rua = "Av. da Boavista, 200", cidade = "Porto", codigoPostal = "4100-130"),
            cadeia = cadeia))
        val loja3 = lojaRepository.save(Loja(nome = "Loja Braga",
            localizacao = Localizacao(rua = "Rua do Souto, 55", cidade = "Braga", codigoPostal = "4700-310"),
            cadeia = cadeia))
        val loja4 = lojaRepository.save(Loja(nome = "Loja Coimbra",
            localizacao = Localizacao(rua = "Rua Ferreira Borges, 12", cidade = "Coimbra", codigoPostal = "3000-165"),
            cadeia = cadeia))
        cadeia.lojas.addAll(listOf(loja1, loja2, loja3, loja4))

        fun mkFunc(num: String, nome: String, cargo: Cargo, loja: Loja, senha: String) =
            funcionarioRepository.save(Funcionario(numero = num, nome = nome, cargo = cargo, loja = loja, senha = senha))

        mkFunc("ADM001", "Carlos Administrador", Cargo.ADMINISTRADOR_CENTRAL, loja1, "admin123")
        mkFunc("GER001", "Ana Gerente",           Cargo.GERENTE,               loja1, "gerente123")
        mkFunc("FUN001", "João Funcionário",       Cargo.FUNCIONARIO,           loja1, "func123")
        mkFunc("FUN002", "Maria Silva",            Cargo.FUNCIONARIO,           loja1, "func123")
        mkFunc("GER002", "Pedro Gerente",          Cargo.GERENTE,               loja2, "gerente123")
        mkFunc("FUN003", "Sofia Costa",            Cargo.FUNCIONARIO,           loja2, "func123")
        mkFunc("FUN004", "Tiago Alves",            Cargo.FUNCIONARIO,           loja2, "func123")
        mkFunc("GER003", "Ricardo Mendes",         Cargo.GERENTE,               loja3, "gerente123")
        mkFunc("FUN005", "Beatriz Oliveira",       Cargo.FUNCIONARIO,           loja3, "func123")
        mkFunc("FUN007", "Catarina Lopes",         Cargo.FUNCIONARIO,           loja3, "func123")
        mkFunc("GER004", "Nuno Figueiredo",        Cargo.GERENTE,               loja4, "gerente123")
        mkFunc("FUN006", "Luís Pereira",           Cargo.FUNCIONARIO,           loja4, "func123")
        mkFunc("FUN008", "Sara Monteiro",          Cargo.FUNCIONARIO,           loja4, "func123")

        val catAlimentar  = categoriaRepository.save(Categoria(nome = "Alimentar",  descricao = "Produtos alimentares"))
        val catBebidas    = categoriaRepository.save(Categoria(nome = "Bebidas",    descricao = "Bebidas em geral"))
        val catLimpeza    = categoriaRepository.save(Categoria(nome = "Limpeza",    descricao = "Produtos de limpeza"))
        val catHigiene    = categoriaRepository.save(Categoria(nome = "Higiene",    descricao = "Higiene pessoal"))
        val catSnacks     = categoriaRepository.save(Categoria(nome = "Snacks",     descricao = "Snacks e aperitivos"))
        val catLaticinios = categoriaRepository.save(Categoria(nome = "Laticínios", descricao = "Leite, queijo e derivados"))
        val catCongelados = categoriaRepository.save(Categoria(nome = "Congelados", descricao = "Produtos congelados"))
        val catPadaria    = categoriaRepository.save(Categoria(nome = "Padaria",    descricao = "Pão e produtos de padaria"))

        val forn1 = fornecedorRepository.save(Fornecedor(nome = "Distribuidora Norte",  contacto = "910000001", email = "norte@dist.pt"))
        val forn2 = fornecedorRepository.save(Fornecedor(nome = "Grocer Sul Lda",       contacto = "910000002", email = "sul@grocer.pt"))
        val forn3 = fornecedorRepository.save(Fornecedor(nome = "Laticínios Nacionais", contacto = "910000003", email = "info@laticinios.pt"))
        val forn4 = fornecedorRepository.save(Fornecedor(nome = "SnackWorld Portugal",  contacto = "910000004", email = "geral@snackworld.pt"))
        val forn5 = fornecedorRepository.save(Fornecedor(nome = "HigieneMax Lda",       contacto = "910000005", email = "contacto@higienemax.pt"))

        loja1.fornecedores.addAll(listOf(forn1, forn2, forn3)); lojaRepository.save(loja1)
        loja2.fornecedores.addAll(listOf(forn1, forn4));        lojaRepository.save(loja2)
        loja3.fornecedores.addAll(listOf(forn2, forn5));        lojaRepository.save(loja3)
        loja4.fornecedores.addAll(listOf(forn3, forn4, forn5)); lojaRepository.save(loja4)

        fun mkProd(nome: String, sku: String, preco: String, forn: Fornecedor, vararg cats: Categoria): Produto {
            val p = Produto(nome = nome, identificador = sku, preco = Preco(valor = BigDecimal(preco)), fornecedor = forn)
            cats.forEach { p.categorias.add(it) }
            return produtoRepository.save(p)
        }

        mkProd("Água Mineral 1.5L",         "5601000000001", "0.59", forn1, catBebidas)
        mkProd("Pão de Forma",              "5601000000002", "1.29", forn1, catAlimentar, catPadaria)
        mkProd("Detergente Loiça 500ml",    "5601000000003", "2.49", forn2, catLimpeza)
        mkProd("Leite Meio-gordo 1L",       "5601000000004", "0.89", forn3, catLaticinios)
        mkProd("Queijo Flamengo 200g",      "5601000000005", "1.99", forn3, catLaticinios)
        mkProd("Manteiga 250g",             "5601000000006", "1.79", forn3, catLaticinios)
        mkProd("Iogurte Natural 4x125g",    "5601000000007", "1.49", forn3, catLaticinios)
        mkProd("Café Moído 250g",           "5601000000008", "3.49", forn1, catAlimentar, catBebidas)
        mkProd("Sumo de Laranja 1L",        "5601000000009", "1.19", forn1, catBebidas)
        mkProd("Coca-Cola 1.5L",            "5601000000010", "1.69", forn1, catBebidas)
        mkProd("Batatas Fritas 150g",       "5601000000011", "1.39", forn4, catSnacks)
        mkProd("Chocolate Negro 100g",      "5601000000012", "1.59", forn4, catSnacks)
        mkProd("Bolachas Maria 400g",       "5601000000013", "0.99", forn4, catSnacks, catAlimentar)
        mkProd("Shampoo 400ml",             "5601000000014", "3.99", forn5, catHigiene)
        mkProd("Gel de Banho 500ml",        "5601000000015", "2.99", forn5, catHigiene)
        mkProd("Pasta de Dentes 75ml",      "5601000000016", "2.49", forn5, catHigiene)
        mkProd("Papel Higiénico 12un",      "5601000000017", "4.49", forn5, catHigiene)
        mkProd("Sabão Líquido 1L",          "5601000000018", "3.29", forn2, catLimpeza)
        mkProd("Pastilhas Lava-Louça 30un", "5601000000019", "5.99", forn2, catLimpeza)
        mkProd("Pizza Margherita",          "5601000000020", "3.99", forn2, catCongelados)
        mkProd("Esparguete 500g",           "5601000000021", "0.99", forn1, catAlimentar)
        mkProd("Arroz Carolino 1kg",        "5601000000022", "1.29", forn1, catAlimentar)
        mkProd("Azeite Virgem 750ml",       "5601000000023", "4.99", forn1, catAlimentar)
        mkProd("Croissant",                 "5601000000024", "0.49", forn2, catPadaria)
        mkProd("Cerveja NP 33cl Pack6",     "5601000000025", "5.49", forn4, catBebidas)

        // Historical sync data for all lojas (last 14 days)
        data class LojaConfig(val id: Long, val baseVendas: Int, val baseFaturacao: Double)
        val lojaConfigs = listOf(
            LojaConfig(loja1.id, 8, 62.0),
            LojaConfig(loja2.id, 6, 48.0),
            LojaConfig(loja3.id, 4, 31.0),
            LojaConfig(loja4.id, 3, 22.0),
        )
        val today = LocalDate.now()
        val seed = intArrayOf(3, 7, 1, 5, 9, 2, 8, 4, 6, 0, 7, 3, 5, 1)
        for (d in 13 downTo 0) {
            val date = today.minusDays(d.toLong())
            lojaConfigs.forEach { cfg ->
                val jitter = seed[d] % 3
                sincronizacaoRepository.save(SincronizacaoLoja(
                    lojaId = cfg.id,
                    dataReferencia = date,
                    totalVendas = cfg.baseVendas + jitter,
                    faturacaoTotal = BigDecimal.valueOf(cfg.baseFaturacao + jitter * 4.5),
                    totalDevolucoes = if (jitter == 2) 1 else 0
                ))
            }
        }

        println("\n========================================")
        println("  Backend CADEIA iniciado! (porta 8080)")
        println("  Lojas: 4  |  Produtos: 25  |  Funcionários: 12")
        println("  Swagger: http://localhost:8080/swagger-ui.html")
        println("  Sync endpoint: POST /api/sync/importar")
        println("========================================\n")
    }
}

fun main(args: Array<String>) {
    runApplication<CadeiaApplication>(*args)
}
