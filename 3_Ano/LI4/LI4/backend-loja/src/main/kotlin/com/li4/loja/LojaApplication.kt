package com.li4.loja

import com.li4.loja.domain.*
import com.li4.loja.repository.*
import com.li4.utils.Cargo
import org.springframework.beans.factory.annotation.Value
import org.springframework.boot.CommandLineRunner
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.context.annotation.Bean
import org.springframework.scheduling.annotation.EnableScheduling
import java.math.BigDecimal
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

@SpringBootApplication
@EnableScheduling
class LojaApplication {

    @Bean
    fun dataLoader(
        produtoRepository: ProdutoRepository,
        categoriaRepository: CategoriaRepository,
        stockRepository: StockRepository,
        clienteRepository: ClienteRepository,
        funcionarioRepository: FuncionarioRepository,
        vendaRepository: VendaRepository,
        historicoVendasRepository: HistoricoVendasRepository,
        @Value("\${loja.id}") lojaId: Long
    ): CommandLineRunner = CommandLineRunner {

        val catAlimentar  = categoriaRepository.save(Categoria(nome = "Alimentar",  descricao = "Produtos alimentares"))
        val catBebidas    = categoriaRepository.save(Categoria(nome = "Bebidas",    descricao = "Bebidas em geral"))
        val catLimpeza    = categoriaRepository.save(Categoria(nome = "Limpeza",    descricao = "Produtos de limpeza"))
        val catHigiene    = categoriaRepository.save(Categoria(nome = "Higiene",    descricao = "Higiene pessoal"))
        val catSnacks     = categoriaRepository.save(Categoria(nome = "Snacks",     descricao = "Snacks e aperitivos"))
        val catLaticinios = categoriaRepository.save(Categoria(nome = "Laticínios", descricao = "Leite, queijo e derivados"))
        val catCongelados = categoriaRepository.save(Categoria(nome = "Congelados", descricao = "Produtos congelados"))
        val catPadaria    = categoriaRepository.save(Categoria(nome = "Padaria",    descricao = "Pão e produtos de padaria"))

        fun mkProd(nome: String, sku: String, preco: String, forn: String, vararg cats: Categoria): Produto {
            val p = Produto(nome = nome, identificador = sku, precoUnitario = BigDecimal(preco), fornecedorNome = forn)
            cats.forEach { p.categorias.add(it) }
            return produtoRepository.save(p)
        }

        val prods = listOf(
            mkProd("Água Mineral 1.5L",         "5601000000001", "0.59", "Distribuidora Norte",  catBebidas),
            mkProd("Pão de Forma",              "5601000000002", "1.29", "Distribuidora Norte",  catAlimentar, catPadaria),
            mkProd("Detergente Loiça 500ml",    "5601000000003", "2.49", "Grocer Sul Lda",       catLimpeza),
            mkProd("Leite Meio-gordo 1L",       "5601000000004", "0.89", "Laticínios Nacionais", catLaticinios),
            mkProd("Queijo Flamengo 200g",      "5601000000005", "1.99", "Laticínios Nacionais", catLaticinios),
            mkProd("Manteiga 250g",             "5601000000006", "1.79", "Laticínios Nacionais", catLaticinios),
            mkProd("Iogurte Natural 4x125g",    "5601000000007", "1.49", "Laticínios Nacionais", catLaticinios),
            mkProd("Café Moído 250g",           "5601000000008", "3.49", "Distribuidora Norte",  catAlimentar, catBebidas),
            mkProd("Sumo de Laranja 1L",        "5601000000009", "1.19", "Distribuidora Norte",  catBebidas),
            mkProd("Coca-Cola 1.5L",            "5601000000010", "1.69", "Distribuidora Norte",  catBebidas),
            mkProd("Batatas Fritas 150g",       "5601000000011", "1.39", "SnackWorld Portugal",  catSnacks),
            mkProd("Chocolate Negro 100g",      "5601000000012", "1.59", "SnackWorld Portugal",  catSnacks),
            mkProd("Bolachas Maria 400g",       "5601000000013", "0.99", "SnackWorld Portugal",  catSnacks, catAlimentar),
            mkProd("Shampoo 400ml",             "5601000000014", "3.99", "HigieneMax Lda",       catHigiene),
            mkProd("Gel de Banho 500ml",        "5601000000015", "2.99", "HigieneMax Lda",       catHigiene),
            mkProd("Pasta de Dentes 75ml",      "5601000000016", "2.49", "HigieneMax Lda",       catHigiene),
            mkProd("Papel Higiénico 12un",      "5601000000017", "4.49", "HigieneMax Lda",       catHigiene),
            mkProd("Sabão Líquido 1L",          "5601000000018", "3.29", "Grocer Sul Lda",       catLimpeza),
            mkProd("Pastilhas Lava-Louça 30un", "5601000000019", "5.99", "Grocer Sul Lda",       catLimpeza),
            mkProd("Pizza Margherita",          "5601000000020", "3.99", "Grocer Sul Lda",       catCongelados),
            mkProd("Esparguete 500g",           "5601000000021", "0.99", "Distribuidora Norte",  catAlimentar),
            mkProd("Arroz Carolino 1kg",        "5601000000022", "1.29", "Distribuidora Norte",  catAlimentar),
            mkProd("Azeite Virgem 750ml",       "5601000000023", "4.99", "Distribuidora Norte",  catAlimentar),
            mkProd("Croissant",                 "5601000000024", "0.49", "Grocer Sul Lda",       catPadaria),
            mkProd("Cerveja NP 33cl Pack6",     "5601000000025", "5.49", "SnackWorld Portugal",  catBebidas)
        )

        val qtys    = listOf(200,80,30,150,60,40,100,50,90,120,70,80,90,40,45,60,35,25,30,20,60,70,25,100,40)
        val stockMin = listOf(20,10,5,15,8,5,10,5,10,15,8,10,10,5,5,8,5,3,3,2,8,10,3,20,5)
        prods.forEachIndexed { i, p ->
            stockRepository.save(Stock(produtoId = p.id, quantidade = qtys[i], quantidadeMinima = stockMin[i]))
        }

        val clientes = listOf(
            clienteRepository.save(Cliente(nome = "António Ferreira",  email = "antonio@email.pt",   telefone = "912345678", nif = "123456789")),
            clienteRepository.save(Cliente(nome = "Margarida Santos",  email = "margarida@email.pt", telefone = "913456789", nif = "234567890")),
            clienteRepository.save(Cliente(nome = "Filipe Rodrigues",  email = "filipe@email.pt",    telefone = "914567890", nif = "345678901")),
            clienteRepository.save(Cliente(nome = "Isabel Nunes",      email = "isabel@email.pt",    telefone = "915678901", nif = "456789012"))
        )

        val adm     = funcionarioRepository.save(Funcionario(numero = "ADM001", nome = "Carlos Administrador", cargo = Cargo.ADMINISTRADOR_CENTRAL, lojaId = lojaId, senha = "admin123"))
        val gerente = funcionarioRepository.save(Funcionario(numero = "GER001", nome = "Ana Gerente",          cargo = Cargo.GERENTE,               lojaId = lojaId, senha = "gerente123"))
        val func1   = funcionarioRepository.save(Funcionario(numero = "FUN001", nome = "João Funcionário",     cargo = Cargo.FUNCIONARIO,           lojaId = lojaId, senha = "func123"))
        val func2   = funcionarioRepository.save(Funcionario(numero = "FUN002", nome = "Maria Silva",          cargo = Cargo.FUNCIONARIO,           lojaId = lojaId, senha = "func123"))

        val funcs = listOf(func1, func2, gerente)
        val today = LocalDate.now()

        for (d in 13 downTo 0) {
            val date = today.minusDays(d.toLong())
            val hist = historicoVendasRepository.save(HistoricoDeVendas(lojaId = lojaId, periodo = date))
            for (v in 0 until 3) {
                val funcionario = funcs[v % funcs.size]
                val venda = vendaRepository.save(
                    Venda(
                        funcionario = funcionario, historico = hist,
                        cliente = if ((d + v) % 3 == 0) clientes[(d + v) % clientes.size] else null,
                        estado = EstadoVenda.CONCLUIDA,
                        dataHora = LocalDateTime.of(date, LocalTime.of(9 + v * 2, (d * 7 + v * 13) % 60))
                    )
                )
                val p1 = prods[(d * 3 + v) % prods.size]
                val p2 = prods[(d * 3 + v + 7) % prods.size]
                venda.linhasDeVenda.add(LinhaDeVenda(produto = p1, quantidade = 1 + v % 3, precoUnitario = p1.precoUnitario, venda = venda))
                venda.linhasDeVenda.add(LinhaDeVenda(produto = p2, quantidade = 1 + d % 2, precoUnitario = p2.precoUnitario, venda = venda))
                vendaRepository.save(venda)
            }
        }

        println("\n========================================")
        println("  Backend LOJA iniciado! (porta 8081)")
        println("  LojaId: $lojaId  |  Produtos: 25  |  Funcionários: 4")
        println("  Vendas históricas: ${14 * 3} (últimos 14 dias)")
        println("  Swagger: http://localhost:8081/swagger-ui.html")
        println("  Sync manual: POST /api/vendas/sync")
        println("  Sync auto: todos os dias às 23:55")
        println("========================================\n")
    }
}

fun main(args: Array<String>) {
    runApplication<LojaApplication>(*args)
}
