package com.li4.loja.repository

import com.li4.loja.domain.*
import org.springframework.data.jpa.repository.JpaRepository
import org.springframework.data.jpa.repository.Query
import java.time.LocalDate
import java.time.LocalDateTime
import java.util.Optional

interface ProdutoRepository : JpaRepository<Produto, Long> {
    fun findByIdentificador(identificador: String): Optional<Produto>
    fun findByCodigoBarras(codigoBarras: String): Optional<Produto>
    fun findByNomeContainingIgnoreCase(nome: String): List<Produto>
}

interface CategoriaRepository : JpaRepository<Categoria, Long>

interface StockRepository : JpaRepository<Stock, Long> {
    fun findByProdutoId(produtoId: Long): Optional<Stock>
    fun findAllByQuantidadeLessThan(limite: Int): List<Stock>
}

interface ClienteRepository : JpaRepository<Cliente, Long>

interface FuncionarioRepository : JpaRepository<Funcionario, Long> {
    fun findByNumero(numero: String): Optional<Funcionario>
    fun findByNumeroAndSenha(numero: String, senha: String): Optional<Funcionario>
    fun findByLojaId(lojaId: Long): List<Funcionario>
}

interface VendaRepository : JpaRepository<Venda, Long> {
    fun findByFuncionarioLojaId(lojaId: Long): List<Venda>

    @Query("SELECT v FROM Venda v WHERE v.funcionario.lojaId = :lojaId AND v.dataHora BETWEEN :inicio AND :fim")
    fun findByLojaIdAndPeriodo(lojaId: Long, inicio: LocalDateTime, fim: LocalDateTime): List<Venda>
}

interface FaturaRepository : JpaRepository<Fatura, Long>

interface DevolucaoRepository : JpaRepository<Devolucao, Long> {
    fun findByVendaId(vendaId: Long): List<Devolucao>
}

interface HistoricoVendasRepository : JpaRepository<HistoricoDeVendas, Long> {
    fun findByLojaIdAndPeriodoBetween(lojaId: Long, inicio: LocalDate, fim: LocalDate): List<HistoricoDeVendas>
    fun findByLojaIdAndPeriodo(lojaId: Long, periodo: LocalDate): Optional<HistoricoDeVendas>
}
