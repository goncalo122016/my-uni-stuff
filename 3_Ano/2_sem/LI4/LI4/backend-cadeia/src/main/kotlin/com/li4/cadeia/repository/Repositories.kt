package com.li4.cadeia.repository

import com.li4.cadeia.domain.*
import org.springframework.data.jpa.repository.JpaRepository
import java.time.LocalDate
import java.util.Optional

interface CadeiaRepository : JpaRepository<Cadeia, Long>

interface LojaRepository : JpaRepository<Loja, Long>

interface CategoriaRepository : JpaRepository<Categoria, Long>

interface ProdutoRepository : JpaRepository<Produto, Long> {
    fun findByIdentificador(identificador: String): Optional<Produto>
    fun findByCodigoBarras(codigoBarras: String): Optional<Produto>
    fun findByNomeContainingIgnoreCase(nome: String): List<Produto>
}

interface FornecedorRepository : JpaRepository<Fornecedor, Long>

interface FuncionarioRepository : JpaRepository<Funcionario, Long> {
    fun findByNumero(numero: String): Optional<Funcionario>
    fun findByNumeroAndSenha(numero: String, senha: String): Optional<Funcionario>
}

interface RelatorioRepository : JpaRepository<Relatorio, Long> {
    fun findByCadeiaId(cadeiaId: Long): List<Relatorio>
}

interface SincronizacaoRepository : JpaRepository<SincronizacaoLoja, Long> {
    fun findByLojaId(lojaId: Long): List<SincronizacaoLoja>
    fun findByLojaIdAndDataReferenciaBetween(lojaId: Long, inicio: LocalDate, fim: LocalDate): List<SincronizacaoLoja>
    fun findByDataReferenciaBetween(inicio: LocalDate, fim: LocalDate): List<SincronizacaoLoja>
    fun existsByLojaIdAndDataReferencia(lojaId: Long, data: LocalDate): Boolean
}
