package com.li4.cadeia.dto

import com.li4.cadeia.domain.*
import com.li4.utils.Cargo
import java.math.BigDecimal
import java.time.LocalDate
import java.time.LocalDateTime

data class CadeiaResponse(val id: Long, val nome: String) {
    companion object { fun from(c: Cadeia) = CadeiaResponse(c.id, c.nome) }
}

data class LojaResponse(
    val id: Long, val nome: String, val cidade: String, val cadeiaId: Long, val cadeaNome: String
) {
    companion object {
        fun from(l: Loja) = LojaResponse(l.id, l.nome, l.localizacao.cidade, l.cadeia.id, l.cadeia.nome)
    }
}

data class FuncionarioResponse(
    val id: Long, val numero: String, val nome: String, val cargo: Cargo, val lojaId: Long, val lojaNome: String
) {
    companion object {
        fun from(f: Funcionario) = FuncionarioResponse(f.id, f.numero, f.nome, f.cargo, f.loja.id, f.loja.nome)
    }
}

data class ProdutoResponse(
    val id: Long, val nome: String, val descricao: String?, val identificador: String,
    val codigoBarras: String?, val preco: BigDecimal, val moeda: String,
    val categorias: List<String>, val fornecedorNome: String?
) {
    companion object {
        fun from(p: Produto) = ProdutoResponse(
            p.id, p.nome, p.descricao, p.identificador, p.codigoBarras,
            p.preco.valor, p.preco.moeda, p.categorias.map { it.nome }, p.fornecedor?.nome
        )
    }
}

data class FornecedorResponse(
    val id: Long, val nome: String, val nif: String?, val morada: String?,
    val contacto: String?, val email: String?, val totalProdutos: Int
) {
    companion object {
        fun from(f: Fornecedor) = FornecedorResponse(
            f.id, f.nome, f.nif, f.morada, f.contacto, f.email, f.produtos.size
        )
    }
}

data class RelatorioResponse(
    val id: Long, val dataGeracao: LocalDateTime, val periodoInicio: LocalDate, val periodoFim: LocalDate,
    val cadeiaId: Long?, val lojaId: Long?, val totalVendas: Int,
    val faturacaoTotal: BigDecimal, val totalDevolucoes: Int
) {
    companion object {
        fun from(r: Relatorio) = RelatorioResponse(
            r.id, r.dataGeracao, r.periodoInicio, r.periodoFim,
            r.cadeia?.id, r.loja?.id, r.totalVendas, r.faturacaoTotal, r.totalDevolucoes
        )
    }
}

data class EstatisticasLojaResponse(
    val lojaId: Long, val lojaNome: String, val totalVendas: Int,
    val faturacaoTotal: BigDecimal, val dataInicio: LocalDate? = null, val dataFim: LocalDate? = null
)

data class EstatisticasCadeiaResponse(
    val cadeiaId: Long, val cadeaNome: String, val totalVendas: Int,
    val faturacaoTotal: BigDecimal, val porLoja: List<EstatisticasLojaResponse>
)

data class SincronizacaoResponse(
    val id: Long, val lojaId: Long, val dataReferencia: LocalDate,
    val totalVendas: Int, val faturacaoTotal: BigDecimal, val timestamp: LocalDateTime
) {
    companion object {
        fun from(s: SincronizacaoLoja) = SincronizacaoResponse(
            s.id, s.lojaId, s.dataReferencia, s.totalVendas, s.faturacaoTotal, s.timestamp
        )
    }
}
