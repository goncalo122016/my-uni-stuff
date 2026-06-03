package com.li4.loja.dto

import com.li4.loja.domain.*
import com.li4.utils.Cargo
import java.math.BigDecimal
import java.time.LocalDate
import java.time.LocalDateTime

data class ClienteResponse(val id: Long, val nome: String, val email: String?, val telefone: String?, val nif: String?) {
    companion object { fun from(c: Cliente) = ClienteResponse(c.id, c.nome, c.email, c.telefone, c.nif) }
}

data class ProdutoResponse(
    val id: Long, val nome: String, val descricao: String?, val identificador: String,
    val codigoBarras: String?, val precoUnitario: BigDecimal,
    val categorias: List<String>, val fornecedorNome: String?
) {
    companion object {
        fun from(p: Produto) = ProdutoResponse(
            p.id, p.nome, p.descricao, p.identificador, p.codigoBarras,
            p.precoUnitario, p.categorias.map { it.nome }, p.fornecedorNome
        )
    }
}

data class StockResponse(
    val produtoId: Long, val produtoNome: String, val quantidade: Int,
    val quantidadeMinima: Int, val abaixoMinimo: Boolean, val precoVenda: BigDecimal?
) {
    companion object {
        fun from(s: Stock, nomeProduto: String) = StockResponse(
            s.produtoId, nomeProduto, s.quantidade, s.quantidadeMinima,
            s.quantidade < s.quantidadeMinima, s.precoVenda
        )
    }
}

data class FuncionarioResponse(val id: Long, val numero: String, val nome: String, val cargo: Cargo, val lojaId: Long) {
    companion object { fun from(f: Funcionario) = FuncionarioResponse(f.id, f.numero, f.nome, f.cargo, f.lojaId) }
}

data class LinhaVendaResponse(
    val produtoId: Long, val produtoNome: String, val quantidade: Int,
    val precoUnitario: BigDecimal, val subtotal: BigDecimal
) {
    companion object {
        fun from(l: LinhaDeVenda) = LinhaVendaResponse(l.produto.id, l.produto.nome, l.quantidade, l.precoUnitario, l.subtotal)
    }
}

data class VendaResponse(
    val id: Long, val dataHora: LocalDateTime, val funcionarioNome: String, val clienteNome: String?,
    val linhas: List<LinhaVendaResponse>, val total: BigDecimal, val estado: EstadoVenda, val faturaNumero: String?
) {
    companion object {
        fun from(v: Venda) = VendaResponse(
            v.id, v.dataHora, v.funcionario.nome, v.cliente?.nome,
            v.linhasDeVenda.map { LinhaVendaResponse.from(it) }, v.total, v.estado, v.fatura?.numero
        )
    }
}

data class FaturaResponse(
    val id: Long, val numero: String, val dataEmissao: LocalDateTime, val nomeCliente: String,
    val nifCliente: String?, val totalSemIva: BigDecimal, val taxaIva: BigDecimal,
    val totalComIva: BigDecimal, val vendaId: Long
) {
    companion object {
        fun from(f: Fatura) = FaturaResponse(
            f.id, f.numero, f.dataEmissao, f.nomeCliente, f.nifCliente,
            f.totalSemIva, f.taxaIva, f.totalComIva, f.venda.id
        )
    }
}

data class LinhaDevolucaoResponse(val produtoId: Long, val produtoNome: String, val quantidade: Int) {
    companion object {
        fun from(l: LinhaDevolucao) = LinhaDevolucaoResponse(l.linhaVenda.produto.id, l.linhaVenda.produto.nome, l.quantidade)
    }
}

data class DevolucaoResponse(
    val id: Long, val vendaId: Long, val dataHora: LocalDateTime,
    val motivo: String?, val linhas: List<LinhaDevolucaoResponse>
) {
    companion object {
        fun from(d: Devolucao) = DevolucaoResponse(d.id, d.venda.id, d.dataHora, d.motivo, d.linhas.map { LinhaDevolucaoResponse.from(it) })
    }
}

data class HistoricoResponse(
    val lojaId: Long, val periodo: LocalDate, val totalVendas: Int,
    val faturacao: BigDecimal, val vendas: List<VendaResponse>
)
