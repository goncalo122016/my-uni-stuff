package com.li4.loja.service

import com.li4.loja.domain.*
import com.li4.loja.dto.*
import com.li4.loja.repository.*
import org.springframework.beans.factory.annotation.Value
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional
import java.math.BigDecimal
import java.time.LocalDate
import java.time.temporal.ChronoUnit
import java.util.UUID

@Service
@Transactional
class VendaService(
    private val vendaRepository: VendaRepository,
    private val funcionarioRepository: FuncionarioRepository,
    private val clienteRepository: ClienteRepository,
    private val produtoRepository: ProdutoRepository,
    private val stockRepository: StockRepository,
    private val faturaRepository: FaturaRepository,
    private val historicoVendasRepository: HistoricoVendasRepository,
    private val devolucaoRepository: DevolucaoRepository,
    @Value("\${loja.id}") private val lojaId: Long
) {
    fun registarVenda(request: RegistarVendaRequest): VendaResponse {
        val funcionario = funcionarioRepository.findById(request.funcionarioId)
            .orElseThrow { NoSuchElementException("Funcionário ${request.funcionarioId} não encontrado") }

        val cliente = request.clienteId?.let {
            clienteRepository.findById(it).orElseThrow { NoSuchElementException("Cliente $it não encontrado") }
        }

        val historico = obterOuCriarHistorico(lojaId, LocalDate.now())
        val venda = vendaRepository.save(Venda(funcionario = funcionario, cliente = cliente, historico = historico))

        request.linhas.forEach { linhaReq ->
            val produto = produtoRepository.findById(linhaReq.produtoId)
                .orElseThrow { NoSuchElementException("Produto ${linhaReq.produtoId} não encontrado") }
            val stock = stockRepository.findByProdutoId(produto.id)
                .orElseThrow { IllegalStateException("Produto '${produto.nome}' sem stock") }

            if (stock.quantidade < linhaReq.quantidade)
                throw IllegalStateException("Stock insuficiente para '${produto.nome}': disponível ${stock.quantidade}")

            stock.quantidade -= linhaReq.quantidade
            stockRepository.save(stock)
            venda.linhasDeVenda.add(LinhaDeVenda(produto = produto, quantidade = linhaReq.quantidade, precoUnitario = produto.precoUnitario, venda = venda))
        }

        venda.estado = EstadoVenda.CONCLUIDA
        val vendaConcluida = vendaRepository.save(venda)

        if (request.emitirFatura) {
            val dados = request.dadosFatura ?: throw IllegalArgumentException("Dados da fatura obrigatórios quando emitirFatura=true")
            emitirFatura(vendaConcluida, dados)
        }

        return VendaResponse.from(vendaConcluida)
    }

    fun emitirFaturaParaVenda(vendaId: Long, dados: DadosFaturaRequest): FaturaResponse {
        val venda = vendaRepository.findById(vendaId).orElseThrow { NoSuchElementException("Venda $vendaId não encontrada") }
        if (venda.fatura != null) throw IllegalStateException("Venda já tem fatura emitida")
        return FaturaResponse.from(emitirFatura(venda, dados))
    }

    fun registarDevolucao(vendaId: Long, request: RegistarDevolucaoRequest): DevolucaoResponse {
        val venda = vendaRepository.findById(vendaId).orElseThrow { NoSuchElementException("Venda $vendaId não encontrada") }
        if (venda.estado != EstadoVenda.CONCLUIDA) throw IllegalStateException("Só é possível devolver vendas CONCLUIDA")

        val diasDesde = ChronoUnit.DAYS.between(venda.dataHora.toLocalDate(), LocalDate.now())
        if (diasDesde > 7) throw IllegalStateException("Prazo de devolução expirado (máximo 7 dias)")

        val linhasParaDevolver = if (request.linhas.isNullOrEmpty())
            venda.linhasDeVenda.map { LinhaDevolucaoRequest(it.id, it.quantidade) }
        else request.linhas

        val devolucao = devolucaoRepository.save(Devolucao(venda = venda, motivo = request.motivo))

        linhasParaDevolver.forEach { linhaReq ->
            val linhaOriginal = venda.linhasDeVenda.find { it.id == linhaReq.linhaVendaId }
                ?: throw NoSuchElementException("Linha ${linhaReq.linhaVendaId} não encontrada nesta venda")
            val stock = stockRepository.findByProdutoId(linhaOriginal.produto.id)
                .orElseThrow { IllegalStateException("Stock não encontrado para '${linhaOriginal.produto.nome}'") }
            stock.quantidade += linhaReq.quantidade
            stockRepository.save(stock)
            devolucao.linhas.add(LinhaDevolucao(devolucao = devolucao, linhaVenda = linhaOriginal, quantidade = linhaReq.quantidade))
        }

        venda.estado = EstadoVenda.CANCELADA
        vendaRepository.save(venda)
        return DevolucaoResponse.from(devolucaoRepository.save(devolucao))
    }

    @Transactional(readOnly = true)
    fun consultarHistorico(inicio: LocalDate, fim: LocalDate, categoriaId: Long? = null): HistoricoResponse {
        val historicos = historicoVendasRepository.findByLojaIdAndPeriodoBetween(lojaId, inicio, fim)
        val todasVendas = historicos.flatMap { it.vendas }
        val vendasFiltradas = if (categoriaId != null)
            todasVendas.filter { v -> v.linhasDeVenda.any { l -> l.produto.categorias.any { it.id == categoriaId } } }
        else todasVendas

        return HistoricoResponse(
            lojaId = lojaId, periodo = inicio,
            totalVendas = vendasFiltradas.size,
            faturacao = vendasFiltradas.sumOf { it.total },
            vendas = vendasFiltradas.map { VendaResponse.from(it) }
        )
    }

    private fun emitirFatura(venda: Venda, dados: DadosFaturaRequest): Fatura {
        val totalSemIva = venda.total
        val taxaIva = BigDecimal("0.23")
        return faturaRepository.save(
            Fatura(
                numero = "FAT-${UUID.randomUUID().toString().take(8).uppercase()}",
                venda = venda, nomeCliente = dados.nomeCliente, nifCliente = dados.nifCliente,
                moradaCliente = dados.moradaCliente, totalSemIva = totalSemIva, taxaIva = taxaIva,
                totalComIva = totalSemIva.multiply(BigDecimal.ONE.add(taxaIva))
            )
        )
    }

    private fun obterOuCriarHistorico(lojaId: Long, data: LocalDate): HistoricoDeVendas =
        historicoVendasRepository.findByLojaIdAndPeriodo(lojaId, data)
            .orElseGet { historicoVendasRepository.save(HistoricoDeVendas(lojaId = lojaId, periodo = data)) }
}
