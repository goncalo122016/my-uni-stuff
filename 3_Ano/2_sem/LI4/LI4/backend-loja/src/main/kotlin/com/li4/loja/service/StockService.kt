package com.li4.loja.service

import com.li4.loja.domain.Stock
import com.li4.loja.dto.AjusteStockRequest
import com.li4.loja.dto.EntradaStockRequest
import com.li4.loja.dto.StockResponse
import com.li4.loja.repository.ProdutoRepository
import com.li4.loja.repository.StockRepository
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional

@Service
@Transactional
class StockService(
    private val stockRepository: StockRepository,
    private val produtoRepository: ProdutoRepository
) {
    fun entradaStock(request: EntradaStockRequest): StockResponse {
        val produto = produtoRepository.findById(request.produtoId)
            .orElseThrow { NoSuchElementException("Produto ${request.produtoId} não encontrado") }
        val stock = stockRepository.findByProdutoId(request.produtoId)
            .orElse(Stock(produtoId = produto.id))
        stock.quantidade += request.quantidade
        request.quantidadeMinima?.let { stock.quantidadeMinima = it }
        request.precoVenda?.let { stock.precoVenda = it }
        return StockResponse.from(stockRepository.save(stock), produto.nome)
    }

    fun ajusteStock(request: AjusteStockRequest): StockResponse {
        val produto = produtoRepository.findById(request.produtoId)
            .orElseThrow { NoSuchElementException("Produto ${request.produtoId} não encontrado") }
        val stock = stockRepository.findByProdutoId(request.produtoId)
            .orElse(Stock(produtoId = produto.id))
        stock.quantidade = request.quantidade
        request.quantidadeMinima?.let { stock.quantidadeMinima = it }
        request.precoVenda?.let { stock.precoVenda = it }
        return StockResponse.from(stockRepository.save(stock), produto.nome)
    }

    @Transactional(readOnly = true)
    fun listarStock(apenasAbaixoMinimo: Boolean = false): List<StockResponse> {
        val stocks = stockRepository.findAll()
        val filtrados = if (apenasAbaixoMinimo) stocks.filter { it.quantidade < it.quantidadeMinima } else stocks
        return filtrados.map { s ->
            val nome = produtoRepository.findById(s.produtoId).map { it.nome }.orElse("Produto ${s.produtoId}")
            StockResponse.from(s, nome)
        }
    }

    @Transactional(readOnly = true)
    fun buscarStockPorProduto(produtoId: Long): StockResponse {
        val produto = produtoRepository.findById(produtoId)
            .orElseThrow { NoSuchElementException("Produto $produtoId não encontrado") }
        val stock = stockRepository.findByProdutoId(produtoId)
            .orElseThrow { NoSuchElementException("Stock para produto $produtoId não encontrado") }
        return StockResponse.from(stock, produto.nome)
    }
}
