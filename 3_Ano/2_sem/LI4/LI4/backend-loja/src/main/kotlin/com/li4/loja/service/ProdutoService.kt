package com.li4.loja.service

import com.li4.loja.dto.ProdutoResponse
import com.li4.loja.repository.ProdutoRepository
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional

@Service
@Transactional(readOnly = true)
class ProdutoService(private val produtoRepository: ProdutoRepository) {

    fun listarProdutos(nome: String?, codigoBarras: String?): List<ProdutoResponse> {
        val prods = when {
            codigoBarras != null -> listOfNotNull(produtoRepository.findByCodigoBarras(codigoBarras).orElse(null))
            nome != null -> produtoRepository.findByNomeContainingIgnoreCase(nome)
            else -> produtoRepository.findAll()
        }
        return prods.map { ProdutoResponse.from(it) }
    }

    fun buscarPorId(id: Long): ProdutoResponse =
        produtoRepository.findById(id).map { ProdutoResponse.from(it) }
            .orElseThrow { NoSuchElementException("Produto $id não encontrado") }
}
