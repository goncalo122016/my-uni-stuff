package com.li4.cadeia.service

import com.li4.cadeia.domain.Preco
import com.li4.cadeia.domain.Produto
import com.li4.cadeia.dto.CriarProdutoRequest
import com.li4.cadeia.dto.EditarProdutoRequest
import com.li4.cadeia.dto.ProdutoResponse
import com.li4.cadeia.repository.CategoriaRepository
import com.li4.cadeia.repository.FornecedorRepository
import com.li4.cadeia.repository.ProdutoRepository
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional

@Service
@Transactional
class ProdutoService(
    private val produtoRepository: ProdutoRepository,
    private val categoriaRepository: CategoriaRepository,
    private val fornecedorRepository: FornecedorRepository
) {
    fun criarProduto(request: CriarProdutoRequest): ProdutoResponse {
        if (produtoRepository.findByIdentificador(request.identificador).isPresent)
            throw IllegalArgumentException("Produto com identificador '${request.identificador}' já existe")

        val categorias = categoriaRepository.findAllById(request.categoriaIds)
        if (categorias.size != request.categoriaIds.size)
            throw NoSuchElementException("Uma ou mais categorias não encontradas")

        val fornecedor = fornecedorRepository.findById(request.fornecedorId)
            .orElseThrow { NoSuchElementException("Fornecedor ${request.fornecedorId} não encontrado") }

        val produto = Produto(
            nome = request.nome, descricao = request.descricao, identificador = request.identificador,
            codigoBarras = request.codigoBarras, preco = Preco(valor = request.preco, moeda = request.moeda),
            fornecedor = fornecedor
        )
        produto.categorias = categorias.toMutableList()
        val salvo = produtoRepository.save(produto)
        fornecedor.produtos.add(salvo)
        return ProdutoResponse.from(salvo)
    }

    fun editarProduto(id: Long, request: EditarProdutoRequest): ProdutoResponse {
        val produto = produtoRepository.findById(id)
            .orElseThrow { NoSuchElementException("Produto $id não encontrado") }
        request.nome?.let { produto.nome = it }
        request.descricao?.let { produto.descricao = it }
        request.preco?.let { produto.preco.valor = it }
        if (request.categoriaIds != null) {
            val cats = categoriaRepository.findAllById(request.categoriaIds)
            if (cats.size != request.categoriaIds.size) throw NoSuchElementException("Uma ou mais categorias não encontradas")
            produto.categorias = cats.toMutableList()
        }
        return ProdutoResponse.from(produtoRepository.save(produto))
    }

    @Transactional(readOnly = true)
    fun listarProdutos(nome: String? = null, codigoBarras: String? = null): List<ProdutoResponse> = when {
        codigoBarras != null -> listOfNotNull(produtoRepository.findByCodigoBarras(codigoBarras).orElse(null)).map { ProdutoResponse.from(it) }
        nome != null -> produtoRepository.findByNomeContainingIgnoreCase(nome).map { ProdutoResponse.from(it) }
        else -> produtoRepository.findAll().map { ProdutoResponse.from(it) }
    }

    @Transactional(readOnly = true)
    fun buscarPorId(id: Long): ProdutoResponse =
        produtoRepository.findById(id).map { ProdutoResponse.from(it) }
            .orElseThrow { NoSuchElementException("Produto $id não encontrado") }
}
