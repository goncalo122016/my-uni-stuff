package com.li4.loja.controller

import com.li4.loja.dto.ProdutoResponse
import com.li4.loja.service.ProdutoService
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/produtos")
class ProdutoController(private val produtoService: ProdutoService) {

    @GetMapping
    fun listarProdutos(
        @RequestParam(required = false) nome: String?,
        @RequestParam(required = false) codigoBarras: String?
    ): ResponseEntity<List<ProdutoResponse>> = ResponseEntity.ok(produtoService.listarProdutos(nome, codigoBarras))

    @GetMapping("/{id}")
    fun buscarPorId(@PathVariable id: Long): ResponseEntity<ProdutoResponse> =
        ResponseEntity.ok(produtoService.buscarPorId(id))
}
