package com.li4.cadeia.controller

import com.li4.cadeia.dto.CriarProdutoRequest
import com.li4.cadeia.dto.EditarProdutoRequest
import com.li4.cadeia.dto.ProdutoResponse
import com.li4.cadeia.service.ProdutoService
import jakarta.validation.Valid
import org.springframework.http.HttpStatus
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

    @PostMapping
    fun criarProduto(@Valid @RequestBody request: CriarProdutoRequest): ResponseEntity<ProdutoResponse> =
        ResponseEntity.status(HttpStatus.CREATED).body(produtoService.criarProduto(request))

    @PutMapping("/{id}")
    fun editarProduto(@PathVariable id: Long, @Valid @RequestBody request: EditarProdutoRequest): ResponseEntity<ProdutoResponse> =
        ResponseEntity.ok(produtoService.editarProduto(id, request))
}
