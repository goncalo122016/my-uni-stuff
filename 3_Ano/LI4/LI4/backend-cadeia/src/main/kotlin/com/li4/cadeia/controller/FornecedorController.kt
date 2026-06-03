package com.li4.cadeia.controller

import com.li4.cadeia.dto.CriarFornecedorRequest
import com.li4.cadeia.dto.FornecedorResponse
import com.li4.cadeia.service.FornecedorService
import jakarta.validation.Valid
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/fornecedores")
class FornecedorController(private val fornecedorService: FornecedorService) {

    @GetMapping
    fun listarFornecedores(): ResponseEntity<List<FornecedorResponse>> = ResponseEntity.ok(fornecedorService.listarFornecedores())

    @GetMapping("/{id}")
    fun buscarPorId(@PathVariable id: Long): ResponseEntity<FornecedorResponse> =
        ResponseEntity.ok(fornecedorService.buscarPorId(id))

    @PostMapping
    fun criarFornecedor(@Valid @RequestBody request: CriarFornecedorRequest): ResponseEntity<FornecedorResponse> =
        ResponseEntity.status(HttpStatus.CREATED).body(fornecedorService.criarFornecedor(request))
}
