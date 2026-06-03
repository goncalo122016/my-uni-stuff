package com.li4.cadeia.controller

import com.li4.cadeia.dto.*
import com.li4.cadeia.service.FuncionarioService
import jakarta.validation.Valid
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/utilizadores")
class FuncionarioController(private val funcionarioService: FuncionarioService) {

    @PostMapping
    fun criarUtilizador(@Valid @RequestBody request: CriarFuncionarioRequest): ResponseEntity<FuncionarioResponse> =
        ResponseEntity.status(HttpStatus.CREATED).body(funcionarioService.criarUtilizador(request))

    @PutMapping("/{id}")
    fun editarUtilizador(@PathVariable id: Long, @Valid @RequestBody request: EditarFuncionarioRequest): ResponseEntity<FuncionarioResponse> =
        ResponseEntity.ok(funcionarioService.editarUtilizador(id, request))

    @PatchMapping("/{id}/loja")
    fun associarLoja(@PathVariable id: Long, @Valid @RequestBody request: AssociarLojaRequest): ResponseEntity<FuncionarioResponse> =
        ResponseEntity.ok(funcionarioService.associarLoja(id, request))

    @GetMapping
    fun listarTodos(): ResponseEntity<List<FuncionarioResponse>> = ResponseEntity.ok(funcionarioService.listarTodos())

    @GetMapping("/{id}")
    fun buscarPorId(@PathVariable id: Long): ResponseEntity<FuncionarioResponse> =
        ResponseEntity.ok(funcionarioService.buscarPorId(id))
}
