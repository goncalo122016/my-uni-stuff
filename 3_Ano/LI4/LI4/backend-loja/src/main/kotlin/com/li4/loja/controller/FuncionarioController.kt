package com.li4.loja.controller

import com.li4.loja.dto.CriarFuncionarioRequest
import com.li4.loja.dto.EditarFuncionarioRequest
import com.li4.loja.dto.FuncionarioResponse
import com.li4.loja.service.FuncionarioService
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

    @GetMapping
    fun listarTodos(): ResponseEntity<List<FuncionarioResponse>> = ResponseEntity.ok(funcionarioService.listarTodos())

    @GetMapping("/{id}")
    fun buscarPorId(@PathVariable id: Long): ResponseEntity<FuncionarioResponse> =
        ResponseEntity.ok(funcionarioService.buscarPorId(id))
}
