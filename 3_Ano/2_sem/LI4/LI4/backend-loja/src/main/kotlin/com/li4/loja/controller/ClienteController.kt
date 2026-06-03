package com.li4.loja.controller

import com.li4.loja.dto.ClienteResponse
import com.li4.loja.dto.CriarClienteRequest
import com.li4.loja.dto.EditarClienteRequest
import com.li4.loja.service.ClienteService
import jakarta.validation.Valid
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/clientes")
class ClienteController(private val clienteService: ClienteService) {

    @PostMapping
    fun criarPerfil(@Valid @RequestBody request: CriarClienteRequest): ResponseEntity<ClienteResponse> =
        ResponseEntity.status(HttpStatus.CREATED).body(clienteService.criarPerfil(request))

    @PutMapping("/{id}")
    fun editarPerfil(@PathVariable id: Long, @Valid @RequestBody request: EditarClienteRequest): ResponseEntity<ClienteResponse> =
        ResponseEntity.ok(clienteService.editarPerfil(id, request))

    @GetMapping("/{id}")
    fun buscarPorId(@PathVariable id: Long): ResponseEntity<ClienteResponse> =
        ResponseEntity.ok(clienteService.buscarPorId(id))

    @GetMapping
    fun listarTodos(): ResponseEntity<List<ClienteResponse>> = ResponseEntity.ok(clienteService.listarTodos())
}
