package com.li4.cadeia.controller

import com.li4.cadeia.dto.CadeiaResponse
import com.li4.cadeia.dto.CriarLojaRequest
import com.li4.cadeia.dto.LojaResponse
import com.li4.cadeia.service.CadeiaService
import com.li4.cadeia.service.LojaService
import jakarta.validation.Valid
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/cadeia")
class CadeiaController(
    private val cadeiaService: CadeiaService,
    private val lojaService: LojaService
) {
    @GetMapping
    fun listarCadeias(): ResponseEntity<List<CadeiaResponse>> = ResponseEntity.ok(cadeiaService.listarCadeias())

    @PostMapping
    fun criarCadeia(@RequestParam nome: String): ResponseEntity<CadeiaResponse> =
        ResponseEntity.status(HttpStatus.CREATED).body(CadeiaResponse.from(cadeiaService.criarCadeia(nome)))

    @PostMapping("/{id}/lojas")
    fun adicionarLoja(@PathVariable id: Long, @Valid @RequestBody request: CriarLojaRequest): ResponseEntity<LojaResponse> {
        val req = CriarLojaRequest(request.nome, request.rua, request.cidade, request.codigoPostal, request.pais, id)
        return ResponseEntity.status(HttpStatus.CREATED).body(lojaService.adicionarLojaACadeia(req))
    }
}
