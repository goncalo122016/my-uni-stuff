package com.li4.cadeia.controller

import com.li4.cadeia.dto.AssociarFornecedorRequest
import com.li4.cadeia.dto.EstatisticasLojaResponse
import com.li4.cadeia.dto.LojaResponse
import com.li4.cadeia.service.LojaService
import jakarta.validation.Valid
import org.springframework.format.annotation.DateTimeFormat
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*
import java.time.LocalDate

@RestController
@RequestMapping("/api/lojas")
class LojaController(private val lojaService: LojaService) {

    @GetMapping
    fun listarLojas(): ResponseEntity<List<LojaResponse>> = ResponseEntity.ok(lojaService.listarLojas())

    @GetMapping("/{id}")
    fun buscarPorId(@PathVariable id: Long): ResponseEntity<LojaResponse> =
        ResponseEntity.ok(lojaService.buscarPorId(id))

    @PostMapping("/{id}/fornecedores")
    fun associarFornecedor(@PathVariable id: Long, @Valid @RequestBody request: AssociarFornecedorRequest): ResponseEntity<LojaResponse> =
        ResponseEntity.ok(lojaService.associarFornecedor(id, request))

    @GetMapping("/{id}/estatisticas")
    fun consultarEstatisticas(
        @PathVariable id: Long,
        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) dataInicio: LocalDate?,
        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) dataFim: LocalDate?
    ): ResponseEntity<EstatisticasLojaResponse> =
        ResponseEntity.ok(lojaService.consultarEstatisticasLoja(id, dataInicio, dataFim))

    @PostMapping("/{id}/sync")
    fun triggerSync(@PathVariable id: Long): ResponseEntity<Map<String, String>> {
        return try {
            lojaService.triggerSync(id)
            ResponseEntity.ok(mapOf("mensagem" to "Sync disparado com sucesso para a loja $id"))
        } catch (ex: Exception) {
            ResponseEntity.status(503).body(mapOf("mensagem" to "Backend da loja indisponível: ${ex.message}"))
        }
    }
}
