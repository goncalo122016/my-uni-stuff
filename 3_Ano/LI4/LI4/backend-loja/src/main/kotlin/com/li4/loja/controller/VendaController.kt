package com.li4.loja.controller

import com.li4.loja.dto.*
import com.li4.loja.service.VendaService
import com.li4.loja.sync.SyncService
import jakarta.validation.Valid
import org.springframework.format.annotation.DateTimeFormat
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*
import java.time.LocalDate

@RestController
@RequestMapping("/api/vendas")
class VendaController(
    private val vendaService: VendaService,
    private val syncService: SyncService
) {
    @PostMapping
    fun registarVenda(@Valid @RequestBody request: RegistarVendaRequest): ResponseEntity<VendaResponse> =
        ResponseEntity.status(HttpStatus.CREATED).body(vendaService.registarVenda(request))

    @PostMapping("/{id}/fatura")
    fun emitirFatura(@PathVariable id: Long, @Valid @RequestBody dados: DadosFaturaRequest): ResponseEntity<FaturaResponse> =
        ResponseEntity.status(HttpStatus.CREATED).body(vendaService.emitirFaturaParaVenda(id, dados))

    @PostMapping("/{id}/devolucao")
    fun registarDevolucao(
        @PathVariable id: Long,
        @RequestBody(required = false) request: RegistarDevolucaoRequest?
    ): ResponseEntity<DevolucaoResponse> =
        ResponseEntity.status(HttpStatus.CREATED).body(vendaService.registarDevolucao(id, request ?: RegistarDevolucaoRequest()))

    @GetMapping("/historico")
    fun consultarHistorico(
        @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) inicio: LocalDate,
        @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) fim: LocalDate,
        @RequestParam(required = false) categoriaId: Long?
    ): ResponseEntity<HistoricoResponse> =
        ResponseEntity.ok(vendaService.consultarHistorico(inicio, fim, categoriaId))

    @PostMapping("/sync")
    fun sincronizarManual(
        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) data: LocalDate?
    ): ResponseEntity<String> {
        syncService.sincronizarDia(data ?: LocalDate.now())
        return ResponseEntity.ok("Sincronização enviada para o backend central")
    }
}
