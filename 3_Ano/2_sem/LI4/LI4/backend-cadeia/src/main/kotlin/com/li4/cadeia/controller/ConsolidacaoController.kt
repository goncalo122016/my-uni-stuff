package com.li4.cadeia.controller

import com.li4.cadeia.dto.EstatisticasCadeiaResponse
import com.li4.cadeia.dto.RelatorioResponse
import com.li4.cadeia.dto.SincronizacaoResponse
import com.li4.cadeia.service.ConsolidacaoService
import org.springframework.format.annotation.DateTimeFormat
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*
import java.time.LocalDate

@RestController
@RequestMapping("/api/central")
class ConsolidacaoController(private val consolidacaoService: ConsolidacaoService) {

    @GetMapping("/estatisticas")
    fun consolidarEstatisticas(
        @RequestParam cadeiaId: Long,
        @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) inicio: LocalDate,
        @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) fim: LocalDate
    ): ResponseEntity<EstatisticasCadeiaResponse> =
        ResponseEntity.ok(consolidacaoService.consolidarEstatisticasCadeia(cadeiaId, inicio, fim))

    @PostMapping("/relatorios")
    fun gerarRelatorio(
        @RequestParam cadeiaId: Long,
        @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) inicio: LocalDate,
        @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) fim: LocalDate
    ): ResponseEntity<RelatorioResponse> =
        ResponseEntity.status(HttpStatus.CREATED).body(consolidacaoService.gerarRelatorio(cadeiaId, inicio, fim))

    @GetMapping("/relatorios")
    fun listarRelatorios(@RequestParam cadeiaId: Long): ResponseEntity<List<RelatorioResponse>> =
        ResponseEntity.ok(consolidacaoService.listarRelatorios(cadeiaId))

    @GetMapping("/sincronizacoes")
    fun listarSincronizacoes(@RequestParam lojaId: Long): ResponseEntity<List<SincronizacaoResponse>> =
        ResponseEntity.ok(consolidacaoService.listarSincronizacoes(lojaId))
}
