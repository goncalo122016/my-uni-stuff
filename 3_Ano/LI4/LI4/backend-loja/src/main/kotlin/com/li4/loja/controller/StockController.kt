package com.li4.loja.controller

import com.li4.loja.dto.AjusteStockRequest
import com.li4.loja.dto.EntradaStockRequest
import com.li4.loja.dto.StockResponse
import com.li4.loja.service.StockService
import jakarta.validation.Valid
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/stock")
class StockController(private val stockService: StockService) {

    @GetMapping
    fun listarStock(
        @RequestParam(required = false, defaultValue = "false") apenasAbaixoMinimo: Boolean
    ): ResponseEntity<List<StockResponse>> = ResponseEntity.ok(stockService.listarStock(apenasAbaixoMinimo))

    @GetMapping("/produto/{produtoId}")
    fun buscarStockProduto(@PathVariable produtoId: Long): ResponseEntity<StockResponse> =
        ResponseEntity.ok(stockService.buscarStockPorProduto(produtoId))

    @PostMapping("/entrada")
    fun entradaStock(@Valid @RequestBody request: EntradaStockRequest): ResponseEntity<StockResponse> =
        ResponseEntity.ok(stockService.entradaStock(request))

    @PostMapping("/ajuste")
    fun ajusteStock(@Valid @RequestBody request: AjusteStockRequest): ResponseEntity<StockResponse> =
        ResponseEntity.ok(stockService.ajusteStock(request))
}
