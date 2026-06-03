package com.li4.cadeia.controller

import com.li4.cadeia.service.ConsolidacaoService
import com.li4.utils.SyncPayload
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RestController
@RequestMapping("/api/sync")
class SyncController(private val consolidacaoService: ConsolidacaoService) {

    @PostMapping("/importar")
    fun importar(@RequestBody payload: SyncPayload): ResponseEntity<Void> {
        consolidacaoService.receberSync(payload)
        return ResponseEntity.ok().build()
    }
}
