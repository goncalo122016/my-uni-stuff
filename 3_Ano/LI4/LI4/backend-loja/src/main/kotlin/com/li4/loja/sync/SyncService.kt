package com.li4.loja.sync

import com.li4.loja.repository.DevolucaoRepository
import com.li4.loja.repository.VendaRepository
import com.li4.utils.SyncPayload
import org.slf4j.LoggerFactory
import org.springframework.beans.factory.annotation.Value
import org.springframework.http.MediaType
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional
import org.springframework.web.client.RestClient
import java.time.LocalDate

@Service
class SyncService(
    private val vendaRepository: VendaRepository,
    private val devolucaoRepository: DevolucaoRepository,
    @Value("\${loja.id}") private val lojaId: Long,
    @Value("\${cadeia.url}") private val cadeiaUrl: String
) {
    private val log = LoggerFactory.getLogger(javaClass)
    private val restClient = RestClient.create()

    @Transactional(readOnly = true)
    fun sincronizarDia(data: LocalDate = LocalDate.now().minusDays(1)) {
        val inicio = data.atStartOfDay()
        val fim = data.atTime(23, 59, 59)

        val vendas = vendaRepository.findByLojaIdAndPeriodo(lojaId, inicio, fim)
        val devolucoes = vendas.flatMap { devolucaoRepository.findByVendaId(it.id) }

        val payload = SyncPayload(
            lojaId = lojaId,
            dataReferencia = data,
            totalVendas = vendas.size,
            faturacaoTotal = vendas.sumOf { it.total },
            totalDevolucoes = devolucoes.size
        )

        try {
            restClient.post()
                .uri("$cadeiaUrl/api/sync/importar")
                .contentType(MediaType.APPLICATION_JSON)
                .body(payload)
                .retrieve()
                .toBodilessEntity()

            log.info("Sync OK: lojaId={}, data={}, vendas={}, faturacao={}", lojaId, data, payload.totalVendas, payload.faturacaoTotal)
        } catch (ex: Exception) {
            log.warn("Sync falhou para {}: {}", data, ex.message)
        }
    }
}
