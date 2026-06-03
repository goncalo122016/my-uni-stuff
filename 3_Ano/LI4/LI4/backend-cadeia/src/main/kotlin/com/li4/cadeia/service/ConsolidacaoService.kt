package com.li4.cadeia.service

import com.li4.cadeia.domain.Relatorio
import com.li4.cadeia.domain.SincronizacaoLoja
import com.li4.cadeia.dto.*
import com.li4.utils.SyncPayload
import com.li4.cadeia.repository.CadeiaRepository
import com.li4.cadeia.repository.RelatorioRepository
import com.li4.cadeia.repository.SincronizacaoRepository
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional
import java.time.LocalDate
import java.time.LocalDateTime

@Service
@Transactional
class ConsolidacaoService(
    private val cadeiaRepository: CadeiaRepository,
    private val sincronizacaoRepository: SincronizacaoRepository,
    private val relatorioRepository: RelatorioRepository
) {
    fun receberSync(payload: SyncPayload) {
        if (sincronizacaoRepository.existsByLojaIdAndDataReferencia(payload.lojaId, payload.dataReferencia)) return

        sincronizacaoRepository.save(
            SincronizacaoLoja(
                lojaId = payload.lojaId,
                dataReferencia = payload.dataReferencia,
                totalVendas = payload.totalVendas,
                faturacaoTotal = payload.faturacaoTotal,
                totalDevolucoes = payload.totalDevolucoes,
                timestamp = LocalDateTime.now()
            )
        )
    }

    @Transactional(readOnly = true)
    fun consolidarEstatisticasCadeia(cadeiaId: Long, inicio: LocalDate, fim: LocalDate): EstatisticasCadeiaResponse {
        val cadeia = cadeiaRepository.findById(cadeiaId)
            .orElseThrow { NoSuchElementException("Cadeia $cadeiaId não encontrada") }

        val porLoja = cadeia.lojas.map { loja ->
            val sincs = sincronizacaoRepository.findByLojaIdAndDataReferenciaBetween(loja.id, inicio, fim)
            EstatisticasLojaResponse(
                lojaId = loja.id, lojaNome = loja.nome,
                totalVendas = sincs.sumOf { it.totalVendas },
                faturacaoTotal = sincs.sumOf { it.faturacaoTotal },
                dataInicio = inicio, dataFim = fim
            )
        }

        return EstatisticasCadeiaResponse(
            cadeiaId = cadeia.id, cadeaNome = cadeia.nome,
            totalVendas = porLoja.sumOf { it.totalVendas },
            faturacaoTotal = porLoja.sumOf { it.faturacaoTotal },
            porLoja = porLoja
        )
    }

    fun gerarRelatorio(cadeiaId: Long, inicio: LocalDate, fim: LocalDate): RelatorioResponse {
        val cadeia = cadeiaRepository.findById(cadeiaId)
            .orElseThrow { NoSuchElementException("Cadeia $cadeiaId não encontrada") }

        val sincs = sincronizacaoRepository.findByDataReferenciaBetween(inicio, fim)
            .filter { s -> cadeia.lojas.any { it.id == s.lojaId } }

        val relatorio = relatorioRepository.save(
            Relatorio(
                dataGeracao = LocalDateTime.now(), periodoInicio = inicio, periodoFim = fim,
                cadeia = cadeia,
                totalVendas = sincs.sumOf { it.totalVendas },
                faturacaoTotal = sincs.sumOf { it.faturacaoTotal },
                totalDevolucoes = sincs.sumOf { it.totalDevolucoes }
            )
        )
        return RelatorioResponse.from(relatorio)
    }

    @Transactional(readOnly = true)
    fun listarRelatorios(cadeiaId: Long): List<RelatorioResponse> {
        cadeiaRepository.findById(cadeiaId).orElseThrow { NoSuchElementException("Cadeia $cadeiaId não encontrada") }
        return relatorioRepository.findByCadeiaId(cadeiaId).map { RelatorioResponse.from(it) }
    }

    @Transactional(readOnly = true)
    fun listarSincronizacoes(lojaId: Long): List<SincronizacaoResponse> =
        sincronizacaoRepository.findByLojaId(lojaId).map { SincronizacaoResponse.from(it) }
}
