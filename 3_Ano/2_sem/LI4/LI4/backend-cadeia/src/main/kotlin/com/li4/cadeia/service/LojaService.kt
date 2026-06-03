package com.li4.cadeia.service

import com.li4.cadeia.domain.Localizacao
import com.li4.cadeia.domain.Loja
import com.li4.cadeia.dto.*
import com.li4.cadeia.repository.CadeiaRepository
import com.li4.cadeia.repository.FornecedorRepository
import com.li4.cadeia.repository.LojaRepository
import com.li4.cadeia.repository.SincronizacaoRepository
import org.slf4j.LoggerFactory
import org.springframework.beans.factory.annotation.Value
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import org.springframework.web.client.RestClient
import java.time.LocalDate

@Service
@Transactional
class LojaService(
    private val lojaRepository: LojaRepository,
    private val cadeiaRepository: CadeiaRepository,
    private val fornecedorRepository: FornecedorRepository,
    private val sincronizacaoRepository: SincronizacaoRepository,
    @Value("\${loja.url}") private val lojaUrl: String
) {
    private val log = LoggerFactory.getLogger(javaClass)
    private val restClient = RestClient.create()
    fun adicionarLojaACadeia(request: CriarLojaRequest): LojaResponse {
        val cadeia = cadeiaRepository.findById(request.cadeiaId)
            .orElseThrow { NoSuchElementException("Cadeia ${request.cadeiaId} não encontrada") }
        val loja = Loja(
            nome = request.nome,
            localizacao = Localizacao(rua = request.rua, cidade = request.cidade, codigoPostal = request.codigoPostal, pais = request.pais),
            cadeia = cadeia
        )
        val saved = lojaRepository.save(loja)
        cadeia.lojas.add(saved)
        return LojaResponse.from(saved)
    }

    fun associarFornecedor(lojaId: Long, request: AssociarFornecedorRequest): LojaResponse {
        val loja = lojaRepository.findById(lojaId)
            .orElseThrow { NoSuchElementException("Loja $lojaId não encontrada") }
        val fornecedor = fornecedorRepository.findById(request.fornecedorId)
            .orElseThrow { NoSuchElementException("Fornecedor ${request.fornecedorId} não encontrado") }
        if (!loja.fornecedores.any { it.id == fornecedor.id }) loja.fornecedores.add(fornecedor)
        return LojaResponse.from(lojaRepository.save(loja))
    }

    @Transactional(readOnly = true)
    fun consultarEstatisticasLoja(lojaId: Long, dataInicio: LocalDate?, dataFim: LocalDate?): EstatisticasLojaResponse {
        val loja = lojaRepository.findById(lojaId)
            .orElseThrow { NoSuchElementException("Loja $lojaId não encontrada") }
        val sincs = if (dataInicio != null && dataFim != null) {
            sincronizacaoRepository.findByLojaIdAndDataReferenciaBetween(lojaId, dataInicio, dataFim)
        } else {
            sincronizacaoRepository.findByLojaId(lojaId)
        }
        return EstatisticasLojaResponse(
            lojaId = loja.id, lojaNome = loja.nome,
            totalVendas = sincs.sumOf { it.totalVendas },
            faturacaoTotal = sincs.sumOf { it.faturacaoTotal },
            dataInicio = dataInicio, dataFim = dataFim
        )
    }

    @Transactional(propagation = Propagation.NOT_SUPPORTED)
    fun triggerSync(lojaId: Long) {
        val loja = lojaRepository.findById(lojaId)
            .orElseThrow { NoSuchElementException("Loja $lojaId não encontrada") }
        try {
            restClient.post()
                .uri("$lojaUrl/api/vendas/sync")
                .retrieve()
                .toBodilessEntity()
            log.info("Sync manual disparado para loja {} ({})", lojaId, loja.nome)
        } catch (ex: Exception) {
            log.warn("Falha ao disparar sync para loja {}: {}", lojaId, ex.message)
            throw RuntimeException("Não foi possível contactar o backend da loja: ${ex.message}")
        }
    }

    @Transactional(readOnly = true)
    fun listarLojas(): List<LojaResponse> = lojaRepository.findAll().map { LojaResponse.from(it) }

    @Transactional(readOnly = true)
    fun buscarPorId(id: Long): LojaResponse =
        lojaRepository.findById(id).map { LojaResponse.from(it) }
            .orElseThrow { NoSuchElementException("Loja $id não encontrada") }
}
