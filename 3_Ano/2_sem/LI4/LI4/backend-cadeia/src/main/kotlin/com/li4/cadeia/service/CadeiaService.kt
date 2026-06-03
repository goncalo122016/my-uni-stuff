package com.li4.cadeia.service

import com.li4.cadeia.domain.Cadeia
import com.li4.cadeia.dto.CadeiaResponse
import com.li4.cadeia.repository.CadeiaRepository
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional

@Service
@Transactional
class CadeiaService(private val cadeiaRepository: CadeiaRepository) {

    fun criarCadeia(nome: String): Cadeia = cadeiaRepository.save(Cadeia(nome = nome))

    @Transactional(readOnly = true)
    fun listarCadeias(): List<CadeiaResponse> = cadeiaRepository.findAll().map { CadeiaResponse.from(it) }
}
