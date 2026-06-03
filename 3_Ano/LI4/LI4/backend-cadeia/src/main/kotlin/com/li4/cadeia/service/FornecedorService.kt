package com.li4.cadeia.service

import com.li4.cadeia.domain.Fornecedor
import com.li4.cadeia.dto.CriarFornecedorRequest
import com.li4.cadeia.dto.FornecedorResponse
import com.li4.cadeia.repository.FornecedorRepository
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional

@Service
@Transactional
class FornecedorService(private val fornecedorRepository: FornecedorRepository) {

    fun criarFornecedor(request: CriarFornecedorRequest): FornecedorResponse {
        val f = Fornecedor(nome = request.nome, nif = request.nif, morada = request.morada, contacto = request.contacto, email = request.email)
        return FornecedorResponse.from(fornecedorRepository.save(f))
    }

    @Transactional(readOnly = true)
    fun listarFornecedores(): List<FornecedorResponse> = fornecedorRepository.findAll().map { FornecedorResponse.from(it) }

    @Transactional(readOnly = true)
    fun buscarPorId(id: Long): FornecedorResponse =
        fornecedorRepository.findById(id).map { FornecedorResponse.from(it) }
            .orElseThrow { NoSuchElementException("Fornecedor $id não encontrado") }
}
