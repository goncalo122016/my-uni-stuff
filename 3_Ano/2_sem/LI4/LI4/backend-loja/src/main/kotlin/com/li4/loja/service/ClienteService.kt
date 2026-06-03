package com.li4.loja.service

import com.li4.loja.domain.Cliente
import com.li4.loja.dto.ClienteResponse
import com.li4.loja.dto.CriarClienteRequest
import com.li4.loja.dto.EditarClienteRequest
import com.li4.loja.repository.ClienteRepository
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional

@Service
@Transactional
class ClienteService(private val clienteRepository: ClienteRepository) {

    fun criarPerfil(request: CriarClienteRequest): ClienteResponse {
        val c = Cliente(nome = request.nome, email = request.email, telefone = request.telefone, nif = request.nif)
        return ClienteResponse.from(clienteRepository.save(c))
    }

    fun editarPerfil(id: Long, request: EditarClienteRequest): ClienteResponse {
        val c = clienteRepository.findById(id).orElseThrow { NoSuchElementException("Cliente $id não encontrado") }
        request.nome?.let { c.nome = it }
        request.email?.let { c.email = it }
        request.telefone?.let { c.telefone = it }
        request.nif?.let { c.nif = it }
        return ClienteResponse.from(clienteRepository.save(c))
    }

    @Transactional(readOnly = true)
    fun buscarPorId(id: Long): ClienteResponse =
        clienteRepository.findById(id).map { ClienteResponse.from(it) }
            .orElseThrow { NoSuchElementException("Cliente $id não encontrado") }

    @Transactional(readOnly = true)
    fun listarTodos(): List<ClienteResponse> = clienteRepository.findAll().map { ClienteResponse.from(it) }
}
