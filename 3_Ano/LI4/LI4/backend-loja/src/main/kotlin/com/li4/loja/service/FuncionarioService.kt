package com.li4.loja.service

import com.li4.loja.domain.Funcionario
import com.li4.loja.dto.CriarFuncionarioRequest
import com.li4.loja.dto.EditarFuncionarioRequest
import com.li4.loja.dto.FuncionarioResponse
import com.li4.loja.repository.FuncionarioRepository
import org.springframework.beans.factory.annotation.Value
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional

@Service
@Transactional
class FuncionarioService(
    private val funcionarioRepository: FuncionarioRepository,
    @Value("\${loja.id}") private val lojaId: Long
) {
    fun criarUtilizador(request: CriarFuncionarioRequest): FuncionarioResponse {
        if (funcionarioRepository.findByNumero(request.numero).isPresent)
            throw IllegalArgumentException("Número '${request.numero}' já existe")
        val f = Funcionario(numero = request.numero, nome = request.nome, cargo = request.cargo, lojaId = lojaId, senha = request.senha)
        return FuncionarioResponse.from(funcionarioRepository.save(f))
    }

    fun editarUtilizador(id: Long, request: EditarFuncionarioRequest): FuncionarioResponse {
        val f = funcionarioRepository.findById(id).orElseThrow { NoSuchElementException("Funcionário $id não encontrado") }
        request.nome?.let { f.nome = it }
        request.cargo?.let { f.cargo = it }
        return FuncionarioResponse.from(funcionarioRepository.save(f))
    }

    @Transactional(readOnly = true)
    fun listarTodos(): List<FuncionarioResponse> = funcionarioRepository.findAll().map { FuncionarioResponse.from(it) }

    @Transactional(readOnly = true)
    fun buscarPorId(id: Long): FuncionarioResponse =
        funcionarioRepository.findById(id).map { FuncionarioResponse.from(it) }
            .orElseThrow { NoSuchElementException("Funcionário $id não encontrado") }

    @Transactional(readOnly = true)
    fun autenticar(numero: String, senha: String): FuncionarioResponse =
        funcionarioRepository.findByNumeroAndSenha(numero, senha)
            .map { FuncionarioResponse.from(it) }
            .orElseThrow { IllegalArgumentException("Credenciais inválidas") }
}
