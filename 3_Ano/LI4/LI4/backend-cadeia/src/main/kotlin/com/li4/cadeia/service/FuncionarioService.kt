package com.li4.cadeia.service

import com.li4.cadeia.domain.Funcionario
import com.li4.cadeia.dto.*
import com.li4.cadeia.repository.FuncionarioRepository
import com.li4.cadeia.repository.LojaRepository
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional

@Service
@Transactional
class FuncionarioService(
    private val funcionarioRepository: FuncionarioRepository,
    private val lojaRepository: LojaRepository
) {
    fun criarUtilizador(request: CriarFuncionarioRequest): FuncionarioResponse {
        if (funcionarioRepository.findByNumero(request.numero).isPresent)
            throw IllegalArgumentException("Número de funcionário '${request.numero}' já existe")
        val loja = lojaRepository.findById(request.lojaId)
            .orElseThrow { NoSuchElementException("Loja ${request.lojaId} não encontrada") }
        val f = Funcionario(numero = request.numero, nome = request.nome, cargo = request.cargo, loja = loja, senha = request.senha)
        return FuncionarioResponse.from(funcionarioRepository.save(f))
    }

    fun editarUtilizador(id: Long, request: EditarFuncionarioRequest): FuncionarioResponse {
        val f = funcionarioRepository.findById(id).orElseThrow { NoSuchElementException("Funcionário $id não encontrado") }
        request.nome?.let { f.nome = it }
        request.cargo?.let { f.cargo = it }
        return FuncionarioResponse.from(funcionarioRepository.save(f))
    }

    fun associarLoja(id: Long, request: AssociarLojaRequest): FuncionarioResponse {
        val f = funcionarioRepository.findById(id).orElseThrow { NoSuchElementException("Funcionário $id não encontrado") }
        val loja = lojaRepository.findById(request.lojaId).orElseThrow { NoSuchElementException("Loja ${request.lojaId} não encontrada") }
        f.loja = loja
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
