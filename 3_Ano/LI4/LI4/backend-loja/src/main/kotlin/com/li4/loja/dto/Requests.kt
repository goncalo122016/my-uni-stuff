package com.li4.loja.dto

import com.li4.utils.Cargo
import jakarta.validation.constraints.*
import java.math.BigDecimal

data class LoginRequest(
    @field:NotBlank val numero: String,
    @field:NotBlank val senha: String
)

data class CriarClienteRequest(
    @field:NotBlank val nome: String,
    val email: String? = null,
    val telefone: String? = null,
    val nif: String? = null
)

data class EditarClienteRequest(
    val nome: String? = null,
    val email: String? = null,
    val telefone: String? = null,
    val nif: String? = null
)

data class CriarFuncionarioRequest(
    @field:NotBlank val numero: String,
    @field:NotBlank val nome: String,
    @field:NotNull val cargo: Cargo,
    @field:NotBlank val senha: String
)

data class EditarFuncionarioRequest(
    val nome: String? = null,
    val cargo: Cargo? = null
)

data class EntradaStockRequest(
    @field:NotNull val produtoId: Long,
    @field:NotNull @field:Min(1) val quantidade: Int,
    val quantidadeMinima: Int? = null,
    val precoVenda: BigDecimal? = null
)

data class AjusteStockRequest(
    @field:NotNull val produtoId: Long,
    @field:NotNull val quantidade: Int,
    val quantidadeMinima: Int? = null,
    val precoVenda: BigDecimal? = null,
    val motivo: String? = null
)

data class LinhaVendaRequest(
    @field:NotNull val produtoId: Long,
    @field:Min(1) val quantidade: Int
)

data class RegistarVendaRequest(
    @field:NotNull val funcionarioId: Long,
    val clienteId: Long? = null,
    @field:NotEmpty val linhas: List<LinhaVendaRequest>,
    val emitirFatura: Boolean = false,
    val dadosFatura: DadosFaturaRequest? = null
)

data class LinhaDevolucaoRequest(
    @field:NotNull val linhaVendaId: Long,
    @field:Min(1) val quantidade: Int
)

data class RegistarDevolucaoRequest(
    val motivo: String? = null,
    val linhas: List<LinhaDevolucaoRequest>? = null
)

data class DadosFaturaRequest(
    @field:NotBlank val nomeCliente: String,
    val nifCliente: String? = null,
    val moradaCliente: String? = null
)
