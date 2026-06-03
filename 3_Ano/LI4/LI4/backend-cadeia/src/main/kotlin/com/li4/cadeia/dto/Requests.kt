package com.li4.cadeia.dto

import com.li4.utils.Cargo
import jakarta.validation.constraints.*
import java.math.BigDecimal

data class LoginRequest(
    @field:NotBlank val numero: String,
    @field:NotBlank val senha: String
)

data class CriarFuncionarioRequest(
    @field:NotBlank val numero: String,
    @field:NotBlank val nome: String,
    @field:NotNull val cargo: Cargo,
    @field:NotNull val lojaId: Long,
    @field:NotBlank val senha: String
)

data class EditarFuncionarioRequest(
    val nome: String? = null,
    val cargo: Cargo? = null
)

data class AssociarLojaRequest(@field:NotNull val lojaId: Long)

data class CriarLojaRequest(
    @field:NotBlank val nome: String,
    @field:NotBlank val rua: String,
    @field:NotBlank val cidade: String,
    @field:NotBlank val codigoPostal: String,
    val pais: String = "Portugal",
    @field:NotNull val cadeiaId: Long
)

data class CriarProdutoRequest(
    @field:NotBlank val nome: String,
    val descricao: String? = null,
    @field:NotBlank val identificador: String,
    val codigoBarras: String? = null,
    @field:NotNull @field:DecimalMin("0.01") val preco: BigDecimal,
    val moeda: String = "EUR",
    @field:NotEmpty val categoriaIds: List<Long>,
    @field:NotNull val fornecedorId: Long
)

data class EditarProdutoRequest(
    val nome: String? = null,
    val descricao: String? = null,
    val preco: BigDecimal? = null,
    val categoriaIds: List<Long>? = null
)

data class CriarFornecedorRequest(
    @field:NotBlank val nome: String,
    val nif: String? = null,
    val morada: String? = null,
    val contacto: String? = null,
    val email: String? = null
)

data class AssociarFornecedorRequest(@field:NotNull val fornecedorId: Long)
