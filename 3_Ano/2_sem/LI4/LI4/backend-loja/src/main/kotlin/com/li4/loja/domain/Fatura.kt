package com.li4.loja.domain

import jakarta.persistence.*
import java.math.BigDecimal
import java.time.LocalDateTime

@Entity
@Table(name = "fatura")
class Fatura(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    @Column(unique = true, nullable = false) var numero: String,
    var dataEmissao: LocalDateTime = LocalDateTime.now(),
    @OneToOne(optional = false)
    @JoinColumn(name = "venda_id")
    var venda: Venda,
    var nomeCliente: String,
    var nifCliente: String? = null,
    var moradaCliente: String? = null,
    @Column(precision = 10, scale = 2) var totalSemIva: BigDecimal,
    @Column(precision = 5, scale = 2) var taxaIva: BigDecimal = BigDecimal("0.23"),
    @Column(precision = 10, scale = 2) var totalComIva: BigDecimal
) {
    override fun equals(other: Any?) = other is Fatura && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "Fatura(id=$id, numero=$numero)"
}
