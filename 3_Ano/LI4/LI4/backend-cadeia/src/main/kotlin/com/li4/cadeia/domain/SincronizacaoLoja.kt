package com.li4.cadeia.domain

import jakarta.persistence.*
import java.math.BigDecimal
import java.time.LocalDate
import java.time.LocalDateTime

@Entity
@Table(name = "sincronizacao_loja")
class SincronizacaoLoja(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    var lojaId: Long,
    var dataReferencia: LocalDate,
    var totalVendas: Int,
    @Column(precision = 12, scale = 2) var faturacaoTotal: BigDecimal,
    var totalDevolucoes: Int = 0,
    var timestamp: LocalDateTime = LocalDateTime.now()
) {
    override fun equals(other: Any?) = other is SincronizacaoLoja && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "SincronizacaoLoja(lojaId=$lojaId, data=$dataReferencia)"
}
