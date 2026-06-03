package com.li4.cadeia.domain

import jakarta.persistence.*
import java.math.BigDecimal
import java.time.LocalDate
import java.time.LocalDateTime

@Entity
@Table(name = "relatorio")
class Relatorio(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    var dataGeracao: LocalDateTime = LocalDateTime.now(),
    var periodoInicio: LocalDate,
    var periodoFim: LocalDate,
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "cadeia_id")
    var cadeia: Cadeia? = null,
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "loja_id")
    var loja: Loja? = null,
    var totalVendas: Int = 0,
    @Column(precision = 12, scale = 2) var faturacaoTotal: BigDecimal = BigDecimal.ZERO,
    var totalDevolucoes: Int = 0
) {
    override fun equals(other: Any?) = other is Relatorio && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "Relatorio(id=$id, periodoInicio=$periodoInicio, periodoFim=$periodoFim)"
}
