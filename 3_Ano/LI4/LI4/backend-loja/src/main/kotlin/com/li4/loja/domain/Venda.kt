package com.li4.loja.domain

import jakarta.persistence.*
import java.math.BigDecimal
import java.time.LocalDateTime

@Entity
@Table(name = "venda")
class Venda(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    var dataHora: LocalDateTime = LocalDateTime.now(),
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "funcionario_id")
    var funcionario: Funcionario,
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "cliente_id")
    var cliente: Cliente? = null,
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "historico_id")
    var historico: HistoricoDeVendas? = null,
    @Enumerated(EnumType.STRING) var estado: EstadoVenda = EstadoVenda.ABERTA
) {
    @OneToMany(mappedBy = "venda", cascade = [CascadeType.ALL], fetch = FetchType.LAZY)
    var linhasDeVenda: MutableList<LinhaDeVenda> = mutableListOf()

    @OneToOne(mappedBy = "venda", cascade = [CascadeType.ALL], fetch = FetchType.LAZY)
    var fatura: Fatura? = null

    val total: BigDecimal
        get() = linhasDeVenda.sumOf { it.subtotal }

    override fun equals(other: Any?) = other is Venda && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "Venda(id=$id, estado=$estado)"
}

enum class EstadoVenda { ABERTA, CONCLUIDA, CANCELADA }
