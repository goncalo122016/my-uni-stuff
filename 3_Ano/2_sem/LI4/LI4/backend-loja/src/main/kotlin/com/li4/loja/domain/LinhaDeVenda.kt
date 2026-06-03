package com.li4.loja.domain

import jakarta.persistence.*
import java.math.BigDecimal

@Entity
@Table(name = "linha_de_venda")
class LinhaDeVenda(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "produto_id")
    var produto: Produto,
    var quantidade: Int,
    @Column(precision = 10, scale = 2) var precoUnitario: BigDecimal,
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "venda_id")
    var venda: Venda
) {
    val subtotal: BigDecimal get() = precoUnitario.multiply(BigDecimal(quantidade))

    override fun equals(other: Any?) = other is LinhaDeVenda && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "LinhaDeVenda(id=$id, quantidade=$quantidade)"
}
