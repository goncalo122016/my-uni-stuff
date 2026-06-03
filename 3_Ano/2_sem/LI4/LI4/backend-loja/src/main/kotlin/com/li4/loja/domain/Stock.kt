package com.li4.loja.domain

import jakarta.persistence.*
import java.math.BigDecimal

@Entity
@Table(name = "stock")
class Stock(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    @Column(unique = true, nullable = false) var produtoId: Long,
    var quantidade: Int = 0,
    var quantidadeMinima: Int = 0,
    var precoVenda: BigDecimal? = null
) {
    override fun equals(other: Any?) = other is Stock && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "Stock(produtoId=$produtoId, quantidade=$quantidade)"
}
