package com.li4.cadeia.domain

import jakarta.persistence.*
import java.math.BigDecimal

@Embeddable
class Preco(
    @Column(name = "preco_valor", precision = 10, scale = 2, nullable = false) var valor: BigDecimal,
    @Column(name = "preco_moeda") var moeda: String = "EUR"
) {
    override fun toString() = "Preco(valor=$valor $moeda)"
}
