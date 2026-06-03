package com.li4.loja.domain

import jakarta.persistence.*
import java.time.LocalDate

@Entity
@Table(name = "historico_vendas")
class HistoricoDeVendas(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    var lojaId: Long,
    var periodo: LocalDate
) {
    @OneToMany(mappedBy = "historico", fetch = FetchType.LAZY)
    var vendas: MutableList<Venda> = mutableListOf()

    override fun equals(other: Any?) = other is HistoricoDeVendas && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "HistoricoDeVendas(id=$id, lojaId=$lojaId, periodo=$periodo)"
}
