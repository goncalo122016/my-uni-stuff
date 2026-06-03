package com.li4.loja.domain

import jakarta.persistence.*
import java.time.LocalDateTime

@Entity
@Table(name = "devolucao")
class Devolucao(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "venda_id")
    var venda: Venda,
    var dataHora: LocalDateTime = LocalDateTime.now(),
    var motivo: String? = null
) {
    @OneToMany(mappedBy = "devolucao", cascade = [CascadeType.ALL], fetch = FetchType.LAZY)
    var linhas: MutableList<LinhaDevolucao> = mutableListOf()

    override fun equals(other: Any?) = other is Devolucao && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "Devolucao(id=$id)"
}

@Entity
@Table(name = "linha_devolucao")
class LinhaDevolucao(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "devolucao_id")
    var devolucao: Devolucao,
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "linha_venda_id")
    var linhaVenda: LinhaDeVenda,
    var quantidade: Int
) {
    override fun equals(other: Any?) = other is LinhaDevolucao && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
}
