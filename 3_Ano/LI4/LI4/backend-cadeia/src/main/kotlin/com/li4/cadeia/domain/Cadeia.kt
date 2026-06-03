package com.li4.cadeia.domain

import jakarta.persistence.*

@Entity
@Table(name = "cadeia")
class Cadeia(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    var nome: String
) {
    @OneToMany(mappedBy = "cadeia", cascade = [CascadeType.ALL], fetch = FetchType.LAZY)
    var lojas: MutableList<Loja> = mutableListOf()

    override fun equals(other: Any?) = other is Cadeia && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "Cadeia(id=$id, nome=$nome)"
}
