package com.li4.cadeia.domain

import jakarta.persistence.*

@Entity
@Table(name = "categoria")
class Categoria(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    @Column(unique = true, nullable = false) var nome: String,
    var descricao: String? = null
) {
    override fun equals(other: Any?) = other is Categoria && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "Categoria(id=$id, nome=$nome)"
}
