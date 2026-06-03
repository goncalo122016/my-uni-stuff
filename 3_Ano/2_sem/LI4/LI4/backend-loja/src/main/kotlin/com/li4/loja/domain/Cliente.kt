package com.li4.loja.domain

import jakarta.persistence.*

@Entity
@Table(name = "cliente")
class Cliente(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    var nome: String,
    @Column(unique = true) var email: String? = null,
    var telefone: String? = null,
    var nif: String? = null
) {
    override fun equals(other: Any?) = other is Cliente && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "Cliente(id=$id, nome=$nome)"
}
