package com.li4.cadeia.domain

import jakarta.persistence.*

@Entity
@Table(name = "fornecedor")
class Fornecedor(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    var nome: String,
    var nif: String? = null,
    var morada: String? = null,
    var contacto: String? = null,
    var email: String? = null
) {
    @OneToMany(mappedBy = "fornecedor", fetch = FetchType.LAZY)
    var produtos: MutableList<Produto> = mutableListOf()

    override fun equals(other: Any?) = other is Fornecedor && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "Fornecedor(id=$id, nome=$nome)"
}
