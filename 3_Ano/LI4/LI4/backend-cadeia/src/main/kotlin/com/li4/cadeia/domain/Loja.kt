package com.li4.cadeia.domain

import jakarta.persistence.*

@Entity
@Table(name = "loja")
class Loja(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    var nome: String,

    @OneToOne(cascade = [CascadeType.ALL], optional = false)
    @JoinColumn(name = "localizacao_id")
    var localizacao: Localizacao,

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "cadeia_id")
    var cadeia: Cadeia
) {
    @OneToMany(mappedBy = "loja", cascade = [CascadeType.ALL], fetch = FetchType.LAZY)
    var funcionarios: MutableList<Funcionario> = mutableListOf()

    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(
        name = "loja_fornecedor",
        joinColumns = [JoinColumn(name = "loja_id")],
        inverseJoinColumns = [JoinColumn(name = "fornecedor_id")]
    )
    var fornecedores: MutableList<Fornecedor> = mutableListOf()

    override fun equals(other: Any?) = other is Loja && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "Loja(id=$id, nome=$nome)"
}
