package com.li4.loja.domain

import jakarta.persistence.*
import java.math.BigDecimal

@Entity
@Table(name = "produto")
class Produto(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    var nome: String,
    var descricao: String? = null,
    @Column(unique = true, nullable = false) var identificador: String,
    var codigoBarras: String? = null,
    @Column(precision = 10, scale = 2) var precoUnitario: BigDecimal,
    var fornecedorNome: String? = null
) {
    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(
        name = "produto_categoria",
        joinColumns = [JoinColumn(name = "produto_id")],
        inverseJoinColumns = [JoinColumn(name = "categoria_id")]
    )
    var categorias: MutableList<Categoria> = mutableListOf()

    override fun equals(other: Any?) = other is Produto && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "Produto(id=$id, nome=$nome)"
}
