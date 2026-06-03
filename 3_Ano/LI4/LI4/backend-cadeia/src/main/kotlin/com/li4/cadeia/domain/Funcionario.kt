package com.li4.cadeia.domain

import com.li4.utils.Cargo
import jakarta.persistence.*

@Entity
@Table(name = "funcionario")
class Funcionario(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    @Column(unique = true, nullable = false) var numero: String,
    var nome: String,
    @Enumerated(EnumType.STRING) @Column(nullable = false) var cargo: Cargo,
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "loja_id")
    var loja: Loja,
    var senha: String
) {
    override fun equals(other: Any?) = other is Funcionario && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "Funcionario(id=$id, nome=$nome, cargo=$cargo)"
}
