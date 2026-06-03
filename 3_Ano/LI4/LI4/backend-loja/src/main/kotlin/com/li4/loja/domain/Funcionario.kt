package com.li4.loja.domain

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
    var lojaId: Long,
    var senha: String
) {
    override fun equals(other: Any?) = other is Funcionario && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "Funcionario(id=$id, nome=$nome, cargo=$cargo)"
}
