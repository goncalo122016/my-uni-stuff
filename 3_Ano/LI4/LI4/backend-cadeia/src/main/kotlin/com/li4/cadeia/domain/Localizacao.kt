package com.li4.cadeia.domain

import jakarta.persistence.*

@Entity
@Table(name = "localizacao")
class Localizacao(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long = 0,
    var rua: String,
    var cidade: String,
    var codigoPostal: String,
    var pais: String = "Portugal"
) {
    override fun equals(other: Any?) = other is Localizacao && id != 0L && id == other.id
    override fun hashCode() = id.hashCode()
    override fun toString() = "Localizacao(rua=$rua, cidade=$cidade)"
}
