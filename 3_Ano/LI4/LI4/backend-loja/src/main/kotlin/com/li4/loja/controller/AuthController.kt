package com.li4.loja.controller

import com.li4.loja.dto.FuncionarioResponse
import com.li4.loja.dto.LoginRequest
import com.li4.loja.service.FuncionarioService
import jakarta.validation.Valid
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RestController
@RequestMapping("/api/auth")
class AuthController(private val funcionarioService: FuncionarioService) {

    @PostMapping("/login")
    fun login(@Valid @RequestBody request: LoginRequest): ResponseEntity<FuncionarioResponse> =
        ResponseEntity.ok(funcionarioService.autenticar(request.numero, request.senha))
}
