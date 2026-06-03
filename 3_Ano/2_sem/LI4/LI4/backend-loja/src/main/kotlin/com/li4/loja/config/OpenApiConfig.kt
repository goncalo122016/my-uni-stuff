package com.li4.loja.config

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.oas.models.info.Info
import io.swagger.v3.oas.models.responses.ApiResponse
import org.springdoc.core.customizers.OpenApiCustomizer
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

@Configuration
class OpenApiConfig {

    @Bean
    fun openApi(): OpenAPI = OpenAPI()
        .info(
            Info()
                .title("Backend Loja — LI4 API Local")
                .version("1.0")
                .description(
                    """
                    API REST para operações locais da loja (Grupo 9).
                    Porta 8081. Sincroniza dados diariamente com o backend central (8080).

                    **Credenciais de demonstração:**
                    - `ADM001 / admin123` → ADMINISTRADOR_CENTRAL
                    - `GER001 / gerente123` → GERENTE
                    - `FUN001 / func123` → FUNCIONARIO
                    """.trimIndent()
                )
        )

    @Bean
    fun globalErrorResponsesCustomizer(): OpenApiCustomizer = OpenApiCustomizer { openApi ->
        val r400 = ApiResponse().description("Pedido inválido")
        val r404 = ApiResponse().description("Recurso não encontrado")
        val r409 = ApiResponse().description("Conflito ou estado inválido")
        openApi.paths?.values?.forEach { pathItem ->
            pathItem.readOperations()?.forEach { operation ->
                operation.responses.addApiResponse("400", r400).addApiResponse("404", r404).addApiResponse("409", r409)
            }
        }
    }
}
