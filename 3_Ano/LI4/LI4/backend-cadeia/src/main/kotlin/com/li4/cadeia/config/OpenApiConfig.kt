package com.li4.cadeia.config

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
                .title("Backend Cadeia — LI4 API Central")
                .version("1.0")
                .description(
                    """
                    API REST para gestão central da cadeia de lojas (Grupo 9).
                    Porta 8080. Recebe dados sincronizados das lojas via `/api/sync/importar`.

                    **Credenciais de demonstração:**
                    - `ADM001 / admin123` → ADMINISTRADOR_CENTRAL
                    - `GER001 / gerente123` → GERENTE
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
