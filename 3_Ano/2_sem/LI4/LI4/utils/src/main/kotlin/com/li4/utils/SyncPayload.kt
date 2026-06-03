package com.li4.utils

import java.math.BigDecimal
import java.time.LocalDate

data class SyncPayload(
    val lojaId: Long,
    val dataReferencia: LocalDate,
    val totalVendas: Int,
    val faturacaoTotal: BigDecimal,
    val totalDevolucoes: Int = 0
)
