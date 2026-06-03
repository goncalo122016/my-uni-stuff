package com.li4.loja.sync

import org.springframework.scheduling.annotation.Scheduled
import org.springframework.stereotype.Component

@Component
class SyncScheduler(private val syncService: SyncService) {

    @Scheduled(cron = "0 55 23 * * *")
    fun syncDiario() {
        syncService.sincronizarDia()
    }
}
