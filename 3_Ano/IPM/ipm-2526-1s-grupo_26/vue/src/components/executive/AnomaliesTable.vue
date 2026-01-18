<template>
  <div>
    <h3 class="mb-4 text-lg font-bold text-gray-900">Anomalias detectadas</h3>
    <div class="overflow-auto">
      <table class="w-full text-left text-sm">
        <thead>
          <tr class="text-xs text-gray-500">
            <th class="py-2 pr-4">Propriedade</th>
            <th class="py-2 pr-4">Problema</th>
            <th class="py-2 pr-4">Zona</th>
            <th class="py-2 pr-4">Gravidade</th>
            <th class="py-2 pr-4">Data</th>
          </tr>
        </thead>
        <tbody>
          <tr
            v-for="row in anomalies"
            :key="row.id"
            class="border-t border-gray-100 hover:bg-gray-50"
          >
            <td class="py-3">{{ row.property }}</td>
            <td class="py-3">{{ row.problem }}</td>
            <td class="py-3">{{ row.zone }}</td>
            <td class="py-3">
              <div class="flex items-center">
                <span
                  class="inline-block rounded-md px-3 py-1 text-xs font-medium shadow-sm"
                  :class="getSeverityClass(row.severity)"
                >
                  {{ formatSeverity(row.severity) }}
                </span>
              </div>
            </td>
            <td class="py-3">{{ row.date }}</td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
</template>

<script setup>
  defineProps({
    anomalies: { type: Array, required: true },
  });

  // helper to normalize severity label shown to users
  function formatSeverity(s) {
    if (!s) return '';
    const lower = String(s).toLowerCase();
    if (lower.includes('alta') || lower.includes('crit')) return 'crítico';
    if (lower.includes('media') || lower.includes('média') || lower.includes('med')) return 'média';
    if (lower.includes('aviso') || lower.includes('warn')) return 'aviso';
    return s;
  }

  function getSeverityClass(s) {
    if (!s) return 'bg-gray-100 text-gray-700 border border-gray-200';
    const lower = String(s).toLowerCase();
    if (lower.includes('alta') || lower.includes('crit')) return 'bg-red-600 text-white';
    if (lower.includes('aviso') || lower.includes('warn'))
      return 'bg-white text-gray-700 border border-gray-300';
    if (lower.includes('media') || lower.includes('média'))
      return 'bg-white text-gray-700 border border-gray-300';
    return 'bg-white text-gray-700 border border-gray-300';
  }
</script>

<style scoped></style>
