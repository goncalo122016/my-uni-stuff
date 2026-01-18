<template>
  <transition
    enter-active-class="transition ease-out duration-300"
    enter-from-class="opacity-0 translate-y-2"
    enter-to-class="opacity-100 translate-y-0"
    leave-active-class="transition ease-in duration-200"
    leave-from-class="opacity-100 translate-y-0"
    leave-to-class="opacity-0 translate-y-2"
  >
    <div
      v-if="toastMessage"
      class="fixed bottom-6 right-6 z-50 rounded-lg bg-gray-900 px-5 py-3 text-sm font-medium text-white shadow-lg"
    >
      {{ toastMessage }}
    </div>
  </transition>
</template>

<script setup>
  import { useToast } from './useToast';

  const { toastMessage } = useToast();
</script>
