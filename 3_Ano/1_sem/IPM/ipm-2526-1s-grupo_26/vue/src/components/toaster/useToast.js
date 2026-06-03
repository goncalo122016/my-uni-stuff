import { ref } from 'vue';

const toastMessage = ref('');
let timeout = null;

export function useToast() {
  function showToast(message, duration = 2500) {
    toastMessage.value = message;
    if (timeout) clearTimeout(timeout);
    timeout = setTimeout(() => (toastMessage.value = ''), duration);
  }

  return {
    toastMessage,
    showToast,
  };
}
