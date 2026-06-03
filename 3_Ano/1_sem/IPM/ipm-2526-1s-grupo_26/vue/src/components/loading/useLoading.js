import { ref } from 'vue';

const isLoading = ref(false);

export function useLoading() {
  function startLoading() {
    isLoading.value = true;
  }

  function stopLoading() {
    isLoading.value = false;
  }

  return {
    isLoading,
    startLoading,
    stopLoading,
  };
}
