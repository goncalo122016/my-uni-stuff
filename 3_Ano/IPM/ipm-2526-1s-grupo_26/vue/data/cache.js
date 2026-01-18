/**
 * Data Cache Manager
 * Armazena dados carregados da API em memória para evitar múltiplas requisições
 */

class DataCache {
  constructor() {
    this.cache = new Map();
    this.ttl = 30 * 60 * 1000; // 30 minutos
  }

  /**
   * Gera uma chave única para o cache
   */
  generateKey(city, type, period = null) {
    if (period) {
      return `${city}:${type}:${period}`;
    }
    return `${city}:${type}`;
  }

  /**
   * Obtém dados do cache
   */
  get(city, type, period = null) {
    const key = this.generateKey(city, type, period);
    const cached = this.cache.get(key);

    if (!cached) return null;

    // Verifica se expirou
    if (Date.now() - cached.timestamp > this.ttl) {
      this.cache.delete(key);
      return null;
    }

    return cached.data;
  }

  /**
   * Armazena dados no cache
   */
  set(city, type, data, period = null) {
    const key = this.generateKey(city, type, period);
    this.cache.set(key, {
      data,
      timestamp: Date.now(),
    });
  }

  /**
   * Limpa o cache
   */
  clear() {
    this.cache.clear();
  }

  /**
   * Remove um item específico do cache
   */
  remove(city, type) {
    const key = this.generateKey(city, type);
    this.cache.delete(key);
  }

  /**
   * Obtém tamanho do cache
   */
  size() {
    return this.cache.size;
  }

  /**
   * Lista todas as chaves em cache
   */
  keys() {
    return Array.from(this.cache.keys());
  }
}

// Instância única do cache
export const dataCache = new DataCache();

export default dataCache;
