function getApiBase(): string {
  if (typeof window === "undefined") return "/loja-api";
  return localStorage.getItem("bv_backend") ?? "/loja-api";
}

function authHeader(): string {
  if (typeof window === "undefined") return "";
  return `Basic ${localStorage.getItem("bv_creds") ?? ""}`;
}

async function req<T>(path: string, init?: RequestInit): Promise<T> {
  const res = await fetch(`${getApiBase()}${path}`, {
    ...init,
    headers: {
      "Content-Type": "application/json",
      Authorization: authHeader(),
      ...init?.headers,
    },
  });
  if (!res.ok) {
    const body = await res.json().catch(() => ({}));
    throw new Error(body.mensagem ?? `Erro ${res.status}`);
  }
  return res.json() as Promise<T>;
}

async function reqLoja<T>(path: string, init?: RequestInit): Promise<T> {
  const res = await fetch(`/loja-api${path}`, {
    ...init,
    headers: {
      "Content-Type": "application/json",
      Authorization: authHeader(),
      ...init?.headers,
    },
  });
  if (!res.ok) {
    const body = await res.json().catch(() => ({}));
    throw new Error(body.mensagem ?? `Erro ${res.status}`);
  }
  return res.json() as Promise<T>;
}

// ─── Types ────────────────────────────────────────────────────────────────────

export type Cargo = "FUNCIONARIO" | "GERENTE" | "ADMINISTRADOR_CENTRAL";
export type EstadoVenda = "ABERTA" | "CONCLUIDA" | "CANCELADA";

export interface LoginResponse {
  id: number;
  numero: string;
  nome: string;
  cargo: Cargo;
  lojaId: number;
}
export interface ProdutoResponse {
  id: number;
  nome: string;
  descricao: string | null;
  identificador: string;
  precoUnitario: number;
  categorias: string[];
  fornecedorNome: string | null;
}
export interface StockResponse {
  produtoId: number;
  produtoNome: string;
  quantidade: number;
  quantidadeMinima: number;
  abaixoMinimo: boolean;
  precoVenda: number | null;
}
export interface LinhaVendaResponse {
  produtoId: number;
  produtoNome: string;
  quantidade: number;
  precoUnitario: number;
  subtotal: number;
}
export interface VendaResponse {
  id: number;
  dataHora: string;
  funcionarioNome: string;
  clienteNome: string | null;
  linhas: LinhaVendaResponse[];
  total: number;
  estado: EstadoVenda;
  faturaNumero: string | null;
}
export interface FaturaResponse {
  id: number;
  numero: string;
  dataEmissao: string;
  nomeCliente: string;
  nifCliente: string | null;
  totalSemIva: number;
  taxaIva: number;
  totalComIva: number;
  vendaId: number;
}
export interface LojaResponse {
  id: number;
  nome: string;
  cidade: string;
  cadeiaId: number;
  cadeaNome: string;
}
export interface FuncionarioResponse {
  id: number;
  numero: string;
  nome: string;
  cargo: Cargo;
  lojaId: number;
}
export interface EstatisticasLojaResponse {
  lojaId: number;
  lojaNome: string;
  totalVendas: number;
  faturacaoTotal: number;
}
export interface EstatisticasCadeiaResponse {
  cadeiaId: number;
  cadeaNome: string;
  totalVendas: number;
  faturacaoTotal: number;
  porLoja: EstatisticasLojaResponse[];
}
export interface HistoricoResponse {
  lojaId: number;
  periodo: string;
  totalVendas: number;
  faturacao: number;
  vendas: VendaResponse[];
}
export interface FornecedorResponse {
  id: number;
  nome: string;
  contacto: string | null;
  email: string | null;
  totalProdutos: number;
}
export interface ClienteResponse {
  id: number;
  nome: string;
  email: string | null;
  telefone: string | null;
  nif: string | null;
}

// ─── Normalizers ─────────────────────────────────────────────────────────────

// cadeia returns `preco`, loja returns `precoUnitario` — normalise to always have `precoUnitario`
function normalizeProduto(p: Record<string, unknown>): ProdutoResponse {
  return { ...p, precoUnitario: (p.precoUnitario ?? p.preco) as number } as ProdutoResponse;
}

// ─── Login helper ─────────────────────────────────────────────────────────────

async function tryLogin(base: string, numero: string, senha: string): Promise<LoginResponse> {
  const res = await fetch(`${base}/auth/login`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ numero, senha }),
  });
  if (!res.ok) {
    const b = await res.json().catch(() => ({}));
    throw new Error(b.mensagem ?? "Credenciais inválidas");
  }
  return res.json() as Promise<LoginResponse>;
}

// ─── API ──────────────────────────────────────────────────────────────────────

export const api = {
  auth: {
    async login(numero: string, senha: string): Promise<{ info: LoginResponse; backend: string }> {
      // Tenta loja primeiro
      try {
        const info = await tryLogin("/loja-api", numero, senha);
        // ADMINISTRADOR_CENTRAL pertence sempre à cadeia, mesmo que a loja o aceite
        if (info.cargo === "ADMINISTRADOR_CENTRAL") {
          const cadeiaInfo = await tryLogin("/cadeia-api", numero, senha);
          return { info: cadeiaInfo, backend: "/cadeia-api" };
        }
        return { info, backend: "/loja-api" };
      } catch {
        const info = await tryLogin("/cadeia-api", numero, senha);
        return { info, backend: "/cadeia-api" };
      }
    },
  },

  produtos: {
    list: (nome?: string) =>
      req<Record<string, unknown>[]>(`/produtos${nome ? `?nome=${encodeURIComponent(nome)}` : ""}`)
        .then((items) => items.map(normalizeProduto)),
    get: (id: number) =>
      req<Record<string, unknown>>(`/produtos/${id}`).then(normalizeProduto),
    create: (data: unknown) =>
      req<Record<string, unknown>>("/produtos", { method: "POST", body: JSON.stringify(data) })
        .then(normalizeProduto),
    update: (id: number, data: unknown) =>
      req<Record<string, unknown>>(`/produtos/${id}`, { method: "PUT", body: JSON.stringify(data) })
        .then(normalizeProduto),
  },

  stock: {
    list: () => reqLoja<StockResponse[]>("/stock"),
    entrada: (data: unknown) =>
      reqLoja<StockResponse>("/stock/entrada", { method: "POST", body: JSON.stringify(data) }),
    ajuste: (data: unknown) =>
      reqLoja<StockResponse>("/stock/ajuste", { method: "POST", body: JSON.stringify(data) }),
  },
  lojaProdutos: {
    list: (nome?: string) =>
      reqLoja<Record<string, unknown>[]>(`/produtos${nome ? `?nome=${encodeURIComponent(nome)}` : ""}`)
        .then((items) => items.map(normalizeProduto)),
  },

  vendas: {
    list: () => req<VendaResponse[]>("/vendas"),
    create: (data: unknown) =>
      req<VendaResponse>("/vendas", { method: "POST", body: JSON.stringify(data) }),
    emitirFatura: (id: number, data: unknown) =>
      req<FaturaResponse>(`/vendas/${id}/fatura`, { method: "POST", body: JSON.stringify(data) }),
    historico: (inicio: string, fim: string) =>
      req<HistoricoResponse>(`/vendas/historico?inicio=${inicio}&fim=${fim}`),
    sync: () => req<void>("/vendas/sync", { method: "POST" }),
  },

  lojas: {
    list: () => req<LojaResponse[]>("/lojas"),
    get: (id: number) => req<LojaResponse>(`/lojas/${id}`),
    estatisticas: (id: number) => req<EstatisticasLojaResponse>(`/lojas/${id}/estatisticas`),
    sync: (id: number) => req<{ mensagem: string }>(`/lojas/${id}/sync`, { method: "POST" }),
  },

  cadeia: {
    list: () => req<{ id: number; nome: string }[]>("/cadeia"),
  },

  lojaVendas: {
    historico: (inicio: string, fim: string) =>
      reqLoja<HistoricoResponse>(`/vendas/historico?inicio=${inicio}&fim=${fim}`),
  },

  central: {
    estatisticas: (cadeiaId: number, inicio: string, fim: string) =>
      req<EstatisticasCadeiaResponse>(`/central/estatisticas?cadeiaId=${cadeiaId}&inicio=${inicio}&fim=${fim}`),
  },

  utilizadores: {
    list: () => req<FuncionarioResponse[]>("/utilizadores"),
    get: (id: number) => req<FuncionarioResponse>(`/utilizadores/${id}`),
    create: (data: unknown) =>
      req<FuncionarioResponse>("/utilizadores", { method: "POST", body: JSON.stringify(data) }),
    update: (id: number, data: unknown) =>
      req<FuncionarioResponse>(`/utilizadores/${id}`, { method: "PUT", body: JSON.stringify(data) }),
  },

  clientes: {
    list: () => req<ClienteResponse[]>("/clientes"),
    create: (data: unknown) =>
      req<ClienteResponse>("/clientes", { method: "POST", body: JSON.stringify(data) }),
    update: (id: number, data: unknown) =>
      req<ClienteResponse>(`/clientes/${id}`, { method: "PUT", body: JSON.stringify(data) }),
  },

  fornecedores: {
    list: () => req<FornecedorResponse[]>("/fornecedores"),
    get: (id: number) => req<FornecedorResponse>(`/fornecedores/${id}`),
    create: (data: unknown) =>
      req<FornecedorResponse>("/fornecedores", { method: "POST", body: JSON.stringify(data) }),
  },
};
