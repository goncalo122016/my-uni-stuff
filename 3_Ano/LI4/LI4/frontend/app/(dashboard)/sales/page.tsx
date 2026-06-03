"use client";

import { useEffect, useState } from "react";
import { Minus, Plus, ShoppingCart, CreditCard, Banknote, X, Search } from "lucide-react";
import Header from "@/components/layout/Header";
import { api } from "@/lib/api";
import { useAuth } from "@/lib/auth";
import type { ProdutoResponse, VendaResponse } from "@/lib/api";

interface OrderItem {
  produto: ProdutoResponse;
  quantidade: number;
}

const CATEGORIES = ["All Items", "Bakery", "Coffee", "Dairy", "Produce"];
const PLACEHOLDER_COLORS = [
  "bg-amber-100", "bg-stone-700", "bg-zinc-200", "bg-red-900",
  "bg-yellow-100", "bg-amber-900",
];

export default function SalesPage() {
  const { user } = useAuth();
  const [produtos, setProdutos] = useState<ProdutoResponse[]>([]);
  const [filtered, setFiltered] = useState<ProdutoResponse[]>([]);
  const [activeCategory, setActiveCategory] = useState("All Items");
  const [search, setSearch] = useState("");
  const [order, setOrder] = useState<OrderItem[]>([]);
  const [loading, setLoading] = useState(true);
  const [completing, setCompleting] = useState(false);
  const [lastVenda, setLastVenda] = useState<VendaResponse | null>(null);

  useEffect(() => {
    api.produtos.list().then((p) => { setProdutos(p); setFiltered(p); }).finally(() => setLoading(false));
  }, []);

  useEffect(() => {
    let result = produtos;
    if (activeCategory !== "All Items") {
      result = result.filter((p) => p.categorias.some((c) => c.toLowerCase() === activeCategory.toLowerCase()));
    }
    if (search) {
      result = result.filter((p) => p.nome.toLowerCase().includes(search.toLowerCase()));
    }
    setFiltered(result);
  }, [activeCategory, search, produtos]);

  function addToOrder(produto: ProdutoResponse) {
    setOrder((prev) => {
      const existing = prev.find((i) => i.produto.id === produto.id);
      if (existing) return prev.map((i) => i.produto.id === produto.id ? { ...i, quantidade: i.quantidade + 1 } : i);
      return [...prev, { produto, quantidade: 1 }];
    });
  }

  function changeQty(produtoId: number, delta: number) {
    setOrder((prev) =>
      prev.flatMap((i) => {
        if (i.produto.id !== produtoId) return [i];
        const q = i.quantidade + delta;
        return q <= 0 ? [] : [{ ...i, quantidade: q }];
      })
    );
  }

  const subtotal = order.reduce((s, i) => s + i.produto.precoUnitario * i.quantidade, 0);
  const tax = subtotal * 0.1;
  const total = subtotal + tax;

  async function finishSale() {
    if (!user || order.length === 0) return;
    setCompleting(true);
    try {
      const venda = await api.vendas.create({
        funcionarioId: user.id,
        linhas: order.map((i) => ({ produtoId: i.produto.id, quantidade: i.quantidade })),
        emitirFatura: false,
      });
      setLastVenda(venda);
      setOrder([]);
    } catch (e) {
      alert(e instanceof Error ? e.message : "Erro ao registar venda");
    } finally {
      setCompleting(false);
    }
  }

  return (
    <div className="flex flex-col flex-1">
      <Header placeholder="Search product or SKU..." />

      <div className="flex flex-1 min-h-0">
        {/* Product grid */}
        <div className="flex-1 flex flex-col p-6 gap-4 overflow-auto">
          <div className="flex items-center justify-between">
            <h2 className="text-lg font-bold text-text-primary">Quick Add</h2>
            {/* Category filters */}
            <div className="flex items-center gap-2">
              {CATEGORIES.map((c) => (
                <button
                  key={c}
                  onClick={() => setActiveCategory(c)}
                  className={`px-3 py-1.5 rounded-lg text-xs font-medium transition-colors ${
                    activeCategory === c
                      ? "bg-primary text-white"
                      : "bg-card border border-border text-text-secondary hover:border-primary hover:text-primary"
                  }`}
                >
                  {c}
                </button>
              ))}
            </div>
          </div>

          {/* Search */}
          <div className="flex items-center gap-2 bg-card rounded-lg border border-border px-3 py-2 max-w-xs">
            <Search size={14} className="text-text-muted shrink-0" />
            <input
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              placeholder="Search products..."
              className="bg-transparent text-sm outline-none text-text-primary placeholder-text-muted w-full"
            />
          </div>

          {loading ? (
            <div className="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-4">
              {Array.from({ length: 8 }).map((_, i) => (
                <div key={i} className="h-48 rounded-xl bg-surface animate-pulse" />
              ))}
            </div>
          ) : (
            <div className="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-4">
              {filtered.map((p, idx) => (
                <button
                  key={p.id}
                  onClick={() => addToOrder(p)}
                  className="group bg-card rounded-xl border border-border overflow-hidden text-left hover:border-primary hover:shadow-md transition-all"
                >
                  <div className={`h-32 flex items-center justify-center ${PLACEHOLDER_COLORS[idx % PLACEHOLDER_COLORS.length]}`}>
                    <span className="text-3xl opacity-30">🛍️</span>
                  </div>
                  <div className="p-3">
                    <p className="text-[10px] font-medium text-text-muted uppercase tracking-wide">
                      {p.categorias[0] ?? "—"}
                    </p>
                    <p className="text-sm font-semibold text-text-primary mt-0.5 line-clamp-2 leading-tight">
                      {p.nome}
                    </p>
                    <p className="text-sm font-bold text-primary mt-1">
                      €{p.precoUnitario.toFixed(2)}
                    </p>
                  </div>
                </button>
              ))}
            </div>
          )}
        </div>

        {/* Order panel */}
        <div className="w-72 bg-card border-l border-border flex flex-col">
          <div className="flex items-center justify-between px-5 py-4 border-b border-border">
            <div className="flex items-center gap-2">
              <ShoppingCart size={16} className="text-text-primary" />
              <h3 className="font-semibold text-text-primary">Current Order</h3>
            </div>
            <button className="text-xs text-primary font-medium hover:underline">HOLD</button>
          </div>

          {lastVenda && (
            <div className="mx-4 mt-4 p-3 bg-success-light rounded-lg flex items-center justify-between">
              <p className="text-xs text-success font-medium">Venda #{lastVenda.id} concluída!</p>
              <button onClick={() => setLastVenda(null)}><X size={12} className="text-success" /></button>
            </div>
          )}

          <div className="flex-1 overflow-auto px-4 py-3 flex flex-col gap-3">
            {order.length === 0 ? (
              <p className="text-sm text-text-muted text-center mt-8">
                Seleciona produtos para adicionar à venda
              </p>
            ) : (
              order.map((item) => (
                <div key={item.produto.id} className="flex items-center gap-3">
                  <div className="w-10 h-10 rounded-lg bg-surface flex items-center justify-center text-lg shrink-0">
                    🛍️
                  </div>
                  <div className="flex-1 min-w-0">
                    <p className="text-xs font-medium text-text-primary truncate">{item.produto.nome}</p>
                    <p className="text-xs text-text-muted">€{item.produto.precoUnitario.toFixed(2)}</p>
                  </div>
                  <div className="flex items-center gap-1.5">
                    <button
                      onClick={() => changeQty(item.produto.id, -1)}
                      className="w-5 h-5 rounded-full border border-border flex items-center justify-center hover:bg-surface transition-colors"
                    >
                      <Minus size={10} />
                    </button>
                    <span className="text-xs font-semibold w-4 text-center">{item.quantidade}</span>
                    <button
                      onClick={() => changeQty(item.produto.id, 1)}
                      className="w-5 h-5 rounded-full border border-border flex items-center justify-center hover:bg-surface transition-colors"
                    >
                      <Plus size={10} />
                    </button>
                    <span className="text-xs font-semibold text-text-primary w-12 text-right">
                      €{(item.produto.precoUnitario * item.quantidade).toFixed(2)}
                    </span>
                  </div>
                </div>
              ))
            )}
          </div>

          {/* Totals */}
          <div className="border-t border-border px-5 py-4">
            <div className="flex justify-between text-xs text-text-secondary mb-1">
              <span>Subtotal</span><span>€{subtotal.toFixed(2)}</span>
            </div>
            <div className="flex justify-between text-xs text-text-secondary mb-3">
              <span>Tax 10%</span><span>€{tax.toFixed(2)}</span>
            </div>
            <div className="flex justify-between font-bold text-text-primary mb-4">
              <span>Total</span>
              <span className="text-primary text-lg">€{total.toFixed(2)}</span>
            </div>

            {/* Payment */}
            <div className="grid grid-cols-2 gap-2 mb-3">
              <button className="flex items-center justify-center gap-1.5 py-2 rounded-lg border border-border text-sm text-text-secondary hover:bg-surface transition-colors">
                <Banknote size={14} />Cash
              </button>
              <button className="flex items-center justify-center gap-1.5 py-2 rounded-lg border border-border text-sm text-text-secondary hover:bg-surface transition-colors">
                <CreditCard size={14} />Card
              </button>
            </div>
            <button
              onClick={finishSale}
              disabled={order.length === 0 || completing}
              className="w-full py-3 rounded-xl bg-primary text-white font-semibold text-sm hover:bg-primary-dark transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
            >
              {completing ? "A processar..." : "Finish Sale"}
            </button>
          </div>
        </div>
      </div>
    </div>
  );
}
