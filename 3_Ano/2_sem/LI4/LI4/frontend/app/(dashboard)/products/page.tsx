"use client";

import { useEffect, useState } from "react";
import { Plus, SlidersHorizontal, Package, TrendingUp, AlertTriangle, ChevronLeft, ChevronRight, X, Tag, Truck } from "lucide-react";
import Header from "@/components/layout/Header";
import { api } from "@/lib/api";
import type { ProdutoResponse, FornecedorResponse } from "@/lib/api";

const BADGE_COLORS = [
  { label: "IN STOCK", bg: "bg-success-light", text: "text-success" },
  { label: "LOW STOCK", bg: "bg-warning-light", text: "text-warning" },
  { label: "PREMIUM", bg: "bg-amber-100", text: "text-amber-700" },
  { label: "HOT ITEM", bg: "bg-danger-light", text: "text-danger" },
];

const CARD_COLORS = [
  "from-zinc-900 to-zinc-700",
  "from-slate-800 to-slate-600",
  "from-stone-900 to-stone-700",
  "from-neutral-900 to-neutral-700",
  "from-gray-900 to-gray-700",
  "from-zinc-800 to-zinc-900",
  "from-slate-700 to-slate-900",
  "from-stone-800 to-stone-900",
];

const PAGE_SIZE = 8;

export default function ProductsPage() {
  const [produtos, setProdutos] = useState<ProdutoResponse[]>([]);
  const [fornecedores, setFornecedores] = useState<FornecedorResponse[]>([]);
  const [loading, setLoading] = useState(true);
  const [page, setPage] = useState(0);
  const [search, setSearch] = useState("");
  const [selected, setSelected] = useState<ProdutoResponse | null>(null);

  useEffect(() => {
    Promise.all([api.produtos.list(), api.fornecedores.list().catch(() => [])])
      .then(([p, f]) => { setProdutos(p); setFornecedores(f); })
      .finally(() => setLoading(false));
  }, []);

  const filtered = search
    ? produtos.filter((p) => p.nome.toLowerCase().includes(search.toLowerCase()))
    : produtos;

  const totalPages = Math.ceil(filtered.length / PAGE_SIZE);
  const paged = filtered.slice(page * PAGE_SIZE, (page + 1) * PAGE_SIZE);
  const lowStock = produtos.filter((_, i) => i % 5 === 2).length; // simulated

  return (
    <div className="flex flex-col flex-1">
      <Header
        placeholder="Search catalog..."
        action={
          <div className="flex items-center gap-2">
            <button className="flex items-center gap-2 px-4 py-2 rounded-lg border border-border text-sm text-text-secondary hover:bg-surface transition-colors">
              <SlidersHorizontal size={14} />
              FILTERS
            </button>
            <button className="flex items-center gap-2 px-4 py-2 rounded-lg bg-primary text-white text-sm font-medium hover:bg-primary-dark transition-colors">
              <Plus size={14} />
              ADD NEW PRODUCT
            </button>
          </div>
        }
      />

      <main className="flex-1 p-6 flex flex-col gap-6">
        <div>
          <h1 className="text-2xl font-bold text-text-primary">Products</h1>
          <p className="text-text-secondary text-sm mt-0.5">
            Manage your global inventory and product specifications.
          </p>
        </div>

        {/* Stats */}
        <div className="grid grid-cols-4 gap-4">
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Total Products</p>
              <Package size={16} className="text-primary" />
            </div>
            <p className="text-2xl font-bold text-text-primary">{produtos.length.toLocaleString()}</p>
            <p className="text-xs text-success mt-1">+10%</p>
          </div>
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Active Suppliers</p>
              <TrendingUp size={16} className="text-success" />
            </div>
            <p className="text-2xl font-bold text-text-primary">{fornecedores.length}</p>
            <p className="text-xs text-text-muted mt-1">Verified</p>
          </div>
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Low Stock Alert</p>
              <AlertTriangle size={16} className="text-warning" />
            </div>
            <p className="text-2xl font-bold text-text-primary">{lowStock}</p>
            <p className="text-xs text-warning mt-1">△ Requires attention</p>
          </div>
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Avg. Margin</p>
              <TrendingUp size={16} className="text-primary" />
            </div>
            <p className="text-2xl font-bold text-text-primary">36.2%</p>
            <p className="text-xs text-success mt-1">+2.1% vs last month</p>
          </div>
        </div>

        {/* Product grid */}
        {loading ? (
          <div className="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-4">
            {Array.from({ length: 8 }).map((_, i) => (
              <div key={i} className="h-56 bg-surface rounded-xl animate-pulse" />
            ))}
          </div>
        ) : (
          <>
            <div className="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-4">
              {paged.map((p, idx) => {
                const badge = BADGE_COLORS[idx % BADGE_COLORS.length];
                const gradient = CARD_COLORS[idx % CARD_COLORS.length];
                return (
                  <div key={p.id} onClick={() => setSelected(p)} className="bg-card rounded-xl border border-border overflow-hidden hover:shadow-md transition-shadow cursor-pointer group">
                    <div className={`h-36 bg-gradient-to-br ${gradient} flex items-center justify-center relative`}>
                      <span className="text-4xl opacity-20">📦</span>
                      <span className={`absolute top-2 left-2 px-1.5 py-0.5 rounded text-[10px] font-bold ${badge.bg} ${badge.text}`}>
                        {badge.label}
                      </span>
                    </div>
                    <div className="p-3.5">
                      <p className="text-sm font-semibold text-text-primary line-clamp-1">{p.nome}</p>
                      <p className="text-xs text-text-muted mt-0.5">{p.identificador}</p>
                      <div className="flex items-center justify-between mt-2">
                        <p className="text-sm font-bold text-text-primary">€{p.precoUnitario.toFixed(2)}</p>
                        {p.fornecedorNome && (
                          <div className="flex items-center gap-1">
                            <div className="w-4 h-4 rounded-full bg-primary/10 text-primary flex items-center justify-center text-[8px] font-bold">
                              {p.fornecedorNome.charAt(0)}
                            </div>
                            <p className="text-[10px] text-text-muted truncate max-w-20">{p.fornecedorNome}</p>
                          </div>
                        )}
                      </div>
                    </div>
                  </div>
                );
              })}
            </div>

            {/* Pagination */}
            <div className="flex items-center justify-between mt-2">
              <p className="text-sm text-text-muted">
                Showing {page * PAGE_SIZE + 1}–{Math.min((page + 1) * PAGE_SIZE, filtered.length)} of {filtered.length} products
              </p>
              <div className="flex items-center gap-1">
                <button
                  onClick={() => setPage((p) => Math.max(0, p - 1))}
                  disabled={page === 0}
                  className="p-1.5 rounded-lg border border-border hover:bg-surface disabled:opacity-40 transition-colors"
                >
                  <ChevronLeft size={14} />
                </button>
                {Array.from({ length: Math.min(totalPages, 5) }).map((_, i) => (
                  <button
                    key={i}
                    onClick={() => setPage(i)}
                    className={`w-7 h-7 rounded-lg text-xs font-medium transition-colors ${
                      page === i ? "bg-primary text-white" : "border border-border hover:bg-surface"
                    }`}
                  >
                    {i + 1}
                  </button>
                ))}
                {totalPages > 5 && <span className="text-text-muted text-xs px-1">...</span>}
                {totalPages > 5 && (
                  <button
                    onClick={() => setPage(totalPages - 1)}
                    className={`w-7 h-7 rounded-lg text-xs font-medium border border-border hover:bg-surface transition-colors ${page === totalPages - 1 ? "bg-primary text-white border-primary" : ""}`}
                  >
                    {totalPages}
                  </button>
                )}
                <button
                  onClick={() => setPage((p) => Math.min(totalPages - 1, p + 1))}
                  disabled={page >= totalPages - 1}
                  className="p-1.5 rounded-lg border border-border hover:bg-surface disabled:opacity-40 transition-colors"
                >
                  <ChevronRight size={14} />
                </button>
              </div>
            </div>
          </>
        )}
      </main>

      {/* Product Detail Modal */}
      {selected && (
        <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
          <div className="bg-card rounded-xl border border-border p-6 w-full max-w-sm">
            <div className="flex items-center justify-between mb-5">
              <h2 className="font-semibold text-text-primary">Product Details</h2>
              <button onClick={() => setSelected(null)} className="text-text-muted hover:text-text-primary transition-colors">
                <X size={16} />
              </button>
            </div>

            <div className="h-32 rounded-xl bg-gradient-to-br from-zinc-900 to-zinc-700 flex items-center justify-center mb-5">
              <span className="text-4xl opacity-20">📦</span>
            </div>

            <p className="text-lg font-bold text-text-primary leading-tight">{selected.nome}</p>
            {selected.descricao && (
              <p className="text-sm text-text-secondary mt-1">{selected.descricao}</p>
            )}

            <div className="flex flex-col gap-2.5 mt-4">
              <div className="flex items-center justify-between py-2 border-b border-border">
                <span className="text-xs text-text-muted uppercase tracking-wide">Price</span>
                <span className="text-sm font-bold text-primary">€{selected.precoUnitario.toFixed(2)}</span>
              </div>
              <div className="flex items-center justify-between py-2 border-b border-border">
                <div className="flex items-center gap-1.5 text-xs text-text-muted uppercase tracking-wide">
                  <Tag size={11} />SKU
                </div>
                <span className="text-xs font-mono text-text-secondary">{selected.identificador}</span>
              </div>
              {selected.fornecedorNome && (
                <div className="flex items-center justify-between py-2 border-b border-border">
                  <div className="flex items-center gap-1.5 text-xs text-text-muted uppercase tracking-wide">
                    <Truck size={11} />Supplier
                  </div>
                  <span className="text-xs text-text-secondary">{selected.fornecedorNome}</span>
                </div>
              )}
              {selected.categorias.length > 0 && (
                <div className="flex items-center justify-between py-2">
                  <span className="text-xs text-text-muted uppercase tracking-wide">Categories</span>
                  <div className="flex gap-1 flex-wrap justify-end">
                    {selected.categorias.map((c) => (
                      <span key={c} className="px-1.5 py-0.5 rounded text-[10px] font-medium bg-primary-light text-primary">
                        {c}
                      </span>
                    ))}
                  </div>
                </div>
              )}
            </div>

            <button
              onClick={() => setSelected(null)}
              className="w-full mt-5 py-2 rounded-lg border border-border text-sm text-text-secondary hover:bg-surface transition-colors"
            >
              Close
            </button>
          </div>
        </div>
      )}
    </div>
  );
}
