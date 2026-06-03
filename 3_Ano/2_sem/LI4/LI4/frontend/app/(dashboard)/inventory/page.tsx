"use client";

import { useEffect, useState } from "react";
import { Plus, Download, Package, AlertTriangle, XCircle, Search, SlidersHorizontal, X } from "lucide-react";
import Header from "@/components/layout/Header";
import { api } from "@/lib/api";
import { useAuth } from "@/lib/auth";
import type { StockResponse, ProdutoResponse } from "@/lib/api";

interface InventoryRow extends StockResponse {
  categorias: string[];
  fornecedorNome: string | null;
  identificador: string;
}

export default function InventoryPage() {
  const { user } = useAuth();
  const [rows, setRows] = useState<InventoryRow[]>([]);
  const [loading, setLoading] = useState(true);
  const [search, setSearch] = useState("");
  const [categoryFilter, setCategoryFilter] = useState("All Categories");
  const [supplierFilter, setSupplierFilter] = useState("All Suppliers");
  const [fornecedores] = useState<{ nome: string }[]>([]);

  // Edit modal state
  const [editing, setEditing] = useState<InventoryRow | null>(null);
  const [editQty, setEditQty] = useState(0);
  const [editMin, setEditMin] = useState(0);
  const [saving, setSaving] = useState(false);

  useEffect(() => {
    if (!user) return;
    Promise.all([
      api.stock.list(),
      api.lojaProdutos.list(),
    ]).then(([stocks, produtos]) => {
      const prodMap = new Map<number, ProdutoResponse>(produtos.map((p) => [p.id, p]));
      setRows(
        stocks.map((s) => {
          const prod = prodMap.get(s.produtoId);
          return {
            ...s,
            categorias: prod?.categorias ?? [],
            fornecedorNome: prod?.fornecedorNome ?? null,
            identificador: prod?.identificador ?? "—",
          };
        })
      );
    }).finally(() => setLoading(false));
  }, [user]);

  function openEdit(row: InventoryRow) {
    setEditing(row);
    setEditQty(row.quantidade);
    setEditMin(row.quantidadeMinima);
  }

  async function saveEdit() {
    if (!editing || !user) return;
    setSaving(true);
    try {
      const updated = await api.stock.ajuste({
        produtoId: editing.produtoId,
        quantidade: editQty,
        quantidadeMinima: editMin,
      });
      setRows((prev) =>
        prev.map((r) =>
          r.produtoId === editing.produtoId
            ? { ...r, quantidade: updated.quantidade, quantidadeMinima: updated.quantidadeMinima }
            : r
        )
      );
      setEditing(null);
    } catch (e) {
      alert(e instanceof Error ? e.message : "Erro ao atualizar stock");
    } finally {
      setSaving(false);
    }
  }

  const categories = ["All Categories", ...Array.from(new Set(rows.flatMap((r) => r.categorias)))];
  const suppliers = ["All Suppliers", ...fornecedores.map((f) => f.nome)];

  const filtered = rows.filter((r) => {
    const matchSearch = !search || r.produtoNome.toLowerCase().includes(search.toLowerCase()) || r.identificador.toLowerCase().includes(search.toLowerCase());
    const matchCat = categoryFilter === "All Categories" || r.categorias.includes(categoryFilter);
    const matchSup = supplierFilter === "All Suppliers" || r.fornecedorNome === supplierFilter;
    return matchSearch && matchCat && matchSup;
  });

  const lowStock = rows.filter((r) => r.quantidade > 0 && r.quantidade <= r.quantidadeMinima).length;
  const outOfStock = rows.filter((r) => r.quantidade === 0).length;

  function stockLevel(row: InventoryRow) {
    if (row.quantidadeMinima === 0) return 100;
    return Math.min(100, Math.round((row.quantidade / (row.quantidadeMinima * 2)) * 100));
  }

  function stockColor(row: InventoryRow) {
    if (row.quantidade === 0) return "bg-danger";
    if (row.quantidade <= row.quantidadeMinima) return "bg-warning";
    return "bg-success";
  }

  function stockBadge(row: InventoryRow) {
    if (row.quantidade === 0) return { label: "Out of Stock", bg: "bg-danger-light", text: "text-danger" };
    if (row.quantidade <= row.quantidadeMinima) return { label: "Low Stock", bg: "bg-warning-light", text: "text-warning" };
    return { label: "In Stock", bg: "bg-success-light", text: "text-success" };
  }

  return (
    <div className="flex flex-col flex-1">
      <Header
        placeholder="Search inventory..."
        action={
          <div className="flex items-center gap-2">
            <button className="flex items-center gap-2 px-4 py-2 rounded-lg border border-border text-sm text-text-secondary hover:bg-surface transition-colors">
              <Download size={14} />
              EXPORT REPORT
            </button>
            <button className="flex items-center gap-2 px-4 py-2 rounded-lg bg-primary text-white text-sm font-medium hover:bg-primary-dark transition-colors">
              <Plus size={14} />
              ADD NEW ITEM
            </button>
          </div>
        }
      />

      <main className="flex-1 p-6 flex flex-col gap-6">
        <div>
          <h1 className="text-2xl font-bold text-text-primary">Inventory Management</h1>
          <p className="text-text-secondary text-sm mt-0.5">
            Real-time stock levels across your store locations.
          </p>
        </div>

        {/* Stats */}
        <div className="grid grid-cols-3 gap-4">
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Total Active Items</p>
              <Package size={16} className="text-primary" />
            </div>
            <p className="text-3xl font-bold text-text-primary">{rows.length.toLocaleString()}</p>
            <p className="text-xs text-success mt-1">Across all categories</p>
          </div>
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Low on Stock</p>
              <AlertTriangle size={16} className="text-warning" />
            </div>
            <p className="text-3xl font-bold text-text-primary">{String(lowStock).padStart(2, "0")}</p>
            <p className="text-xs text-warning mt-1">△ Urgent</p>
          </div>
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Out of Stock</p>
              <XCircle size={16} className="text-danger" />
            </div>
            <p className="text-3xl font-bold text-text-primary">{String(outOfStock).padStart(2, "0")}</p>
            <p className="text-xs text-danger mt-1">Critical</p>
          </div>
        </div>

        {/* Filters */}
        <div className="flex items-center gap-3 flex-wrap">
          <div className="flex items-center gap-2 bg-card rounded-lg border border-border px-3 py-2">
            <Search size={14} className="text-text-muted shrink-0" />
            <input
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              placeholder="Search products..."
              className="bg-transparent text-sm outline-none text-text-primary placeholder-text-muted w-44"
            />
          </div>
          <select
            value={categoryFilter}
            onChange={(e) => setCategoryFilter(e.target.value)}
            className="bg-card border border-border rounded-lg px-3 py-2 text-sm text-text-secondary outline-none hover:border-primary transition-colors cursor-pointer"
          >
            {categories.map((c) => <option key={c}>{c}</option>)}
          </select>
          <select
            value={supplierFilter}
            onChange={(e) => setSupplierFilter(e.target.value)}
            className="bg-card border border-border rounded-lg px-3 py-2 text-sm text-text-secondary outline-none hover:border-primary transition-colors cursor-pointer"
          >
            {suppliers.map((s) => <option key={s}>{s}</option>)}
          </select>
          <button className="flex items-center gap-2 px-3 py-2 rounded-lg border border-border text-sm text-text-secondary hover:bg-surface transition-colors ml-auto">
            <SlidersHorizontal size={14} />
            Sort by Stock Level
          </button>
        </div>

        {/* Table */}
        <div className="bg-card rounded-xl border border-border overflow-hidden">
          <table className="w-full">
            <thead>
              <tr className="border-b border-border bg-surface/50">
                {["Product Title", "SKU", "Category", "Supplier", "Stock Level", "Actions"].map((h) => (
                  <th key={h} className="text-left px-5 py-3 text-xs font-medium text-text-muted uppercase tracking-wide">
                    {h}
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {loading
                ? Array.from({ length: 6 }).map((_, i) => (
                    <tr key={i} className="border-b border-border">
                      {Array.from({ length: 6 }).map((_, j) => (
                        <td key={j} className="px-5 py-3.5">
                          <div className="h-4 bg-surface rounded animate-pulse w-24" />
                        </td>
                      ))}
                    </tr>
                  ))
                : filtered.map((row) => {
                    const badge = stockBadge(row);
                    const level = stockLevel(row);
                    return (
                      <tr key={row.produtoId} className="border-b border-border last:border-0 hover:bg-surface/40 transition-colors">
                        <td className="px-5 py-3.5">
                          <div className="flex items-center gap-3">
                            <div className="w-8 h-8 rounded-lg bg-primary/10 flex items-center justify-center text-xs font-bold text-primary shrink-0">
                              {row.produtoNome.charAt(0)}
                            </div>
                            <span className="text-sm font-medium text-text-primary">{row.produtoNome}</span>
                          </div>
                        </td>
                        <td className="px-5 py-3.5 text-sm text-text-muted font-mono">{row.identificador}</td>
                        <td className="px-5 py-3.5 text-sm text-text-secondary">
                          {row.categorias[0] ?? "—"}
                        </td>
                        <td className="px-5 py-3.5 text-sm text-text-secondary">
                          {row.fornecedorNome ?? "—"}
                        </td>
                        <td className="px-5 py-3.5">
                          <div className="flex items-center gap-3">
                            <div className="flex-1 h-1.5 bg-surface rounded-full overflow-hidden min-w-16">
                              <div
                                className={`h-full rounded-full transition-all ${stockColor(row)}`}
                                style={{ width: `${level}%` }}
                              />
                            </div>
                            <span className={`inline-flex px-2 py-0.5 rounded-full text-[10px] font-medium whitespace-nowrap ${badge.bg} ${badge.text}`}>
                              {badge.label}
                            </span>
                            <span className="text-xs text-text-muted w-16 text-right">
                              {row.quantidade} / {row.quantidadeMinima * 2}
                            </span>
                          </div>
                        </td>
                        <td className="px-5 py-3.5">
                          <div className="flex items-center gap-3">
                            <button
                              onClick={() => openEdit(row)}
                              className="text-xs text-primary font-medium hover:underline"
                            >
                              Edit
                            </button>
                            <button
                              onClick={() => setRows((prev) => prev.filter((r) => r.produtoId !== row.produtoId))}
                              className="text-xs text-text-muted hover:text-danger transition-colors"
                            >
                              Remove
                            </button>
                          </div>
                        </td>
                      </tr>
                    );
                  })}
              {!loading && filtered.length === 0 && (
                <tr>
                  <td colSpan={6} className="px-5 py-10 text-center text-sm text-text-muted">
                    No inventory items found.
                  </td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
      </main>

      {/* Edit Stock Modal */}
      {editing && (
        <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
          <div className="bg-card rounded-xl border border-border p-6 w-full max-w-sm">
            <div className="flex items-center justify-between mb-5">
              <div>
                <h2 className="font-semibold text-text-primary">Edit Stock</h2>
                <p className="text-xs text-text-muted mt-0.5">{editing.produtoNome}</p>
              </div>
              <button onClick={() => setEditing(null)} className="text-text-muted hover:text-text-primary transition-colors">
                <X size={16} />
              </button>
            </div>

            <div className="flex flex-col gap-4">
              <div>
                <label className="block text-xs font-medium text-text-muted uppercase tracking-wide mb-1.5">
                  Quantity
                </label>
                <input
                  type="number"
                  min={0}
                  value={editQty}
                  onChange={(e) => setEditQty(Number(e.target.value))}
                  className="w-full bg-surface border border-border rounded-lg px-3 py-2 text-sm text-text-primary outline-none focus:border-primary transition-colors"
                />
              </div>
              <div>
                <label className="block text-xs font-medium text-text-muted uppercase tracking-wide mb-1.5">
                  Minimum Quantity
                </label>
                <input
                  type="number"
                  min={0}
                  value={editMin}
                  onChange={(e) => setEditMin(Number(e.target.value))}
                  className="w-full bg-surface border border-border rounded-lg px-3 py-2 text-sm text-text-primary outline-none focus:border-primary transition-colors"
                />
              </div>
            </div>

            <div className="flex gap-2 mt-6">
              <button
                onClick={() => setEditing(null)}
                className="flex-1 py-2 rounded-lg border border-border text-sm text-text-secondary hover:bg-surface transition-colors"
              >
                Cancel
              </button>
              <button
                onClick={saveEdit}
                disabled={saving}
                className="flex-1 py-2 rounded-lg bg-primary text-white text-sm font-medium hover:bg-primary-dark transition-colors disabled:opacity-50"
              >
                {saving ? "Saving…" : "Save Changes"}
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
