"use client";

import { useEffect, useState } from "react";
import { Plus, Store, TrendingUp, Activity, Eye, X, MapPin, ShoppingBag, DollarSign, RefreshCw } from "lucide-react";
import Header from "@/components/layout/Header";
import { api } from "@/lib/api";
import type { LojaResponse, EstatisticasCadeiaResponse, EstatisticasLojaResponse } from "@/lib/api";

export default function StoresPage() {
  const [lojas, setLojas] = useState<LojaResponse[]>([]);
  const [cadeiaStats, setCadeiaStats] = useState<EstatisticasCadeiaResponse | null>(null);
  const [loading, setLoading] = useState(true);

  // Details modal state
  const [selectedLoja, setSelectedLoja] = useState<LojaResponse | null>(null);
  const [lojaStats, setLojaStats] = useState<EstatisticasLojaResponse | null>(null);
  const [statsLoading, setStatsLoading] = useState(false);
  const [syncingId, setSyncingId] = useState<number | null>(null);
  const [syncResult, setSyncResult] = useState<{ id: number; ok: boolean; msg: string } | null>(null);

  useEffect(() => {
    const today = new Date().toISOString().slice(0, 10);
    Promise.all([api.lojas.list().catch(() => []), api.cadeia.list().catch(() => [])])
      .then(async ([l, cadeias]) => {
        setLojas(l);
        if (cadeias.length > 0) {
          const s = await api.central.estatisticas(cadeias[0].id, "2020-01-01", today).catch(() => null);
          setCadeiaStats(s);
        }
      })
      .finally(() => setLoading(false));
  }, []);

  async function handleSync(lojaId: number) {
    setSyncingId(lojaId);
    setSyncResult(null);
    try {
      const res = await api.lojas.sync(lojaId);
      setSyncResult({ id: lojaId, ok: true, msg: res.mensagem });
    } catch (e) {
      setSyncResult({ id: lojaId, ok: false, msg: e instanceof Error ? e.message : "Erro de sync" });
    } finally {
      setSyncingId(null);
      setTimeout(() => setSyncResult(null), 4000);
    }
  }

  async function openDetails(loja: LojaResponse) {
    setSelectedLoja(loja);
    setLojaStats(null);
    setStatsLoading(true);
    try {
      const s = await api.lojas.estatisticas(loja.id);
      setLojaStats(s);
    } catch {
      // stats unavailable — modal still shows basic info
    } finally {
      setStatsLoading(false);
    }
  }

  return (
    <div className="flex flex-col flex-1">
      <Header
        placeholder="Search stores..."
        action={
          <button className="flex items-center gap-2 px-4 py-2 rounded-lg bg-sidebar text-white text-sm font-medium hover:bg-sidebar-hover transition-colors">
            <Plus size={14} />
            REGISTER NEW STORE
          </button>
        }
      />

      <main className="flex-1 p-6 flex flex-col gap-6">
        <div>
          <h1 className="text-2xl font-bold text-text-primary">Store Network</h1>
          <p className="text-text-secondary text-sm mt-0.5">
            Strategic oversight of all BelaVista physical locations.
          </p>
        </div>

        {/* Stats */}
        <div className="grid grid-cols-3 gap-4">
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Total Stores</p>
              <Store size={16} className="text-primary" />
            </div>
            <p className="text-3xl font-bold text-text-primary">{lojas.length}</p>
            <p className="text-xs text-success mt-1">+3 this quarter</p>
          </div>
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Active Stores</p>
              <Activity size={16} className="text-success" />
            </div>
            <p className="text-3xl font-bold text-text-primary">{lojas.length}</p>
            <p className="text-xs text-success mt-1">+3 vs last period</p>
          </div>
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Avg. Revenue / Store</p>
              <TrendingUp size={16} className="text-warning" />
            </div>
            <p className="text-3xl font-bold text-text-primary">
              {loading || !cadeiaStats
                ? "—"
                : `$${(cadeiaStats.faturacaoTotal / Math.max(lojas.length, 1)).toLocaleString("en", { maximumFractionDigits: 0 })}`}
            </p>
            <p className="text-xs text-success mt-1">+4.1% YoY</p>
          </div>
        </div>

        <div className="grid grid-cols-3 gap-4">
          {/* Store table */}
          <div className="col-span-2 bg-card rounded-xl border border-border">
            <div className="flex items-center justify-between px-5 py-4 border-b border-border">
              <h3 className="font-semibold text-text-primary">Store Inventory</h3>
            </div>
            <table className="w-full">
              <thead>
                <tr className="border-b border-border">
                  {["Store Name", "Location", "Manager", "Status", "Sync", "Action"].map((h) => (
                    <th key={h} className="text-left px-5 py-3 text-xs font-medium text-text-muted uppercase tracking-wide">
                      {h}
                    </th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {loading
                  ? Array.from({ length: 3 }).map((_, i) => (
                      <tr key={i} className="border-b border-border">
                        {Array.from({ length: 6 }).map((_, j) => (
                          <td key={j} className="px-5 py-3">
                            <div className="h-4 bg-surface rounded animate-pulse w-24" />
                          </td>
                        ))}
                      </tr>
                    ))
                  : lojas.map((l) => (
                      <tr key={l.id} className="border-b border-border last:border-0 hover:bg-surface/50 transition-colors">
                        <td className="px-5 py-3">
                          <div className="flex items-center gap-2">
                            <div className="w-7 h-7 rounded-lg bg-primary/10 flex items-center justify-center text-xs font-bold text-primary">
                              {l.nome.charAt(0)}
                            </div>
                            <span className="text-sm font-medium text-text-primary">{l.nome}</span>
                          </div>
                        </td>
                        <td className="px-5 py-3 text-sm text-text-secondary">{l.cidade}</td>
                        <td className="px-5 py-3 text-sm text-text-secondary">—</td>
                        <td className="px-5 py-3">
                          <span className="inline-flex items-center gap-1 px-2 py-0.5 rounded-full text-xs font-medium bg-success-light text-success">
                            ● Open
                          </span>
                        </td>
                        <td className="px-5 py-3">
                          {syncResult?.id === l.id ? (
                            <span className={`text-xs font-medium ${syncResult.ok ? "text-success" : "text-danger"}`}>
                              {syncResult.ok ? "✓ Sincronizado" : "✗ Falhou"}
                            </span>
                          ) : (
                            <button
                              onClick={() => handleSync(l.id)}
                              disabled={syncingId === l.id}
                              className="flex items-center gap-1.5 px-2.5 py-1 rounded-lg bg-primary/10 text-primary text-xs font-medium hover:bg-primary/20 transition-colors disabled:opacity-50"
                            >
                              <RefreshCw size={11} className={syncingId === l.id ? "animate-spin" : ""} />
                              {syncingId === l.id ? "A sincronizar…" : "Sync"}
                            </button>
                          )}
                        </td>
                        <td className="px-5 py-3">
                          <button
                            onClick={() => openDetails(l)}
                            className="flex items-center gap-1 text-xs text-primary hover:underline font-medium"
                          >
                            <Eye size={12} />
                            View Details
                          </button>
                        </td>
                      </tr>
                    ))}
              </tbody>
            </table>
            {lojas.length > 4 && (
              <div className="px-5 py-3 border-t border-border">
                <button className="text-xs text-primary hover:underline">
                  Show All {lojas.length} Stores
                </button>
              </div>
            )}
          </div>

          {/* Expansion card */}
          <div className="flex flex-col gap-4">
            <div className="bg-sidebar rounded-xl p-5 flex-1 flex flex-col gap-3">
              <div className="flex items-center gap-2">
                <span className="px-2 py-0.5 rounded-full text-[10px] font-bold bg-primary text-white">FAST GROWTH</span>
                <span className="px-2 py-0.5 rounded-full text-[10px] font-bold bg-success/20 text-success">STRATEGIC</span>
              </div>
              <h3 className="text-white text-lg font-bold">Expansion Plan 2024</h3>
              <p className="text-white/50 text-xs leading-relaxed">
                Targeting 10 new flagship locations across the Southern hemisphere by Q4. Explore the architectural pipeline.
              </p>
              <button className="flex items-center gap-1.5 px-3 py-1.5 rounded-lg bg-white/10 text-white text-xs font-medium hover:bg-white/20 transition-colors w-fit mt-auto">
                View Blueprint →
              </button>
            </div>
            <div className="bg-card rounded-xl border border-border p-5">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide mb-3">Next Opening</p>
              <p className="text-sm font-semibold text-text-primary">Porto Alegre Flagship</p>
              <p className="text-xs text-text-secondary mt-1">Currently in construction — 67% complete</p>
              <div className="mt-3 h-1.5 bg-surface rounded-full overflow-hidden">
                <div className="h-full bg-primary rounded-full" style={{ width: "67%" }} />
              </div>
              <div className="flex items-center justify-between mt-2">
                <p className="text-[10px] text-text-muted">Due Oct 2026</p>
                <p className="text-[10px] text-text-muted">Phase 2/3</p>
              </div>
            </div>
          </div>
        </div>
      </main>

      {/* Store Details Modal */}
      {selectedLoja && (
        <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
          <div className="bg-card rounded-xl border border-border p-6 w-full max-w-md">
            <div className="flex items-center justify-between mb-5">
              <div className="flex items-center gap-3">
                <div className="w-10 h-10 rounded-xl bg-primary/10 flex items-center justify-center text-lg font-bold text-primary">
                  {selectedLoja.nome.charAt(0)}
                </div>
                <div>
                  <h2 className="font-semibold text-text-primary">{selectedLoja.nome}</h2>
                  <p className="text-xs text-text-muted mt-0.5">{selectedLoja.cadeaNome}</p>
                </div>
              </div>
              <button
                onClick={() => setSelectedLoja(null)}
                className="text-text-muted hover:text-text-primary transition-colors"
              >
                <X size={16} />
              </button>
            </div>

            {/* Location */}
            <div className="flex items-center gap-2 px-3 py-2.5 bg-surface rounded-lg mb-4">
              <MapPin size={14} className="text-text-muted shrink-0" />
              <span className="text-sm text-text-secondary">{selectedLoja.cidade}</span>
              <span className="inline-flex px-1.5 py-0.5 rounded text-[10px] font-medium bg-success-light text-success ml-auto">
                ● Open
              </span>
            </div>

            {/* Stats */}
            <div className="grid grid-cols-2 gap-3">
              <div className="bg-surface rounded-lg p-4">
                <div className="flex items-center gap-2 mb-2">
                  <ShoppingBag size={14} className="text-primary" />
                  <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Total Sales</p>
                </div>
                {statsLoading ? (
                  <div className="h-7 bg-card rounded animate-pulse w-16" />
                ) : (
                  <p className="text-2xl font-bold text-text-primary">{lojaStats?.totalVendas ?? "—"}</p>
                )}
              </div>
              <div className="bg-surface rounded-lg p-4">
                <div className="flex items-center gap-2 mb-2">
                  <DollarSign size={14} className="text-success" />
                  <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Revenue</p>
                </div>
                {statsLoading ? (
                  <div className="h-7 bg-card rounded animate-pulse w-24" />
                ) : (
                  <p className="text-2xl font-bold text-text-primary">
                    {lojaStats
                      ? `€${lojaStats.faturacaoTotal.toLocaleString("pt-PT", { minimumFractionDigits: 2 })}`
                      : "—"}
                  </p>
                )}
              </div>
            </div>

            <button
              onClick={() => setSelectedLoja(null)}
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
