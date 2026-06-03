"use client";

import { useEffect, useState, useCallback } from "react";
import { Download, TrendingUp, ShoppingBag, BarChart2 } from "lucide-react";
import dynamic from "next/dynamic";
import Header from "@/components/layout/Header";
import { api } from "@/lib/api";
import { useAuth } from "@/lib/auth";
import type { EstatisticasLojaResponse, EstatisticasCadeiaResponse, HistoricoResponse } from "@/lib/api";

const PerformanceLineChart = dynamic(() => import("@/components/charts/PerformanceLineChart"), { ssr: false });
const SalesBarChart = dynamic(() => import("@/components/charts/SalesBarChart"), { ssr: false });

const MOCK_PERF = [
  { name: "Mon", Revenue: 3200, Orders: 28 },
  { name: "Tue", Revenue: 4100, Orders: 35 },
  { name: "Wed", Revenue: 2800, Orders: 22 },
  { name: "Thu", Revenue: 5200, Orders: 44 },
  { name: "Fri", Revenue: 6800, Orders: 58 },
  { name: "Sat", Revenue: 8100, Orders: 72 },
  { name: "Sun", Revenue: 5400, Orders: 46 },
];

const MOCK_THIS_WEEK = [42, 58, 35, 67, 89, 112, 95];
const MOCK_LAST_WEEK = [38, 52, 48, 55, 72, 98, 81];

export default function ReportsPage() {
  const { user } = useAuth();
  const [lojaStats, setLojaStats] = useState<EstatisticasLojaResponse | null>(null);
  const [cadeiaStats, setCadeiaStats] = useState<EstatisticasCadeiaResponse | null>(null);
  const [historico, setHistorico] = useState<HistoricoResponse | null>(null);
  const [loading, setLoading] = useState(true);

  const loadData = useCallback(async () => {
    if (!user) return;
    const today = new Date().toISOString().slice(0, 10);
    const monthAgo = new Date(Date.now() - 30 * 86400000).toISOString().slice(0, 10);
    try {
      if (user.backend === "/loja-api") {
        const h = await api.vendas.historico(monthAgo, today).catch(() => null);
        if (h) {
          setLojaStats({ lojaId: h.lojaId, lojaNome: "Loja Local", totalVendas: h.totalVendas, faturacaoTotal: h.faturacao });
          setHistorico(h);
        }
      } else {
        const [cs, h] = await Promise.all([
          api.cadeia.list().then((list) => list.length > 0 ? api.central.estatisticas(list[0].id, monthAgo, today).catch(() => null) : null),
          api.lojaVendas.historico(monthAgo, today).catch(() => null),
        ]);
        setCadeiaStats(cs);
        if (cs) {
          setLojaStats({ lojaId: 0, lojaNome: cs.cadeaNome, totalVendas: cs.totalVendas, faturacaoTotal: cs.faturacaoTotal });
        }
        setHistorico(h);
      }
    } finally {
      setLoading(false);
    }
  }, [user]);

  // Initial load + auto-refresh every 60 s + refresh on tab focus
  useEffect(() => {
    loadData();
    const interval = setInterval(loadData, 60_000);
    window.addEventListener("focus", loadData);
    return () => {
      clearInterval(interval);
      window.removeEventListener("focus", loadData);
    };
  }, [loadData]);

  function handleExportCSV() {
    const today = new Date().toLocaleDateString("pt-PT");
    const monthAgo = new Date(Date.now() - 30 * 86400000).toLocaleDateString("pt-PT");

    const rows: string[][] = [
      ["BelaVista — Relatório de Vendas"],
      [`Período: ${monthAgo} — ${today}`],
      [`Gerado em: ${new Date().toLocaleString("pt-PT")}`],
      [],
      ["RESUMO"],
      ["Receita Total", `€${(lojaStats?.faturacaoTotal ?? 0).toFixed(2)}`],
      ["Total de Vendas", String(lojaStats?.totalVendas ?? 0)],
      ["Ticket Médio", avgTicket !== null ? `€${avgTicket.toFixed(2)}` : "—"],
      ...(cadeiaStats ? [
        [],
        ["RESUMO DA CADEIA"],
        ["Cadeia", cadeiaStats.cadeaNome],
        ["Receita Total (Cadeia)", `€${cadeiaStats.faturacaoTotal.toFixed(2)}`],
        ["Total de Vendas (Cadeia)", String(cadeiaStats.totalVendas)],
        [],
        ["LOJAS", "Receita", "Vendas"],
        ...cadeiaStats.porLoja.map((l) => [l.lojaNome, `€${l.faturacaoTotal.toFixed(2)}`, String(l.totalVendas)]),
      ] : []),
    ];

    if (historico && historico.vendas.length > 0) {
      rows.push(
        [],
        ["TRANSAÇÕES RECENTES"],
        ["ID", "Funcionário", "Cliente", "Total (€)", "Items", "Estado", "Data/Hora"],
        ...historico.vendas.map((v) => [
          `#${v.id}`,
          v.funcionarioNome,
          v.clienteNome ?? "—",
          v.total.toFixed(2),
          String(v.linhas.length),
          v.estado,
          new Date(v.dataHora).toLocaleString("pt-PT"),
        ])
      );
    }

    const csv = rows.map((r) => r.map((c) => `"${c.replace(/"/g, '""')}"`).join(",")).join("\r\n");
    const blob = new Blob(["﻿" + csv], { type: "text/csv;charset=utf-8;" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = `relatorio-belavista-${new Date().toISOString().slice(0, 10)}.csv`;
    a.click();
    URL.revokeObjectURL(url);
  }

  const avgTicket = lojaStats && lojaStats.totalVendas > 0
    ? lojaStats.faturacaoTotal / lojaStats.totalVendas
    : null;

  return (
    <div className="flex flex-col flex-1">
      <Header
        placeholder="Search reports..."
        action={
          <button
            onClick={handleExportCSV}
            disabled={loading}
            className="flex items-center gap-2 px-4 py-2 rounded-lg bg-primary text-white text-sm font-medium hover:bg-primary-dark transition-colors disabled:opacity-50"
          >
            <Download size={14} />
            EXPORTAR CSV
          </button>
        }
      />

      <main className="flex-1 p-6 flex flex-col gap-6">
        <div>
          <h1 className="text-2xl font-bold text-text-primary">Strategic Analytics</h1>
          <p className="text-text-secondary text-sm mt-0.5">
            Performance insights and financial overview for your store.
          </p>
        </div>

        {/* KPI cards */}
        <div className="grid grid-cols-4 gap-4">
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Total Revenue</p>
              <div className="w-8 h-8 rounded-lg bg-success-light flex items-center justify-center">
                <TrendingUp size={16} className="text-success" />
              </div>
            </div>
            <p className="text-2xl font-bold text-text-primary">
              {loading ? "—" : `€${(lojaStats?.faturacaoTotal ?? 0).toLocaleString("pt-PT", { minimumFractionDigits: 2 })}`}
            </p>
            <p className="text-xs text-success mt-1">+12.5% vs last month</p>
          </div>
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Total Orders</p>
              <div className="w-8 h-8 rounded-lg bg-primary-light flex items-center justify-center">
                <ShoppingBag size={16} className="text-primary" />
              </div>
            </div>
            <p className="text-2xl font-bold text-text-primary">
              {loading ? "—" : (lojaStats?.totalVendas ?? 0).toLocaleString()}
            </p>
            <p className="text-xs text-primary mt-1">Last 30 days</p>
          </div>
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Avg. Ticket</p>
              <div className="w-8 h-8 rounded-lg bg-warning-light flex items-center justify-center">
                <BarChart2 size={16} className="text-warning" />
              </div>
            </div>
            <p className="text-2xl font-bold text-text-primary">
              {loading ? "—" : avgTicket !== null ? `€${avgTicket.toFixed(2)}` : "—"}
            </p>
            <p className="text-xs text-text-muted mt-1">Per transaction</p>
          </div>
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Chain Revenue</p>
              <div className="w-8 h-8 rounded-lg bg-primary-light flex items-center justify-center">
                <TrendingUp size={16} className="text-primary" />
              </div>
            </div>
            <p className="text-2xl font-bold text-text-primary">
              {loading || !cadeiaStats
                ? "—"
                : `€${(cadeiaStats.faturacaoTotal).toLocaleString("pt-PT", { maximumFractionDigits: 0 })}`}
            </p>
            <p className="text-xs text-success mt-1">All stores combined</p>
          </div>
        </div>

        <div className="grid grid-cols-3 gap-4">
          {/* Performance chart */}
          <div className="col-span-2 bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-4">
              <div>
                <h3 className="font-semibold text-text-primary">Revenue & Orders (7 Days)</h3>
                <p className="text-xs text-text-secondary mt-0.5">Daily performance trend</p>
              </div>
              <div className="flex items-center gap-1">
                {["7D", "30D", "90D"].map((p) => (
                  <button key={p} className={`px-2.5 py-1 rounded text-xs font-medium transition-colors ${p === "7D" ? "bg-primary text-white" : "border border-border text-text-muted hover:bg-surface"}`}>
                    {p}
                  </button>
                ))}
              </div>
            </div>
            <PerformanceLineChart data={MOCK_PERF} />
          </div>

          {/* Store breakdown */}
          <div className="bg-card rounded-xl border border-border p-5 flex flex-col gap-4">
            <h3 className="font-semibold text-text-primary">Revenue by Store</h3>
            {loading || !cadeiaStats
              ? Array.from({ length: 3 }).map((_, i) => (
                  <div key={i} className="h-12 bg-surface rounded-lg animate-pulse" />
                ))
              : cadeiaStats.porLoja.map((loja, idx) => {
                  const max = Math.max(...cadeiaStats.porLoja.map((l) => l.faturacaoTotal), 1);
                  const pct = Math.round((loja.faturacaoTotal / max) * 100);
                  const colors = ["bg-primary", "bg-success", "bg-warning", "bg-danger"];
                  return (
                    <div key={loja.lojaId}>
                      <div className="flex items-center justify-between mb-1">
                        <p className="text-xs font-medium text-text-primary truncate max-w-32">{loja.lojaNome}</p>
                        <p className="text-xs font-semibold text-text-primary">
                          €{loja.faturacaoTotal.toLocaleString("pt-PT", { maximumFractionDigits: 0 })}
                        </p>
                      </div>
                      <div className="h-1.5 bg-surface rounded-full overflow-hidden">
                        <div className={`h-full rounded-full ${colors[idx % colors.length]}`} style={{ width: `${pct}%` }} />
                      </div>
                    </div>
                  );
                })}
            {!loading && !cadeiaStats && (
              <p className="text-sm text-text-muted text-center py-4">No chain data available.</p>
            )}
          </div>
        </div>

        {/* Weekly comparison chart */}
        <div className="bg-card rounded-xl border border-border p-5">
          <div className="flex items-center justify-between mb-4">
            <div>
              <h3 className="font-semibold text-text-primary">Weekly Sales Comparison</h3>
              <p className="text-xs text-text-secondary mt-0.5">This week vs last week</p>
            </div>
          </div>
          <SalesBarChart thisWeek={MOCK_THIS_WEEK} lastWeek={MOCK_LAST_WEEK} />
        </div>

        {/* Recent transactions */}
        {historico && historico.vendas.length > 0 && (
          <div className="bg-card rounded-xl border border-border">
            <div className="flex items-center justify-between px-5 py-4 border-b border-border">
              <h3 className="font-semibold text-text-primary">Recent Transactions</h3>
              <p className="text-xs text-text-muted">{historico.periodo}</p>
            </div>
            <table className="w-full">
              <thead>
                <tr className="border-b border-border bg-surface/50">
                  {["ID", "Employee", "Total", "Items", "Status", "Date"].map((h) => (
                    <th key={h} className="text-left px-5 py-3 text-xs font-medium text-text-muted uppercase tracking-wide">
                      {h}
                    </th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {historico.vendas.slice(0, 8).map((v) => (
                  <tr key={v.id} className="border-b border-border last:border-0 hover:bg-surface/40 transition-colors">
                    <td className="px-5 py-3 text-sm font-mono text-text-muted">#{v.id}</td>
                    <td className="px-5 py-3">
                      <div className="flex items-center gap-2">
                        <div className="w-6 h-6 rounded-full bg-primary/10 flex items-center justify-center text-[10px] font-bold text-primary shrink-0">
                          {v.funcionarioNome.charAt(0)}
                        </div>
                        <span className="text-sm text-text-primary">{v.funcionarioNome}</span>
                      </div>
                    </td>
                    <td className="px-5 py-3 text-sm font-semibold text-text-primary">€{v.total.toFixed(2)}</td>
                    <td className="px-5 py-3 text-sm text-text-secondary">{v.linhas.length} items</td>
                    <td className="px-5 py-3">
                      <span className={`inline-flex px-2 py-0.5 rounded-full text-[10px] font-medium ${
                        v.estado === "CONCLUIDA" ? "bg-success-light text-success" :
                        v.estado === "CANCELADA" ? "bg-danger-light text-danger" :
                        "bg-warning-light text-warning"
                      }`}>
                        {v.estado}
                      </span>
                    </td>
                    <td className="px-5 py-3 text-xs text-text-muted">
                      {new Date(v.dataHora).toLocaleString("pt-PT", { dateStyle: "short", timeStyle: "short" })}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}
      </main>
    </div>
  );
}
