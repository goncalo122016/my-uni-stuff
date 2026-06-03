"use client";

import { useEffect, useState } from "react";
import { TrendingUp, AlertTriangle, ShoppingBag, Download, CheckCircle } from "lucide-react";
import dynamic from "next/dynamic";
import { useRouter } from "next/navigation";
import Header from "@/components/layout/Header";
import { useAuth } from "@/lib/auth";
import { api } from "@/lib/api";
import type { EstatisticasLojaResponse, VendaResponse } from "@/lib/api";

const SalesBarChart = dynamic(() => import("@/components/charts/SalesBarChart"), { ssr: false });

const MOCK_THIS_WEEK = [42, 58, 35, 67, 89, 112, 95];
const MOCK_LAST_WEEK = [38, 52, 48, 55, 72, 98, 81];

export default function DashboardPage() {
  const { user } = useAuth();
  const router = useRouter();
  const [stats, setStats] = useState<EstatisticasLojaResponse | null>(null);
  const [vendas, setVendas] = useState<VendaResponse[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    if (!user) return;
    const today = new Date().toISOString().slice(0, 10);
    const weekAgo = new Date(Date.now() - 7 * 86400000).toISOString().slice(0, 10);

    if (user.backend === "/loja-api") {
      api.vendas.historico(weekAgo, today)
        .then((h) => {
          setStats({ lojaId: h.lojaId, lojaNome: "Loja Local", totalVendas: h.totalVendas, faturacaoTotal: h.faturacao });
          setVendas(h.vendas.slice(0, 5));
        })
        .catch(() => {})
        .finally(() => setLoading(false));
    } else {
      api.cadeia.list()
        .then(async (cadeias) => {
          if (cadeias.length > 0) {
            const cs = await api.central.estatisticas(cadeias[0].id, weekAgo, today);
            setStats({
              lojaId: cs.cadeiaId,
              lojaNome: cs.cadeaNome,
              totalVendas: cs.totalVendas,
              faturacaoTotal: cs.faturacaoTotal,
            });
          }
        })
        .catch(() => {})
        .finally(() => setLoading(false));
    }
  }, [user]);

  const greeting = () => {
    const h = new Date().getHours();
    if (h < 12) return "Bom dia";
    if (h < 18) return "Boa tarde";
    return "Boa noite";
  };

  const cargoLabel: Record<string, string> = {
    ADMINISTRADOR_CENTRAL: "Diretor",
    GERENTE: "Gerente",
    FUNCIONARIO: "Funcionário",
  };

  return (
    <div className="flex flex-col flex-1">
      <Header
        placeholder="Pesquisar em BelaVista..."
        action={
          <button className="flex items-center gap-2 px-4 py-2 rounded-lg bg-primary text-white text-sm font-medium hover:bg-primary-dark transition-colors">
            <Download size={14} />
            EXPORTAR RELATÓRIO
          </button>
        }
      />

      <main className="flex-1 p-6 flex flex-col gap-6">
        {/* Greeting */}
        <div>
          <h1 className="text-2xl font-bold text-text-primary">
            {greeting()}, {cargoLabel[user?.cargo ?? ""] ?? ""}
          </h1>
          <p className="text-text-secondary text-sm mt-0.5">
            {new Date().toLocaleDateString("pt-PT", { weekday: "long", year: "numeric", month: "long", day: "numeric" })}
          </p>
        </div>

        <div className="grid grid-cols-3 gap-4 xl:grid-cols-4">
          {/* Stat cards */}
          <div className="col-span-3 grid grid-cols-3 gap-4">
            <StatCard
              label="REVENUE"
              value={loading ? "—" : `R$ ${(stats?.faturacaoTotal ?? 0).toLocaleString("pt-PT", { minimumFractionDigits: 2 })}`}
              trend="+45%"
              trendUp
              icon={<TrendingUp size={18} className="text-success" />}
              color="success"
            />
            <StatCard
              label="CRITICAL STOCK"
              value={loading ? "—" : "42 SKUs"}
              trend="Attention Required"
              trendUp={false}
              icon={<AlertTriangle size={18} className="text-warning" />}
              color="warning"
            />
            <StatCard
              label="TOTAL TRANSACTIONS"
              value={loading ? "—" : String(stats?.totalVendas ?? 0)}
              trend="Last 24h"
              trendUp
              icon={<ShoppingBag size={18} className="text-primary" />}
              color="primary"
            />
          </div>

          {/* Training card */}
          <div className="col-span-3 xl:col-span-1 row-span-2 bg-sidebar rounded-xl p-5 flex flex-col gap-3">
            <p className="text-white/50 text-xs font-medium uppercase tracking-wide">Training Workshop</p>
            <h3 className="text-white text-lg font-bold leading-snug">Advanced Leadership</h3>
            <p className="text-white/50 text-xs">Próxima aula: Terça, 14:00 — Sala das Armas</p>
            <button className="mt-auto w-full py-2.5 rounded-lg bg-primary text-white text-sm font-semibold hover:bg-primary-dark transition-colors">
              CONFIRMAR PRESENÇA
            </button>
          </div>

          {/* Chart */}
          <div className="col-span-3 bg-card rounded-xl p-5 border border-border">
            <div className="flex items-center justify-between mb-4">
              <div>
                <h3 className="font-semibold text-text-primary">Weekly Sales Performance</h3>
                <p className="text-xs text-text-secondary mt-0.5">Track returns across all comparison stores</p>
              </div>
            </div>
            <SalesBarChart thisWeek={MOCK_THIS_WEEK} lastWeek={MOCK_LAST_WEEK} />
          </div>
        </div>

        <div className="grid grid-cols-3 gap-4">
          {/* Recent Activity */}
          <div className="col-span-2 bg-card rounded-xl border border-border">
            <div className="flex items-center justify-between px-5 py-4 border-b border-border">
              <h3 className="font-semibold text-text-primary">Recent Activity</h3>
              <button onClick={() => router.push("/reports")} className="text-xs text-primary hover:underline">See all →</button>
            </div>
            <table className="w-full">
              <thead>
                <tr className="border-b border-border">
                  <th className="text-left px-5 py-3 text-xs font-medium text-text-muted uppercase tracking-wide">User</th>
                  <th className="text-left px-5 py-3 text-xs font-medium text-text-muted uppercase tracking-wide">Action</th>
                  <th className="text-left px-5 py-3 text-xs font-medium text-text-muted uppercase tracking-wide">Time</th>
                </tr>
              </thead>
              <tbody>
                {vendas.length > 0
                  ? vendas.map((v) => (
                      <tr key={v.id} className="border-b border-border last:border-0 hover:bg-surface/50 transition-colors">
                        <td className="px-5 py-3 flex items-center gap-2">
                          <div className="w-7 h-7 rounded-full bg-primary/10 flex items-center justify-center text-xs font-bold text-primary">
                            {v.funcionarioNome.charAt(0)}
                          </div>
                          <span className="text-sm font-medium text-text-primary">{v.funcionarioNome}</span>
                        </td>
                        <td className="px-5 py-3 text-sm text-text-secondary">
                          Venda #{v.id} — R$ {v.total.toFixed(2)}
                        </td>
                        <td className="px-5 py-3 text-xs text-text-muted">
                          {new Date(v.dataHora).toLocaleTimeString("pt-PT", { hour: "2-digit", minute: "2-digit" })}
                        </td>
                      </tr>
                    ))
                  : MOCK_ACTIVITY.map((a) => (
                      <tr key={a.id} className="border-b border-border last:border-0 hover:bg-surface/50 transition-colors">
                        <td className="px-5 py-3">
                          <div className="flex items-center gap-2">
                            <div className="w-7 h-7 rounded-full bg-primary/10 flex items-center justify-center text-xs font-bold text-primary">
                              {a.user[0]}
                            </div>
                            <span className="text-sm font-medium text-text-primary">{a.user}</span>
                          </div>
                        </td>
                        <td className="px-5 py-3 text-sm text-text-secondary">{a.action}</td>
                        <td className="px-5 py-3 text-xs text-text-muted">{a.time}</td>
                      </tr>
                    ))}
              </tbody>
            </table>
          </div>

          {/* Store Health Score */}
          <div className="bg-card rounded-xl border border-border p-5 flex flex-col items-center justify-center gap-4">
            <p className="text-xs font-medium text-text-muted uppercase tracking-wide self-start">STORE HEALTH SCORE</p>
            <div className="relative w-28 h-28">
              <svg viewBox="0 0 100 100" className="w-full h-full -rotate-90">
                <circle cx="50" cy="50" r="42" fill="none" stroke="#e5e7eb" strokeWidth="10" />
                <circle
                  cx="50" cy="50" r="42" fill="none" stroke="#10b981" strokeWidth="10"
                  strokeDasharray="264" strokeDashoffset="26" strokeLinecap="round"
                />
              </svg>
              <div className="absolute inset-0 flex flex-col items-center justify-center">
                <CheckCircle size={20} className="text-success" />
                <span className="text-lg font-bold text-text-primary mt-0.5">90%</span>
              </div>
            </div>
            <div className="text-center">
              <p className="text-sm font-semibold text-success">Excellent Performance</p>
              <p className="text-xs text-text-muted mt-0.5">All metrics within targets</p>
              <button onClick={() => router.push("/reports")} className="text-xs text-primary hover:underline mt-2">See more →</button>
            </div>
          </div>
        </div>
      </main>
    </div>
  );
}

function StatCard({
  label, value, trend, trendUp, icon, color,
}: {
  label: string; value: string; trend: string; trendUp: boolean;
  icon: React.ReactNode; color: "success" | "warning" | "primary";
}) {
  const bg = { success: "bg-success-light", warning: "bg-warning-light", primary: "bg-primary-light" }[color];
  const tc = { success: "text-success", warning: "text-warning", primary: "text-primary" }[color];
  return (
    <div className="bg-card rounded-xl border border-border p-5">
      <div className="flex items-center justify-between mb-3">
        <p className="text-xs font-medium text-text-muted uppercase tracking-wide">{label}</p>
        <div className={`w-8 h-8 rounded-lg flex items-center justify-center ${bg}`}>{icon}</div>
      </div>
      <p className="text-2xl font-bold text-text-primary">{value}</p>
      <p className={`text-xs mt-1 ${trendUp ? tc : "text-warning"}`}>{trend}</p>
    </div>
  );
}

const MOCK_ACTIVITY = [
  { id: 1, user: "Marina A.", action: "Book sales — $142", time: "2m ago" },
  { id: 2, user: "Sandra L.", action: "Closed Sale #635", time: "8m ago" },
  { id: 3, user: "Ricardo C.", action: "New employee added", time: "20m ago" },
  { id: 4, user: "João R.", action: "Bulk inventory CSV", time: "1h ago" },
];
