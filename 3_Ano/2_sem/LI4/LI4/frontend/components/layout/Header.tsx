"use client";

import { useEffect, useRef, useState } from "react";
import { Bell, Settings, Search, X, LogOut, User, ChevronRight, AlertTriangle, Package, CheckCircle } from "lucide-react";
import { useRouter } from "next/navigation";
import { useAuth } from "@/lib/auth";
import { api } from "@/lib/api";
import type { StockResponse } from "@/lib/api";

interface HeaderProps {
  placeholder?: string;
  action?: React.ReactNode;
}

export default function Header({ placeholder = "Search...", action }: HeaderProps) {
  const { user, logout } = useAuth();
  const router = useRouter();

  const [notifOpen, setNotifOpen] = useState(false);
  const [settingsOpen, setSettingsOpen] = useState(false);
  const [alerts, setAlerts] = useState<StockResponse[]>([]);

  const notifRef = useRef<HTMLDivElement>(null);
  const settingsRef = useRef<HTMLDivElement>(null);

  // Load stock alerts once on mount (only for loja users)
  useEffect(() => {
    if (!user || user.backend !== "/loja-api") return;
    api.stock.list()
      .then((stocks) => setAlerts(stocks.filter((s: StockResponse) => s.quantidade <= s.quantidadeMinima)))
      .catch(() => {});
  }, [user]);

  // Close dropdowns on outside click
  useEffect(() => {
    function onMouseDown(e: MouseEvent) {
      if (notifRef.current && !notifRef.current.contains(e.target as Node)) setNotifOpen(false);
      if (settingsRef.current && !settingsRef.current.contains(e.target as Node)) setSettingsOpen(false);
    }
    document.addEventListener("mousedown", onMouseDown);
    return () => document.removeEventListener("mousedown", onMouseDown);
  }, []);

  function handleLogout() {
    logout();
    router.push("/login");
  }

  const initials = user?.nome.split(" ").slice(0, 2).map((w) => w[0]).join("").toUpperCase() ?? "U";

  const cargoLabel: Record<string, string> = {
    ADMINISTRADOR_CENTRAL: "Director",
    GERENTE: "Manager",
    FUNCIONARIO: "Associate",
  };

  const outOfStock = alerts.filter((a) => a.quantidade === 0);
  const lowStock   = alerts.filter((a) => a.quantidade > 0);

  return (
    <header className="flex items-center gap-3 px-6 py-3.5 bg-card border-b border-border">
      {/* Search */}
      <div className="flex items-center gap-2 flex-1 max-w-sm bg-surface rounded-lg px-3 py-2 border border-border">
        <Search size={14} className="text-text-secondary shrink-0" />
        <input
          type="text"
          placeholder={placeholder}
          className="bg-transparent text-sm text-text-primary placeholder-text-muted outline-none w-full"
        />
      </div>

      <div className="flex items-center gap-2 ml-auto">
        {action}

        {/* ── Notifications ─────────────────────────────────────────────── */}
        <div ref={notifRef} className="relative">
          <button
            onClick={() => { setNotifOpen((v) => !v); setSettingsOpen(false); }}
            className="relative p-2 rounded-lg hover:bg-surface transition-colors"
          >
            <Bell size={18} className="text-text-secondary" />
            {alerts.length > 0 && (
              <span className="absolute top-1 right-1 min-w-[16px] h-4 rounded-full bg-danger flex items-center justify-center text-[9px] font-bold text-white px-0.5">
                {alerts.length > 9 ? "9+" : alerts.length}
              </span>
            )}
          </button>

          {notifOpen && (
            <div className="absolute right-0 top-full mt-2 w-80 bg-card border border-border rounded-xl shadow-lg z-50 overflow-hidden">
              <div className="flex items-center justify-between px-4 py-3 border-b border-border">
                <h3 className="text-sm font-semibold text-text-primary">Notifications</h3>
                <div className="flex items-center gap-2">
                  {alerts.length > 0 && (
                    <span className="px-1.5 py-0.5 rounded-full bg-danger-light text-danger text-[10px] font-bold">
                      {alerts.length} alerts
                    </span>
                  )}
                  <button onClick={() => setNotifOpen(false)} className="text-text-muted hover:text-text-primary">
                    <X size={14} />
                  </button>
                </div>
              </div>

              <div className="max-h-72 overflow-y-auto">
                {alerts.length === 0 ? (
                  <div className="flex flex-col items-center gap-2 py-8">
                    <CheckCircle size={28} className="text-success" />
                    <p className="text-sm font-medium text-text-primary">All clear!</p>
                    <p className="text-xs text-text-muted">No stock issues detected.</p>
                  </div>
                ) : (
                  <>
                    {outOfStock.length > 0 && (
                      <>
                        <div className="px-4 py-1.5 bg-danger/5">
                          <p className="text-[10px] font-bold text-danger uppercase tracking-wide">Out of Stock</p>
                        </div>
                        {outOfStock.map((a) => (
                          <div key={a.produtoId} className="flex items-center gap-3 px-4 py-3 border-b border-border hover:bg-surface/50 transition-colors">
                            <div className="w-7 h-7 rounded-lg bg-danger-light flex items-center justify-center shrink-0">
                              <Package size={12} className="text-danger" />
                            </div>
                            <div className="flex-1 min-w-0">
                              <p className="text-xs font-medium text-text-primary truncate">{a.produtoNome}</p>
                              <p className="text-[10px] text-danger">Out of stock — reorder now</p>
                            </div>
                          </div>
                        ))}
                      </>
                    )}
                    {lowStock.length > 0 && (
                      <>
                        <div className="px-4 py-1.5 bg-warning/5">
                          <p className="text-[10px] font-bold text-warning uppercase tracking-wide">Low Stock</p>
                        </div>
                        {lowStock.map((a) => (
                          <div key={a.produtoId} className="flex items-center gap-3 px-4 py-3 border-b border-border last:border-0 hover:bg-surface/50 transition-colors">
                            <div className="w-7 h-7 rounded-lg bg-warning-light flex items-center justify-center shrink-0">
                              <AlertTriangle size={12} className="text-warning" />
                            </div>
                            <div className="flex-1 min-w-0">
                              <p className="text-xs font-medium text-text-primary truncate">{a.produtoNome}</p>
                              <p className="text-[10px] text-text-muted">{a.quantidade} units — min {a.quantidadeMinima}</p>
                            </div>
                          </div>
                        ))}
                      </>
                    )}
                  </>
                )}
              </div>

              <div className="px-4 py-2.5 border-t border-border">
                <button
                  onClick={() => { router.push("/inventory"); setNotifOpen(false); }}
                  className="text-xs text-primary hover:underline font-medium"
                >
                  View all in Inventory →
                </button>
              </div>
            </div>
          )}
        </div>

        {/* ── Settings ──────────────────────────────────────────────────── */}
        <div ref={settingsRef} className="relative">
          <button
            onClick={() => { setSettingsOpen((v) => !v); setNotifOpen(false); }}
            className="p-2 rounded-lg hover:bg-surface transition-colors"
          >
            <Settings
              size={18}
              className={`text-text-secondary transition-transform duration-300 ${settingsOpen ? "rotate-45" : ""}`}
            />
          </button>

          {settingsOpen && (
            <div className="absolute right-0 top-full mt-2 w-56 bg-card border border-border rounded-xl shadow-lg z-50 overflow-hidden">
              <div className="px-4 py-3 border-b border-border">
                <p className="text-xs font-semibold text-text-primary">{user?.nome}</p>
                <p className="text-[10px] text-text-muted mt-0.5">{user ? cargoLabel[user.cargo] : ""}</p>
              </div>

              <div className="py-1">
                <button className="w-full flex items-center justify-between px-4 py-2.5 text-sm text-text-secondary hover:bg-surface transition-colors">
                  <div className="flex items-center gap-2.5">
                    <User size={14} />
                    Profile
                  </div>
                  <ChevronRight size={12} className="text-text-muted" />
                </button>
                <button
                  onClick={() => { router.push("/reports"); setSettingsOpen(false); }}
                  className="w-full flex items-center justify-between px-4 py-2.5 text-sm text-text-secondary hover:bg-surface transition-colors"
                >
                  <div className="flex items-center gap-2.5">
                    <Settings size={14} />
                    Reports
                  </div>
                  <ChevronRight size={12} className="text-text-muted" />
                </button>
              </div>

              <div className="py-1 border-t border-border">
                <button
                  onClick={handleLogout}
                  className="w-full flex items-center gap-2.5 px-4 py-2.5 text-sm text-danger hover:bg-danger-light transition-colors"
                >
                  <LogOut size={14} />
                  Sign Out
                </button>
              </div>
            </div>
          )}
        </div>

        {/* Avatar */}
        <div className="flex items-center gap-2 pl-2 border-l border-border">
          <div className="w-8 h-8 rounded-full bg-primary flex items-center justify-center text-white text-xs font-bold">
            {initials}
          </div>
          <div className="hidden sm:block">
            <p className="text-xs font-medium text-text-primary leading-none">{user?.nome ?? "—"}</p>
            <p className="text-[10px] text-text-muted mt-0.5">{user ? cargoLabel[user.cargo] : ""}</p>
          </div>
        </div>
      </div>
    </header>
  );
}
