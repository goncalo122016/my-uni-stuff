"use client";

import { useState } from "react";
import { useRouter } from "next/navigation";
import { Store } from "lucide-react";
import { api } from "@/lib/api";
import { useAuth } from "@/lib/auth";

export default function LoginPage() {
  const router = useRouter();
  const { login } = useAuth();
  const [numero, setNumero] = useState("");
  const [senha, setSenha] = useState("");
  const [error, setError] = useState("");
  const [loading, setLoading] = useState(false);

  async function handleSubmit(e: React.FormEvent) {
    e.preventDefault();
    setError("");
    setLoading(true);
    try {
      const { info, backend } = await api.auth.login(numero, senha);
      login(numero, senha, info, backend);
      router.push("/dashboard");
    } catch (err) {
      setError(err instanceof Error ? err.message : "Erro de autenticação");
    } finally {
      setLoading(false);
    }
  }

  return (
    <div className="min-h-screen bg-sidebar flex items-center justify-center p-4">
      <div className="w-full max-w-sm">
        {/* Logo */}
        <div className="flex flex-col items-center mb-8">
          <div className="w-14 h-14 rounded-2xl bg-primary flex items-center justify-center mb-4 shadow-lg">
            <Store size={28} className="text-white" />
          </div>
          <h1 className="text-white text-2xl font-bold tracking-wide">BELAVISTA</h1>
          <p className="text-white/40 text-sm mt-1">Store Management</p>
        </div>

        {/* Card */}
        <div className="bg-card rounded-2xl p-8 shadow-xl">
          <h2 className="text-text-primary text-xl font-semibold mb-1">Welcome back</h2>
          <p className="text-text-secondary text-sm mb-6">Sign in to your account to continue</p>

          <form onSubmit={handleSubmit} className="flex flex-col gap-4">
            <div>
              <label className="text-xs font-medium text-text-secondary uppercase tracking-wide block mb-1.5">
                Número
              </label>
              <input
                type="text"
                value={numero}
                onChange={(e) => setNumero(e.target.value)}
                placeholder="Ex: FUN001"
                required
                className="w-full px-3.5 py-2.5 rounded-lg border border-border bg-surface text-sm text-text-primary placeholder-text-muted outline-none focus:border-primary focus:ring-2 focus:ring-primary/20 transition-all"
              />
            </div>

            <div>
              <label className="text-xs font-medium text-text-secondary uppercase tracking-wide block mb-1.5">
                Senha
              </label>
              <input
                type="password"
                value={senha}
                onChange={(e) => setSenha(e.target.value)}
                placeholder="Enter your password"
                required
                className="w-full px-3.5 py-2.5 rounded-lg border border-border bg-surface text-sm text-text-primary placeholder-text-muted outline-none focus:border-primary focus:ring-2 focus:ring-primary/20 transition-all"
              />
            </div>

            {error && (
              <p className="text-danger text-sm bg-danger-light rounded-lg px-3.5 py-2.5">
                {error}
              </p>
            )}

            <button
              type="submit"
              disabled={loading}
              className="w-full py-2.5 rounded-lg bg-primary text-white text-sm font-semibold hover:bg-primary-dark transition-colors disabled:opacity-60 disabled:cursor-not-allowed mt-1"
            >
              {loading ? "A autenticar..." : "Entrar"}
            </button>
          </form>

          <div className="mt-5 text-xs text-text-muted space-y-1">
            <p className="font-semibold text-text-secondary">Contas de demo:</p>
            <p>Loja → <strong>FUN001</strong> / func123 &nbsp;|&nbsp; <strong>GER001</strong> / gerente123</p>
            <p>Cadeia → <strong>ADM001</strong> / admin123</p>
          </div>
        </div>
      </div>
    </div>
  );
}
