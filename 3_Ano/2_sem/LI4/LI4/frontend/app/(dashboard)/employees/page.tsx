"use client";

import { useEffect, useState } from "react";
import { Plus, Users, Shield, Search, MoreVertical, X } from "lucide-react";
import Header from "@/components/layout/Header";
import { api } from "@/lib/api";
import type { FuncionarioResponse, Cargo } from "@/lib/api";

const CARGO_LABELS: Record<Cargo, string> = {
  FUNCIONARIO: "Employee",
  GERENTE: "Manager",
  ADMINISTRADOR_CENTRAL: "Director",
};

const CARGO_BADGE: Record<Cargo, { bg: string; text: string }> = {
  FUNCIONARIO: { bg: "bg-surface", text: "text-text-secondary" },
  GERENTE: { bg: "bg-primary-light", text: "text-primary" },
  ADMINISTRADOR_CENTRAL: { bg: "bg-warning-light", text: "text-warning" },
};

const AVATAR_COLORS = [
  "bg-primary/10 text-primary",
  "bg-success/10 text-success",
  "bg-warning/10 text-warning",
  "bg-danger/10 text-danger",
  "bg-purple-100 text-purple-600",
  "bg-cyan-100 text-cyan-600",
];

export default function EmployeesPage() {
  const [employees, setEmployees] = useState<FuncionarioResponse[]>([]);
  const [loading, setLoading] = useState(true);
  const [search, setSearch] = useState("");
  const [roleFilter, setRoleFilter] = useState<Cargo | "ALL">("ALL");

  const [editing, setEditing] = useState<FuncionarioResponse | null>(null);
  const [editNome, setEditNome] = useState("");
  const [editCargo, setEditCargo] = useState<Cargo>("FUNCIONARIO");
  const [saving, setSaving] = useState(false);

  useEffect(() => {
    api.utilizadores.list()
      .then(setEmployees)
      .finally(() => setLoading(false));
  }, []);

  function openEdit(emp: FuncionarioResponse) {
    setEditing(emp);
    setEditNome(emp.nome);
    setEditCargo(emp.cargo);
  }

  async function saveEdit() {
    if (!editing) return;
    setSaving(true);
    try {
      const updated = await api.utilizadores.update(editing.id, {
        nome: editNome,
        cargo: editCargo,
      });
      setEmployees((prev) => prev.map((e) => e.id === editing.id ? updated : e));
      setEditing(null);
    } catch (e) {
      alert(e instanceof Error ? e.message : "Erro ao atualizar funcionário");
    } finally {
      setSaving(false);
    }
  }

  const filtered = employees.filter((e) => {
    const matchSearch = !search ||
      e.nome.toLowerCase().includes(search.toLowerCase()) ||
      e.numero.toLowerCase().includes(search.toLowerCase());
    const matchRole = roleFilter === "ALL" || e.cargo === roleFilter;
    return matchSearch && matchRole;
  });

  const managers = employees.filter((e) => e.cargo === "GERENTE" || e.cargo === "ADMINISTRADOR_CENTRAL").length;

  return (
    <div className="flex flex-col flex-1">
      <Header
        placeholder="Search employees..."
        action={
          <button className="flex items-center gap-2 px-4 py-2 rounded-lg bg-primary text-white text-sm font-medium hover:bg-primary-dark transition-colors">
            <Plus size={14} />
            ADD EMPLOYEE
          </button>
        }
      />

      <main className="flex-1 p-6 flex flex-col gap-6">
        <div>
          <h1 className="text-2xl font-bold text-text-primary">Employee Directory</h1>
          <p className="text-text-secondary text-sm mt-0.5">
            Manage your workforce across all store locations.
          </p>
        </div>

        {/* Stats */}
        <div className="grid grid-cols-2 gap-4">
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Total Employees</p>
              <Users size={16} className="text-primary" />
            </div>
            <p className="text-3xl font-bold text-text-primary">{employees.length}</p>
          </div>
          <div className="bg-card rounded-xl border border-border p-5">
            <div className="flex items-center justify-between mb-3">
              <p className="text-xs font-medium text-text-muted uppercase tracking-wide">Management</p>
              <Shield size={16} className="text-warning" />
            </div>
            <p className="text-3xl font-bold text-text-primary">{managers}</p>
            <p className="text-xs text-text-muted mt-1">Managers & Directors</p>
          </div>
        </div>

        {/* Filters */}
        <div className="flex items-center gap-3 flex-wrap">
          <div className="flex items-center gap-2 bg-card rounded-lg border border-border px-3 py-2">
            <Search size={14} className="text-text-muted shrink-0" />
            <input
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              placeholder="Search by name or number..."
              className="bg-transparent text-sm outline-none text-text-primary placeholder-text-muted w-48"
            />
          </div>
          <div className="flex items-center gap-1">
            {(["ALL", "FUNCIONARIO", "GERENTE", "ADMINISTRADOR_CENTRAL"] as const).map((role) => (
              <button
                key={role}
                onClick={() => setRoleFilter(role)}
                className={`px-3 py-1.5 rounded-lg text-xs font-medium transition-colors ${
                  roleFilter === role
                    ? "bg-primary text-white"
                    : "bg-card border border-border text-text-secondary hover:border-primary hover:text-primary"
                }`}
              >
                {role === "ALL" ? "All Roles" : CARGO_LABELS[role]}
              </button>
            ))}
          </div>
        </div>

        {/* Employee table */}
        <div className="bg-card rounded-xl border border-border overflow-hidden">
          <table className="w-full">
            <thead>
              <tr className="border-b border-border bg-surface/50">
                {["Employee", "Number", "Role", "Store ID", "Actions"].map((h) => (
                  <th key={h} className="text-left px-5 py-3 text-xs font-medium text-text-muted uppercase tracking-wide">
                    {h}
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {loading
                ? Array.from({ length: 5 }).map((_, i) => (
                    <tr key={i} className="border-b border-border">
                      {Array.from({ length: 5 }).map((_, j) => (
                        <td key={j} className="px-5 py-3.5">
                          <div className="h-4 bg-surface rounded animate-pulse w-24" />
                        </td>
                      ))}
                    </tr>
                  ))
                : filtered.map((emp, idx) => {
                    const badge = CARGO_BADGE[emp.cargo];
                    const avatarColor = AVATAR_COLORS[idx % AVATAR_COLORS.length];
                    return (
                      <tr key={emp.id} className="border-b border-border last:border-0 hover:bg-surface/40 transition-colors">
                        <td className="px-5 py-3.5">
                          <div className="flex items-center gap-3">
                            <div className={`w-9 h-9 rounded-full flex items-center justify-center text-sm font-bold shrink-0 ${avatarColor}`}>
                              {emp.nome.charAt(0)}
                            </div>
                            <p className="text-sm font-semibold text-text-primary">{emp.nome}</p>
                          </div>
                        </td>
                        <td className="px-5 py-3.5 text-sm font-mono text-text-muted">{emp.numero}</td>
                        <td className="px-5 py-3.5">
                          <span className={`inline-flex px-2 py-0.5 rounded-full text-xs font-medium ${badge.bg} ${badge.text}`}>
                            {CARGO_LABELS[emp.cargo]}
                          </span>
                        </td>
                        <td className="px-5 py-3.5 text-sm text-text-secondary">{emp.lojaId}</td>
                        <td className="px-5 py-3.5">
                          <div className="flex items-center gap-3">
                            <button
                              onClick={() => openEdit(emp)}
                              className="text-xs text-primary font-medium hover:underline"
                            >
                              Edit
                            </button>
                            <button className="text-text-muted hover:text-text-primary transition-colors">
                              <MoreVertical size={14} />
                            </button>
                          </div>
                        </td>
                      </tr>
                    );
                  })}
              {!loading && filtered.length === 0 && (
                <tr>
                  <td colSpan={5} className="px-5 py-10 text-center text-sm text-text-muted">
                    No employees found.
                  </td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
      </main>

      {/* Edit Employee Modal */}
      {editing && (
        <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
          <div className="bg-card rounded-xl border border-border p-6 w-full max-w-sm">
            <div className="flex items-center justify-between mb-5">
              <div>
                <h2 className="font-semibold text-text-primary">Edit Employee</h2>
                <p className="text-xs text-text-muted mt-0.5">{editing.numero}</p>
              </div>
              <button onClick={() => setEditing(null)} className="text-text-muted hover:text-text-primary transition-colors">
                <X size={16} />
              </button>
            </div>

            <div className="flex flex-col gap-4">
              <div>
                <label className="block text-xs font-medium text-text-muted uppercase tracking-wide mb-1.5">
                  Full Name
                </label>
                <input
                  type="text"
                  value={editNome}
                  onChange={(e) => setEditNome(e.target.value)}
                  className="w-full bg-surface border border-border rounded-lg px-3 py-2 text-sm text-text-primary outline-none focus:border-primary transition-colors"
                />
              </div>

              <div>
                <label className="block text-xs font-medium text-text-muted uppercase tracking-wide mb-1.5">
                  Role
                </label>
                <select
                  value={editCargo}
                  onChange={(e) => setEditCargo(e.target.value as Cargo)}
                  className="w-full bg-surface border border-border rounded-lg px-3 py-2 text-sm text-text-primary outline-none focus:border-primary transition-colors cursor-pointer"
                >
                  <option value="FUNCIONARIO">Employee</option>
                  <option value="GERENTE">Manager</option>
                  <option value="ADMINISTRADOR_CENTRAL">Director</option>
                </select>
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
                disabled={saving || !editNome.trim()}
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
