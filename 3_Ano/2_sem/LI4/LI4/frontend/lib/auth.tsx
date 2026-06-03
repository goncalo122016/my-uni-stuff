"use client";

import React, { createContext, useContext, useEffect, useState } from "react";
import type { Cargo, LoginResponse } from "./api";

interface AuthUser {
  id: number;
  numero: string;
  nome: string;
  cargo: Cargo;
  lojaId: number;
  backend: string; // "/loja-api" ou "/cadeia-api"
}

interface AuthContextValue {
  user: AuthUser | null;
  login: (numero: string, senha: string, info: LoginResponse, backend: string) => void;
  logout: () => void;
  isLoading: boolean;
}

const AuthContext = createContext<AuthContextValue | null>(null);

export function AuthProvider({ children }: { children: React.ReactNode }) {
  const [user, setUser] = useState<AuthUser | null>(null);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    const stored = localStorage.getItem("bv_user");
    if (stored) {
      try {
        setUser(JSON.parse(stored));
      } catch {
        localStorage.removeItem("bv_user");
        localStorage.removeItem("bv_creds");
        localStorage.removeItem("bv_backend");
      }
    }
    setIsLoading(false);
  }, []);

  function login(numero: string, senha: string, info: LoginResponse, backend: string) {
    const credentials = btoa(`${numero}:${senha}`);
    const authUser: AuthUser = {
      id: info.id,
      numero: info.numero,
      nome: info.nome,
      cargo: info.cargo,
      lojaId: info.lojaId,
      backend,
    };
    localStorage.setItem("bv_creds", credentials);
    localStorage.setItem("bv_backend", backend);
    localStorage.setItem("bv_user", JSON.stringify(authUser));
    setUser(authUser);
  }

  function logout() {
    localStorage.removeItem("bv_creds");
    localStorage.removeItem("bv_backend");
    localStorage.removeItem("bv_user");
    setUser(null);
  }

  return (
    <AuthContext.Provider value={{ user, login, logout, isLoading }}>
      {children}
    </AuthContext.Provider>
  );
}

export function useAuth() {
  const ctx = useContext(AuthContext);
  if (!ctx) throw new Error("useAuth must be used inside AuthProvider");
  return ctx;
}
