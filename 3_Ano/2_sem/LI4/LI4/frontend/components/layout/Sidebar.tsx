"use client";

import React from "react";
import Link from "next/link";
import { usePathname, useRouter } from "next/navigation";
import {
  LayoutDashboard, Store, Package, Layers, ShoppingCart,
  Users, BarChart2, HelpCircle, LogOut,
} from "lucide-react";
import { useAuth } from "@/lib/auth";

type Cargo = "FUNCIONARIO" | "GERENTE" | "ADMINISTRADOR_CENTRAL";

const nav: { label: string; href: string; icon: React.ElementType; hidden?: Cargo[] }[] = [
  { label: "Dashboard", href: "/dashboard", icon: LayoutDashboard },
  { label: "Stores",    href: "/stores",    icon: Store,        hidden: ["FUNCIONARIO", "GERENTE"] },
  { label: "Products",  href: "/products",  icon: Package },
  { label: "Inventory", href: "/inventory", icon: Layers },
  { label: "Sales",     href: "/sales",     icon: ShoppingCart, hidden: ["ADMINISTRADOR_CENTRAL"] },
  { label: "Employees", href: "/employees", icon: Users },
  { label: "Reports",   href: "/reports",   icon: BarChart2 },
];

export default function Sidebar() {
  const pathname = usePathname();
  const router = useRouter();
  const { user, logout } = useAuth();

  function handleLogout() {
    logout();
    router.push("/login");
  }

  return (
    <aside className="flex flex-col w-52 min-h-screen bg-sidebar shrink-0">
      {/* Logo */}
      <div className="flex items-center gap-2.5 px-5 py-5 border-b border-white/10">
        <div className="flex items-center justify-center w-8 h-8 rounded-lg bg-primary">
          <Store size={16} className="text-white" />
        </div>
        <div>
          <p className="text-white text-sm font-bold tracking-wide leading-none">BELAVISTA</p>
          <p className="text-white/40 text-[10px] mt-0.5 leading-none">Store Management</p>
        </div>
      </div>

      {/* Navigation */}
      <nav className="flex flex-col gap-0.5 px-3 py-4 flex-1">
        {nav
          .filter(({ hidden }) => !hidden || !user?.cargo || !hidden.includes(user.cargo as Cargo))
          .map(({ label, href, icon: Icon }) => {
            const active = pathname === href || (href !== "/dashboard" && pathname.startsWith(href));
            return (
              <Link
                key={href}
                href={href}
                className={`flex items-center gap-3 px-3 py-2.5 rounded-lg text-sm transition-colors ${
                  active
                    ? "bg-sidebar-active text-white font-medium"
                    : "text-white/60 hover:bg-sidebar-hover hover:text-white"
                }`}
              >
                <Icon size={16} />
                {label}
              </Link>
            );
          })}
      </nav>

      {/* Footer */}
      <div className="px-3 pb-4 flex flex-col gap-0.5 border-t border-white/10 pt-3">
        <Link
          href="/help"
          className="flex items-center gap-3 px-3 py-2.5 rounded-lg text-sm text-white/50 hover:bg-sidebar-hover hover:text-white transition-colors"
        >
          <HelpCircle size={16} />
          Help Center
        </Link>
        <button
          onClick={handleLogout}
          className="flex items-center gap-3 px-3 py-2.5 rounded-lg text-sm text-white/50 hover:bg-sidebar-hover hover:text-white transition-colors w-full text-left"
        >
          <LogOut size={16} />
          Logout
        </button>
      </div>
    </aside>
  );
}
