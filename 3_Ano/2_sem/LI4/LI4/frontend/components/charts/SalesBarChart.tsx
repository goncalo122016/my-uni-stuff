"use client";

import {
  BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip,
  Legend, ResponsiveContainer,
} from "recharts";

const days = ["MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"];

interface Props {
  thisWeek: number[];
  lastWeek: number[];
}

export default function SalesBarChart({ thisWeek, lastWeek }: Props) {
  const data = days.map((name, i) => ({
    name,
    "This Week": thisWeek[i] ?? 0,
    "Last Week": lastWeek[i] ?? 0,
  }));

  return (
    <ResponsiveContainer width="100%" height={180}>
      <BarChart data={data} barSize={8} barGap={4}>
        <CartesianGrid strokeDasharray="3 3" stroke="#f0f0f0" vertical={false} />
        <XAxis dataKey="name" tick={{ fontSize: 11, fill: "#9ca3af" }} axisLine={false} tickLine={false} />
        <YAxis tick={{ fontSize: 11, fill: "#9ca3af" }} axisLine={false} tickLine={false} width={35} />
        <Tooltip
          contentStyle={{ borderRadius: 8, border: "1px solid #e5e7eb", fontSize: 12 }}
          cursor={{ fill: "#f9fafb" }}
        />
        <Legend iconType="circle" iconSize={8} wrapperStyle={{ fontSize: 12 }} />
        <Bar dataKey="This Week" fill="#2563eb" radius={[3, 3, 0, 0]} />
        <Bar dataKey="Last Week" fill="#bfdbfe" radius={[3, 3, 0, 0]} />
      </BarChart>
    </ResponsiveContainer>
  );
}
