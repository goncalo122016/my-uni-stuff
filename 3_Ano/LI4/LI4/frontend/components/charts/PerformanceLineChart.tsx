"use client";

import {
  LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip,
  Legend, ResponsiveContainer, Dot,
} from "recharts";

interface DataPoint {
  name: string;
  Revenue: number;
  Orders: number;
}

interface Props {
  data: DataPoint[];
}

export default function PerformanceLineChart({ data }: Props) {
  return (
    <ResponsiveContainer width="100%" height={200}>
      <LineChart data={data}>
        <CartesianGrid strokeDasharray="3 3" stroke="#f0f0f0" vertical={false} />
        <XAxis dataKey="name" tick={{ fontSize: 11, fill: "#9ca3af" }} axisLine={false} tickLine={false} />
        <YAxis tick={{ fontSize: 11, fill: "#9ca3af" }} axisLine={false} tickLine={false} width={40} />
        <Tooltip contentStyle={{ borderRadius: 8, border: "1px solid #e5e7eb", fontSize: 12 }} />
        <Legend iconType="circle" iconSize={8} wrapperStyle={{ fontSize: 12 }} />
        <Line
          type="monotone" dataKey="Revenue" stroke="#2563eb" strokeWidth={2}
          dot={<Dot r={3} fill="#2563eb" />} activeDot={{ r: 5 }}
        />
        <Line
          type="monotone" dataKey="Orders" stroke="#10b981" strokeWidth={2}
          dot={<Dot r={3} fill="#10b981" />} activeDot={{ r: 5 }}
        />
      </LineChart>
    </ResponsiveContainer>
  );
}
