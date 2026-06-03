import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  async rewrites() {
    return [
      {
        source: "/loja-api/:path*",
        destination: "http://localhost:8081/api/:path*",
      },
      {
        source: "/cadeia-api/:path*",
        destination: "http://localhost:8080/api/:path*",
      },
    ];
  },
};

export default nextConfig;
