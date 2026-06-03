import json, glob, sys

print("=== IOPS, BW, LAT ===")
for p in [0, 50, 100]:
    for prefix in ["dedupe_", "normal_dedupe_"]:
        with open(f"fio_results/{prefix}{p}.json") as f:
            d = json.load(f)
            w = d["jobs"][0]["write"]
            print(f"{prefix}{p}: IOPS={w['iops']:.2f}, BW={w['bw']} KB/s, Lat={w['lat_ns']['mean']/1000000:.2f} ms")

print("\n=== CPU / MEM ===")
def avg_stats(filename):
    cpu_vals = []
    mem_vals = []
    try:
        with open(filename) as f:
            lines = f.readlines()
            for line in lines:
                parts = line.split()
                if len(parts) >= 8 and "UID" not in line and "Linux" not in line:
                    try:
                        # %CPU is usually the 7th column (index 6), but let's look at the header
                        # UID       PID    %usr %system  %guest   %wait    %CPU   CPU  Command
                        if "passthrough" in line and not "minflt" in line and len(parts) >= 8:
                            # Try to parse CPU, usually it's the one before CPU core ID
                            idx = parts.index("passthrough") if "passthrough" in parts else parts.index("passthrough_nor")
                            cpu_vals.append(float(parts[idx-2]))
                        elif "passthrough" in line and "minflt" not in line and "%MEM" not in line and len(parts) >= 8: # The memory one
                            # UID       PID  minflt/s  majflt/s     VSZ     RSS   %MEM  Command
                            idx = parts.index("passthrough") if "passthrough" in parts else parts.index("passthrough_nor")
                            mem_vals.append(float(parts[idx-1]))
                    except:
                        pass
        return (sum(cpu_vals)/len(cpu_vals)) if cpu_vals else 0.0, (sum(mem_vals)/len(mem_vals)) if mem_vals else 0.0
    except Exception as e:
        return 0,0

for p in [0, 50, 100]:
    for prefix in ["fuse_resources_", "normal_fuse_resources_"]:
        c, m = avg_stats(f"fio_results/{prefix}{p}.txt")
        print(f"{prefix}{p}: CPU={c:.2f}%, MEM={m:.2f}%")

print("\n=== DISK USAGE ===")
for f in ["fio_results/disk_usage.txt", "fio_results/normal_disk_usage.txt"]:
    print(f"--- {f} ---")
    with open(f) as fh:
        print(fh.read())
