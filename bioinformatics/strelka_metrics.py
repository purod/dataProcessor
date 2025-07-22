#!/usr/bin/env python3

import pysam
import matplotlib.pyplot as plt
import pandas as pd

def calculate_vaf(ref_count, alt_count):
    total = ref_count + alt_count
    return alt_count / total if total > 0 else 0.0

# Initialize lists
metrics = {
    "Tumor DP": [],
    "INFO DP": [],
    "SNVSB": [],
    "VAF": [],
    "SomaticEVS": [],
    "MQ": [],
    "MQ0": [],
    "PNOISE": [],
    "PNOISE2": [],
    "ReadPosRankSum": [],
    "FDP": [],
    "SDP": [],
    "SUBDP": []
}

# Open VCF
vcf_in = pysam.VariantFile("snvs_selected.vcf", "r")

for rec in vcf_in.fetch():
    try:
        info = rec.info
        sample = rec.samples["TUMOR"]

        # DP and other fields
        metrics["Tumor DP"].append(sample.get("DP", 0))
        metrics["INFO DP"].append(info.get("DP", 0))
        metrics["SNVSB"].append(info.get("SNVSB", 0.0))
        metrics["SomaticEVS"].append(info.get("SomaticEVS", 0.0))
        metrics["MQ"].append(info.get("MQ", 0.0))
        metrics["MQ0"].append(info.get("MQ0", 0))
        metrics["PNOISE"].append(info.get("PNOISE", 0.0))
        metrics["PNOISE2"].append(info.get("PNOISE2", 0.0))
        metrics["ReadPosRankSum"].append(info.get("ReadPosRankSum", 0.0))
        metrics["FDP"].append(sample.get("FDP", 0))
        metrics["SDP"].append(sample.get("SDP", 0))
        metrics["SUBDP"].append(sample.get("SUBDP", 0))

        # Calculate VAF
        au = sample.get("AU", [0, 0])
        cu = sample.get("CU", [0, 0])
        gu = sample.get("GU", [0, 0])
        tu = sample.get("TU", [0, 0])

        ref_base = rec.ref.upper()
        alt_base = rec.alts[0].upper()

        ref_count = {"A": au, "C": cu, "G": gu, "T": tu}[ref_base][0]
        alt_count = {"A": au, "C": cu, "G": gu, "T": tu}[alt_base][0]
        vaf = calculate_vaf(ref_count, alt_count)
        metrics["VAF"].append(vaf)

    except Exception as e:
        print(f"Skipping variant due to error: {e}")

vcf_in.close()

# Plot
plt.figure(figsize=(18, 12))
for i, (label, values) in enumerate(metrics.items(), start=1):
    plt.subplot(4, 4, i)
    plt.hist(values, bins=40, edgecolor='black', color='skyblue')
    plt.title(label)
    plt.xlabel(label)
    plt.ylabel("Count")
    plt.tight_layout()

plt.suptitle("Distributions of Strelka Filtering Criteria", fontsize=16, y=1.02)
plt.savefig("strelka_filtering_criteria_distributions.png")
plt.show()

