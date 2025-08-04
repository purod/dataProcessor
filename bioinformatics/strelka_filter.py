#!/usr/bin/env python3

import pysam
import matplotlib.pyplot as plt

def calculate_vaf(ref_count, alt_count):
    total = ref_count + alt_count
    return alt_count / total if total > 0 else 0.0

# Initialize lists to collect values for plotting
tumor_dp_list = []
info_dp_list = []
snvsb_list = []
vaf_list = []

vcf_in = pysam.VariantFile("snvs_selected.vcf", "r")
vcf_out = pysam.VariantFile("filtered_output.vcf", "w", header=vcf_in.header)

for rec in vcf_in.fetch():
    try:
        info_dp = rec.info.get("DP", 0)
        snvsb = rec.info.get("SNVSB", 0.0)
        tumor_sample = rec.samples["TUMOR"]

        tumor_dp = tumor_sample.get("DP", 0)
        au = tumor_sample.get("AU", [0, 0])
        cu = tumor_sample.get("CU", [0, 0])
        gu = tumor_sample.get("GU", [0, 0])
        tu = tumor_sample.get("TU", [0, 0])

        ref_base = rec.ref.upper()
        alt_base = rec.alts[0].upper()

        ref_count = {"A": au, "C": cu, "G": gu, "T": tu}[ref_base][0]
        alt_count = {"A": au, "C": cu, "G": gu, "T": tu}[alt_base][0]

        vaf = calculate_vaf(ref_count, alt_count)

        # Store values for plotting
        tumor_dp_list.append(tumor_dp)
        info_dp_list.append(info_dp)
        snvsb_list.append(snvsb)
        vaf_list.append(vaf)

        if tumor_dp >= 20 and info_dp >= 20 and abs(snvsb) < 10 and vaf >= 0.3:
            vcf_out.write(rec)

    except Exception as e:
        print(f"Skipping record due to error: {e}")

vcf_in.close()
vcf_out.close()

# Plot distributions
fig, axs = plt.subplots(2, 2, figsize=(12, 10))

axs[0, 0].hist(tumor_dp_list, bins=40, color='skyblue', edgecolor='black')
axs[0, 0].set_title("Tumor Sample Depth (DP)")
axs[0, 0].set_xlabel("Depth")
axs[0, 0].set_ylabel("Count")

axs[0, 1].hist(info_dp_list, bins=40, color='orange', edgecolor='black')
axs[0, 1].set_title("INFO DP (Combined Depth)")
axs[0, 1].set_xlabel("Depth")
axs[0, 1].set_ylabel("Count")

axs[1, 0].hist(snvsb_list, bins=40, color='lightgreen', edgecolor='black')
axs[1, 0].set_title("SNV Strand Bias (SNVSB)")
axs[1, 0].set_xlabel("SNVSB")
axs[1, 0].set_ylabel("Count")

axs[1, 1].hist(vaf_list, bins=40, color='salmon', edgecolor='black')
axs[1, 1].set_title("Variant Allele Frequency (VAF)")
axs[1, 1].set_xlabel("VAF")
axs[1, 1].set_ylabel("Count")

plt.tight_layout()
plt.savefig("filtering_distributions.png")
plt.show()

