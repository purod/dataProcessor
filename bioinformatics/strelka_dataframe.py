#!/usr/bin/env python3
import os
import gzip
import pandas as pd

def extract_target_vcf_files(root_dir, suffix=".strelka.somatic_snvs.vcf.gz"):
    vcf_files = []
    for folder in os.listdir(root_dir):
        if "_vs_" not in folder:
            continue
        folder_path = os.path.join(root_dir, folder)
        if os.path.isdir(folder_path):
            for file in os.listdir(folder_path):
                if file.endswith(suffix):
                    vcf_files.append((folder, os.path.join(folder_path, file)))
    return vcf_files

def parse_vcf_to_dataframe(vcf_files, somatic_evs_threshold=0):
    records = []
    for sample_name, vcf_path in vcf_files:
        with gzip.open(vcf_path, 'rt') as f:
            for line in f:
                if line.startswith("#"):
                    continue
                fields = line.strip().split('\t')
                chrom = fields[0]
                pos = int(fields[1])
                ref = fields[3]
                alt = fields[4]
                filter_col = fields[6]
                info_col = fields[7]

                if filter_col != "PASS":
                    continue

                # Extract SomaticEVS value from INFO
                info_dict = dict(item.split('=') for item in info_col.split(';') if '=' in item)
                evs = float(info_dict.get('SomaticEVS', -1))
                if evs < somatic_evs_threshold:
                    continue

                records.append({
                    "Sample": sample_name,
                    "CHROM": chrom,
                    "POS": pos,
                    "END": pos,
                    "REF": ref,
                    "ALT": alt,
                    "SomaticEVS": evs
                })
    return pd.DataFrame(records)

# === Main ===
root_dir = "./"  # adjust as needed
vcf_list = extract_target_vcf_files(root_dir)
df = parse_vcf_to_dataframe(vcf_list, somatic_evs_threshold=0)
df.to_csv("filtered_variants_summary.csv", index=False)
print("Saved filtered variant table to 'filtered_variants_summary.csv'")
