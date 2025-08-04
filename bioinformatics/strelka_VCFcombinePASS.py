#!/usr/bin/env python3
import os
import gzip
import subprocess
import matplotlib.pyplot as plt

def extract_target_vcf_files(root_dir, suffix=".strelka.somatic_snvs.vcf.gz"):
    vcf_files = []
    for folder in os.listdir(root_dir):
        if "_vs_" not in folder:
            continue
        folder_path = os.path.join(root_dir, folder)
        if os.path.isdir(folder_path):
            for file in os.listdir(folder_path):
                if file.endswith(suffix):
                    vcf_files.append(os.path.join(folder_path, file))
    return vcf_files

def filter_and_concatenate_vcfs(vcf_files, raw_output_path, somatic_evs_threshold=0):
    somatic_evs_values = []
    header_written = False
    with open(raw_output_path, 'w') as outfile:
        for vcf_file in vcf_files:
            with gzip.open(vcf_file, 'rt') as f:
                for line in f:
                    if line.startswith('#'):
                        if not header_written:
                            outfile.write(line)
                    else:
                        fields = line.strip().split('\t')
                        filter_col = fields[6]
                        info_col = fields[7]
                        if filter_col != 'PASS':
                            continue
                        info_items = dict(item.split('=') for item in info_col.split(';') if '=' in item)
                        evs = float(info_items.get('SomaticEVS', -1))
                        if evs >= somatic_evs_threshold:
                            outfile.write(line)
                            somatic_evs_values.append(evs)
            header_written = True
    return somatic_evs_values

def plot_evs_distribution(somatic_evs_values, output_png):
    if not somatic_evs_values:
        print("No SomaticEVS values found.")
        return
    plt.figure(figsize=(8, 5))
    plt.hist(somatic_evs_values, bins=30, color='skyblue', edgecolor='black')
    plt.title('Distribution of SomaticEVS values')
    plt.xlabel('SomaticEVS')
    plt.ylabel('Frequency')
    plt.grid(True)
    plt.tight_layout()
    plt.savefig(output_png)
    print(f"EVS distribution plot saved to {output_png}")

def sort_and_compress_vcf(input_vcf, sorted_vcf_gz):
    cmd_sort = ["bcftools", "sort", "-Oz", "-o", sorted_vcf_gz, input_vcf]
    cmd_index = ["bcftools", "index", sorted_vcf_gz]
    subprocess.run(cmd_sort, check=True)
    subprocess.run(cmd_index, check=True)
    print(f"Sorted and indexed VCF saved to {sorted_vcf_gz}")

# ==== Main ====
root_directory = "./"  # Update to your actual path
raw_combined_vcf = "filtered_combined_raw.vcf"
sorted_combined_vcf_gz = "filtered_combined_sorted.vcf.gz"
output_plot = "evs_distribution.png"
evs_threshold = 0  # Update if needed

vcf_list = extract_target_vcf_files(root_directory)
print(f"Found {len(vcf_list)} VCF files for processing.")

evs_values = filter_and_concatenate_vcfs(vcf_list, raw_combined_vcf, somatic_evs_threshold=evs_threshold)
plot_evs_distribution(evs_values, output_plot)
sort_and_compress_vcf(raw_combined_vcf, sorted_combined_vcf_gz)

