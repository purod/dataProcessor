#!/home/jdai/miniconda3/envs/VEP/bin/python
def simple_allele_to_vcf(input_file, output_file):
    with open(input_file, 'r') as input_fh, open(output_file, 'w') as output_fh:
        # Write VCF header
        output_fh.write('##fileformat=VCFv4.2\n')
        output_fh.write('#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\n')

        # Parse variants from input file and convert to VCF format
        for line in input_fh:
            line = line.strip()
            if line:
                chrom, pos, ref, alt = line.split(':')
                vcf_entry = f'{chrom}\t{pos}\t.\t{ref}\t{alt}\t.\t.\t.\n'
                output_fh.write(vcf_entry)

# Example usage
input_file = 'VEP_input.txt'  # Replace with your input file containing variants in Simple Allele Format
output_file = 'VEP_input.vcf'  # Specify the output file name
simple_allele_to_vcf(input_file, output_file)
