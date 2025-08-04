# %% 
#!/home/jdai/miniconda3/envs/VEP/bin/python
import subprocess

def simple_allele_to_vcf(input_file, output_file, genome_version='GRCh38',separator=" "):
    # Dictionary to store contig information for both GRCh37 and GRCh38
    contig_lengths = {
        'GRCh37': [
            '##contig=<ID=1,length=249250621>',
            '##contig=<ID=2,length=243199373>',
            '##contig=<ID=3,length=198022430>',
            '##contig=<ID=4,length=191154276>',
            '##contig=<ID=5,length=180915260>',
            '##contig=<ID=6,length=171115067>',
            '##contig=<ID=7,length=159138663>',
            '##contig=<ID=8,length=146364022>',
            '##contig=<ID=9,length=141213431>',
            '##contig=<ID=10,length=135534747>',
            '##contig=<ID=11,length=135006516>',
            '##contig=<ID=12,length=133851895>',
            '##contig=<ID=13,length=115169878>',
            '##contig=<ID=14,length=107349540>',
            '##contig=<ID=15,length=102531392>',
            '##contig=<ID=16,length=90354753>',
            '##contig=<ID=17,length=81195210>',
            '##contig=<ID=18,length=78077248>',
            '##contig=<ID=19,length=59128983>',
            '##contig=<ID=20,length=63025520>',
            '##contig=<ID=21,length=48129895>',
            '##contig=<ID=22,length=51304566>',
            '##contig=<ID=X,length=155270560>',
            '##contig=<ID=Y,length=59373566>',
            '##contig=<ID=MT,length=16569>'
        ],
        'GRCh38': [
            '##contig=<ID=1,length=248956422>',
            '##contig=<ID=2,length=242193529>',
            '##contig=<ID=3,length=198295559>',
            '##contig=<ID=4,length=190214555>',
            '##contig=<ID=5,length=181538259>',
            '##contig=<ID=6,length=170805979>',
            '##contig=<ID=7,length=159345973>',
            '##contig=<ID=8,length=145138636>',
            '##contig=<ID=9,length=138394717>',
            '##contig=<ID=10,length=133797422>',
            '##contig=<ID=11,length=135086622>',
            '##contig=<ID=12,length=133275309>',
            '##contig=<ID=13,length=114364328>',
            '##contig=<ID=14,length=107043718>',
            '##contig=<ID=15,length=101991189>',
            '##contig=<ID=16,length=90338345>',
            '##contig=<ID=17,length=83257441>',
            '##contig=<ID=18,length=80373285>',
            '##contig=<ID=19,length=58617616>',
            '##contig=<ID=20,length=64444167>',
            '##contig=<ID=21,length=46709983>',
            '##contig=<ID=22,length=50818468>',
            '##contig=<ID=X,length=156040895>',
            '##contig=<ID=Y,length=57227415>',
            '##contig=<ID=MT,length=16569>'
        ]
    }
    
    with open(input_file, 'r') as input_fh, open(output_file, 'w') as output_fh:
        # Write VCF header
        output_fh.write('##fileformat=VCFv4.2\n')
        output_fh.write(f'##reference={genome_version}\n')

        # Add the contig information based on the genome version
        if genome_version in contig_lengths:
            for contig in contig_lengths[genome_version]:
                output_fh.write(f'{contig}\n')

        # Write column headers for VCF
        output_fh.write('#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\n')

        # Parse variants from input file and convert to VCF format
        for line in input_fh:
            line = line.strip()
            if line:
                chrom, pos, ref, alt = line.split(separator)
                vcf_entry = f'{chrom}\t{pos}\t.\t{ref}\t{alt}\t.\t.\t.\n'
                output_fh.write(vcf_entry)

    print(f"VCF file '{output_file}' has been created.")

def sort_vcf(input_vcf, output_vcf):
    """Sort VCF file using bcftools."""
    try:
        # Run the bcftools sort command
        subprocess.run(['bcftools', 'sort', input_vcf, '-o', output_vcf], check=True)
        print(f"VCF file '{output_vcf}' has been sorted.")
    except subprocess.CalledProcessError as e:
        print(f"An error occurred while sorting the VCF file: {e}")
    except FileNotFoundError:
        print("Error: bcftools is not installed or not in the system's PATH.")

# %%
# Example usage
input_file = '/home/qdu/git/OrionMO/data/MGSP/MGSP_VEP_input.txt'  # Replace with your input file containing variants in Simple Allele Format
output_file = 'VEP_input.vcf'  # Specify the output file name
sorted_output_file = 'VEP_input_sorted.vcf'  # Specify the name of the sorted output file
genome_version = 'GRCh37'  # Choose between GRCh37 or GRCh38
separator = ' '
# Generate the VCF file
simple_allele_to_vcf(input_file, output_file, genome_version, separator)

# Sort the VCF file using bcftools
sort_vcf(output_file, sorted_output_file)

# Annotation with VEP
# online: VEP has 50M limitation

# %%
