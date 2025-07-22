#!/home/jdai/miniconda3/envs/VEP/bin/python
 
# import os
# for f in os.listdir():
#     if f.endswith("sorted.vcf"):
#         fname =f.split(".")[0]
#         if not os.path.exists(fname+".vep.txt"):
#             cmd = "vep --cache /home/qdu/.vep --assembly GRCh37 -i " + f + " -o " + fname +".vep.txt " +" --offline --tab --symbol --variant_class --af --af_gnomad"
#             os.system(cmd)

import os, time

for f in os.listdir():
    if f.endswith("sorted.vcf"):
        fname = f.split(".")[0]
        out_file = fname + ".vep.vcf"  # note the new extension for VCF output
        if not os.path.exists(out_file):
            cmd = (
                "vep --cache --offline "
                "--dir_cache /home/qdu/.vep "
                #"--dir_cache /projects/oncology/databases/ensembl_ref "
                "--assembly GRCh37 "
                f"-i {f} -o {out_file} "
                "--vcf --symbol --variant_class --af --af_gnomad "
                "--canonical --biotype --sift b --polyphen b --fork 64"
            )
            print(f"Running: {cmd}")
            start = time.time()
            os.system(cmd)
            print(f"Finished in {time.time() - start:.2f} seconds\n")
