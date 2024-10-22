#!/home/jdai/miniconda3/envs/VEP/bin/python
 
import os
for f in os.listdir():
    if f.endswith(".vcf"):
        fname =f.split(".")[0]
        if not os.path.exists(fname+".vep.txt"):
            cmd = "vep --cache /users/jdai/vep_reference/ --assembly GRCh38 -i " + f + " -o " + fname +".vep.txt " +" --tab --symbol --variant_class --af --af_gnomad --af_esp"
            os.system(cmd)
