#! usr/bin/perl -w
use strict;
use Getopt::Long;

open(VCF_INPUT,"$ARGV[0]");  # input vcf file
open(OUT,"$ARGV[2]"); # output genotype matrix

while(<VCF_INPUT>){
	chomp;
	my $record = 1;
	if(/^##/){ # remove head info
		next;
	}
	my @array = split(/\t/);
	if($array[0] eq "#CHROM"){ # column names
		if( $ARGV[1] eq "annovar_output" ){
			print OUT "CHROM\tPOS\tID\tREF\tALT\tGENE\tAF\tREFE\tENSE\tKNOWNE\tSIFT\tPOLYPHEN_HDIV";
			print OUT "\tPOLYPHEN_HVAR\tFATHMM\tVEST3\tCLNDBN\tCLINSIG\t";
			print OUT join("\t",@array[9..$#array]);
		        print OUT "\n";	
		}
	}else{
		if(index($array[4],",") != -1){ # skip multi-allelic variants
                	next;
                }
		
		# filtering based on the annotation column IF, can be further improved in the future	
		my %info;
		my @info = split /;/, $array[7];
		foreach my $unit(@info) {
  			my ($i,$j)= split(/=/, $unit);
  			$info{$i} = $j;
		}

		# filtering based on sample MAF
		if( exists($ARGV[3]) ){
			if( $info{"AF"}>$ARGV[3]){
				next;
			}
		}
		
		## 1. ANNOVAR
		my %exclude = ("." => ".", "unknown"=>"unknown", "synonymous_SNV"=>"synonymous_SNV");		
		## keep missense and frameshift variants
		if( exists($exclude{ $info{'ExonicFunc.refGene'} }) 
			and exists($exclude{ $info{'ExonicFunc.ensGene'} })
			and exists($exclude{ $info{'ExonicFunc.knownGene'} }) ){
			next;
		}
		## keep variants which are predicted to be deleterious by at least two algorithms 
		if( $ARGV[2] eq "deleterious" ){
			my $del_count=0;
			if( $info{'SIFT_pred'} eq "D" ){
				$del_count=$del_count+1;
			}
			if( $info{'Polyphen2_HDIV_pred'} eq "D" or $info{'Polyphen2_HDIV_pred'} eq "P" 
			    or $info{'Polyphen2_HVAR_pred'} eq "D" or $info{'Polyphen2_HVAR_pred'} eq "P" ){
				$del_count=$del_count+1;
			}
			if( $info{"FATHMM_pred"} eq "D" ){
				$del_count=$del_count+1;
			}
			if( ($info{"VEST3_score"} ne ".") and ($info{VEST3_score}>0.25) ){
				$del_count=$del_count+1;
			}
			if($del_count<2){
				next;
			}
		}

		$array[7]="$info{'AF'}\t$info{'ExonicFunc.refGene'}\t$info{'ExonicFunc.ensGene'}\t$info{'ExonicFunc.knownGene'}\t$info{'SIFT_pred'}\t$info{'Polyphen2_HDIV_pred'}\t$info{'Polyphen2_HVAR_pred'}\t$info{'FATHMM_pred'}\t$info{'VEST3_score'}\t$info{'CLNDBN'}\t$info{'CLINSIG'}";
		
		$array[5]="$info{'Gene.refGene'}";

		## Locate genotype and genotyping quality
                my @format = split(/\:/,$array[8]);
		my $GT=0;
		my $GQ=0;
                foreach my $id(0..$#format){
                        if($format[$id] eq "GT"){
				$GT=$id; last;
			}
		}
		foreach my $id(0..$#format){
			if($format[$id] eq "GQ"){
				$GQ=$id; last;
			}
                }

		# convert genotypes into numbers
		if($ARGV[1] eq "annovar_output"){
			foreach my $item(9..$#array){
				my @detail = split(/\:/,$array[$item]);
				if($detail[$GT] eq "0/0"){
					$array[$item] = 0;
				}elsif(($detail[$GT] eq "0/1")|($detail[$GT] eq "1/0")){
					$array[$item] = 1;
				}elsif($detail[$GT] eq "1/1"){
					$array[$item] = 2;
				}elsif($detail[$GT] eq "./." or $detail[$GQ] eq "." or $detail[$GQ]<21){
					$array[$item] = 'NA'
				}else{
					print "$array[$item]warning: weird genotype found at $record row $item column!";
				}
			}
			print OUT join("\t",@array[0..5]);
			print OUT "\t$array[7]\t";
			print OUT join("\t",@array[9..$#array]);
			print OUT "\n";
		# convert VCF into default input of VEP
		}elsif($ARGV[1] eq "default_input"){
			print OUT "$array[0] ";
			if(length($array[3])>1 and length($array[4])==1){
				$array[1] = $array[1]+1;
				$array[3] =~ s/^.//;
				$array[4] = $array[1]+length($array[3])-1;
				print OUT "$array[1] $array[4] $array[3]/-";
			}elsif(length($array[4])>1 and length($array[3])==1){
				$array[1] = $array[1]+1;
				$array[4] =~ s/^.//;
				$array[4] = $array[1]+1;
				print OUT "$array[1] $array[4] -/$array[3]";
			}else{
				print OUT "$array[1] $array[1] $array[3]/$array[4]";
			}
			print OUT " +\n";
		}
	$record = $record + 1;	
	}
}
close(VCF_INPUT);
close(OUT);

