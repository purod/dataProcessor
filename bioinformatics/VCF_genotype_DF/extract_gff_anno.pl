#! usr/bin/perl -w
use strict;

open(IN,"$ARGV[0]")||die "$!";
print "chr\tsource\tgeneType\tstart\tend\tensembleID\tgeneName\tbiotype\tversion\n";

while(<IN>){
        if(/^#/){
                next;
        }
        chomp;
        my @array = split/\t/;
        if($array[2]=~/gene/ && $array[8]=~/ID=gene/){
		print join("\t",@array[0..4]);
		print "\t";
        	# $array[8]=~/ID=gene:(.+?);.+description=(.+?) \[/;
		if($array[8]=~/Name=/){
        		$array[8]=~/ID=gene:(.+);Name=(.+);biotype=(.+?);.+version=(\d+)/;
			print "$1\t$2\t$3\t$4\n";
		}else{
			$array[8]=~/ID=gene:(.+);biotype=(.+?);.+version=(\d+)/;
			print "$1\t\t$2\t$3\n";
		}
        }
}
close(IN);

#sub urldecode {
#    my $s = shift;
#    $s =~ s/\%([A-Fa-f0-9]{2})/pack('C', hex($1))/seg;
#    $s =~ s/\+/ /g;
#    return $s;
#}


