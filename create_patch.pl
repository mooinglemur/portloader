#!/usr/bin/env perl

use Getopt::Long;
use Data::Dumper;
use POSIX qw(ceil);
use Digest::CRC qw(crcccitt);
use warnings;
use strict;
use bytes;

Getopt::Long::Configure ("bundling");

my $romfile;
my $prgfile;
my @bankfile;
my $outfile;
my $desc = "";

my $out_h = "";
my $out_c = "";
my $out_s = "";

# For testing afterwards
my $lowram;
my @bankram;
my @bankdata;
my $bank;
my %banks;

$|=1;
# Command line args: 
#   -r <filename>         the source ROM file from which the port was made
#   -p <filename>         the PRG file for the X16 (usually loaded at $0801)
#   -b <bank>,<filename>  supplemental file loaded at $A000, can be specified
#                         multiple times
#   -o <filename>         output filename for patch
#   -d <description>      Description string for display in portloader

GetOptions(
    "r|rom=s" => \$romfile,
    "p|prg=s" => \$prgfile,
    "b|bank=s" => \@bankfile,
    "o|out=s" => \$outfile,
    "d|description=s" => \$desc,
);

die "Must specify -o <outfile>\n" unless (defined $outfile && length($outfile));
die "Must specify -r <romfile>\n" unless (defined $romfile && length($romfile));
die "Must specify -p <prgfile>\n" unless (defined $prgfile && length($prgfile));

my $rom;
open my $romfh, "<", $romfile or die "Unable to open ROM file: $!\n";
{
    local $/ = undef;
    $rom = <$romfh>;
}
close $romfh;

# Handle the PRG first

my $prg;
open my $prgfh, "<", $prgfile or die "Unable to open PRG file: $!\n";
{
    local $/ = undef;
    $prg = <$prgfh>;
}
close $prgfh;

printf "Original ROM size: \%d\n",length($rom);

my $prg_origin = unpack("v",substr($prg,0,2));

print "Processing PRG...";

process_segment(substr($prg,2),$rom,$prg_origin);

printf "\nLoad address: \$%04X\nOriginal PRG size: \%d bytes\nPatch size: \%d bytes\nRatio: \%02.1f\%\%\n",$prg_origin,length($prg),length($out_c)+length($out_s),(100*(length($out_c)+length($out_s))/length($prg));

my $cum_length = length($prg);
$lowram = "\x00"x(length($prg)-2);


for my $b (@bankfile) {
    my ($bank,$binfile) = split(/,/,$b);
    my $bin;
    open my $binfh, "<", $binfile or die "Unable to open BIN file $binfile: $!\n";
    {
        local $/ = undef;
        $bin = <$binfh>;
    }
    close $binfh;

    $bankdata[$bank] = $bin;

    $out_c .= "\x1f".pack("C",$bank);
    $out_s .= "\x1f".pack("C",$bank);

    print "Processing bank $bank...";

    process_segment($bin,$rom,0xA000);

    $bankram[$bank] = "\x00"x(length($bin));

    $cum_length += length($bin);

    printf "\nLoad address: \$%04X\nOriginal BIN size: \%d bytes\nCumulative PRG+BIN size: \%d\nPatch size: \%d bytes\nRatio: \%02.1f\%\%\n",0xA000,length($bin),$cum_length,length($out_c)+length($out_s),(100*(length($out_c)+length($out_s))/$cum_length);

}

printf "Original ROM size: \%d\n",length($rom);

print "Set metadata:\n";
print "Version: 1\n";
$out_h .= "\x01\x01";
if (defined($desc) && length($desc)) {
    if (length($desc) > 32) {
        $desc = substr($desc,0,64);
    }

    printf "Description: \%s\n",$desc;
    $out_h .= "\x02".pack("C",length($desc)).$desc;
}
my $size_blocks = ceil(length($rom)/256);
printf "Size: \%d blocks\n",$size_blocks;
$out_h .= "\x06".(pack("v",$size_blocks));

# Test for ROM type.  If NES, CRC-16 the PRG part
if (substr($rom,0,4) eq "NES\x1A") {
    my $base = 16; # default offset for PRG data
    my $prgs = unpack("C",substr($rom,4,1));
    my $flags = unpack("C",substr($rom,6,1));
    if ($flags & 4) {
        $base += 512;
    }

    my $crc = crcccitt(substr($rom,$base,$prgs*16384));
    printf "Source file is NES: CRC-16: \$%04X\n",$crc;
    printf "Offset: \%d, length: \%d\n",$base,$prgs*16384;
    $out_h .= "\x05".(pack("CCC",$base & 0xff,($base >> 8) & 0xff,($base >> 16 & 0xff)));
    $out_h .= pack("CCC",($prgs*16384) & 0xff,(($prgs*16384) >> 8) & 0xff,(($prgs*16384) >> 16 & 0xff));
    $out_h .= pack("v",$crc);

} else {
    my $crc = crcccitt($rom);
    printf "Calculating CRC-16 of entire ROM: \$%04X\n",$crc;
    $out_h .= "\x05\x00\x00\x00";
    $out_h .= pack("CCC",length($rom) & 0xff,(length($rom) >> 8) & 0xff,(length($rom) >> 16 & 0xff));
    $out_h .= pack("v",$crc);
}

$out_h .= "\x0f";

print "Writing out patch file.\n";
open my $outfh, ">", $outfile or die "Unable to open $outfile: $!";
print $outfh $out_h;
print $outfh $out_c;
print $outfh $out_s;
close $outfh;

print "Processing patch file to test decoder.\n";

my $patch;

open my $infh, "<", $outfile or die "Unable to reopen $outfile: $!";
{
    local $/ = undef;
    $patch = <$infh>;
}
close $infh;

my $i = 0;
while ($i < length($patch)) {
    my $cmd = unpack("C",substr($patch,$i,1));
    $i++;
    if ($cmd == 0x1) {
        if (substr($patch,$i,1) ne "\x01") {
            die "Unrecognized patch version."
        }
        $i++;
        next;
    }
    if ($cmd == 0x2) {
        $i += 1 + unpack("C",substr($patch,$i,1));
        next;
    }
    if ($cmd == 0x3) {
        $i += 4 + unpack("C",substr($patch,$i+3,1));
        next;
    }
    if ($cmd == 0x4) {
        $i += 10;
        next;
    }
    if ($cmd == 0x5) {
        $i += 8;
        next;
    }
    if ($cmd == 0x6) {
        $i += 2;
        next;
    }
    if ($cmd == 0x0f) {
        next;
    }
    if ($cmd == 0x10) { # copy
        my ($off,$offh,$len,$addr) = unpack("vCvv",substr($patch,$i,7));
        $off += $offh * 0x10000;
        if ($addr < 0xA000) {
            my $a = $addr - $prg_origin;
            substr($lowram,$a,$len,substr($rom,$off,$len));
        } else {
            my $a = $addr - 0xA000;
            substr($bankram[$bank],$a,$len,substr($rom,$off,$len));
        }

        $i += 7;
        next;
    }
    if ($cmd == 0x11) { # supplement 
        my ($len,$addr) = unpack("vv",substr($patch,$i,4));
        my $data = substr($patch,$i+4,$len);

        if ($addr < 0xA000) {
            my $a = $addr - $prg_origin;
            substr($lowram,$a,$len,$data);
        } else {
            my $a = $addr - 0xA000;
            substr($bankram[$bank],$a,$len,$data);
        }
        $i += 4 + $len;
        next;
    }

    if ($cmd == 0x1f) { # bank switch
        $bank = unpack("C",substr($patch,$i,1));
        $banks{$bank}++;
        $i++;
        next;
    }
    die "Unsupported command $cmd \@ $i";
}

print "Comparing low RAM...";
if ($lowram eq substr($prg,2)) {
    printf "equal! (length \%d)\n",length($lowram);
} else {
    print "failed!\n";
    print Dumper(substr($lowram,1024));
    print Dumper(substr($prg,2,1024));
}


for $bank (sort { $a <=> $b } keys %banks) {
    print "Comparing banked RAM...";
    print "$bank...";
    if ($bankram[$bank] eq $bankdata[$bank]) {
        printf "equal! (length \%d)\n",length($bankram[$bank]);
    } else {
        print "failed!\n";
        print Dumper(substr($bankram[$bank],1024));
        print Dumper(substr($bankdata[$bank],1024));
    }
}
    
exit 0;

sub process_segment {
    my $mem = shift;
    my $rom = shift;
    my $origin = shift;
    my ($pcopies,$pmisses) = scan_bin($mem,$rom);

    # turn misses into ranges

    my @pranges;
    my $pl = shift @$pmisses;
    my $ph = $pl;
    while (scalar @$pmisses) {
        my $m = shift @$pmisses;
        if ($m <= $ph+5) { # colesce ranges that differ by 5 or less
            $ph = $m;
            next;
        }
        push @pranges, [$pl, $ph];
        $pl = $m;
        $ph = $m;
    }
    push @pranges, [$pl, $ph];

    my $copy_bytes = 0;

    # Turn copy commands into patch data
    for my $c (@$pcopies) {
        my ($addr,$offset,$length) = @$c;
        $addr += $origin;
        my $str = "\x10";
        $str .= pack("CCC",$offset & 0xff,($offset >> 8) & 0xff,($offset >> 16) & 0xff);
        $str .= pack("CC",$length & 0xff,($length >> 8) & 0xff);
        $str .= pack("CC",$addr & 0xff,($addr >> 8) & 0xff);
        $out_c .= $str;
        $copy_bytes += $length;
    }

    my $suppl_bytes = 0;

    # Turn supplement data into patch data
    for my $c (@pranges) {
        my ($low,$high) = @$c;
        my $length = 1+($high-$low);
        my $addr = $origin+$low;
        my $str = "\x11";
        $str .= pack("CC",$length & 0xff,($length >> 8) & 0xff);
        $str .= pack("CC",$addr & 0xff,($addr >> 8) & 0xff);
        $str .= substr($mem,$low,$length);
        $out_s .= $str;
        $suppl_bytes += $length;
    }
}

sub scan_bin {
    my $bin = shift;
    my $rom = shift;
    
    my @misses = ();
    my @copies = ();
    my $ptr = 0;

    BINLOOP: while ($ptr < length($bin)) {
        # find_longest_match(<search string>,<search space>)
        my ($l,$o,$m,$r) = find_longest_match(substr($bin,$ptr),$rom);
#        warn "$ptr: Len: $l / Hits: ".($l-scalar(@$m))." / Ratio: $r" if ($l >= 20+scalar(@$m));
        if ($l < 20+scalar(@$m)) { # no worthwhile match found
            push @misses, $ptr++;
            next BINLOOP;
        }
        for my $mi (@$m) {
            push @misses, $mi+$ptr;
        }

        if ($ptr+$l > length($bin)) {
            $l = length($bin)-$ptr;
        }
        push @copies, [$ptr,$o,$l];
        $ptr+=$l;
    }

    return (\@copies,\@misses)
}  

sub find_longest_match {
    my $left = shift;
    my $right = shift;

    my $o=0;  # start of match
    my $lo = 0;
    my $ll = 0;
    my $lm = [];
    my $lr = 0;
    my $llwm = 0;

    MATCHLOOP: while ($o < length($right)) {
        my @misses = ();
        my $ml=0; # length of good match (with possible holes)
        my $l=0;  # length of total match (including misses at the end)
        my $m=0;  # number of consecutive misses

        if (substr($left,0,1) ne substr($right,$o,1)) {
            $o++;
            next;
        }

        while ($l < length($left) && $o+$l < length($right)) {
            if (substr($left,$l,1) eq substr($right,$o+$l,1)) {
                $ml=$l;
                $m=0;
            } else {
                $m++;
                push @misses,$l;
            }

            last if ($m > 16);
            last if ($ml > 64 && scalar @misses > $ml);

            $l++;
        }

        do {
            # shave off the end misses
            while (scalar @misses && $misses[-1] > $ml) {
                pop @misses;
            }


            my $r = $ml/(scalar @misses || 0.0001);

            if ($ml-(scalar @misses) > $llwm) {
                $lo = $o;
                $ll = $ml;
                $lm = [@misses];
                $lr = $r;
                $llwm = $ml-(scalar @misses);
            }

            do {
                --$ml;
            } while (grep { $ml == $_ } @misses)

        } while (scalar @misses);

        $o++;
    }

    return ($ll,$lo,$lm,$lr);
}
