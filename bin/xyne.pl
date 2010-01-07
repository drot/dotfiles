#!/usr/bin/perl

use strict;
use warnings;

my $n = (`pacman -Qu | wc -l`);
chomp ($n);
if ($n == 0)
{
	print "No new packages."
}
elsif($n == 1)
{
	print "1 new package."
}
else
{
	print "$n new packages."
}
