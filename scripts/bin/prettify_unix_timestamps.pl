#!/usr/bin/perl

use strict;
use warnings;

while (<>) {  # https://perldoc.perl.org/perlop#I%2fO-Operators
  my $line = $_;
  if ($line =~ /\b(\d{10})\b/) {
    print(scalar localtime $1, "\t");  # https://stackoverflow.com/questions/15064446/convert-unix-timestamp-to-a-readable-date-in-perl
  }
  print $line;
}
