#!/usr/bin/env perl
use strict;
#use warnings;

use Text::CSV;

my @columns = qw/3 7 11 15 19 23 27 31 35 38 41/;
my @years = (1998 .. 2008);

print "focus,state,year,result,male,female\n";

my $csv = Text::CSV->new();
open (my $fh, "data/NFOCUS27ST.csv") or die "can't open: NFOCUS01.csv: $!";
  my $focus = '';
  my $state = '';
  my @results;
  my(@male, @female);
  while (my $row = $csv->getline( $fh )) {
	if ($row->[0] =~ /^\d+/) {
	   $focus = $row->[0];
	   next;
	}elsif ($row->[0] =~ /STATE:\s*(.*)\s*/) {
	   $state = $1;
	   @results = fetch_row($row);
        }elsif ($row->[0] =~ /\bmale/i) {
	   @male = fetch_row($row);
	}elsif ($row->[0] =~ /\bfemale/i) {
	   @female = fetch_row($row);
	}

        next unless $state && scalar(@male) && scalar(@female);

	for (0 .. $#results) {
	    print "\"$focus\",\"$state\",$years[$_],$male[$_],$female[$_],$results[$_]\n";
	}

	$state = '';
	@male = ();
	@female = ();
  }

sub fetch_row {
    my $row = shift;
    my @values;
    for (@columns) {
	push(@values, $row->[$_] !~ /\d/ ? '' : $row->[$_]);
    }
@values
}

	   
