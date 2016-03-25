#(0 (1.0000000000005116e-2 1))

#ex/pol_bday.prob (1) 1 1/1 1.000000 6.92736911773682

sub float {
  my ($n) = @_;
  if ($n =~ m/^(.+)\/(.+)$/) {
    return ($1 / $2);
  } else {
    return int($n);
  }
}

while (my $line = <STDIN>) {
  chomp $line;
  if ($line =~ m/^\(/) {
    my ($t, $p) = ($line =~ m/\(.+ \((.+) (.+)\)\)/);
    #print "$t " . float($p). "\n";
  } elsif ($line =~ m/^ex\/pol_bday\.prob [^\(]/) {
    my ($p, $t) = ($line =~ m/ ([^ ]+?) ([^ ]+?)$/);
    #print "$t $p\n";
  } elsif ($line =~ m/^ex\/pol_bday_large\.prob [^\(]/) {
    my ($p, $t) = ($line =~ m/ ([^ ]+?) ([^ ]+?)$/);
    #print "$t $p\n";
  } elsif ($line =~ m/^ex\/pol_bday\.prob\t\(3/) {
    #my ($p, $t, $tt) = ($line =~ m/ ([^ ]+?) ([^ ]+?) ([^ ]+?)$/);
    #print "$t $tt $p\n";
    print $line . "\n";
  }
}

