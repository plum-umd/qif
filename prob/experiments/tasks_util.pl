use strict;
use Time::HiRes;

package util;

sub key_of_data {
  my ($d) = @_;

  my $reftype = ref $d;

  if ($reftype eq "ARRAY") {
    return join(",", @$d);
  } elsif ($reftype eq "SCALAR") {
    return $d;
  } else {
    die ("dont know how to make a key of $reftype reference");
  }
}

sub new_dir {
  my ($prefix, $base) = @_;

  my ($sec, $msec) = Time::HiRes::gettimeofday();
  my $filename = "$base/${prefix}_${sec}_$msec";

  while (-e $filename) {
    usleep(10);
    ($sec, $msec) = Time::HiRes::gettimeofday();
    $filename = "$base/${prefix}_${sec}_$msec";
  }

  mkdir $filename;

  return $filename;
}

sub kill_all {
  my ($pid) = @_;

  my $children = `ps -o pid --no-headers --ppid $pid`;

  print "children = $children\n";
}

sub fill_template {
  my ($data, $temp) = @_;

  my @ks = keys %$temp;
  @ks = sort {length($b) <=> length($a)} @ks;

  foreach my $k (@ks) {
    my $v = $temp->{$k};
    $data =~ s/$k/$v/g;
  }

  return $data;
}

sub read_from_file {
  my ($file) = @_;
  return join("", read_lines_from_file($file));
}

sub read_lines_from_file {
  my ($file) = @_;

  open (my $in, $file) or die "cannot read file $file\n";
  my @lines = <$in>;
  close ($in);

  return @lines;
}

sub write_to_file {
  my ($file, $data) = @_;

  open (my $out, ">$file");
  print $out $data;
  close ($out);
}

package csv_header;
sub new {
  my $self = {};
  $self->{'cols'} = {};
  $self->{'num_cols'} = 0;
  $self->{'sep'} = ",";
  $self->{'head'} = [];

  bless $self;
  return $self;
}

sub copy {
  my $self = {};

  my $ret = csv_header->new();

  $ret->{'cols'} = {%{$self->{'cols'}}};
  $ret->{'num_cols'} = $self->{'num_cols'};
  $ret->{'sep'} = $self->{'sep'};
  $ret->{'head'} = [@{$self->{'head'}}];

  return $ret
}

sub find_cols {
  my ($self, $pattern) = @_;

  my $ret = [grep {$_ =~ m/$pattern/} @{$self->{'head'}}];

  return $ret;
}

sub add_col {
  my ($self, $colname) = @_;
  $self->{'cols'}->{$colname} = $self->{'num_cols'};
  $self->{'num_cols'} += 1;
  push @{$self->{'head'}}, $colname;
  return ($self->{'num_cols'} - 1);
}

sub new_from_string {
  my ($s, $sep, $keyer) = @_;
  $sep = "," if not $s;

  my $self = new();
  $self->{'sep'} = $sep;
  $self->{'keyer'} = $keyer;

  my $i = 0;
  $self->{'head'} = [split(/$sep/,$s)];
  foreach my $cname (@{$self->{'head'}}) {
    $self->{'cols'}->{$cname} = $i;
    $i++;
  }
  $self->{'num_cols'} = $i;

  if (defined $keyer && ! defined $self->{'cols'}->{$keyer}) {
    die ("key column $keyer not present in header string $s");
  }

  return $self;
}

sub to_string {
  my ($self) = @_;
  return join($self->{'sep'}, @{$self->{'head'}});
}

package csv_line;
sub new {
  my ($header) = @_;
  my $self = {};
  #$self->{'line'} = [];
  $self->{'header'} = $header;
  $self->{'data'} = {};

  bless $self;

  return $self;
}

sub copy {
  my ($self) = @_;
  my $ret = csv_line->new($self->{'header'}->copy());
  $ret->{'data'} = {%{$self->{'data'}}};
  return $ret;
}

sub new_from_string {
  my ($s, $header) = @_;

  my $self = new($header);
  #$self->{'line'} = [split(/$header->{'sep'}/, $s)];
  my $temp = [split(/$header->{'sep'}/, $s)];

  for (my $i = 0; $i < scalar(@$temp); $i++) {
    $self->{'data'}->{$header->{'head'}->[$i]} = $temp->[$i];
  }

  return $self;
}

sub set_header {
  my ($self, $newhead) = @_;
  $self->{'header'} = $newhead;
}

sub get_data_at_index {
  my ($self, $i) = @_;
  return $self->{'data'}->{$self->{'header'}->{'head'}->[$i]};
  #return $self->{'line'}->[$i];
}

sub get_data_at_key {
  my ($self, $k) = @_;
  #return $self->{'line'}->[$self->{'header'}->{'cols'}->{$k}];
  return $self->{'data'}->{$k};
}

sub set_data_at_index {
  my ($self, $i, $v) = @_;
  $self->{'data'}->{$self->{'header'}->{'head'}->[$i]} = $v;
  #$self->{'line'}->[$i] = $v;
}

sub set_data_at_key {
  my ($self, $k, $v) = @_;
  #$self->{'line'}->[$self->{'header'}->{'cols'}->{$k}] = $v;
  $self->{'data'}->{$k} = $v;
}

sub to_string {
  my ($self) = @_;
  return join($self->{'header'}->{'sep'},
	      map {$self->{'data'}->{$self->{'header'}->{'head'}->[$_]}}
	      (0 .. (scalar(@{$self->{'header'}->{'head'}}) - 1)));
}

package csv;

sub new {
  my ($keyer) = @_;

  my $self = {};
  $self->{'header'} = undef;
  $self->{'lines'} = [];
  $self->{'lines_keyed'} = {};

  bless($self);

  return $self;
}

sub write_to_file {
  my ($self, $filename) = @_;

  my $sep = $self->{'header'}->{'sep'};

  open(my $out, ">$filename") or die "cannot open $filename for writing";
  print $out $self->{'header'}->to_string() . "\n";
  foreach my $line (@{$self->{'lines'}}) {
    print $out $line->to_string() . "\n";
  }
  close($out);
}

sub new_from_file {
  my ($filename, $keyer, $sep) = @_;

  $sep = "," if not $sep;

  my @lines = util::read_lines_from_file($filename);

  my $head_line = shift @lines;
  chomp $head_line;

  my $self = new();
  $self->{'header'} = csv_header::new_from_string($head_line, $sep, $keyer);

  foreach my $line (@lines) {
    chomp $line;
    my $add = csv_line::new_from_string($line, $self->{'header'});
    push @{$self->{'lines'}}, $add;
    if (defined $keyer) {
      my $hash = $add->get_data_at_key($keyer);
      $self->{'lines_keyed'}->{$hash} = [] if not defined $self->{'lines_keyed'}->{$hash};
      push @{$self->{'lines_keyed'}->{$hash}}, $add;
    }
  }

  return $self;
}

sub set_header {
  my ($self, $newhead) = @_;

  $self->{'header'} = $newhead;

  foreach my $line (@{$self->{'lines'}}) {
    $line->set_header($newhead);
  }
}

sub add_col {
  my ($self, $colname) = @_;
  my $ret = $self->{'header'}->add_col($colname);
  $self->foreach_line(sub {
			push @{$_[0]->{'line'}}, "-";
		      });
  return $ret;
}

sub get_line {
  my ($self, $l) = @_;
  return $self->{'lines'}->[$l];
}

sub get_lines_at_key {
  my ($self, $i) = @_;
  if (! defined $self->{'header'}->{'keyer'}) {
    die("key column was not specified");
  }
  return $self->{'lines_keyed'}->{$i};
}

sub foreach_line {
  my ($self, $do) = @_;
  foreach my $line (@{$self->{'lines'}}) {
    $do->($line);
  }
}

sub filter {
  my ($self, $ffun) = @_;

  my $new_lines = [];

  $self->foreach_line(sub {
			my ($line) = @_;
			if ($ffun->($line)) {
			  push @$new_lines, $line;
			}
		      });

  $self->{'lines'} = $new_lines;
}

sub fold {
  my ($self, $fun, $accum) = @_;

  $self->foreach_line(sub {
			my ($line) = @_;
			$accum = $fun->($accum, $line);
		      });

  return $accum;
}

sub add_line {
  my ($self, $line) = @_;

  $line->{'header'} = $self->{'header'};

  push @{$self->{'lines'}}, $line;

  my $keyer = $self->{'header'}->{'keyer'};
  if (defined $keyer) {
    my $hash = $line->get_data_at_key($keyer);
    $self->{'lines_keyed'}->{$hash} = [] if not defined $self->{'lines_keyed'}->{$hash};
    push @{$self->{'lines_keyed'}->{$hash}}, $line;
  }
}

sub add_csv {
  my ($self, $other) = @_;

  $other->foreach_line(sub {
			 my ($line) = @_;
			 $self->add_line($line);
		       });
}

sub add_linestring {
  my ($self, $lines) = @_;

  my $line = csv_line::new_from_string($lines, $self->{'header'});

  push @{$self->{'lines'}}, $line;

  my $keyer = $self->{'header'}->{'keyer'};
  if (defined $keyer) {
    my $hash = $line->get_data_at_key($keyer);
    $self->{'lines_keyed'}->{$hash} = [] if not defined $self->{'lines_keyed'}->{$hash};
    push @{$self->{'lines_keyed'}->{$hash}}, $line;
  }
}

1;
