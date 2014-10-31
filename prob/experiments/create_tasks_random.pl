use Storable;
use strict;

require 'tasks_config.pl';

if (scalar(@ARGV) < 4) {
  print "policy prec-begin prec-end prec-inc seed-begin seed-end others-opts\n";
  exit(0);
}

my ($pol, $prec_begin, $prec_end, $prec_inc, $rand_begin, $rand_end, @others) = @ARGV;

$prec_inc = 1 if ! $prec_inc;
push @others, "--simrandom";

my $temp;
if (-r $config::FILE_TASKS_ADD) {
  $temp = retrieve $config::FILE_TASKS_ADD;
} else {
  $temp = {};
}

for (my $prec = $prec_begin; $prec <= $prec_end; $prec += $prec_inc) {
  for (my $seed = $rand_begin; $seed <= $rand_end; $seed += 1) {
    my $k = "$pol-$prec-$seed";

    my $opts = {};



    foreach my $other (@others) {
      if ($other =~ m/^--(.+?)=(.+?)$/) {
	$opts->{$1} = $2;
	$k .= "-$1=$2";
      } elsif ($other =~ m/^--(.+?)$/) {
	$opts->{$1} = 1;
	$k .= "-$1";
      }
    }

    print "adding task $k\n";

    $temp->{$k} = {'key' => $k,
		   'policy' => $pol,
		   'precision' => $prec,
		   'options' => $opts,
		   'seed' => $seed,
		   'options_cmd' => [@others],
		  };
  }
}

store $temp, $config::FILE_TASKS_ADD;
