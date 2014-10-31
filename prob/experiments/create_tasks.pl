use Storable;
use strict;

require 'tasks_config.pl';

if (scalar(@ARGV) < 4) {
  print "policy prec-begin prec-end prec-inc reps domain others-opts simplify\n";
  exit(0);
}

my ($pol, $prec_begin, $prec_end, $prec_inc, $reps, $domain, $simplify) = @ARGV;

$prec_inc = 1 if ! $prec_inc;
$reps = 1 if not defined $reps;

my $temp;
if (-r $config::FILE_TASKS_ADD) {
  $temp = retrieve $config::FILE_TASKS_ADD;
} else {
  $temp = {};
}

my $cnt = 0;

for (my $prec = $prec_begin; $prec <= $prec_end; $prec += $prec_inc) {
  for (my $rep = 0; $rep < $reps; $rep++) {
    $cnt += 1;
    my $k = "$pol-$prec-$domain-$simplify-$rep";

#    my $opts = {};

#    foreach my $other (@others) {
#      if ($other =~ m/^--(.+?)=(.+?)$/) {
#	$opts->{$1} = $2;
#	$k .= "-$1=$2";
#      } elsif ($other =~ m/^--(.+?)$/) {
#	$opts->{$1} = 1;
#	$k .= "$1";
#      }
#    }

    print "adding task $k\n";

    $temp->{$k} = {'key' => $k,
		   'policy' => $pol,
		   'rep' => $rep,
		   'precision' => $prec,
		   'simplify' => $simplify,
		   'domain' => $domain,
#		   'options' => $opts,
#		   'options_cmd' => [@others],
		  };
  }
}

store $temp, $config::FILE_TASKS_ADD;

print "added $cnt task(s)\n";
