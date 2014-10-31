use Benchmark ':hireswallclock';
use IPC::Open3;
use Storable;
use Data::Dumper;
use strict;

require 'tasks_util.pl';

my $pid;
my ($greader, $gwriter, $gerrer);

sub task_prob {
  my ($dir, $task) = @_;

  my $ret =
    eval {
      local $SIG{ALRM} = sub {
	master::task_timeout($task);

	ipc::print_child("killing $pid");

	close $greader if $greader;
	close $gwriter if $gwriter;
	close $gerrer if $gerrer;

	kill (-9, $pid);

	waitpid($pid, 0);

	die("\n");
      };

      alarm $config::TIMEOUT;
      my $success = _task_prob($dir, $task);
      alarm 0;

      $success;
    };

  if ($@) {
    print "an error happened: $@\n";
    undef $@;
    return 1;
  } else {
    return $ret;
  }
}

sub _task_prob {
  my ($dir, $task) = @_;

  my $k         = $task->{'key'};
  my $policy    = $task->{'policy'};
  my $precision = $task->{'precision'};
  my $rep       = $task->{'rep'};
  my $domain    = $task->{'domain'};
  my $simplify  = $task->{'simplify'};

  my $file_out = "$dir/results.tsv";
  my $file_timing = "$dir/timing.csv";
  my $file_latte = "$dir/latte.csv";

  my $cmd = "../prob --pmock --bench $file_timing --bench-latte $file_latte --precision $precision --domain $domain --simplify $simplify --seed $rep $policy";

  my $conf = $task;
  $conf->{'file_timing'} = 'timing.csv';
  $conf->{'file_out'} = 'results.tsv';
  $conf->{'file_latte'} = 'latte.csv';
  $conf->{'policy_data'} = util::read_from_file($policy);

  `cp $policy $dir`;

  my $tstart = Benchmark->new;
  my $tprev = $tstart;

  my ($writer, $reader, $errer);

  $pid = open3($writer, $reader, $errer,
	       $cmd);

  if (! $pid) {
    return 0;
  }

  $greader = $reader;
  $gwriter = $writer;
  $gerrer = $errer;

  ipc::print_child("running at $pid: $cmd");

  my $i = 0;

  my $failed = 1;

  my $lines = "";

  while (not eof($reader)) {
    my $line = <$reader>;
    $lines .= $line;
    chomp $line;
    #print "got line [$line]\n";
    if ($line =~ m/no errors/) {
      $failed = 0;
    }
    if ($line =~ m/overall max_belief = (.+) = (.+)/) {
      $i++;

      my ($max_prec_frac, $max_prec_float) = ($1, $2);
      my $t = Benchmark->new;

      my $elapsed_start = (timediff($t, $tstart))->[0];
      my $elapsed_prev = (timediff($t, $tprev))->[0];

      my $write = join("\t",
		       ($policy, $i, $precision, $max_prec_frac, $max_prec_float, $elapsed_start, $elapsed_prev)) . "\n";
      #print $write;

      open (OUT, ">>", $file_out) or die $!;
      print OUT $write;
      close (OUT);

      $tprev = $t;
    }
  }

  close ($reader) if $reader;
  close ($writer) if $writer;
  close ($errer) if $errer;
  waitpid($pid, 0);

  #ipc::print_child("eof = " . eof($reader));
  #ipc::print_child("err = " . $@);

  $task->{'failed'} = $failed;

  if ($failed) {
    print STDERR "\n\n################# prob failed #####################\n";
    print STDERR $lines;
    print STDERR     "###################################################\n";
  }

  return (! $failed);
}

1;
