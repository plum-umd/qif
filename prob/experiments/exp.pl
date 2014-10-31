use Benchmark ':hireswallclock';
use IPC::Open3;

my ($pol, $precision, $file_out) = @ARGV;

$file_out = "experiments.dat" if ! $file_out;

my $cmd = "./prob --pmock --ppss --precision $precision $pol";

print "running $cmd\n";

my $tstart = Benchmark->new;
my $tprev = $tstart;

my ($writer, $reader, $errer);
open3($writer, $reader, $errer,
      $cmd);

my $i = 0;

while (my $line = <$reader>) {
  chomp $line;
  if ($line =~ m/overall max_belief = (.+) = (.+)/) {
    $i++;

    my ($max_prec_frac, $max_prec_float) = ($1, $2);
    my $t = Benchmark->new;

    my $elapsed_start = (timediff($t, $tstart))->[0];
    my $elapsed_prev = (timediff($t, $tprev))->[0];

    my $write = join("\t",
		     ($pol, "($i)", $precision, $max_prec_frac, $max_prec_float, $elapsed_start, $elapsed_prev)) . "\n";

    print $write;

    open (OUT, ">>", $file_out) or die $!;
    print OUT $write;
    close (OUT);

    $tprev = $t;
  }
}
