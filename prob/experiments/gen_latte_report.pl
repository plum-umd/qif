use strict;
use Storable;
use Data::Dumper;

require 'tasks_config.pl';
require 'tasks_util.pl';
require 'gen_util.pl';

my ($type, $dims) = @ARGV;

make_latte_report($type);

sub make_latte_report {
  my ($type) = @_;
  print "making latte report (restricted to $type)\n";

  my $data = gen::gen_latte_data($type, $dims);

  die ("no latte data found") if not defined $data;

  my $col_dims      = $data->{'header'}->{'cols'}->{'dimensions'};
  my $col_conss     = $data->{'header'}->{'cols'}->{'constraints'};
  my $col_realtime  = $data->{'header'}->{'cols'}->{'time (real s)'};

  my $data_stats = gen::summary_data_of_data($data, ['time (real s)']);

  my $col_stats_conss   = $data_stats->{'header'}->{'cols'}->{'constraints'};
  my $col_stats_qlower  = $data_stats->{'header'}->{'cols'}->{'_q_lower time (real s)'};
  my $col_stats_qupper  = $data_stats->{'header'}->{'cols'}->{'_q_upper time (real s)'};
  my $col_stats_median  = $data_stats->{'header'}->{'cols'}->{'_median time (real s)'};
  my $col_stats_min  = $data_stats->{'header'}->{'cols'}->{'_min time (real s)'};
  my $col_stats_max  = $data_stats->{'header'}->{'cols'}->{'_max time (real s)'};

  my $dir = gen::make_report_dir();

  my $file_data       = "$dir/data.tsv";
  my $file_data_stats = "$dir/data_stats.tsv";
  my $file_template = $config::DIR_TEMPLATES . "/plot_latte_template.gnu";

  my $file_gnu = "$dir/plot.gnu";
  my $file_out = "$dir/graph";

  $data_stats->{'header'}->{'sep'} = "\t";
  $data_stats->write_to_file($file_data_stats);

  #my $file_conf = "$dir/conf.storable";
  #my $conf = gen::conf_sample($full);
  #store $conf, $file_conf;

  $data->{'header'}->{'sep'} = "\t";
  $data->write_to_file($file_data);

  my $template = util::read_from_file($file_template);

  my $terms = [['png crop', 'png'],
	       ['postscript eps 22', 'eps']];

  my $gnu = util::fill_template($template,
				{ 'FILE_IN1' => $file_data,
				  'FILE_IN2' => $file_data_stats,
				  'COL_CONSS' => $col_conss + 1,
				  'COL_TIME'  => $col_realtime + 1,
				  'COL2_CONSS' => $col_stats_conss + 1,
				  'COL2_QLOWER' => $col_stats_qlower + 1,
				  'COL2_QUPPER' => $col_stats_qupper + 1,
				  'COL2_MIN' => $col_stats_min + 1,
				  'COL2_MAX' => $col_stats_max + 1,
				  'COL2_MEDIAN' => $col_stats_median + 1,
				});

  foreach my $term (@$terms) {
    my $gnu_term = util::fill_template($gnu,
				       {'TERM' => $term->[0],
					'FILE_OUT' => "$file_out.$term->[1]"});
    util::write_to_file($file_gnu, $gnu_term);
    gen::run_gnuplot($file_gnu);
  }

  print "\n";

  `open $dir`;
}
