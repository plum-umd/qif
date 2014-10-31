use strict;
use Storable;
use Data::Dumper;

require 'tasks_config.pl';
require 'tasks_util.pl';
require 'gen_util.pl';

my ($pol, $query_num, $policy_num) = @ARGV;
die "policy query_num policy_num" if ! defined $pol;
$query_num = 0 if ! defined $query_num;
$policy_num = 0 if ! defined $policy_num;

my $full = {'policy' => $pol,
	    'query_num' => $query_num,
	    'policy_num' => $policy_num,
	    'domain' => 'poly',
	    'simplify' => 'halfs',
	   };

make_prec_report($full);

sub make_prec_report {
  my ($full) = @_;

  print "making report for $full->{'policy'}\n";

  my $data = gen::gen_prec_data($full);

  if (! defined $data) {
    die "no data found for this experiment";
  }

  my $col_precision = $data->{'header'}->{'cols'}->{'precision'};
  my $col_time      = $data->{'header'}->{'cols'}->{'_epoch total (s)'};
  my $col_realtime  = $data->{'header'}->{'cols'}->{'_epoch total (real s)'};
  my $pol_cols = $data->{'header'}->find_cols("policy");
  if (scalar(@$pol_cols) <= $full->{'policy_num'}) {
    die "data file didn't have policy # $full->{'policy_num'}";
  }
  my $col_prob = $data->{'header'}->{'cols'}->{$pol_cols->[$policy_num]};

  my $data_stats = gen::summary_data_of_data($data, ['_epoch total (real s)',
						     '_epoch total (s)',
						     $pol_cols->[$policy_num]]);

  my $col_stats_precision = $data_stats->{'header'}->{'cols'}->{'precision'};
  my $col_stats_realtime  = $data_stats->{'header'}->{'cols'}->{'_average _epoch total (real s)'};
  my $col_stats_prob      = $data_stats->{'header'}->{'cols'}->{'_average ' . $pol_cols->[$policy_num]};

  print "using prob from column \"$pol_cols->[$policy_num]\"\n";

  my $dir = gen::make_report_dir();
  #print "making report $dir\n";

  my $file_data = "$dir/data.tsv";
  my $file_template = $config::DIR_TEMPLATES . "/plot_template.gnu";
  my $file_template_stats = $config::DIR_TEMPLATES . "/plot_template_stats.gnu";
  my $file_gnu = "$dir/plot.gnu";
  my $file_stats_gnu = "$dir/plot_stats.gnu";
  my $file_realtime_gnu = "$dir/plot_realtime.gnu";
  #  my $file_graph_eps = "$dir/graph.eps";
  my $file_graph_realtime_png = "$dir/graph_realtime.png";
  my $file_graph2_realtime_png = "$dir/graph_realtime2.png";
  my $file_graph_png = "$dir/graph.png";
  my $file_graph2_png = "$dir/graph2.png";
  my $file_conf = "$dir/conf.storable";

  my $file_data_stats = "$dir/data_stats.tsv";

  $data_stats->{'header'}->{'sep'} = "\t";
  $data_stats->write_to_file($file_data_stats);

  my $conf = gen::conf_sample($full);
  store $conf, $file_conf;

  $data->{'header'}->{'sep'} = "\t";
  $data->write_to_file($file_data);

  my $template = util::read_from_file($file_template);
  my $template_stats = util::read_from_file($file_template_stats);

  my $gnu = util::fill_template($template,
				{ 'FILE_IN' => $file_data,
				  'COL_PROB' => $col_prob + 1,
				  'COL_PREC' => $col_precision + 1,
				});

  my $gnu_stats = util::fill_template($template_stats,
				      { 'FILE_IN' => $file_data_stats,
					'COL_PROB' => $col_stats_prob + 1,
					'COL_PREC' => $col_stats_precision + 1,
					'COL_TIME' => $col_stats_realtime + 1,
					'GRAPH_OUT' => "$dir/stats_graph.png",
					'GRAPH2_OUT' => "$dir/stats_graph2.png",
					'TERM' => 'png crop',
				      });

  my $gnu_eps = util::fill_template($template_stats,
				    { 'FILE_IN' => $file_data_stats,
				      'COL_PROB' => $col_stats_prob + 1,
				      'COL_PREC' => $col_stats_precision + 1,
				      'COL_TIME' => $col_stats_realtime + 1,
				      'GRAPH_OUT' => "$dir/stats_graph.eps",
				      'GRAPH2_OUT' => "$dir/stats_graph2.eps",
				      'TERM' => 'postscript eps 22',
				    }
				   );

  my $gnu_png = util::fill_template
    ($gnu,
     {
      'COL_TIME' => $col_time + 1,
      'TERM' => 'png crop',
      'GRAPH_OUT' => $file_graph_png,
      'GRAPH2_OUT' => $file_graph2_png,
     },
    );

  my $gnu_realtime_png = util::fill_template
    (
     $gnu,
     {
      'COL_TIME' => $col_realtime + 1,
      'TERM' => 'png crop',
      'GRAPH_OUT' => $file_graph_realtime_png,
      'GRAPH2_OUT' => $file_graph2_realtime_png,},
    );

  util::write_to_file($file_gnu, $gnu_eps);
  gen::run_gnuplot($file_gnu);

  #util::write_to_file($file_gnu, $gnu_png);
  #gen::run_gnuplot($file_gnu);

  #util::write_to_file($file_realtime_gnu, $gnu_realtime_png);
  #gen::run_gnuplot($file_realtime_gnu);

  #util::write_to_file($file_stats_gnu, $gnu_stats);
  #gen::run_gnuplot($file_stats_gnu);

  print "\n";

  `open $dir`;
}
