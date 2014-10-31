use strict;
use Storable;
use Data::Dumper;

require 'tasks_config.pl';
require 'tasks_util.pl';
require 'gen_util.pl';

my ($file) = @ARGV;
die "file" if ! defined $file;

make_report ($file);

sub make_report {
  my $data = csv::new_from_file($file, "policy_on", "\t");

  my $cname = "max_belief_float";

  my $col_policy  = $data->{'header'}->{'cols'}->{'policy_on'};
  my $col_belief  = $data->{'header'}->{'cols'}->{$cname};
  my $col_secret  = $data->{'header'}->{'cols'}->{'secret_value'};

  my $data_stats = gen::summary_data_of_data($data, [$cname]);

  my $col_stats_policy = $data_stats->{'header'}->{'cols'}->{'policy_on'};
  my $col_stats_belief_qlower = $data_stats->{'header'}->{'cols'}->{"_q_lower $cname"};
  my $col_stats_belief_qupper = $data_stats->{'header'}->{'cols'}->{"_q_upper $cname"};
  my $col_stats_belief_median = $data_stats->{'header'}->{'cols'}->{"_median $cname"};
  my $col_stats_belief_min = $data_stats->{'header'}->{'cols'}->{"_min $cname"};
  my $col_stats_belief_max = $data_stats->{'header'}->{'cols'}->{"_max $cname"};

  my $dir = gen::make_report_dir();

  my $file_data = "$dir/data.tsv";
  my $file_gnu = "$dir/plot.gnu";

  my $file_template = $config::DIR_TEMPLATES . "/plot_smc_template.gnu";

  my $file_graph_eps = "$dir/graph.eps";
  my $file_graph_latex = "$dir/graph.tex";

  my $file_data_stats = "$dir/data_stats.tsv";

  $data_stats->{'header'}->{'sep'} = "\t";
  $data_stats->write_to_file($file_data_stats);

  $data->{'header'}->{'sep'} = "\t";
  $data->write_to_file($file_data);

  my $template = util::read_from_file($file_template);

  my $gnu_stats = util::fill_template($template,
				      {
				       'FILE_IN' => $file_data,
				       'FILE_IN_STATS' => $file_data_stats,
				       'COL_POLICY' => $col_policy + 1,
				       'COL_BELIEF' => $col_belief + 1,
				       'COL_STATS_POLICY' => $col_stats_policy + 1,
				       'COL_STATS_BELIEF_MIN' => $col_stats_belief_min + 1,
				       'COL_STATS_BELIEF_MAX' => $col_stats_belief_max + 1,
				       'COL_STATS_BELIEF_MEDIAN' => $col_stats_belief_median + 1,
				       'COL_STATS_BELIEF_QLOWER' => $col_stats_belief_qlower + 1,
				       'COL_STATS_BELIEF_QUPPER' => $col_stats_belief_qupper + 1,

				      });

  my $gnu_stats_eps = util::fill_template($gnu_stats,
					  {
					   #'GRAPH_OUT' => "$dir/stats_graph.png",
					   #'TERM'      => 'png crop'
					   'GRAPH_OUT'  => "$dir/stats_graph.eps",
					   'TERM'       => 'postscript eps enhanced "Helvetica" 20',
					   #'TERM'       => 'postscript eps enhanced "cmmi10" 20',
					   #'TERM'       => 'postscript eps enhanced solid "cm-super" 20',

					  });

  my $gnu_stats_latex = util::fill_template($gnu_stats,
					  {
					   'GRAPH_OUT'  => "$dir/stats_graph.tex",
					   'TERM'       => 'latex',
					  });

  util::write_to_file($file_gnu, $gnu_stats_eps);
  gen::run_gnuplot($file_gnu);

  util::write_to_file($file_gnu, $gnu_stats_latex);
  gen::run_gnuplot($file_gnu);

  print "print $dir\n";

  #`open $dir`;
  `open $dir/stats_graph.eps`;
}
