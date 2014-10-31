use strict;
use Storable;
use Data::Dumper;
use List::Util qw(min max);

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
	    'simplify' => 'random',
	   };

make_report ($full);

sub make_report {
  my ($full) = @_;

  #my $domains = ['poly', 'octalatte', 'box'];
  my $domains = ['box'];

  my $data_stats_all;

  my $col_stats_prec;
  #my $col_stats_realtime;
  my $col_stats_belief;
  my $col_stats_relative;
  my $col_stats_domain;

  my $col_prec;
  #my $col_time;
  #my $col_realtime;
  my $col_belief;

  my $relative_vals = {};

  my $mins = {'poly' => 999,
	      'octa' => 999,
	      'box' => 999,
	     };
  my $maxs = {'poly' => 0,
	      'octa' => 0,
	      'box' => 0};

  my $pol_cols;

  my $dir = gen::make_report_dir();

  my $file_data_stats = "$dir/data_stats.tsv";
  my $file_data_box = "$dir/data_box.tsv";
  my $file_data = {'box' => $file_data_box};

  foreach my $domain (@$domains) {
    $full->{'domain'} = $domain;

    my $data = gen::gen_prec_data($full);

    if (! defined $data) {
      die "no data found for this experiment";
    }

    $col_prec = $data->{'header'}->{'cols'}->{'precision'};
    #$col_time      = $data->{'header'}->{'cols'}->{'_epoch total (s)'};
    #$col_realtime  = $data->{'header'}->{'cols'}->{'_epoch total (real s)'};
    $pol_cols = $data->{'header'}->find_cols("policy");
    if (scalar(@$pol_cols) <= $full->{'policy_num'}) {
      die "data file didn't have policy # $full->{'policy_num'}";
    }
    $col_belief  = $data->{'header'}->{'cols'}->{$pol_cols->[$policy_num]};

      #use Data::Dumper;
      #print Dumper($data);

    my $file_data = $file_data->{$domain};

    $data->{'header'}->{'sep'} = "\t";
    $data->write_to_file($file_data);

    my $data_stats = gen::summary_data_of_data($data, [#'belief',
						       #						       '_epoch total (real s)',
						       $pol_cols->[$policy_num],
						      ]);

    $col_stats_domain = $data_stats->{'header'}->add_col("domain");

    my $colname_stats_belief = '_median ' . $pol_cols->[$policy_num];

    $col_stats_prec = $data_stats->{'header'}->{'cols'}->{'precision'};
#    $col_stats_realtime  = $data_stats->{'header'}->{'cols'}->{'_median _epoch total (real s)'};
#    $col_stats_belief    = $data_stats->{'header'}->{'cols'}->{$colname_stats_belief};

    my $best_belief = $data_stats->fold(sub {
					  my ($best, $line) = @_;
					  my $line_belief = $line->get_data_at_index($col_stats_belief);
					  if ($line_belief <= $best) {return $line_belief} else {return $best}
					}, 1);

    $col_stats_relative = $data_stats->{'header'}->add_col("relative");

    $data_stats->foreach_line(sub {
				my ($line) = @_;
				my $belief = $line->get_data_at_index($col_stats_belief);
				my $ratio = $belief - $best_belief;
				$relative_vals->{$ratio} = 1;
				$line->set_data_at_index($col_stats_relative, $ratio);
				$line->set_data_at_index($col_stats_domain, $domain);
			      });

    if (! $data_stats_all) {
      $data_stats_all = $data_stats;
    } else {
      $data_stats_all->add_csv($data_stats);
    }

    print "best = $best_belief\n";
  }

  my @relative_vals = keys %$relative_vals;
  @relative_vals = sort {$a <=> $b} @relative_vals;
  my $psizes = {};

  my $num_vals = scalar(@relative_vals);

  for (my $i = 0; $i < $num_vals; $i++) {
    my $ratio = $relative_vals[$i];
    $psizes->{$ratio} = 0.2 + ($i / $num_vals) * 1.2;
  }

  my $col_stats_psize = $data_stats_all->{'header'}->add_col("psize");
  my $cname = $pol_cols->[$policy_num];
  my $col_stats_belief_qlower = $data_stats_all->{'header'}->{'cols'}->{"_q_lower $cname"};
  my $col_stats_belief_qupper = $data_stats_all->{'header'}->{'cols'}->{"_q_upper $cname"};
  my $col_stats_belief_median = $data_stats_all->{'header'}->{'cols'}->{"_median $cname"};
  my $col_stats_belief_min = $data_stats_all->{'header'}->{'cols'}->{"_min $cname"};
  my $col_stats_belief_max = $data_stats_all->{'header'}->{'cols'}->{"_max $cname"};

  $data_stats_all->foreach_line
    (
     sub {
       my ($line) = @_;
       my $ratio = $line->get_data_at_index($col_stats_relative);
       my $belief = $line->get_data_at_index($col_stats_belief);
       $line->set_data_at_index($col_stats_psize, ($belief == 1 ? 1.5 : $psizes->{$ratio}));

       my $domain = $line->get_data_at_index($col_stats_domain);
       my $amin = $line->get_data_at_index($col_stats_belief_qlower);
       my $amax = $line->get_data_at_index($col_stats_belief_qupper);

       if ($amin < $mins->{$domain}) {
	 $mins->{$domain} = $amin;
       }
       if ($amax > $maxs->{$domain}) {
	 $maxs->{$domain} = $amax;
       }
     });

  #my $file_data = "$dir/data.tsv";
  my $file_gnu = "$dir/plot.gnu";

  my $file_template = $config::DIR_TEMPLATES . "/plot_random_template.gnu";

  my $file_graph_eps = "$dir/graph.eps";

  $data_stats_all->{'header'}->{'sep'} = "\t";
  $data_stats_all->write_to_file($file_data_stats);

  #$data->{'header'}->{'sep'} = "\t";
  #$data->write_to_file($file_data);

  my $template = util::read_from_file($file_template);

  my $gnu_stats = util::fill_template($template,
				      {
				       'FILE_IN_BOX' => $file_data_box,
				       'FILE_IN_STATS' => $file_data_stats,
				       'COL_PREC' => $col_prec + 1,
				       #'COL_REALTIME' => $col_realtime + 1,
				       'COL_BELIEF' => $col_belief + 1,
#				       'STATS_MIN_POLY' => $mins->{'poly'},
#				       'STATS_MAX_POLY' => $maxs->{'poly'},
#				       'STATS_MIN_OCTA' => $mins->{'octa'},
#				       'STATS_MAX_OCTA' => $maxs->{'octa'},
				       'STATS_MIN_BOX' => $mins->{'box'},
				       'STATS_MAX_BOX' => $maxs->{'box'},
#				       'STATS_MIN_ALL' => min(($mins->{'poly'}, $mins->{'octa'}, $mins->{'box'})),
#				       'STATS_MAX_ALL' => max(($maxs->{'poly'}, $maxs->{'octa'}, $maxs->{'box'})),
				       'STATS_MIN_ALL' => $mins->{'box'},
				       'STATS_MAX_ALL' => $maxs->{'box'},
				       'COL_STATS_PSIZE'  => $col_stats_psize + 1,
				       'COL_STATS_DOMAIN' => $col_stats_domain + 1,
				       'COL_STATS_PRECISION' => $col_stats_prec + 1,
#				       'COL_STATS_REALTIME' => $col_stats_realtime + 1,
				       'COL_STATS_BELIEF' => $col_stats_belief + 1,
				       'COL_STATS_RELATIVE' => $col_stats_relative + 1,
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
#					   'GRAPH_OUT2'  => "$dir/stats_graph_intervals.eps",
					   'TERM'       => 'postscript eps enhanced "Helvetica" 20',
					   #'TERM'       => 'postscript eps enhanced "cmmi10" 20',
					   #'TERM'       => 'postscript eps enhanced solid "cm-super" 20',
					  });

  util::write_to_file($file_gnu, $gnu_stats_eps);

  gen::run_gnuplot($file_gnu);

  print "print $dir\n";

  #`open $dir`;
  #`open $dir/stats_graph_intervals.eps`;
  `open $dir/stats_graph.eps`;
  #`pushd .; cd $dir`;
}
