use strict;
use Data::Dumper;

require 'tasks_config.pl';
require 'tasks_util.pl';
require 'gen_util.pl';

gen_table();

sub gen_table {
  my $pols = [['bday.pol', 0, "bday \\par 1"],
	      ['bday.pol', 1, "bday \\par 1+2"],
	      ['bday.pol', 2, "bday \\par 1+2+special"],
	      ['bday_large.pol', 0, "bday large \\par 1"],
	      ['bday_large.pol', 1, "bday large \\par 1+2"],
	      ['bday_large.pol', 2, "bday large \\par 1+2+special"],
	      ['pizza.pol', 0, "pizza"],
	      ['photo3.pol', 0, "photo"],
	      ['travel.pol', 0, "travel"]];

  my $precs = [1 .. 9, 10, 15, 20, 25, 30, 35, 40, 0];

  print "\\begin{tabular}{||p{1.35cm}|";
  print "p{0.50cm}" x scalar(@$precs);
  print "c||}\n";

  print "\\hline \\hline " .
    "{\\small time [s]} " .
      "\\par {\\scriptsize\\parbox{2cm}{SIQR (outliers)}}" .
	"\\par {\\scriptsize max belief}" .
#	  "\\par{\\scriptsize latte time \\%}" .
#	    "\\par{\\scriptsize -count \\%}" .
#	      "\\par{\\scriptsize -maximize \\%}" .
#		"\\par{\\scriptsize max conss}" .
#		  "\\par{\\scriptsize total conss} " .
		    "& \\multicolumn{" . (scalar(@$precs) + 1). "}{c||}{\\textbf{prob-poly set size bound}} \\\\\n";
  print "\\hline \\hline \\textbf{query} & " . join(" & ", map {"\\textbf{" . ($_ ? $_ : "\$ \\infty \$") . "}"} @$precs) . " & \\\\\n";

  my $tasks_creation = {};

  my $last_file = "";

  pols_loop: foreach my $pol (@$pols) {
    my $pol_file = "../examples/bench/$pol->[0]";
    my $pol_query = $pol->[1];

    my $full = {'policy' => $pol_file,
		'query_num' => $pol_query,
		'policy_num' => 0};

    my $precs_data = gen::gen_prec_data($full);

    #print Dumper($precs_data);

    if ((! $precs_data) && (! $tasks_creation->{$pol_file})) {
      print STDERR "perl create_tasks.pl $pol_file 0 40 1\n";
      $tasks_creation->{$pol_file} = 1;
    }

    print "\\hline ";
    if ($last_file ne $pol_file) {
      print "\\hline ";
      $last_file = $pol_file;
    }
    print "$pol->[2] & ";

    print join(" & ",(map
	       {
		 my $prec = $_;

		 if ($precs_data) {
		   my $pol_cols = $precs_data->{'header'}->find_cols("policy");
		   my $pol_col = $pol_cols->[0];

		   my $data = $precs_data->get_lines_at_key($prec);
		   #print STDERR Dumper($data);
		   if ((defined $data) && (scalar(@$data))) {
		     my $prob = $data->[0]->get_data_at_key($pol_col);

		     my $benches = [map {$_->get_data_at_key('_epoch total (real s)')} @$data];
		     my $benches_latte_count = [map {100 * ($_->get_data_at_key('latte count total (real s)')) / $_->get_data_at_key('_epoch total (real s)')} @$data];
		     my $benches_latte_max = [map {100 * ($_->get_data_at_key('latte maximize total (real s)')) / $_->get_data_at_key('_epoch total (real s)')} @$data];
		     my $benches_latte = [map {100 * ($_->get_data_at_key('latte count total (real s)') + $_->get_data_at_key('latte maximize total (real s)')) / $_->get_data_at_key('_epoch total (real s)')} @$data];
		     my $benches_latte_cons_total = [map {$_->get_data_at_key('latte constraints total')} @$data];
		     my $benches_latte_cons_max = [map {$_->get_data_at_key('latte constraints max')} @$data];

		     #my $bench = $data->get_data_at_key('_epoch total (real s)');
		     my $stats = gen::stats_of_list($benches);
		     my $stats_latte_count = gen::stats_of_list($benches_latte_count);
		     my $stats_latte_max = gen::stats_of_list($benches_latte_max);
		     my $stats_latte_cons_total = gen::stats_of_list($benches_latte_cons_total);
		     my $stats_latte_cons_max = gen::stats_of_list($benches_latte_cons_max);
		     my $stats_latte = gen::stats_of_list($benches_latte);

		     my $p = gen::render_float($prob);
		     my $avg = sprintf("%0.1f", $stats->{'median'});
		     my $med_latte_count = sprintf("%0.1f", $stats_latte_count->{'median'});
		     my $med_latte_max   = sprintf("%0.1f", $stats_latte_max->{'median'});
		     my $med_latte_cons_total = $stats_latte_cons_total->{'average'};
		     my $med_latte_cons_max   = $stats_latte_cons_max->{'average'};
		     my $med_latte = sprintf("%0.1f", $stats_latte->{'median'});
		     my $siqr      = sprintf("%0.1f", $stats->{'siqr'});
		     my $outliers = $stats->{'outliers'};
		     my $samples  = $stats->{'samples'};
		     #my $b = sprintf("%0.1f", $bench);

#		     "{\\small $avg}\\par{\\scriptsize\\parbox{1.0cm}{$siqr ($outliers)}}\\par{\\scriptsize\\parbox{1.0cm}{$p}} \\par{\\scriptsize \\parbox{1.0cm}{$med_latte}} \\par{\\scriptsize \\parbox{1.0cm}{$med_latte_count}} \\par{\\scriptsize \\parbox{1.0cm}{$med_latte_max}} \\par{\\scriptsize \\parbox{1.0cm}{$med_latte_cons_max}} \\par{\\scriptsize \\parbox{1.0cm}{ $med_latte_cons_total}}";
		     "{\\small $avg}\\par{\\scriptsize\\parbox{1.0cm}{$siqr ($outliers)}}\\par{\\scriptsize\\parbox{1.0cm}{$p}}";
		   } else {
		     "? \\par ?";
		   }
		 } else {
		   "? \\par ?";
		 }
	       } @$precs));
    print " & \\\\\n";
  }

  print "\\hline \\hline\n";
  print "\\end{tabular}\n";
}
