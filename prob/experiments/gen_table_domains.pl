use strict;
use Data::Dumper;

require 'tasks_config.pl';
require 'tasks_util.pl';
require 'gen_util.pl';

gen_table();

sub gen_table {
  my $pols = [['bday.pol', 0, "bday \\par 1", "bday 1"],
#	      ['bday.pol', 1, "bday \\par 1+2", "bday 1+2"],
	      ['bday.pol', 2, "bday \\par 1+2+special", "bday 1+2+special"],
	      ['bday_large.pol', 0, "bday large \\par 1", "bday large 1"],
#	      ['bday_large.pol', 1, "bday large \\par 1+2", "bday large 1+2"],
	      ['bday_large.pol', 2, "bday large \\par 1+2+special", "bday large 1+2+special"],
	      ['pizza.pol', 0, "pizza", "pizza"],
	      ['photo.pol', 0, "photo", "photo"],
	      ['travel.pol', 0, "travel", "travel"]];

  my $domains = ['box', 'octalatte', 'poly'];
  my $domain_names = {'box' => "\$\\mathbf{\\Box}\$",
		      "octalatte" => "\$\\Diamond\$",
		      "poly" => "\$\\pentagon\$"};

  my $precs = [1 .. 9, 10, 15, 20, 25, 30, 35, 40, 0];
#  my $precs = [1, 2, 4, 6, 8, 10, 15, 20, 30, 40, 0];

  print "\\begin{longtable}{|p{0.01cm}|";
  print "p{0.25cm}" x scalar(@$precs);
  print "|}\n";

#  print "{\\small \\caption{Query evaluation benchmarks\\label{fig:bench_table}}}\\\\\n";

#  print "\\hline \\hline "
#  print "\\hline "
#    . "{\\scriptsize time [s]} "
#      . "{\\scriptsize SIQR(out)} "
      #      "\\par {\\scriptsize\\parbox{2cm}{SIQR (outliers)}}" .
#      . "\\par {\\scriptsize belief}"
	#	  "\\par{\\scriptsize latte time \\%}" .
	#	    "\\par{\\scriptsize -count \\%}" .
	#	      "\\par{\\scriptsize -maximize \\%}" .
	#		"\\par{\\scriptsize max conss}" .
	#		  "\\par{\\scriptsize total conss} "
#	. "& \\multicolumn{" . (scalar(@$precs) + 1). "}{c|}{\\textbf{powerset size bound}} \\\\\n";
#    . "\\\\\n";

  print "\\hline \\textbf{} & " . join(" & ", map {"{\\tiny" . ($_ ? $_ : "\$ \\infty \$") . "}"} @$precs) . " \\\\\n";
  print "\\endfirsthead ";

  print "\\caption{Continued}\\\\\n";
  print "\\hline \\textbf{} & " . join(" & ", map {"{\\tiny" . ($_ ? $_ : "\$ \\infty \$") . "}"} @$precs) . " \\\\\n";
  print "\\hline \\endhead\n";

  my $tasks_creation = {};

  my $last_file = "";

 pols_loop: foreach my $pol (@$pols) {
#    print "\\hline\\hline {\\scriptsize \\textbf{domain}} & \\multicolumn{" . (scalar(@$precs) + 1). "}{c||}{\\small \\textbf{$pol->[3]}} \\\\\n";
    print "\\hline\\hline \\multicolumn{" . (scalar(@$precs) + 1). "}{|c|}{\\tiny{$pol->[3]}} \\\\\n";
#    print "\\nopagebreak \\hline ";

  doms_loop: foreach my $dom (@$domains) {
      my $pol_file = "../examples/bench/$pol->[0]";
      my $pol_query = $pol->[1];

      my $full = {'policy' => $pol_file,
		  'query_num' => $pol_query,
		  'policy_num' => 0,
		  'domain' => $dom,
		  'simplify' => 'halfs',
		 };

      my $precs_data = gen::gen_prec_data($full);

      #print Dumper($precs_data);

      if ((! $precs_data) && (! $tasks_creation->{$pol_file})) {
	print STDERR "perl create_tasks.pl $pol_file 0 9 1 1 $dom simple\n";
	print STDERR "perl create_tasks.pl $pol_file 10 40 5 1 $dom simple\n";
	$tasks_creation->{$pol_file} = 1;
      }

      print "\\nopagebreak \\nobreakhline ";

#      if ($last_file ne $pol_file) {
#	print "\\hline ";
#	$last_file = $pol_file;
#      }

      print "{\\par{\\scriptsize \\parbox{0.5cm}{}\\parbox{0.5cm}{\\hspace{-0.09cm}$domain_names->{$dom}}}} & ";

      print join(" & ",
		 (map
		    {
		      my $prec = $_;

		      if ($precs_data) {
			my $pol_cols = $precs_data->{'header'}->find_cols("policy");
			my $pol_col = $pol_cols->[0];

			my $data = $precs_data->get_lines_at_key($prec);
			#print STDERR Dumper($data);
			if ((defined $data) && (scalar(@$data))) {
			  my $probs = [map {$_->get_data_at_key($pol_col)} @$data];
			  my $stats_prob = gen::stats_of_list($probs);

			  my $prob = $stats_prob->{'average'};
			  my $prob_outliers = $stats_prob->{'outliers'};
			  my $prob_siqr = sprintf("%0.1f", $stats_prob->{'siqr'});

			  my $benches = [map {$_->get_data_at_key('_epoch total (real s)')} @$data];
			  my $benches_latte_max = [map {100 * ($_->get_data_at_key('latte maximize total (real s)')) / $_->get_data_at_key('_epoch total (real s)')} @$data];
			  my $benches_latte_cons_max = [map {$_->get_data_at_key('latte constraints max')} @$data];

			  my $stats = gen::stats_of_list($benches);
			  my $stats_latte_max = gen::stats_of_list($benches_latte_max);
			  my $stats_latte_cons_max = gen::stats_of_list($benches_latte_cons_max);

			  my $p = gen::render_float($prob);
			  #my $avg = sprintf("%0.3f", $stats->{'median'});
			  my $avg = gen::render_float_noexp($stats->{'median'});

			  my $med_latte_max   = sprintf("%0.1f", $stats_latte_max->{'median'});
			  my $med_latte_cons_max   = $stats_latte_cons_max->{'average'};

			  #my $siqr      = sprintf("%0.3f", $stats->{'siqr'});
			  my $siqr = gen::render_float_noexp($stats->{'siqr'});
			  my $outliers = $stats->{'outliers'};
			  my $samples  = $stats->{'samples'};
			  #my $b = sprintf("%0.1f", $bench);

			  #"{\\small $avg ($outliers)}\\par{\\scriptsize\\parbox{1.0cm}{$p}}";
			  "\\par{\\tiny \\parbox{1cm}{\\hspace{-0.17cm}$avg}}" .
			    "\\par{\\tiny \\parbox{1cm}{\\hspace{-0.17cm}$siqr($outliers)}}".
			      "\\par{\\tiny \\parbox{1cm}{\\hspace{-0.17cm}$p}}";
			} else {
			  "? \\par ?";
			}
		      } else {
			"? \\par ?";
		      }
		    } @$precs));
      #print " & \\\\\n";
      print "\\\\\n";
      }
    }

  print "\\hline\n";
  print "\\end{longtable}\n";
}
