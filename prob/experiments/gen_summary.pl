use strict;
use Storable;
use Data::Dumper;

require 'tasks_config.pl';
require 'tasks_util.pl';

make_summary();

sub make_summary {
  my $reports = report_labels();

  my $rtypes = {};

  foreach my $report (@$reports) {
    my $l = $report->{'label'};
    $rtypes->{$l} = [] if ! defined $rtypes->{$l};
    push @{$rtypes->{$l}}, $report;
  }

  my $ret = <<TEXT
<html>
<head>
<style type="text/css">
 .pol_box {
  background: #dddddd;
  border: 1px solid black;
  padding: 10px;
  float: left;
  font-size: 10px;
  }

 .graph_box {
   padding: 10px;
   float: right;
   background: #dddddd;
  }

 .label_box {
  width: 95%;
  border-bottom: 2px solid black;
  padding: 10px;
  float: left;
  clear: both;
}

</style>
<body>
TEXT
;

  foreach my $rtype (keys %$rtypes) {
    my $reports = $rtypes->{$rtype};

    my $report = $reports->[0];

    $ret .= <<TEXT
<div class="label_box">
<b>$rtype</b><br/>
<div class="pol_box">
<pre>$report->{'conf'}->{'policy_data'}</pre>
</div>
TEXT
;

    foreach my $rep (@$reports) {
      print "have report $rep->{file}\n";
      $ret .= gen_report_summary($rep);
    }

    $ret .= <<TEXT
</div>
<br/><br/>
TEXT
;
  }

  $ret .= <<TEXT
</body>
</html>
TEXT
;

  my $file_summary = "$config::DIR_REPORTS/summary.html";
  util::write_to_file($file_summary, $ret);
}

sub gen_report_summary {
  my ($rep) = @_;

  my $conf = $rep->{'conf'};
  my $pol_data = $conf->{'policy_data'};

  my $ret = <<TEXT
<div class="graph_box">
<center> $rep->{'label'} simplifier = <b>$rep->{'simplifier'}</b> query = <b>$rep->{'epoch'}</b> policy = <b>$rep->{'policy_num'}</b> </center>
<img src="$rep->{'file'}/graph.png"/><br/>
<center> <b> realtime </b> </center>
<img src="$rep->{'file'}/graph_realtime.png"/>
</div>
TEXT
;

  return $ret;
}

sub report_labels {
  opendir(my $dh, $config::DIR_REPORTS);

  my @files = readdir($dh);

  closedir($dh);

  my $ret = [];

  foreach my $file (@files) {
    if (my $l = is_report($file)) {
      #print "is label with $l->{precision}\n";
      push @$ret, $l;
    }
  }

  return $ret;
}

sub is_report {
  my ($file) = @_;
  if ($file =~ m/^(.*?)-(.*?)-(.*?)-(.*?)$/) {
    return {'label' => $1,
	    'epoch' => $3,
	    'simplifier' => $2,
	    'policy_num' => $4,
	    'file' => $file,
	    'conf' => retrieve "$config::DIR_REPORTS/$file/conf.storable",
	   };
  } else {
    return 0;
  }
}
