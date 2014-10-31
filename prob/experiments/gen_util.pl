use strict;
use POSIX;
use Data::Dumper;

require 'tasks_config.pl';
require 'tasks_util.pl';

package gen;

use Storable;

sub gen_latte_data {
  my ($type, $dims) = @_;

  my $tasks = tasks_all();

  my $ret = undef;

  foreach my $task (@$tasks) {
    if (defined $task->{'file_latte'}) {
      my $temp = csv::new_from_file("$task->{'dir_task'}/$task->{'file_latte'}", "constraints", ",");
      if (defined $type) {
	$temp->filter(sub {
			my ($line) = @_;
			return ($line->get_data_at_key('type') eq $type);
		      });
      }
      if (defined $dims) {
	$temp->filter(sub {
			my ($line) = @_;
			return ($line->get_data_at_key('dimensions') eq $dims);
		      });
      }
      if (not defined $ret) {
	$ret = $temp;
      } else {
	$ret->add_csv($temp);
      }
    }
  }

  return $ret;
}

sub gen_prec_data {
  my ($full) = @_;

  my $tasks = task_label_files($full);

  my $lines = {};

  my $dat = csv::new("precision");

  my $best_header = undef;

  use Data::Dumper;
  #print Dumper($full);

  foreach my $task (@$tasks) {

    #print Dumper($task);

    my $file = $task->{'dir_task'};
    my $prec = $task->{'precision'};

    #my $qlines = get_query_lines($file, $full->{'query_num'});
    my $qdata = get_query_data($file);
    $qdata->{'header'}->add_col("precision");
    my $qlines = $qdata->get_lines_at_key($full->{'query_num'});
    if (! defined $qlines) {
      die ("query number $full->{'query_num'} is not present");
    }

    if (scalar(@$qlines)) {
      foreach my $qline (@$qlines) {
	$qline->{'header'}->{'keyer'} = 'precision';
	$qline->set_data_at_key("precision", $prec);

	#print Dumper($qline);

	if ((! $best_header) || (scalar @{$best_header->{'head'}} < scalar @{$qline->{'header'}->{'head'}})) {
	  $best_header = $qline->{'header'};
	  $dat->set_header($best_header);
	}

	$dat->add_line($qline);
      }
    } else {
      print STDERR "*** warning: no data for precision $prec was found\n"
    }
  }

  if (! defined $dat->{'header'}) {
    return undef;
  }


  return $dat;
}

sub run_gnuplot {
  my ($target) = @_;

  my $cmd = "gnuplot $target";

  print "running [$cmd]\n";

  `$cmd`;
}

sub make_report_dir {
  my $dir = util::new_dir("report", $config::DIR_REPORTS);

  return $dir;
}

sub conf_sample {
  my ($full) = @_;

  my $files = task_label_files($full);

  if (scalar(@$files) > 0) {
    return (retrieve "$files->[0]->{'dir_task'}/task.storable");
  } else {
    die "no config samples present";
  }
}

my $tasks = undef;

sub _read_tasks {
  $tasks = read_tasks($config::DIR_TASKS) if not defined $tasks;
}

sub tasks_all {
 _read_tasks();
 return $tasks;
}

sub task_label_files {
  my ($full) = @_;

  _read_tasks();

  my $ret = [];

  #use Data::Dumper;
  #print Dumper($tasks);

  foreach my $task (@$tasks) {
    if (my $l = is_task_label($full, $task)) {
      #print "is label with $l->{precision}\n";
      push @$ret, $task;
    }
  }

  return $ret;
}

sub equal_on_first_hashes {
  my ($h1, $h2) = @_;
  foreach my $k (keys %$h1) {
    return 0 if $h1->{$k} ne $h2->{$k};
  }

  return 1;
}

sub is_task_label {
  my ($full, $task) = @_;
  return equal_on_first_hashes ({'policy'   => $full->{'policy'},
				 'domain'   => $full->{'domain'},
				 'simplify' => $full->{'simplify'},
				},
				$task);
}

sub read_tasks {
  my ($dir) = @_;

  opendir(my $dh, $dir);

  my $ret = [];

  foreach my $file (readdir($dh)) {
    if ($file =~ m/^task_.*?$/) {
      my $taskfile = "$dir/$file/task.storable";
      if (-r $taskfile) {
	my $task = retrieve $taskfile;
	push @$ret, $task;
      } else {
	print STDERR "!!! bad task $file\n";
      }
    }
  }

  closedir($dh);

  return $ret;
}

sub get_query_data {
  my ($dir_task) = @_;

  my $data = csv::new_from_file("$dir_task/timing.csv",
				"_epoch #",
				",");

  return $data;
}

sub get_query_lines {
  my ($dir_task, $query_num) = @_;

  my $data = csv::new_from_file("$dir_task/timing.csv",
				"_epoch #",
				",");

  return $data->get_lines_at_key($query_num);
}

sub stats_of_list {
  my ($l) = @_;

  my $l = [grep {defined $_} @$l];

  my $l = [sort {$a <=> $b} @$l];

  my $avg = avg_of_list($l);
  my $median = quart_of_sorted_list($l, 0.5);

  my $q_lower = quart_of_sorted_list($l, 0.25);
  my $q_upper = quart_of_sorted_list($l, 0.75);

  my $siqr = 0.5 * ($q_upper - $q_lower);

  my $outliers = 0;

  my $min = $l->[0];
  my $max = $l->[0];

  foreach my $item (@$l) {
    $min = $item if $item < $min;
    $max = $item if $item > $max;
    if (($item < $q_lower - 3 * $siqr) ||
	($item > $q_upper + 3 * $siqr)) {
      $outliers += 1;
    }
  }

  return {'average' => $avg,
	  'median' => $median,
	  'siqr' => $siqr,
	  'q_lower' => $q_lower,
	  'q_upper' => $q_upper,
	  'min'     => $min,
	  'max'     => $max,
	  'outliers' => $outliers,
	  'samples' => scalar(@$l),
	 };
}

sub quart_of_sorted_list {
  my ($l, $q) = @_;
  my $n = scalar(@$l);

  my $index = ($n-1)*$q;

  if ($index == int($index)) {
    return $l->[$index];
  } else {
    return (0.5 * ($l->[POSIX::floor($index)] + $l->[POSIX::ceil($index)]));
  }
}

sub avg_of_list {
  my ($l) = @_;
  my $s = 0;
  foreach my $i (@$l) {
    $s += $i;
  }
  if (scalar(@$l)) {
    return ($s / scalar(@$l));
  } else {
    return 0;
  }
}

sub summary_data_of_data {
  my ($data, $summary_cols) = @_;

  my $head = $data->{'header'};
  my $keyer = $head->{'keyer'};
  if (not defined $keyer) {
    die "cannot produce summary statistics without keyed data";
  }

  my $stats = ["average", "median", "siqr", "q_lower", "q_upper", "min", "max"];

  my @ks = sort {$a <=> $b} (keys %{$data->{'lines_keyed'}});

  my $ret = csv::new();
  my $new_head = csv_header::new();
  $new_head->add_col($keyer);
  $new_head->add_col("_samples");
  $new_head->{'keyer'} = $keyer;
  $ret->{'header'} = $new_head;

  foreach my $col (@$summary_cols) {
    foreach my $stat (@$stats) {
      $new_head->add_col("_$stat $col");
    }
  }

  foreach my $k (@ks) {
    my $lines = $data->get_lines_at_key($k);

    my $new_line = csv_line::new();
    $new_line->{'header'} = $new_head;

    $new_line->set_data_at_key($keyer, $k);

    foreach my $col (@$summary_cols) {
      my $col_data = [map {$_->get_data_at_key($col)} @$lines];
      my $data_stats = gen::stats_of_list($col_data);

      $new_line->set_data_at_key("_samples", scalar(@$lines));

      foreach my $stat (@$stats) {
	$new_line->set_data_at_key("_$stat $col", $data_stats->{$stat});
      }
    }

    $ret->add_line($new_line);
  }

  return $ret;
}

sub render_float_noexp {
  my ($f) = @_;

  my $format = "%0.3f";

  if (abs($f) < 1) {
    $format = "%0.3f";
  } elsif (abs($f) < 10) {
    $format = "%0.3f";
  } elsif (abs($f) < 100) {
    $format = "%0.2f";
  } else {
    $format = "%0.1f";
  }

  return sprintf($format, $f);
}

sub render_float {
  my ($f) = @_;

  if ($f == 0) {
    return "0";
  }
  if ($f == 1) {
    return "1";
  }

  if ($f < 0.001) {
    my $temp = sprintf("%0.2e", $f);
    if ($temp =~ m/^(.+?)e-0(.)$/) {
      return "$1e-$2";
    } else {
      return $temp;
    }
  } else {
    return sprintf("%0.4f", $f);
  }
}

1;
