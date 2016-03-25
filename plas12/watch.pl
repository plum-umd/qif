my ($do, @files) = @ARGV;

printf "will do [$do] when changes detected in " . join(", ", @files) . "\n";

my $stats = {};

while (1) {
  my $has_changed = 0;

  for my $file (@files) {
    my $mtime = (stat($file))[9];
    if ($stats->{$file} != $mtime) { $has_changed = 1; }
    $stats->{$file} = $mtime;
  }

  if ($has_changed) {
    print "running [$do] ... ";
    my $out = `$do`;
    print "done\n";
    $has_changed = 0;
  }

  sleep(1);
}
