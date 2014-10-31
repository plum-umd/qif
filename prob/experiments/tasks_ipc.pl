use strict;
use Storable;
use IO::Select;
use IO::Handle;

package ipc;

my $child_read;
my $child_write;

sub make_call_parent {
  # http://www252.pair.com/comdog/mastering_perl/Chapters/09.dynamic_subroutines.html

  my ($fun_name, $fun_name_new) = @_;

  no strict 'refs';

  *{ $fun_name_new } = sub {
    return call_parent($fun_name, @_);
  };
}

sub time_string {
  return localtime(time());
}

sub print_child {
  my ($s) = @_;
  my $t = time_string();
  print "[$t \@ $$] $s\n";
}

sub start_child {
  my ($do) = @_;
  print_child("worker started");

  $do->();

  print_child("worker finished");
}

sub call_parent {
  my ($fun_name, @args) = @_;

  my $send = {'fun' => $fun_name,
	      'args' => [@args],
	     };

  Storable::store_fd($send, $child_write);

  my $temp = Storable::fd_retrieve($child_read);

  return $temp->{'ret'};
}

sub receive_child {
  my ($parent_read, $parent_write) = @_;

  my $temp = Storable::fd_retrieve($parent_read);

  my $fun_name = $temp->{'fun'};
  my $fun_args = $temp->{'args'};

  no strict 'refs';

  my $fun_ret = (*{ "main::$fun_name" })->(@$fun_args);

  my $ret = {'ret' => $fun_ret};

  Storable::store_fd($ret, $parent_write);
}

sub fork_off {
  my ($fun) = @_;

  my $temp_child_read = IO::Handle->new();
  my $temp_child_write = IO::Handle->new();
  my $parent_read = IO::Handle->new();
  my $parent_write = IO::Handle->new();

  $temp_child_write->autoflush(1);
  $parent_write->autoflush(1);

  pipe $parent_read, $temp_child_write;
  pipe $temp_child_read, $parent_write;

  my $pid = fork();

  if ($pid) {
  } else {
    $child_read = $temp_child_read;
    $child_write = $temp_child_write;

    start_child($fun);
    exit();
  }
  return ($pid, $parent_read, $parent_write);
}

my $childs = {};
my $pids = {};

sub create_children {
  my ($children, $do) = @_;

  local $| = 1;

  my $s = IO::Select->new();

  my $writers = {};

  for (my $i = 0; $i < $children; $i++) {
    my ($pid, $reader, $writer) = fork_off($do);
    $s->add($reader);
    $writers->{$reader} = $writer;
    $childs->{$pid} = 1;
    $pids->{$reader} = $pid;
  }

  while ($s->count() > 0) {
    my @ready = $s->can_read(1);
    my $t = time_string();
    print "\r[$t @ $$]\r";
    for my $reader (@ready) {
      my $writer = $writers->{$reader};
      if ($reader->eof()) {
	$s->remove($reader);
	delete $childs->{$pids->{$reader}};
	delete $pids->{$reader};
      } else {
	receive_child($reader, $writer);
      }
    }
  }

  print_child("no more workers");
}

sub kill_children {
  foreach my $pid (keys %{$childs}) {
    print_child("killing $pid");
    kill 2, $pid;
    waitpid $pid, 0;
    delete $childs->{$pid};
  }
}

1;

