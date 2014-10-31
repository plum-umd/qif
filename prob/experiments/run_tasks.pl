use Storable;
use strict;

require 'tasks_config.pl';
require 'tasks_prob.pl';
require 'tasks_ipc.pl';

my ($workers) = @ARGV;
$workers = $config::CONCURRENT if ! defined $workers;

my $tasks;
my $tasks_active = {};

sub load_tasks {
  if (-e $config::FILE_TASKS) {
    $tasks = retrieve($config::FILE_TASKS);
    print "retrieved " . (scalar(keys %$tasks)) . " task(s)\n";
  } else {
    $tasks = {};
  }
}

sub task_get {
  my @ks = keys %$tasks;

  if (scalar(@ks) == 0) {
    return undef;
  }

  my $k = shift(@ks);
  my $task = $tasks->{$k};
  $tasks_active->{$k} = $task;
  delete $tasks->{$k};

  return $task;
}

ipc::make_call_parent("task_get", "master::task_get");

sub task_done {
  my ($task) = @_;

  my $k = $task->{'key'};

  ipc::print_child("task $task->{'key'} done");

  delete $tasks_active->{$k};

  tasks_remember();
}

ipc::make_call_parent("task_done", "master::task_done");

sub task_failed {
  my ($task) = @_;

  $task->{'failed_count'} += 1;

  ipc::print_child("*** task $task->{'key'} failed $task->{'failed_count'} time(s)");

  if ($task->{'failed_count'} > 10) {
    ipc::print_child("*** task $task->{'key'} failed too many times, stopping everything");
    ipc::kill_children();
    exit(1);
  } else {
    ipc::print_child("*** will try task $task->{'key'} again");
  }


  $tasks->{$task->{'key'}} = $task;
  tasks_remember();
}

ipc::make_call_parent("task_failed", "master::task_failed");

sub task_timeout {
  my ($task) = @_;
  ipc::print_child("*** task $task->{key} timed out");
  #task_done($task);
}

ipc::make_call_parent("task_timeout", "master::task_timeout");

sub clone {
  # http://www.perlmonks.org/index.pl?node_id=883319
  my $ref = shift;
  my $type = ref $ref;
  if( $type eq 'HASH' ) {
    return { map clone( $_ ), %{ $ref } };
  }
  elsif( $type eq 'ARRAY' ) {
    return [ map clone( $_ ),@{ $ref } ];
  }
  elsif( $type eq 'REF' ) {
    return \ clone( $$ref );
  }
  else {
    return $ref;
  }
}

sub tasks_remember {
  # note, storable doesn't like threads::shared
  lock ($tasks);
  lock ($tasks_active);
  my $tasks_temp = {};

  for my $k (keys %$tasks) {
    $tasks_temp->{$k} = clone($tasks->{$k});
  }
  for my $k (keys %$tasks_active) {
    $tasks_temp->{$k} = clone($tasks_active->{$k});
  }
  `cp $config::FILE_TASKS $config::FILE_TASKS.backup`;
  store $tasks_temp, $config::FILE_TASKS;

  ipc::print_child("remembered " . (scalar(keys %$tasks_temp)) . " task(s)");
}

ipc::make_call_parent("tasks_remember", "master::tasks_remember");

sub tasks_hastask {
  return (scalar(keys %$tasks) > 0);
}

ipc::make_call_parent("tasks_hastask", "master::tasks_hastask");

sub task_do {
  my ($task) = @_;
  ipc::print_child("doing task $task->{key}");

  my $k = $task->{'key'};

  my $dir_task;

  local $SIG{INT} = sub {
    ipc::print_child("task interrupted");
    if ($dir_task) {
      `rm -Rf $dir_task`;
    }
    exit(1);
  };

  $dir_task = util::new_dir("task", $config::DIR_TASKS);

  my $temp = task_prob($dir_task, $task);

  $task->{'dir_task'} = $dir_task;

  store $task, "$dir_task/task.storable";

  if (! $temp) {
    `rm -Rf $dir_task`;
  }

  return $temp;
}

sub start_child {
  undef $SIG{INT};
  while (my $task = master::task_get()) {
    if ($task) {
      if (! task_do($task)) {
	master::task_failed($task);
      } else {
	master::task_done($task);
      }
      sleep(5); # make sure the system calms down a bit before going to next task
    }
  }
}

sub start_children {
  ipc::create_children($workers, \&start_child);
}

sub tasks_add {
  my ($task) = @_;
  lock ($tasks);
  $tasks->{$task->{'key'}} = $task;
}

sub tasks_add_from_file {
  my ($filename) = @_;
  if (not -r $filename) {
    return 0;
  }

  my $temp = retrieve $filename;

  my $num_tasks = scalar(keys %$temp);

  if ($num_tasks == 0) {
    return 0;
  }

  printf "adding $num_tasks additional tasks from $filename\n";

  for my $k (keys %$temp) {
    tasks_add($temp->{$k});
  }

  store {}, $filename;

  tasks_remember();

  return 1;
}

$SIG{INT} = sub {
  ipc::print_child("interrupted");
  ipc::kill_children();
  tasks_remember();
  exit(0);
};

$SIG{ALRM} = sub {
  ipc::print_child("alarm");
  exit(0);
};

load_tasks();

do {
  start_children();
} while (tasks_add_from_file($config::FILE_TASKS_ADD));
