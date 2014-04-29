let num_cores = Util.cpu_count ();;
let list_cores = Util.list_range 0 num_cores;;
let initial_workload = 100 * num_cores;;
let shared_pool_size = 1024 * 1024 * 1024 * num_cores;;
let shared_box_msg_size = 1024 * 1024;;
let shared_box_size = Netsys_posix.sem_value_max;;
