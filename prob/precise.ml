type complexity = 
    {
      max_z: float;   (* max complexity of any integer in use *)
      max_q: float;   (* any rational in use *)
      max_num: float; (* any number in use *)
      max_elements: float; (* max number of pstatesets of an abstraction *)
      max_element: float;  (* max complexity of any pstateset in abstraction *)
      total_z: float;   (* total complexity of all integers *)
      total_q: float;   (* of all rationals *)
      total_num: float; (* of all numbers *)
      total_elements: float; (* total number of pstatesets of an abstraction *)
      total_element: float   (* total complexity of all pstatesets *)
    }

type subprecise =
  | Full
  | Bounded of float

type proj_goodness = float
    
type proj_method =
  | AllPancake (* project onto removed dimensions, find size,
		  all removed dimensions are removed at the same time *)
  | OneLP (* dimensions removed one at a time, width determined using LP *)

type plus_goodness = float

type precise =
    {
      complexity: complexity;
      num_complexity: subprecise;
      elements: subprecise;
      element_complexity: subprecise;
      proj: proj_method;
    }
