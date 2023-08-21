theory LocalActions

imports Main

begin

text "Each action operates on its own local state, which is defined as a 'v view type. The relationship
     between 'v and 's is definable by a lens. This is different than the seL4 process model,
      since that effectively only has a single lens whereas multiple lenses are required for
      each action here."

type_synonym ('n, 's) state = "('n * 's) set"

record ('n, 's) lens  = 
  Get :: "('n, 's) state \<Rightarrow> ('n, 's) state"
  Put :: "('n, 's) state \<Rightarrow> ('n, 's) state \<Rightarrow> ('n, 's) state"

text "Well-behaved lenses must follow the 'lens laws' which ensure that they retrieve and update
      the same part of the source state."

definition "local_lens l s lv \<equiv>
  ((Get l s) \<subseteq> s
  \<and> (Put l) lv s = s - (Get l s) \<union> lv)"

definition "well_behaved l \<equiv> (\<forall>ls lv lvv.
  local_lens l ls lv \<and> 
  (Put l) ((Get l) ls) ls = ls \<and> 
  (Get l) ((Put l) lv ls) = lv \<and> 
  (Put l) lvv ((Put l) lv ls) = (Put l) lvv ls)"

text "A process is a representation of a program that proceeds through a sequence of states in 
      response to actions (inputs) of type 'e, i.e. a state machine."
type_synonym ('e, 'n, 's) process = "'e \<Rightarrow> ('n, 's) state \<Rightarrow> ('n, 's) state"

text "A local action is a step function on its own local state type 'a, where 'a is defineable as a lens
      on some global state type 's"

record ('n, 's) local_action =
  lens :: "('n, 's) lens"
  step :: "('n, 's) state \<Rightarrow> ('n, 's) state"

text "An action mapping maps an action 'e to its corresponding local action"

type_synonym ('e, 'n, 's) action_mapping = "'e \<Rightarrow> ('n, 's) local_action"

text "compose_local_actions defines how a global process can be defined from a set of local actions.
      It uses the lens defined in the local action for each action type to get the local action state
      from the global state, execute the local step function, and write the result back into
      the global state."

definition compose_local_actions :: "('e, 'n, 's) action_mapping \<Rightarrow> ('e, 'n, 's) process"  where
"compose_local_actions am = (\<lambda>e s.(
  let locact = am e;
  lns = (lens locact);
  locact_s = (Get lns) s;
  res = (step locact) locact_s in
  
  (Put lns) res s
))"

text "'Local simulation' is where each local action simulates its corresponding
    action in the model."

definition "local_simulates am_i am_m e s = (\<forall>v.
  let proc_i = step (am_i e) in
  let proc_m = step (am_m e) in
  let impl_start = (Get (lens (am_i e))) s in
  let model_start = (Get (lens (am_m e))) s in
  
  proc_i impl_start = v \<longrightarrow> proc_m model_start = v)"

text "Basic notion of simulation."

definition "simulates impl_proc model_proc e s t = (impl_proc e s = t \<longrightarrow> model_proc e s = t)"

text "We want to show that in order to prove that an implementation process simulates another 
      process, we can instead decompose both processes each into a set of local actions. If the
      local actions simulate each other, and the lenses used to define the local action states are
      well-behaved, then the global implementation process simulates the global model."

theorem local_sim_imp_sim:
  assumes "well_behaved (lens (am_i e))"
  and "well_behaved (lens (am_m e))"
  and "local_simulates am_i am_m e s"
  shows "simulates (compose_local_actions am_i) (compose_local_actions am_m) e s t"
  using assms
  unfolding simulates_def local_simulates_def compose_local_actions_def well_behaved_def local_lens_def Let_def
  by (metis sup.idem sup_bot.right_neutral)

type_synonym ('s) invariant_func = "'s \<Rightarrow> bool"

definition "local_invariant am inv_f e s = (
  let step_f = step (am e) in
  let local_state = Get (lens (am e)) s in
  let put = Put (lens (am e)) in

   inv_f s \<and> inv_f (put (step_f local_state) s)
)"

text "Local invariance implies global invariance.

     Note how well-behavedness of the lens isn't a required assumption, because all that matters
     is that the invariant holds before and after the action completes."

text "An improvement would be to also define a selector (just the read part of a lens) on the global
      state so that the invariant only needs to operate on the data that it cares about."

theorem local_inv_imp_inv:
  assumes "local_invariant am_i inv_f e s"
  shows "inv_f ((compose_local_actions am_i) e s)"
  using assms
  unfolding local_invariant_def compose_local_actions_def
  by metis

end