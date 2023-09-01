theory LocalActions

imports Main

begin

text "A state is a set of variable names each with an associated value."

type_synonym ('n, 's) state = "('n * 's) set"

text "A process is a representation of a program that proceeds through a sequence of states in 
      response to actions (inputs) of type 'e, i.e. a state machine."

type_synonym ('e, 'n, 's) process = "'e \<Rightarrow> ('n, 's) state \<Rightarrow> ('n, 's) state"

text "Lenses map between two states"

record ('n, 's) lens  = 
  Get :: "('n, 's) state \<Rightarrow> ('n, 's) state"
  Put :: "('n, 's) state \<Rightarrow> ('n, 's) state \<Rightarrow> ('n, 's) state"

text "Proper local lenses extract a subset of the global state, and restore that exact subset once
      processing is complete."

definition "var_names s = {(fst v) | v. v \<in> s}"

definition "restore_subvars s vs = s - {v | v. v \<in> s \<and> (fst v) \<in> var_names vs } \<union> vs"

definition "local_lens l s \<equiv> (\<forall>lv.
  (Get l s) \<subseteq> s
  \<and> (Put l) lv s = restore_subvars s lv)"

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

definition "local_simulates af am_i am_m e i = (\<forall>v.
  let proc_i = step (am_i e) in
  let proc_m = step (am_m e) in
  let impl_start = (Get (lens (am_i e))) i in
  let model_start = (Get (lens (am_m e))) (af i) in
  
  proc_i impl_start = v \<longrightarrow> proc_m model_start = (af v) )"

text "Basic notion of simulation."

definition "simulates af impl_proc model_proc e i t =
  (impl_proc e i = t \<longrightarrow>  model_proc e (af i) = (af t))"

text "The value that compose_local_actions e s evaluates to is the value that the step function evals
      to with the lens applied"
lemma comp_act: "(compose_local_actions am) e s = Put (lens (am e)) (step (am e) (Get (lens (am e)) s))  s"
  unfolding compose_local_actions_def Let_def
  by simp

text "We want to show that in order to prove that an implementation process simulates another 
      process, we can instead decompose both processes each into a set of local actions. If the
      local actions simulate each other, and the lenses used to define the local action states are
      well-behaved, then the global implementation process simulates the global model."

theorem local_sim_imp_sim:
  assumes "local_lens (lens (am_i e)) i"
  and "local_lens (lens (am_m e)) (af i)"


  and "var_names i = var_names (step (am_i e) i)"
  and "var_names (af i) = var_names (step (am_m e) (af i))"

  and "local_simulates af am_i am_m e i"
  shows "simulates af (compose_local_actions am_i) (compose_local_actions am_m) e i t"
  using assms
  unfolding simulates_def local_simulates_def compose_local_actions_def
    local_lens_def restore_subvars_def var_names_def Let_def
  nitpick



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

section "Example"

datatype PointEvt = TranslateX int | TranslateY int

(* f is an update function *)
definition "write s n f =
  s 
    - {var | var. (fst var) = n \<and> var \<in> s}
    \<union> {(n, f (snd var)) | var. (fst var) = n \<and> var \<in> s}"

definition "extract_subvars s vs = {v | v. v \<in> s \<and> (fst v) \<in> vs }"

definition "model_action_mapping e = (case e of
  TranslateX n \<Rightarrow> 
    let lens = \<lparr> 
      Get=(\<lambda>s. extract_subvars s {''x''}),
      Put=(\<lambda>v s. restore_subvars s v) 
    \<rparr> in  
    let step = (\<lambda>s. write s ''x'' (\<lambda>v. v + n)) in

    \<lparr> lens=lens, step=step \<rparr>
  | TranslateY n \<Rightarrow>
     let lens = \<lparr> 
      Get=(\<lambda>s. extract_subvars s {''y''}),
      Put=(\<lambda>v s. restore_subvars s v) 
    \<rparr> in  
    let step = (\<lambda>s. write s ''y'' (\<lambda>v. v + n)) in

    \<lparr> lens=lens, step=step \<rparr>)"

definition "impl_action_mapping e = (case e of
  TranslateX n \<Rightarrow> 
    let lens = \<lparr> 
      Get=(\<lambda>s. extract_subvars s {''x''}),
      Put=(\<lambda>v s. restore_subvars s v) 
    \<rparr> in  
    let step = (\<lambda>s. write s ''x'' (\<lambda>v. v + n)) in

    \<lparr> lens=lens, step=step \<rparr>
  | TranslateY n \<Rightarrow>
     let lens = \<lparr> 
      Get=(\<lambda>s. extract_subvars s {''y''}),
      Put=(\<lambda>v s. restore_subvars s v) 
    \<rparr> in  
    let step = (\<lambda>s. write s ''y'' (\<lambda>v. v + n)) in

    \<lparr> lens=lens, step=step \<rparr>)"

lemma get_subset_mod: "Get (lens (model_action_mapping e)) s \<subseteq> s"
  unfolding model_action_mapping_def
  apply (case_tac e)
   apply(auto)
  using extract_subvars_def apply blast+
  done

lemma put_restores_mod:
  "Put (lens (model_action_mapping e)) lv s = restore_subvars s lv"
  unfolding model_action_mapping_def
  apply (case_tac e)
   apply(auto)
  done

lemma get_subset_impl: "Get (lens (impl_action_mapping e)) s \<subseteq> s"
  unfolding impl_action_mapping_def
  apply (case_tac e)
   apply(auto)
  using extract_subvars_def apply blast+
  done

lemma put_restores_impl:
  "Put (lens (impl_action_mapping e)) lv s = restore_subvars s lv"
  unfolding impl_action_mapping_def
  apply (case_tac e)
   apply(auto)
  done

lemma 
  shows mod_loc_lens: "local_lens (lens (model_action_mapping e)) s"
  unfolding local_lens_def
  apply(rule allI)
  apply(rule conjI)
   apply(simp add: get_subset_mod)
  apply(simp add: put_restores_mod)
  done

lemma 
  shows impl_loc_lens: "local_lens (lens (impl_action_mapping e)) s"
  unfolding local_lens_def
  apply(rule allI)
  apply(rule conjI)
   apply(simp add: get_subset_impl)
  apply(simp add: put_restores_impl)
  done

definition "abstraction_func s = s" 

lemma loc_sim: "local_simulates abstraction_func impl_action_mapping model_action_mapping e s"
  by (simp add: impl_action_mapping_def local_simulates_def model_action_mapping_def abstraction_func_def)

theorem "simulates abstraction_func (compose_local_actions impl_action_mapping)
  (compose_local_actions model_action_mapping) e s t"
  apply (rule local_sim_imp_sim)
    apply(simp add: impl_loc_lens)
   apply(simp add: mod_loc_lens)
  apply(simp add: loc_sim)
  done

end