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

definition "var_names s = {(fst v) | v. v \<in> s}"

definition "extract_subvars s vs = {v | v. v \<in> s \<and> (fst v) \<in> vs }"

definition "restore_subvars s vs = s - {v | v. v \<in> s \<and> (fst v) \<in> var_names vs } \<union> vs"

definition "local_lens l s lv \<equiv>
  ((Get l s) \<subseteq> s
  \<and> (Put l) lv s = restore_subvars s lv)"

definition "well_behaved l \<equiv> (\<forall>ls lv.
  local_lens l ls lv)"

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
  unfolding simulates_def local_simulates_def compose_local_actions_def well_behaved_def
    local_lens_def restore_subvars_def var_names_def
  by metis

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

lemma ex_sub: "extract_subvars s v \<subseteq> s"
  by (simp add: extract_subvars_def)

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
  shows mod_loc_lens: "local_lens (lens (model_action_mapping e)) s lv"
  unfolding local_lens_def
  apply(rule conjI)
   apply(simp add: get_subset_mod)
  apply(simp add: put_restores_mod)
  done

lemma 
  shows impl_loc_lens: "local_lens (lens (impl_action_mapping e)) s lv"
  unfolding local_lens_def
  apply(rule conjI)
   apply(simp add: get_subset_impl)
  apply(simp add: put_restores_impl)
  done


lemma mod_well_b: "well_behaved (lens (model_action_mapping e))"
  apply (unfold well_behaved_def)
  apply (simp add: mod_loc_lens)
  done

lemma impl_well_b: "well_behaved (lens (impl_action_mapping e))"
  apply (unfold well_behaved_def)
  apply (simp add: impl_loc_lens)
  done

lemma loc_sim: "local_simulates impl_action_mapping model_action_mapping e s"
  by (simp add: impl_action_mapping_def local_simulates_def model_action_mapping_def)

theorem "simulates (compose_local_actions impl_action_mapping)
  (compose_local_actions model_action_mapping) e s t"
  apply (rule local_sim_imp_sim)
    apply(simp add: impl_well_b)
   apply(simp add: mod_well_b)
  apply(simp add: loc_sim)
  done

end