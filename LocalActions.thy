theory LocalActions

imports Main

begin

(*

* A lens from global to local state for each proc
* A local step function for each proc
  * step \<subseteq>
* A way to build a global proc from the lens and step function



  O       O
    *   * 

    *   *
  O       O


*)

text "Each action operates on its own local state, which is defined as a 'v view type. The relationship
     between 'v and 's is definable by a lens. This is different than the seL4 process model,
      since that effectively only has a single lens whereas multiple lenses are required for
      each action here."

type_synonym ('s) state = "string \<rightharpoonup> 's"
                                
record ('s) lens = 
  Get :: "('s) state \<Rightarrow> ('s) state"
  Put :: "('s) state \<Rightarrow> ('s) state \<Rightarrow> ('s) state"

definition "make_lens footprint s =( \<lparr> 
  Get=(\<lambda>s. restrict_map s footprint), 
  Put=(\<lambda>ls s. s ++ ls)
\<rparr> )"

lemma assumes "f \<subseteq> dom s"
  and "l = make_lens f s"
  shows "(Put l) (Get l s) s = s"
  using assms
  unfolding make_lens_def
  by (simp add: map_add_subsumed2 map_le_def)

lemma assumes "f \<subseteq> dom s"
  and "l = make_lens f s"
  and "dom ls = f"
shows "(Get l) ((Put l) ls s) = ls"
  using assms
  unfolding make_lens_def
  by (simp add: map_add_dom_app_simps(1) map_le_antisym map_le_def)

text "Well-behaved lenses must follow the 'lens laws' which ensure that they retrieve and update
      the same part of the source state."

definition "var_names s = {dom v | v. v \<in> s}"

definition "extract_subvars s vs = {v | v. v \<in> s \<and> (fst v) \<in> vs }"

text "This might still need to check for well-formedness?"

definition "local_lens l s \<equiv> (\<forall>ls.
  map_le (Get l s) s
  \<and> (Put l) ls s = map_add s ls)"

lemma loc_lens_put_get: assumes "local_lens l s"
  shows "(Put l) (Get l s) s = s"
  by (metis assms local_lens_def map_add_subsumed2)

lemma assumes "f \<subseteq> dom s"
  and "l = make_lens f s"
shows "local_lens l s"
  using assms
  by (simp add: local_lens_def make_lens_def map_le_def)

(*
ls = {a=1}

s ={a=0,b=1}
 
{a=1, b=1} \<longrightarrow> {a=1}
  
*)


text "A process is a representation of a program that proceeds through a sequence of states in 
      response to actions (inputs) of type 'e, i.e. a state machine."

type_synonym ('e, 's) proc = "'e \<Rightarrow> 's state \<rightharpoonup> 's state"

definition "well_typed_rel pr V = (\<forall>s s'. (s, s') \<in> pr \<longrightarrow> (dom s = V \<and> dom s' = V))"

definition "well_typed p V = (well_typed_rel (Map.graph p) V)"  

definition "well_formed_rel p S = (\<forall>e s s'. (e, s, s') \<in> p \<longrightarrow> (s \<in> S \<and> s' \<in> S))"

lemma 
  fixes p :: "('e, 's) proc"
  assumes "well_typed (p e) V"
  and "V \<noteq> {}"
  and "card V = 2"
shows  False
  nitpick
  oops

type_synonym ('e, 's) process = "'e \<Rightarrow> (('s) state \<rightharpoonup> ('s) state)"

text "A local action is a step function on its own local state type 'a, where 'a is defineable as a lens
      on some global state type 's"

record ('s) local_action =
  lens :: "('s) lens"
  step :: "('s) state \<rightharpoonup> ('s) state"

text "An action mapping maps an action 'e to its corresponding local action"

type_synonym ('e, 's) action_mapping = "'e \<Rightarrow> ('s) local_action"

text "compose_local_actions defines how a global process can be defined from a set of local actions.
      It uses the lens defined in the local action for each action type to get the local action state
      from the global state, execute the local step function, and write the result back into
      the global state."

definition transform_step :: "('s state \<rightharpoonup> 's state) \<Rightarrow> 's state \<Rightarrow> 's state option" where
"transform_step st s = (
  let res = st s in res)"

 (* Map version:
   
*)
definition compose_local_actions :: "('e, 's) action_mapping \<Rightarrow> ('e, 's) process"  where
"compose_local_actions am = (\<lambda>e s.(
  let locact = am e;
  lns = (lens locact);
  locact_s = (Get lns) s;
  res = (step locact) locact_s in

  case res of
  Some sres \<Rightarrow> Some ((Put lns) sres s) 
  | None \<Rightarrow> None
))" 

text "'Local simulation' is where each local action simulates its corresponding
    action in the model."

definition "local_simulates af afl am_i am_m e s = (
  let proc_i = step (am_i e) in
  let proc_m = step (am_m e) in
  let impl_start = (Get (lens (am_i e))) s in
  let model_start = (Get (lens (am_m e))) (af s) in

  case (proc_i impl_start, proc_m model_start) of
  (Some si, Some sm) \<Rightarrow> afl si = sm
  | _ \<Rightarrow> True)"
  
text "Basic notion of simulation."

definition "simulates af impl_proc model_proc e s = (
case (impl_proc e s, model_proc e (af s)) of
  (Some si, Some sm) \<Rightarrow> af si = sm
  | _ \<Rightarrow> True)"

text "Vacuous check"

lemma assumes "lm = (lens (am_m e))"
    and "fm \<subseteq> Vm"
    and "lm = make_lens fm (af s)"
    and "well_typed (step (am_m e)) fm"

    and "li = (lens (am_i e))"
    and "fi \<subseteq> Vi"
    and "dom s = Vi"
    and "li = make_lens fi s"
    and "well_typed (step (am_i e)) fi"

    and "af = (\<lambda>s. (Put lm) (afl (Get li s)) s)"

    and "local_simulates af afl am_i am_m e s"
  shows False
  oops

text "We want to show that in order to prove that an implementation process simulates another 
      process, we can instead decompose both processes each into a set of local actions. If the
      local actions simulate each other, and the lenses used to define the local action states are
      well-behaved, then the global implementation process simulates the global model."

lemma assumes "dom s = V"
      and "f \<subseteq> V"
      and "l = make_lens f s"
    shows "False"
  oops

lemma assumes "f \<subseteq> V"
  and "well_typed (step (am_i e)) f"
  and "lens (am_i e) = make_lens f s"
  and "dom s = V"
shows "well_typed (compose_local_actions am_i e) V"
  using assms
  unfolding well_typed_def compose_local_actions_def well_typed_rel_def
  oops

text "Written once and for all: make_lens, af, compose_local_actions.
  Derived from each process in a system: V, f.
  Hand-written for each system: afl."

definition "well_formed_am f V s stp = \<lparr> lens = make_lens f s, step=stp \<rparr>"

(*
I:

  O       O
    *   * 

    *   *
  O       O

M:

To prove, we want a path from the current subgoal all the way to a result of no subgoals.
At each step, we want to transform the current goal into a set of subgoals, each of which are
provable either directly or using the assumptions.
*)

lemma gen_sim: 
  assumes "case (I e s, M e (af s)) of (Some i, Some m) \<Rightarrow> af i = m | _ \<Rightarrow> True"
  shows "simulates af I M e s"
  using assms
  unfolding simulates_def
  by simp


lemma 

shows "simulates af (compose_local_actions am_i) (compose_local_actions am_m) e s"
  apply(rule gen_sim)
  oops

lemma loc_glob_step: assumes "l = lens (am e)"
    and "l = make_lens f s"
  shows "case ((step (am e)) (Get l s), (compose_local_actions am) e s) of
    (Some ls,  Some gs) \<Rightarrow>  gs = s ++ ls
    | _ \<Rightarrow> True"
  using assms
  unfolding compose_local_actions_def make_lens_def well_typed_def well_typed_rel_def
  by (smt (verit, best) case_prodI in_graphI lens.select_convs(2) not_Some_eq option.case(1) option.case(2))

lemma step_equiv: 
  shows "(Put l) (step (am e) (Get l s)) s = compse_local_actions s"

theorem local_sim_imp_sim:
  assumes "lm = (lens (am_m e))"
    and "fm \<subseteq> Vm"
    and "lm = make_lens fm (af s)"
    and "well_typed (step (am_m e)) fm"

    and "li = (lens (am_i e))"
    and "fi \<subseteq> Vi"
    and "dom s = Vi"
    and "li = make_lens fi s"
    and "well_typed (step (am_i e)) fi"

    and abs_func: "af = (\<lambda>s. (Put lm) (afl (Get li s)) s)"

    and loc_sim: "local_simulates af afl am_i am_m e s"
  shows "simulates af (compose_local_actions am_i) (compose_local_actions am_m) e s"

  unfolding simulates_def local_simulates_def abs_func
  apply(case_tac "(compose_local_actions am_i e s, compose_local_actions am_m e (Put lm (afl (Get li s)) s))")
  apply(simp)
  apply(case_tac "compose_local_actions am_i e s")
   apply(auto)
  apply(case_tac "compose_local_actions am_m e (Put lm (afl (Get li s)) s)")
   apply(auto)
  nitpick
  sledgehammer
  apply(unfold assms)

  apply(simp add: well_typed_def well_typed_rel_def make_lens_def)



  apply(simp add: abs_func)
  sledgehammer
  apply(cases)
  apply(simp add: loc_glob_step)
  sledgehammer

  apply(rule gen_sim)
  apply(cases)
   apply(simp)
  apply(auto)

  unfolding simulates_def local_simulates_def compose_local_actions_def
    local_lens_def make_lens_def well_typed_def well_typed_rel_def 
  apply(cases)

   apply(auto)
  sledgehammer


theorem local_sim_imp_sim:
  assumes "lm = (lens (am_m e))"
  and "li = (lens (am_i e))"
  and "local_lens li s"
  and "local_lens lm s"
  and "local_simulates af afl am_i am_m e s"
  and "af = (\<lambda>s. (Put lm) (afl (Get li s)) s)"
  shows "simulates af (compose_local_actions am_i) (compose_local_actions am_m) e s"
  using assms
  nitpick
  unfolding simulates_def local_simulates_def compose_local_actions_def
    local_lens_def
  sledgehammer

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

lemma loc_sim: "local_simulates impl_action_mapping model_action_mapping e s"
  by (simp add: impl_action_mapping_def local_simulates_def model_action_mapping_def)

theorem "simulates (compose_local_actions impl_action_mapping)
  (compose_local_actions model_action_mapping) e s t"
  apply (rule local_sim_imp_sim)
    apply(simp add: impl_loc_lens)
   apply(simp add: mod_loc_lens)
  apply(simp add: loc_sim)
  done

end