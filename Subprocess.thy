theory Subprocess

imports Main

begin

text "Related to assume-guarantee reasoning, e.g. https://arxiv.org/pdf/2103.13743.pdf"
text "Goes back to Lamport & Abadi: https://lamport.azurewebsites.net/pubs/abadi-conjoining.pdf"

section "Step function only"

text "Each action operates on its own state, which is defined as a 'v view type that's
      definable by a lens on the state type. This is different than the seL4 process model,
      since that effectively only has a single lens whereas multiple lenses are required for
      each action here."

record ('s, 'v) lens  = 
  Get :: "'s \<Rightarrow> 'v"
  Put :: "'v \<Rightarrow> 's \<Rightarrow> 's"

definition "well_formed l \<equiv> (\<forall>ls lv. (Put l) ((Get l) ls) ls = ls \<and> (Get l) ((Put l) lv ls) = ls)"

type_synonym ('e, 's) process = "'e \<Rightarrow> 's \<Rightarrow> 's"

definition exec :: "('e, 's) process \<Rightarrow> 'e list \<Rightarrow> 's \<Rightarrow> 's" where
"exec step es i = foldl (\<lambda>s e. step e s) i es"

record ('s, 'a) subaction =
  salens :: "('s, 'a) lens"
  sastep :: "'a \<Rightarrow> 'a"

type_synonym ('e, 's, 'a) action_mapping = "'e \<Rightarrow> ('s, 'a) subaction"

(* Project the global state to the action state, then step at the local action level, then write the 
  result back to the global state *)
definition compose_subactions :: "('e, 's, 'a) action_mapping \<Rightarrow> ('e, 's) process"  where
"compose_subactions samap = (\<lambda>e s.(
  let subact = samap e;
  lns = (salens subact);
  subact_v = (Get lns) s;
  res = (sastep subact) subact_v in
  
  (Put lns) res s
))"

(* lemma - a process built with compose_subactions is equivalent to a process defined without it? *)

text "'Action Refinement' is where each isolated implementation action refines its corresponding
    action in the model. The state type of the step function vfn is local to the action and does
    not refer to the global state in any way."

definition "action_simulates am_i am_m e s t = (
  let proc_i = sastep (am_i e); proc_m = sastep (am_m e) in

  proc_i s = t \<longrightarrow> proc_m s = t)"

definition "simulates impl_proc model_proc e s t = (impl_proc e s = t \<longrightarrow> model_proc e s = t)"

theorem
  assumes "well_formed lm"
    and "am_m = (\<lambda>e. \<lparr>salens=lm, sastep=stm\<rparr>)"
    and "well_formed li"
    and "am_i = (\<lambda>e. \<lparr>salens=li, sastep=stm\<rparr> )"
    and "model_proc \<equiv> compose_subactions am_m"
    and "impl_proc \<equiv> compose_subactions am_i"
    and "action_simulates am_i am_m e s t"
  shows "simulates impl_proc model_proc e s t"
  using assms
  unfolding simulates_def action_simulates_def compose_subactions_def Let_def well_formed_def
  by (metis subaction.select_convs(1))

definition "refines impl_proc model_proc es = (\<forall>s. exec impl_proc es = s \<longrightarrow> exec model_proc es = s)"

theorem assumes "simulates impl_proc model_proc e s t"
shows "refines impl_proc model_proc es"
  oops


text "Action refinement implies refinement of the processes with global state, provided that 
    each process at the global level's step function is defined via the 'compose_subprocs' 
    function"

text "Need to relate action_simulates to global execution"

(* If a model and implementation can be built by composing sub-actions together,
   and the individal actions refine each other at the local level,
   then the global implementation refines the global model. *)
theorem 
  assumes
    "model_proc = compose_subactions am_m" and
    "impl_proc =  compose_subactions am_i" and
    "action_simulates am_i am_m"
  shows "refines impl_proc model_proc es"
proof(induction es arbitrary: s am_i am_m)
  case Nil
  then show ?case unfolding exec_def refines_def by simp
next
  case (Cons a es)
  then show ?case
    sorry
  qed

definition "simulates impl_proc model_proc= (\<forall>e s s'.
  impl_proc e s = s' \<longrightarrow> model_proc e s = s')"

theorem
  assumes "simulates I M"
  shows "refines I M es"
  oops


(* 

GlobalState |> ActionLens.Get |> Action |> ActionLens.Put

*)


record ('s, 'e) dt =
  state :: 's
  step :: "'e \<Rightarrow> 's \<Rightarrow> 's"

definition exec_dt :: "('s, 'e) dt \<Rightarrow> 'e list \<Rightarrow> 's" where
"exec_dt dt es = foldl (\<lambda>s e. (step dt) e s) (state dt) es"

type_synonym ('e, 's) dt_step = "'e \<Rightarrow> 's \<Rightarrow> 's"

section "Substate Mapping"

text "A subprocess consists of a lens on the global state and
      a function that modifies the view that's projected from the 
      lens. This view is meant to have one variant per event in 
      the original data type, where each variant represents the 
      subset of the state that that event needs to carry out its
      operation."
      
record ('s, 'v) subproc =
  splens :: "('s, 'v) lens"
  spstep :: "'v \<Rightarrow> 'v"

text "The subproc_mapping generates a sub data type given an event"

type_synonym ('e, 's, 'v) subproc_mapping = "'e \<Rightarrow> ('s, 'v) subproc"

definition compose_subprocs :: "('e, 's, 'v) subproc_mapping \<Rightarrow> ('e, 's) dt_step"  where
"compose_subprocs spmap = (\<lambda>e s.(
  let subproc = spmap e in
  let lns = (splens subproc) in
  let stp = (spstep subproc) in
  let v = (Get lns) s in
  let res = stp v in
  
  (Put lns) res s
))"

(* Some equivalence between a process and its decomposition into a set of subprocesses"
theorem
  assumes "proc = \<lparr> state=s, step=step \<rparr>"
    and   "subproc = 
  shows proc_subproc_equiv: "x = y"
*)

definition "refines_dt C M = (\<forall>s s' es. exec_dt C s es = s' \<longrightarrow> exec_dt M s es = s')"

theorem "\<lbrakk>
  model_proc = \<lparr> state=s, step=compose_subprocs spmm \<rparr>;
  impl_proc = \<lparr> state=s, step=compose_subprocs spmi \<rparr>;
  action_simulates spmi spmm
\<rbrakk> \<Longrightarrow> exec_dt impl_proc es = s' \<longrightarrow> exec_dt model_proc es = s'"
proof(induction es arbitrary: s s' spmm spmi)
  case Nil
  then show ?case unfolding exec_dt_def by auto
next
  case (Cons e es)
  then show ?case
    apply (clarsimp simp: compose_subprocs_def action_simulates_def single_action_simulates_def exec_dt_def)
    unfolding compose_subprocs_def action_simulates_def single_action_simulates_def refines_dt_def 
      exec_dt_def Let_def
    sorry
   
qed

section "Subprocs for banking"

text "operations: Open account, Transfer between accounts"

record account =
  name :: string
  balance :: int

record transaction = 
  src :: account
  dst :: account
  amount :: int

datatype BankingEvent =
    OpenAccount string
    | Transfer account account int

record banking_state_m =
  accounts :: "account set"
  ledger :: "transaction set"

definition "banking_step_m e s = (case e of
  OpenAccount nm \<Rightarrow> s\<lparr> accounts := insert \<lparr>name=nm, balance=0\<rparr> (accounts s) \<rparr>
| Transfer srcAct dstAct amt \<Rightarrow> s\<lparr> ledger := insert \<lparr>src=srcAct, dst=dstAct, amount=amt \<rparr> (ledger s) \<rparr>)"

(* Subproc version *)

definition "open_account nm acts = insert \<lparr>name=nm, balance=0\<rparr> acts"

definition "transfer srcAct dstAct amt ledg = insert \<lparr>src=srcAct, dst=dstAct, amount=amt \<rparr> ledg"

datatype banking_subproc_view =
    VOpenAccount "account set"
    | VTransfer "transaction set"

definition "set_banking_state v s = (case v of
  VOpenAccount as \<Rightarrow> s\<lparr> accounts := as \<rparr>
| VTransfer ts \<Rightarrow> s\<lparr> ledger := ts \<rparr> )"

definition "get_acts s = VOpenAccount (accounts s)"
definition "get_ledg s = ledger s"

definition "bank_subproc_mapping e = (case e of
    OpenAccount nm \<Rightarrow> \<lparr> splens=\<lparr>Get=get_acts, Put=set_banking_state\<rparr>, spstep=(\<lambda>v. open_account nm acts) \<rparr>
  | Transfer srcAct dstAct amt \<Rightarrow> 
    \<lparr>splens=\<lparr>Get=get_ledg, Put=set_banking_state\<rparr>, spstep=(\<lambda>ledg. transfer srcAct dstAct amt ledg) \<rparr>)"

end