theory Subprocess

imports Main

begin

text "A simpler version of assume-guarantee reasoning that focuses on non-interference of state 
      updates alone, e.g. https://arxiv.org/pdf/2103.13743.pdf"
text "Goes back to Lamport & Abadi: https://lamport.azurewebsites.net/pubs/abadi-conjoining.pdf"

text "Each action operates on its own state, which is defined as a 'v view type. The relationship
     between 'v and 's is definable by a lens. This is different than the seL4 process model,
      since that effectively only has a single lens whereas multiple lenses are required for
      each action here."

record ('s, 'v) lens  = 
  Get :: "'s \<Rightarrow> 'v"
  Put :: "'v \<Rightarrow> 's \<Rightarrow> 's"

text "Well-behaved lenses must follow the 'lens laws' which ensure that they retrieve and update
      the same part of the source state."

definition "well_behaved l \<equiv> (\<forall>ls lv. (Put l) ((Get l) ls) ls = ls \<and> (Get l) ((Put l) lv ls) = ls)"

text "A process is a representation of a program that proceeds through a sequence of states in 
      response to actions (inputs) of type 'e, i.e. a state machine."
type_synonym ('e, 's) process = "'e \<Rightarrow> 's \<Rightarrow> 's"

text "A subaction is a step function on its own local state type 'a, where 'a is defineable as a lens
      on some global state type 's"

record ('s, 'a) subaction =
  salens :: "('s, 'a) lens"
  sastep :: "'a \<Rightarrow> 'a"

text "An action mapping maps an action 'e to its corresponding subaction"

type_synonym ('e, 's, 'a) action_mapping = "'e \<Rightarrow> ('s, 'a) subaction"

text "compose_subactions defines how a global process can be defined from a set of subactions.
      It uses the lens defined in the subaction for each action type to get the local action state
      from the global state, execute the local action step function, and write the result back into
      the global state."

definition compose_subactions :: "('e, 's, 'a) action_mapping \<Rightarrow> ('e, 's) process"  where
"compose_subactions samap = (\<lambda>e s.(
  let subact = samap e;
  lns = (salens subact);
  subact_v = (Get lns) s;
  res = (sastep subact) subact_v in
  
  (Put lns) res s
))"

text "'Action Simulation' is where each local implementation action simulates its corresponding
    action in the model."

definition "action_simulates am_i am_m e s t = (
  let proc_i = sastep (am_i e) in
  let proc_m = sastep (am_m e) in
  let impl_start = (Get (salens (am_i e))) s in
  let model_start = (Get (salens (am_m e))) s in

  (proc_i impl_start = t) \<longrightarrow> (proc_m model_start = t))"

text "Basic notion of simulation."

definition "simulates impl_proc model_proc e s t = (impl_proc e s = t \<longrightarrow> model_proc e s = t)"

text "We want to show that in order to prove that an implementation process simulates another 
      process, we can instead decompose both processes each into a set of local actions. If the
      local actions simulate each other, and the lenses used to define the local action states are
      well-behaved, then the global implementation process simulates the global model."

theorem
  assumes "well_behaved lm"
    and "am_m = (\<lambda>e. \<lparr>salens=lm, sastep=stm\<rparr>)"
    and "well_behaved li"
    and "am_i = (\<lambda>e. \<lparr>salens=li, sastep=sti\<rparr> )"
    and "model_proc \<equiv> compose_subactions am_m"
    and "impl_proc \<equiv> compose_subactions am_i"
    and "action_simulates am_i am_m e s t"
  shows "simulates impl_proc model_proc e s t"
  using assms
  unfolding simulates_def action_simulates_def compose_subactions_def well_behaved_def
  by (metis subaction.select_convs(1))

theorem local_action_simulates:
  assumes "well_behaved (salens (am_m e))"
    and "well_behaved (salens (am_i e))"
    and "action_simulates am_i am_m e s t"
  shows "simulates (compose_subactions am_m) (compose_subactions am_i) e s t"
  using assms
  unfolding simulates_def action_simulates_def compose_subactions_def well_behaved_def
  by metis

type_synonym ('s) invariant_func = "'s \<Rightarrow> bool"

definition invariant :: "('s \<Rightarrow> bool) \<Rightarrow> 's \<Rightarrow> bool" where 
"invariant i s = i s"

(* Invariant: Unauthenticated users are always denied access" *)

definition action_invariant :: "('e, 's, 'a) action_mapping \<Rightarrow> 'a invariant_func \<Rightarrow> 'e \<Rightarrow> 's \<Rightarrow> bool" where
"action_invariant am inv e s = (
  let step = sastep (am e) in
  (inv s \<and> inv (step s))
)"

theorem local_action_invariant:
  assumes "well_behaved (salens (am_i e))"
    and "action_invariant am_i inv e s"
    and "invariant s (compose_subactions am_i)"
  shows "invariant t (compose_subactions am_i)"

definition exec :: "('e, 's) process \<Rightarrow> 'e list \<Rightarrow> 's \<Rightarrow> 's" where
"exec step es i = foldl (\<lambda>s e. step e s) i es"

definition "refines I M es s s' = (exec I es s = s' \<longrightarrow> exec M es s = s')"

theorem assumes "simulates impl_proc model_proc e s t"
  shows "refines impl_proc model_proc es s t"
oops

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