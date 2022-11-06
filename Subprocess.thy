theory Subprocess

imports Main

begin

record ('s, 'v) lens  = 
  Get :: "'s \<Rightarrow> 'v"
  Put :: "'v \<Rightarrow> 's \<Rightarrow> 's"

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

text "'Action Refinement' is where each isolated implementation action refines its corresponding
    action in the model. The state type of the step function vfn is local to the action and does
    not refer to the global state in any way."

definition "single_action_refines stp_i stp_m = (\<forall>ss ss'. stp_i ss = ss' \<longrightarrow> stp_m ss = ss')"

definition "action_refines spmi spmm = (\<forall>e.
  let stp_i = spstep (spmi e) in 
  let stp_m = spstep (spmm e) in

  single_action_refines stp_i stp_m)"

text "Action refinement implies refinement of the processes with global state, provided that 
    each process at the global level's step function is defined via the 'compose_subprocs' 
    function"

definition "refines_dt C M = (\<forall>s s' es. exec_dt C s es = s' \<longrightarrow> exec_dt M s es = s')"

theorem "\<lbrakk>
  model_proc = \<lparr> state=s, step=compose_subprocs spmm \<rparr>;
  impl_proc = \<lparr> state=s, step=compose_subprocs spmi \<rparr>;
  action_refines spmi spmm
\<rbrakk> \<Longrightarrow> exec_dt impl_proc es = s' \<longrightarrow> exec_dt model_proc es = s'"
proof(induction es arbitrary: s s' spmm spmi)
  case Nil
  then show ?case unfolding exec_dt_def by auto
next
  case (Cons e es)
  then show ?case
    apply (clarsimp simp: compose_subprocs_def action_refines_def single_action_refines_def exec_dt_def)
    unfolding compose_subprocs_def action_refines_def single_action_refines_def refines_dt_def 
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