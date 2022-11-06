theory State

imports Main

begin

text "This is a study about showing the correctness of a program without referring
      to the entire state of that program / decomposing the correctness problem into a 
      set of smaller, more mangable problems. The idea being that the syntax of a program
      only ever refers to a subset of the global state, and probably a very small subset in 
      practice. So a correctness argument can be produced.

      * 'Slices' can be defined with lenses. *

      Concretely, this is a way to show use only a subset of the global state in a simulation
      proof. Typically, a simulation proof is of the form:

      forall event. Step C s event --> Step A s event, where C is a concrete program and A
      is an abstract program. This is then saying that each event can be associated with a 
      'state slice', and a new Step function can be derived which only operates on that slice.
      forall event. SlicedStep C slice event --> SlicedStep A slice event"



type_synonym ('s) state = "'s list"

record ('s, 'v) lens  = 
  Get :: "'s \<Rightarrow> 'v"
  Put :: "'v \<Rightarrow> 's \<Rightarrow> 's"

section "Point Datatype with Translation Functionality"

record Point = 
  X :: int
  Y :: int

definition "translateX x p = p\<lparr> X := (X p) + x \<rparr>"
definition "translateY y p = p\<lparr> Y := (Y p) + y \<rparr>" 
definition "initPoint = \<lparr> X=0, Y=0 \<rparr>"

section "Point Datatype with Transaction Functionality Implemented with Lenses"

text "These don't operate on the Point type, but operate on just an individual X or Y value"

definition "translateXInt x xv = xv + x"
definition "translateYInt y yv = yv + y"

text "Now define lenses"

definition getX :: "Point \<Rightarrow> int" where
"getX p = (X p)"

definition updateX :: "int \<Rightarrow> Point \<Rightarrow> Point" where
"updateX i p = p\<lparr> X := i \<rparr>"

definition "xlens \<equiv> \<lparr> Get=getX, Put=updateX \<rparr>"

definition getY :: "Point \<Rightarrow> int" where
"getY p = (Y p)"

definition updateY :: "int \<Rightarrow> Point \<Rightarrow> Point" where
"updateY i p = p\<lparr> Y := i \<rparr>"

definition "ylens \<equiv> \<lparr> Get=getY, Put=updateY \<rparr>"

definition "translateXLens x p = 
  (let get = Get xlens in 
  let put = Put xlens in
  let px = get p in
 
  put (translateXInt px x) p)"

definition "translateYLens y p = 
  (let get = Get ylens in 
  let put = Put ylens in
  let py = get p in
 
  put (translateYInt py y) p)"

value "translateXLens 8 initPoint"

datatype point_action =
  TranslateX int
  | TranslateY int

record ('s, 'e) dt =
  state :: 's
  step :: "'e \<Rightarrow> 's \<Rightarrow> 's"

definition exec_dt :: "('s, 'e) dt \<Rightarrow> 'e list \<Rightarrow> 's" where
"exec_dt dt es = foldl (\<lambda>s e. (step dt) e s) (state dt) es"

definition "point_step e p = (case e of
  TranslateX x \<Rightarrow> p\<lparr> X := (X p) + x \<rparr> 
| TranslateY y \<Rightarrow> p\<lparr> Y := (Y p) + y \<rparr>)"

definition "point_dt = \<lparr> state=initPoint, step=point_step \<rparr>"

definition "point_step_lens e p = (case e of
  TranslateX x \<Rightarrow>  (let get = Get xlens in 
  let put = Put xlens in
  let px = get p in
 
  put (translateXInt px x) p)

  | TranslateY y \<Rightarrow> (let get = Get ylens in 
  let put = Put ylens in
  let py = get p in
 
  put (translateYInt py y) p))"

definition "point_dt_lens2 = \<lparr> state=initPoint, step=point_step_lens \<rparr>"

theorem "exec_dt point_dt es = exec_dt point_dt_lens2 es"
unfolding exec_dt_def and point_dt_def and point_dt_lens2_def and point_step_def 
    and point_step_lens_def translateXInt_def translateYInt_def
    and initPoint_def and xlens_def and ylens_def and updateX_def and updateY_def and getX_def
    and getY_def and Let_def 
proof (induction es)
  case Nil
  then show ?case
    by simp
next
  case (Cons a es)
  then show ?case 
    by (auto simp: algebra_simps)
qed

datatype View =
  TranslateXView int
  | TranslateYView int

type_synonym ('e, 's) dt_step = "'e \<Rightarrow> 's \<Rightarrow> 's"

definition "x_view p = TranslateXView (X p)"
text "Had bug here where the y_view impl returned TranslateXView instead - this made the 
      lens not well behaved"
definition "y_view p = TranslateYView (Y p)"

definition "lens_put v p = (case v of
  TranslateXView x \<Rightarrow> p\<lparr> X := x \<rparr>
  | TranslateYView y \<Rightarrow> p\<lparr> Y := y \<rparr>)"


definition "translate_view i = (\<lambda>v. (case v of
  TranslateXView x \<Rightarrow> TranslateXView (translateXInt i x)
  | TranslateYView y \<Rightarrow> TranslateYView (translateYInt i y)))"


text "What full-state step function components would look like"

definition "translatex_full x p = p\<lparr> X := translateXInt (X p) x \<rparr>"
definition "translatey_full y p = p\<lparr> Y := translateYInt (Y p) y \<rparr>"


section "Substate Mapping"

text "A sub data type consists of a lens on the state and
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

text "Show a sufficient condition / proof obligation for proving that an optimized datatype
     will always have the same execution as an unoptimized version. Show the left hand side of
     the implication, and you know the dts are equivalent.
    
    Has to be some kind of relation between the lens and view_func.
  
    The value proposition of this method is that writing a well behaved lens is relatively easy,
    and once you have that you can test drastically reduced subsets of the state space and get the
    same simulation and refinement guarantees since the lens-optimized data type has the exact same
    execution as the original."


text "If we have the view-level functions which instead of mapping a global state to a global state
      map a view state to a view state, we can build a global state data type and view state 
      data type such that they have equal execution. This means that the view state data type can be
      substituded for the global state dt anywhere.

      In practice, this means building an optimized data type in this way requires defining the
      lens between global state and view given an event, the view state function, and the global state
      dt and optmized dts can be derived from that."

definition test :: "int \<Rightarrow> point_action" where "test =  TranslateX"

definition "take_step e dt = (step dt) e (state dt)"

theorem "\<lbrakk>
  subproc_mapping = (\<lambda>e. ssm);
  vfn = spstep ssm;
  lns = splens ssm;
  gs_dt = \<lparr> state = s, step=compose_subprocs subproc_mapping \<rparr>;
  ss_dt = \<lparr> state=(Get lns) s, step=(\<lambda>e s. vfn s) \<rparr>
\<rbrakk> \<Longrightarrow> take_step e gs_dt = (Put lns) (take_step e ss_dt) s"
  unfolding compose_subprocs_def take_step_def
  by (metis (no_types, lifting) dt.select_convs(1) dt.select_convs(2))

theorem "\<lbrakk>take_step e dt1 = take_step e dt2\<rbrakk> \<Longrightarrow> (exec_dt dt1 es) = (exec_dt dt2 es)" 
  sorry

text "Correctness - dt2 refines dt1, or exec_dt dt2 \<longrightarrow> exec_dt dt1.
      Can be shown by simulation (forward), which involves reasoning about
      global states. Create a substate mapping such that taking a step in the
      substate mapping implies forward simulation."

section "Well-behavedness of lenses"

definition "put_get lns v s = (
  let get = Get lns in
  let put = Put lns in
  get (put v s) = v
)"

definition "get_put lns  s = (
  let get = Get lns in
  let put = Put lns in
  put (get s) s = s 
)"

definition "well_behaved lns v s = (put_get lns v s \<and> get_put lns s)"


section "Example substate mapping of Point translation"

definition "point_subproc_mapping e = (case e of
  TranslateX x \<Rightarrow> (let lns = \<lparr> Get=x_view, Put=lens_put \<rparr> in
    \<lparr> splens=lns, spstep=translate_view x \<rparr>)
  | TranslateY y \<Rightarrow> (let lns = \<lparr> Get=y_view, Put=lens_put \<rparr> in
    \<lparr> splens=lns, spstep=translate_view y \<rparr>))"

theorem "well_behaved (splens (point_subproc_mapping e)) v s"
  sorry

definition "optimized_point_step = compose_subprocs point_subproc_mapping"
definition "optimized_point_dt = \<lparr> state=initPoint, step=optimized_point_step \<rparr>"

theorem "exec_dt point_dt es = exec_dt optimized_point_dt es"
unfolding exec_dt_def and point_dt_def and point_dt_lens2_def and point_step_def 
    and point_step_lens_def translateXInt_def translateYInt_def
    and initPoint_def and xlens_def and ylens_def and updateX_def and updateY_def and getX_def
    and getY_def and Let_def and optimized_point_dt_def and optimized_point_step_def 
    and point_subproc_mapping_def and compose_subprocs_def and lens_put_def and x_view_def and y_view_def
    and translate_view_def
proof(induction es)
  case Nil
  then show ?case sorry
next
  case (Cons a es)
  then show ?case  sorry
qed

section "Global processes exhibit data refinement"

record ('ext, 's, 'e) dt_dr =
  init :: "'ext \<Rightarrow> 's"
  step :: "'e \<Rightarrow> 's \<Rightarrow> 's"
  fin :: "'s \<Rightarrow> 'ext"

definition "exec_dt_dr dt s es = (fin dt) (foldl (\<lambda>v e. (step dt) e v) ((init dt) s) es)"

definition "refines C M = (\<forall>s s' es. exec_dt_dr C s es = s' \<longrightarrow> exec_dt_dr M s es = s')"

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

theorem "\<lbrakk>
  model_proc = \<lparr> init = any, step=compose_subprocs spmm, fin=anyf \<rparr>; 
  impl_proc = \<lparr> init = any', step=compose_subprocs spmi, fin=anyf' \<rparr>;
  action_refines spmi spmm
\<rbrakk> \<Longrightarrow> exec_dt_dr impl_proc s es = s' \<longrightarrow> exec_dt_dr model_proc s es = s'"
proof (induction es arbitrary: any any' anyf anyf' spmm spmi)
  case Nil
  then show ?case unfolding compose_subprocs_def refines_def action_refines_def exec_dt_dr_def single_action_refines_def Let_def sorry
next
  case (Cons a es)
  then show ?case sorry
qed

section "No data refinement"

lemma 
  fixes n :: nat
  shows "n mod 2 = 0 \<or> n mod 2 = 1"
  using [[simp_trace]]
proof (induction n)
  case 0
  then show ?case by auto
next
  case (Suc n)
  then show ?case by auto
qed

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

section "Data refinement point translation"

text "No way to create a function from 's \<Rightarrow> 'v for substates because that depends on 'e"
(*definition "point_dt_dr = \<lparr> init = \<lambda>p. ? ? \<rparr>*)

(*

definition "view_dt = \<lparr> init=view_get, step=view_step, fin=view_put \<rparr>" 

theorem "exec_dt_dr view_dt s es = exec_dt point_dt_2 s es"

*)

end