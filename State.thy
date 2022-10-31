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
"exec_dt dt i = foldl (\<lambda>s e. (step dt) e s) (state dt) i"

definition "point_step e p = (case e of
  TranslateX x \<Rightarrow> p\<lparr> X := (X p) + x \<rparr> 
| TranslateY y \<Rightarrow> p\<lparr> Y := (Y p) + y \<rparr>)"

definition "point_dt2 = \<lparr> state=initPoint, step=point_step \<rparr>"

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

theorem "exec_dt point_dt2 es = exec_dt point_dt_lens2 es"
unfolding exec_dt_def and point_dt2_def and point_dt_lens2_def and point_step_def 
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
      
record ('s, 'v) sub_dt =
  sdlens :: "('s, 'v) lens"
  sdview_func :: "'v \<Rightarrow> 'v"

text "The substate_mapping generates a sub data type given an event"

type_synonym ('e, 's, 'v) substate_mapping = "'e \<Rightarrow> ('s, 'v) sub_dt"

definition compose_substates :: "('e, 's, 'v) substate_mapping \<Rightarrow> ('e, 's) dt_step"  where
"compose_substates ssmap = (\<lambda>e s.(
  let sub_dt = ssmap e in
  let lns = (sdlens sub_dt) in
  let vfn = (sdview_func sub_dt) in
  let v = (Get lns) s in
  let res = vfn v in
  
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

text "Something not right about this - lens has to be derived from event, which makes this 
      equivalent to substate_step"

definition "view_step ssmap = (\<lambda>e s. (
  let sub_dt = ssmap e in
  let lns = (sdlens sub_dt) in
  let vfn = (sdview_func sub_dt) in
  let v = (Get lns) s in
  let res = vfn v in
  
  (Put lns) res s)
)"

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
  substate_mapping = (\<lambda>e. ssm);
  vfn = sdview_func ssm;
  lns = sdlens ssm;
  gs_dt = \<lparr> state = s, step=compose_substates substate_mapping \<rparr>;
  ss_dt = \<lparr> state=(Get lns) s, step=(\<lambda>e s. vfn s) \<rparr>
\<rbrakk> \<Longrightarrow> take_step e gs_dt = (Put lns) (take_step e ss_dt) s"
  unfolding compose_substates_def take_step_def
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

definition "point_substate_mapping e = (case e of
  TranslateX x \<Rightarrow> (let lns = \<lparr> Get=x_view, Put=lens_put \<rparr> in
    \<lparr> sdlens=lns, sdview_func=translate_view x \<rparr>)
  | TranslateY y \<Rightarrow> (let lns = \<lparr> Get=y_view, Put=lens_put \<rparr> in
    \<lparr> sdlens=lns, sdview_func=translate_view y \<rparr>))"

theorem "well_behaved (sdlens (point_substate_mapping e)) v s"
  sorry

definition "optimized_point_step = compose_substates point_substate_mapping"
definition "optimized_point_dt = \<lparr> state=initPoint, step=optimized_point_step \<rparr>"

theorem "exec_dt point_dt2 es = exec_dt optimized_point_dt es"
unfolding exec_dt_def and point_dt2_def and point_dt_lens2_def and point_step_def 
    and point_step_lens_def translateXInt_def translateYInt_def
    and initPoint_def and xlens_def and ylens_def and updateX_def and updateY_def and getX_def
    and getY_def and Let_def and optimized_point_dt_def and optimized_point_step_def 
    and point_substate_mapping_def and compose_substates_def and lens_put_def and x_view_def and y_view_def
    and translate_view_def
proof(induction es)
  case Nil
  then show ?case sorry
next
  case (Cons a es)
  then show ?case  sorry
qed

section "Global processes exhibit  data refinement"

record ('ext, 's, 'e) dt_dr =
  init :: "'ext \<Rightarrow> 's"
  step :: "'e \<Rightarrow> 's \<Rightarrow> 's"
  fin :: "'s \<Rightarrow> 'ext"

definition "exec_dt_dr dt s es = foldl (\<lambda>v e. (step dt) e v) ((init dt) s) es"

definition "refines C M = (\<forall>s s' es. exec_dt_dr C s es = s' \<longrightarrow> exec_dt_dr M s es = s')"

definition "action_refines vfn_c vfn_m = (\<forall>v v'. vfn_c v = v' \<longrightarrow> vfn_m v = v')"

theorem "\<lbrakk>  
  model_proc = \<lparr> init = (\<lambda>ex. s), step=compose_substates spmm, fin=(\<lambda>s. ext) \<rparr>; 
  impl_proc = \<lparr> init = (\<lambda>ex. s), step=compose_substates spmi, fin=(\<lambda>s. ext) \<rparr>;
  
  (\<forall>e. 
    let vfn_m = sdview_func (spmm e) in
    let lns_m = sdlens (spmm e) in
    let vfn_i = sdview_func (spmi e) in
    let lns_i = sdlens (spmi e) in
    action_refines vfn_i vfn_m)
\<rbrakk> \<Longrightarrow> refines impl_proc model_proc"
  unfolding compose_substates_def refines_def action_refines_def exec_dt_dr_def
  sorry

(*text "SR is a relation between states in I and M, i.e. SR \<subseteq> 's x 's"

definition "fw_sim SR I M = 
  (\<forall>ex s. init I ex = s \<longrightarrow>   (\<forall>s s' e. (step I) e s = s'\<longrightarrow> (step M) e s = s')" *)

section "Data refinement point translation"

text "No way to create a function from 's \<Rightarrow> 'v for substates because that depends on 'e"
(*definition "point_dt_dr = \<lparr> init = \<lambda>p. ? ? \<rparr>*)

(*

definition "view_dt = \<lparr> init=view_get, step=view_step, fin=view_put \<rparr>" 

theorem "exec_dt_dr view_dt s es = exec_dt point_dt_2 s es"

*)

end