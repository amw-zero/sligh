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

type_synonym ('s, 'e) program = "'s \<Rightarrow> 'e \<Rightarrow> 's"

record ('s, 'v) lens  = 
  Get :: "'s \<Rightarrow> 'v"
  Put :: "'v \<Rightarrow> 's \<Rightarrow> 's"

definition execution :: "('s, 'e) program \<Rightarrow> 's \<Rightarrow> 'e list \<Rightarrow> 's" where
"execution p s es = foldl p s es"

definition refines :: "('s, 'e) program \<Rightarrow> ('s, 'e) program \<Rightarrow> bool" where
"refines c a = (\<forall>s s' es. execution c s es = s' \<longrightarrow> execution a s es = s')"

definition simpl_simulates :: "('s, 'e) program \<Rightarrow> ('s, 'e) program \<Rightarrow> bool" where
"simpl_simulates impl model = (\<forall>s e. (impl s e) = (model s e))"

record ('s, 'e) data_type =
  state :: 's
  actions :: "('e \<Rightarrow> 's \<Rightarrow> 's) list"

type_synonym counter_state = "int"

definition "inc i c = c + i"
definition "dec i c = c - i"

definition "counter = \<lparr> state=0, actions=[inc, dec] \<rparr>"

section "Point Datatype with Translation Functionality"

record Point = 
  X :: int
  Y :: int

print_theorems

definition "translateX x p = p\<lparr> X := (X p) + x \<rparr>"
definition "translateY y p = p\<lparr> Y := (Y p) + y \<rparr>" 
definition "initPoint = \<lparr> X=0, Y=0 \<rparr>"

definition "point_dt = \<lparr> state=initPoint, actions=[translateX, translateY] \<rparr>"

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

definition "point_dt_lens = \<lparr> state=initPoint, actions=[translateXLens, translateYLens] \<rparr>"

datatype point_action =
  TranslateX int
  | TranslateY int

record ('s, 'e) dt =
  state :: 's
  step :: "'e \<Rightarrow> 's \<Rightarrow> 's"

definition exec_dt :: "('s, 'e) dt \<Rightarrow> 'e list \<Rightarrow> 's" where
"exec_dt dt i = foldl (\<lambda>s e. (step dt) e s) (state dt) i"

definition "fw_sim I M = (\<forall>s s' es. exec_dt I es s = s'\<longrightarrow> exec_dt M es s = s')" 

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

definition map_lens :: "point_action \<Rightarrow> (Point, View) lens" where
"map_lens e = (case e of
  TranslateX x \<Rightarrow> \<lparr> Get=x_view, Put=lens_put \<rparr>
  | TranslateY y \<Rightarrow> \<lparr>Get=y_view, Put=lens_put\<rparr>)"

definition "translate_view i = (\<lambda>v. (case v of
  TranslateXView x \<Rightarrow> TranslateXView (translateXInt i x)
  | TranslateYView y \<Rightarrow> TranslateYView (translateYInt i y)))"


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

definition substate_step :: "('e, 's, 'v) substate_mapping \<Rightarrow> ('e, 's) dt_step"  where
"substate_step ssmap = (\<lambda>e s.(
  let sub_dt = ssmap e in
  let lns = (sdlens sub_dt) in
  let vfn = (sdview_func sub_dt) in
  let v = (Get lns) s in
  let res = vfn v in
  
  (Put lns) res s
))"

(*definition fullstate_step = "'e \<Rightarrow> 'f"*)

definition "translatex_full x p = p\<lparr> X := translateXInt (X p) x \<rparr>"
definition "translatey_full y p = p\<lparr> Y := translateYInt (Y p) y \<rparr>"

text "Show a sufficient condition / proof obligation for proving that an optimized datatype
     will always have the same execution as an unoptimized version. Show the left hand side of
     the implication, and you know the dts are equivalent.
    
    Has to be some kind of relation between the lens and view_func.

    Fundmentally looking for something that gets rid of the 's - the assumption is that states of
    type 's are always large, even in the model.

    As long as the lens is _well_behaved_, then an optimized datatype derived using the substate_step
    combinator has _equal_ execution.
  
    The value proposition of this method is that writing a well behaved lens is relatively easy,
    and once you have that you can test drastically reduced subsets of the state space and get the
    same simulation and refinement guarantees since the lens-optimized data type has the exact same
    execution as the original."

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

definition "regular_step lns vfn = (\<lambda>e s. (
  let v = (Get lns) s in
  let res = vfn v in
  
  (Put lns) res s)
)"

definition "well_behaved lns v s = (put_get lns v s \<and> get_put lns s)"

theorem "\<lbrakk>
  substate_mapping = (\<lambda>e. \<lparr>sdlens=lns, sdview_func=vfn \<rparr>);
  normal = \<lparr> state = s, step=regular_step lns vfn \<rparr>;
  optimized = \<lparr> state=s, step=substate_step substate_mapping\<rparr>
\<rbrakk> \<Longrightarrow> exec_dt normal es = exec_dt optimized es"
  unfolding substate_step_def regular_step_def exec_dt_def
  by simp

section "Example substate mapping of Point translation"

definition "point_substate_mapping e = (case e of
  TranslateX x \<Rightarrow> (let lns = \<lparr> Get=x_view, Put=lens_put \<rparr> in
    \<lparr> sdlens=lns, sdview_func=translate_view x \<rparr>)
  | TranslateY y \<Rightarrow> (let lns = \<lparr> Get=y_view, Put=lens_put \<rparr> in
    \<lparr> sdlens=lns, sdview_func=translate_view y \<rparr>))"

theorem "well_behaved (sdlens (point_substate_mapping e)) v s"
  sorry

definition "optimized_point_step = substate_step point_substate_mapping"
definition "optimized_point_dt = \<lparr> state=initPoint, step=optimized_point_step \<rparr>"

theorem "exec_dt point_dt2 es = exec_dt optimized_point_dt es"
unfolding exec_dt_def and point_dt2_def and point_dt_lens2_def and point_step_def 
    and point_step_lens_def translateXInt_def translateYInt_def
    and initPoint_def and xlens_def and ylens_def and updateX_def and updateY_def and getX_def
    and getY_def and Let_def and optimized_point_dt_def and optimized_point_step_def 
    and point_substate_mapping_def and substate_step_def and lens_put_def and x_view_def and y_view_def
    and translate_view_def
proof(induction es)
  case Nil
  then show ?case by simp
next
  case (Cons a es)
  then show ?case apply auto
qed

end