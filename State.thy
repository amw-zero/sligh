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


end