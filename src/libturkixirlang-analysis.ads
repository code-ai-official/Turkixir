







with Ada.Containers;
private with Ada.Finalization;
with Ada.Strings.Unbounded;

with GNATCOLL.Refcount;


with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;
with Langkit_Support.Lexical_Envs; use Langkit_Support.Lexical_Envs;
with Langkit_Support.Symbols;      use Langkit_Support.Symbols;

with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with Libturkixirlang.Common; use Libturkixirlang.Common;
private with Libturkixirlang.Implementation;
private with Libturkixirlang.Debug;




--  This package provides types and primitives to analyze source files as
--  analysis units.
--
--  This is the entry point to parse and process a unit: first create an
--  analysis context with ``Create_Context``, then get analysis units out of it
--  using the ``Get_From_*`` functions.

package Libturkixirlang.Analysis is

   use Support.Diagnostics, Support.Slocs, Support.Text;

   type Analysis_Context is tagged private;
   --  This type represents a context for all source analysis. This is the
   --  first type you need to create to use Libturkixirlang. It will contain
   --  the results of all analysis, and is the main holder for all the data.
   --
   --  You can create several analysis contexts if you need to, which enables
   --  you, for example to:
   --
   --  * analyze several different projects at the same time;
   --
   --  * analyze different parts of the same projects in parallel.
   --
   --  In the current design, contexts always keep all of their analysis units
   --  allocated. If you need to get this memory released, the only option at
   --  your disposal is to destroy your analysis context instance.

   type Analysis_Unit is new Langkit_Support.Text.Text_Buffer_Ifc with private;
   --  This type represents the analysis of a single file.
   --
   --  This type has strong-reference semantics and is ref-counted.
   --  Furthermore, a reference to a unit contains an implicit reference to the
   --  context that owns it. This means that keeping a reference to a unit will
   --  keep the context and all the unit it contains allocated.

   No_Analysis_Context : constant Analysis_Context;
   --  Special value to mean the absence of analysis context

   No_Analysis_Unit : constant Analysis_Unit;
   --  Special value to mean the absence of analysis unit. No analysis units
   --  can be passed this value.

   ---------------
   -- AST nodes --
   ---------------

      type Turkixir_Node is tagged private;
      --  Data type for all nodes. Nodes are assembled to make up a tree.  See
      --  the node primitives below to inspect such trees.
      --
      --  Unlike for contexts and units, this type has weak-reference
      --  semantics: keeping a reference to a node has no effect on the
      --  decision to keep the unit that it owns allocated. This means that
      --  once all references to the context and units related to a node are
      --  dropped, the context and its units are deallocated and the node
      --  becomes a stale reference: most operations on it will raise a
      --  ``Stale_Reference_Error``.
      --
      --  Note that since reparsing an analysis unit deallocates all the nodes
      --  it contains, this operation makes all reference to these nodes stale
      --  as well.
      --
      
      type Expr is new Turkixir_Node with private
      ;
      
      type And_Expr is new Expr with private
      ;
      
      type And_Op is new Expr with private
      ;
      
      type Arg is new Turkixir_Node with private
      ;
      
      type Arg_Assoc is new Arg with private
      ;
      
      type Arg_Gen is new Arg with private
      ;
      
      type Turkixir_Node_Base_List is new Turkixir_Node with private
      ;
      
      type Arg_List is new Turkixir_Node_Base_List with private
         with Iterable => (First       => Arg_List_First,
                           Next        => Arg_List_Next,
                           Has_Element => Arg_List_Has_Element,
                           Element     => Arg_List_Element)
      ;
      --  List of Arg.
      type Bin_Op is new Expr with private
      ;
      
      type Arith_Expr is new Bin_Op with private
      ;
      
      type As_Name_Node is new Turkixir_Node with private
      ;
      
      type As_Name_Node_List is new Turkixir_Node_Base_List with private
         with Iterable => (First       => As_Name_Node_List_First,
                           Next        => As_Name_Node_List_Next,
                           Has_Element => As_Name_Node_List_Has_Element,
                           Element     => As_Name_Node_List_Element)
      ;
      --  List of AsNameNode.
      type Stmt is new Turkixir_Node with private
      ;
      
      type Assert_Stmt is new Stmt with private
      ;
      
      type Assign_Stmt is new Stmt with private
      ;
      
      type Aug_Assign_Stmt is new Stmt with private
      ;
      
      type Break_Stmt is new Stmt with private
      ;
      
      type Call_Expr is new Expr with private
      ;
      
      type Def_Stmt is new Stmt with private
      ;
      
      type Class_Def is new Def_Stmt with private
      ;
      
      type Comprehension is new Turkixir_Node with private
      ;
      
      type Comp_For is new Comprehension with private
      ;
      
      type Comp_ForL is new Comprehension with private
      ;
      
      type Comp_If is new Turkixir_Node with private
      ;
      
      type Comp_Op is new Expr with private
      ;
      
      type Comp_Op_Kind is new Turkixir_Node with private
      ;
      
      type Comp_Op_Kind_Diamond is new Comp_Op_Kind with private
      ;
      
      type Comp_Op_Kind_Eq is new Comp_Op_Kind with private
      ;
      
      type Comp_Op_Kind_Gt is new Comp_Op_Kind with private
      ;
      
      type Comp_Op_Kind_Gte is new Comp_Op_Kind with private
      ;
      
      type Comp_Op_Kind_In is new Comp_Op_Kind with private
      ;
      
      type Comp_Op_Kind_Is is new Comp_Op_Kind with private
      ;
      
      type Comp_Op_Kind_Isnot is new Comp_Op_Kind with private
      ;
      
      type Comp_Op_Kind_Lt is new Comp_Op_Kind with private
      ;
      
      type Comp_Op_Kind_Lte is new Comp_Op_Kind with private
      ;
      
      type Comp_Op_Kind_Noteq is new Comp_Op_Kind with private
      ;
      
      type Comp_Op_Kind_Notin is new Comp_Op_Kind with private
      ;
      
      type Concat_String_Lit is new Expr with private
      ;
      
      type Continue_Stmt is new Stmt with private
      ;
      
      type Decorated is new Stmt with private
      ;
      
      type Decorator is new Turkixir_Node with private
      ;
      
      type Decorator_List is new Turkixir_Node_Base_List with private
         with Iterable => (First       => Decorator_List_First,
                           Next        => Decorator_List_Next,
                           Has_Element => Decorator_List_Has_Element,
                           Element     => Decorator_List_Element)
      ;
      --  List of Decorator.
      type Del_Stmt is new Stmt with private
      ;
      
      type Dict_Assoc is new Turkixir_Node with private
      ;
      
      type Dict_Assoc_List is new Turkixir_Node_Base_List with private
         with Iterable => (First       => Dict_Assoc_List_First,
                           Next        => Dict_Assoc_List_Next,
                           Has_Element => Dict_Assoc_List_Has_Element,
                           Element     => Dict_Assoc_List_Element)
      ;
      --  List of DictAssoc.
      type Dict_Comp is new Expr with private
      ;
      
      type Dict_Lit is new Expr with private
      ;
      
      type Dot is new Expr with private
      ;
      
      type Dot_List is new Turkixir_Node_Base_List with private
         with Iterable => (First       => Dot_List_First,
                           Next        => Dot_List_Next,
                           Has_Element => Dot_List_Has_Element,
                           Element     => Dot_List_Element)
      ;
      --  List of Dot.
      type Name is new Expr with private
      ;
      
      type Dotted_Name is new Name with private
      ;
      
      type Elif_Branch is new Stmt with private
      ;
      
      type Elif_Branch_List is new Turkixir_Node_Base_List with private
         with Iterable => (First       => Elif_Branch_List_First,
                           Next        => Elif_Branch_List_Next,
                           Has_Element => Elif_Branch_List_Has_Element,
                           Element     => Elif_Branch_List_Element)
      ;
      --  List of ElifBranch.
      type Ellipsis_Expr is new Expr with private
      ;
      
      type Else_Part is new Turkixir_Node with private
      ;
      
      type Except_Part is new Turkixir_Node with private
      ;
      
      type Except_Part_List is new Turkixir_Node_Base_List with private
         with Iterable => (First       => Except_Part_List_First,
                           Next        => Except_Part_List_Next,
                           Has_Element => Except_Part_List_Has_Element,
                           Element     => Except_Part_List_Element)
      ;
      --  List of ExceptPart.
      type Exec_Stmt is new Stmt with private
      ;
      
      type Expr_List is new Turkixir_Node_Base_List with private
         with Iterable => (First       => Expr_List_First,
                           Next        => Expr_List_Next,
                           Has_Element => Expr_List_Has_Element,
                           Element     => Expr_List_Element)
      ;
      --  List of Expr.
      --
      --  This list node can contain one of the following nodes:
      --
      --  * And_Expr
      --
      --  * And_Op
      --
      --  * Bin_Op
      --
      --  * Call_Expr
      --
      --  * Comp_Op
      --
      --  * Concat_String_Lit
      --
      --  * Dict_Comp
      --
      --  * Dict_Lit
      --
      --  * Ellipsis_Expr
      --
      --  * Factor
      --
      --  * If_Expr
      --
      --  * Inline_Eval
      --
      --  * Lambda_Def
      --
      --  * List_Comp
      --
      --  * List_Gen
      --
      --  * List_Lit
      --
      --  * Name
      --
      --  * Not_Op
      --
      --  * Number_Lit
      --
      --  * Or_Expr
      --
      --  * Or_Op
      --
      --  * Power
      --
      --  * Set_Comp
      --
      --  * Set_Lit
      --
      --  * Slice_Expr
      --
      --  * String_Lit
      --
      --  * Subscript_Expr
      --
      --  * Tuple_Lit
      --
      --  * Xor_Expr
      --
      --  * Yield_Expr
      type Slice_Expr is new Expr with private
      ;
      
      type Ext_Slice_Expr is new Slice_Expr with private
      ;
      
      type Factor is new Expr with private
      ;
      
      type File_Node is new Turkixir_Node with private
      ;
      
      type For_Stmt is new Stmt with private
      ;
      
      type Func_Def is new Def_Stmt with private
      ;
      
      type Global_Stmt is new Stmt with private
      ;
      
      type Id is new Name with private
      ;
      
      type Id_List is new Turkixir_Node_Base_List with private
         with Iterable => (First       => Id_List_First,
                           Next        => Id_List_Next,
                           Has_Element => Id_List_Has_Element,
                           Element     => Id_List_Element)
      ;
      --  List of Id.
      type If_Expr is new Expr with private
      ;
      
      type If_Stmt is new Stmt with private
      ;
      
      type Import_From is new Stmt with private
      ;
      
      type Import_Name is new Stmt with private
      ;
      
      type Import_Star is new Turkixir_Node with private
      ;
      
      type Inline_Eval is new Expr with private
      ;
      
      type Kw_Args is new Arg with private
      ;
      
      type Kw_Args_Flag is new Turkixir_Node with private
      ;
      
      type Kw_Args_Flag_Absent is new Kw_Args_Flag with private
      ;
      
      type Kw_Args_Flag_Present is new Kw_Args_Flag with private
      ;
      
      type Lambda_Def is new Expr with private
      ;
      
      type List_Comp is new Expr with private
      ;
      
      type List_Gen is new Expr with private
      ;
      
      type List_Lit is new Expr with private
      ;
      
      type NL is new Turkixir_Node with private
      ;
      
      type NL_List is new Turkixir_Node_Base_List with private
         with Iterable => (First       => NL_List_First,
                           Next        => NL_List_Next,
                           Has_Element => NL_List_Has_Element,
                           Element     => NL_List_Element)
      ;
      --  List of NL.
      type Not_Op is new Expr with private
      ;
      
      type Number_Lit is new Expr with private
      ;
      
      type Op is new Turkixir_Node with private
      ;
      
      type Or_Expr is new Expr with private
      ;
      
      type Or_Op is new Expr with private
      ;
      
      type Params is new Turkixir_Node with private
      ;
      
      type Pass_Stmt is new Stmt with private
      ;
      
      type Power is new Expr with private
      ;
      
      type Print_Stmt is new Stmt with private
      ;
      
      type Raise_Stmt is new Stmt with private
      ;
      
      type Rel_Name is new Turkixir_Node with private
      ;
      
      type Return_Stmt is new Stmt with private
      ;
      
      type Set_Comp is new Expr with private
      ;
      
      type Set_Lit is new Expr with private
      ;
      
      type Shift_Expr is new Bin_Op with private
      ;
      
      type Single_Param is new Turkixir_Node with private
      ;
      
      type Single_Param_List is new Turkixir_Node_Base_List with private
         with Iterable => (First       => Single_Param_List_First,
                           Next        => Single_Param_List_Next,
                           Has_Element => Single_Param_List_Has_Element,
                           Element     => Single_Param_List_Element)
      ;
      --  List of SingleParam.
      type Stream_Print_Stmt is new Stmt with private
      ;
      
      type String_Lit is new Expr with private
      ;
      
      type String_Lit_List is new Turkixir_Node_Base_List with private
         with Iterable => (First       => String_Lit_List_First,
                           Next        => String_Lit_List_Next,
                           Has_Element => String_Lit_List_Has_Element,
                           Element     => String_Lit_List_Element)
      ;
      --  List of StringLit.
      type Subscript_Expr is new Expr with private
      ;
      
      type Term is new Bin_Op with private
      ;
      
      type Try_Stmt is new Stmt with private
      ;
      
      type Tuple_Lit is new Expr with private
      ;
      
      type Turkixir_Node_List is new Turkixir_Node_Base_List with private
         with Iterable => (First       => Turkixir_Node_List_First,
                           Next        => Turkixir_Node_List_Next,
                           Has_Element => Turkixir_Node_List_Has_Element,
                           Element     => Turkixir_Node_List_Element)
      ;
      --  List of TurkixirNode.
      --
      --  This list node can contain one of the following nodes:
      --
      --  * As_Name_Node
      --
      --  * Assert_Stmt
      --
      --  * Assign_Stmt
      --
      --  * Aug_Assign_Stmt
      --
      --  * Break_Stmt
      --
      --  * Continue_Stmt
      --
      --  * Decorated
      --
      --  * Def_Stmt
      --
      --  * Del_Stmt
      --
      --  * Exec_Stmt
      --
      --  * Expr_List
      --
      --  * For_Stmt
      --
      --  * Global_Stmt
      --
      --  * If_Stmt
      --
      --  * Import_From
      --
      --  * Import_Name
      --
      --  * Name
      --
      --  * Pass_Stmt
      --
      --  * Print_Stmt
      --
      --  * Raise_Stmt
      --
      --  * Return_Stmt
      --
      --  * Stream_Print_Stmt
      --
      --  * Try_Stmt
      --
      --  * Turkixir_Node_List
      --
      --  * While_Stmt
      --
      --  * With_Stmt
      --
      --  * Yield_Expr
      type Var_Args is new Arg with private
      ;
      
      type Var_Args_Flag is new Turkixir_Node with private
      ;
      
      type Var_Args_Flag_Absent is new Var_Args_Flag with private
      ;
      
      type Var_Args_Flag_Present is new Var_Args_Flag with private
      ;
      
      type While_Stmt is new Stmt with private
      ;
      
      type With_Stmt is new Stmt with private
      ;
      
      type Xor_Expr is new Expr with private
      ;
      
      type Yield_Expr is new Expr with private
      ;
      

      No_Turkixir_Node : constant Turkixir_Node;
      --  Special value to represent the absence of a node. Note that every
      --  node type derived from the root type has a similar ``No_Node``
      --  constant.
      No_Expr : constant Expr;
      --% no-document: True
      No_And_Expr : constant And_Expr;
      --% no-document: True
      No_And_Op : constant And_Op;
      --% no-document: True
      No_Arg : constant Arg;
      --% no-document: True
      No_Arg_Assoc : constant Arg_Assoc;
      --% no-document: True
      No_Arg_Gen : constant Arg_Gen;
      --% no-document: True
      No_Turkixir_Node_Base_List : constant Turkixir_Node_Base_List;
      --% no-document: True
      No_Arg_List : constant Arg_List;
      --% no-document: True
      No_Bin_Op : constant Bin_Op;
      --% no-document: True
      No_Arith_Expr : constant Arith_Expr;
      --% no-document: True
      No_As_Name_Node : constant As_Name_Node;
      --% no-document: True
      No_As_Name_Node_List : constant As_Name_Node_List;
      --% no-document: True
      No_Stmt : constant Stmt;
      --% no-document: True
      No_Assert_Stmt : constant Assert_Stmt;
      --% no-document: True
      No_Assign_Stmt : constant Assign_Stmt;
      --% no-document: True
      No_Aug_Assign_Stmt : constant Aug_Assign_Stmt;
      --% no-document: True
      No_Break_Stmt : constant Break_Stmt;
      --% no-document: True
      No_Call_Expr : constant Call_Expr;
      --% no-document: True
      No_Def_Stmt : constant Def_Stmt;
      --% no-document: True
      No_Class_Def : constant Class_Def;
      --% no-document: True
      No_Comprehension : constant Comprehension;
      --% no-document: True
      No_Comp_For : constant Comp_For;
      --% no-document: True
      No_Comp_ForL : constant Comp_ForL;
      --% no-document: True
      No_Comp_If : constant Comp_If;
      --% no-document: True
      No_Comp_Op : constant Comp_Op;
      --% no-document: True
      No_Comp_Op_Kind : constant Comp_Op_Kind;
      --% no-document: True
      No_Comp_Op_Kind_Diamond : constant Comp_Op_Kind_Diamond;
      --% no-document: True
      No_Comp_Op_Kind_Eq : constant Comp_Op_Kind_Eq;
      --% no-document: True
      No_Comp_Op_Kind_Gt : constant Comp_Op_Kind_Gt;
      --% no-document: True
      No_Comp_Op_Kind_Gte : constant Comp_Op_Kind_Gte;
      --% no-document: True
      No_Comp_Op_Kind_In : constant Comp_Op_Kind_In;
      --% no-document: True
      No_Comp_Op_Kind_Is : constant Comp_Op_Kind_Is;
      --% no-document: True
      No_Comp_Op_Kind_Isnot : constant Comp_Op_Kind_Isnot;
      --% no-document: True
      No_Comp_Op_Kind_Lt : constant Comp_Op_Kind_Lt;
      --% no-document: True
      No_Comp_Op_Kind_Lte : constant Comp_Op_Kind_Lte;
      --% no-document: True
      No_Comp_Op_Kind_Noteq : constant Comp_Op_Kind_Noteq;
      --% no-document: True
      No_Comp_Op_Kind_Notin : constant Comp_Op_Kind_Notin;
      --% no-document: True
      No_Concat_String_Lit : constant Concat_String_Lit;
      --% no-document: True
      No_Continue_Stmt : constant Continue_Stmt;
      --% no-document: True
      No_Decorated : constant Decorated;
      --% no-document: True
      No_Decorator : constant Decorator;
      --% no-document: True
      No_Decorator_List : constant Decorator_List;
      --% no-document: True
      No_Del_Stmt : constant Del_Stmt;
      --% no-document: True
      No_Dict_Assoc : constant Dict_Assoc;
      --% no-document: True
      No_Dict_Assoc_List : constant Dict_Assoc_List;
      --% no-document: True
      No_Dict_Comp : constant Dict_Comp;
      --% no-document: True
      No_Dict_Lit : constant Dict_Lit;
      --% no-document: True
      No_Dot : constant Dot;
      --% no-document: True
      No_Dot_List : constant Dot_List;
      --% no-document: True
      No_Name : constant Name;
      --% no-document: True
      No_Dotted_Name : constant Dotted_Name;
      --% no-document: True
      No_Elif_Branch : constant Elif_Branch;
      --% no-document: True
      No_Elif_Branch_List : constant Elif_Branch_List;
      --% no-document: True
      No_Ellipsis_Expr : constant Ellipsis_Expr;
      --% no-document: True
      No_Else_Part : constant Else_Part;
      --% no-document: True
      No_Except_Part : constant Except_Part;
      --% no-document: True
      No_Except_Part_List : constant Except_Part_List;
      --% no-document: True
      No_Exec_Stmt : constant Exec_Stmt;
      --% no-document: True
      No_Expr_List : constant Expr_List;
      --% no-document: True
      No_Slice_Expr : constant Slice_Expr;
      --% no-document: True
      No_Ext_Slice_Expr : constant Ext_Slice_Expr;
      --% no-document: True
      No_Factor : constant Factor;
      --% no-document: True
      No_File_Node : constant File_Node;
      --% no-document: True
      No_For_Stmt : constant For_Stmt;
      --% no-document: True
      No_Func_Def : constant Func_Def;
      --% no-document: True
      No_Global_Stmt : constant Global_Stmt;
      --% no-document: True
      No_Id : constant Id;
      --% no-document: True
      No_Id_List : constant Id_List;
      --% no-document: True
      No_If_Expr : constant If_Expr;
      --% no-document: True
      No_If_Stmt : constant If_Stmt;
      --% no-document: True
      No_Import_From : constant Import_From;
      --% no-document: True
      No_Import_Name : constant Import_Name;
      --% no-document: True
      No_Import_Star : constant Import_Star;
      --% no-document: True
      No_Inline_Eval : constant Inline_Eval;
      --% no-document: True
      No_Kw_Args : constant Kw_Args;
      --% no-document: True
      No_Kw_Args_Flag : constant Kw_Args_Flag;
      --% no-document: True
      No_Kw_Args_Flag_Absent : constant Kw_Args_Flag_Absent;
      --% no-document: True
      No_Kw_Args_Flag_Present : constant Kw_Args_Flag_Present;
      --% no-document: True
      No_Lambda_Def : constant Lambda_Def;
      --% no-document: True
      No_List_Comp : constant List_Comp;
      --% no-document: True
      No_List_Gen : constant List_Gen;
      --% no-document: True
      No_List_Lit : constant List_Lit;
      --% no-document: True
      No_NL : constant NL;
      --% no-document: True
      No_NL_List : constant NL_List;
      --% no-document: True
      No_Not_Op : constant Not_Op;
      --% no-document: True
      No_Number_Lit : constant Number_Lit;
      --% no-document: True
      No_Op : constant Op;
      --% no-document: True
      No_Or_Expr : constant Or_Expr;
      --% no-document: True
      No_Or_Op : constant Or_Op;
      --% no-document: True
      No_Params : constant Params;
      --% no-document: True
      No_Pass_Stmt : constant Pass_Stmt;
      --% no-document: True
      No_Power : constant Power;
      --% no-document: True
      No_Print_Stmt : constant Print_Stmt;
      --% no-document: True
      No_Raise_Stmt : constant Raise_Stmt;
      --% no-document: True
      No_Rel_Name : constant Rel_Name;
      --% no-document: True
      No_Return_Stmt : constant Return_Stmt;
      --% no-document: True
      No_Set_Comp : constant Set_Comp;
      --% no-document: True
      No_Set_Lit : constant Set_Lit;
      --% no-document: True
      No_Shift_Expr : constant Shift_Expr;
      --% no-document: True
      No_Single_Param : constant Single_Param;
      --% no-document: True
      No_Single_Param_List : constant Single_Param_List;
      --% no-document: True
      No_Stream_Print_Stmt : constant Stream_Print_Stmt;
      --% no-document: True
      No_String_Lit : constant String_Lit;
      --% no-document: True
      No_String_Lit_List : constant String_Lit_List;
      --% no-document: True
      No_Subscript_Expr : constant Subscript_Expr;
      --% no-document: True
      No_Term : constant Term;
      --% no-document: True
      No_Try_Stmt : constant Try_Stmt;
      --% no-document: True
      No_Tuple_Lit : constant Tuple_Lit;
      --% no-document: True
      No_Turkixir_Node_List : constant Turkixir_Node_List;
      --% no-document: True
      No_Var_Args : constant Var_Args;
      --% no-document: True
      No_Var_Args_Flag : constant Var_Args_Flag;
      --% no-document: True
      No_Var_Args_Flag_Absent : constant Var_Args_Flag_Absent;
      --% no-document: True
      No_Var_Args_Flag_Present : constant Var_Args_Flag_Present;
      --% no-document: True
      No_While_Stmt : constant While_Stmt;
      --% no-document: True
      No_With_Stmt : constant With_Stmt;
      --% no-document: True
      No_Xor_Expr : constant Xor_Expr;
      --% no-document: True
      No_Yield_Expr : constant Yield_Expr;
      --% no-document: True

   function Is_Null (Node : Turkixir_Node'Class) return Boolean;
   --  Return whether this node is a null node reference.

   function Is_Token_Node
     (Node : Turkixir_Node'Class) return Boolean;
   --  Return whether this node is a node that contains only a single token.

   function Is_Synthetic
     (Node : Turkixir_Node'Class) return Boolean;
   --  Return whether this node is synthetic.

   function "=" (L, R : Turkixir_Node'Class) return Boolean;
   --  Return whether ``L`` and ``R`` designate the same node

   function Image (Node : Turkixir_Node'Class) return String;
   --  Return a short string describing ``Node``, or None" if ``Node.Is_Null``
   --  is true.

   -------------------
   -- Event handler --
   -------------------

   type Event_Handler_Interface is interface;
   --  Interface to handle events sent by the analysis context.

   procedure Unit_Requested_Callback
     (Self               : in out Event_Handler_Interface;
      Context            : Analysis_Context'Class;
      Name               : Text_Type;
      From               : Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean) is null;
   --  Callback that will be called when a unit is requested from the context
   --  ``Context``.
   --
   --  ``Name`` is the name of the requested unit.
   --
   --  ``From`` is the unit from which the unit was requested.
   --
   --  ``Found`` indicates whether the requested unit was found or not.
   --
   --  ``Is_Not_Found_Error`` indicates whether the fact that the unit was not
   --  found is an error or not.

   procedure Unit_Parsed_Callback
     (Self     : in out Event_Handler_Interface;
      Context  : Analysis_Context'Class;
      Unit     : Analysis_Unit'Class;
      Reparsed : Boolean) is null;
   --  Callback that will be called when any unit is parsed from the context
   --  ``Context``.
   --
   --  ``Unit`` is the resulting unit.
   --
   --  ``Reparsed`` indicates whether the unit was reparsed, or whether it was
   --  the first parse.

   procedure Release (Self : in out Event_Handler_Interface) is abstract;
   --  Actions to perform when releasing resources associated to Self

   procedure Do_Release (Self : in out Event_Handler_Interface'Class);
   --  Helper for the instantiation below

   package Event_Handler_References is new GNATCOLL.Refcount.Shared_Pointers
     (Event_Handler_Interface'Class, Do_Release);

   subtype Event_Handler_Reference is Event_Handler_References.Ref;
   No_Event_Handler_Ref : Event_Handler_Reference renames
      Event_Handler_References.Null_Ref;

   function Create_Event_Handler_Reference
     (Handler : Event_Handler_Interface'Class) return Event_Handler_Reference;
   --  Simple wrapper around the GNATCOLL.Refcount API to create event handler
   --  references.

   --------------------
   -- Unit providers --
   --------------------

   type Unit_Provider_Interface is interface;
   --  Interface to fetch analysis units from a name and a unit kind.
   --
   --  The unit provider mechanism provides an abstraction which assumes that
   --  to any couple (unit name, unit kind) we can associate at most one source
   --  file. This means that several couples can be associated to the same
   --  source file, but on the other hand, only one one source file can be
   --  associated to a couple.
   --
   --  This is used to make the semantic analysis able to switch from one
   --  analysis units to another.
   --
   --  See the documentation of each unit provider for the exact semantics of
   --  the unit name/kind information.

   function Get_Unit_Filename
     (Provider : Unit_Provider_Interface;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String is abstract;
   --  Return the filename corresponding to the given unit name/unit kind.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   function Get_Unit
     (Provider    : Unit_Provider_Interface;
      Context     : Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Analysis_Unit'Class is abstract;
   --  Fetch and return the analysis unit referenced by the given unit name.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   procedure Release (Provider : in out Unit_Provider_Interface) is abstract;
   --  Actions to perform when releasing resources associated to Provider

   procedure Do_Release (Provider : in out Unit_Provider_Interface'Class);
   --  Helper for the instantiation below

   package Unit_Provider_References is new GNATCOLL.Refcount.Shared_Pointers
     (Unit_Provider_Interface'Class, Do_Release);

   subtype Unit_Provider_Reference is Unit_Provider_References.Ref;
   No_Unit_Provider_Reference : Unit_Provider_Reference renames
      Unit_Provider_References.Null_Ref;

   function Create_Unit_Provider_Reference
     (Provider : Unit_Provider_Interface'Class) return Unit_Provider_Reference;
   --  Simple wrapper around the GNATCOLL.Refcount API to create unit provider
   --  references.

   ---------------------------------
   -- Analysis context primitives --
   ---------------------------------

   function Create_Context
     (Charset       : String := Default_Charset;
      File_Reader   : File_Reader_Reference := No_File_Reader_Reference;
      Unit_Provider : Unit_Provider_Reference := No_Unit_Provider_Reference;
      Event_Handler : Event_Handler_Reference := No_Event_Handler_Ref;
      With_Trivia   : Boolean := True;
      Tab_Stop      : Positive := 8)
      return Analysis_Context;
   --  Create a new analysis context.
   --
   --  ``Charset`` will be used as a default charset to decode input sources in
   --  analysis units. Please see ``GNATCOLL.Iconv`` for several supported
   --  charsets. Be careful: passing an unsupported charset is not guaranteed
   --  to raise an error here. If no charset is provided, ``"utf-8"`` is the
   --  default.
   --
   --  .. todo:: Passing an unsupported charset here is not guaranteed to raise
   --     an error right here, but this would be really helpful for users.
   --
   --  When ``With_Trivia`` is true, the parsed analysis units will contain
   --  trivias.
   --
   --  If provided, ``File_Reader`` will be used to fetch the contents of
   --  source files instead of the default, which is to just read it from the
   --  filesystem and decode it using the regular charset rules. Note that if
   --  provided, all parsing APIs that provide a buffer are forbidden, and any
   --  use of the rewriting API with the returned context is rejected.
   --
   --  If provided, ``Unit_Provider`` will be used to query the file name that
   --  corresponds to a unit reference during semantic analysis. If it is
   --  ``null``, the default one is used instead.
   --
   --  ``Tab_Stop`` is a positive number to describe the effect of tabulation
   --  characters on the column number in source files.
   --% belongs-to: Analysis_Context

   function Has_Unit
     (Context       : Analysis_Context'Class;
      Unit_Filename : String) return Boolean;
   --  Return whether ``Context`` contains a unit correponding to
   --  ``Unit_Filename``.

   function Get_From_File
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
   --  Create a new analysis unit for ``Filename`` or return the existing one
   --  if any. If ``Reparse`` is true and the analysis unit already exists,
   --  reparse it from ``Filename``.
   --
   --  ``Rule`` controls which grammar rule is used to parse the unit.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as file opening, decoding, lexing or parsing
   --  failure, return an analysis unit anyway: errors are described as
   --  diagnostics of the returned analysis unit.
   --
   --  It is invalid to pass ``True`` to ``Reparse`` if a rewriting context is
   --  active.

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
   --  Create a new analysis unit for ``Filename`` or return the existing one
   --  if any. Whether the analysis unit already exists or not, (re)parse it
   --  from the source code in ``Buffer``.
   --
   --  ``Rule`` controls which grammar rule is used to parse the unit.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as file opening, decoding, lexing or parsing
   --  failure, return an analysis unit anyway: errors are described as
   --  diagnostics of the returned analysis unit.
   --
   --  Calling this is invalid if a rewriting context is active.

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : Ada.Strings.Unbounded.Unbounded_String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
   --  Likewise, but working on an unbounded string

   function Get_With_Error
     (Context  : Analysis_Context'Class;
      Filename : String;
      Error    : Text_Type;
      Charset  : String := "";
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
   --  If a Unit for ``Filename`` already exists, return it unchanged.
   --  Otherwise, create an empty analysis unit for ``Filename`` with a
   --  diagnostic that contains the ``Error`` message.


   function Unit_Provider
     (Context : Analysis_Context'Class) return Unit_Provider_Reference;
   --  Return the unit provider for ``Context``
   --
   --% belongs-to: Analysis_Context

   function Hash (Context : Analysis_Context) return Ada.Containers.Hash_Type;
   --  Return a hash for this context, to be used in hash tables.

   function Has_With_Trivia (Context : Analysis_Context'Class) return Boolean;
   --  Return whether ``Context`` keeps trivia when parsing units

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Analysis_Context'Class; Discard : Boolean);
   --  Debug helper. Set whether ``Property_Error`` exceptions raised in
   --  ``Populate_Lexical_Env`` should be discarded. They are by default.

   procedure Set_Logic_Resolution_Timeout
     (Context : Analysis_Context'Class; Timeout : Natural);
   --  If ``Timeout`` is greater than zero, set a timeout for the resolution of
   --  logic equations. The unit is the number of steps in ANY/ALL relations.
   --  If ``Timeout`` is zero, disable the timeout. By default, the timeout is
   --  ``100 000`` steps.

   procedure Set_Lookup_Cache_Mode (Mode : Lookup_Cache_Kind);
   --  Set the lexical environments lookup cache mode according to ``Mode``.
   --  Note: Mainly meant for debugging the default mode.

   function Has_Rewriting_Handle
     (Context : Analysis_Context'Class) return Boolean;
   --  Return whether ``Context`` has a rewriting handler (see
   --  ``Libturkixirlang.Rewriting``), i.e. whether it is in the process of
   --  rewriting. If true, this means that the set of currently loaded analysis
   --  units is frozen until the rewriting process is done.

   function Get_Symbol_Table
     (Context : Analysis_Context'Class) return Symbol_Table;
   --  Return the symbol table attached to this context. Useful for users
   --  needing their own symbolization and wanting to share it with their
   --  language frontend.
   --
   --  WARNING: EXPERIMENTAL & UNSAFE - The Symbol_Table exposes an unsafe API,
   --  that might be subject to some changes, use with caution.

   ------------------------------
   -- Analysis unit primitives --
   ------------------------------

   function Context (Unit : Analysis_Unit'Class) return Analysis_Context;
   --  Return the context that owns this unit.

   function Hash (Unit : Analysis_Unit) return Ada.Containers.Hash_Type;
   --  Return a hash for this unit, to be used in hash tables.

   procedure Reparse (Unit : Analysis_Unit'Class; Charset : String := "");
   --  Reparse an analysis unit from the associated file.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   procedure Reparse
     (Unit    : Analysis_Unit'Class;
      Charset : String := "";
      Buffer  : String);
   --  Reparse an analysis unit from a buffer.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   procedure Populate_Lexical_Env (Unit : Analysis_Unit'Class);
   --  Create lexical environments for this analysis unit, according to the
   --  specifications given in the language spec.
   --
   --  If not done before, it will be automatically called during semantic
   --  analysis. Calling it before enables one to control where the latency
   --  occurs.
   --
   --  Depending on whether errors are discarded (see
   --  ``Discard_Errors_In_Populate_Lexical_Env``), raise a ``Property_Error``
   --  on failure.

   function Get_Filename (Unit : Analysis_Unit'Class) return String;
   --  Return the filename this unit is associated to.

   function Get_Charset (Unit : Analysis_Unit'Class) return String;
   --  Return the charset that was used to parse Unit

   function Has_Diagnostics (Unit : Analysis_Unit'Class) return Boolean;
   --  Return whether this unit has associated diagnostics.

   function Diagnostics (Unit : Analysis_Unit'Class) return Diagnostics_Array;
   --  Return an array that contains the diagnostics associated to this unit.

   function Format_GNU_Diagnostic
     (Unit : Analysis_Unit'Class; D : Diagnostic) return String;
   --  Format a diagnostic in a GNU fashion. See
   --  <https://www.gnu.org/prep/standards/html_node/Errors.html>.

   pragma Warnings (Off, "defined after private extension");
   function Root (Unit : Analysis_Unit'Class) return Turkixir_Node;
   --  Return the root node for this unit, or ``null`` if there is none.
   pragma Warnings (On, "defined after private extension");

   function First_Token (Unit : Analysis_Unit'Class) return Token_Reference;
   --  Return a reference to the first token scanned in this unit.

   function Last_Token (Unit : Analysis_Unit'Class) return Token_Reference;
   --  Return a reference to the last token scanned in this unit.

   function Token_Count (Unit : Analysis_Unit'Class) return Natural;
   --  Return the number of tokens in this unit.

   function Trivia_Count (Unit : Analysis_Unit'Class) return Natural;
   --  Return the number of trivias in this unit. This is 0 for units that were
   --  parsed with trivia analysis disabled.

   function Text (Unit : Analysis_Unit'Class) return Text_Type;
   --  Return the source buffer associated to this unit.

   function Lookup_Token
     (Unit : Analysis_Unit'Class; Sloc : Source_Location)
      return Token_Reference;
   --  Look for a token in this unit that contains the given source location.
   --  If this falls before the first token, return the first token. If this
   --  falls between two tokens, return the token that appears before. If this
   --  falls after the last token, return the last token. If there is no token
   --  in this unit, return no token.

   procedure Dump_Lexical_Env (Unit : Analysis_Unit'Class);
   --  Debug helper: output the lexical envs for the given analysis unit.

   procedure Trigger_Envs_Debug (Is_Active : Boolean);
   --  Debug helper: activate debug traces for lexical envs lookups

   procedure Print (Unit : Analysis_Unit'Class; Show_Slocs : Boolean := True);
   --  Debug helper: output the AST and eventual diagnostic for this unit on
   --  standard output.
   --
   --  If Show_Slocs, include AST nodes' source locations in the output.

   procedure PP_Trivia (Unit : Analysis_Unit'Class);
   --  Debug helper: output a minimal AST with mixed trivias

   overriding function Get_Line
     (Unit : Analysis_Unit; Line_Number : Positive) return Text_Type;
   --  Return the line of text at line number ``Line_Number``

   type Child_Record (Kind : Child_Or_Trivia := Child) is record
      case Kind is
         when Child =>
            Node : Turkixir_Node;
         when Trivia =>
            Trivia : Token_Reference;
      end case;
   end record;
   --  Variant that holds either an AST node or a token

   type Children_Array is array (Positive range <>) of Child_Record;

   function Children_And_Trivia
     (Node : Turkixir_Node'Class) return Children_Array;
   --  Return the children of this node interleaved with Trivia token nodes, so
   --  that:
   --
   --  - Every trivia contained between ``Node.Start_Token`` and
   --    ``Node.End_Token - 1`` will be part of the returned array.
   --
   --  - Nodes and trivias will be lexically ordered.

   ---------------------
   -- Composite types --
   ---------------------

            
   type Turkixir_Node_Array is
      array (Positive range <>) of Turkixir_Node;


   --------------------
   -- Token Iterator --
   --------------------

   type Token_Iterator is private
      with Iterable => (First       => First_Token,
                        Next        => Next_Token,
                        Has_Element => Has_Element,
                        Element     => Element);
   --  Allow iteration on a range of tokens corresponding to a node

   function First_Token (Self : Token_Iterator) return Token_Reference;
   --  Return the first token corresponding to the node

   function Next_Token
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference;
   --  Return the token that follows Tok in the token stream

   function Has_Element
     (Self : Token_Iterator; Tok : Token_Reference) return Boolean;
   --  Return if Tok is in Self's iteration range

   function Element
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference;
   --  Identity function: helper for the Iterable aspect

   -------------------------
   -- AST Node primitives --
   -------------------------

   function Kind
     (Node : Turkixir_Node'Class) return Turkixir_Node_Kind_Type;
   function Kind_Name (Node : Turkixir_Node'Class) return String;
   --  Return the concrete kind for Node

   pragma Warnings (Off, "defined after private extension");




         
   function Parent
     (Node : Turkixir_Node'Class) return Turkixir_Node;
   --  Return the syntactic parent for this node. Return null for the root
   --  node.

         
   function Parents
     (Node : Turkixir_Node'Class;
      With_Self : Boolean := True) return Turkixir_Node_Array;
   --  Return an array that contains the lexical parents, this node included
   --  iff ``with_self`` is True. Nearer parents are first in the list.

         
   function Children
     (Node : Turkixir_Node'Class) return Turkixir_Node_Array;
   --  Return an array that contains the direct lexical children.
   --
   --  .. warning:: This constructs a whole array, and as such is much less
   --     efficient than calling the :ada:ref:`Child` built-in.

         
   function Token_Start
     (Node : Turkixir_Node'Class) return Token_Reference;
   --  Return the first token used to parse this node.

         
   function Token_End
     (Node : Turkixir_Node'Class) return Token_Reference;
   --  Return the last token used to parse this node.

         
   function Child_Index
     (Node : Turkixir_Node'Class) return Integer;
   --  Return the 0-based index for Node in its parent's children.

         
   function Previous_Sibling
     (Node : Turkixir_Node'Class) return Turkixir_Node;
   --  Return the node's previous sibling, or null if there is no such sibling.

         
   function Next_Sibling
     (Node : Turkixir_Node'Class) return Turkixir_Node;
   --  Return the node's next sibling, or null if there is no such sibling.

         
   function Unit
     (Node : Turkixir_Node'Class) return Analysis_Unit;
   --  Return the analysis unit owning this node.

         
   function Is_Ghost
     (Node : Turkixir_Node'Class) return Boolean;
   --  Return whether the node is a ghost.
   --
   --  Unlike regular nodes, ghost nodes cover no token in the input source:
   --  they are logically located instead between two tokens. Both the
   --  ``token_start`` and the ``token_end`` of all ghost nodes is the token
   --  right after this logical position.

         
   function Full_Sloc_Image
     (Node : Turkixir_Node'Class) return Text_Type;
   --  Return a string containing the filename + the sloc in GNU conformant
   --  format. Useful to create diagnostics from a node.










         
   

   function F_Left
     (Node : And_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr


         
   

   function F_Right
     (Node : And_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr







         
   

   function F_Left
     (Node : And_Op'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Right
     (Node : And_Op'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr












         
   

   function F_Name
     (Node : Arg_Assoc'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Expr
     (Node : Arg_Assoc'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr







         
   

   function F_Expr
     (Node : Arg_Gen'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Comprehension
     (Node : Arg_Gen'Class) return Comp_For;
   










         function List_Child
           (Node : Arg_List'Class; Index : Positive)
            return Arg;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Arg_List_First (Node : Arg_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Arg_List_Next
           (Node : Arg_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Arg_List_Has_Element
           (Node : Arg_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Arg_List_Element
           (Node : Arg_List; Cursor : Positive)
            return Arg'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Left
     (Node : Bin_Op'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr


         
   

   function F_Op
     (Node : Bin_Op'Class) return Op;
   


         
   

   function F_Right
     (Node : Bin_Op'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Arith_Expr
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Term
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr












         
   

   function F_Imported
     (Node : As_Name_Node'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_As_Name
     (Node : As_Name_Node'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr





         function List_Child
           (Node : As_Name_Node_List'Class; Index : Positive)
            return As_Name_Node;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function As_Name_Node_List_First (Node : As_Name_Node_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function As_Name_Node_List_Next
           (Node : As_Name_Node_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function As_Name_Node_List_Has_Element
           (Node : As_Name_Node_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function As_Name_Node_List_Element
           (Node : As_Name_Node_List; Cursor : Positive)
            return As_Name_Node'Class;
         --  Implementation detail for the Iterable aspect











         
   

   function F_Test_Expr
     (Node : Assert_Stmt'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Msg
     (Node : Assert_Stmt'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr







         
   

   function F_L_Value
     (Node : Assign_Stmt'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_R_Values
     (Node : Assign_Stmt'Class) return Turkixir_Node_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Expr_List
   --
   --  * Yield_Expr







         
   

   function F_L_Value
     (Node : Aug_Assign_Stmt'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Op
     (Node : Aug_Assign_Stmt'Class) return Op;
   


         
   

   function F_R_Value
     (Node : Aug_Assign_Stmt'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Expr_List
   --
   --  * Yield_Expr












         
   

   function F_Prefix
     (Node : Call_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr


         
   

   function F_Suffix
     (Node : Call_Expr'Class) return Arg_List;
   












         
   

   function F_Name
     (Node : Class_Def'Class) return Id;
   


         
   

   function F_Bases
     (Node : Class_Def'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Statements
     (Node : Class_Def'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr












         
   

   function F_Exprs
     (Node : Comp_For'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Target
     (Node : Comp_For'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Comp
     (Node : Comp_For'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Comp_For
   --
   --  * Comp_If







         
   

   function F_Exprs
     (Node : Comp_ForL'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Target
     (Node : Comp_ForL'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Comp
     (Node : Comp_ForL'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Comp_ForL
   --
   --  * Comp_If







         
   

   function F_Test
     (Node : Comp_If'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Comp
     (Node : Comp_If'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Comp_If
   --
   --  * Comprehension







         
   

   function F_Left
     (Node : Comp_Op'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Op
     (Node : Comp_Op'Class) return Comp_Op_Kind;
   

      function F_Op
        (Node : Comp_Op'Class) return Turkixir_Comp_Op_Kind;

         
   

   function F_Right
     (Node : Comp_Op'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr



































































         
   

   function F_First_Str
     (Node : Concat_String_Lit'Class) return String_Lit;
   


         
   

   function F_Subsequent_Str
     (Node : Concat_String_Lit'Class) return String_Lit_List;
   












         
   

   function F_Decorators
     (Node : Decorated'Class) return Decorator_List;
   


         
   

   function F_Defn
     (Node : Decorated'Class) return Def_Stmt;
   







         
   

   function F_Dec_Name
     (Node : Decorator'Class) return Name;
   


         
   

   function F_Arg_List
     (Node : Decorator'Class) return Arg_List;
   





         function List_Child
           (Node : Decorator_List'Class; Index : Positive)
            return Decorator;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Decorator_List_First (Node : Decorator_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Decorator_List_Next
           (Node : Decorator_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Decorator_List_Has_Element
           (Node : Decorator_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Decorator_List_Element
           (Node : Decorator_List; Cursor : Positive)
            return Decorator'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Exprs
     (Node : Del_Stmt'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr







         
   

   function F_Key
     (Node : Dict_Assoc'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Value
     (Node : Dict_Assoc'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr





         function List_Child
           (Node : Dict_Assoc_List'Class; Index : Positive)
            return Dict_Assoc;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Dict_Assoc_List_First (Node : Dict_Assoc_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Dict_Assoc_List_Next
           (Node : Dict_Assoc_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Dict_Assoc_List_Has_Element
           (Node : Dict_Assoc_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Dict_Assoc_List_Element
           (Node : Dict_Assoc_List; Cursor : Positive)
            return Dict_Assoc'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Assoc
     (Node : Dict_Comp'Class) return Dict_Assoc;
   


         
   

   function F_Comprehension
     (Node : Dict_Comp'Class) return Comp_For;
   







         
   

   function F_Assocs
     (Node : Dict_Lit'Class) return Dict_Assoc_List;
   










         function List_Child
           (Node : Dot_List'Class; Index : Positive)
            return Dot;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Dot_List_First (Node : Dot_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Dot_List_Next
           (Node : Dot_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Dot_List_Has_Element
           (Node : Dot_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Dot_List_Element
           (Node : Dot_List; Cursor : Positive)
            return Dot'Class;
         --  Implementation detail for the Iterable aspect











         
   

   function F_Prefix
     (Node : Dotted_Name'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr


         
   

   function F_Suffix
     (Node : Dotted_Name'Class) return Id;
   







         
   

   function F_Cond_Test
     (Node : Elif_Branch'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Statements
     (Node : Elif_Branch'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr





         function List_Child
           (Node : Elif_Branch_List'Class; Index : Positive)
            return Elif_Branch;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Elif_Branch_List_First (Node : Elif_Branch_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Elif_Branch_List_Next
           (Node : Elif_Branch_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Elif_Branch_List_Has_Element
           (Node : Elif_Branch_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Elif_Branch_List_Element
           (Node : Elif_Branch_List; Cursor : Positive)
            return Elif_Branch'Class;
         --  Implementation detail for the Iterable aspect











         
   

   function F_Statements
     (Node : Else_Part'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr







         
   

   function F_As_Name
     (Node : Except_Part'Class) return As_Name_Node;
   


         
   

   function F_Statements
     (Node : Except_Part'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr





         function List_Child
           (Node : Except_Part_List'Class; Index : Positive)
            return Except_Part;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Except_Part_List_First (Node : Except_Part_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Except_Part_List_Next
           (Node : Except_Part_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Except_Part_List_Has_Element
           (Node : Except_Part_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Except_Part_List_Element
           (Node : Except_Part_List; Cursor : Positive)
            return Except_Part'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Expr
     (Node : Exec_Stmt'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_In_List
     (Node : Exec_Stmt'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr





         function List_Child
           (Node : Expr_List'Class; Index : Positive)
            return Expr;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Expr_List_First (Node : Expr_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Expr_List_Next
           (Node : Expr_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Expr_List_Has_Element
           (Node : Expr_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Expr_List_Element
           (Node : Expr_List; Cursor : Positive)
            return Expr'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_First
     (Node : Slice_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Last
     (Node : Slice_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr







         
   

   function F_Stride
     (Node : Ext_Slice_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr







         
   

   function F_Op
     (Node : Factor'Class) return Op;
   


         
   

   function F_Expr
     (Node : Factor'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr







         
   

   function F_Statements
     (Node : File_Node'Class) return Turkixir_Node_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Decorated
   --
   --  * Def_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * For_Stmt
   --
   --  * Global_Stmt
   --
   --  * If_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Try_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * While_Stmt
   --
   --  * With_Stmt
   --
   --  * Yield_Expr







         
   

   function F_Bindings
     (Node : For_Stmt'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Expr
     (Node : For_Stmt'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Statements
     (Node : For_Stmt'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr


         
   

   function F_Else_Part
     (Node : For_Stmt'Class) return Else_Part;
   







         
   

   function F_Name
     (Node : Func_Def'Class) return Id;
   


         
   

   function F_Parameters
     (Node : Func_Def'Class) return Params;
   


         
   

   function F_Body
     (Node : Func_Def'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr







         
   

   function F_Names
     (Node : Global_Stmt'Class) return Id_List;
   










         function List_Child
           (Node : Id_List'Class; Index : Positive)
            return Id;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Id_List_First (Node : Id_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Id_List_Next
           (Node : Id_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Id_List_Has_Element
           (Node : Id_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Id_List_Element
           (Node : Id_List; Cursor : Positive)
            return Id'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Expr
     (Node : If_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Cond
     (Node : If_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Else_Expr
     (Node : If_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr







         
   

   function F_Cond_Test
     (Node : If_Stmt'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Statements
     (Node : If_Stmt'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr


         
   

   function F_Elif_Branchs
     (Node : If_Stmt'Class) return Elif_Branch_List;
   


         
   

   function F_Else_Part
     (Node : If_Stmt'Class) return Else_Part;
   







         
   

   function F_Rel_Name
     (Node : Import_From'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Name
   --
   --  * Rel_Name


         
   

   function F_Imported
     (Node : Import_From'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Import_Star
   --
   --  * Turkixir_Node_List







         
   

   function F_Imported_Names
     (Node : Import_Name'Class) return Turkixir_Node_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * As_Name_Node
   --
   --  * Name












         
   

   function F_Exprs
     (Node : Inline_Eval'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr







         
   

   function F_Expr
     (Node : Kw_Args'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr








         
   function P_As_Bool
     (Node : Kw_Args_Flag'Class) return Boolean;
   --  Return whether this is an instance of KwArgsFlagPresent















         
   

   function F_Args
     (Node : Lambda_Def'Class) return Params;
   


         
   

   function F_Expr
     (Node : Lambda_Def'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr







         
   

   function F_Expr
     (Node : List_Comp'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Comprehension
     (Node : List_Comp'Class) return Comp_ForL;
   







         
   

   function F_Expr
     (Node : List_Gen'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Comprehension
     (Node : List_Gen'Class) return Comp_ForL;
   







         
   

   function F_Exprs
     (Node : List_Lit'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr










         function List_Child
           (Node : NL_List'Class; Index : Positive)
            return NL;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function NL_List_First (Node : NL_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function NL_List_Next
           (Node : NL_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function NL_List_Has_Element
           (Node : NL_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function NL_List_Element
           (Node : NL_List; Cursor : Positive)
            return NL'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Expr
     (Node : Not_Op'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

















         
   

   function F_Left
     (Node : Or_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Right
     (Node : Or_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr







         
   

   function F_Left
     (Node : Or_Op'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Right
     (Node : Or_Op'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr







         
   

   function F_Single_Params
     (Node : Params'Class) return Single_Param_List;
   












         
   

   function F_Left
     (Node : Power'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr


         
   

   function F_Right
     (Node : Power'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr







         
   

   function F_Exprs
     (Node : Print_Stmt'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr







         
   

   function F_Exprs
     (Node : Raise_Stmt'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr







         
   

   function F_Dots
     (Node : Rel_Name'Class) return Dot_List;
   


         
   

   function F_Name
     (Node : Rel_Name'Class) return Name;
   







         
   

   function F_Exprs
     (Node : Return_Stmt'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr







         
   

   function F_Expr
     (Node : Set_Comp'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Comprehension
     (Node : Set_Comp'Class) return Comp_For;
   







         
   

   function F_Exprs
     (Node : Set_Lit'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr












         
   

   function F_Is_Varargs
     (Node : Single_Param'Class) return Var_Args_Flag;
   

      function F_Is_Varargs (Node : Single_Param'Class) return Boolean;


         
   

   function F_Is_Kwargs
     (Node : Single_Param'Class) return Kw_Args_Flag;
   

      function F_Is_Kwargs (Node : Single_Param'Class) return Boolean;


         
   

   function F_Name
     (Node : Single_Param'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Id
   --
   --  * Id_List


         
   

   function F_Default_Value
     (Node : Single_Param'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr





         function List_Child
           (Node : Single_Param_List'Class; Index : Positive)
            return Single_Param;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Single_Param_List_First (Node : Single_Param_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Single_Param_List_Next
           (Node : Single_Param_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Single_Param_List_Has_Element
           (Node : Single_Param_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Single_Param_List_Element
           (Node : Single_Param_List; Cursor : Positive)
            return Single_Param'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Stream_Expr
     (Node : Stream_Print_Stmt'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Exprs
     (Node : Stream_Print_Stmt'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr










         function List_Child
           (Node : String_Lit_List'Class; Index : Positive)
            return String_Lit;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function String_Lit_List_First (Node : String_Lit_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function String_Lit_List_Next
           (Node : String_Lit_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function String_Lit_List_Has_Element
           (Node : String_Lit_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function String_Lit_List_Element
           (Node : String_Lit_List; Cursor : Positive)
            return String_Lit'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Prefix
     (Node : Subscript_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr


         
   

   function F_Suffix
     (Node : Subscript_Expr'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Ellipsis_Expr
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * Slice_Expr
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr












         
   

   function F_Statements
     (Node : Try_Stmt'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr


         
   

   function F_Except_Parts
     (Node : Try_Stmt'Class) return Except_Part_List;
   


         
   

   function F_Else_Part
     (Node : Try_Stmt'Class) return Else_Part;
   


         
   

   function F_Finally_Part
     (Node : Try_Stmt'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr







         
   

   function F_Exprs
     (Node : Tuple_Lit'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr






         function Turkixir_Node_List_First (Node : Turkixir_Node_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Turkixir_Node_List_Next
           (Node : Turkixir_Node_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Turkixir_Node_List_Has_Element
           (Node : Turkixir_Node_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Turkixir_Node_List_Element
           (Node : Turkixir_Node_List; Cursor : Positive)
            return Turkixir_Node'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Expr
     (Node : Var_Args'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr








         
   function P_As_Bool
     (Node : Var_Args_Flag'Class) return Boolean;
   --  Return whether this is an instance of VarArgsFlagPresent















         
   

   function F_Cond_Test
     (Node : While_Stmt'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Statements
     (Node : While_Stmt'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr


         
   

   function F_Else_Part
     (Node : While_Stmt'Class) return Else_Part;
   







         
   

   function F_Bindings
     (Node : With_Stmt'Class) return As_Name_Node_List;
   


         
   

   function F_Statements
     (Node : With_Stmt'Class) return Turkixir_Node;
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr







         
   

   function F_Left
     (Node : Xor_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr


         
   

   function F_Right
     (Node : Xor_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr







         
   

   function F_Exprs
     (Node : Yield_Expr'Class) return Expr_List;
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr




   pragma Warnings (On, "defined after private extension");

   -------------------------------
   -- Tree traversal operations --
   -------------------------------

   function Children_Count
     (Node : Turkixir_Node'Class) return Natural;
   --  Return the number of children ``Node`` has

   function First_Child_Index
     (Node : Turkixir_Node'Class) return Natural;
   --  Return the index of the first child ``Node` has

   function Last_Child_Index
     (Node : Turkixir_Node'Class) return Natural;
   --  Return the index of the last child ``Node`` has, or 0 if there is no
   --  child.

   pragma Warnings (Off, "defined after private extension");
   procedure Get_Child
     (Node            : Turkixir_Node'Class;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Turkixir_Node);
   --  Return the ``Index``'th child of node, storing it into ``Result``.
   --
   --  Child indexing is 1-based. Store in ``Index_In_Bounds`` whether ``Node``
   --  had such a child: if not (i.e. ``Index`` is out-of-bounds), set
   --  ``Result`` to a null node.

   function Child
     (Node  : Turkixir_Node'Class;
      Index : Positive)
      return Turkixir_Node;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function First_Child
     (Node : Turkixir_Node'Class) return Turkixir_Node;
   --  Return the first child ``Node` has, or ``No_Turkixir_Node`` if
   --  there is none.

   function Last_Child
     (Node : Turkixir_Node'Class) return Turkixir_Node;
   --  Return the last child ``Node`` has, or ``No_Turkixir_Node`` if
   --  there is none.
   pragma Warnings (On, "defined after private extension");

   function Traverse
     (Node  : Turkixir_Node'Class;
      Visit : access function (Node : Turkixir_Node'Class)
                               return Visit_Status)
     return Visit_Status;
   --  Call ``Visit`` on ``Node`` and all its children, transitively. Calls
   --  happen in prefix order (i.e. top-down and left first). The traversal is
   --  controlled as follows by the result returned by Visit:
   --
   --  ``Into``
   --     The traversal continues normally with the syntactic children of the
   --     node just processed.
   --
   --  ``Over``
   --     The children of the node just processed are skipped and excluded from
   --     the traversal, but otherwise processing continues elsewhere in the
   --     tree.
   --
   --  ``Stop``
   --     The entire traversal is immediately abandoned, and the original call
   --     to ``Traverse`` returns ``Stop``.

   procedure Traverse
     (Node  : Turkixir_Node'Class;
      Visit : access function (Node : Turkixir_Node'Class)
                               return Visit_Status);
   --  This is the same as ``Traverse`` function except that no result is
   --  returned i.e. the ``Traverse`` function is called and the result is
   --  simply discarded.

   ----------------------------------------
   -- Source location-related operations --
   ----------------------------------------

   function Sloc_Range
     (Node : Turkixir_Node'Class) return Source_Location_Range;
   --  Return the source location range corresponding to the set of tokens from
   --  which Node was parsed.

   function Compare
     (Node : Turkixir_Node'Class;
      Sloc : Source_Location) return Relative_Position;
   --  Compare Sloc to the sloc range of Node

   pragma Warnings (Off, "defined after private extension");
   function Lookup
     (Node : Turkixir_Node'Class;
      Sloc : Source_Location) return Turkixir_Node;
   --  Look for the bottom-most AST node whose sloc range contains Sloc. Return
   --  it, or null if no such node was found.
   pragma Warnings (On, "defined after private extension");

   -----------------------
   -- Lexical utilities --
   -----------------------

   function Text (Node : Turkixir_Node'Class) return Text_Type;
   --  Return the source buffer slice corresponding to the text that spans
   --  between the first and the last tokens of this node.
   --
   --  Note that this returns the empty string for synthetic nodes.

   function Token_Range
     (Node : Turkixir_Node'Class) return Token_Iterator;
   --  Return an iterator on the range of tokens encompassed by Node

   


   -------------------
   -- Debug helpers --
   -------------------

   procedure Print
     (Node        : Turkixir_Node'Class;
      Show_Slocs  : Boolean := True;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children.
   --
   --  If Show_Slocs, include AST nodes' source locations in the output.
   --
   --  Line_Prefix is prepended to each output line.

   procedure PP_Trivia
     (Node        : Turkixir_Node'Class;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children along
   --  with the trivia associated to them. Line_Prefix is prepended to each
   --  output line.

   procedure Assign_Names_To_Logic_Vars (Node : Turkixir_Node'Class);
   --  Debug helper: Assign names to every logical variable in the root node,
   --  so that we can trace logical variables.

   --  The following As_* functions convert references to nodes from one type
   --  to another (Turkixir_Node can refer to any node type). They
   --  raise a Constraint_Error if the conversion is invalid.

   pragma Warnings (Off, "defined after private extension");
      function As_Turkixir_Node
        (Node : Turkixir_Node'Class) return Turkixir_Node;
      --% no-document: True
      function As_Expr
        (Node : Turkixir_Node'Class) return Expr;
      --% no-document: True
      function As_And_Expr
        (Node : Turkixir_Node'Class) return And_Expr;
      --% no-document: True
      function As_And_Op
        (Node : Turkixir_Node'Class) return And_Op;
      --% no-document: True
      function As_Arg
        (Node : Turkixir_Node'Class) return Arg;
      --% no-document: True
      function As_Arg_Assoc
        (Node : Turkixir_Node'Class) return Arg_Assoc;
      --% no-document: True
      function As_Arg_Gen
        (Node : Turkixir_Node'Class) return Arg_Gen;
      --% no-document: True
      function As_Turkixir_Node_Base_List
        (Node : Turkixir_Node'Class) return Turkixir_Node_Base_List;
      --% no-document: True
      function As_Arg_List
        (Node : Turkixir_Node'Class) return Arg_List;
      --% no-document: True
      function As_Bin_Op
        (Node : Turkixir_Node'Class) return Bin_Op;
      --% no-document: True
      function As_Arith_Expr
        (Node : Turkixir_Node'Class) return Arith_Expr;
      --% no-document: True
      function As_As_Name_Node
        (Node : Turkixir_Node'Class) return As_Name_Node;
      --% no-document: True
      function As_As_Name_Node_List
        (Node : Turkixir_Node'Class) return As_Name_Node_List;
      --% no-document: True
      function As_Stmt
        (Node : Turkixir_Node'Class) return Stmt;
      --% no-document: True
      function As_Assert_Stmt
        (Node : Turkixir_Node'Class) return Assert_Stmt;
      --% no-document: True
      function As_Assign_Stmt
        (Node : Turkixir_Node'Class) return Assign_Stmt;
      --% no-document: True
      function As_Aug_Assign_Stmt
        (Node : Turkixir_Node'Class) return Aug_Assign_Stmt;
      --% no-document: True
      function As_Break_Stmt
        (Node : Turkixir_Node'Class) return Break_Stmt;
      --% no-document: True
      function As_Call_Expr
        (Node : Turkixir_Node'Class) return Call_Expr;
      --% no-document: True
      function As_Def_Stmt
        (Node : Turkixir_Node'Class) return Def_Stmt;
      --% no-document: True
      function As_Class_Def
        (Node : Turkixir_Node'Class) return Class_Def;
      --% no-document: True
      function As_Comprehension
        (Node : Turkixir_Node'Class) return Comprehension;
      --% no-document: True
      function As_Comp_For
        (Node : Turkixir_Node'Class) return Comp_For;
      --% no-document: True
      function As_Comp_ForL
        (Node : Turkixir_Node'Class) return Comp_ForL;
      --% no-document: True
      function As_Comp_If
        (Node : Turkixir_Node'Class) return Comp_If;
      --% no-document: True
      function As_Comp_Op
        (Node : Turkixir_Node'Class) return Comp_Op;
      --% no-document: True
      function As_Comp_Op_Kind
        (Node : Turkixir_Node'Class) return Comp_Op_Kind;
      --% no-document: True
      function As_Comp_Op_Kind_Diamond
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Diamond;
      --% no-document: True
      function As_Comp_Op_Kind_Eq
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Eq;
      --% no-document: True
      function As_Comp_Op_Kind_Gt
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Gt;
      --% no-document: True
      function As_Comp_Op_Kind_Gte
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Gte;
      --% no-document: True
      function As_Comp_Op_Kind_In
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_In;
      --% no-document: True
      function As_Comp_Op_Kind_Is
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Is;
      --% no-document: True
      function As_Comp_Op_Kind_Isnot
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Isnot;
      --% no-document: True
      function As_Comp_Op_Kind_Lt
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Lt;
      --% no-document: True
      function As_Comp_Op_Kind_Lte
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Lte;
      --% no-document: True
      function As_Comp_Op_Kind_Noteq
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Noteq;
      --% no-document: True
      function As_Comp_Op_Kind_Notin
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Notin;
      --% no-document: True
      function As_Concat_String_Lit
        (Node : Turkixir_Node'Class) return Concat_String_Lit;
      --% no-document: True
      function As_Continue_Stmt
        (Node : Turkixir_Node'Class) return Continue_Stmt;
      --% no-document: True
      function As_Decorated
        (Node : Turkixir_Node'Class) return Decorated;
      --% no-document: True
      function As_Decorator
        (Node : Turkixir_Node'Class) return Decorator;
      --% no-document: True
      function As_Decorator_List
        (Node : Turkixir_Node'Class) return Decorator_List;
      --% no-document: True
      function As_Del_Stmt
        (Node : Turkixir_Node'Class) return Del_Stmt;
      --% no-document: True
      function As_Dict_Assoc
        (Node : Turkixir_Node'Class) return Dict_Assoc;
      --% no-document: True
      function As_Dict_Assoc_List
        (Node : Turkixir_Node'Class) return Dict_Assoc_List;
      --% no-document: True
      function As_Dict_Comp
        (Node : Turkixir_Node'Class) return Dict_Comp;
      --% no-document: True
      function As_Dict_Lit
        (Node : Turkixir_Node'Class) return Dict_Lit;
      --% no-document: True
      function As_Dot
        (Node : Turkixir_Node'Class) return Dot;
      --% no-document: True
      function As_Dot_List
        (Node : Turkixir_Node'Class) return Dot_List;
      --% no-document: True
      function As_Name
        (Node : Turkixir_Node'Class) return Name;
      --% no-document: True
      function As_Dotted_Name
        (Node : Turkixir_Node'Class) return Dotted_Name;
      --% no-document: True
      function As_Elif_Branch
        (Node : Turkixir_Node'Class) return Elif_Branch;
      --% no-document: True
      function As_Elif_Branch_List
        (Node : Turkixir_Node'Class) return Elif_Branch_List;
      --% no-document: True
      function As_Ellipsis_Expr
        (Node : Turkixir_Node'Class) return Ellipsis_Expr;
      --% no-document: True
      function As_Else_Part
        (Node : Turkixir_Node'Class) return Else_Part;
      --% no-document: True
      function As_Except_Part
        (Node : Turkixir_Node'Class) return Except_Part;
      --% no-document: True
      function As_Except_Part_List
        (Node : Turkixir_Node'Class) return Except_Part_List;
      --% no-document: True
      function As_Exec_Stmt
        (Node : Turkixir_Node'Class) return Exec_Stmt;
      --% no-document: True
      function As_Expr_List
        (Node : Turkixir_Node'Class) return Expr_List;
      --% no-document: True
      function As_Slice_Expr
        (Node : Turkixir_Node'Class) return Slice_Expr;
      --% no-document: True
      function As_Ext_Slice_Expr
        (Node : Turkixir_Node'Class) return Ext_Slice_Expr;
      --% no-document: True
      function As_Factor
        (Node : Turkixir_Node'Class) return Factor;
      --% no-document: True
      function As_File_Node
        (Node : Turkixir_Node'Class) return File_Node;
      --% no-document: True
      function As_For_Stmt
        (Node : Turkixir_Node'Class) return For_Stmt;
      --% no-document: True
      function As_Func_Def
        (Node : Turkixir_Node'Class) return Func_Def;
      --% no-document: True
      function As_Global_Stmt
        (Node : Turkixir_Node'Class) return Global_Stmt;
      --% no-document: True
      function As_Id
        (Node : Turkixir_Node'Class) return Id;
      --% no-document: True
      function As_Id_List
        (Node : Turkixir_Node'Class) return Id_List;
      --% no-document: True
      function As_If_Expr
        (Node : Turkixir_Node'Class) return If_Expr;
      --% no-document: True
      function As_If_Stmt
        (Node : Turkixir_Node'Class) return If_Stmt;
      --% no-document: True
      function As_Import_From
        (Node : Turkixir_Node'Class) return Import_From;
      --% no-document: True
      function As_Import_Name
        (Node : Turkixir_Node'Class) return Import_Name;
      --% no-document: True
      function As_Import_Star
        (Node : Turkixir_Node'Class) return Import_Star;
      --% no-document: True
      function As_Inline_Eval
        (Node : Turkixir_Node'Class) return Inline_Eval;
      --% no-document: True
      function As_Kw_Args
        (Node : Turkixir_Node'Class) return Kw_Args;
      --% no-document: True
      function As_Kw_Args_Flag
        (Node : Turkixir_Node'Class) return Kw_Args_Flag;
      --% no-document: True
      function As_Kw_Args_Flag_Absent
        (Node : Turkixir_Node'Class) return Kw_Args_Flag_Absent;
      --% no-document: True
      function As_Kw_Args_Flag_Present
        (Node : Turkixir_Node'Class) return Kw_Args_Flag_Present;
      --% no-document: True
      function As_Lambda_Def
        (Node : Turkixir_Node'Class) return Lambda_Def;
      --% no-document: True
      function As_List_Comp
        (Node : Turkixir_Node'Class) return List_Comp;
      --% no-document: True
      function As_List_Gen
        (Node : Turkixir_Node'Class) return List_Gen;
      --% no-document: True
      function As_List_Lit
        (Node : Turkixir_Node'Class) return List_Lit;
      --% no-document: True
      function As_NL
        (Node : Turkixir_Node'Class) return NL;
      --% no-document: True
      function As_NL_List
        (Node : Turkixir_Node'Class) return NL_List;
      --% no-document: True
      function As_Not_Op
        (Node : Turkixir_Node'Class) return Not_Op;
      --% no-document: True
      function As_Number_Lit
        (Node : Turkixir_Node'Class) return Number_Lit;
      --% no-document: True
      function As_Op
        (Node : Turkixir_Node'Class) return Op;
      --% no-document: True
      function As_Or_Expr
        (Node : Turkixir_Node'Class) return Or_Expr;
      --% no-document: True
      function As_Or_Op
        (Node : Turkixir_Node'Class) return Or_Op;
      --% no-document: True
      function As_Params
        (Node : Turkixir_Node'Class) return Params;
      --% no-document: True
      function As_Pass_Stmt
        (Node : Turkixir_Node'Class) return Pass_Stmt;
      --% no-document: True
      function As_Power
        (Node : Turkixir_Node'Class) return Power;
      --% no-document: True
      function As_Print_Stmt
        (Node : Turkixir_Node'Class) return Print_Stmt;
      --% no-document: True
      function As_Raise_Stmt
        (Node : Turkixir_Node'Class) return Raise_Stmt;
      --% no-document: True
      function As_Rel_Name
        (Node : Turkixir_Node'Class) return Rel_Name;
      --% no-document: True
      function As_Return_Stmt
        (Node : Turkixir_Node'Class) return Return_Stmt;
      --% no-document: True
      function As_Set_Comp
        (Node : Turkixir_Node'Class) return Set_Comp;
      --% no-document: True
      function As_Set_Lit
        (Node : Turkixir_Node'Class) return Set_Lit;
      --% no-document: True
      function As_Shift_Expr
        (Node : Turkixir_Node'Class) return Shift_Expr;
      --% no-document: True
      function As_Single_Param
        (Node : Turkixir_Node'Class) return Single_Param;
      --% no-document: True
      function As_Single_Param_List
        (Node : Turkixir_Node'Class) return Single_Param_List;
      --% no-document: True
      function As_Stream_Print_Stmt
        (Node : Turkixir_Node'Class) return Stream_Print_Stmt;
      --% no-document: True
      function As_String_Lit
        (Node : Turkixir_Node'Class) return String_Lit;
      --% no-document: True
      function As_String_Lit_List
        (Node : Turkixir_Node'Class) return String_Lit_List;
      --% no-document: True
      function As_Subscript_Expr
        (Node : Turkixir_Node'Class) return Subscript_Expr;
      --% no-document: True
      function As_Term
        (Node : Turkixir_Node'Class) return Term;
      --% no-document: True
      function As_Try_Stmt
        (Node : Turkixir_Node'Class) return Try_Stmt;
      --% no-document: True
      function As_Tuple_Lit
        (Node : Turkixir_Node'Class) return Tuple_Lit;
      --% no-document: True
      function As_Turkixir_Node_List
        (Node : Turkixir_Node'Class) return Turkixir_Node_List;
      --% no-document: True
      function As_Var_Args
        (Node : Turkixir_Node'Class) return Var_Args;
      --% no-document: True
      function As_Var_Args_Flag
        (Node : Turkixir_Node'Class) return Var_Args_Flag;
      --% no-document: True
      function As_Var_Args_Flag_Absent
        (Node : Turkixir_Node'Class) return Var_Args_Flag_Absent;
      --% no-document: True
      function As_Var_Args_Flag_Present
        (Node : Turkixir_Node'Class) return Var_Args_Flag_Present;
      --% no-document: True
      function As_While_Stmt
        (Node : Turkixir_Node'Class) return While_Stmt;
      --% no-document: True
      function As_With_Stmt
        (Node : Turkixir_Node'Class) return With_Stmt;
      --% no-document: True
      function As_Xor_Expr
        (Node : Turkixir_Node'Class) return Xor_Expr;
      --% no-document: True
      function As_Yield_Expr
        (Node : Turkixir_Node'Class) return Yield_Expr;
      --% no-document: True

   function Hash
     (Node : Turkixir_Node) return Ada.Containers.Hash_Type;
   --  Generic hash function, to be used for nodes as keys in hash tables
   pragma Warnings (On, "defined after private extension");

private

   type Internal_Context_Access is
      access all Implementation.Analysis_Context_Type;
   type Internal_Unit_Access is
      access all Implementation.Analysis_Unit_Type;

   type Analysis_Context is new Ada.Finalization.Controlled with record
      Internal : Internal_Context_Access;
   end record;

   overriding procedure Initialize (Context : in out Analysis_Context);
   overriding procedure Adjust (Context : in out Analysis_Context);
   overriding procedure Finalize (Context : in out Analysis_Context);

   type Analysis_Unit is new Langkit_Support.Text.Text_Buffer_Ifc with record
      Internal : Internal_Unit_Access;

      Context : Analysis_Context;
      --  Keep a reference to the owning context so that the context lives as
      --  long as there is at least one reference to one of its units.
   end record;

   No_Analysis_Context : constant Analysis_Context :=
     (Ada.Finalization.Controlled with Internal => null);
   No_Analysis_Unit    : constant Analysis_Unit :=
     (Internal => null,
      Context  => (Ada.Finalization.Controlled with Internal => null));

   --------------------------
   -- AST nodes (internal) --
   --------------------------

         type Turkixir_Node is tagged record
            Internal   : Implementation.AST_Envs.Entity;
            Safety_Net : Implementation.Node_Safety_Net;
         end record;
      No_Turkixir_Node : constant Turkixir_Node :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Expr is new Turkixir_Node with null record;
      No_Expr : constant Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type And_Expr is new Expr with null record;
      No_And_Expr : constant And_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type And_Op is new Expr with null record;
      No_And_Op : constant And_Op :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Arg is new Turkixir_Node with null record;
      No_Arg : constant Arg :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Arg_Assoc is new Arg with null record;
      No_Arg_Assoc : constant Arg_Assoc :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Arg_Gen is new Arg with null record;
      No_Arg_Gen : constant Arg_Gen :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Turkixir_Node_Base_List is new Turkixir_Node with null record;
      No_Turkixir_Node_Base_List : constant Turkixir_Node_Base_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Arg_List is new Turkixir_Node_Base_List with null record;
      No_Arg_List : constant Arg_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Bin_Op is new Expr with null record;
      No_Bin_Op : constant Bin_Op :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Arith_Expr is new Bin_Op with null record;
      No_Arith_Expr : constant Arith_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type As_Name_Node is new Turkixir_Node with null record;
      No_As_Name_Node : constant As_Name_Node :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type As_Name_Node_List is new Turkixir_Node_Base_List with null record;
      No_As_Name_Node_List : constant As_Name_Node_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Stmt is new Turkixir_Node with null record;
      No_Stmt : constant Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Assert_Stmt is new Stmt with null record;
      No_Assert_Stmt : constant Assert_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Assign_Stmt is new Stmt with null record;
      No_Assign_Stmt : constant Assign_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Aug_Assign_Stmt is new Stmt with null record;
      No_Aug_Assign_Stmt : constant Aug_Assign_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Break_Stmt is new Stmt with null record;
      No_Break_Stmt : constant Break_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Call_Expr is new Expr with null record;
      No_Call_Expr : constant Call_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Def_Stmt is new Stmt with null record;
      No_Def_Stmt : constant Def_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Class_Def is new Def_Stmt with null record;
      No_Class_Def : constant Class_Def :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comprehension is new Turkixir_Node with null record;
      No_Comprehension : constant Comprehension :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_For is new Comprehension with null record;
      No_Comp_For : constant Comp_For :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_ForL is new Comprehension with null record;
      No_Comp_ForL : constant Comp_ForL :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_If is new Turkixir_Node with null record;
      No_Comp_If : constant Comp_If :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_Op is new Expr with null record;
      No_Comp_Op : constant Comp_Op :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_Op_Kind is new Turkixir_Node with null record;
      No_Comp_Op_Kind : constant Comp_Op_Kind :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_Op_Kind_Diamond is new Comp_Op_Kind with null record;
      No_Comp_Op_Kind_Diamond : constant Comp_Op_Kind_Diamond :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_Op_Kind_Eq is new Comp_Op_Kind with null record;
      No_Comp_Op_Kind_Eq : constant Comp_Op_Kind_Eq :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_Op_Kind_Gt is new Comp_Op_Kind with null record;
      No_Comp_Op_Kind_Gt : constant Comp_Op_Kind_Gt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_Op_Kind_Gte is new Comp_Op_Kind with null record;
      No_Comp_Op_Kind_Gte : constant Comp_Op_Kind_Gte :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_Op_Kind_In is new Comp_Op_Kind with null record;
      No_Comp_Op_Kind_In : constant Comp_Op_Kind_In :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_Op_Kind_Is is new Comp_Op_Kind with null record;
      No_Comp_Op_Kind_Is : constant Comp_Op_Kind_Is :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_Op_Kind_Isnot is new Comp_Op_Kind with null record;
      No_Comp_Op_Kind_Isnot : constant Comp_Op_Kind_Isnot :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_Op_Kind_Lt is new Comp_Op_Kind with null record;
      No_Comp_Op_Kind_Lt : constant Comp_Op_Kind_Lt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_Op_Kind_Lte is new Comp_Op_Kind with null record;
      No_Comp_Op_Kind_Lte : constant Comp_Op_Kind_Lte :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_Op_Kind_Noteq is new Comp_Op_Kind with null record;
      No_Comp_Op_Kind_Noteq : constant Comp_Op_Kind_Noteq :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Comp_Op_Kind_Notin is new Comp_Op_Kind with null record;
      No_Comp_Op_Kind_Notin : constant Comp_Op_Kind_Notin :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Concat_String_Lit is new Expr with null record;
      No_Concat_String_Lit : constant Concat_String_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Continue_Stmt is new Stmt with null record;
      No_Continue_Stmt : constant Continue_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Decorated is new Stmt with null record;
      No_Decorated : constant Decorated :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Decorator is new Turkixir_Node with null record;
      No_Decorator : constant Decorator :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Decorator_List is new Turkixir_Node_Base_List with null record;
      No_Decorator_List : constant Decorator_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Del_Stmt is new Stmt with null record;
      No_Del_Stmt : constant Del_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Dict_Assoc is new Turkixir_Node with null record;
      No_Dict_Assoc : constant Dict_Assoc :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Dict_Assoc_List is new Turkixir_Node_Base_List with null record;
      No_Dict_Assoc_List : constant Dict_Assoc_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Dict_Comp is new Expr with null record;
      No_Dict_Comp : constant Dict_Comp :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Dict_Lit is new Expr with null record;
      No_Dict_Lit : constant Dict_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Dot is new Expr with null record;
      No_Dot : constant Dot :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Dot_List is new Turkixir_Node_Base_List with null record;
      No_Dot_List : constant Dot_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Name is new Expr with null record;
      No_Name : constant Name :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Dotted_Name is new Name with null record;
      No_Dotted_Name : constant Dotted_Name :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Elif_Branch is new Stmt with null record;
      No_Elif_Branch : constant Elif_Branch :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Elif_Branch_List is new Turkixir_Node_Base_List with null record;
      No_Elif_Branch_List : constant Elif_Branch_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ellipsis_Expr is new Expr with null record;
      No_Ellipsis_Expr : constant Ellipsis_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Else_Part is new Turkixir_Node with null record;
      No_Else_Part : constant Else_Part :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Except_Part is new Turkixir_Node with null record;
      No_Except_Part : constant Except_Part :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Except_Part_List is new Turkixir_Node_Base_List with null record;
      No_Except_Part_List : constant Except_Part_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Exec_Stmt is new Stmt with null record;
      No_Exec_Stmt : constant Exec_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Expr_List is new Turkixir_Node_Base_List with null record;
      No_Expr_List : constant Expr_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Slice_Expr is new Expr with null record;
      No_Slice_Expr : constant Slice_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ext_Slice_Expr is new Slice_Expr with null record;
      No_Ext_Slice_Expr : constant Ext_Slice_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Factor is new Expr with null record;
      No_Factor : constant Factor :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type File_Node is new Turkixir_Node with null record;
      No_File_Node : constant File_Node :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type For_Stmt is new Stmt with null record;
      No_For_Stmt : constant For_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Func_Def is new Def_Stmt with null record;
      No_Func_Def : constant Func_Def :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Global_Stmt is new Stmt with null record;
      No_Global_Stmt : constant Global_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Id is new Name with null record;
      No_Id : constant Id :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Id_List is new Turkixir_Node_Base_List with null record;
      No_Id_List : constant Id_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type If_Expr is new Expr with null record;
      No_If_Expr : constant If_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type If_Stmt is new Stmt with null record;
      No_If_Stmt : constant If_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Import_From is new Stmt with null record;
      No_Import_From : constant Import_From :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Import_Name is new Stmt with null record;
      No_Import_Name : constant Import_Name :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Import_Star is new Turkixir_Node with null record;
      No_Import_Star : constant Import_Star :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Inline_Eval is new Expr with null record;
      No_Inline_Eval : constant Inline_Eval :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Kw_Args is new Arg with null record;
      No_Kw_Args : constant Kw_Args :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Kw_Args_Flag is new Turkixir_Node with null record;
      No_Kw_Args_Flag : constant Kw_Args_Flag :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Kw_Args_Flag_Absent is new Kw_Args_Flag with null record;
      No_Kw_Args_Flag_Absent : constant Kw_Args_Flag_Absent :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Kw_Args_Flag_Present is new Kw_Args_Flag with null record;
      No_Kw_Args_Flag_Present : constant Kw_Args_Flag_Present :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Lambda_Def is new Expr with null record;
      No_Lambda_Def : constant Lambda_Def :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type List_Comp is new Expr with null record;
      No_List_Comp : constant List_Comp :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type List_Gen is new Expr with null record;
      No_List_Gen : constant List_Gen :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type List_Lit is new Expr with null record;
      No_List_Lit : constant List_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type NL is new Turkixir_Node with null record;
      No_NL : constant NL :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type NL_List is new Turkixir_Node_Base_List with null record;
      No_NL_List : constant NL_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Not_Op is new Expr with null record;
      No_Not_Op : constant Not_Op :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Number_Lit is new Expr with null record;
      No_Number_Lit : constant Number_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op is new Turkixir_Node with null record;
      No_Op : constant Op :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Or_Expr is new Expr with null record;
      No_Or_Expr : constant Or_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Or_Op is new Expr with null record;
      No_Or_Op : constant Or_Op :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Params is new Turkixir_Node with null record;
      No_Params : constant Params :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Pass_Stmt is new Stmt with null record;
      No_Pass_Stmt : constant Pass_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Power is new Expr with null record;
      No_Power : constant Power :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Print_Stmt is new Stmt with null record;
      No_Print_Stmt : constant Print_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Raise_Stmt is new Stmt with null record;
      No_Raise_Stmt : constant Raise_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Rel_Name is new Turkixir_Node with null record;
      No_Rel_Name : constant Rel_Name :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Return_Stmt is new Stmt with null record;
      No_Return_Stmt : constant Return_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Set_Comp is new Expr with null record;
      No_Set_Comp : constant Set_Comp :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Set_Lit is new Expr with null record;
      No_Set_Lit : constant Set_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Shift_Expr is new Bin_Op with null record;
      No_Shift_Expr : constant Shift_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Single_Param is new Turkixir_Node with null record;
      No_Single_Param : constant Single_Param :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Single_Param_List is new Turkixir_Node_Base_List with null record;
      No_Single_Param_List : constant Single_Param_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Stream_Print_Stmt is new Stmt with null record;
      No_Stream_Print_Stmt : constant Stream_Print_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type String_Lit is new Expr with null record;
      No_String_Lit : constant String_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type String_Lit_List is new Turkixir_Node_Base_List with null record;
      No_String_Lit_List : constant String_Lit_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Subscript_Expr is new Expr with null record;
      No_Subscript_Expr : constant Subscript_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Term is new Bin_Op with null record;
      No_Term : constant Term :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Try_Stmt is new Stmt with null record;
      No_Try_Stmt : constant Try_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Tuple_Lit is new Expr with null record;
      No_Tuple_Lit : constant Tuple_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Turkixir_Node_List is new Turkixir_Node_Base_List with null record;
      No_Turkixir_Node_List : constant Turkixir_Node_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Var_Args is new Arg with null record;
      No_Var_Args : constant Var_Args :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Var_Args_Flag is new Turkixir_Node with null record;
      No_Var_Args_Flag : constant Var_Args_Flag :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Var_Args_Flag_Absent is new Var_Args_Flag with null record;
      No_Var_Args_Flag_Absent : constant Var_Args_Flag_Absent :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Var_Args_Flag_Present is new Var_Args_Flag with null record;
      No_Var_Args_Flag_Present : constant Var_Args_Flag_Present :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type While_Stmt is new Stmt with null record;
      No_While_Stmt : constant While_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type With_Stmt is new Stmt with null record;
      No_With_Stmt : constant With_Stmt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Xor_Expr is new Expr with null record;
      No_Xor_Expr : constant Xor_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Yield_Expr is new Expr with null record;
      No_Yield_Expr : constant Yield_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);

   procedure Check_Safety_Net (Self : Turkixir_Node'Class);
   --  Check that Self's node and rebindings are still valid, raising a
   --  Stale_Reference_Error if one is not.

   --------------------------------
   -- Token Iterator (internals) --
   --------------------------------

   type Token_Iterator is record
      Node : Turkixir_Node;
      Last : Token_Index;
   end record;

   ---------------------------------
   -- Composite types (internals) --
   ---------------------------------

            


   --  The dummy references to these packages forces them to be included in
   --  statically linked builds (thanks to the binder). This benefits the GDB
   --  helpers at no cost.

   Version : String renames Libturkixirlang.Version;
   procedure RN (Node : Libturkixirlang.Implementation.Bare_Turkixir_Node)
      renames Libturkixirlang.Debug.PN;

end Libturkixirlang.Analysis;
