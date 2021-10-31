









with Ada.Containers;              use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System; use System;

   with GNATCOLL.Traces;
with GNATCOLL.GMP.Integers;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Eq_Same;
with Langkit_Support.Bump_Ptr;     use Langkit_Support.Bump_Ptr;
with Langkit_Support.Cheap_Sets;
with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;
with Langkit_Support.Lexical_Envs; use Langkit_Support.Lexical_Envs;
with Langkit_Support.Lexical_Envs_Impl;
with Langkit_Support.Symbols;      use Langkit_Support.Symbols;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;
with Langkit_Support.Types;        use Langkit_Support.Types;
with Langkit_Support.Vectors;

with Libturkixirlang.Parsers; use Libturkixirlang.Parsers;
with Libturkixirlang.Common;  use Libturkixirlang.Common;
with Libturkixirlang.Lexer_Implementation;
use Libturkixirlang.Lexer_Implementation;




--  Internal package: low-level primitives to implement public types and
--  operations in Libturkixirlang.Analysis.

private package Libturkixirlang.Implementation is

   use Support.Diagnostics, Support.Slocs, Support.Text;

   type Analysis_Context_Type;
   type Internal_Context is access all Analysis_Context_Type;

   Unexpected_Call_Depth : exception;
   --  Raised when the Call_Depth for two matching calls to Enter_Call and
   --  Exit_Call don't match, i.e. when there is a bug in the counting of
   --  recursive calls.

   procedure Enter_Call
     (Context : Internal_Context; Call_Depth : access Natural);
   --  Increment the call depth in Context. If the depth exceeds Context's
   --  maximum, raise a Property_Error for "stack overflow".
   --
   --  Note that in the case of an exception, the depth is still incremented.
   --  This means that all calls to Enter_Call must be wrapped in an exception
   --  handler which calls Exit_Call on exception.
   --
   --  Put in Call_Depth the incremented call depth.

   procedure Exit_Call (Context : Internal_Context; Call_Depth : Natural);
   --  Decrement the call depth in Context. If Call_Depth does not match the
   --  current call depth, raise an Unexpected_Call_Depth.

   type Analysis_Unit_Type;
   type Internal_Unit is access all Analysis_Unit_Type;

   type Root_Node_Record;
   type Bare_Turkixir_Node is access all Root_Node_Record;
   No_Bare_Turkixir_Node : constant Bare_Turkixir_Node := null;
   --  Most generic AST node type

   function "<" (Left, Right : Bare_Turkixir_Node) return Boolean;
   --  Abritrary but deterministic ordering criteria for parsing nodes. This
   --  handles null nodes as well. Raise a Property_Error for synthetic nodes.

   function Is_Null (Node : Bare_Turkixir_Node) return Boolean;
   function Kind (Node : Bare_Turkixir_Node) return Turkixir_Node_Kind_Type;

         subtype Bare_Arg is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Arg)
               or else Kind (Bare_Arg) in Turkixir_Arg;
         subtype Bare_Arg_Assoc is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Arg_Assoc)
               or else Kind (Bare_Arg_Assoc) in Turkixir_Arg_Assoc_Range;
         subtype Bare_Arg_Gen is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Arg_Gen)
               or else Kind (Bare_Arg_Gen) in Turkixir_Arg_Gen_Range;
         subtype Bare_Kw_Args is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Kw_Args)
               or else Kind (Bare_Kw_Args) in Turkixir_Kw_Args_Range;
         subtype Bare_Var_Args is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Var_Args)
               or else Kind (Bare_Var_Args) in Turkixir_Var_Args_Range;
         subtype Bare_As_Name_Node is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_As_Name_Node)
               or else Kind (Bare_As_Name_Node) in Turkixir_As_Name_Node_Range;
         subtype Bare_Comp_If is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_If)
               or else Kind (Bare_Comp_If) in Turkixir_Comp_If_Range;
         subtype Bare_Comp_Op_Kind is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_Op_Kind)
               or else Kind (Bare_Comp_Op_Kind) in Turkixir_Comp_Op_Kind;
         subtype Bare_Comp_Op_Kind_Diamond is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_Op_Kind_Diamond)
               or else Kind (Bare_Comp_Op_Kind_Diamond) in Turkixir_Comp_Op_Kind_Diamond_Range;
         subtype Bare_Comp_Op_Kind_Eq is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_Op_Kind_Eq)
               or else Kind (Bare_Comp_Op_Kind_Eq) in Turkixir_Comp_Op_Kind_Eq_Range;
         subtype Bare_Comp_Op_Kind_Gt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_Op_Kind_Gt)
               or else Kind (Bare_Comp_Op_Kind_Gt) in Turkixir_Comp_Op_Kind_Gt_Range;
         subtype Bare_Comp_Op_Kind_Gte is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_Op_Kind_Gte)
               or else Kind (Bare_Comp_Op_Kind_Gte) in Turkixir_Comp_Op_Kind_Gte_Range;
         subtype Bare_Comp_Op_Kind_In is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_Op_Kind_In)
               or else Kind (Bare_Comp_Op_Kind_In) in Turkixir_Comp_Op_Kind_In_Range;
         subtype Bare_Comp_Op_Kind_Is is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_Op_Kind_Is)
               or else Kind (Bare_Comp_Op_Kind_Is) in Turkixir_Comp_Op_Kind_Is_Range;
         subtype Bare_Comp_Op_Kind_Isnot is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_Op_Kind_Isnot)
               or else Kind (Bare_Comp_Op_Kind_Isnot) in Turkixir_Comp_Op_Kind_Isnot_Range;
         subtype Bare_Comp_Op_Kind_Lt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_Op_Kind_Lt)
               or else Kind (Bare_Comp_Op_Kind_Lt) in Turkixir_Comp_Op_Kind_Lt_Range;
         subtype Bare_Comp_Op_Kind_Lte is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_Op_Kind_Lte)
               or else Kind (Bare_Comp_Op_Kind_Lte) in Turkixir_Comp_Op_Kind_Lte_Range;
         subtype Bare_Comp_Op_Kind_Noteq is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_Op_Kind_Noteq)
               or else Kind (Bare_Comp_Op_Kind_Noteq) in Turkixir_Comp_Op_Kind_Noteq_Range;
         subtype Bare_Comp_Op_Kind_Notin is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_Op_Kind_Notin)
               or else Kind (Bare_Comp_Op_Kind_Notin) in Turkixir_Comp_Op_Kind_Notin_Range;
         subtype Bare_Comprehension is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comprehension)
               or else Kind (Bare_Comprehension) in Turkixir_Comprehension;
         subtype Bare_Comp_For is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_For)
               or else Kind (Bare_Comp_For) in Turkixir_Comp_For_Range;
         subtype Bare_Comp_ForL is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_ForL)
               or else Kind (Bare_Comp_ForL) in Turkixir_Comp_ForL_Range;
         subtype Bare_Decorator is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Decorator)
               or else Kind (Bare_Decorator) in Turkixir_Decorator_Range;
         subtype Bare_Dict_Assoc is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Dict_Assoc)
               or else Kind (Bare_Dict_Assoc) in Turkixir_Dict_Assoc_Range;
         subtype Bare_Else_Part is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Else_Part)
               or else Kind (Bare_Else_Part) in Turkixir_Else_Part_Range;
         subtype Bare_Except_Part is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Except_Part)
               or else Kind (Bare_Except_Part) in Turkixir_Except_Part_Range;
         subtype Bare_Expr is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Expr)
               or else Kind (Bare_Expr) in Turkixir_Expr;
         subtype Bare_And_Expr is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_And_Expr)
               or else Kind (Bare_And_Expr) in Turkixir_And_Expr_Range;
         subtype Bare_And_Op is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_And_Op)
               or else Kind (Bare_And_Op) in Turkixir_And_Op_Range;
         subtype Bare_Bin_Op is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Bin_Op)
               or else Kind (Bare_Bin_Op) in Turkixir_Bin_Op;
         subtype Bare_Arith_Expr is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Arith_Expr)
               or else Kind (Bare_Arith_Expr) in Turkixir_Arith_Expr_Range;
         subtype Bare_Shift_Expr is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Shift_Expr)
               or else Kind (Bare_Shift_Expr) in Turkixir_Shift_Expr_Range;
         subtype Bare_Term is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Term)
               or else Kind (Bare_Term) in Turkixir_Term_Range;
         subtype Bare_Call_Expr is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Call_Expr)
               or else Kind (Bare_Call_Expr) in Turkixir_Call_Expr_Range;
         subtype Bare_Comp_Op is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Comp_Op)
               or else Kind (Bare_Comp_Op) in Turkixir_Comp_Op_Range;
         subtype Bare_Concat_String_Lit is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Concat_String_Lit)
               or else Kind (Bare_Concat_String_Lit) in Turkixir_Concat_String_Lit_Range;
         subtype Bare_Dict_Comp is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Dict_Comp)
               or else Kind (Bare_Dict_Comp) in Turkixir_Dict_Comp_Range;
         subtype Bare_Dict_Lit is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Dict_Lit)
               or else Kind (Bare_Dict_Lit) in Turkixir_Dict_Lit_Range;
         subtype Bare_Dot is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Dot)
               or else Kind (Bare_Dot) in Turkixir_Dot_Range;
         subtype Bare_Ellipsis_Expr is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ellipsis_Expr)
               or else Kind (Bare_Ellipsis_Expr) in Turkixir_Ellipsis_Expr_Range;
         subtype Bare_Factor is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Factor)
               or else Kind (Bare_Factor) in Turkixir_Factor_Range;
         subtype Bare_If_Expr is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_If_Expr)
               or else Kind (Bare_If_Expr) in Turkixir_If_Expr_Range;
         subtype Bare_Inline_Eval is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Inline_Eval)
               or else Kind (Bare_Inline_Eval) in Turkixir_Inline_Eval_Range;
         subtype Bare_Lambda_Def is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Lambda_Def)
               or else Kind (Bare_Lambda_Def) in Turkixir_Lambda_Def_Range;
         subtype Bare_List_Comp is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_List_Comp)
               or else Kind (Bare_List_Comp) in Turkixir_List_Comp_Range;
         subtype Bare_List_Gen is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_List_Gen)
               or else Kind (Bare_List_Gen) in Turkixir_List_Gen_Range;
         subtype Bare_List_Lit is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_List_Lit)
               or else Kind (Bare_List_Lit) in Turkixir_List_Lit_Range;
         subtype Bare_Name is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Name)
               or else Kind (Bare_Name) in Turkixir_Name;
         subtype Bare_Dotted_Name is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Dotted_Name)
               or else Kind (Bare_Dotted_Name) in Turkixir_Dotted_Name_Range;
         subtype Bare_Id is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Id)
               or else Kind (Bare_Id) in Turkixir_Id_Range;
         subtype Bare_Not_Op is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Not_Op)
               or else Kind (Bare_Not_Op) in Turkixir_Not_Op_Range;
         subtype Bare_Number_Lit is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Number_Lit)
               or else Kind (Bare_Number_Lit) in Turkixir_Number_Lit_Range;
         subtype Bare_Or_Expr is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Or_Expr)
               or else Kind (Bare_Or_Expr) in Turkixir_Or_Expr_Range;
         subtype Bare_Or_Op is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Or_Op)
               or else Kind (Bare_Or_Op) in Turkixir_Or_Op_Range;
         subtype Bare_Power is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Power)
               or else Kind (Bare_Power) in Turkixir_Power_Range;
         subtype Bare_Set_Comp is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Set_Comp)
               or else Kind (Bare_Set_Comp) in Turkixir_Set_Comp_Range;
         subtype Bare_Set_Lit is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Set_Lit)
               or else Kind (Bare_Set_Lit) in Turkixir_Set_Lit_Range;
         subtype Bare_Slice_Expr is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Slice_Expr)
               or else Kind (Bare_Slice_Expr) in Turkixir_Slice_Expr_Range;
         subtype Bare_Ext_Slice_Expr is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ext_Slice_Expr)
               or else Kind (Bare_Ext_Slice_Expr) in Turkixir_Ext_Slice_Expr_Range;
         subtype Bare_String_Lit is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_String_Lit)
               or else Kind (Bare_String_Lit) in Turkixir_String_Lit_Range;
         subtype Bare_Subscript_Expr is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Subscript_Expr)
               or else Kind (Bare_Subscript_Expr) in Turkixir_Subscript_Expr_Range;
         subtype Bare_Tuple_Lit is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Tuple_Lit)
               or else Kind (Bare_Tuple_Lit) in Turkixir_Tuple_Lit_Range;
         subtype Bare_Xor_Expr is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Xor_Expr)
               or else Kind (Bare_Xor_Expr) in Turkixir_Xor_Expr_Range;
         subtype Bare_Yield_Expr is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Yield_Expr)
               or else Kind (Bare_Yield_Expr) in Turkixir_Yield_Expr_Range;
         subtype Bare_File_Node is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_File_Node)
               or else Kind (Bare_File_Node) in Turkixir_File_Node_Range;
         subtype Bare_Import_Star is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Import_Star)
               or else Kind (Bare_Import_Star) in Turkixir_Import_Star_Range;
         subtype Bare_Kw_Args_Flag is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Kw_Args_Flag)
               or else Kind (Bare_Kw_Args_Flag) in Turkixir_Kw_Args_Flag;
         subtype Bare_Kw_Args_Flag_Absent is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Kw_Args_Flag_Absent)
               or else Kind (Bare_Kw_Args_Flag_Absent) in Turkixir_Kw_Args_Flag_Absent_Range;
         subtype Bare_Kw_Args_Flag_Present is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Kw_Args_Flag_Present)
               or else Kind (Bare_Kw_Args_Flag_Present) in Turkixir_Kw_Args_Flag_Present_Range;
         subtype Bare_NL is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_NL)
               or else Kind (Bare_NL) in Turkixir_NL_Range;
         subtype Bare_Op is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op)
               or else Kind (Bare_Op) in Turkixir_Op_Range;
         subtype Bare_Params is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Params)
               or else Kind (Bare_Params) in Turkixir_Params_Range;
         subtype Bare_Rel_Name is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Rel_Name)
               or else Kind (Bare_Rel_Name) in Turkixir_Rel_Name_Range;
         subtype Bare_Single_Param is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Single_Param)
               or else Kind (Bare_Single_Param) in Turkixir_Single_Param_Range;
         subtype Bare_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Stmt)
               or else Kind (Bare_Stmt) in Turkixir_Stmt;
         subtype Bare_Assert_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Assert_Stmt)
               or else Kind (Bare_Assert_Stmt) in Turkixir_Assert_Stmt_Range;
         subtype Bare_Assign_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Assign_Stmt)
               or else Kind (Bare_Assign_Stmt) in Turkixir_Assign_Stmt_Range;
         subtype Bare_Aug_Assign_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Aug_Assign_Stmt)
               or else Kind (Bare_Aug_Assign_Stmt) in Turkixir_Aug_Assign_Stmt_Range;
         subtype Bare_Break_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Break_Stmt)
               or else Kind (Bare_Break_Stmt) in Turkixir_Break_Stmt_Range;
         subtype Bare_Continue_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Continue_Stmt)
               or else Kind (Bare_Continue_Stmt) in Turkixir_Continue_Stmt_Range;
         subtype Bare_Decorated is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Decorated)
               or else Kind (Bare_Decorated) in Turkixir_Decorated_Range;
         subtype Bare_Def_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Def_Stmt)
               or else Kind (Bare_Def_Stmt) in Turkixir_Def_Stmt;
         subtype Bare_Class_Def is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Class_Def)
               or else Kind (Bare_Class_Def) in Turkixir_Class_Def_Range;
         subtype Bare_Func_Def is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Func_Def)
               or else Kind (Bare_Func_Def) in Turkixir_Func_Def_Range;
         subtype Bare_Del_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Del_Stmt)
               or else Kind (Bare_Del_Stmt) in Turkixir_Del_Stmt_Range;
         subtype Bare_Elif_Branch is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Elif_Branch)
               or else Kind (Bare_Elif_Branch) in Turkixir_Elif_Branch_Range;
         subtype Bare_Exec_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Exec_Stmt)
               or else Kind (Bare_Exec_Stmt) in Turkixir_Exec_Stmt_Range;
         subtype Bare_For_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_For_Stmt)
               or else Kind (Bare_For_Stmt) in Turkixir_For_Stmt_Range;
         subtype Bare_Global_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Global_Stmt)
               or else Kind (Bare_Global_Stmt) in Turkixir_Global_Stmt_Range;
         subtype Bare_If_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_If_Stmt)
               or else Kind (Bare_If_Stmt) in Turkixir_If_Stmt_Range;
         subtype Bare_Import_From is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Import_From)
               or else Kind (Bare_Import_From) in Turkixir_Import_From_Range;
         subtype Bare_Import_Name is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Import_Name)
               or else Kind (Bare_Import_Name) in Turkixir_Import_Name_Range;
         subtype Bare_Pass_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Pass_Stmt)
               or else Kind (Bare_Pass_Stmt) in Turkixir_Pass_Stmt_Range;
         subtype Bare_Print_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Print_Stmt)
               or else Kind (Bare_Print_Stmt) in Turkixir_Print_Stmt_Range;
         subtype Bare_Raise_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Raise_Stmt)
               or else Kind (Bare_Raise_Stmt) in Turkixir_Raise_Stmt_Range;
         subtype Bare_Return_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Return_Stmt)
               or else Kind (Bare_Return_Stmt) in Turkixir_Return_Stmt_Range;
         subtype Bare_Stream_Print_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Stream_Print_Stmt)
               or else Kind (Bare_Stream_Print_Stmt) in Turkixir_Stream_Print_Stmt_Range;
         subtype Bare_Try_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Try_Stmt)
               or else Kind (Bare_Try_Stmt) in Turkixir_Try_Stmt_Range;
         subtype Bare_While_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_While_Stmt)
               or else Kind (Bare_While_Stmt) in Turkixir_While_Stmt_Range;
         subtype Bare_With_Stmt is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_With_Stmt)
               or else Kind (Bare_With_Stmt) in Turkixir_With_Stmt_Range;
         subtype Bare_Turkixir_Node_Base_List is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Turkixir_Node_Base_List)
               or else Kind (Bare_Turkixir_Node_Base_List) in Turkixir_Turkixir_Node_Base_List;
         subtype Bare_Arg_List is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Arg_List)
               or else Kind (Bare_Arg_List) in Turkixir_Arg_List_Range;
         subtype Bare_As_Name_Node_List is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_As_Name_Node_List)
               or else Kind (Bare_As_Name_Node_List) in Turkixir_As_Name_Node_List_Range;
         subtype Bare_Decorator_List is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Decorator_List)
               or else Kind (Bare_Decorator_List) in Turkixir_Decorator_List_Range;
         subtype Bare_Dict_Assoc_List is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Dict_Assoc_List)
               or else Kind (Bare_Dict_Assoc_List) in Turkixir_Dict_Assoc_List_Range;
         subtype Bare_Dot_List is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Dot_List)
               or else Kind (Bare_Dot_List) in Turkixir_Dot_List_Range;
         subtype Bare_Elif_Branch_List is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Elif_Branch_List)
               or else Kind (Bare_Elif_Branch_List) in Turkixir_Elif_Branch_List_Range;
         subtype Bare_Except_Part_List is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Except_Part_List)
               or else Kind (Bare_Except_Part_List) in Turkixir_Except_Part_List_Range;
         subtype Bare_Expr_List is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Expr_List)
               or else Kind (Bare_Expr_List) in Turkixir_Expr_List_Range;
         subtype Bare_Id_List is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Id_List)
               or else Kind (Bare_Id_List) in Turkixir_Id_List_Range;
         subtype Bare_NL_List is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_NL_List)
               or else Kind (Bare_NL_List) in Turkixir_NL_List_Range;
         subtype Bare_Single_Param_List is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Single_Param_List)
               or else Kind (Bare_Single_Param_List) in Turkixir_Single_Param_List_Range;
         subtype Bare_String_Lit_List is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_String_Lit_List)
               or else Kind (Bare_String_Lit_List) in Turkixir_String_Lit_List_Range;
         subtype Bare_Turkixir_Node_List is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Turkixir_Node_List)
               or else Kind (Bare_Turkixir_Node_List) in Turkixir_Turkixir_Node_List_Range;
         subtype Bare_Var_Args_Flag is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Var_Args_Flag)
               or else Kind (Bare_Var_Args_Flag) in Turkixir_Var_Args_Flag;
         subtype Bare_Var_Args_Flag_Absent is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Var_Args_Flag_Absent)
               or else Kind (Bare_Var_Args_Flag_Absent) in Turkixir_Var_Args_Flag_Absent_Range;
         subtype Bare_Var_Args_Flag_Present is Bare_Turkixir_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Var_Args_Flag_Present)
               or else Kind (Bare_Var_Args_Flag_Present) in Turkixir_Var_Args_Flag_Present_Range;

   package Alloc_AST_List_Array is new Langkit_Support.Bump_Ptr.Array_Alloc
     (Element_T  => Bare_Turkixir_Node,
      Index_Type => Positive);
   --  Allocator for array of nodes, used in list nodes

   type Rewriting_Handle_Pointer is new System.Address;
   No_Rewriting_Handle_Pointer : constant Rewriting_Handle_Pointer :=
      Rewriting_Handle_Pointer (System.Null_Address);

      Properties_Traces : constant GNATCOLL.Traces.Trace_Handle :=
         GNATCOLL.Traces.Create
           ("LANGKIT.PROPERTIES", GNATCOLL.Traces.On
           );

   function Short_Text_Image (Self : Bare_Turkixir_Node) return Text_Type;
   --  Return a short representation of the node, containing just the kind
   --  name and the sloc, or "None" if Self is null.

   function Is_Token_Node (Node : Bare_Turkixir_Node) return Boolean;
   --  Return whether this node is a node that contains only a single token.

   function Is_Synthetic (Node : Bare_Turkixir_Node) return Boolean;
   --  Return whether this node is synthetic.

   ---------------------------
   -- Iterators safety nets --
   ---------------------------

   type Iterator_Safety_Net is record
      Context         : Internal_Context;
      Context_Serial  : Version_Number;
      Context_Version : Version_Number;
      --  Analysis context, its serial number and version number at the time
      --  this safety net was produced.
   end record;

   No_Iterator_Safety_Net : constant Iterator_Safety_Net := (null, 0, 0);

   function Create_Safety_Net
     (Context : Internal_Context) return Iterator_Safety_Net;
   --  Create an iterator safety net from the given Context

   procedure Check_Safety_Net (Self : Iterator_Safety_Net);
   --  Check that the given iterator safety net is still valid, raising a
   --  Stale_Reference_Error if it is not.

   -----------------
   -- String type --
   -----------------

   type String_Record (Length : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Content : Text_Type (1 .. Length);
   end record;

   type String_Type is access all String_Record;

   Empty_String_Record : aliased String_Record :=
     (Length => 0, Ref_Count => -1, Content => (others => <>));
   Empty_String        : constant String_Type := Empty_String_Record'Access;

   procedure Inc_Ref (Self : String_Type);
   procedure Dec_Ref (Self : in out String_Type);
   procedure Free is new Ada.Unchecked_Deallocation
     (String_Record, String_Type);

   function Create_String (Content : Text_Type) return String_Type;
   function Create_String (Content : Unbounded_Text_Type) return String_Type;
   --  Create string values from their content. The overload for unbounded
   --  strings makes it easier for callers to avoid using the secondary stack,
   --  which can be a problem for big strings.

   function Concat_String (Left, Right : String_Type) return String_Type;
   --  Return a new string that is the concatenation of ``Left`` and ``Right``

   function Equivalent (Left, Right : String_Type) return Boolean;
   --  Return whether ``Left`` and ``Right`` contain equal strings

   ---------------------------
   -- Environments handling --
   ---------------------------

   
      type Internal_Metadata;
      

   

      

      type Internal_Metadata is record

            null;
      end record
        with Convention => C;




   
      function Hash (R : Internal_Metadata) return Hash_Type;


      function Trace_Image (R : Internal_Metadata) return String;


   
      


      No_Metadata : constant Internal_Metadata :=
      (null record);


   
      type Internal_Inner_Env_Assoc;
      

   

      

      type Internal_Inner_Env_Assoc is record

               Key : aliased Symbol_Type;
               
               
               Val : aliased Bare_Turkixir_Node;
               
               
               Metadata : aliased Internal_Metadata;
               
               
      end record
        with Convention => C;




   


      function Trace_Image (R : Internal_Inner_Env_Assoc) return String;


   
      


      No_Inner_Env_Assoc : constant Internal_Inner_Env_Assoc :=
      (
               Key => null, 
               Val => No_Bare_Turkixir_Node, 
               Metadata => No_Metadata
      );

   function Get_Key (Self : Internal_Inner_Env_Assoc) return Symbol_Type
   is (Self.Key);
   function Get_Node
     (Self : Internal_Inner_Env_Assoc) return Bare_Turkixir_Node
   is (Self.Val);
   function Get_Metadata
     (Self : Internal_Inner_Env_Assoc) return Internal_Metadata
   is (Self.Metadata);

   
   type Internal_Inner_Env_Assoc_Array_Record;
   type Internal_Inner_Env_Assoc_Array_Access is access all Internal_Inner_Env_Assoc_Array_Record;

      
   type Internal_Internal_Inner_Env_Assoc_Iterator;
   type Internal_Inner_Env_Assoc_Iterator_Access is access all Internal_Internal_Inner_Env_Assoc_Iterator;


   

   

   type Internal_Internal_Inner_Env_Assoc_Array is
      array (Positive range <>) of Internal_Inner_Env_Assoc;

   type Internal_Inner_Env_Assoc_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Inner_Env_Assoc_Array (1 .. N);
   end record;

   Empty_Internal_Inner_Env_Assoc_Array_Record : aliased Internal_Inner_Env_Assoc_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Inner_Env_Assoc_Array_Type : constant Internal_Inner_Env_Assoc_Array_Access :=
      Empty_Internal_Inner_Env_Assoc_Array_Record'Access;


   function Create_Internal_Inner_Env_Assoc_Array (Items_Count : Natural) return Internal_Inner_Env_Assoc_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Inner_Env_Assoc_Array
     (Items : Internal_Internal_Inner_Env_Assoc_Array) return Internal_Inner_Env_Assoc_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Inner_Env_Assoc_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Inner_Env_Assoc;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Inner_Env_Assoc_Array_Access) return Internal_Inner_Env_Assoc_Array_Access;


   function Length (T : Internal_Inner_Env_Assoc_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Inner_Env_Assoc_Array_Access);
   procedure Dec_Ref (T : in out Internal_Inner_Env_Assoc_Array_Access);

   function Equivalent (L, R : Internal_Inner_Env_Assoc_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Inner_Env_Assoc_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Inner_Env_Assoc_Array_Record, Internal_Inner_Env_Assoc_Array_Access);

      

   

   type Internal_Internal_Inner_Env_Assoc_Iterator is record
      Ref_Count : Integer;
      --  Reference count. The iterator is freed when this drops to zero.
      --  Negative values are interpreted as "always living singleton".

      Safety_Net : Iterator_Safety_Net;
      --  Safety net for the iterator. Used to check that values produced by
      --  the iterator are still valid. Unlike for other types, we put the
      --  safety net in the internal type so that it can be used in all other
      --  APIs (Python, ...).
      --
      --  While other types (except nodes) are "deeply" converted to native
      --  APIs (for instance: internal arrays are turned into native Python
      --  lists, likewise for array items, etc.), iterators are lazy, so the
      --  deep conversion is not possible.

      Elements : Internal_Inner_Env_Assoc_Array_Access;
      Index    : Positive;
   end record;

   Empty_Internal_Internal_Inner_Env_Assoc_Iterator : aliased Internal_Internal_Inner_Env_Assoc_Iterator :=
     (Ref_Count  => -1,
      Safety_Net => No_Iterator_Safety_Net,
      Elements   => No_Internal_Inner_Env_Assoc_Array_Type,
      Index      => 1);
   No_Internal_Inner_Env_Assoc_Iterator_Type : constant Internal_Inner_Env_Assoc_Iterator_Access :=
      Empty_Internal_Internal_Inner_Env_Assoc_Iterator'Access;

   function Next
     (T       : Internal_Inner_Env_Assoc_Iterator_Access;
      Element : out Internal_Inner_Env_Assoc) return Boolean;

   procedure Inc_Ref (T : Internal_Inner_Env_Assoc_Iterator_Access);
   procedure Dec_Ref (T : in out Internal_Inner_Env_Assoc_Iterator_Access);

      function Trace_Image (A : Internal_Inner_Env_Assoc_Iterator_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Internal_Inner_Env_Assoc_Iterator, Internal_Inner_Env_Assoc_Iterator_Access);


   function Inner_Env_Assoc_Get
     (Self  : Internal_Inner_Env_Assoc_Array_Access;
      Index : Positive) return Internal_Inner_Env_Assoc
   is (Self.Items (Index));

   function Combine
     (L, R : Internal_Metadata) return Internal_Metadata;
   --  The combine function on environments metadata does a boolean Or on every
   --  boolean component of the env metadata.

   function Can_Reach (El, From : Bare_Turkixir_Node) return Boolean;
   --  Return whether El can reach From, from a sequential viewpoint. If
   --  elements are declared in different units, it will always return True,
   --  eg this does not handle general visibility issues, just sequentiality of
   --  declarations.

   function AST_Envs_Node_Text_Image
     (Node  : Bare_Turkixir_Node;
      Short : Boolean := True) return Text_Type;
   --  Return a "sourcefile:lineno:columnno" corresponding to the starting sloc
   --  of Node. Used to create a human-readable representation for env.
   --  rebindings.

   function Is_Rebindable (Node : Bare_Turkixir_Node) return Boolean;

   function Acquire_Rebinding
     (Node             : Bare_Turkixir_Node;
      Parent           : Env_Rebindings;
      Old_Env, New_Env : Lexical_Env) return Env_Rebindings;
   --  Initialize and return a fresh rebinding

   procedure Release_Rebinding (Self : in out Env_Rebindings);
   --  Mark the rebinding as unused, so that a future call to Acquire_Rebinding
   --  can return it.

   procedure Register_Rebinding
     (Node : Bare_Turkixir_Node; Rebinding : Env_Rebindings);
   --  Register a rebinding to be destroyed when Node's analysis unit is
   --  destroyed or reparsed.

   function Element_Parent
     (Node : Bare_Turkixir_Node) return Bare_Turkixir_Node;

   function Hash (Node : Bare_Turkixir_Node) return Hash_Type;
   function Node_Unit (Node : Bare_Turkixir_Node) return Generic_Unit_Ptr;
   function Named_Hash (Node : Bare_Turkixir_Node) return Hash_Type is
     (Hash (Node));

   No_Analysis_Unit : constant Internal_Unit := null;

   function Convert_Unit is new Ada.Unchecked_Conversion
     (Generic_Unit_Ptr, Internal_Unit);
   function Convert_Unit is new Ada.Unchecked_Conversion
     (Internal_Unit, Generic_Unit_Ptr);

   function Unit_Version (Unit : Generic_Unit_Ptr) return Version_Number;
   --  Return the version for Unit. Version is a number that is incremented
   --  every time Unit changes.

   function Get_Context_Version
     (Node : Bare_Turkixir_Node) return Version_Number;
   --  Assuming that Node is not null, return the version number for Node's
   --  context, which is incremented every time a unit in this context is
   --  parsed.

   type Ref_Category is
     (Nocat);
   type Ref_Categories is array (Ref_Category) of Boolean;
   pragma Pack (Ref_Categories);

   package AST_Envs is new Langkit_Support.Lexical_Envs_Impl
     (Get_Unit_Version         => Unit_Version,
      Node_Type                => Bare_Turkixir_Node,
      Node_Metadata            => Internal_Metadata,
      No_Node                  => null,
      Empty_Metadata           => No_Metadata,
      Node_Unit                => Node_Unit,
      Node_Hash                => Named_Hash,
      Metadata_Hash            => Hash,
      Combine                  => Combine,
      Node_Text_Image          => AST_Envs_Node_Text_Image,
      Acquire_Rebinding        => Acquire_Rebinding,
      Register_Rebinding       => Register_Rebinding,
      Ref_Category             => Ref_Category,
      Ref_Categories           => Ref_Categories,
      Inner_Env_Assoc          => Internal_Inner_Env_Assoc,
      Inner_Env_Assoc_Array    => Internal_Inner_Env_Assoc_Array_Access,
      Get                      => Inner_Env_Assoc_Get);

   use AST_Envs;
   subtype Internal_Entity is AST_Envs.Entity;
   subtype Internal_Entity_Info is AST_Envs.Entity_Info;

   No_Entity_Info : constant Internal_Entity_Info :=
     (No_Metadata, null, False);
   No_Entity : constant Internal_Entity :=
     (null, No_Entity_Info);

   function Hash_Entity (Self : Internal_Entity) return Hash_Type;
   --  Hash function to use in the public API. It's like the regular one, but
   --  disregards metadata.

   function Compare_Entity (Left, Right : Internal_Entity) return Boolean;
   --  Equality function to use in the public API. It's like the regular one,
   --  but disregards metadata.

   function Create_Dynamic_Lexical_Env
     (Self              : Bare_Turkixir_Node;
      Assocs_Getter     : Inner_Env_Assocs_Resolver;
      Assoc_Resolver    : Entity_Resolver;
      Transitive_Parent : Boolean) return Lexical_Env;
   --  Helper for properties code generation: wrapper around
   --  AST_Envs.Create_Dynamic_Lexical_Env.

      function Hash (B : Boolean) return Hash_Type;




   --------------------------
   -- Big integers wrapper --
   --------------------------

   type Big_Integer_Record is limited record
      Value     : GNATCOLL.GMP.Integers.Big_Integer;
      Ref_Count : Integer;
      --  Number of owners. When it drops to 0, this record can be destroyed.
      --  If -1, this is a static big integer: Inc_Ref and Dec_Ref are no-ops.
   end record;

   type Big_Integer_Type is access all Big_Integer_Record;

   function Create_Big_Integer
     (Image : String; Base : Integer := 10) return Big_Integer_Type;
   function Create_Big_Integer
     (Big_Int : GNATCOLL.GMP.Integers.Big_Integer) return Big_Integer_Type;
   function Create_Big_Integer (Int : Integer) return Big_Integer_Type;
   function Create_Public_Big_Integer
     (Big_Int : Big_Integer_Type) return GNATCOLL.GMP.Integers.Big_Integer;

   No_Big_Integer_Record : aliased Big_Integer_Record :=
     (Value => <>, Ref_Count => -1);
   No_Big_Integer : constant Big_Integer_Type := No_Big_Integer_Record'Access;

   function To_Integer (Big_Int : Big_Integer_Type) return Integer;
   --  Convert Big_Int into a regular integer, raising a Property_Error if it
   --  is out of range.

   procedure Inc_Ref (Big_Int : Big_Integer_Type);
   procedure Dec_Ref (Big_Int : in out Big_Integer_Type);

   function Equivalent (Left, Right : Big_Integer_Type) return Boolean;
   function "<" (Left, Right : Big_Integer_Type) return Boolean;
   function "<=" (Left, Right : Big_Integer_Type) return Boolean;
   function ">" (Left, Right : Big_Integer_Type) return Boolean;
   function ">=" (Left, Right : Big_Integer_Type) return Boolean;

   function "+" (Left, Right : Big_Integer_Type) return Big_Integer_Type;
   function "-" (Left, Right : Big_Integer_Type) return Big_Integer_Type;

   function Trace_Image (I : Big_Integer_Type) return String;

      function Trace_Image
        (Node       : Bare_Turkixir_Node;
         Decoration : Boolean := True) return String;

   function Is_Incomplete (Node : Bare_Turkixir_Node) return Boolean;
   --  Return whether this node is incomplete or not.  Incomplete nodes are a
   --  result of the parsing of a node failing as a result of a NoBacktrack
   --  parser annotation.

   function Kind_Name (Node : Bare_Turkixir_Node) return String;
   --  Return the concrete kind for Node

   ---------------------------
   -- Adalog instantiations --
   ---------------------------

   function Text_Image (Ent : Internal_Entity) return Text_Type;
   function Image (Ent : Internal_Entity) return String;
   --  Return a representation of this entity as a string.

   package Eq_Node is new Langkit_Support.Adalog.Eq_Same
     (LR_Type       => Internal_Entity,
      Element_Image => Image);
   subtype Logic_Var is Eq_Node.Refs.Raw_Var;
   subtype Logic_Var_Record is Eq_Node.Refs.Var;
   Null_Var : constant Logic_Var := null;
   Null_Var_Record : constant Logic_Var_Record := (Reset => True, others => <>);

   subtype Logic_Equation is Relation;
   Null_Logic_Equation : constant Logic_Equation := null;

      function Trace_Image (K : Analysis_Unit_Kind) return String;
      function Trace_Image (B : Boolean) return String;
      function Trace_Image (I : Integer) return String;
      function Trace_Image (S : Symbol_Type) return String;
      function Trace_Image (C : Character_Type) return String;
      function Trace_Image (S : String_Type) return String;
      function Trace_Image (Env : Lexical_Env) return String;
      function Trace_Image (R : Env_Rebindings) return String;
      function Trace_Image (Unit : Internal_Unit) return String;
      function Trace_Image (Eq : Logic_Equation) return String;
      function Trace_Image (Var : Logic_Var) return String;

   


   -----------------------------------------------
   -- Structure types (incomplete declarations) --
   -----------------------------------------------

         
      type Internal_DesignatedEnv;
      --  Designate an environment for an env spec action.
   --
   --  The designated environment can be either, depending on the ``Kind``
   --  field:
   --
   --  * If ``Kind`` is ``None``, no environment is designated.
   --
   --  * If ``Kind`` is ``Current_Env``, designate the current environment at
   --  this point during PLE.
   --
   --  * If ``Kind`` is ``Named_Env``, designate the environment which has
   --  precedence for the ``Env_Name`` environment name. If ``Env_Name`` is
   --  null, this designates to environment.
   --
   --  * If ``Kind`` is ``Direct_Env``, the direct value for the designated
   --  environment. That environment must be a primary one and cannot be
   --  foreign to the node currently processed by PLE. If it is the empty
   --  environment, do nothing.

         

         

         
      type Internal_Entity_Expr;
      

         
      type Internal_Entity_And_Expr;
      

         
      type Internal_Entity_And_Op;
      

         
      type Internal_Entity_Arg;
      

         
      type Internal_Entity_Arg_Assoc;
      

         
      type Internal_Entity_Arg_Gen;
      

         
      type Internal_Entity_Turkixir_Node_Base_List;
      

         
      type Internal_Entity_Arg_List;
      

         
      type Internal_Entity_Bin_Op;
      

         
      type Internal_Entity_Arith_Expr;
      

         
      type Internal_Entity_As_Name_Node;
      

         
      type Internal_Entity_As_Name_Node_List;
      

         
      type Internal_Entity_Stmt;
      

         
      type Internal_Entity_Assert_Stmt;
      

         
      type Internal_Entity_Assign_Stmt;
      

         
      type Internal_Entity_Aug_Assign_Stmt;
      

         
      type Internal_Entity_Break_Stmt;
      

         
      type Internal_Entity_Call_Expr;
      

         
      type Internal_Entity_Def_Stmt;
      

         
      type Internal_Entity_Class_Def;
      

         
      type Internal_Entity_Comprehension;
      

         
      type Internal_Entity_Comp_For;
      

         
      type Internal_Entity_Comp_ForL;
      

         
      type Internal_Entity_Comp_If;
      

         
      type Internal_Entity_Comp_Op;
      

         
      type Internal_Entity_Comp_Op_Kind;
      

         
      type Internal_Entity_Comp_Op_Kind_Diamond;
      

         
      type Internal_Entity_Comp_Op_Kind_Eq;
      

         
      type Internal_Entity_Comp_Op_Kind_Gt;
      

         
      type Internal_Entity_Comp_Op_Kind_Gte;
      

         
      type Internal_Entity_Comp_Op_Kind_In;
      

         
      type Internal_Entity_Comp_Op_Kind_Is;
      

         
      type Internal_Entity_Comp_Op_Kind_Isnot;
      

         
      type Internal_Entity_Comp_Op_Kind_Lt;
      

         
      type Internal_Entity_Comp_Op_Kind_Lte;
      

         
      type Internal_Entity_Comp_Op_Kind_Noteq;
      

         
      type Internal_Entity_Comp_Op_Kind_Notin;
      

         
      type Internal_Entity_Concat_String_Lit;
      

         
      type Internal_Entity_Continue_Stmt;
      

         
      type Internal_Entity_Decorated;
      

         
      type Internal_Entity_Decorator;
      

         
      type Internal_Entity_Decorator_List;
      

         
      type Internal_Entity_Del_Stmt;
      

         
      type Internal_Entity_Dict_Assoc;
      

         
      type Internal_Entity_Dict_Assoc_List;
      

         
      type Internal_Entity_Dict_Comp;
      

         
      type Internal_Entity_Dict_Lit;
      

         
      type Internal_Entity_Dot;
      

         
      type Internal_Entity_Dot_List;
      

         
      type Internal_Entity_Name;
      

         
      type Internal_Entity_Dotted_Name;
      

         
      type Internal_Entity_Elif_Branch;
      

         
      type Internal_Entity_Elif_Branch_List;
      

         
      type Internal_Entity_Ellipsis_Expr;
      

         
      type Internal_Entity_Else_Part;
      

         
      type Internal_Entity_Except_Part;
      

         
      type Internal_Entity_Except_Part_List;
      

         
      type Internal_Entity_Exec_Stmt;
      

         
      type Internal_Entity_Expr_List;
      

         
      type Internal_Entity_Slice_Expr;
      

         
      type Internal_Entity_Ext_Slice_Expr;
      

         
      type Internal_Entity_Factor;
      

         
      type Internal_Entity_File_Node;
      

         
      type Internal_Entity_For_Stmt;
      

         
      type Internal_Entity_Func_Def;
      

         
      type Internal_Entity_Global_Stmt;
      

         
      type Internal_Entity_Id;
      

         
      type Internal_Entity_Id_List;
      

         
      type Internal_Entity_If_Expr;
      

         
      type Internal_Entity_If_Stmt;
      

         
      type Internal_Entity_Import_From;
      

         
      type Internal_Entity_Import_Name;
      

         
      type Internal_Entity_Import_Star;
      

         
      type Internal_Entity_Inline_Eval;
      

         
      type Internal_Entity_Kw_Args;
      

         
      type Internal_Entity_Kw_Args_Flag;
      

         
      type Internal_Entity_Kw_Args_Flag_Absent;
      

         
      type Internal_Entity_Kw_Args_Flag_Present;
      

         
      type Internal_Entity_Lambda_Def;
      

         
      type Internal_Entity_List_Comp;
      

         
      type Internal_Entity_List_Gen;
      

         
      type Internal_Entity_List_Lit;
      

         
      type Internal_Entity_NL;
      

         
      type Internal_Entity_NL_List;
      

         
      type Internal_Entity_Not_Op;
      

         
      type Internal_Entity_Number_Lit;
      

         
      type Internal_Entity_Op;
      

         
      type Internal_Entity_Or_Expr;
      

         
      type Internal_Entity_Or_Op;
      

         
      type Internal_Entity_Params;
      

         
      type Internal_Entity_Pass_Stmt;
      

         
      type Internal_Entity_Power;
      

         
      type Internal_Entity_Print_Stmt;
      

         
      type Internal_Entity_Raise_Stmt;
      

         
      type Internal_Entity_Rel_Name;
      

         
      type Internal_Entity_Return_Stmt;
      

         
      type Internal_Entity_Set_Comp;
      

         
      type Internal_Entity_Set_Lit;
      

         
      type Internal_Entity_Shift_Expr;
      

         
      type Internal_Entity_Single_Param;
      

         
      type Internal_Entity_Single_Param_List;
      

         
      type Internal_Entity_Stream_Print_Stmt;
      

         
      type Internal_Entity_String_Lit;
      

         
      type Internal_Entity_String_Lit_List;
      

         
      type Internal_Entity_Subscript_Expr;
      

         
      type Internal_Entity_Term;
      

         
      type Internal_Entity_Try_Stmt;
      

         
      type Internal_Entity_Tuple_Lit;
      

         
      type Internal_Entity_Turkixir_Node_List;
      

         
      type Internal_Entity_Var_Args;
      

         
      type Internal_Entity_Var_Args_Flag;
      

         
      type Internal_Entity_Var_Args_Flag_Absent;
      

         
      type Internal_Entity_Var_Args_Flag_Present;
      

         
      type Internal_Entity_While_Stmt;
      

         
      type Internal_Entity_With_Stmt;
      

         
      type Internal_Entity_Xor_Expr;
      

         
      type Internal_Entity_Yield_Expr;
      

         
      type Internal_Env_Assoc;
      


   -------------------------------------------
   -- Array types (incomplete declarations) --
   -------------------------------------------

         
   type Bare_Turkixir_Node_Array_Record;
   type Bare_Turkixir_Node_Array_Access is access all Bare_Turkixir_Node_Array_Record;

         
   type Internal_Entity_Array_Record;
   type Internal_Entity_Array_Access is access all Internal_Entity_Array_Record;

         
   type Internal_Env_Assoc_Array_Record;
   type Internal_Env_Assoc_Array_Access is access all Internal_Env_Assoc_Array_Record;

         
   type Lexical_Env_Array_Record;
   type Lexical_Env_Array_Access is access all Lexical_Env_Array_Record;

         
   type Symbol_Type_Array_Record;
   type Symbol_Type_Array_Access is access all Symbol_Type_Array_Record;


   ----------------------------------------------
   -- Iterator types (incomplete declarations) --
   ----------------------------------------------

         
   type Internal_Bare_Turkixir_Node_Iterator;
   type Bare_Turkixir_Node_Iterator_Access is access all Internal_Bare_Turkixir_Node_Iterator;

         
   type Internal_Internal_Entity_Iterator;
   type Internal_Entity_Iterator_Access is access all Internal_Internal_Entity_Iterator;


   -----------------------------------------
   -- Structure types (full declarations) --
   -----------------------------------------

         

      

      type Internal_DesignatedEnv is record

               Kind : aliased Designated_Env_Kind;
               
               
               Env_Name : aliased Symbol_Type;
               
               
               Direct_Env : aliased Lexical_Env;
               
               
      end record
        with Convention => C;
      No_DesignatedEnv : constant Internal_DesignatedEnv;

      procedure Inc_Ref (R : Internal_DesignatedEnv);
      procedure Dec_Ref (R : in out Internal_DesignatedEnv);


      function Equivalent (L, R : Internal_DesignatedEnv) return Boolean;

   


      function Trace_Image (R : Internal_DesignatedEnv) return String;


         





   
      function Hash (R : Internal_Entity_Info) return Hash_Type;


      function Trace_Image (R : Internal_Entity_Info) return String;


         



      function Create_Internal_Entity
        (Node : Bare_Turkixir_Node; Info : Internal_Entity_Info)
         return Internal_Entity;


   
      function Hash (R : Internal_Entity) return Hash_Type;


      function Trace_Image (R : Internal_Entity) return String;


         

      

      type Internal_Entity_Expr is record

               Node : aliased Bare_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Expr : constant Internal_Entity_Expr;


      function Create_Internal_Entity_Expr
        (Node : Bare_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Expr;


   


      function Trace_Image (R : Internal_Entity_Expr) return String;


         

      

      type Internal_Entity_And_Expr is record

               Node : aliased Bare_And_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_And_Expr : constant Internal_Entity_And_Expr;


      function Create_Internal_Entity_And_Expr
        (Node : Bare_And_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_And_Expr;


   


      function Trace_Image (R : Internal_Entity_And_Expr) return String;


         

      

      type Internal_Entity_And_Op is record

               Node : aliased Bare_And_Op;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_And_Op : constant Internal_Entity_And_Op;


      function Create_Internal_Entity_And_Op
        (Node : Bare_And_Op; Info : Internal_Entity_Info)
         return Internal_Entity_And_Op;


   


      function Trace_Image (R : Internal_Entity_And_Op) return String;


         

      

      type Internal_Entity_Arg is record

               Node : aliased Bare_Arg;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Arg : constant Internal_Entity_Arg;


      function Create_Internal_Entity_Arg
        (Node : Bare_Arg; Info : Internal_Entity_Info)
         return Internal_Entity_Arg;


   


      function Trace_Image (R : Internal_Entity_Arg) return String;


         

      

      type Internal_Entity_Arg_Assoc is record

               Node : aliased Bare_Arg_Assoc;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Arg_Assoc : constant Internal_Entity_Arg_Assoc;


      function Create_Internal_Entity_Arg_Assoc
        (Node : Bare_Arg_Assoc; Info : Internal_Entity_Info)
         return Internal_Entity_Arg_Assoc;


   


      function Trace_Image (R : Internal_Entity_Arg_Assoc) return String;


         

      

      type Internal_Entity_Arg_Gen is record

               Node : aliased Bare_Arg_Gen;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Arg_Gen : constant Internal_Entity_Arg_Gen;


      function Create_Internal_Entity_Arg_Gen
        (Node : Bare_Arg_Gen; Info : Internal_Entity_Info)
         return Internal_Entity_Arg_Gen;


   


      function Trace_Image (R : Internal_Entity_Arg_Gen) return String;


         

      

      type Internal_Entity_Turkixir_Node_Base_List is record

               Node : aliased Bare_Turkixir_Node_Base_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Turkixir_Node_Base_List : constant Internal_Entity_Turkixir_Node_Base_List;


      function Create_Internal_Entity_Turkixir_Node_Base_List
        (Node : Bare_Turkixir_Node_Base_List; Info : Internal_Entity_Info)
         return Internal_Entity_Turkixir_Node_Base_List;


   


      function Trace_Image (R : Internal_Entity_Turkixir_Node_Base_List) return String;


         

      

      type Internal_Entity_Arg_List is record

               Node : aliased Bare_Arg_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Arg_List : constant Internal_Entity_Arg_List;


      function Create_Internal_Entity_Arg_List
        (Node : Bare_Arg_List; Info : Internal_Entity_Info)
         return Internal_Entity_Arg_List;


   


      function Trace_Image (R : Internal_Entity_Arg_List) return String;


         

      

      type Internal_Entity_Bin_Op is record

               Node : aliased Bare_Bin_Op;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Bin_Op : constant Internal_Entity_Bin_Op;


      function Create_Internal_Entity_Bin_Op
        (Node : Bare_Bin_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Bin_Op;


   


      function Trace_Image (R : Internal_Entity_Bin_Op) return String;


         

      

      type Internal_Entity_Arith_Expr is record

               Node : aliased Bare_Arith_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Arith_Expr : constant Internal_Entity_Arith_Expr;


      function Create_Internal_Entity_Arith_Expr
        (Node : Bare_Arith_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Arith_Expr;


   


      function Trace_Image (R : Internal_Entity_Arith_Expr) return String;


         

      

      type Internal_Entity_As_Name_Node is record

               Node : aliased Bare_As_Name_Node;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_As_Name_Node : constant Internal_Entity_As_Name_Node;


      function Create_Internal_Entity_As_Name_Node
        (Node : Bare_As_Name_Node; Info : Internal_Entity_Info)
         return Internal_Entity_As_Name_Node;


   


      function Trace_Image (R : Internal_Entity_As_Name_Node) return String;


         

      

      type Internal_Entity_As_Name_Node_List is record

               Node : aliased Bare_As_Name_Node_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_As_Name_Node_List : constant Internal_Entity_As_Name_Node_List;


      function Create_Internal_Entity_As_Name_Node_List
        (Node : Bare_As_Name_Node_List; Info : Internal_Entity_Info)
         return Internal_Entity_As_Name_Node_List;


   


      function Trace_Image (R : Internal_Entity_As_Name_Node_List) return String;


         

      

      type Internal_Entity_Stmt is record

               Node : aliased Bare_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Stmt : constant Internal_Entity_Stmt;


      function Create_Internal_Entity_Stmt
        (Node : Bare_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Stmt;


   


      function Trace_Image (R : Internal_Entity_Stmt) return String;


         

      

      type Internal_Entity_Assert_Stmt is record

               Node : aliased Bare_Assert_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Assert_Stmt : constant Internal_Entity_Assert_Stmt;


      function Create_Internal_Entity_Assert_Stmt
        (Node : Bare_Assert_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Assert_Stmt;


   


      function Trace_Image (R : Internal_Entity_Assert_Stmt) return String;


         

      

      type Internal_Entity_Assign_Stmt is record

               Node : aliased Bare_Assign_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Assign_Stmt : constant Internal_Entity_Assign_Stmt;


      function Create_Internal_Entity_Assign_Stmt
        (Node : Bare_Assign_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Assign_Stmt;


   


      function Trace_Image (R : Internal_Entity_Assign_Stmt) return String;


         

      

      type Internal_Entity_Aug_Assign_Stmt is record

               Node : aliased Bare_Aug_Assign_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Aug_Assign_Stmt : constant Internal_Entity_Aug_Assign_Stmt;


      function Create_Internal_Entity_Aug_Assign_Stmt
        (Node : Bare_Aug_Assign_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Aug_Assign_Stmt;


   


      function Trace_Image (R : Internal_Entity_Aug_Assign_Stmt) return String;


         

      

      type Internal_Entity_Break_Stmt is record

               Node : aliased Bare_Break_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Break_Stmt : constant Internal_Entity_Break_Stmt;


      function Create_Internal_Entity_Break_Stmt
        (Node : Bare_Break_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Break_Stmt;


   


      function Trace_Image (R : Internal_Entity_Break_Stmt) return String;


         

      

      type Internal_Entity_Call_Expr is record

               Node : aliased Bare_Call_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Call_Expr : constant Internal_Entity_Call_Expr;


      function Create_Internal_Entity_Call_Expr
        (Node : Bare_Call_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Call_Expr;


   


      function Trace_Image (R : Internal_Entity_Call_Expr) return String;


         

      

      type Internal_Entity_Def_Stmt is record

               Node : aliased Bare_Def_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Def_Stmt : constant Internal_Entity_Def_Stmt;


      function Create_Internal_Entity_Def_Stmt
        (Node : Bare_Def_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Def_Stmt;


   


      function Trace_Image (R : Internal_Entity_Def_Stmt) return String;


         

      

      type Internal_Entity_Class_Def is record

               Node : aliased Bare_Class_Def;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Class_Def : constant Internal_Entity_Class_Def;


      function Create_Internal_Entity_Class_Def
        (Node : Bare_Class_Def; Info : Internal_Entity_Info)
         return Internal_Entity_Class_Def;


   


      function Trace_Image (R : Internal_Entity_Class_Def) return String;


         

      

      type Internal_Entity_Comprehension is record

               Node : aliased Bare_Comprehension;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comprehension : constant Internal_Entity_Comprehension;


      function Create_Internal_Entity_Comprehension
        (Node : Bare_Comprehension; Info : Internal_Entity_Info)
         return Internal_Entity_Comprehension;


   


      function Trace_Image (R : Internal_Entity_Comprehension) return String;


         

      

      type Internal_Entity_Comp_For is record

               Node : aliased Bare_Comp_For;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_For : constant Internal_Entity_Comp_For;


      function Create_Internal_Entity_Comp_For
        (Node : Bare_Comp_For; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_For;


   


      function Trace_Image (R : Internal_Entity_Comp_For) return String;


         

      

      type Internal_Entity_Comp_ForL is record

               Node : aliased Bare_Comp_ForL;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_ForL : constant Internal_Entity_Comp_ForL;


      function Create_Internal_Entity_Comp_ForL
        (Node : Bare_Comp_ForL; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_ForL;


   


      function Trace_Image (R : Internal_Entity_Comp_ForL) return String;


         

      

      type Internal_Entity_Comp_If is record

               Node : aliased Bare_Comp_If;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_If : constant Internal_Entity_Comp_If;


      function Create_Internal_Entity_Comp_If
        (Node : Bare_Comp_If; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_If;


   


      function Trace_Image (R : Internal_Entity_Comp_If) return String;


         

      

      type Internal_Entity_Comp_Op is record

               Node : aliased Bare_Comp_Op;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_Op : constant Internal_Entity_Comp_Op;


      function Create_Internal_Entity_Comp_Op
        (Node : Bare_Comp_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op;


   


      function Trace_Image (R : Internal_Entity_Comp_Op) return String;


         

      

      type Internal_Entity_Comp_Op_Kind is record

               Node : aliased Bare_Comp_Op_Kind;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_Op_Kind : constant Internal_Entity_Comp_Op_Kind;


      function Create_Internal_Entity_Comp_Op_Kind
        (Node : Bare_Comp_Op_Kind; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind;


   


      function Trace_Image (R : Internal_Entity_Comp_Op_Kind) return String;


         

      

      type Internal_Entity_Comp_Op_Kind_Diamond is record

               Node : aliased Bare_Comp_Op_Kind_Diamond;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_Op_Kind_Diamond : constant Internal_Entity_Comp_Op_Kind_Diamond;


      function Create_Internal_Entity_Comp_Op_Kind_Diamond
        (Node : Bare_Comp_Op_Kind_Diamond; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Diamond;


   


      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Diamond) return String;


         

      

      type Internal_Entity_Comp_Op_Kind_Eq is record

               Node : aliased Bare_Comp_Op_Kind_Eq;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_Op_Kind_Eq : constant Internal_Entity_Comp_Op_Kind_Eq;


      function Create_Internal_Entity_Comp_Op_Kind_Eq
        (Node : Bare_Comp_Op_Kind_Eq; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Eq;


   


      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Eq) return String;


         

      

      type Internal_Entity_Comp_Op_Kind_Gt is record

               Node : aliased Bare_Comp_Op_Kind_Gt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_Op_Kind_Gt : constant Internal_Entity_Comp_Op_Kind_Gt;


      function Create_Internal_Entity_Comp_Op_Kind_Gt
        (Node : Bare_Comp_Op_Kind_Gt; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Gt;


   


      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Gt) return String;


         

      

      type Internal_Entity_Comp_Op_Kind_Gte is record

               Node : aliased Bare_Comp_Op_Kind_Gte;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_Op_Kind_Gte : constant Internal_Entity_Comp_Op_Kind_Gte;


      function Create_Internal_Entity_Comp_Op_Kind_Gte
        (Node : Bare_Comp_Op_Kind_Gte; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Gte;


   


      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Gte) return String;


         

      

      type Internal_Entity_Comp_Op_Kind_In is record

               Node : aliased Bare_Comp_Op_Kind_In;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_Op_Kind_In : constant Internal_Entity_Comp_Op_Kind_In;


      function Create_Internal_Entity_Comp_Op_Kind_In
        (Node : Bare_Comp_Op_Kind_In; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_In;


   


      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_In) return String;


         

      

      type Internal_Entity_Comp_Op_Kind_Is is record

               Node : aliased Bare_Comp_Op_Kind_Is;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_Op_Kind_Is : constant Internal_Entity_Comp_Op_Kind_Is;


      function Create_Internal_Entity_Comp_Op_Kind_Is
        (Node : Bare_Comp_Op_Kind_Is; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Is;


   


      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Is) return String;


         

      

      type Internal_Entity_Comp_Op_Kind_Isnot is record

               Node : aliased Bare_Comp_Op_Kind_Isnot;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_Op_Kind_Isnot : constant Internal_Entity_Comp_Op_Kind_Isnot;


      function Create_Internal_Entity_Comp_Op_Kind_Isnot
        (Node : Bare_Comp_Op_Kind_Isnot; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Isnot;


   


      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Isnot) return String;


         

      

      type Internal_Entity_Comp_Op_Kind_Lt is record

               Node : aliased Bare_Comp_Op_Kind_Lt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_Op_Kind_Lt : constant Internal_Entity_Comp_Op_Kind_Lt;


      function Create_Internal_Entity_Comp_Op_Kind_Lt
        (Node : Bare_Comp_Op_Kind_Lt; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Lt;


   


      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Lt) return String;


         

      

      type Internal_Entity_Comp_Op_Kind_Lte is record

               Node : aliased Bare_Comp_Op_Kind_Lte;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_Op_Kind_Lte : constant Internal_Entity_Comp_Op_Kind_Lte;


      function Create_Internal_Entity_Comp_Op_Kind_Lte
        (Node : Bare_Comp_Op_Kind_Lte; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Lte;


   


      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Lte) return String;


         

      

      type Internal_Entity_Comp_Op_Kind_Noteq is record

               Node : aliased Bare_Comp_Op_Kind_Noteq;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_Op_Kind_Noteq : constant Internal_Entity_Comp_Op_Kind_Noteq;


      function Create_Internal_Entity_Comp_Op_Kind_Noteq
        (Node : Bare_Comp_Op_Kind_Noteq; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Noteq;


   


      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Noteq) return String;


         

      

      type Internal_Entity_Comp_Op_Kind_Notin is record

               Node : aliased Bare_Comp_Op_Kind_Notin;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Comp_Op_Kind_Notin : constant Internal_Entity_Comp_Op_Kind_Notin;


      function Create_Internal_Entity_Comp_Op_Kind_Notin
        (Node : Bare_Comp_Op_Kind_Notin; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Notin;


   


      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Notin) return String;


         

      

      type Internal_Entity_Concat_String_Lit is record

               Node : aliased Bare_Concat_String_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Concat_String_Lit : constant Internal_Entity_Concat_String_Lit;


      function Create_Internal_Entity_Concat_String_Lit
        (Node : Bare_Concat_String_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Concat_String_Lit;


   


      function Trace_Image (R : Internal_Entity_Concat_String_Lit) return String;


         

      

      type Internal_Entity_Continue_Stmt is record

               Node : aliased Bare_Continue_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Continue_Stmt : constant Internal_Entity_Continue_Stmt;


      function Create_Internal_Entity_Continue_Stmt
        (Node : Bare_Continue_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Continue_Stmt;


   


      function Trace_Image (R : Internal_Entity_Continue_Stmt) return String;


         

      

      type Internal_Entity_Decorated is record

               Node : aliased Bare_Decorated;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Decorated : constant Internal_Entity_Decorated;


      function Create_Internal_Entity_Decorated
        (Node : Bare_Decorated; Info : Internal_Entity_Info)
         return Internal_Entity_Decorated;


   


      function Trace_Image (R : Internal_Entity_Decorated) return String;


         

      

      type Internal_Entity_Decorator is record

               Node : aliased Bare_Decorator;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Decorator : constant Internal_Entity_Decorator;


      function Create_Internal_Entity_Decorator
        (Node : Bare_Decorator; Info : Internal_Entity_Info)
         return Internal_Entity_Decorator;


   


      function Trace_Image (R : Internal_Entity_Decorator) return String;


         

      

      type Internal_Entity_Decorator_List is record

               Node : aliased Bare_Decorator_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Decorator_List : constant Internal_Entity_Decorator_List;


      function Create_Internal_Entity_Decorator_List
        (Node : Bare_Decorator_List; Info : Internal_Entity_Info)
         return Internal_Entity_Decorator_List;


   


      function Trace_Image (R : Internal_Entity_Decorator_List) return String;


         

      

      type Internal_Entity_Del_Stmt is record

               Node : aliased Bare_Del_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Del_Stmt : constant Internal_Entity_Del_Stmt;


      function Create_Internal_Entity_Del_Stmt
        (Node : Bare_Del_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Del_Stmt;


   


      function Trace_Image (R : Internal_Entity_Del_Stmt) return String;


         

      

      type Internal_Entity_Dict_Assoc is record

               Node : aliased Bare_Dict_Assoc;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Dict_Assoc : constant Internal_Entity_Dict_Assoc;


      function Create_Internal_Entity_Dict_Assoc
        (Node : Bare_Dict_Assoc; Info : Internal_Entity_Info)
         return Internal_Entity_Dict_Assoc;


   


      function Trace_Image (R : Internal_Entity_Dict_Assoc) return String;


         

      

      type Internal_Entity_Dict_Assoc_List is record

               Node : aliased Bare_Dict_Assoc_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Dict_Assoc_List : constant Internal_Entity_Dict_Assoc_List;


      function Create_Internal_Entity_Dict_Assoc_List
        (Node : Bare_Dict_Assoc_List; Info : Internal_Entity_Info)
         return Internal_Entity_Dict_Assoc_List;


   


      function Trace_Image (R : Internal_Entity_Dict_Assoc_List) return String;


         

      

      type Internal_Entity_Dict_Comp is record

               Node : aliased Bare_Dict_Comp;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Dict_Comp : constant Internal_Entity_Dict_Comp;


      function Create_Internal_Entity_Dict_Comp
        (Node : Bare_Dict_Comp; Info : Internal_Entity_Info)
         return Internal_Entity_Dict_Comp;


   


      function Trace_Image (R : Internal_Entity_Dict_Comp) return String;


         

      

      type Internal_Entity_Dict_Lit is record

               Node : aliased Bare_Dict_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Dict_Lit : constant Internal_Entity_Dict_Lit;


      function Create_Internal_Entity_Dict_Lit
        (Node : Bare_Dict_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Dict_Lit;


   


      function Trace_Image (R : Internal_Entity_Dict_Lit) return String;


         

      

      type Internal_Entity_Dot is record

               Node : aliased Bare_Dot;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Dot : constant Internal_Entity_Dot;


      function Create_Internal_Entity_Dot
        (Node : Bare_Dot; Info : Internal_Entity_Info)
         return Internal_Entity_Dot;


   


      function Trace_Image (R : Internal_Entity_Dot) return String;


         

      

      type Internal_Entity_Dot_List is record

               Node : aliased Bare_Dot_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Dot_List : constant Internal_Entity_Dot_List;


      function Create_Internal_Entity_Dot_List
        (Node : Bare_Dot_List; Info : Internal_Entity_Info)
         return Internal_Entity_Dot_List;


   


      function Trace_Image (R : Internal_Entity_Dot_List) return String;


         

      

      type Internal_Entity_Name is record

               Node : aliased Bare_Name;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Name : constant Internal_Entity_Name;


      function Create_Internal_Entity_Name
        (Node : Bare_Name; Info : Internal_Entity_Info)
         return Internal_Entity_Name;


   


      function Trace_Image (R : Internal_Entity_Name) return String;


         

      

      type Internal_Entity_Dotted_Name is record

               Node : aliased Bare_Dotted_Name;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Dotted_Name : constant Internal_Entity_Dotted_Name;


      function Create_Internal_Entity_Dotted_Name
        (Node : Bare_Dotted_Name; Info : Internal_Entity_Info)
         return Internal_Entity_Dotted_Name;


   


      function Trace_Image (R : Internal_Entity_Dotted_Name) return String;


         

      

      type Internal_Entity_Elif_Branch is record

               Node : aliased Bare_Elif_Branch;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Elif_Branch : constant Internal_Entity_Elif_Branch;


      function Create_Internal_Entity_Elif_Branch
        (Node : Bare_Elif_Branch; Info : Internal_Entity_Info)
         return Internal_Entity_Elif_Branch;


   


      function Trace_Image (R : Internal_Entity_Elif_Branch) return String;


         

      

      type Internal_Entity_Elif_Branch_List is record

               Node : aliased Bare_Elif_Branch_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Elif_Branch_List : constant Internal_Entity_Elif_Branch_List;


      function Create_Internal_Entity_Elif_Branch_List
        (Node : Bare_Elif_Branch_List; Info : Internal_Entity_Info)
         return Internal_Entity_Elif_Branch_List;


   


      function Trace_Image (R : Internal_Entity_Elif_Branch_List) return String;


         

      

      type Internal_Entity_Ellipsis_Expr is record

               Node : aliased Bare_Ellipsis_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ellipsis_Expr : constant Internal_Entity_Ellipsis_Expr;


      function Create_Internal_Entity_Ellipsis_Expr
        (Node : Bare_Ellipsis_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Ellipsis_Expr;


   


      function Trace_Image (R : Internal_Entity_Ellipsis_Expr) return String;


         

      

      type Internal_Entity_Else_Part is record

               Node : aliased Bare_Else_Part;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Else_Part : constant Internal_Entity_Else_Part;


      function Create_Internal_Entity_Else_Part
        (Node : Bare_Else_Part; Info : Internal_Entity_Info)
         return Internal_Entity_Else_Part;


   


      function Trace_Image (R : Internal_Entity_Else_Part) return String;


         

      

      type Internal_Entity_Except_Part is record

               Node : aliased Bare_Except_Part;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Except_Part : constant Internal_Entity_Except_Part;


      function Create_Internal_Entity_Except_Part
        (Node : Bare_Except_Part; Info : Internal_Entity_Info)
         return Internal_Entity_Except_Part;


   


      function Trace_Image (R : Internal_Entity_Except_Part) return String;


         

      

      type Internal_Entity_Except_Part_List is record

               Node : aliased Bare_Except_Part_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Except_Part_List : constant Internal_Entity_Except_Part_List;


      function Create_Internal_Entity_Except_Part_List
        (Node : Bare_Except_Part_List; Info : Internal_Entity_Info)
         return Internal_Entity_Except_Part_List;


   


      function Trace_Image (R : Internal_Entity_Except_Part_List) return String;


         

      

      type Internal_Entity_Exec_Stmt is record

               Node : aliased Bare_Exec_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Exec_Stmt : constant Internal_Entity_Exec_Stmt;


      function Create_Internal_Entity_Exec_Stmt
        (Node : Bare_Exec_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Exec_Stmt;


   


      function Trace_Image (R : Internal_Entity_Exec_Stmt) return String;


         

      

      type Internal_Entity_Expr_List is record

               Node : aliased Bare_Expr_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Expr_List : constant Internal_Entity_Expr_List;


      function Create_Internal_Entity_Expr_List
        (Node : Bare_Expr_List; Info : Internal_Entity_Info)
         return Internal_Entity_Expr_List;


   


      function Trace_Image (R : Internal_Entity_Expr_List) return String;


         

      

      type Internal_Entity_Slice_Expr is record

               Node : aliased Bare_Slice_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Slice_Expr : constant Internal_Entity_Slice_Expr;


      function Create_Internal_Entity_Slice_Expr
        (Node : Bare_Slice_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Slice_Expr;


   


      function Trace_Image (R : Internal_Entity_Slice_Expr) return String;


         

      

      type Internal_Entity_Ext_Slice_Expr is record

               Node : aliased Bare_Ext_Slice_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ext_Slice_Expr : constant Internal_Entity_Ext_Slice_Expr;


      function Create_Internal_Entity_Ext_Slice_Expr
        (Node : Bare_Ext_Slice_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Ext_Slice_Expr;


   


      function Trace_Image (R : Internal_Entity_Ext_Slice_Expr) return String;


         

      

      type Internal_Entity_Factor is record

               Node : aliased Bare_Factor;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Factor : constant Internal_Entity_Factor;


      function Create_Internal_Entity_Factor
        (Node : Bare_Factor; Info : Internal_Entity_Info)
         return Internal_Entity_Factor;


   


      function Trace_Image (R : Internal_Entity_Factor) return String;


         

      

      type Internal_Entity_File_Node is record

               Node : aliased Bare_File_Node;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_File_Node : constant Internal_Entity_File_Node;


      function Create_Internal_Entity_File_Node
        (Node : Bare_File_Node; Info : Internal_Entity_Info)
         return Internal_Entity_File_Node;


   


      function Trace_Image (R : Internal_Entity_File_Node) return String;


         

      

      type Internal_Entity_For_Stmt is record

               Node : aliased Bare_For_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_For_Stmt : constant Internal_Entity_For_Stmt;


      function Create_Internal_Entity_For_Stmt
        (Node : Bare_For_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_For_Stmt;


   


      function Trace_Image (R : Internal_Entity_For_Stmt) return String;


         

      

      type Internal_Entity_Func_Def is record

               Node : aliased Bare_Func_Def;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Func_Def : constant Internal_Entity_Func_Def;


      function Create_Internal_Entity_Func_Def
        (Node : Bare_Func_Def; Info : Internal_Entity_Info)
         return Internal_Entity_Func_Def;


   


      function Trace_Image (R : Internal_Entity_Func_Def) return String;


         

      

      type Internal_Entity_Global_Stmt is record

               Node : aliased Bare_Global_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Global_Stmt : constant Internal_Entity_Global_Stmt;


      function Create_Internal_Entity_Global_Stmt
        (Node : Bare_Global_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Global_Stmt;


   


      function Trace_Image (R : Internal_Entity_Global_Stmt) return String;


         

      

      type Internal_Entity_Id is record

               Node : aliased Bare_Id;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Id : constant Internal_Entity_Id;


      function Create_Internal_Entity_Id
        (Node : Bare_Id; Info : Internal_Entity_Info)
         return Internal_Entity_Id;


   


      function Trace_Image (R : Internal_Entity_Id) return String;


         

      

      type Internal_Entity_Id_List is record

               Node : aliased Bare_Id_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Id_List : constant Internal_Entity_Id_List;


      function Create_Internal_Entity_Id_List
        (Node : Bare_Id_List; Info : Internal_Entity_Info)
         return Internal_Entity_Id_List;


   


      function Trace_Image (R : Internal_Entity_Id_List) return String;


         

      

      type Internal_Entity_If_Expr is record

               Node : aliased Bare_If_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_If_Expr : constant Internal_Entity_If_Expr;


      function Create_Internal_Entity_If_Expr
        (Node : Bare_If_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_If_Expr;


   


      function Trace_Image (R : Internal_Entity_If_Expr) return String;


         

      

      type Internal_Entity_If_Stmt is record

               Node : aliased Bare_If_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_If_Stmt : constant Internal_Entity_If_Stmt;


      function Create_Internal_Entity_If_Stmt
        (Node : Bare_If_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_If_Stmt;


   


      function Trace_Image (R : Internal_Entity_If_Stmt) return String;


         

      

      type Internal_Entity_Import_From is record

               Node : aliased Bare_Import_From;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Import_From : constant Internal_Entity_Import_From;


      function Create_Internal_Entity_Import_From
        (Node : Bare_Import_From; Info : Internal_Entity_Info)
         return Internal_Entity_Import_From;


   


      function Trace_Image (R : Internal_Entity_Import_From) return String;


         

      

      type Internal_Entity_Import_Name is record

               Node : aliased Bare_Import_Name;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Import_Name : constant Internal_Entity_Import_Name;


      function Create_Internal_Entity_Import_Name
        (Node : Bare_Import_Name; Info : Internal_Entity_Info)
         return Internal_Entity_Import_Name;


   


      function Trace_Image (R : Internal_Entity_Import_Name) return String;


         

      

      type Internal_Entity_Import_Star is record

               Node : aliased Bare_Import_Star;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Import_Star : constant Internal_Entity_Import_Star;


      function Create_Internal_Entity_Import_Star
        (Node : Bare_Import_Star; Info : Internal_Entity_Info)
         return Internal_Entity_Import_Star;


   


      function Trace_Image (R : Internal_Entity_Import_Star) return String;


         

      

      type Internal_Entity_Inline_Eval is record

               Node : aliased Bare_Inline_Eval;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Inline_Eval : constant Internal_Entity_Inline_Eval;


      function Create_Internal_Entity_Inline_Eval
        (Node : Bare_Inline_Eval; Info : Internal_Entity_Info)
         return Internal_Entity_Inline_Eval;


   


      function Trace_Image (R : Internal_Entity_Inline_Eval) return String;


         

      

      type Internal_Entity_Kw_Args is record

               Node : aliased Bare_Kw_Args;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Kw_Args : constant Internal_Entity_Kw_Args;


      function Create_Internal_Entity_Kw_Args
        (Node : Bare_Kw_Args; Info : Internal_Entity_Info)
         return Internal_Entity_Kw_Args;


   


      function Trace_Image (R : Internal_Entity_Kw_Args) return String;


         

      

      type Internal_Entity_Kw_Args_Flag is record

               Node : aliased Bare_Kw_Args_Flag;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Kw_Args_Flag : constant Internal_Entity_Kw_Args_Flag;


      function Create_Internal_Entity_Kw_Args_Flag
        (Node : Bare_Kw_Args_Flag; Info : Internal_Entity_Info)
         return Internal_Entity_Kw_Args_Flag;


   


      function Trace_Image (R : Internal_Entity_Kw_Args_Flag) return String;


         

      

      type Internal_Entity_Kw_Args_Flag_Absent is record

               Node : aliased Bare_Kw_Args_Flag_Absent;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Kw_Args_Flag_Absent : constant Internal_Entity_Kw_Args_Flag_Absent;


      function Create_Internal_Entity_Kw_Args_Flag_Absent
        (Node : Bare_Kw_Args_Flag_Absent; Info : Internal_Entity_Info)
         return Internal_Entity_Kw_Args_Flag_Absent;


   


      function Trace_Image (R : Internal_Entity_Kw_Args_Flag_Absent) return String;


         

      

      type Internal_Entity_Kw_Args_Flag_Present is record

               Node : aliased Bare_Kw_Args_Flag_Present;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Kw_Args_Flag_Present : constant Internal_Entity_Kw_Args_Flag_Present;


      function Create_Internal_Entity_Kw_Args_Flag_Present
        (Node : Bare_Kw_Args_Flag_Present; Info : Internal_Entity_Info)
         return Internal_Entity_Kw_Args_Flag_Present;


   


      function Trace_Image (R : Internal_Entity_Kw_Args_Flag_Present) return String;


         

      

      type Internal_Entity_Lambda_Def is record

               Node : aliased Bare_Lambda_Def;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Lambda_Def : constant Internal_Entity_Lambda_Def;


      function Create_Internal_Entity_Lambda_Def
        (Node : Bare_Lambda_Def; Info : Internal_Entity_Info)
         return Internal_Entity_Lambda_Def;


   


      function Trace_Image (R : Internal_Entity_Lambda_Def) return String;


         

      

      type Internal_Entity_List_Comp is record

               Node : aliased Bare_List_Comp;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_List_Comp : constant Internal_Entity_List_Comp;


      function Create_Internal_Entity_List_Comp
        (Node : Bare_List_Comp; Info : Internal_Entity_Info)
         return Internal_Entity_List_Comp;


   


      function Trace_Image (R : Internal_Entity_List_Comp) return String;


         

      

      type Internal_Entity_List_Gen is record

               Node : aliased Bare_List_Gen;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_List_Gen : constant Internal_Entity_List_Gen;


      function Create_Internal_Entity_List_Gen
        (Node : Bare_List_Gen; Info : Internal_Entity_Info)
         return Internal_Entity_List_Gen;


   


      function Trace_Image (R : Internal_Entity_List_Gen) return String;


         

      

      type Internal_Entity_List_Lit is record

               Node : aliased Bare_List_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_List_Lit : constant Internal_Entity_List_Lit;


      function Create_Internal_Entity_List_Lit
        (Node : Bare_List_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_List_Lit;


   


      function Trace_Image (R : Internal_Entity_List_Lit) return String;


         

      

      type Internal_Entity_NL is record

               Node : aliased Bare_NL;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_NL : constant Internal_Entity_NL;


      function Create_Internal_Entity_NL
        (Node : Bare_NL; Info : Internal_Entity_Info)
         return Internal_Entity_NL;


   


      function Trace_Image (R : Internal_Entity_NL) return String;


         

      

      type Internal_Entity_NL_List is record

               Node : aliased Bare_NL_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_NL_List : constant Internal_Entity_NL_List;


      function Create_Internal_Entity_NL_List
        (Node : Bare_NL_List; Info : Internal_Entity_Info)
         return Internal_Entity_NL_List;


   


      function Trace_Image (R : Internal_Entity_NL_List) return String;


         

      

      type Internal_Entity_Not_Op is record

               Node : aliased Bare_Not_Op;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Not_Op : constant Internal_Entity_Not_Op;


      function Create_Internal_Entity_Not_Op
        (Node : Bare_Not_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Not_Op;


   


      function Trace_Image (R : Internal_Entity_Not_Op) return String;


         

      

      type Internal_Entity_Number_Lit is record

               Node : aliased Bare_Number_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Number_Lit : constant Internal_Entity_Number_Lit;


      function Create_Internal_Entity_Number_Lit
        (Node : Bare_Number_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Number_Lit;


   


      function Trace_Image (R : Internal_Entity_Number_Lit) return String;


         

      

      type Internal_Entity_Op is record

               Node : aliased Bare_Op;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op : constant Internal_Entity_Op;


      function Create_Internal_Entity_Op
        (Node : Bare_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Op;


   


      function Trace_Image (R : Internal_Entity_Op) return String;


         

      

      type Internal_Entity_Or_Expr is record

               Node : aliased Bare_Or_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Or_Expr : constant Internal_Entity_Or_Expr;


      function Create_Internal_Entity_Or_Expr
        (Node : Bare_Or_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Or_Expr;


   


      function Trace_Image (R : Internal_Entity_Or_Expr) return String;


         

      

      type Internal_Entity_Or_Op is record

               Node : aliased Bare_Or_Op;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Or_Op : constant Internal_Entity_Or_Op;


      function Create_Internal_Entity_Or_Op
        (Node : Bare_Or_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Or_Op;


   


      function Trace_Image (R : Internal_Entity_Or_Op) return String;


         

      

      type Internal_Entity_Params is record

               Node : aliased Bare_Params;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Params : constant Internal_Entity_Params;


      function Create_Internal_Entity_Params
        (Node : Bare_Params; Info : Internal_Entity_Info)
         return Internal_Entity_Params;


   


      function Trace_Image (R : Internal_Entity_Params) return String;


         

      

      type Internal_Entity_Pass_Stmt is record

               Node : aliased Bare_Pass_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Pass_Stmt : constant Internal_Entity_Pass_Stmt;


      function Create_Internal_Entity_Pass_Stmt
        (Node : Bare_Pass_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Pass_Stmt;


   


      function Trace_Image (R : Internal_Entity_Pass_Stmt) return String;


         

      

      type Internal_Entity_Power is record

               Node : aliased Bare_Power;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Power : constant Internal_Entity_Power;


      function Create_Internal_Entity_Power
        (Node : Bare_Power; Info : Internal_Entity_Info)
         return Internal_Entity_Power;


   


      function Trace_Image (R : Internal_Entity_Power) return String;


         

      

      type Internal_Entity_Print_Stmt is record

               Node : aliased Bare_Print_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Print_Stmt : constant Internal_Entity_Print_Stmt;


      function Create_Internal_Entity_Print_Stmt
        (Node : Bare_Print_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Print_Stmt;


   


      function Trace_Image (R : Internal_Entity_Print_Stmt) return String;


         

      

      type Internal_Entity_Raise_Stmt is record

               Node : aliased Bare_Raise_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Raise_Stmt : constant Internal_Entity_Raise_Stmt;


      function Create_Internal_Entity_Raise_Stmt
        (Node : Bare_Raise_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Raise_Stmt;


   


      function Trace_Image (R : Internal_Entity_Raise_Stmt) return String;


         

      

      type Internal_Entity_Rel_Name is record

               Node : aliased Bare_Rel_Name;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Rel_Name : constant Internal_Entity_Rel_Name;


      function Create_Internal_Entity_Rel_Name
        (Node : Bare_Rel_Name; Info : Internal_Entity_Info)
         return Internal_Entity_Rel_Name;


   


      function Trace_Image (R : Internal_Entity_Rel_Name) return String;


         

      

      type Internal_Entity_Return_Stmt is record

               Node : aliased Bare_Return_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Return_Stmt : constant Internal_Entity_Return_Stmt;


      function Create_Internal_Entity_Return_Stmt
        (Node : Bare_Return_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Return_Stmt;


   


      function Trace_Image (R : Internal_Entity_Return_Stmt) return String;


         

      

      type Internal_Entity_Set_Comp is record

               Node : aliased Bare_Set_Comp;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Set_Comp : constant Internal_Entity_Set_Comp;


      function Create_Internal_Entity_Set_Comp
        (Node : Bare_Set_Comp; Info : Internal_Entity_Info)
         return Internal_Entity_Set_Comp;


   


      function Trace_Image (R : Internal_Entity_Set_Comp) return String;


         

      

      type Internal_Entity_Set_Lit is record

               Node : aliased Bare_Set_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Set_Lit : constant Internal_Entity_Set_Lit;


      function Create_Internal_Entity_Set_Lit
        (Node : Bare_Set_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Set_Lit;


   


      function Trace_Image (R : Internal_Entity_Set_Lit) return String;


         

      

      type Internal_Entity_Shift_Expr is record

               Node : aliased Bare_Shift_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Shift_Expr : constant Internal_Entity_Shift_Expr;


      function Create_Internal_Entity_Shift_Expr
        (Node : Bare_Shift_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Shift_Expr;


   


      function Trace_Image (R : Internal_Entity_Shift_Expr) return String;


         

      

      type Internal_Entity_Single_Param is record

               Node : aliased Bare_Single_Param;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Single_Param : constant Internal_Entity_Single_Param;


      function Create_Internal_Entity_Single_Param
        (Node : Bare_Single_Param; Info : Internal_Entity_Info)
         return Internal_Entity_Single_Param;


   


      function Trace_Image (R : Internal_Entity_Single_Param) return String;


         

      

      type Internal_Entity_Single_Param_List is record

               Node : aliased Bare_Single_Param_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Single_Param_List : constant Internal_Entity_Single_Param_List;


      function Create_Internal_Entity_Single_Param_List
        (Node : Bare_Single_Param_List; Info : Internal_Entity_Info)
         return Internal_Entity_Single_Param_List;


   


      function Trace_Image (R : Internal_Entity_Single_Param_List) return String;


         

      

      type Internal_Entity_Stream_Print_Stmt is record

               Node : aliased Bare_Stream_Print_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Stream_Print_Stmt : constant Internal_Entity_Stream_Print_Stmt;


      function Create_Internal_Entity_Stream_Print_Stmt
        (Node : Bare_Stream_Print_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Stream_Print_Stmt;


   


      function Trace_Image (R : Internal_Entity_Stream_Print_Stmt) return String;


         

      

      type Internal_Entity_String_Lit is record

               Node : aliased Bare_String_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_String_Lit : constant Internal_Entity_String_Lit;


      function Create_Internal_Entity_String_Lit
        (Node : Bare_String_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_String_Lit;


   


      function Trace_Image (R : Internal_Entity_String_Lit) return String;


         

      

      type Internal_Entity_String_Lit_List is record

               Node : aliased Bare_String_Lit_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_String_Lit_List : constant Internal_Entity_String_Lit_List;


      function Create_Internal_Entity_String_Lit_List
        (Node : Bare_String_Lit_List; Info : Internal_Entity_Info)
         return Internal_Entity_String_Lit_List;


   


      function Trace_Image (R : Internal_Entity_String_Lit_List) return String;


         

      

      type Internal_Entity_Subscript_Expr is record

               Node : aliased Bare_Subscript_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Subscript_Expr : constant Internal_Entity_Subscript_Expr;


      function Create_Internal_Entity_Subscript_Expr
        (Node : Bare_Subscript_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Subscript_Expr;


   


      function Trace_Image (R : Internal_Entity_Subscript_Expr) return String;


         

      

      type Internal_Entity_Term is record

               Node : aliased Bare_Term;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Term : constant Internal_Entity_Term;


      function Create_Internal_Entity_Term
        (Node : Bare_Term; Info : Internal_Entity_Info)
         return Internal_Entity_Term;


   


      function Trace_Image (R : Internal_Entity_Term) return String;


         

      

      type Internal_Entity_Try_Stmt is record

               Node : aliased Bare_Try_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Try_Stmt : constant Internal_Entity_Try_Stmt;


      function Create_Internal_Entity_Try_Stmt
        (Node : Bare_Try_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Try_Stmt;


   


      function Trace_Image (R : Internal_Entity_Try_Stmt) return String;


         

      

      type Internal_Entity_Tuple_Lit is record

               Node : aliased Bare_Tuple_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Tuple_Lit : constant Internal_Entity_Tuple_Lit;


      function Create_Internal_Entity_Tuple_Lit
        (Node : Bare_Tuple_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Tuple_Lit;


   


      function Trace_Image (R : Internal_Entity_Tuple_Lit) return String;


         

      

      type Internal_Entity_Turkixir_Node_List is record

               Node : aliased Bare_Turkixir_Node_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Turkixir_Node_List : constant Internal_Entity_Turkixir_Node_List;


      function Create_Internal_Entity_Turkixir_Node_List
        (Node : Bare_Turkixir_Node_List; Info : Internal_Entity_Info)
         return Internal_Entity_Turkixir_Node_List;


   


      function Trace_Image (R : Internal_Entity_Turkixir_Node_List) return String;


         

      

      type Internal_Entity_Var_Args is record

               Node : aliased Bare_Var_Args;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Var_Args : constant Internal_Entity_Var_Args;


      function Create_Internal_Entity_Var_Args
        (Node : Bare_Var_Args; Info : Internal_Entity_Info)
         return Internal_Entity_Var_Args;


   


      function Trace_Image (R : Internal_Entity_Var_Args) return String;


         

      

      type Internal_Entity_Var_Args_Flag is record

               Node : aliased Bare_Var_Args_Flag;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Var_Args_Flag : constant Internal_Entity_Var_Args_Flag;


      function Create_Internal_Entity_Var_Args_Flag
        (Node : Bare_Var_Args_Flag; Info : Internal_Entity_Info)
         return Internal_Entity_Var_Args_Flag;


   


      function Trace_Image (R : Internal_Entity_Var_Args_Flag) return String;


         

      

      type Internal_Entity_Var_Args_Flag_Absent is record

               Node : aliased Bare_Var_Args_Flag_Absent;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Var_Args_Flag_Absent : constant Internal_Entity_Var_Args_Flag_Absent;


      function Create_Internal_Entity_Var_Args_Flag_Absent
        (Node : Bare_Var_Args_Flag_Absent; Info : Internal_Entity_Info)
         return Internal_Entity_Var_Args_Flag_Absent;


   


      function Trace_Image (R : Internal_Entity_Var_Args_Flag_Absent) return String;


         

      

      type Internal_Entity_Var_Args_Flag_Present is record

               Node : aliased Bare_Var_Args_Flag_Present;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Var_Args_Flag_Present : constant Internal_Entity_Var_Args_Flag_Present;


      function Create_Internal_Entity_Var_Args_Flag_Present
        (Node : Bare_Var_Args_Flag_Present; Info : Internal_Entity_Info)
         return Internal_Entity_Var_Args_Flag_Present;


   


      function Trace_Image (R : Internal_Entity_Var_Args_Flag_Present) return String;


         

      

      type Internal_Entity_While_Stmt is record

               Node : aliased Bare_While_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_While_Stmt : constant Internal_Entity_While_Stmt;


      function Create_Internal_Entity_While_Stmt
        (Node : Bare_While_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_While_Stmt;


   


      function Trace_Image (R : Internal_Entity_While_Stmt) return String;


         

      

      type Internal_Entity_With_Stmt is record

               Node : aliased Bare_With_Stmt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_With_Stmt : constant Internal_Entity_With_Stmt;


      function Create_Internal_Entity_With_Stmt
        (Node : Bare_With_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_With_Stmt;


   


      function Trace_Image (R : Internal_Entity_With_Stmt) return String;


         

      

      type Internal_Entity_Xor_Expr is record

               Node : aliased Bare_Xor_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Xor_Expr : constant Internal_Entity_Xor_Expr;


      function Create_Internal_Entity_Xor_Expr
        (Node : Bare_Xor_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Xor_Expr;


   


      function Trace_Image (R : Internal_Entity_Xor_Expr) return String;


         

      

      type Internal_Entity_Yield_Expr is record

               Node : aliased Bare_Yield_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Yield_Expr : constant Internal_Entity_Yield_Expr;


      function Create_Internal_Entity_Yield_Expr
        (Node : Bare_Yield_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Yield_Expr;


   


      function Trace_Image (R : Internal_Entity_Yield_Expr) return String;


         

      

      type Internal_Env_Assoc is record

               Key : aliased Symbol_Type;
               
               
               Val : aliased Bare_Turkixir_Node;
               
               
               Dest_Env : aliased Internal_DesignatedEnv;
               
               
               Metadata : aliased Internal_Metadata;
               
               
      end record
        with Convention => C;
      No_Env_Assoc : constant Internal_Env_Assoc;

      procedure Inc_Ref (R : Internal_Env_Assoc);
      procedure Dec_Ref (R : in out Internal_Env_Assoc);


      function Equivalent (L, R : Internal_Env_Assoc) return Boolean;

   


      function Trace_Image (R : Internal_Env_Assoc) return String;



   -----------------
   -- Array types --
   -----------------

   --  We implement array types as discriminated records so that binding to C
   --  can be done without copy.

         

   

   type Internal_Bare_Turkixir_Node_Array is
      array (Positive range <>) of Bare_Turkixir_Node;

   type Bare_Turkixir_Node_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Bare_Turkixir_Node_Array (1 .. N);
   end record;

   Empty_Bare_Turkixir_Node_Array_Record : aliased Bare_Turkixir_Node_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Bare_Turkixir_Node_Array_Type : constant Bare_Turkixir_Node_Array_Access :=
      Empty_Bare_Turkixir_Node_Array_Record'Access;


   function Create_Bare_Turkixir_Node_Array (Items_Count : Natural) return Bare_Turkixir_Node_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Bare_Turkixir_Node_Array
     (Items : Internal_Bare_Turkixir_Node_Array) return Bare_Turkixir_Node_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Bare_Turkixir_Node_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Bare_Turkixir_Node;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Bare_Turkixir_Node_Array_Access) return Bare_Turkixir_Node_Array_Access;


   function Length (T : Bare_Turkixir_Node_Array_Access) return Natural;

   procedure Inc_Ref (T : Bare_Turkixir_Node_Array_Access);
   procedure Dec_Ref (T : in out Bare_Turkixir_Node_Array_Access);

   function Equivalent (L, R : Bare_Turkixir_Node_Array_Access) return Boolean;


      function Trace_Image (A : Bare_Turkixir_Node_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Bare_Turkixir_Node_Array_Record, Bare_Turkixir_Node_Array_Access);

         

   

   type Internal_Internal_Entity_Array is
      array (Positive range <>) of Internal_Entity;

   type Internal_Entity_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Entity_Array (1 .. N);
   end record;

   Empty_Internal_Entity_Array_Record : aliased Internal_Entity_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Array_Type : constant Internal_Entity_Array_Access :=
      Empty_Internal_Entity_Array_Record'Access;

   function Create_Internal_Entity_Array
     (Items : AST_Envs.Entity_Array) return Internal_Entity_Array_Access;

   function Create_Internal_Entity_Array (Items_Count : Natural) return Internal_Entity_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Array
     (Items : Internal_Internal_Entity_Array) return Internal_Entity_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Entity_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Entity_Array_Access) return Internal_Entity_Array_Access;


   function Length (T : Internal_Entity_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Array_Access);

   function Equivalent (L, R : Internal_Entity_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Entity_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Entity_Array_Record, Internal_Entity_Array_Access);

         

   

   type Internal_Internal_Env_Assoc_Array is
      array (Positive range <>) of Internal_Env_Assoc;

   type Internal_Env_Assoc_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Env_Assoc_Array (1 .. N);
   end record;

   Empty_Internal_Env_Assoc_Array_Record : aliased Internal_Env_Assoc_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Env_Assoc_Array_Type : constant Internal_Env_Assoc_Array_Access :=
      Empty_Internal_Env_Assoc_Array_Record'Access;


   function Create_Internal_Env_Assoc_Array (Items_Count : Natural) return Internal_Env_Assoc_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Env_Assoc_Array
     (Items : Internal_Internal_Env_Assoc_Array) return Internal_Env_Assoc_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Internal_Env_Assoc_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Env_Assoc;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Env_Assoc_Array_Access) return Internal_Env_Assoc_Array_Access;


   function Length (T : Internal_Env_Assoc_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Env_Assoc_Array_Access);
   procedure Dec_Ref (T : in out Internal_Env_Assoc_Array_Access);

   function Equivalent (L, R : Internal_Env_Assoc_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Env_Assoc_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Env_Assoc_Array_Record, Internal_Env_Assoc_Array_Access);

         

   

   type Internal_Lexical_Env_Array is
      array (Positive range <>) of Lexical_Env;

   type Lexical_Env_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Lexical_Env_Array (1 .. N);
   end record;

   Empty_Lexical_Env_Array_Record : aliased Lexical_Env_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Lexical_Env_Array_Type : constant Lexical_Env_Array_Access :=
      Empty_Lexical_Env_Array_Record'Access;


   function Create_Lexical_Env_Array (Items_Count : Natural) return Lexical_Env_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Lexical_Env_Array
     (Items : Internal_Lexical_Env_Array) return Lexical_Env_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Lexical_Env_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Lexical_Env;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Lexical_Env_Array_Access) return Lexical_Env_Array_Access;


   function Length (T : Lexical_Env_Array_Access) return Natural;

   procedure Inc_Ref (T : Lexical_Env_Array_Access);
   procedure Dec_Ref (T : in out Lexical_Env_Array_Access);

   function Equivalent (L, R : Lexical_Env_Array_Access) return Boolean;


      function Trace_Image (A : Lexical_Env_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Lexical_Env_Array_Record, Lexical_Env_Array_Access);

         

   

   type Internal_Symbol_Type_Array is
      array (Positive range <>) of Symbol_Type;

   type Symbol_Type_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Symbol_Type_Array (1 .. N);
   end record;

   Empty_Symbol_Type_Array_Record : aliased Symbol_Type_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Symbol_Type_Array_Type : constant Symbol_Type_Array_Access :=
      Empty_Symbol_Type_Array_Record'Access;


   function Create_Symbol_Type_Array (Items_Count : Natural) return Symbol_Type_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Symbol_Type_Array
     (Items : Internal_Symbol_Type_Array) return Symbol_Type_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (T       : Symbol_Type_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Symbol_Type;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Symbol_Type_Array_Access) return Symbol_Type_Array_Access;


   function Length (T : Symbol_Type_Array_Access) return Natural;

   procedure Inc_Ref (T : Symbol_Type_Array_Access);
   procedure Dec_Ref (T : in out Symbol_Type_Array_Access);

   function Equivalent (L, R : Symbol_Type_Array_Access) return Boolean;


      function Trace_Image (A : Symbol_Type_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Symbol_Type_Array_Record, Symbol_Type_Array_Access);


   --------------------
   -- Iterator types --
   --------------------

         

   

   type Internal_Bare_Turkixir_Node_Iterator is record
      Ref_Count : Integer;
      --  Reference count. The iterator is freed when this drops to zero.
      --  Negative values are interpreted as "always living singleton".

      Safety_Net : Iterator_Safety_Net;
      --  Safety net for the iterator. Used to check that values produced by
      --  the iterator are still valid. Unlike for other types, we put the
      --  safety net in the internal type so that it can be used in all other
      --  APIs (Python, ...).
      --
      --  While other types (except nodes) are "deeply" converted to native
      --  APIs (for instance: internal arrays are turned into native Python
      --  lists, likewise for array items, etc.), iterators are lazy, so the
      --  deep conversion is not possible.

      Elements : Bare_Turkixir_Node_Array_Access;
      Index    : Positive;
   end record;

   Empty_Internal_Bare_Turkixir_Node_Iterator : aliased Internal_Bare_Turkixir_Node_Iterator :=
     (Ref_Count  => -1,
      Safety_Net => No_Iterator_Safety_Net,
      Elements   => No_Bare_Turkixir_Node_Array_Type,
      Index      => 1);
   No_Bare_Turkixir_Node_Iterator_Type : constant Bare_Turkixir_Node_Iterator_Access :=
      Empty_Internal_Bare_Turkixir_Node_Iterator'Access;

   function Next
     (T       : Bare_Turkixir_Node_Iterator_Access;
      Element : out Bare_Turkixir_Node) return Boolean;

   procedure Inc_Ref (T : Bare_Turkixir_Node_Iterator_Access);
   procedure Dec_Ref (T : in out Bare_Turkixir_Node_Iterator_Access);

      function Trace_Image (A : Bare_Turkixir_Node_Iterator_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Bare_Turkixir_Node_Iterator, Bare_Turkixir_Node_Iterator_Access);

         

   

   type Internal_Internal_Entity_Iterator is record
      Ref_Count : Integer;
      --  Reference count. The iterator is freed when this drops to zero.
      --  Negative values are interpreted as "always living singleton".

      Safety_Net : Iterator_Safety_Net;
      --  Safety net for the iterator. Used to check that values produced by
      --  the iterator are still valid. Unlike for other types, we put the
      --  safety net in the internal type so that it can be used in all other
      --  APIs (Python, ...).
      --
      --  While other types (except nodes) are "deeply" converted to native
      --  APIs (for instance: internal arrays are turned into native Python
      --  lists, likewise for array items, etc.), iterators are lazy, so the
      --  deep conversion is not possible.

      Elements : Internal_Entity_Array_Access;
      Index    : Positive;
   end record;

   Empty_Internal_Internal_Entity_Iterator : aliased Internal_Internal_Entity_Iterator :=
     (Ref_Count  => -1,
      Safety_Net => No_Iterator_Safety_Net,
      Elements   => No_Internal_Entity_Array_Type,
      Index      => 1);
   No_Internal_Entity_Iterator_Type : constant Internal_Entity_Iterator_Access :=
      Empty_Internal_Internal_Entity_Iterator'Access;

   function Next
     (T       : Internal_Entity_Iterator_Access;
      Element : out Internal_Entity) return Boolean;

   procedure Inc_Ref (T : Internal_Entity_Iterator_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Iterator_Access);

      function Trace_Image (A : Internal_Entity_Iterator_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Internal_Entity_Iterator, Internal_Entity_Iterator_Access);


   ------------------------
   -- Named environments --
   ------------------------

   --  The goal of named environments is to provide a sound mechanism to
   --  associate nodes and environments across analysis units: nodes whose
   --  Self_Env comes from another unit ("foreign env"), environments whose
   --  parent comes from another unit (also foreign env), or that contain
   --  symbol/node mappings for nodes coming from other units ("foreign
   --  nodes").
   --
   --  This mechanism comes with the following requirements:
   --
   --  * Ensure that, after unit reparsing, all cross-unit associations are
   --    still valid. For instance, no node's Self_Env can refer to a lexical
   --    environment that has been deallocated.
   --
   --  * Ensure that regardless of the sequence of unit parsing/reparsing that
   --    led to a given set of units (considering only unit filename and source
   --    buffer), the node/env graph (i.e. the result of PLE) is always the
   --    same, i.e. make incremental PLE idempotent.
   --
   --  Note that even though the end goal for named envs is to replace the
   --  previous mechanism (proved to be unsound, as violating the second
   --  requirement), both still coexist during the transition period.
   --
   --  Here is how this mechanism works:
   --
   --  1. Environments can be assigned zero, one or several names (i.e. one or
   --     several symbols). Name(s) assignment happens at environment
   --     construction.
   --
   --  2. As a consequence, multiple environments can be associated to a given
   --     env name. Using a total and deterministic ordering predicate, only
   --     one of them is said to have "precedence": looking up an environment
   --     using that name will return this unique environment.
   --
   --  3. For a given env name, we keep track of all uses of the environment
   --     that is looked up by its name: environment parent link, symbol/node
   --     mapping addition, node's Self_Env assignment. This info is
   --     tracked using the Named_Env_Descriptor record type below, often
   --     abbreviated NED. Note that this tracking happens even when there is
   --     no environment associated to the env name, as we need to do such
   --     updates when an environment gets associated to that env name.
   --
   --  4. Unit reparsing can destroy existing environments and/or create new
   --     ones. This means that, depending on their "ranking" using the
   --     ordering predicate, environments can earn or lose precedence for a
   --     given name.
   --
   --  5. When the precedence changes for a given name, we use the info
   --     collected as per 3. to perform relocation: relevant environment
   --     parent links are updated, symbol/node mappings are removed from the
   --     env that lost precedence and added to the env that earned precedence,
   --     etc.

   --  Tables to populate lexical entries in named envs

   package NED_Assoc_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Internal_Map_Node_Vectors.Vector,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => Internal_Map_Node_Vectors."=");
   --  Symbol/lexical env entry mappings for a given named env descriptor.
   --  Symbols are not unique in all mappings, so the lexical env entries are
   --  stored in a vector.

   procedure Add
     (Self : in out NED_Assoc_Maps.Map;
      Key  : Symbol_Type;
      Node : AST_Envs.Internal_Map_Node);
   --  Add a symbol/lexical env entry mapping in Self

   procedure Remove
     (Self : in out NED_Assoc_Maps.Map;
      Key  : Symbol_Type;
      Node : Bare_Turkixir_Node);
   --  Remove a symbol/lexical env entry mapping from Self

   --  Global table for named environments

   package Sorted_Env_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Bare_Turkixir_Node,
      Element_Type => Lexical_Env);
   --  List of lexical environments, sorted by owning node. This means that the
   --  following must be true for all cursors in such maps::
   --
   --     Key (Cur) = Element (Cur).Env.Node

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Bare_Turkixir_Node,
      Hash                => Hash,
      Equivalent_Elements => "=");

   type Named_Env_Descriptor is record
      Name : Symbol_Type;
      --  Name corresponding to this descriptor. Useful during debugging.

      Envs : Sorted_Env_Maps.Map;
      --  For each env name, we can have one or several environments
      --  (concurrent definitions). Just like foreign nodes in lexical
      --  environments, we keep them sorted by node to preserve determinism:
      --  given a set of loaded units, we will always have the same set of
      --  name:env associations sorted in the same order and thus always the
      --  same results at lookup time.

      Env_With_Precedence : Lexical_Env;
      --  Named environment that has precedence for this name.
      --
      --  Most of the time, if Envs is empty, this is Empty_Env and otherwise,
      --  shortcut to Envs.First_Element. However, when a change in Envs
      --  invalidates Env_With_Precedence, we reset it to Empty_Env momentarily
      --  during PLE as a way to tag the temprorary inconsistency. Later on, we
      --  recompute it and perform the needed relocations.

      Foreign_Nodes : NED_Assoc_Maps.Map;
      --  This maps symbols to lists of env entries for all the foreign nodes
      --  in Env_With_Precedence.
      --
      --  This set allows efficient relocation of env entries when
      --  Env_With_Precedence changes.

      Foreign_Envs : Sorted_Env_Maps.Map;
      --  This maps the owning node to env mapping for all lexical environments
      --  whose parent must be Env_With_Precedence. Envs are indexed by owning
      --  node for quick lookup during updates.
      --
      --  This set allows efficient env parent link updates when
      --  Env_With_Precedence changes.

      Nodes_With_Foreign_Env : Node_Sets.Set;
      --  Set of nodes whose env (Self_Env) must be Env_With_Precedence.
      --
      --  This set allows efficient Self_Env updates when Env_With_Precedence
      --  changes.

      --  Note that during the updating process of a reparsed unit
      --  (Update_After_Reparse procedure), these data structures become
      --  temporarily inconsistent: Env_With_Precedence can become Empty_Env
      --  even though Envs is not empty.  This is fine, because when it does,
      --  Update_After_Reparse keeps track of it as to be updated
      --  (Named_Envs_Needing_Update map).
   end record;
   type Named_Env_Descriptor_Access is access Named_Env_Descriptor;
   procedure Destroy is new Ada.Unchecked_Deallocation
     (Named_Env_Descriptor, Named_Env_Descriptor_Access);

   package NED_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Named_Env_Descriptor_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");
   --  Context-wide table that tracks for all env names the set of lexical envs
   --  that define it.

   type Exiled_Entry_In_NED is record
      Named_Env : Named_Env_Descriptor_Access;
      --  Named env descriptor in which Node is registered

      Key : Symbol_Type;
      --  Key in that Env's internal map that leads to the env descriptor that
      --  contains Node.

      Node : Bare_Turkixir_Node;
      --  Exiled node
   end record;

   package Exiled_Entry_In_NED_Vectors is new
      Langkit_Support.Vectors (Exiled_Entry_In_NED);

   type Exiled_Env is record
      Named_Env : Named_Env_Descriptor_Access;
      --  Named env descriptor in which Env is registered

      Env : Lexical_Env;
      --  Exiled environment
   end record;

   package Exiled_Env_Vectors is new Langkit_Support.Vectors (Exiled_Env);

   type Named_Env_Pair is record
      Name : Symbol_Type;
      --  Name on the lexical environment

      Env  : Lexical_Env;
      --  Named lexical environment
   end record;

   package Named_Env_Vectors is new Langkit_Support.Vectors (Named_Env_Pair);

   --  High-level primitives to handle the life cycle of named environment

   function Get_Named_Env_Descriptor
     (Context : Internal_Context;
      Name    : Symbol_Type) return Named_Env_Descriptor_Access;
   --  Return the named env descriptor in Context corresponding to Name. Create
   --  it first, if needed.

   procedure Register_Named_Env
     (Context                   : Internal_Context;
      Name                      : Symbol_Type;
      Env                       : Lexical_Env;
      Named_Envs_Needing_Update : in out NED_Maps.Map);
   --  Register Name as the environment name for Env. If Env takes the
   --  precedence for this name, add Name/its named env descriptor to
   --  Named_Envs_Needing_Update.

   procedure Update_Named_Envs (Named_Envs : NED_Maps.Map);
   --  For each named environment in Named_Envs, update Env_With_Precedence and
   --  do the necessary adjustments: relocate exiled entries, etc.

   -------------------------------
   -- Tree traversal operations --
   -------------------------------

   Kind_To_Node_Children_Count : constant array (Turkixir_Node_Kind_Type) of Integer :=
     (Turkixir_Arg_Assoc => 2, 
Turkixir_Arg_Gen => 2, 
Turkixir_Kw_Args => 1, 
Turkixir_Var_Args => 1, 
Turkixir_As_Name_Node => 2, 
Turkixir_Comp_If => 2, 
Turkixir_Comp_Op_Kind_Diamond => 0, 
Turkixir_Comp_Op_Kind_Eq => 0, 
Turkixir_Comp_Op_Kind_Gt => 0, 
Turkixir_Comp_Op_Kind_Gte => 0, 
Turkixir_Comp_Op_Kind_In => 0, 
Turkixir_Comp_Op_Kind_Is => 0, 
Turkixir_Comp_Op_Kind_Isnot => 0, 
Turkixir_Comp_Op_Kind_Lt => 0, 
Turkixir_Comp_Op_Kind_Lte => 0, 
Turkixir_Comp_Op_Kind_Noteq => 0, 
Turkixir_Comp_Op_Kind_Notin => 0, 
Turkixir_Comp_For => 3, 
Turkixir_Comp_ForL => 3, 
Turkixir_Decorator => 2, 
Turkixir_Dict_Assoc => 2, 
Turkixir_Else_Part => 1, 
Turkixir_Except_Part => 2, 
Turkixir_And_Expr => 2, 
Turkixir_And_Op => 2, 
Turkixir_Arith_Expr => 3, 
Turkixir_Shift_Expr => 3, 
Turkixir_Term => 3, 
Turkixir_Call_Expr => 2, 
Turkixir_Comp_Op => 3, 
Turkixir_Concat_String_Lit => 2, 
Turkixir_Dict_Comp => 2, 
Turkixir_Dict_Lit => 1, 
Turkixir_Dot => 0, 
Turkixir_Ellipsis_Expr => 0, 
Turkixir_Factor => 2, 
Turkixir_If_Expr => 3, 
Turkixir_Inline_Eval => 1, 
Turkixir_Lambda_Def => 2, 
Turkixir_List_Comp => 2, 
Turkixir_List_Gen => 2, 
Turkixir_List_Lit => 1, 
Turkixir_Dotted_Name => 2, 
Turkixir_Id => 0, 
Turkixir_Not_Op => 1, 
Turkixir_Number_Lit => 0, 
Turkixir_Or_Expr => 2, 
Turkixir_Or_Op => 2, 
Turkixir_Power => 2, 
Turkixir_Set_Comp => 2, 
Turkixir_Set_Lit => 1, 
Turkixir_Slice_Expr => 2, 
Turkixir_Ext_Slice_Expr => 3, 
Turkixir_String_Lit => 0, 
Turkixir_Subscript_Expr => 2, 
Turkixir_Tuple_Lit => 1, 
Turkixir_Xor_Expr => 2, 
Turkixir_Yield_Expr => 1, 
Turkixir_File_Node => 1, 
Turkixir_Import_Star => 0, 
Turkixir_Kw_Args_Flag_Absent => 0, 
Turkixir_Kw_Args_Flag_Present => 0, 
Turkixir_NL => 0, 
Turkixir_Op => 0, 
Turkixir_Params => 1, 
Turkixir_Rel_Name => 2, 
Turkixir_Single_Param => 4, 
Turkixir_Assert_Stmt => 2, 
Turkixir_Assign_Stmt => 2, 
Turkixir_Aug_Assign_Stmt => 3, 
Turkixir_Break_Stmt => 0, 
Turkixir_Continue_Stmt => 0, 
Turkixir_Decorated => 2, 
Turkixir_Class_Def => 3, 
Turkixir_Func_Def => 3, 
Turkixir_Del_Stmt => 1, 
Turkixir_Elif_Branch => 2, 
Turkixir_Exec_Stmt => 2, 
Turkixir_For_Stmt => 4, 
Turkixir_Global_Stmt => 1, 
Turkixir_If_Stmt => 4, 
Turkixir_Import_From => 2, 
Turkixir_Import_Name => 1, 
Turkixir_Pass_Stmt => 0, 
Turkixir_Print_Stmt => 1, 
Turkixir_Raise_Stmt => 1, 
Turkixir_Return_Stmt => 1, 
Turkixir_Stream_Print_Stmt => 2, 
Turkixir_Try_Stmt => 4, 
Turkixir_While_Stmt => 3, 
Turkixir_With_Stmt => 2, 
Turkixir_Arg_List => -1, 
Turkixir_As_Name_Node_List => -1, 
Turkixir_Decorator_List => -1, 
Turkixir_Dict_Assoc_List => -1, 
Turkixir_Dot_List => -1, 
Turkixir_Elif_Branch_List => -1, 
Turkixir_Except_Part_List => -1, 
Turkixir_Expr_List => -1, 
Turkixir_Id_List => -1, 
Turkixir_NL_List => -1, 
Turkixir_Single_Param_List => -1, 
Turkixir_String_Lit_List => -1, 
Turkixir_Turkixir_Node_List => -1, 
Turkixir_Var_Args_Flag_Absent => 0, 
Turkixir_Var_Args_Flag_Present => 0);
   --  For each AST node kind, this array gives the number of AST node children
   --  it has. For AST node lists, this is -1 as this number varies from one
   --  list instance to another.

   function First_Child_Index (Node : Bare_Turkixir_Node) return Natural;
   --  Return the index of the first child Node has

   function Last_Child_Index (Node : Bare_Turkixir_Node) return Natural;
   --  Return the index of the last child Node has, or 0 if there is no child

   function Children_Count (Node : Bare_Turkixir_Node) return Natural;
   --  Return the number of children that Node has

   procedure Get_Child
     (Node            : Bare_Turkixir_Node;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Bare_Turkixir_Node);
   --  Return the Index'th child of node, storing it into Result.
   --
   --  Child indexing is 1-based. Store in Index_In_Bounds whether Node had
   --  such a child: if not (i.e. ``Index`` is out-of-bounds), set ``Result``
   --  to a null node.

   function Child
     (Node  : Bare_Turkixir_Node;
      Index : Positive) return Bare_Turkixir_Node;
   --  Return the Index'th child of Node, or null if Node has no such child

   function Children
     (Node : Bare_Turkixir_Node) return Internal_Bare_Turkixir_Node_Array;
   --  Return an array containing all the children of Node.
   --  This is an alternative to the Child/Children_Count pair, useful if you
   --  want the convenience of Ada arrays, and you don't care about the small
   --  performance hit of creating an array.

   function Parents
     (Node      : Bare_Turkixir_Node;
      With_Self : Boolean := True)
      return Bare_Turkixir_Node_Array_Access;
   --  Return an array that contains the lexical parents, this node included
   --  iff ``with_self`` is True. Nearer parents are first in the list.

   function Parent (Node : Bare_Turkixir_Node) return Bare_Turkixir_Node;

   function Fetch_Sibling
     (Node   : Bare_Turkixir_Node;
      Offset : Integer) return Bare_Turkixir_Node;
   function Fetch_Sibling
     (Node   : Bare_Turkixir_Node;
      E_Info : Internal_Entity_Info;
      Offset : Integer) return Internal_Entity;
   --  Assuming Node is the Nth child of its parent, return the (N + Offset)'th
   --  child of the same parent, or null/No_Entity if there is no such sibling.

   function Traverse
     (Node  : Bare_Turkixir_Node;
      Visit : access function (Node : Bare_Turkixir_Node) return Visit_Status)
      return Visit_Status;
   --  Given the parent node for a subtree, traverse all syntactic nodes of
   --  this tree, calling the given function on each node in prefix order (i.e.
   --  top-down). The order of traversing subtrees follows the order of
   --  declaration of the corresponding attributes in the grammar. The
   --  traversal is controlled as follows by the result returned by Visit:
   --
   --     Into   The traversal continues normally with the syntactic
   --            children of the node just processed.
   --
   --     Over   The children of the node just processed are skipped and
   --            excluded from the traversal, but otherwise processing
   --            continues elsewhere in the tree.
   --
   --     Stop   The entire traversal is immediately abandoned, and the
   --            original call to Traverse returns Stop.

   procedure Traverse
     (Node  : Bare_Turkixir_Node;
      Visit : access function (Node : Bare_Turkixir_Node)
                               return Visit_Status);
   --  This is the same as Traverse function except that no result is returned
   --  i.e. the Traverse function is called and the result is simply discarded.

   generic
      type Data_Type is private;
      Reset_After_Traversal : Boolean := False;
   function Traverse_With_Data
     (Node  : Bare_Turkixir_Node;
      Visit : access function (Node : Bare_Turkixir_Node;
                               Data : in out Data_Type)
                               return Visit_Status;
      Data  : in out Data_Type)
      return Visit_Status;
   --  This is the same as the first Traverse function except it accepts an
   --  argument that is passed to all Visit calls.
   --
   --  If Reset_After_Traversal is True, the Data formal is left unchanged when
   --  Traverse_With_Data returns no matter what Visit does. Visit can change
   --  it otherwise.

   ----------------------------------------
   -- Source location-related operations --
   ----------------------------------------

   function Sloc_Range
     (Node : Bare_Turkixir_Node) return Source_Location_Range;
   --  Return the source location range corresponding to the set of tokens from
   --  which Node was parsed.

   function Compare
     (Node : Bare_Turkixir_Node;
      Sloc : Source_Location) return Relative_Position;
   --  Compare Sloc to the sloc range of Node

   function Lookup
     (Node : Bare_Turkixir_Node;
      Sloc : Source_Location) return Bare_Turkixir_Node;
   --  Look for the bottom-most AST node whose sloc range contains Sloc. Return
   --  it, or null if no such node was found.

   function Compare
     (Left, Right : Bare_Turkixir_Node;
      Relation    : Comparison_Relation) return Boolean;
   --  If Left and Right don't belong to the same analysis units or if one of
   --  them is null, raise a Property_Error. Otherwise, return the comparison
   --  of their starting source location according to Relation.

   -------------------
   -- Debug helpers --
   -------------------

   procedure Print
     (Node        : Bare_Turkixir_Node;
      Show_Slocs  : Boolean;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children.
   --  Line_Prefix is prepended to each output line.

   procedure PP_Trivia
     (Node        : Bare_Turkixir_Node;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children along
   --  with the trivia associated to them. Line_Prefix is prepended to each
   --  output line.

   procedure Assign_Names_To_Logic_Vars (Node : Bare_Turkixir_Node);
   --  Debug helper: Assign names to every logical variable in the root node,
   --  so that we can trace logical variables.

   -------------------------------
   -- Root AST node (internals) --
   -------------------------------

   type Root_Node_Record (Kind : Turkixir_Node_Kind_Type) is record
      Parent : Bare_Turkixir_Node;
      --  Reference to the parent node, or null if this is the root one

      Unit : Internal_Unit;
      --  Reference to the analysis unit that owns this node

      Token_Start_Index : Token_Index;
      Token_End_Index   : Token_Index;
      --  Reference to the start and end token that constitutes this node. If
      --  this node is a ghost, Token_Start_Index is the token that this AST
      --  node relates to and Token_End_Index is No_Token_Index. Otherwise,
      --  both tokens are inclusive, i.e. they both belong to this node.

      Self_Env : Lexical_Env;
      --  Hold the environment this node defines, or the parent environment
      --  otherwise.

      Last_Attempted_Child : Integer;
      --  0-based index for the last child we tried to parse for this node. -1
      --  if parsing for all children was successful.

      

      
         



         


            case Kind is
                  when Turkixir_Arg =>
                     
         



         


            case Kind is
                  when Turkixir_Arg_Assoc_Range =>
                     
         


            Arg_Assoc_F_Name : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Arg_Assoc_F_Expr : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Arg_Gen_Range =>
                     
         


            Arg_Gen_F_Expr : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Arg_Gen_F_Comprehension : aliased Bare_Comp_For :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Kw_Args_Range =>
                     
         


            Kw_Args_F_Expr : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Var_Args_Range =>
                     
         


            Var_Args_F_Expr : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
               when others => null;
            end case;

      
                  when Turkixir_As_Name_Node_Range =>
                     
         


            As_Name_Node_F_Imported : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            As_Name_Node_F_As_Name : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Comp_If_Range =>
                     
         


            Comp_If_F_Test : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Comp_If_F_Comp : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Comp_Op_Kind =>
                     
         



         


            case Kind is
                  when Turkixir_Comp_Op_Kind_Diamond_Range =>
                     
         



         



            null;
      
                  when Turkixir_Comp_Op_Kind_Eq_Range =>
                     
         



         



            null;
      
                  when Turkixir_Comp_Op_Kind_Gt_Range =>
                     
         



         



            null;
      
                  when Turkixir_Comp_Op_Kind_Gte_Range =>
                     
         



         



            null;
      
                  when Turkixir_Comp_Op_Kind_In_Range =>
                     
         



         



            null;
      
                  when Turkixir_Comp_Op_Kind_Is_Range =>
                     
         



         



            null;
      
                  when Turkixir_Comp_Op_Kind_Isnot_Range =>
                     
         



         



            null;
      
                  when Turkixir_Comp_Op_Kind_Lt_Range =>
                     
         



         



            null;
      
                  when Turkixir_Comp_Op_Kind_Lte_Range =>
                     
         



         



            null;
      
                  when Turkixir_Comp_Op_Kind_Noteq_Range =>
                     
         



         



            null;
      
                  when Turkixir_Comp_Op_Kind_Notin_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Turkixir_Comprehension =>
                     
         



         


            case Kind is
                  when Turkixir_Comp_For_Range =>
                     
         


            Comp_For_F_Exprs : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;
            Comp_For_F_Target : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Comp_For_F_Comp : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Comp_ForL_Range =>
                     
         


            Comp_ForL_F_Exprs : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;
            Comp_ForL_F_Target : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;
            Comp_ForL_F_Comp : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;

         



      
               when others => null;
            end case;

      
                  when Turkixir_Decorator_Range =>
                     
         


            Decorator_F_Dec_Name : aliased Bare_Name :=
               No_Bare_Turkixir_Node;
            Decorator_F_Arg_List : aliased Bare_Arg_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Dict_Assoc_Range =>
                     
         


            Dict_Assoc_F_Key : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Dict_Assoc_F_Value : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Else_Part_Range =>
                     
         


            Else_Part_F_Statements : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Except_Part_Range =>
                     
         


            Except_Part_F_As_Name : aliased Bare_As_Name_Node :=
               No_Bare_Turkixir_Node;
            Except_Part_F_Statements : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Expr =>
                     
         



         


            case Kind is
                  when Turkixir_And_Expr_Range =>
                     
         


            And_Expr_F_Left : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            And_Expr_F_Right : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_And_Op_Range =>
                     
         


            And_Op_F_Left : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            And_Op_F_Right : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Bin_Op =>
                     
         


            Bin_Op_F_Left : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Bin_Op_F_Op : aliased Bare_Op :=
               No_Bare_Turkixir_Node;
            Bin_Op_F_Right : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         


            case Kind is
                  when Turkixir_Arith_Expr_Range =>
                     
         



         



            null;
      
                  when Turkixir_Shift_Expr_Range =>
                     
         



         



            null;
      
                  when Turkixir_Term_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Turkixir_Call_Expr_Range =>
                     
         


            Call_Expr_F_Prefix : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Call_Expr_F_Suffix : aliased Bare_Arg_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Comp_Op_Range =>
                     
         


            Comp_Op_F_Left : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Comp_Op_F_Op : aliased Bare_Comp_Op_Kind :=
               No_Bare_Turkixir_Node;
            Comp_Op_F_Right : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Concat_String_Lit_Range =>
                     
         


            Concat_String_Lit_F_First_Str : aliased Bare_String_Lit :=
               No_Bare_Turkixir_Node;
            Concat_String_Lit_F_Subsequent_Str : aliased Bare_String_Lit_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Dict_Comp_Range =>
                     
         


            Dict_Comp_F_Assoc : aliased Bare_Dict_Assoc :=
               No_Bare_Turkixir_Node;
            Dict_Comp_F_Comprehension : aliased Bare_Comp_For :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Dict_Lit_Range =>
                     
         


            Dict_Lit_F_Assocs : aliased Bare_Dict_Assoc_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Dot_Range =>
                     
         



         



            null;
      
                  when Turkixir_Ellipsis_Expr_Range =>
                     
         



         



            null;
      
                  when Turkixir_Factor_Range =>
                     
         


            Factor_F_Op : aliased Bare_Op :=
               No_Bare_Turkixir_Node;
            Factor_F_Expr : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_If_Expr_Range =>
                     
         


            If_Expr_F_Expr : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            If_Expr_F_Cond : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            If_Expr_F_Else_Expr : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Inline_Eval_Range =>
                     
         


            Inline_Eval_F_Exprs : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Lambda_Def_Range =>
                     
         


            Lambda_Def_F_Args : aliased Bare_Params :=
               No_Bare_Turkixir_Node;
            Lambda_Def_F_Expr : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_List_Comp_Range =>
                     
         


            List_Comp_F_Expr : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            List_Comp_F_Comprehension : aliased Bare_Comp_ForL :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_List_Gen_Range =>
                     
         


            List_Gen_F_Expr : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            List_Gen_F_Comprehension : aliased Bare_Comp_ForL :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_List_Lit_Range =>
                     
         


            List_Lit_F_Exprs : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Name =>
                     
         



         


            case Kind is
                  when Turkixir_Dotted_Name_Range =>
                     
         


            Dotted_Name_F_Prefix : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Dotted_Name_F_Suffix : aliased Bare_Id :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Id_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Turkixir_Not_Op_Range =>
                     
         


            Not_Op_F_Expr : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Number_Lit_Range =>
                     
         



         



            null;
      
                  when Turkixir_Or_Expr_Range =>
                     
         


            Or_Expr_F_Left : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Or_Expr_F_Right : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Or_Op_Range =>
                     
         


            Or_Op_F_Left : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Or_Op_F_Right : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Power_Range =>
                     
         


            Power_F_Left : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Power_F_Right : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Set_Comp_Range =>
                     
         


            Set_Comp_F_Expr : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Set_Comp_F_Comprehension : aliased Bare_Comp_For :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Set_Lit_Range =>
                     
         


            Set_Lit_F_Exprs : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Slice_Expr_Range =>
                     
         


            Slice_Expr_F_First : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Slice_Expr_F_Last : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         


            case Kind is
                  when Turkixir_Ext_Slice_Expr_Range =>
                     
         


            Ext_Slice_Expr_F_Stride : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
               when others => null;
            end case;

      
                  when Turkixir_String_Lit_Range =>
                     
         



         



            null;
      
                  when Turkixir_Subscript_Expr_Range =>
                     
         


            Subscript_Expr_F_Prefix : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Subscript_Expr_F_Suffix : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Tuple_Lit_Range =>
                     
         


            Tuple_Lit_F_Exprs : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Xor_Expr_Range =>
                     
         


            Xor_Expr_F_Left : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Xor_Expr_F_Right : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Yield_Expr_Range =>
                     
         


            Yield_Expr_F_Exprs : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;

         



      
               when others => null;
            end case;

      
                  when Turkixir_File_Node_Range =>
                     
         


            File_Node_F_Statements : aliased Bare_Turkixir_Node_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Import_Star_Range =>
                     
         



         



            null;
      
                  when Turkixir_Kw_Args_Flag =>
                     
         



         


            case Kind is
                  when Turkixir_Kw_Args_Flag_Absent_Range =>
                     
         



         



            null;
      
                  when Turkixir_Kw_Args_Flag_Present_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Turkixir_NL_Range =>
                     
         



         



            null;
      
                  when Turkixir_Op_Range =>
                     
         



         



            null;
      
                  when Turkixir_Params_Range =>
                     
         


            Params_F_Single_Params : aliased Bare_Single_Param_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Rel_Name_Range =>
                     
         


            Rel_Name_F_Dots : aliased Bare_Dot_List :=
               No_Bare_Turkixir_Node;
            Rel_Name_F_Name : aliased Bare_Name :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Single_Param_Range =>
                     
         


            Single_Param_F_Is_Varargs : aliased Bare_Var_Args_Flag :=
               No_Bare_Turkixir_Node;
            Single_Param_F_Is_Kwargs : aliased Bare_Kw_Args_Flag :=
               No_Bare_Turkixir_Node;
            Single_Param_F_Name : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;
            Single_Param_F_Default_Value : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Stmt =>
                     
         



         


            case Kind is
                  when Turkixir_Assert_Stmt_Range =>
                     
         


            Assert_Stmt_F_Test_Expr : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Assert_Stmt_F_Msg : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Assign_Stmt_Range =>
                     
         


            Assign_Stmt_F_L_Value : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;
            Assign_Stmt_F_R_Values : aliased Bare_Turkixir_Node_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Aug_Assign_Stmt_Range =>
                     
         


            Aug_Assign_Stmt_F_L_Value : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;
            Aug_Assign_Stmt_F_Op : aliased Bare_Op :=
               No_Bare_Turkixir_Node;
            Aug_Assign_Stmt_F_R_Value : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Break_Stmt_Range =>
                     
         



         



            null;
      
                  when Turkixir_Continue_Stmt_Range =>
                     
         



         



            null;
      
                  when Turkixir_Decorated_Range =>
                     
         


            Decorated_F_Decorators : aliased Bare_Decorator_List :=
               No_Bare_Turkixir_Node;
            Decorated_F_Defn : aliased Bare_Def_Stmt :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Def_Stmt =>
                     
         



         


            case Kind is
                  when Turkixir_Class_Def_Range =>
                     
         


            Class_Def_F_Name : aliased Bare_Id :=
               No_Bare_Turkixir_Node;
            Class_Def_F_Bases : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;
            Class_Def_F_Statements : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Func_Def_Range =>
                     
         


            Func_Def_F_Name : aliased Bare_Id :=
               No_Bare_Turkixir_Node;
            Func_Def_F_Parameters : aliased Bare_Params :=
               No_Bare_Turkixir_Node;
            Func_Def_F_Body : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;

         



      
               when others => null;
            end case;

      
                  when Turkixir_Del_Stmt_Range =>
                     
         


            Del_Stmt_F_Exprs : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Elif_Branch_Range =>
                     
         


            Elif_Branch_F_Cond_Test : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Elif_Branch_F_Statements : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Exec_Stmt_Range =>
                     
         


            Exec_Stmt_F_Expr : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Exec_Stmt_F_In_List : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_For_Stmt_Range =>
                     
         


            For_Stmt_F_Bindings : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;
            For_Stmt_F_Expr : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;
            For_Stmt_F_Statements : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;
            For_Stmt_F_Else_Part : aliased Bare_Else_Part :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Global_Stmt_Range =>
                     
         


            Global_Stmt_F_Names : aliased Bare_Id_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_If_Stmt_Range =>
                     
         


            If_Stmt_F_Cond_Test : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            If_Stmt_F_Statements : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;
            If_Stmt_F_Elif_Branchs : aliased Bare_Elif_Branch_List :=
               No_Bare_Turkixir_Node;
            If_Stmt_F_Else_Part : aliased Bare_Else_Part :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Import_From_Range =>
                     
         


            Import_From_F_Rel_Name : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;
            Import_From_F_Imported : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Import_Name_Range =>
                     
         


            Import_Name_F_Imported_Names : aliased Bare_Turkixir_Node_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Pass_Stmt_Range =>
                     
         



         



            null;
      
                  when Turkixir_Print_Stmt_Range =>
                     
         


            Print_Stmt_F_Exprs : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Raise_Stmt_Range =>
                     
         


            Raise_Stmt_F_Exprs : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Return_Stmt_Range =>
                     
         


            Return_Stmt_F_Exprs : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Stream_Print_Stmt_Range =>
                     
         


            Stream_Print_Stmt_F_Stream_Expr : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            Stream_Print_Stmt_F_Exprs : aliased Bare_Expr_List :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_Try_Stmt_Range =>
                     
         


            Try_Stmt_F_Statements : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;
            Try_Stmt_F_Except_Parts : aliased Bare_Except_Part_List :=
               No_Bare_Turkixir_Node;
            Try_Stmt_F_Else_Part : aliased Bare_Else_Part :=
               No_Bare_Turkixir_Node;
            Try_Stmt_F_Finally_Part : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_While_Stmt_Range =>
                     
         


            While_Stmt_F_Cond_Test : aliased Bare_Expr :=
               No_Bare_Turkixir_Node;
            While_Stmt_F_Statements : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;
            While_Stmt_F_Else_Part : aliased Bare_Else_Part :=
               No_Bare_Turkixir_Node;

         



      
                  when Turkixir_With_Stmt_Range =>
                     
         


            With_Stmt_F_Bindings : aliased Bare_As_Name_Node_List :=
               No_Bare_Turkixir_Node;
            With_Stmt_F_Statements : aliased Bare_Turkixir_Node :=
               No_Bare_Turkixir_Node;

         



      
               when others => null;
            end case;

      
                  when Turkixir_Turkixir_Node_Base_List =>
                     
         

            Count : Natural;
            Nodes : Alloc_AST_List_Array.Element_Array_Access;


         


            case Kind is
                  when Turkixir_Arg_List_Range =>
                     
         



         



            null;
      
                  when Turkixir_As_Name_Node_List_Range =>
                     
         



         



            null;
      
                  when Turkixir_Decorator_List_Range =>
                     
         



         



            null;
      
                  when Turkixir_Dict_Assoc_List_Range =>
                     
         



         



            null;
      
                  when Turkixir_Dot_List_Range =>
                     
         



         



            null;
      
                  when Turkixir_Elif_Branch_List_Range =>
                     
         



         



            null;
      
                  when Turkixir_Except_Part_List_Range =>
                     
         



         



            null;
      
                  when Turkixir_Expr_List_Range =>
                     
         



         



            null;
      
                  when Turkixir_Id_List_Range =>
                     
         



         



            null;
      
                  when Turkixir_NL_List_Range =>
                     
         



         



            null;
      
                  when Turkixir_Single_Param_List_Range =>
                     
         



         



            null;
      
                  when Turkixir_String_Lit_List_Range =>
                     
         



         



            null;
      
                  when Turkixir_Turkixir_Node_List_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Turkixir_Var_Args_Flag =>
                     
         



         


            case Kind is
                  when Turkixir_Var_Args_Flag_Absent_Range =>
                     
         



         



            null;
      
                  when Turkixir_Var_Args_Flag_Present_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
               when others => null;
            end case;

      
   end record;

   procedure Initialize
     (Self              : Bare_Turkixir_Node;
      Kind              : Turkixir_Node_Kind_Type;
      Unit              : Internal_Unit;
      Token_Start_Index : Token_Index;
      Token_End_Index   : Token_Index;
      Parent            : Bare_Turkixir_Node := null;
      Self_Env          : Lexical_Env := AST_Envs.Empty_Env);
   --  Helper for parsers, to initialize a freshly allocated node

   type PLE_Unit_State is record
      Named_Envs_Needing_Update : NED_Maps.Map;
      --  Set of named env entries whose Env_With_Precedence needs to be
      --  updated.
   end record;
   --  State of PLE on a specific unit

   type PLE_Unit_State_Access is access all PLE_Unit_State;

   type PLE_Node_State is record
      Unit_State : PLE_Unit_State_Access;
      --  State of PLE on the unit that owns this node

      Current_Env : Lexical_Env;
      --  Current environment when processing the node: initially inheritted
      --  from the Current_Env of the parent node (or Root_Scope on the root
      --  node), SetInitialEnv actions can change this.
      --
      --  Other environment actions such as AddEnv or AddToEnv can use this.

      Current_NED : Named_Env_Descriptor_Access;
      --  If the current environment was looked up by name, reference to the
      --  named environment descriptor. Null otherwise.
   end record;
   --  State of PLE on a specific node

   procedure Use_Direct_Env (State : in out PLE_Node_State; Env : Lexical_Env);
   --  Change State so that the current environment is Env, and record that it
   --  was *not* looked up by name.

   procedure Use_Named_Env
     (State   : in out PLE_Node_State;
      Context : Internal_Context;
      Name    : Symbol_Type);
   --  Change State so that the current environment comes from the named
   --  environment looked up with Name.

   procedure Set_Initial_Env
     (Self         : Bare_Turkixir_Node;
      State        : in out PLE_Node_State;
      Env          : Internal_DesignatedEnv;
      DSL_Location : String);
   --  Helper for ``Populate_Lexical_Env``: fetch the initial environment for
   --  ``Self`` according to ``Env`` and update ``State`` accordingly.

   procedure Add_To_Env
     (Self         : Bare_Turkixir_Node;
      State        : PLE_Node_State;
      Key          : Symbol_Type;
      Value        : Bare_Turkixir_Node;
      MD           : Internal_Metadata;
      Resolver     : Entity_Resolver;
      Dest_Env     : Internal_DesignatedEnv;
      DSL_Location : String);
   --  Helper for Populate_Lexical_Env: insert the Key/Value/MD/Resolver entry
   --  in the appropriate lexical env.
   --
   --  The destination environment is:
   --
   --  * If Dest_Env_Name is not null, this is the corresponding named
   --    environment.
   --
   --  * Otherwise, use Dest_Env_Fallback if is not the empty environment.
   --
   --  * Finally, use State's current environment.
   --
   --  If the destination environment is foreign and not fetched from its name
   --  while DSL_Location is not empty, raise a Property_Error.

   procedure Ref_Env
     (Self                : Bare_Turkixir_Node;
      Dest_Env            : Lexical_Env;
      Ref_Env_Nodes       : in out Bare_Turkixir_Node_Array_Access;
      Resolver            : Lexical_Env_Resolver;
      Kind                : Ref_Kind;
      Cats                : Ref_Categories;
      Shed_Rebindings     : Boolean);
   --  Helper for Populate_Lexical_Env: add referenced environments to
   --  Dest_Env. Calling this takes an ownership share for Ref_Env_Nodes.

   procedure Add_Env
     (Self              : Bare_Turkixir_Node;
      State             : in out PLE_Node_State;
      No_Parent         : Boolean;
      Transitive_Parent : Boolean;
      Names             : in out Symbol_Type_Array_Access);
   --  Helper for Populate_Lexical_Env: create a new environment for Self, and
   --  update State accordingly.
   --
   --  State and No_Parent participate to the computation of the parent for
   --  this new environment. Transitive_Parent is directly forwarded to the
   --  lexical environment constructor.
   --
   --  If Names is not null, this also registers the new environment as a named
   --  env for all the given names. For PLE code brevity, Add_Env takes care of
   --  freeing Names before returning.

   procedure Pre_Env_Actions
     (Self            : Bare_Turkixir_Node;
      State           : in out PLE_Node_State;
      Add_To_Env_Only : Boolean := False);
   --  Internal procedure that will execute all necessary lexical env actions
   --  for Node. This is meant to be called by Populate_Lexical_Env, and not by
   --  the user.

   procedure Post_Env_Actions
     (Self : Bare_Turkixir_Node; State : in out PLE_Node_State);
   --  Internal procedure that will execute all post add to env actions for
   --  Node. This is meant to be called by Populate_Lexical_Env.

   function Get_Symbol (Node : Bare_Turkixir_Node) return Symbol_Type
      with Pre => Node = null or else Is_Token_Node (Node);
   --  Assuming Node is a token node, return the corresponding symbol for the
   --  token it contains.

   function Image (Self : Symbol_Type) return String_Type;
   --  Transform a Symbol into an internal String

   function Text (Node : Bare_Turkixir_Node) return Text_Type;
   --  Retun the fragment of text from which Node was parsed

   ------------------------------
   -- Root AST node properties --
   ------------------------------


   -----------------------
   -- Generic list type --
   -----------------------


   function Length (Node : Bare_Turkixir_Node_Base_List) return Natural;

   function Children
     (Node : Bare_Turkixir_Node) return Bare_Turkixir_Node_Array_Access;
   --  Return an array containing all the children of Node.
   --  This is an alternative to the Child/Children_Count pair, useful if you
   --  want the convenience of ada arrays, and you don't care about the small
   --  performance hit of creating an array.

   function Item
     (Node  : Bare_Turkixir_Node_Base_List;
      Index : Positive) return Bare_Turkixir_Node renames Child;

   function Get
     (Node    : Bare_Turkixir_Node_Base_List;
      Index   : Integer;
      Or_Null : Boolean := False) return Bare_Turkixir_Node;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.

   procedure Free_User_Fields (Node : Bare_Turkixir_Node);
   --  Free resources associated to user fields in ``Node``

   procedure Set_Parents (Node, Parent : Bare_Turkixir_Node);
   --  Set Node.Parent to Parent, and initialize recursively the parent of all
   --  child nodes.

   procedure Destroy (Node : Bare_Turkixir_Node);
   --  Free the resources allocated to this node and all its children

   --------------------------------------
   -- Environments handling (internal) --
   --------------------------------------

   function Create_Static_Lexical_Env
     (Parent            : Lexical_Env;
      Node              : Bare_Turkixir_Node;
      Transitive_Parent : Boolean := False) return Lexical_Env;
   --  Wrapper around AST_Envs.Create_Lexical_Env. Create the environment and,
   --  if Node is not null, register the result for destruction in Node's
   --  analysis unit.

   function Get (A : AST_Envs.Entity_Array; Index : Integer) return Entity;
   --  Simple getter that raises Property_Error on out-of-bound accesses.
   --  Useful for code generation.

   function Group
     (Envs   : Lexical_Env_Array_Access;
      Env_Md : Internal_Metadata := No_Metadata) return Lexical_Env;
   --  Convenience wrapper for uniform types handling in code generation

   package Bare_Turkixir_Node_Vectors is
      new Langkit_Support.Vectors (Bare_Turkixir_Node);

   function Is_Visible_From
     (Referenced_Env, Base_Env : Lexical_Env) return Boolean;
   --  Return whether the unit that Referenced_Env belongs to is visible from
   --  the unit that Base_Env belongs to. If at least one of these two lexical
   --  environments does not belong to a particular analysis unit, this raises
   --  a Property_Error.

   function Populate_Lexical_Env (Node : Bare_Turkixir_Node) return Boolean;
   --  Populate the lexical environment for node and all its children. Return
   --  whether a Property_Error error occurred in the process.

   -----------------------------------
   -- Lexical utilities (internals) --
   -----------------------------------

   function Token
     (Node  : Bare_Turkixir_Node;
      Index : Token_Index) return Token_Reference;
   --  Helper for properties. This is used to turn token indexes as stored in
   --  AST nodes into Token_Reference values.

   function Stored_Token
     (Node  : Bare_Turkixir_Node;
      Token : Token_Reference) return Token_Index;
   --  Helper for properties. This is used to turn a Token_Reference value into
   --  a Token_Index value that can be stored as a field in Node. This raises a
   --  Property_Error if Node and Token don't belong to the same analysis unit
   --  or if Token is actually a Trivia.

   type Bare_Child_Record (Kind : Child_Or_Trivia := Child) is record
      case Kind is
         when Child =>
            Node : Bare_Turkixir_Node;
         when Trivia =>
            Trivia : Token_Reference;
      end case;
   end record;
   --  Variant that holds either an node or a token

   type Bare_Children_Array is array (Positive range <>) of Bare_Child_Record;

   function Children_And_Trivia
     (Node : Bare_Turkixir_Node) return Bare_Children_Array;
   --  Implementation for Analysis.Children_And_Trivia

      

   



         



 function Node_Env
   
  (Node : Bare_Turkixir_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  For nodes that introduce a new environment, return the parent lexical
--  environment. Return the "inherited" environment otherwise.

         



 function Children_Env
   
  (Node : Bare_Turkixir_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  For nodes that introduce a new environment, return it. Return the
--  "inherited" environment otherwise.

         



 function Parent
   
  (Node : Bare_Turkixir_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity
   ;
--  Return the syntactic parent for this node. Return null for the root node.

         



 function Parents
   
  (Node : Bare_Turkixir_Node
      ; With_Self : Boolean
         := True
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Array_Access
   ;
--  Return an array that contains the lexical parents, this node included iff
--  ``with_self`` is True. Nearer parents are first in the list.

         



 function Children
   
  (Node : Bare_Turkixir_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Array_Access
   ;
--  Return an array that contains the direct lexical children.
--
--  .. warning:: This constructs a whole array, and as such is much less
--     efficient than calling the :ada:ref:`Child` built-in.

         



 function Token_Start
   
  (Node : Bare_Turkixir_Node
  )

   return Token_Reference
   ;
--  Return the first token used to parse this node.

         



 function Token_End
   
  (Node : Bare_Turkixir_Node
  )

   return Token_Reference
   ;
--  Return the last token used to parse this node.

         



 function Child_Index
   
  (Node : Bare_Turkixir_Node
  )

   return Integer
   ;
--  Return the 0-based index for Node in its parent's children.

         



 function Previous_Sibling
   
  (Node : Bare_Turkixir_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity
   ;
--  Return the node's previous sibling, or null if there is no such sibling.

         



 function Next_Sibling
   
  (Node : Bare_Turkixir_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity
   ;
--  Return the node's next sibling, or null if there is no such sibling.

         



 function Unit
   
  (Node : Bare_Turkixir_Node
  )

   return Internal_Unit
   ;
--  Return the analysis unit owning this node.

         



 function Is_Ghost
   
  (Node : Bare_Turkixir_Node
  )

   return Boolean
   ;
--  Return whether the node is a ghost.
--
--  Unlike regular nodes, ghost nodes cover no token in the input source: they
--  are logically located instead between two tokens. Both the ``token_start``
--  and the ``token_end`` of all ghost nodes is the token right after this
--  logical position.

         



 function Text
   
  (Node : Bare_Turkixir_Node
  )

   return String_Type
   ;
--  Return the text corresponding to this node. Private property (for internal
--  DSL use).

         



 function Full_Sloc_Image
   
  (Node : Bare_Turkixir_Node
  )

   return String_Type
   ;
--  Return a string containing the filename + the sloc in GNU conformant
--  format. Useful to create diagnostics from a node.


   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Arg_Assoc
        (Self : Bare_Arg_Assoc
         ; Arg_Assoc_F_Name : Bare_Expr
         ; Arg_Assoc_F_Expr : Bare_Expr
        );

      
   function Arg_Assoc_F_Name
     (Node : Bare_Arg_Assoc) return Bare_Expr;

      
   function Arg_Assoc_F_Expr
     (Node : Bare_Arg_Assoc) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Arg_Gen
        (Self : Bare_Arg_Gen
         ; Arg_Gen_F_Expr : Bare_Expr
         ; Arg_Gen_F_Comprehension : Bare_Comp_For
        );

      
   function Arg_Gen_F_Expr
     (Node : Bare_Arg_Gen) return Bare_Expr;

      
   function Arg_Gen_F_Comprehension
     (Node : Bare_Arg_Gen) return Bare_Comp_For;



   




      

   

      
      procedure Initialize_Fields_For_Kw_Args
        (Self : Bare_Kw_Args
         ; Kw_Args_F_Expr : Bare_Expr
        );

      
   function Kw_Args_F_Expr
     (Node : Bare_Kw_Args) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Var_Args
        (Self : Bare_Var_Args
         ; Var_Args_F_Expr : Bare_Expr
        );

      
   function Var_Args_F_Expr
     (Node : Bare_Var_Args) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_As_Name_Node
        (Self : Bare_As_Name_Node
         ; As_Name_Node_F_Imported : Bare_Expr
         ; As_Name_Node_F_As_Name : Bare_Expr
        );

      
   function As_Name_Node_F_Imported
     (Node : Bare_As_Name_Node) return Bare_Expr;

      
   function As_Name_Node_F_As_Name
     (Node : Bare_As_Name_Node) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Comp_If
        (Self : Bare_Comp_If
         ; Comp_If_F_Test : Bare_Expr
         ; Comp_If_F_Comp : Bare_Turkixir_Node
        );

      
   function Comp_If_F_Test
     (Node : Bare_Comp_If) return Bare_Expr;

      
   function Comp_If_F_Comp
     (Node : Bare_Comp_If) return Bare_Turkixir_Node;



   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Comp_For
        (Self : Bare_Comp_For
         ; Comp_For_F_Exprs : Bare_Expr_List
         ; Comp_For_F_Target : Bare_Expr
         ; Comp_For_F_Comp : Bare_Turkixir_Node
        );

      
   function Comp_For_F_Exprs
     (Node : Bare_Comp_For) return Bare_Expr_List;

      
   function Comp_For_F_Target
     (Node : Bare_Comp_For) return Bare_Expr;

      
   function Comp_For_F_Comp
     (Node : Bare_Comp_For) return Bare_Turkixir_Node;



   




      

   

      
      procedure Initialize_Fields_For_Comp_ForL
        (Self : Bare_Comp_ForL
         ; Comp_ForL_F_Exprs : Bare_Expr_List
         ; Comp_ForL_F_Target : Bare_Expr_List
         ; Comp_ForL_F_Comp : Bare_Turkixir_Node
        );

      
   function Comp_ForL_F_Exprs
     (Node : Bare_Comp_ForL) return Bare_Expr_List;

      
   function Comp_ForL_F_Target
     (Node : Bare_Comp_ForL) return Bare_Expr_List;

      
   function Comp_ForL_F_Comp
     (Node : Bare_Comp_ForL) return Bare_Turkixir_Node;



   




      

   

      
      procedure Initialize_Fields_For_Decorator
        (Self : Bare_Decorator
         ; Decorator_F_Dec_Name : Bare_Name
         ; Decorator_F_Arg_List : Bare_Arg_List
        );

      
   function Decorator_F_Dec_Name
     (Node : Bare_Decorator) return Bare_Name;

      
   function Decorator_F_Arg_List
     (Node : Bare_Decorator) return Bare_Arg_List;



   




      

   

      
      procedure Initialize_Fields_For_Dict_Assoc
        (Self : Bare_Dict_Assoc
         ; Dict_Assoc_F_Key : Bare_Expr
         ; Dict_Assoc_F_Value : Bare_Expr
        );

      
   function Dict_Assoc_F_Key
     (Node : Bare_Dict_Assoc) return Bare_Expr;

      
   function Dict_Assoc_F_Value
     (Node : Bare_Dict_Assoc) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Else_Part
        (Self : Bare_Else_Part
         ; Else_Part_F_Statements : Bare_Turkixir_Node
        );

      
   function Else_Part_F_Statements
     (Node : Bare_Else_Part) return Bare_Turkixir_Node;



   




      

   

      
      procedure Initialize_Fields_For_Except_Part
        (Self : Bare_Except_Part
         ; Except_Part_F_As_Name : Bare_As_Name_Node
         ; Except_Part_F_Statements : Bare_Turkixir_Node
        );

      
   function Except_Part_F_As_Name
     (Node : Bare_Except_Part) return Bare_As_Name_Node;

      
   function Except_Part_F_Statements
     (Node : Bare_Except_Part) return Bare_Turkixir_Node;



   




      

   




   




      

   

      
      procedure Initialize_Fields_For_And_Expr
        (Self : Bare_And_Expr
         ; And_Expr_F_Left : Bare_Expr
         ; And_Expr_F_Right : Bare_Expr
        );

      
   function And_Expr_F_Left
     (Node : Bare_And_Expr) return Bare_Expr;

      
   function And_Expr_F_Right
     (Node : Bare_And_Expr) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_And_Op
        (Self : Bare_And_Op
         ; And_Op_F_Left : Bare_Expr
         ; And_Op_F_Right : Bare_Expr
        );

      
   function And_Op_F_Left
     (Node : Bare_And_Op) return Bare_Expr;

      
   function And_Op_F_Right
     (Node : Bare_And_Op) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Bin_Op
        (Self : Bare_Bin_Op
         ; Bin_Op_F_Left : Bare_Expr
         ; Bin_Op_F_Op : Bare_Op
         ; Bin_Op_F_Right : Bare_Expr
        );

      
   function Bin_Op_F_Left
     (Node : Bare_Bin_Op) return Bare_Expr;

      
   function Bin_Op_F_Op
     (Node : Bare_Bin_Op) return Bare_Op;

      
   function Bin_Op_F_Right
     (Node : Bare_Bin_Op) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Arith_Expr
        (Self : Bare_Arith_Expr
         ; Bin_Op_F_Left : Bare_Expr
         ; Bin_Op_F_Op : Bare_Op
         ; Bin_Op_F_Right : Bare_Expr
        );



   




      

   

      
      procedure Initialize_Fields_For_Shift_Expr
        (Self : Bare_Shift_Expr
         ; Bin_Op_F_Left : Bare_Expr
         ; Bin_Op_F_Op : Bare_Op
         ; Bin_Op_F_Right : Bare_Expr
        );



   




      

   

      
      procedure Initialize_Fields_For_Term
        (Self : Bare_Term
         ; Bin_Op_F_Left : Bare_Expr
         ; Bin_Op_F_Op : Bare_Op
         ; Bin_Op_F_Right : Bare_Expr
        );



   




      

   

      
      procedure Initialize_Fields_For_Call_Expr
        (Self : Bare_Call_Expr
         ; Call_Expr_F_Prefix : Bare_Expr
         ; Call_Expr_F_Suffix : Bare_Arg_List
        );

      
   function Call_Expr_F_Prefix
     (Node : Bare_Call_Expr) return Bare_Expr;

      
   function Call_Expr_F_Suffix
     (Node : Bare_Call_Expr) return Bare_Arg_List;



   




      

   

      
      procedure Initialize_Fields_For_Comp_Op
        (Self : Bare_Comp_Op
         ; Comp_Op_F_Left : Bare_Expr
         ; Comp_Op_F_Op : Bare_Comp_Op_Kind
         ; Comp_Op_F_Right : Bare_Expr
        );

      
   function Comp_Op_F_Left
     (Node : Bare_Comp_Op) return Bare_Expr;

      
   function Comp_Op_F_Op
     (Node : Bare_Comp_Op) return Bare_Comp_Op_Kind;

      
   function Comp_Op_F_Right
     (Node : Bare_Comp_Op) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Concat_String_Lit
        (Self : Bare_Concat_String_Lit
         ; Concat_String_Lit_F_First_Str : Bare_String_Lit
         ; Concat_String_Lit_F_Subsequent_Str : Bare_String_Lit_List
        );

      
   function Concat_String_Lit_F_First_Str
     (Node : Bare_Concat_String_Lit) return Bare_String_Lit;

      
   function Concat_String_Lit_F_Subsequent_Str
     (Node : Bare_Concat_String_Lit) return Bare_String_Lit_List;



   




      

   

      
      procedure Initialize_Fields_For_Dict_Comp
        (Self : Bare_Dict_Comp
         ; Dict_Comp_F_Assoc : Bare_Dict_Assoc
         ; Dict_Comp_F_Comprehension : Bare_Comp_For
        );

      
   function Dict_Comp_F_Assoc
     (Node : Bare_Dict_Comp) return Bare_Dict_Assoc;

      
   function Dict_Comp_F_Comprehension
     (Node : Bare_Dict_Comp) return Bare_Comp_For;



   




      

   

      
      procedure Initialize_Fields_For_Dict_Lit
        (Self : Bare_Dict_Lit
         ; Dict_Lit_F_Assocs : Bare_Dict_Assoc_List
        );

      
   function Dict_Lit_F_Assocs
     (Node : Bare_Dict_Lit) return Bare_Dict_Assoc_List;



   




      

   




   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Factor
        (Self : Bare_Factor
         ; Factor_F_Op : Bare_Op
         ; Factor_F_Expr : Bare_Expr
        );

      
   function Factor_F_Op
     (Node : Bare_Factor) return Bare_Op;

      
   function Factor_F_Expr
     (Node : Bare_Factor) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_If_Expr
        (Self : Bare_If_Expr
         ; If_Expr_F_Expr : Bare_Expr
         ; If_Expr_F_Cond : Bare_Expr
         ; If_Expr_F_Else_Expr : Bare_Expr
        );

      
   function If_Expr_F_Expr
     (Node : Bare_If_Expr) return Bare_Expr;

      
   function If_Expr_F_Cond
     (Node : Bare_If_Expr) return Bare_Expr;

      
   function If_Expr_F_Else_Expr
     (Node : Bare_If_Expr) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Inline_Eval
        (Self : Bare_Inline_Eval
         ; Inline_Eval_F_Exprs : Bare_Expr_List
        );

      
   function Inline_Eval_F_Exprs
     (Node : Bare_Inline_Eval) return Bare_Expr_List;



   




      

   

      
      procedure Initialize_Fields_For_Lambda_Def
        (Self : Bare_Lambda_Def
         ; Lambda_Def_F_Args : Bare_Params
         ; Lambda_Def_F_Expr : Bare_Expr
        );

      
   function Lambda_Def_F_Args
     (Node : Bare_Lambda_Def) return Bare_Params;

      
   function Lambda_Def_F_Expr
     (Node : Bare_Lambda_Def) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_List_Comp
        (Self : Bare_List_Comp
         ; List_Comp_F_Expr : Bare_Expr
         ; List_Comp_F_Comprehension : Bare_Comp_ForL
        );

      
   function List_Comp_F_Expr
     (Node : Bare_List_Comp) return Bare_Expr;

      
   function List_Comp_F_Comprehension
     (Node : Bare_List_Comp) return Bare_Comp_ForL;



   




      

   

      
      procedure Initialize_Fields_For_List_Gen
        (Self : Bare_List_Gen
         ; List_Gen_F_Expr : Bare_Expr
         ; List_Gen_F_Comprehension : Bare_Comp_ForL
        );

      
   function List_Gen_F_Expr
     (Node : Bare_List_Gen) return Bare_Expr;

      
   function List_Gen_F_Comprehension
     (Node : Bare_List_Gen) return Bare_Comp_ForL;



   




      

   

      
      procedure Initialize_Fields_For_List_Lit
        (Self : Bare_List_Lit
         ; List_Lit_F_Exprs : Bare_Expr_List
        );

      
   function List_Lit_F_Exprs
     (Node : Bare_List_Lit) return Bare_Expr_List;



   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Dotted_Name
        (Self : Bare_Dotted_Name
         ; Dotted_Name_F_Prefix : Bare_Expr
         ; Dotted_Name_F_Suffix : Bare_Id
        );

      
   function Dotted_Name_F_Prefix
     (Node : Bare_Dotted_Name) return Bare_Expr;

      
   function Dotted_Name_F_Suffix
     (Node : Bare_Dotted_Name) return Bare_Id;



   




      

   



         



 function Id_P_Sym
   
  (Node : Bare_Id
  )

   return Symbol_Type
   ;
--  Shortcut to get the symbol of this node


   




      

   

      
      procedure Initialize_Fields_For_Not_Op
        (Self : Bare_Not_Op
         ; Not_Op_F_Expr : Bare_Expr
        );

      
   function Not_Op_F_Expr
     (Node : Bare_Not_Op) return Bare_Expr;



   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Or_Expr
        (Self : Bare_Or_Expr
         ; Or_Expr_F_Left : Bare_Expr
         ; Or_Expr_F_Right : Bare_Expr
        );

      
   function Or_Expr_F_Left
     (Node : Bare_Or_Expr) return Bare_Expr;

      
   function Or_Expr_F_Right
     (Node : Bare_Or_Expr) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Or_Op
        (Self : Bare_Or_Op
         ; Or_Op_F_Left : Bare_Expr
         ; Or_Op_F_Right : Bare_Expr
        );

      
   function Or_Op_F_Left
     (Node : Bare_Or_Op) return Bare_Expr;

      
   function Or_Op_F_Right
     (Node : Bare_Or_Op) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Power
        (Self : Bare_Power
         ; Power_F_Left : Bare_Expr
         ; Power_F_Right : Bare_Expr
        );

      
   function Power_F_Left
     (Node : Bare_Power) return Bare_Expr;

      
   function Power_F_Right
     (Node : Bare_Power) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Set_Comp
        (Self : Bare_Set_Comp
         ; Set_Comp_F_Expr : Bare_Expr
         ; Set_Comp_F_Comprehension : Bare_Comp_For
        );

      
   function Set_Comp_F_Expr
     (Node : Bare_Set_Comp) return Bare_Expr;

      
   function Set_Comp_F_Comprehension
     (Node : Bare_Set_Comp) return Bare_Comp_For;



   




      

   

      
      procedure Initialize_Fields_For_Set_Lit
        (Self : Bare_Set_Lit
         ; Set_Lit_F_Exprs : Bare_Expr_List
        );

      
   function Set_Lit_F_Exprs
     (Node : Bare_Set_Lit) return Bare_Expr_List;



   




      

   

      
      procedure Initialize_Fields_For_Slice_Expr
        (Self : Bare_Slice_Expr
         ; Slice_Expr_F_First : Bare_Expr
         ; Slice_Expr_F_Last : Bare_Expr
        );

      
   function Slice_Expr_F_First
     (Node : Bare_Slice_Expr) return Bare_Expr;

      
   function Slice_Expr_F_Last
     (Node : Bare_Slice_Expr) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Ext_Slice_Expr
        (Self : Bare_Ext_Slice_Expr
         ; Slice_Expr_F_First : Bare_Expr
         ; Slice_Expr_F_Last : Bare_Expr
         ; Ext_Slice_Expr_F_Stride : Bare_Expr
        );

      
   function Ext_Slice_Expr_F_Stride
     (Node : Bare_Ext_Slice_Expr) return Bare_Expr;



   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Subscript_Expr
        (Self : Bare_Subscript_Expr
         ; Subscript_Expr_F_Prefix : Bare_Expr
         ; Subscript_Expr_F_Suffix : Bare_Expr_List
        );

      
   function Subscript_Expr_F_Prefix
     (Node : Bare_Subscript_Expr) return Bare_Expr;

      
   function Subscript_Expr_F_Suffix
     (Node : Bare_Subscript_Expr) return Bare_Expr_List;



   




      

   

      
      procedure Initialize_Fields_For_Tuple_Lit
        (Self : Bare_Tuple_Lit
         ; Tuple_Lit_F_Exprs : Bare_Expr_List
        );

      
   function Tuple_Lit_F_Exprs
     (Node : Bare_Tuple_Lit) return Bare_Expr_List;



   




      

   

      
      procedure Initialize_Fields_For_Xor_Expr
        (Self : Bare_Xor_Expr
         ; Xor_Expr_F_Left : Bare_Expr
         ; Xor_Expr_F_Right : Bare_Expr
        );

      
   function Xor_Expr_F_Left
     (Node : Bare_Xor_Expr) return Bare_Expr;

      
   function Xor_Expr_F_Right
     (Node : Bare_Xor_Expr) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Yield_Expr
        (Self : Bare_Yield_Expr
         ; Yield_Expr_F_Exprs : Bare_Expr_List
        );

      
   function Yield_Expr_F_Exprs
     (Node : Bare_Yield_Expr) return Bare_Expr_List;



   




      

   

      
      procedure Initialize_Fields_For_File_Node
        (Self : Bare_File_Node
         ; File_Node_F_Statements : Bare_Turkixir_Node_List
        );

      
   function File_Node_F_Statements
     (Node : Bare_File_Node) return Bare_Turkixir_Node_List;



   




      

   




   




      

   



         



 function Dispatcher_Kw_Args_Flag_P_As_Bool
   
  (Node : Bare_Kw_Args_Flag
  )

   return Boolean
   ;
--  Return whether this is an instance of KwArgsFlagPresent


   




      

   



         



 function Kw_Args_Flag_Absent_P_As_Bool
   
  (Node : Bare_Kw_Args_Flag_Absent
  )

   return Boolean
   ;



   




      

   



         



 function Kw_Args_Flag_Present_P_As_Bool
   
  (Node : Bare_Kw_Args_Flag_Present
  )

   return Boolean
   ;



   




      

   




   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Params
        (Self : Bare_Params
         ; Params_F_Single_Params : Bare_Single_Param_List
        );

      
   function Params_F_Single_Params
     (Node : Bare_Params) return Bare_Single_Param_List;



   




      

   

      
      procedure Initialize_Fields_For_Rel_Name
        (Self : Bare_Rel_Name
         ; Rel_Name_F_Dots : Bare_Dot_List
         ; Rel_Name_F_Name : Bare_Name
        );

      
   function Rel_Name_F_Dots
     (Node : Bare_Rel_Name) return Bare_Dot_List;

      
   function Rel_Name_F_Name
     (Node : Bare_Rel_Name) return Bare_Name;



   




      

   

      
      procedure Initialize_Fields_For_Single_Param
        (Self : Bare_Single_Param
         ; Single_Param_F_Is_Varargs : Bare_Var_Args_Flag
         ; Single_Param_F_Is_Kwargs : Bare_Kw_Args_Flag
         ; Single_Param_F_Name : Bare_Turkixir_Node
         ; Single_Param_F_Default_Value : Bare_Expr
        );

      
   function Single_Param_F_Is_Varargs
     (Node : Bare_Single_Param) return Bare_Var_Args_Flag;

      
   function Single_Param_F_Is_Kwargs
     (Node : Bare_Single_Param) return Bare_Kw_Args_Flag;

      
   function Single_Param_F_Name
     (Node : Bare_Single_Param) return Bare_Turkixir_Node;

      
   function Single_Param_F_Default_Value
     (Node : Bare_Single_Param) return Bare_Expr;


         



 function Env_Mappings_1
   
  (Node : Bare_Single_Param
  )

   return Internal_Env_Assoc_Array_Access
   ;



   



         procedure Single_Param_Pre_Env_Actions
           (Self            : Bare_Single_Param;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);




      

   




   




      

   

      
      procedure Initialize_Fields_For_Assert_Stmt
        (Self : Bare_Assert_Stmt
         ; Assert_Stmt_F_Test_Expr : Bare_Expr
         ; Assert_Stmt_F_Msg : Bare_Expr
        );

      
   function Assert_Stmt_F_Test_Expr
     (Node : Bare_Assert_Stmt) return Bare_Expr;

      
   function Assert_Stmt_F_Msg
     (Node : Bare_Assert_Stmt) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Assign_Stmt
        (Self : Bare_Assign_Stmt
         ; Assign_Stmt_F_L_Value : Bare_Expr_List
         ; Assign_Stmt_F_R_Values : Bare_Turkixir_Node_List
        );

      
   function Assign_Stmt_F_L_Value
     (Node : Bare_Assign_Stmt) return Bare_Expr_List;

      
   function Assign_Stmt_F_R_Values
     (Node : Bare_Assign_Stmt) return Bare_Turkixir_Node_List;


         



 function Env_Mappings_2
   
  (Node : Bare_Assign_Stmt
  )

   return Internal_Env_Assoc_Array_Access
   ;



   



         procedure Assign_Stmt_Pre_Env_Actions
           (Self            : Bare_Assign_Stmt;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);




      

   

      
      procedure Initialize_Fields_For_Aug_Assign_Stmt
        (Self : Bare_Aug_Assign_Stmt
         ; Aug_Assign_Stmt_F_L_Value : Bare_Expr_List
         ; Aug_Assign_Stmt_F_Op : Bare_Op
         ; Aug_Assign_Stmt_F_R_Value : Bare_Turkixir_Node
        );

      
   function Aug_Assign_Stmt_F_L_Value
     (Node : Bare_Aug_Assign_Stmt) return Bare_Expr_List;

      
   function Aug_Assign_Stmt_F_Op
     (Node : Bare_Aug_Assign_Stmt) return Bare_Op;

      
   function Aug_Assign_Stmt_F_R_Value
     (Node : Bare_Aug_Assign_Stmt) return Bare_Turkixir_Node;



   




      

   




   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Decorated
        (Self : Bare_Decorated
         ; Decorated_F_Decorators : Bare_Decorator_List
         ; Decorated_F_Defn : Bare_Def_Stmt
        );

      
   function Decorated_F_Decorators
     (Node : Bare_Decorated) return Bare_Decorator_List;

      
   function Decorated_F_Defn
     (Node : Bare_Decorated) return Bare_Def_Stmt;



   




      

   



         



 function Env_Trans_Parent_0
   
  (Node : Bare_Def_Stmt
  )

   return Boolean
   ;



   



         procedure Def_Stmt_Pre_Env_Actions
           (Self            : Bare_Def_Stmt;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);




      

   

      
      procedure Initialize_Fields_For_Class_Def
        (Self : Bare_Class_Def
         ; Class_Def_F_Name : Bare_Id
         ; Class_Def_F_Bases : Bare_Expr_List
         ; Class_Def_F_Statements : Bare_Turkixir_Node
        );

      
   function Class_Def_F_Name
     (Node : Bare_Class_Def) return Bare_Id;

      
   function Class_Def_F_Bases
     (Node : Bare_Class_Def) return Bare_Expr_List;

      
   function Class_Def_F_Statements
     (Node : Bare_Class_Def) return Bare_Turkixir_Node;



   




      

   

      
      procedure Initialize_Fields_For_Func_Def
        (Self : Bare_Func_Def
         ; Func_Def_F_Name : Bare_Id
         ; Func_Def_F_Parameters : Bare_Params
         ; Func_Def_F_Body : Bare_Turkixir_Node
        );

      
   function Func_Def_F_Name
     (Node : Bare_Func_Def) return Bare_Id;

      
   function Func_Def_F_Parameters
     (Node : Bare_Func_Def) return Bare_Params;

      
   function Func_Def_F_Body
     (Node : Bare_Func_Def) return Bare_Turkixir_Node;



   




      

   

      
      procedure Initialize_Fields_For_Del_Stmt
        (Self : Bare_Del_Stmt
         ; Del_Stmt_F_Exprs : Bare_Expr_List
        );

      
   function Del_Stmt_F_Exprs
     (Node : Bare_Del_Stmt) return Bare_Expr_List;



   




      

   

      
      procedure Initialize_Fields_For_Elif_Branch
        (Self : Bare_Elif_Branch
         ; Elif_Branch_F_Cond_Test : Bare_Expr
         ; Elif_Branch_F_Statements : Bare_Turkixir_Node
        );

      
   function Elif_Branch_F_Cond_Test
     (Node : Bare_Elif_Branch) return Bare_Expr;

      
   function Elif_Branch_F_Statements
     (Node : Bare_Elif_Branch) return Bare_Turkixir_Node;



   




      

   

      
      procedure Initialize_Fields_For_Exec_Stmt
        (Self : Bare_Exec_Stmt
         ; Exec_Stmt_F_Expr : Bare_Expr
         ; Exec_Stmt_F_In_List : Bare_Expr_List
        );

      
   function Exec_Stmt_F_Expr
     (Node : Bare_Exec_Stmt) return Bare_Expr;

      
   function Exec_Stmt_F_In_List
     (Node : Bare_Exec_Stmt) return Bare_Expr_List;



   




      

   

      
      procedure Initialize_Fields_For_For_Stmt
        (Self : Bare_For_Stmt
         ; For_Stmt_F_Bindings : Bare_Expr_List
         ; For_Stmt_F_Expr : Bare_Expr_List
         ; For_Stmt_F_Statements : Bare_Turkixir_Node
         ; For_Stmt_F_Else_Part : Bare_Else_Part
        );

      
   function For_Stmt_F_Bindings
     (Node : Bare_For_Stmt) return Bare_Expr_List;

      
   function For_Stmt_F_Expr
     (Node : Bare_For_Stmt) return Bare_Expr_List;

      
   function For_Stmt_F_Statements
     (Node : Bare_For_Stmt) return Bare_Turkixir_Node;

      
   function For_Stmt_F_Else_Part
     (Node : Bare_For_Stmt) return Bare_Else_Part;



   




      

   

      
      procedure Initialize_Fields_For_Global_Stmt
        (Self : Bare_Global_Stmt
         ; Global_Stmt_F_Names : Bare_Id_List
        );

      
   function Global_Stmt_F_Names
     (Node : Bare_Global_Stmt) return Bare_Id_List;



   




      

   

      
      procedure Initialize_Fields_For_If_Stmt
        (Self : Bare_If_Stmt
         ; If_Stmt_F_Cond_Test : Bare_Expr
         ; If_Stmt_F_Statements : Bare_Turkixir_Node
         ; If_Stmt_F_Elif_Branchs : Bare_Elif_Branch_List
         ; If_Stmt_F_Else_Part : Bare_Else_Part
        );

      
   function If_Stmt_F_Cond_Test
     (Node : Bare_If_Stmt) return Bare_Expr;

      
   function If_Stmt_F_Statements
     (Node : Bare_If_Stmt) return Bare_Turkixir_Node;

      
   function If_Stmt_F_Elif_Branchs
     (Node : Bare_If_Stmt) return Bare_Elif_Branch_List;

      
   function If_Stmt_F_Else_Part
     (Node : Bare_If_Stmt) return Bare_Else_Part;



   




      

   

      
      procedure Initialize_Fields_For_Import_From
        (Self : Bare_Import_From
         ; Import_From_F_Rel_Name : Bare_Turkixir_Node
         ; Import_From_F_Imported : Bare_Turkixir_Node
        );

      
   function Import_From_F_Rel_Name
     (Node : Bare_Import_From) return Bare_Turkixir_Node;

      
   function Import_From_F_Imported
     (Node : Bare_Import_From) return Bare_Turkixir_Node;



   




      

   

      
      procedure Initialize_Fields_For_Import_Name
        (Self : Bare_Import_Name
         ; Import_Name_F_Imported_Names : Bare_Turkixir_Node_List
        );

      
   function Import_Name_F_Imported_Names
     (Node : Bare_Import_Name) return Bare_Turkixir_Node_List;



   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Print_Stmt
        (Self : Bare_Print_Stmt
         ; Print_Stmt_F_Exprs : Bare_Expr_List
        );

      
   function Print_Stmt_F_Exprs
     (Node : Bare_Print_Stmt) return Bare_Expr_List;



   




      

   

      
      procedure Initialize_Fields_For_Raise_Stmt
        (Self : Bare_Raise_Stmt
         ; Raise_Stmt_F_Exprs : Bare_Expr_List
        );

      
   function Raise_Stmt_F_Exprs
     (Node : Bare_Raise_Stmt) return Bare_Expr_List;



   




      

   

      
      procedure Initialize_Fields_For_Return_Stmt
        (Self : Bare_Return_Stmt
         ; Return_Stmt_F_Exprs : Bare_Expr_List
        );

      
   function Return_Stmt_F_Exprs
     (Node : Bare_Return_Stmt) return Bare_Expr_List;



   




      

   

      
      procedure Initialize_Fields_For_Stream_Print_Stmt
        (Self : Bare_Stream_Print_Stmt
         ; Stream_Print_Stmt_F_Stream_Expr : Bare_Expr
         ; Stream_Print_Stmt_F_Exprs : Bare_Expr_List
        );

      
   function Stream_Print_Stmt_F_Stream_Expr
     (Node : Bare_Stream_Print_Stmt) return Bare_Expr;

      
   function Stream_Print_Stmt_F_Exprs
     (Node : Bare_Stream_Print_Stmt) return Bare_Expr_List;



   




      

   

      
      procedure Initialize_Fields_For_Try_Stmt
        (Self : Bare_Try_Stmt
         ; Try_Stmt_F_Statements : Bare_Turkixir_Node
         ; Try_Stmt_F_Except_Parts : Bare_Except_Part_List
         ; Try_Stmt_F_Else_Part : Bare_Else_Part
         ; Try_Stmt_F_Finally_Part : Bare_Turkixir_Node
        );

      
   function Try_Stmt_F_Statements
     (Node : Bare_Try_Stmt) return Bare_Turkixir_Node;

      
   function Try_Stmt_F_Except_Parts
     (Node : Bare_Try_Stmt) return Bare_Except_Part_List;

      
   function Try_Stmt_F_Else_Part
     (Node : Bare_Try_Stmt) return Bare_Else_Part;

      
   function Try_Stmt_F_Finally_Part
     (Node : Bare_Try_Stmt) return Bare_Turkixir_Node;



   




      

   

      
      procedure Initialize_Fields_For_While_Stmt
        (Self : Bare_While_Stmt
         ; While_Stmt_F_Cond_Test : Bare_Expr
         ; While_Stmt_F_Statements : Bare_Turkixir_Node
         ; While_Stmt_F_Else_Part : Bare_Else_Part
        );

      
   function While_Stmt_F_Cond_Test
     (Node : Bare_While_Stmt) return Bare_Expr;

      
   function While_Stmt_F_Statements
     (Node : Bare_While_Stmt) return Bare_Turkixir_Node;

      
   function While_Stmt_F_Else_Part
     (Node : Bare_While_Stmt) return Bare_Else_Part;



   




      

   

      
      procedure Initialize_Fields_For_With_Stmt
        (Self : Bare_With_Stmt
         ; With_Stmt_F_Bindings : Bare_As_Name_Node_List
         ; With_Stmt_F_Statements : Bare_Turkixir_Node
        );

      
   function With_Stmt_F_Bindings
     (Node : Bare_With_Stmt) return Bare_As_Name_Node_List;

      
   function With_Stmt_F_Statements
     (Node : Bare_With_Stmt) return Bare_Turkixir_Node;



   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   



         



 function Dispatcher_Var_Args_Flag_P_As_Bool
   
  (Node : Bare_Var_Args_Flag
  )

   return Boolean
   ;
--  Return whether this is an instance of VarArgsFlagPresent


   




      

   



         



 function Var_Args_Flag_Absent_P_As_Bool
   
  (Node : Bare_Var_Args_Flag_Absent
  )

   return Boolean
   ;



   




      

   



         



 function Var_Args_Flag_Present_P_As_Bool
   
  (Node : Bare_Var_Args_Flag_Present
  )

   return Boolean
   ;



   





   function "<" (Left, Right : Internal_Unit) return Boolean;

   type Exiled_Entry is record
      Env  : Lexical_Env;
      Key  : Symbol_Type;
      Node : Bare_Turkixir_Node;
   end record;
   --  Tuple of values passed to AST_Envs.Add. Used in the lexical
   --  environment rerooting machinery: see Remove_Exiled_Entries and
   --  Reroot_Foreign_Nodes.

   package Exiled_Entry_Vectors is new Langkit_Support.Vectors (Exiled_Entry);

   type Foreign_Node_Entry is record
      Node : Bare_Turkixir_Node;
      --  The foreign node that has been added to an analysis unit's lexical
      --  environment.

      Unit : Internal_Unit;
      --  Analysis unit that owns Node
   end record;

   package Foreign_Node_Entry_Vectors is new Langkit_Support.Vectors
     (Foreign_Node_Entry);

   procedure Register_Destroyable
     (Unit : Internal_Unit; Node : Bare_Turkixir_Node);
   --  Register Node to be destroyed when Unit is deallocated/reparsed

   procedure Register_Destroyable
     (Unit : Internal_Unit; Env : AST_Envs.Lexical_Env_Access);
   --  Register Env to be destroyed when Unit is deallocated/reparsed


   -----------------------------
   -- Miscellanous operations --
   -----------------------------

   type Destroy_Procedure is access procedure (Object : System.Address);

   type Destroyable_Type is record
      Object  : System.Address;
      --  Object to destroy

      Destroy : Destroy_Procedure;
      --  Procedure to destroy Object
   end record;
   --  Simple holder to associate an object to destroy and the procedure to
   --  perform the destruction.

   package Destroyable_Vectors is new Langkit_Support.Vectors
     (Destroyable_Type);

   package Analysis_Unit_Sets is new Langkit_Support.Cheap_Sets
     (Internal_Unit, null);

   package Units_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => GNATCOLL.VFS.Virtual_File,
      Element_Type    => Internal_Unit,
      Hash            => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Keys => GNATCOLL.VFS."=");

   function Token_Data (Unit : Internal_Unit) return Token_Data_Handler_Access;

   function Lookup_Symbol
     (Context : Internal_Context; Symbol : Text_Type) return Symbol_Type;
   --  Return the given symbol text as a symbol for this context. Raise an
   --  Invalid_Symbol_Error if it is invalid.

   function Create_Special_Unit
     (Context             : Internal_Context;
      Normalized_Filename : GNATCOLL.VFS.Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Internal_Unit;
   --  Create a new special analysis unit, i.e. a unit that is not registered
   --  in Context's unit map.

   function Templates_Unit (Context : Internal_Context) return Internal_Unit;
   --  Return the analysis unit to be used to parse tree rewriting templates.
   --  This creates it if it does not exists yet.

   procedure Set_Rule (Unit : Internal_Unit; Rule : Grammar_Rule);

   package Virtual_File_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => GNATCOLL.VFS.Virtual_File,
      Equivalent_Keys => "=",
      "="             => GNATCOLL.VFS."=",
      Hash            => Ada.Strings.Unbounded.Hash);

   function Normalized_Unit_Filename
     (Context : Internal_Context; Filename : String)
      return GNATCOLL.VFS.Virtual_File;
   --  Try to return a canonical filename. This is used to have an
   --  as-unique-as-possible analysis unit identifier.

   ------------------------------------
   -- File reader internal interface --
   ------------------------------------

   type Internal_File_Reader is limited interface;
   type Internal_File_Reader_Access is access all Internal_File_Reader'Class;

   procedure Inc_Ref (Self : in out Internal_File_Reader) is abstract;
   --  Create an ownership share for this file reader.

   function Dec_Ref (Self : in out Internal_File_Reader) return Boolean
   is abstract;
   --  Release an ownership share for this file reader. This destroys the file
   --  reader if there are no shares left.
   --
   --  Return whether there are no ownership shares left.

   procedure Read
     (Self        : Internal_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector) is abstract;
   --  Read the content of the source at the given filename, decoding it using
   --  the given charset and decoding the byte order mark if ``Read_BOM`` is
   --  true.
   --
   --  If there is an error during this process, append an error message to
   --  Diagnostics. In that case, Contents is considered uninitialized.
   --
   --  Otherwise, allocate a Text_Type buffer, fill it and initialize Contents
   --  to refer to it.

   procedure Dec_Ref (File_Reader : in out Internal_File_Reader_Access);
   --  Call Dec_Ref on File_Reader.all and, if the ref-count reaches 0,
   --  dealloacte it.

   --------------------------------------
   -- Unit provider internal interface --
   --------------------------------------

   type Internal_Unit_Provider is limited interface;
   type Internal_Unit_Provider_Access is
      access all Internal_Unit_Provider'Class;

   procedure Inc_Ref (Provider : in out Internal_Unit_Provider) is abstract;
   --  Create an ownership share for this unit provider.

   function Dec_Ref (Provider : in out Internal_Unit_Provider) return Boolean
   is abstract;
   --  Release an ownership share for this unit provider. This destroys the
   --  unit provider if there are no shares left.
   --
   --  Return whether there are no ownership shares left.

   function Get_Unit_Filename
     (Provider : Internal_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String is abstract;
   --  Return the filename corresponding to the given unit name/unit kind.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   function Get_Unit
     (Provider    : Internal_Unit_Provider;
      Context     : Internal_Context;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Internal_Unit is abstract;
   --  Fetch and return the analysis unit referenced by the given unit name.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   procedure Dec_Ref (Provider : in out Internal_Unit_Provider_Access);

   --------------------------------------
   -- Event handler internal interface --
   --------------------------------------

   type Internal_Event_Handler is limited interface;
   type Internal_Event_Handler_Access is
      access all Internal_Event_Handler'Class;

   procedure Inc_Ref (Self : in out Internal_Event_Handler) is abstract;
   --  Create an ownership share for this event handler.

   function Dec_Ref (Self : in out Internal_Event_Handler) return Boolean
   is abstract;
   --  Release an ownership share for this event handler. This destroys the
   --  event handler if there are no shares left.
   --
   --  Return whether there are no ownership shares left.

   procedure Unit_Requested_Callback
     (Self               : in out Internal_Event_Handler;
      Context            : Internal_Context;
      Name               : Text_Type;
      From               : Internal_Unit;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean) is null;

   procedure Unit_Parsed_Callback
     (Self     : in out Internal_Event_Handler;
      Context  : Internal_Context;
      Unit     : Internal_Unit;
      Reparsed : Boolean) is null;

   procedure Dec_Ref (Self : in out Internal_Event_Handler_Access);

   ---------------------------------
   -- Analysis context definition --
   ---------------------------------

   type Analysis_Context_Type is limited record
      --  Start of ABI area. In order to perform fast checks from foreign
      --  languages, we maintain minimal ABI for analysis context: this allows
      --  us in language bindings to directly peek in this record rather than
      --  rely on (slow) calls to getters.

      Serial_Number : Version_Number;
      --  Serial number that is incremented each time this context allocation
      --  is released.

      --  End of ABI area

      Ref_Count : Natural;

      Units : Units_Maps.Map;
      --  Collection of analysis units loaded in this context

      Filenames : Virtual_File_Maps.Map;
      --  Cache for GNATCOLL.VFS.Virtual_File we create for String filenames.
      --  Re-using older Virtual_File values is useful as this reduces the need
      --  to normalize paths, which is a costly operation.

      Symbols : Symbol_Table;
      --  Symbol table used in this whole context

      Charset : Unbounded_String;
      --  Default charset to use in analysis units

      Tab_Stop : aliased Positive;
      --  Tab stop for the lexer to correctly interpret ASCII.HT input
      --  characters.

      With_Trivia : Boolean;
      --  Whether Trivia nodes were parsed and included in analysis units

      Root_Scope : Lexical_Env;
      --  The lexical scope that is shared amongst every compilation unit. Used
      --  to resolve cross file references.

      Named_Envs : NED_Maps.Map;
      --  Map env names to the corresponding named environment descriptors

      File_Reader : Internal_File_Reader_Access;
      --  Object to override the reading and decoding of source files

      Event_Handler : Internal_Event_Handler_Access;
      --  Object to provide event callbacks

      Unit_Provider : Internal_Unit_Provider_Access;
      --  Object to translate unit names to file names

      Parser : Parser_Type;
      --  Main parser type. TODO: If we want to parse in several tasks, we'll
      --  replace that by an array of parsers.

      Discard_Errors_In_Populate_Lexical_Env : Boolean;
      --  See the eponym procedure

      In_Populate_Lexical_Env : Boolean;
      --  Flag to tell whether we are running the Populate_Lexical_Env pass.
      --  When it's on, we must not use the memoization map as the hash of
      --  lexical environment changes when their content changes.

      Logic_Resolution_Timeout : Natural;
      --  If zero, inefficient. Otherwise, designates the maximal number of
      --  steps allowed in the resolution of logic equations before
      --  interrupting the resolution because of timeout. See the
      --  Set_Logic_Resolution_Timeout procedure.

      Cache_Version : Version_Number;
      --  Version number used to invalidate memoization caches in a lazy
      --  fashion. If an analysis unit's version number is strictly inferior to
      --  this, its memoization map should be cleared.

      Reparse_Cache_Version : Version_Number;
      --  Version number used to invalidate referenced envs caches. It is
      --  incremented only when a unit is reparsed in the context.

      Rewriting_Handle : Rewriting_Handle_Pointer :=
         No_Rewriting_Handle_Pointer;
      --  Rewriting handle for this context's current rewriting session.
      --  No_Rewriting_Handle_Pointer if there is no such session currently.

      Templates_Unit : Internal_Unit := No_Analysis_Unit;
      --  Special analysis unit used only as a containing unit to parse
      --  templates in the context of tree rewriting.

      Current_Call_Depth : Natural := 0;
      --  Number of recursive calls currently running. This counter is
      --  used as a mitigation against infinite recursions. The calls
      --  considered here include:
      --
      --  * parsing functions;
      --  * properties calls.

      Call_Depth_High_Water_Mark : Natural := 0;
      --  Maximum number of recursive calls seen in this context so far

      Max_Call_Depth : Natural := 0;
      --  Maximum number of recursive calls allowed

      Available_Rebindings : Env_Rebindings_Vectors.Vector;
      --  List of allocated-but-unused Env_Rebinding_Type records.
      --
      --  Each rebinding we allocate for an analysis context is deallocated
      --  only when the whole context is released, so when this list is not
      --  empty, we pick one of its element instead of allocating another
      --  rebinding (see the Acquire_Rebindings and Release_Rebindings
      --  subprograms).
      --
      --  Thanks to this mechanism, we have a very simple way to implement
      --  rebindings validity checking for nodes: once we have established that
      --  the node reference is valid regarding its context, we know that the
      --  rebindings pointer is valid, and thus we can just check the rebinding
      --  version number.
   end record;

   package Node_To_Named_Env_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Bare_Turkixir_Node,
      Element_Type    => Named_Env_Descriptor_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Analysis_Unit_Type is limited record
      --  Start of ABI area. In order to perform fast checks from foreign
      --  languages, we maintain minimal ABI for analysis context: this allows
      --  us in language bindings to directly peek in this record rather than
      --  rely on (slow) calls to getters.

      Unit_Version : Version_Number := 0;
      --  Version for this particular unit. This will be incremented every time
      --  a reparse occurs.

      --  End of ABI area

      Context : Internal_Context;
      --  The owning context for this analysis unit

      Is_Internal : Boolean;
      --  Whether this unit is internal.
      --
      --  The use of file readers for parsing is disabled for internal units,
      --  which allows in-memory parsing for them even when a file reader is
      --  active.
      --
      --  It is illegal for users of public APIs to reparse an internal unit.
      --  Setting this flag allows generated libraries to create internal units
      --  to implement language internals and forbid library users to mess with
      --  this unit.

      AST_Root : Bare_Turkixir_Node;

      Filename : GNATCOLL.VFS.Virtual_File;
      --  The originating name for this analysis unit. This should be set even
      --  if the analysis unit was parsed from a buffer.

      Charset : Unbounded_String;
      --  The parsing charset for this analysis unit, as a string. If the
      --  charset used actually came from a byte order mark, this is
      --  nevertheless set to the one the user requested.

      TDH : aliased Token_Data_Handler;
      --  The token data handler that handles all token data during parsing and
      --  owns it afterwards.

      Diagnostics : Diagnostics_Vectors.Vector;
      --  The list of diagnostics produced for this analysis unit

      Rule : Grammar_Rule;
      --  The grammar rule used to parse this unit

      AST_Mem_Pool : Bump_Ptr_Pool;
      --  This memory pool shall only be used for AST parsing. Stored here
      --  because it is more convenient, but one shall not allocate from it.

      Destroyables : Destroyable_Vectors.Vector;
      --  Collection of objects to destroy when destroying the analysis unit

      Referenced_Units : Analysis_Unit_Sets.Set;
      --  Units that are referenced from this one. Useful for
      --  visibility/computation of the reference graph.

      Is_Env_Populated : Boolean;
      --  Whether Populate_Lexical_Env was called on this unit. Used not to
      --  populate multiple times the same unit and hence avoid infinite
      --  populate recursions for circular dependencies.

      Exiled_Entries : Exiled_Entry_Vectors.Vector;
      --  Lexical env population for this unit may have added AST nodes it owns
      --  to the lexical environments that belong to other units ("exiled"
      --  entries). For each of these AST nodes, this vector contains an entry
      --  that records the target environment, the AST node and the
      --  corresponding symbol.

      Foreign_Nodes : Foreign_Node_Entry_Vectors.Vector;
      --  This unit owns a set of lexical environments. This vector contains
      --  the list of AST nodes that were added to these environments and that
      --  come from other units.

      Exiled_Entries_In_NED : Exiled_Entry_In_NED_Vectors.Vector;
      --  Like Exiled_Entries, but for symbol/node associations exclusively
      --  handled by the named environments mechanism.
      --
      --  This list allows efficient removal of these entries from
      --  Named_Env_Descriptor.Foreign_Nodes components when unloading this
      --  unit.

      Exiled_Envs : Exiled_Env_Vectors.Vector;
      --  List of lexical environments created in this unit and whose parent is
      --  a named environment.
      --
      --  This list allows efficient removal for these envs from
      --  Named_Env_Descriptor.Foreign_Envs components when unloading this
      --  unit.

      Named_Envs : Named_Env_Vectors.Vector;
      --  List of named environment created in this unit.
      --
      --  This list allows efficient removal for these envs from the
      --  Named_Env_Descriptor.Envs components when unloading this unit.

      Nodes_With_Foreign_Env : Node_To_Named_Env_Maps.Map;
      --  Mapping from a node to its Self_Env's named env descriptor, for each
      --  node in this unit whose Self_Env is a named environment.
      --
      --  This mapping allows efficient removal for these nodes from the
      --  Named_Env_Descriptor.Nodes_With_Foreign_Env components when unloading
      --  this unit.

      Rebindings : aliased Env_Rebindings_Vectors.Vector;
      --  List of rebindings for which Old_Env and/or New_Env belong to this
      --  unit. When this unit gets destroyed or reparsed, these rebindings
      --  need to be destroyed too (see Destroy_Rebindings).


      Cache_Version : Version_Number := 0;
      --  See the eponym field in Analysis_Context_Type

      

   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Context_Type, Internal_Context);

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Unit_Type, Internal_Unit);

   type Reparsed_Unit is record
      TDH          : Token_Data_Handler;
      Diagnostics  : Diagnostics_Vectors.Vector;
      AST_Mem_Pool : Bump_Ptr_Pool;
      AST_Root     : Bare_Turkixir_Node;
   end record;
   --  Holder for fields affected by an analysis unit reparse. This makes it
   --  possible to separate the "reparsing" and the "replace" steps.

   procedure Destroy (Reparsed : in out Reparsed_Unit);
   --  Free all resources in Reparsed

   function Basename (Filename : String) return String;
   --  Return the base filename for String

   ----------------------------------------------------
   -- Implementation for analysis context primitives --
   ----------------------------------------------------

   function Create_Context
     (Charset        : String;
      File_Reader    : Internal_File_Reader_Access;
      Unit_Provider  : Internal_Unit_Provider_Access;
      Event_Handler  : Internal_Event_Handler_Access;
      With_Trivia    : Boolean;
      Tab_Stop       : Positive;
      Max_Call_Depth : Natural := 1000)
      return Internal_Context;
   --  Implementation for Analysis.Create_Context

   function Create_Unit
     (Context             : Internal_Context;
      Normalized_Filename : GNATCOLL.VFS.Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Internal_Unit
      with Pre => not Has_Unit (Context, +Normalized_Filename.Full_Name);
   --  Create a new analysis unit and register it in Context

   function Get_Unit
     (Context           : Internal_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Input             : Internal_Lexer_Input;
      Rule              : Grammar_Rule;
      Is_Internal       : Boolean := False) return Internal_Unit;
   --  Helper for Get_From_File and Get_From_Buffer. Return the resulting
   --  analysis unit.
   --
   --  If ``Is_Internal`` is True, allow parsing from buffer even if
   --  ``Context`` has a file reader, and forbid later calls to
   --  Get_From_File/Get_From_Buffer/Reparse on the returned unit.

   function Has_Unit
     (Context : Internal_Context; Unit_Filename : String) return Boolean;
   --  Implementation for Analysis.Has_Unit

   function Get_From_File
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Reparse  : Boolean;
      Rule     : Grammar_Rule) return Internal_Unit;
   --  Implementation for Analysis.Get_From_File

   function Get_From_Buffer
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Buffer   : String;
      Rule     : Grammar_Rule) return Internal_Unit;
   --  Implementation for Analysis.Get_From_Buffer

   function Get_With_Error
     (Context  : Internal_Context;
      Filename : String;
      Error    : Text_Type;
      Charset  : String;
      Rule     : Grammar_Rule) return Internal_Unit;
   --  Implementation for Analysis.Get_With_Error


   function Unit_Provider
     (Context : Internal_Context) return Internal_Unit_Provider_Access;
   --  Implementation for Analysis.Unit_Provider

   function Hash (Context : Internal_Context) return Hash_Type;
   --  Implementation for Analysis.Hash

   function Has_With_Trivia (Context : Internal_Context) return Boolean;
   --  Implementation for Analysis.Has_With_Trivia

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Internal_Context; Discard : Boolean);
   --  Implementation for Analysis.Discard_Errors_In_Populate_Lexical_Env

   procedure Set_Logic_Resolution_Timeout
     (Context : Internal_Context; Timeout : Natural);
   --  Implementation for Analysis.Set_Logic_Resolution_Timeout

   function Has_Rewriting_Handle (Context : Internal_Context) return Boolean;
   --  Implementation for Analysis.Has_Rewriting_Handle

   procedure Inc_Ref (Context : Internal_Context);
   --  Increment the ref-count of Context. This does nothing if Context is
   --  null.

   procedure Dec_Ref (Context : in out Internal_Context);
   --  Decrement the ref-count of Context, destroying it if the ref-count
   --  reaches zero. This does nothing if Context is null.

   procedure Destroy (Context : in out Internal_Context)
      with Pre => not Has_Rewriting_Handle (Context);
   --  Free all resources allocated for Context

   -------------------------------------------------
   -- Implementation for analysis unit primitives --
   -------------------------------------------------

   function Context (Unit : Internal_Unit) return Internal_Context;
   --  Implementation for Analysis.Context

   function Hash (Unit : Internal_Unit) return Hash_Type;
   --  Implementation for Analysis.Hash

   procedure Reparse (Unit : Internal_Unit; Charset : String);
   --  Implementation for Analysis.Reparse

   procedure Reparse
     (Unit : Internal_Unit; Charset : String; Buffer  : String);
   --  Implementation for Analysis.Reparse

   procedure Populate_Lexical_Env (Unit : Internal_Unit);
   --  Implementation for Analysis.Populate_Lexical_Env

   function Get_Filename (Unit : Internal_Unit) return String;
   --  Implementation for Analysis.Get_Filename

   function Get_Charset (Unit : Internal_Unit) return String;
   --  Implementation for Analysis.Get_Charset

   function Has_Diagnostics (Unit : Internal_Unit) return Boolean;
   --  Implementation for Analysis.Has_Diagnostics

   function Diagnostics (Unit : Internal_Unit) return Diagnostics_Array;
   --  Implementation for Analysis.Diagnostics

   function Format_GNU_Diagnostic
     (Unit : Internal_Unit; D : Diagnostic) return String;
   --  Implementation for Analysis.Format_GNU_Diagnostic

   function Root (Unit : Internal_Unit) return Bare_Turkixir_Node;
   --  Implementation for Analysis.Root

   function First_Token (Unit : Internal_Unit) return Token_Reference;
   --  Implementation for Analysis.First_Token

   function Last_Token (Unit : Internal_Unit) return Token_Reference;
   --  Implementation for Analysis.Last_Token

   function Token_Count (Unit : Internal_Unit) return Natural;
   --  Implementation for Analysis.Token_Count

   function Trivia_Count (Unit : Internal_Unit) return Natural;
   --  Implementation for Analysis.Trivia_Count

   function Text (Unit : Internal_Unit) return Text_Type;
   --  Implementation for Analysis.Text

   function Lookup_Token
     (Unit : Internal_Unit; Sloc : Source_Location) return Token_Reference;
   --  Implementation for Analysis.Lookup_Token

   procedure Dump_Lexical_Env (Unit : Internal_Unit);
   --  Implementation for Analysis.Dump_Lexical_Env

   procedure Print (Unit : Internal_Unit; Show_Slocs : Boolean);
   --  Implementation for Analysis.Print

   procedure PP_Trivia (Unit : Internal_Unit);
   --  Implementation for Analysis.PP_Trivia

   procedure Destroy (Unit : in out Internal_Unit);
   --  TODO???

   function Basename (Unit : Internal_Unit) return String;
   --  Return the base filename for Unit

   procedure Invalidate_Caches
     (Context : Internal_Context; Invalidate_Envs : Boolean);
   --  Invalidate memoization caches. If Invalidate_Envs is true, also
   --  invalidate referenced envs caches.

   procedure Reset_Caches (Unit : Internal_Unit);
   --  Destroy Unit's memoization cache. This resets Unit's version number to
   --  Unit.Context.Cache_Version.

   procedure Reference_Unit (From, Referenced : Internal_Unit);
   --  Set the Referenced unit as being referenced from the From unit. This is
   --  useful for visibility purposes, and is mainly meant to be used in the
   --  env hooks.

   function Get_Line
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type;
   --  Return the line of text at line number ``Line_Number``

   function Is_Referenced_From
     (Self, Unit : Internal_Unit) return Boolean;

   procedure Do_Parsing
     (Unit   : Internal_Unit;
      Input  : Internal_Lexer_Input;
      Result : out Reparsed_Unit);
   --  Parse text for Unit using Input and store the result in Result. This
   --  leaves Unit unchanged.

   procedure Update_After_Reparse
     (Unit : Internal_Unit; Reparsed : in out Reparsed_Unit);
   --  Update Unit's AST from Reparsed and update stale lexical environment
   --  data after the reparsing of Unit.

   procedure Destroy_Unit_Destroyables (Unit : Internal_Unit);
   --  Destroy all destroyables objects in Unit and clear this list in Unit

   procedure Remove_Exiled_Entries (Unit : Internal_Unit);
   --  Remove lexical environment entries referencing nodes in Unit from
   --  lexical environments Unit does not own. Remove foreign node entries in
   --  foreign units that correspond to these exiled entries. Clear
   --  Unit.Exiled_Entries afterwards.

   procedure Remove_Named_Envs
     (Unit                      : Internal_Unit;
      Named_Envs_Needing_Update : in out NED_Maps.Map);
   --  Remove envs that belong to Unit from all relevant NEDs, and keep track
   --  in Named_Env_Needing_Update of the env names whose env with precedence
   --  must change because of this.

   procedure Extract_Foreign_Nodes
     (Unit          : Internal_Unit;
      Foreign_Nodes : in out Bare_Turkixir_Node_Vectors.Vector);
   --  Collect in Foreign_Nodes all foreign nodes in Unit's lexical
   --  environments (i.e. lexical env entries that refer to nodes which belongs
   --  to other analysis units). Remove the exiled entries in foreign units
   --  that correspond to these foreign nodes. Clear Unit.Foreign_Nodes
   --  afterwards.

   procedure Reroot_Foreign_Node (Node : Bare_Turkixir_Node);
   --  Re-create the lexical env entry for Node. This is to be used in
   --  Flush_Populate_Lexical_Env_Queue, after reparsing removed the target
   --  lexical environment.

   procedure Destroy_Rebindings
     (Rebindings : access Env_Rebindings_Vectors.Vector);
   --  Destroy all rebindings in Rebindings, plus their child rebindings. Note
   --  that children can belong to various analysis units, so this takes care
   --  of removing the destroyed rebindings from each concerned analysis unit's
   --  Rebindings vector.
   --
   --  This require an access parameter in order to avoid aliasing issues in
   --  the body.

   function Get_Rewriting_Handle
     (Context : Internal_Context) return Rewriting_Handle_Pointer;
   --  Return the Rewriting_Handle component of Context

   procedure Set_Rewriting_Handle
     (Context : Internal_Context; Handle : Rewriting_Handle_Pointer);
   --  Set the Rewriting_Handle component of Context

   type Node_Safety_Net is record
      Context        : Internal_Context;
      Context_Serial : Version_Number;
      --  Analysis context and serial number at the time this safety net was
      --  produced.

      Unit         : Internal_Unit;
      Unit_Version : Version_Number;
      --  Analysis unit and unit version at the time this safety net was
      --  produced.

      Rebindings_Version : Version_Number;
      --  Version of the associated rebinding at the time this safety net was
      --  procuded.
   end record;
   --  Information to embed in public APIs, used to check before accessing data
   --  that the said-data is still valid.

   No_Node_Safety_Net : constant Node_Safety_Net := (null, 0, null, 0, 0);

   function String_To_Symbol
     (Context : Internal_Context; S : String_Type) return Symbol_Type;
   --  Convert S into the corresponding symbol, raising a Property_Error if
   --  symbol canonicalization fails. If S is empty, just return null.

   function Solve_Wrapper
     (R            : Relation;
      Context_Node : Bare_Turkixir_Node) return Boolean;
   --  Wrapper for Langkit_Support.Adalog.Solve; will handle setting the debug
   --  strings in the equation if in debug mode.

   generic
      type T (<>) is limited private;
      type T_Access is access all T;
      with procedure Destroy (Object : in out T_Access);
   procedure Register_Destroyable_Gen
     (Unit : Internal_Unit; Object : T_Access);
   --  Generic procedure to register an object so that it is automatically
   --  destroyed when Unit is destroyed.

   function New_Unit_String
     (Unit : Internal_Unit; Str : String) return String_Access;
   --  This function allocates a string whose lifetime will be associated with
   --  ``Unit``.

private
   --  We only have a private part to defer the initialization of struct
   --  constants. This allows us to circumvent circularity problems between
   --  arrays and structs.

         
      


      No_DesignatedEnv : constant Internal_DesignatedEnv :=
      (
               Kind => None, 
               Env_Name => null, 
               Direct_Env => Empty_Env
      );

         

         

         
      


      No_Entity_Expr : constant Internal_Entity_Expr :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_And_Expr : constant Internal_Entity_And_Expr :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_And_Op : constant Internal_Entity_And_Op :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Arg : constant Internal_Entity_Arg :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Arg_Assoc : constant Internal_Entity_Arg_Assoc :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Arg_Gen : constant Internal_Entity_Arg_Gen :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Turkixir_Node_Base_List : constant Internal_Entity_Turkixir_Node_Base_List :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Arg_List : constant Internal_Entity_Arg_List :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Bin_Op : constant Internal_Entity_Bin_Op :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Arith_Expr : constant Internal_Entity_Arith_Expr :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_As_Name_Node : constant Internal_Entity_As_Name_Node :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_As_Name_Node_List : constant Internal_Entity_As_Name_Node_List :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Stmt : constant Internal_Entity_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Assert_Stmt : constant Internal_Entity_Assert_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Assign_Stmt : constant Internal_Entity_Assign_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Aug_Assign_Stmt : constant Internal_Entity_Aug_Assign_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Break_Stmt : constant Internal_Entity_Break_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Call_Expr : constant Internal_Entity_Call_Expr :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Def_Stmt : constant Internal_Entity_Def_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Class_Def : constant Internal_Entity_Class_Def :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comprehension : constant Internal_Entity_Comprehension :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_For : constant Internal_Entity_Comp_For :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_ForL : constant Internal_Entity_Comp_ForL :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_If : constant Internal_Entity_Comp_If :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_Op : constant Internal_Entity_Comp_Op :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_Op_Kind : constant Internal_Entity_Comp_Op_Kind :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_Op_Kind_Diamond : constant Internal_Entity_Comp_Op_Kind_Diamond :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_Op_Kind_Eq : constant Internal_Entity_Comp_Op_Kind_Eq :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_Op_Kind_Gt : constant Internal_Entity_Comp_Op_Kind_Gt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_Op_Kind_Gte : constant Internal_Entity_Comp_Op_Kind_Gte :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_Op_Kind_In : constant Internal_Entity_Comp_Op_Kind_In :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_Op_Kind_Is : constant Internal_Entity_Comp_Op_Kind_Is :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_Op_Kind_Isnot : constant Internal_Entity_Comp_Op_Kind_Isnot :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_Op_Kind_Lt : constant Internal_Entity_Comp_Op_Kind_Lt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_Op_Kind_Lte : constant Internal_Entity_Comp_Op_Kind_Lte :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_Op_Kind_Noteq : constant Internal_Entity_Comp_Op_Kind_Noteq :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Comp_Op_Kind_Notin : constant Internal_Entity_Comp_Op_Kind_Notin :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Concat_String_Lit : constant Internal_Entity_Concat_String_Lit :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Continue_Stmt : constant Internal_Entity_Continue_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Decorated : constant Internal_Entity_Decorated :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Decorator : constant Internal_Entity_Decorator :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Decorator_List : constant Internal_Entity_Decorator_List :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Del_Stmt : constant Internal_Entity_Del_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Dict_Assoc : constant Internal_Entity_Dict_Assoc :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Dict_Assoc_List : constant Internal_Entity_Dict_Assoc_List :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Dict_Comp : constant Internal_Entity_Dict_Comp :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Dict_Lit : constant Internal_Entity_Dict_Lit :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Dot : constant Internal_Entity_Dot :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Dot_List : constant Internal_Entity_Dot_List :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Name : constant Internal_Entity_Name :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Dotted_Name : constant Internal_Entity_Dotted_Name :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Elif_Branch : constant Internal_Entity_Elif_Branch :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Elif_Branch_List : constant Internal_Entity_Elif_Branch_List :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ellipsis_Expr : constant Internal_Entity_Ellipsis_Expr :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Else_Part : constant Internal_Entity_Else_Part :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Except_Part : constant Internal_Entity_Except_Part :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Except_Part_List : constant Internal_Entity_Except_Part_List :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Exec_Stmt : constant Internal_Entity_Exec_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Expr_List : constant Internal_Entity_Expr_List :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Slice_Expr : constant Internal_Entity_Slice_Expr :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ext_Slice_Expr : constant Internal_Entity_Ext_Slice_Expr :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Factor : constant Internal_Entity_Factor :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_File_Node : constant Internal_Entity_File_Node :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_For_Stmt : constant Internal_Entity_For_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Func_Def : constant Internal_Entity_Func_Def :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Global_Stmt : constant Internal_Entity_Global_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Id : constant Internal_Entity_Id :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Id_List : constant Internal_Entity_Id_List :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_If_Expr : constant Internal_Entity_If_Expr :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_If_Stmt : constant Internal_Entity_If_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Import_From : constant Internal_Entity_Import_From :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Import_Name : constant Internal_Entity_Import_Name :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Import_Star : constant Internal_Entity_Import_Star :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Inline_Eval : constant Internal_Entity_Inline_Eval :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Kw_Args : constant Internal_Entity_Kw_Args :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Kw_Args_Flag : constant Internal_Entity_Kw_Args_Flag :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Kw_Args_Flag_Absent : constant Internal_Entity_Kw_Args_Flag_Absent :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Kw_Args_Flag_Present : constant Internal_Entity_Kw_Args_Flag_Present :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Lambda_Def : constant Internal_Entity_Lambda_Def :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_List_Comp : constant Internal_Entity_List_Comp :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_List_Gen : constant Internal_Entity_List_Gen :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_List_Lit : constant Internal_Entity_List_Lit :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_NL : constant Internal_Entity_NL :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_NL_List : constant Internal_Entity_NL_List :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Not_Op : constant Internal_Entity_Not_Op :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Number_Lit : constant Internal_Entity_Number_Lit :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op : constant Internal_Entity_Op :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Or_Expr : constant Internal_Entity_Or_Expr :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Or_Op : constant Internal_Entity_Or_Op :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Params : constant Internal_Entity_Params :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Pass_Stmt : constant Internal_Entity_Pass_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Power : constant Internal_Entity_Power :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Print_Stmt : constant Internal_Entity_Print_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Raise_Stmt : constant Internal_Entity_Raise_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Rel_Name : constant Internal_Entity_Rel_Name :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Return_Stmt : constant Internal_Entity_Return_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Set_Comp : constant Internal_Entity_Set_Comp :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Set_Lit : constant Internal_Entity_Set_Lit :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Shift_Expr : constant Internal_Entity_Shift_Expr :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Single_Param : constant Internal_Entity_Single_Param :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Single_Param_List : constant Internal_Entity_Single_Param_List :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Stream_Print_Stmt : constant Internal_Entity_Stream_Print_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_String_Lit : constant Internal_Entity_String_Lit :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_String_Lit_List : constant Internal_Entity_String_Lit_List :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Subscript_Expr : constant Internal_Entity_Subscript_Expr :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Term : constant Internal_Entity_Term :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Try_Stmt : constant Internal_Entity_Try_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Tuple_Lit : constant Internal_Entity_Tuple_Lit :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Turkixir_Node_List : constant Internal_Entity_Turkixir_Node_List :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Var_Args : constant Internal_Entity_Var_Args :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Var_Args_Flag : constant Internal_Entity_Var_Args_Flag :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Var_Args_Flag_Absent : constant Internal_Entity_Var_Args_Flag_Absent :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Var_Args_Flag_Present : constant Internal_Entity_Var_Args_Flag_Present :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_While_Stmt : constant Internal_Entity_While_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_With_Stmt : constant Internal_Entity_With_Stmt :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Xor_Expr : constant Internal_Entity_Xor_Expr :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Yield_Expr : constant Internal_Entity_Yield_Expr :=
      (
               Node => No_Bare_Turkixir_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Env_Assoc : constant Internal_Env_Assoc :=
      (
               Key => null, 
               Val => No_Bare_Turkixir_Node, 
               Dest_Env => No_DesignatedEnv, 
               Metadata => No_Metadata
      );


end Libturkixirlang.Implementation;
