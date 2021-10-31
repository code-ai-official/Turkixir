


with GNATCOLL.GMP.Integers;
with GNATCOLL.Traces;

with Langkit_Support.Errors;
private with Langkit_Support.Internal.Analysis;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Symbols.Precomputed;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;
with Langkit_Support.Types;   use Langkit_Support.Types;


--  This package provides types and functions used in the whole Libturkixirlang
--  package tree.

package Libturkixirlang.Common is

   use Support.Slocs, Support.Text;

   

   Main_Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("LIBTURKIXIRLANG.MAIN_TRACE", GNATCOLL.Traces.From_Config);

   PLE_Errors_Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("LIBTURKIXIRLANG.PLE_ERRORS", GNATCOLL.Traces.From_Config);

   Default_Charset : constant String := "utf-8";
   --  Default charset to use when creating analysis contexts

   subtype Big_Integer is GNATCOLL.GMP.Integers.Big_Integer;
   --  Shortcut for ``GNATCOLL.GMP.Integers.Big_Integer``

   -------------------------------------
   -- Symbols and token data handlers --
   -------------------------------------

   type Precomputed_Symbol_Index is
         new Integer range 1 .. 0
   ;

   function Precomputed_Symbol
     (Index : Precomputed_Symbol_Index) return Text_Type;

   --  GNAT emits an incorrect value not in range in instantiation warning...
   --  So deactivate them at the instantiation point.
   pragma Warnings (Off, "value not in range");
   package Precomputed_Symbols
   is new Langkit_Support.Symbols.Precomputed
     (Precomputed_Symbol_Index, Precomputed_Symbol);
   pragma Warnings (On, "value not in range");

   -----------
   -- Nodes --
   -----------

   type Turkixir_Node_Kind_Type is
     (Turkixir_Arg_Assoc, Turkixir_Arg_Gen, Turkixir_Kw_Args, Turkixir_Var_Args, Turkixir_As_Name_Node, Turkixir_Comp_If, Turkixir_Comp_Op_Kind_Diamond, Turkixir_Comp_Op_Kind_Eq, Turkixir_Comp_Op_Kind_Gt, Turkixir_Comp_Op_Kind_Gte, Turkixir_Comp_Op_Kind_In, Turkixir_Comp_Op_Kind_Is, Turkixir_Comp_Op_Kind_Isnot, Turkixir_Comp_Op_Kind_Lt, Turkixir_Comp_Op_Kind_Lte, Turkixir_Comp_Op_Kind_Noteq, Turkixir_Comp_Op_Kind_Notin, Turkixir_Comp_For, Turkixir_Comp_ForL, Turkixir_Decorator, Turkixir_Dict_Assoc, Turkixir_Else_Part, Turkixir_Except_Part, Turkixir_And_Expr, Turkixir_And_Op, Turkixir_Arith_Expr, Turkixir_Shift_Expr, Turkixir_Term, Turkixir_Call_Expr, Turkixir_Comp_Op, Turkixir_Concat_String_Lit, Turkixir_Dict_Comp, Turkixir_Dict_Lit, Turkixir_Dot, Turkixir_Ellipsis_Expr, Turkixir_Factor, Turkixir_If_Expr, Turkixir_Inline_Eval, Turkixir_Lambda_Def, Turkixir_List_Comp, Turkixir_List_Gen, Turkixir_List_Lit, Turkixir_Dotted_Name, Turkixir_Id, Turkixir_Not_Op, Turkixir_Number_Lit, Turkixir_Or_Expr, Turkixir_Or_Op, Turkixir_Power, Turkixir_Set_Comp, Turkixir_Set_Lit, Turkixir_Slice_Expr, Turkixir_Ext_Slice_Expr, Turkixir_String_Lit, Turkixir_Subscript_Expr, Turkixir_Tuple_Lit, Turkixir_Xor_Expr, Turkixir_Yield_Expr, Turkixir_File_Node, Turkixir_Import_Star, Turkixir_Kw_Args_Flag_Absent, Turkixir_Kw_Args_Flag_Present, Turkixir_NL, Turkixir_Op, Turkixir_Params, Turkixir_Rel_Name, Turkixir_Single_Param, Turkixir_Assert_Stmt, Turkixir_Assign_Stmt, Turkixir_Aug_Assign_Stmt, Turkixir_Break_Stmt, Turkixir_Continue_Stmt, Turkixir_Decorated, Turkixir_Class_Def, Turkixir_Func_Def, Turkixir_Del_Stmt, Turkixir_Elif_Branch, Turkixir_Exec_Stmt, Turkixir_For_Stmt, Turkixir_Global_Stmt, Turkixir_If_Stmt, Turkixir_Import_From, Turkixir_Import_Name, Turkixir_Pass_Stmt, Turkixir_Print_Stmt, Turkixir_Raise_Stmt, Turkixir_Return_Stmt, Turkixir_Stream_Print_Stmt, Turkixir_Try_Stmt, Turkixir_While_Stmt, Turkixir_With_Stmt, Turkixir_Arg_List, Turkixir_As_Name_Node_List, Turkixir_Decorator_List, Turkixir_Dict_Assoc_List, Turkixir_Dot_List, Turkixir_Elif_Branch_List, Turkixir_Except_Part_List, Turkixir_Expr_List, Turkixir_Id_List, Turkixir_NL_List, Turkixir_Single_Param_List, Turkixir_String_Lit_List, Turkixir_Turkixir_Node_List, Turkixir_Var_Args_Flag_Absent, Turkixir_Var_Args_Flag_Present);
   --  Type for concrete nodes

   for Turkixir_Node_Kind_Type use
     (Turkixir_Arg_Assoc => 1, Turkixir_Arg_Gen => 2, Turkixir_Kw_Args => 3, Turkixir_Var_Args => 4, Turkixir_As_Name_Node => 5, Turkixir_Comp_If => 6, Turkixir_Comp_Op_Kind_Diamond => 7, Turkixir_Comp_Op_Kind_Eq => 8, Turkixir_Comp_Op_Kind_Gt => 9, Turkixir_Comp_Op_Kind_Gte => 10, Turkixir_Comp_Op_Kind_In => 11, Turkixir_Comp_Op_Kind_Is => 12, Turkixir_Comp_Op_Kind_Isnot => 13, Turkixir_Comp_Op_Kind_Lt => 14, Turkixir_Comp_Op_Kind_Lte => 15, Turkixir_Comp_Op_Kind_Noteq => 16, Turkixir_Comp_Op_Kind_Notin => 17, Turkixir_Comp_For => 18, Turkixir_Comp_ForL => 19, Turkixir_Decorator => 20, Turkixir_Dict_Assoc => 21, Turkixir_Else_Part => 22, Turkixir_Except_Part => 23, Turkixir_And_Expr => 24, Turkixir_And_Op => 25, Turkixir_Arith_Expr => 26, Turkixir_Shift_Expr => 27, Turkixir_Term => 28, Turkixir_Call_Expr => 29, Turkixir_Comp_Op => 30, Turkixir_Concat_String_Lit => 31, Turkixir_Dict_Comp => 32, Turkixir_Dict_Lit => 33, Turkixir_Dot => 34, Turkixir_Ellipsis_Expr => 35, Turkixir_Factor => 36, Turkixir_If_Expr => 37, Turkixir_Inline_Eval => 38, Turkixir_Lambda_Def => 39, Turkixir_List_Comp => 40, Turkixir_List_Gen => 41, Turkixir_List_Lit => 42, Turkixir_Dotted_Name => 43, Turkixir_Id => 44, Turkixir_Not_Op => 45, Turkixir_Number_Lit => 46, Turkixir_Or_Expr => 47, Turkixir_Or_Op => 48, Turkixir_Power => 49, Turkixir_Set_Comp => 50, Turkixir_Set_Lit => 51, Turkixir_Slice_Expr => 52, Turkixir_Ext_Slice_Expr => 53, Turkixir_String_Lit => 54, Turkixir_Subscript_Expr => 55, Turkixir_Tuple_Lit => 56, Turkixir_Xor_Expr => 57, Turkixir_Yield_Expr => 58, Turkixir_File_Node => 59, Turkixir_Import_Star => 60, Turkixir_Kw_Args_Flag_Absent => 61, Turkixir_Kw_Args_Flag_Present => 62, Turkixir_NL => 63, Turkixir_Op => 64, Turkixir_Params => 65, Turkixir_Rel_Name => 66, Turkixir_Single_Param => 67, Turkixir_Assert_Stmt => 68, Turkixir_Assign_Stmt => 69, Turkixir_Aug_Assign_Stmt => 70, Turkixir_Break_Stmt => 71, Turkixir_Continue_Stmt => 72, Turkixir_Decorated => 73, Turkixir_Class_Def => 74, Turkixir_Func_Def => 75, Turkixir_Del_Stmt => 76, Turkixir_Elif_Branch => 77, Turkixir_Exec_Stmt => 78, Turkixir_For_Stmt => 79, Turkixir_Global_Stmt => 80, Turkixir_If_Stmt => 81, Turkixir_Import_From => 82, Turkixir_Import_Name => 83, Turkixir_Pass_Stmt => 84, Turkixir_Print_Stmt => 85, Turkixir_Raise_Stmt => 86, Turkixir_Return_Stmt => 87, Turkixir_Stream_Print_Stmt => 88, Turkixir_Try_Stmt => 89, Turkixir_While_Stmt => 90, Turkixir_With_Stmt => 91, Turkixir_Arg_List => 92, Turkixir_As_Name_Node_List => 93, Turkixir_Decorator_List => 94, Turkixir_Dict_Assoc_List => 95, Turkixir_Dot_List => 96, Turkixir_Elif_Branch_List => 97, Turkixir_Except_Part_List => 98, Turkixir_Expr_List => 99, Turkixir_Id_List => 100, Turkixir_NL_List => 101, Turkixir_Single_Param_List => 102, Turkixir_String_Lit_List => 103, Turkixir_Turkixir_Node_List => 104, Turkixir_Var_Args_Flag_Absent => 105, Turkixir_Var_Args_Flag_Present => 106);

      subtype Turkixir_Turkixir_Node is Turkixir_Node_Kind_Type
            range Turkixir_Arg_Assoc .. Turkixir_Var_Args_Flag_Present;
      --% no-document: True
      subtype Turkixir_Arg is Turkixir_Node_Kind_Type
            range Turkixir_Arg_Assoc .. Turkixir_Var_Args;
      --% no-document: True
      subtype Turkixir_Arg_Assoc_Range is Turkixir_Node_Kind_Type
            range Turkixir_Arg_Assoc .. Turkixir_Arg_Assoc;
      --% no-document: True
      subtype Turkixir_Arg_Gen_Range is Turkixir_Node_Kind_Type
            range Turkixir_Arg_Gen .. Turkixir_Arg_Gen;
      --% no-document: True
      subtype Turkixir_Kw_Args_Range is Turkixir_Node_Kind_Type
            range Turkixir_Kw_Args .. Turkixir_Kw_Args;
      --% no-document: True
      subtype Turkixir_Var_Args_Range is Turkixir_Node_Kind_Type
            range Turkixir_Var_Args .. Turkixir_Var_Args;
      --% no-document: True
      subtype Turkixir_As_Name_Node_Range is Turkixir_Node_Kind_Type
            range Turkixir_As_Name_Node .. Turkixir_As_Name_Node;
      --% no-document: True
      subtype Turkixir_Comp_If_Range is Turkixir_Node_Kind_Type
            range Turkixir_Comp_If .. Turkixir_Comp_If;
      --% no-document: True
      subtype Turkixir_Comp_Op_Kind is Turkixir_Node_Kind_Type
            range Turkixir_Comp_Op_Kind_Diamond .. Turkixir_Comp_Op_Kind_Notin;
      --% no-document: True
      subtype Turkixir_Comp_Op_Kind_Diamond_Range is Turkixir_Node_Kind_Type
            range Turkixir_Comp_Op_Kind_Diamond .. Turkixir_Comp_Op_Kind_Diamond;
      --% no-document: True
      subtype Turkixir_Comp_Op_Kind_Eq_Range is Turkixir_Node_Kind_Type
            range Turkixir_Comp_Op_Kind_Eq .. Turkixir_Comp_Op_Kind_Eq;
      --% no-document: True
      subtype Turkixir_Comp_Op_Kind_Gt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Comp_Op_Kind_Gt .. Turkixir_Comp_Op_Kind_Gt;
      --% no-document: True
      subtype Turkixir_Comp_Op_Kind_Gte_Range is Turkixir_Node_Kind_Type
            range Turkixir_Comp_Op_Kind_Gte .. Turkixir_Comp_Op_Kind_Gte;
      --% no-document: True
      subtype Turkixir_Comp_Op_Kind_In_Range is Turkixir_Node_Kind_Type
            range Turkixir_Comp_Op_Kind_In .. Turkixir_Comp_Op_Kind_In;
      --% no-document: True
      subtype Turkixir_Comp_Op_Kind_Is_Range is Turkixir_Node_Kind_Type
            range Turkixir_Comp_Op_Kind_Is .. Turkixir_Comp_Op_Kind_Is;
      --% no-document: True
      subtype Turkixir_Comp_Op_Kind_Isnot_Range is Turkixir_Node_Kind_Type
            range Turkixir_Comp_Op_Kind_Isnot .. Turkixir_Comp_Op_Kind_Isnot;
      --% no-document: True
      subtype Turkixir_Comp_Op_Kind_Lt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Comp_Op_Kind_Lt .. Turkixir_Comp_Op_Kind_Lt;
      --% no-document: True
      subtype Turkixir_Comp_Op_Kind_Lte_Range is Turkixir_Node_Kind_Type
            range Turkixir_Comp_Op_Kind_Lte .. Turkixir_Comp_Op_Kind_Lte;
      --% no-document: True
      subtype Turkixir_Comp_Op_Kind_Noteq_Range is Turkixir_Node_Kind_Type
            range Turkixir_Comp_Op_Kind_Noteq .. Turkixir_Comp_Op_Kind_Noteq;
      --% no-document: True
      subtype Turkixir_Comp_Op_Kind_Notin_Range is Turkixir_Node_Kind_Type
            range Turkixir_Comp_Op_Kind_Notin .. Turkixir_Comp_Op_Kind_Notin;
      --% no-document: True
      subtype Turkixir_Comprehension is Turkixir_Node_Kind_Type
            range Turkixir_Comp_For .. Turkixir_Comp_ForL;
      --% no-document: True
      subtype Turkixir_Comp_For_Range is Turkixir_Node_Kind_Type
            range Turkixir_Comp_For .. Turkixir_Comp_For;
      --% no-document: True
      subtype Turkixir_Comp_ForL_Range is Turkixir_Node_Kind_Type
            range Turkixir_Comp_ForL .. Turkixir_Comp_ForL;
      --% no-document: True
      subtype Turkixir_Decorator_Range is Turkixir_Node_Kind_Type
            range Turkixir_Decorator .. Turkixir_Decorator;
      --% no-document: True
      subtype Turkixir_Dict_Assoc_Range is Turkixir_Node_Kind_Type
            range Turkixir_Dict_Assoc .. Turkixir_Dict_Assoc;
      --% no-document: True
      subtype Turkixir_Else_Part_Range is Turkixir_Node_Kind_Type
            range Turkixir_Else_Part .. Turkixir_Else_Part;
      --% no-document: True
      subtype Turkixir_Except_Part_Range is Turkixir_Node_Kind_Type
            range Turkixir_Except_Part .. Turkixir_Except_Part;
      --% no-document: True
      subtype Turkixir_Expr is Turkixir_Node_Kind_Type
            range Turkixir_And_Expr .. Turkixir_Yield_Expr;
      --% no-document: True
      subtype Turkixir_And_Expr_Range is Turkixir_Node_Kind_Type
            range Turkixir_And_Expr .. Turkixir_And_Expr;
      --% no-document: True
      subtype Turkixir_And_Op_Range is Turkixir_Node_Kind_Type
            range Turkixir_And_Op .. Turkixir_And_Op;
      --% no-document: True
      subtype Turkixir_Bin_Op is Turkixir_Node_Kind_Type
            range Turkixir_Arith_Expr .. Turkixir_Term;
      --% no-document: True
      subtype Turkixir_Arith_Expr_Range is Turkixir_Node_Kind_Type
            range Turkixir_Arith_Expr .. Turkixir_Arith_Expr;
      --% no-document: True
      subtype Turkixir_Shift_Expr_Range is Turkixir_Node_Kind_Type
            range Turkixir_Shift_Expr .. Turkixir_Shift_Expr;
      --% no-document: True
      subtype Turkixir_Term_Range is Turkixir_Node_Kind_Type
            range Turkixir_Term .. Turkixir_Term;
      --% no-document: True
      subtype Turkixir_Call_Expr_Range is Turkixir_Node_Kind_Type
            range Turkixir_Call_Expr .. Turkixir_Call_Expr;
      --% no-document: True
      subtype Turkixir_Comp_Op_Range is Turkixir_Node_Kind_Type
            range Turkixir_Comp_Op .. Turkixir_Comp_Op;
      --% no-document: True
      subtype Turkixir_Concat_String_Lit_Range is Turkixir_Node_Kind_Type
            range Turkixir_Concat_String_Lit .. Turkixir_Concat_String_Lit;
      --% no-document: True
      subtype Turkixir_Dict_Comp_Range is Turkixir_Node_Kind_Type
            range Turkixir_Dict_Comp .. Turkixir_Dict_Comp;
      --% no-document: True
      subtype Turkixir_Dict_Lit_Range is Turkixir_Node_Kind_Type
            range Turkixir_Dict_Lit .. Turkixir_Dict_Lit;
      --% no-document: True
      subtype Turkixir_Dot_Range is Turkixir_Node_Kind_Type
            range Turkixir_Dot .. Turkixir_Dot;
      --% no-document: True
      subtype Turkixir_Ellipsis_Expr_Range is Turkixir_Node_Kind_Type
            range Turkixir_Ellipsis_Expr .. Turkixir_Ellipsis_Expr;
      --% no-document: True
      subtype Turkixir_Factor_Range is Turkixir_Node_Kind_Type
            range Turkixir_Factor .. Turkixir_Factor;
      --% no-document: True
      subtype Turkixir_If_Expr_Range is Turkixir_Node_Kind_Type
            range Turkixir_If_Expr .. Turkixir_If_Expr;
      --% no-document: True
      subtype Turkixir_Inline_Eval_Range is Turkixir_Node_Kind_Type
            range Turkixir_Inline_Eval .. Turkixir_Inline_Eval;
      --% no-document: True
      subtype Turkixir_Lambda_Def_Range is Turkixir_Node_Kind_Type
            range Turkixir_Lambda_Def .. Turkixir_Lambda_Def;
      --% no-document: True
      subtype Turkixir_List_Comp_Range is Turkixir_Node_Kind_Type
            range Turkixir_List_Comp .. Turkixir_List_Comp;
      --% no-document: True
      subtype Turkixir_List_Gen_Range is Turkixir_Node_Kind_Type
            range Turkixir_List_Gen .. Turkixir_List_Gen;
      --% no-document: True
      subtype Turkixir_List_Lit_Range is Turkixir_Node_Kind_Type
            range Turkixir_List_Lit .. Turkixir_List_Lit;
      --% no-document: True
      subtype Turkixir_Name is Turkixir_Node_Kind_Type
            range Turkixir_Dotted_Name .. Turkixir_Id;
      --% no-document: True
      subtype Turkixir_Dotted_Name_Range is Turkixir_Node_Kind_Type
            range Turkixir_Dotted_Name .. Turkixir_Dotted_Name;
      --% no-document: True
      subtype Turkixir_Id_Range is Turkixir_Node_Kind_Type
            range Turkixir_Id .. Turkixir_Id;
      --% no-document: True
      subtype Turkixir_Not_Op_Range is Turkixir_Node_Kind_Type
            range Turkixir_Not_Op .. Turkixir_Not_Op;
      --% no-document: True
      subtype Turkixir_Number_Lit_Range is Turkixir_Node_Kind_Type
            range Turkixir_Number_Lit .. Turkixir_Number_Lit;
      --% no-document: True
      subtype Turkixir_Or_Expr_Range is Turkixir_Node_Kind_Type
            range Turkixir_Or_Expr .. Turkixir_Or_Expr;
      --% no-document: True
      subtype Turkixir_Or_Op_Range is Turkixir_Node_Kind_Type
            range Turkixir_Or_Op .. Turkixir_Or_Op;
      --% no-document: True
      subtype Turkixir_Power_Range is Turkixir_Node_Kind_Type
            range Turkixir_Power .. Turkixir_Power;
      --% no-document: True
      subtype Turkixir_Set_Comp_Range is Turkixir_Node_Kind_Type
            range Turkixir_Set_Comp .. Turkixir_Set_Comp;
      --% no-document: True
      subtype Turkixir_Set_Lit_Range is Turkixir_Node_Kind_Type
            range Turkixir_Set_Lit .. Turkixir_Set_Lit;
      --% no-document: True
      subtype Turkixir_Slice_Expr_Range is Turkixir_Node_Kind_Type
            range Turkixir_Slice_Expr .. Turkixir_Ext_Slice_Expr;
      --% no-document: True
      subtype Turkixir_Ext_Slice_Expr_Range is Turkixir_Node_Kind_Type
            range Turkixir_Ext_Slice_Expr .. Turkixir_Ext_Slice_Expr;
      --% no-document: True
      subtype Turkixir_String_Lit_Range is Turkixir_Node_Kind_Type
            range Turkixir_String_Lit .. Turkixir_String_Lit;
      --% no-document: True
      subtype Turkixir_Subscript_Expr_Range is Turkixir_Node_Kind_Type
            range Turkixir_Subscript_Expr .. Turkixir_Subscript_Expr;
      --% no-document: True
      subtype Turkixir_Tuple_Lit_Range is Turkixir_Node_Kind_Type
            range Turkixir_Tuple_Lit .. Turkixir_Tuple_Lit;
      --% no-document: True
      subtype Turkixir_Xor_Expr_Range is Turkixir_Node_Kind_Type
            range Turkixir_Xor_Expr .. Turkixir_Xor_Expr;
      --% no-document: True
      subtype Turkixir_Yield_Expr_Range is Turkixir_Node_Kind_Type
            range Turkixir_Yield_Expr .. Turkixir_Yield_Expr;
      --% no-document: True
      subtype Turkixir_File_Node_Range is Turkixir_Node_Kind_Type
            range Turkixir_File_Node .. Turkixir_File_Node;
      --% no-document: True
      subtype Turkixir_Import_Star_Range is Turkixir_Node_Kind_Type
            range Turkixir_Import_Star .. Turkixir_Import_Star;
      --% no-document: True
      subtype Turkixir_Kw_Args_Flag is Turkixir_Node_Kind_Type
            range Turkixir_Kw_Args_Flag_Absent .. Turkixir_Kw_Args_Flag_Present;
      --% no-document: True
      subtype Turkixir_Kw_Args_Flag_Absent_Range is Turkixir_Node_Kind_Type
            range Turkixir_Kw_Args_Flag_Absent .. Turkixir_Kw_Args_Flag_Absent;
      --% no-document: True
      subtype Turkixir_Kw_Args_Flag_Present_Range is Turkixir_Node_Kind_Type
            range Turkixir_Kw_Args_Flag_Present .. Turkixir_Kw_Args_Flag_Present;
      --% no-document: True
      subtype Turkixir_NL_Range is Turkixir_Node_Kind_Type
            range Turkixir_NL .. Turkixir_NL;
      --% no-document: True
      subtype Turkixir_Op_Range is Turkixir_Node_Kind_Type
            range Turkixir_Op .. Turkixir_Op;
      --% no-document: True
      subtype Turkixir_Params_Range is Turkixir_Node_Kind_Type
            range Turkixir_Params .. Turkixir_Params;
      --% no-document: True
      subtype Turkixir_Rel_Name_Range is Turkixir_Node_Kind_Type
            range Turkixir_Rel_Name .. Turkixir_Rel_Name;
      --% no-document: True
      subtype Turkixir_Single_Param_Range is Turkixir_Node_Kind_Type
            range Turkixir_Single_Param .. Turkixir_Single_Param;
      --% no-document: True
      subtype Turkixir_Stmt is Turkixir_Node_Kind_Type
            range Turkixir_Assert_Stmt .. Turkixir_With_Stmt;
      --% no-document: True
      subtype Turkixir_Assert_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Assert_Stmt .. Turkixir_Assert_Stmt;
      --% no-document: True
      subtype Turkixir_Assign_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Assign_Stmt .. Turkixir_Assign_Stmt;
      --% no-document: True
      subtype Turkixir_Aug_Assign_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Aug_Assign_Stmt .. Turkixir_Aug_Assign_Stmt;
      --% no-document: True
      subtype Turkixir_Break_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Break_Stmt .. Turkixir_Break_Stmt;
      --% no-document: True
      subtype Turkixir_Continue_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Continue_Stmt .. Turkixir_Continue_Stmt;
      --% no-document: True
      subtype Turkixir_Decorated_Range is Turkixir_Node_Kind_Type
            range Turkixir_Decorated .. Turkixir_Decorated;
      --% no-document: True
      subtype Turkixir_Def_Stmt is Turkixir_Node_Kind_Type
            range Turkixir_Class_Def .. Turkixir_Func_Def;
      --% no-document: True
      subtype Turkixir_Class_Def_Range is Turkixir_Node_Kind_Type
            range Turkixir_Class_Def .. Turkixir_Class_Def;
      --% no-document: True
      subtype Turkixir_Func_Def_Range is Turkixir_Node_Kind_Type
            range Turkixir_Func_Def .. Turkixir_Func_Def;
      --% no-document: True
      subtype Turkixir_Del_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Del_Stmt .. Turkixir_Del_Stmt;
      --% no-document: True
      subtype Turkixir_Elif_Branch_Range is Turkixir_Node_Kind_Type
            range Turkixir_Elif_Branch .. Turkixir_Elif_Branch;
      --% no-document: True
      subtype Turkixir_Exec_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Exec_Stmt .. Turkixir_Exec_Stmt;
      --% no-document: True
      subtype Turkixir_For_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_For_Stmt .. Turkixir_For_Stmt;
      --% no-document: True
      subtype Turkixir_Global_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Global_Stmt .. Turkixir_Global_Stmt;
      --% no-document: True
      subtype Turkixir_If_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_If_Stmt .. Turkixir_If_Stmt;
      --% no-document: True
      subtype Turkixir_Import_From_Range is Turkixir_Node_Kind_Type
            range Turkixir_Import_From .. Turkixir_Import_From;
      --% no-document: True
      subtype Turkixir_Import_Name_Range is Turkixir_Node_Kind_Type
            range Turkixir_Import_Name .. Turkixir_Import_Name;
      --% no-document: True
      subtype Turkixir_Pass_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Pass_Stmt .. Turkixir_Pass_Stmt;
      --% no-document: True
      subtype Turkixir_Print_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Print_Stmt .. Turkixir_Print_Stmt;
      --% no-document: True
      subtype Turkixir_Raise_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Raise_Stmt .. Turkixir_Raise_Stmt;
      --% no-document: True
      subtype Turkixir_Return_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Return_Stmt .. Turkixir_Return_Stmt;
      --% no-document: True
      subtype Turkixir_Stream_Print_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Stream_Print_Stmt .. Turkixir_Stream_Print_Stmt;
      --% no-document: True
      subtype Turkixir_Try_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_Try_Stmt .. Turkixir_Try_Stmt;
      --% no-document: True
      subtype Turkixir_While_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_While_Stmt .. Turkixir_While_Stmt;
      --% no-document: True
      subtype Turkixir_With_Stmt_Range is Turkixir_Node_Kind_Type
            range Turkixir_With_Stmt .. Turkixir_With_Stmt;
      --% no-document: True
      subtype Turkixir_Turkixir_Node_Base_List is Turkixir_Node_Kind_Type
            range Turkixir_Arg_List .. Turkixir_Turkixir_Node_List;
      --% no-document: True
      subtype Turkixir_Arg_List_Range is Turkixir_Node_Kind_Type
            range Turkixir_Arg_List .. Turkixir_Arg_List;
      --% no-document: True
      subtype Turkixir_As_Name_Node_List_Range is Turkixir_Node_Kind_Type
            range Turkixir_As_Name_Node_List .. Turkixir_As_Name_Node_List;
      --% no-document: True
      subtype Turkixir_Decorator_List_Range is Turkixir_Node_Kind_Type
            range Turkixir_Decorator_List .. Turkixir_Decorator_List;
      --% no-document: True
      subtype Turkixir_Dict_Assoc_List_Range is Turkixir_Node_Kind_Type
            range Turkixir_Dict_Assoc_List .. Turkixir_Dict_Assoc_List;
      --% no-document: True
      subtype Turkixir_Dot_List_Range is Turkixir_Node_Kind_Type
            range Turkixir_Dot_List .. Turkixir_Dot_List;
      --% no-document: True
      subtype Turkixir_Elif_Branch_List_Range is Turkixir_Node_Kind_Type
            range Turkixir_Elif_Branch_List .. Turkixir_Elif_Branch_List;
      --% no-document: True
      subtype Turkixir_Except_Part_List_Range is Turkixir_Node_Kind_Type
            range Turkixir_Except_Part_List .. Turkixir_Except_Part_List;
      --% no-document: True
      subtype Turkixir_Expr_List_Range is Turkixir_Node_Kind_Type
            range Turkixir_Expr_List .. Turkixir_Expr_List;
      --% no-document: True
      subtype Turkixir_Id_List_Range is Turkixir_Node_Kind_Type
            range Turkixir_Id_List .. Turkixir_Id_List;
      --% no-document: True
      subtype Turkixir_NL_List_Range is Turkixir_Node_Kind_Type
            range Turkixir_NL_List .. Turkixir_NL_List;
      --% no-document: True
      subtype Turkixir_Single_Param_List_Range is Turkixir_Node_Kind_Type
            range Turkixir_Single_Param_List .. Turkixir_Single_Param_List;
      --% no-document: True
      subtype Turkixir_String_Lit_List_Range is Turkixir_Node_Kind_Type
            range Turkixir_String_Lit_List .. Turkixir_String_Lit_List;
      --% no-document: True
      subtype Turkixir_Turkixir_Node_List_Range is Turkixir_Node_Kind_Type
            range Turkixir_Turkixir_Node_List .. Turkixir_Turkixir_Node_List;
      --% no-document: True
      subtype Turkixir_Var_Args_Flag is Turkixir_Node_Kind_Type
            range Turkixir_Var_Args_Flag_Absent .. Turkixir_Var_Args_Flag_Present;
      --% no-document: True
      subtype Turkixir_Var_Args_Flag_Absent_Range is Turkixir_Node_Kind_Type
            range Turkixir_Var_Args_Flag_Absent .. Turkixir_Var_Args_Flag_Absent;
      --% no-document: True
      subtype Turkixir_Var_Args_Flag_Present_Range is Turkixir_Node_Kind_Type
            range Turkixir_Var_Args_Flag_Present .. Turkixir_Var_Args_Flag_Present;
      --% no-document: True

   subtype Synthetic_Nodes is Turkixir_Node_Kind_Type
      with Static_Predicate =>
         False
   ;
   --  Set of nodes that are synthetic.
      --
      --  Parsers cannot create synthetic nodes, so these correspond to no
      --  source text. These nodes are created dynamically for convenience
      --  during semantic analysis.

      type Analysis_Unit_Kind is
        (Unit_Specification, Unit_Body)
         with Convention => C;
      --  Specify a kind of analysis unit. Specification units provide an
      --  interface to the outer world while body units provide an
      --  implementation for the corresponding interface.


      function Trace_Image (Self : Analysis_Unit_Kind) return String
      is (Self'Image);

      type Lookup_Kind is
        (Recursive, Flat, Minimal)
         with Convention => C;
      


      function Trace_Image (Self : Lookup_Kind) return String
      is (Self'Image);

      type Designated_Env_Kind is
        (None, Current_Env, Named_Env, Direct_Env)
         with Convention => C;
      --  Discriminant for DesignatedEnv structures.


      function Trace_Image (Self : Designated_Env_Kind) return String
      is (Self'Image);

      type Grammar_Rule is
        (Name_Rule, Number_Rule, String_Rule, Cat_String_Rule, Nl_Rule, Main_Rule_Rule, Decorator_Rule, Decorators_Rule, Decorated_Rule, Func_Def_Rule, Parameters_Rule, Varargslist_Rule, Fpdef_Rule, Name_List_Rule, Stmt_Rule, Simple_Stmt_Rule, Small_Stmt_Rule, Expr_Stmt_Rule, Print_Stmt_Rule, Del_Stmt_Rule, Pass_Stmt_Rule, Flow_Stmt_Rule, Break_Stmt_Rule, Continue_Stmt_Rule, Return_Stmt_Rule, Yield_Stmt_Rule, Raise_Stmt_Rule, Import_Stmt_Rule, Import_Name_Rule, Dot_Rule, Import_From_Rule, As_Name_Rule, Dotted_As_Name_Rule, Import_As_Names_Rule, Dotted_As_Names_Rule, Dotted_Name_Rule, Global_Stmt_Rule, Exec_Stmt_Rule, Assert_Stmt_Rule, Compound_Stmt_Rule, Else_Part_Rule, If_Stmt_Rule, While_Stmt_Rule, For_Stmt_Rule, Try_Stmt_Rule, With_Stmt_Rule, With_Item_Rule, Suite_Rule, Test_Rule, Or_Test_Rule, And_Test_Rule, Not_Test_Rule, Comparison_Rule, Expr_Rule, Xor_Expr_Rule, And_Expr_Rule, Shift_Expr_Rule, Arith_Expr_Rule, Term_Rule, Factor_Rule, Power_Rule, Atom_Expr_Rule, Dict_Assoc_Rule, Yield_Expr_Rule, Atom_Rule, Set_Lit_Rule, Lambdef_Rule, Subscript_List_Rule, Subscript_Rule, Expr_List_Rule, Test_List_Rule, Empty_Test_List_Rule, Class_Def_Rule, Arg_List_Rule, List_Iter_Rule, List_For_Rule, List_If_Rule, Comp_Iter_Rule, Comp_For_Rule, Comp_If_Rule)
         with Convention => C;
      --  Gramar rule to use for parsing.


      function Trace_Image (Self : Grammar_Rule) return String
      is (Self'Image);


   Default_Grammar_Rule : constant Grammar_Rule := Main_Rule_Rule;
   --  Default grammar rule to use when parsing analysis units

   type Lexer_Input_Kind is
     (File,
      --  Readable source file

      Bytes_Buffer,
      --  Buffer of undecoded bytes

      Text_Buffer
      --  Buffer of decoded bytes
   );
   --  Kind of lexer input

   subtype Undecoded_Lexer_Input is
      Lexer_Input_Kind range File ..  Bytes_Buffer;

   type Token_Kind is (
      Turkixir_Termination,
Turkixir_Lexing_Failure,
Turkixir_T_T__Rsh_Assign,
Turkixir_T_T__Is,
Turkixir_T_T__Equals,
Turkixir_T_T__Def,
Turkixir_T_T__Lte,
Turkixir_T_T__Raise,
Turkixir_T_T__Mod,
Turkixir_T_T__Yield,
Turkixir_T_T__Xor_Assign,
Turkixir_T_T__As,
Turkixir_T_T__Lambda,
Turkixir_T_T__Backtick,
Turkixir_T_T__Try,
Turkixir_T_T__Divide,
Turkixir_T_T__Invert,
Turkixir_T_T__Return,
Turkixir_T_T__Assert,
Turkixir_T_T__Xor,
Turkixir_T_T__Break,
Turkixir_T_T__Rbrack,
Turkixir_T_T__Power_Assign,
Turkixir_T_T__Import,
Turkixir_T_T__Exec,
Turkixir_T_T__Comma,
Turkixir_T_T_L_Par,
Turkixir_T_T__Dot,
Turkixir_T_T__Gte,
Turkixir_T_T__Floordiv_Assign,
Turkixir_T_T__Multiply,
Turkixir_T_T__Div_Assign,
Turkixir_T_T__At,
Turkixir_T_T__Assign,
Turkixir_T_T__Floordiv,
Turkixir_T_T__Notequal,
Turkixir_T_T__Mult_Assign,
Turkixir_T_T__Mod_Assign,
Turkixir_T_T__Gt,
Turkixir_T_T__Power,
Turkixir_T_T__Amp,
Turkixir_T_T__Not,
Turkixir_T_T__Colon,
Turkixir_T_T__Diamond,
Turkixir_T_T__In,
Turkixir_T_T_L_Curl,
Turkixir_T_T__Class,
Turkixir_T_T__Or_Assign,
Turkixir_T_T__Elif,
Turkixir_T_T__And,
Turkixir_T_T__Semicolon,
Turkixir_T_T__Add_Asign,
Turkixir_T_T__Print,
Turkixir_T_T__Lsh,
Turkixir_T_T__Continue,
Turkixir_T_T__While,
Turkixir_T_T__Except,
Turkixir_T_T__If,
Turkixir_T_T__Else,
Turkixir_T_T__Del,
Turkixir_T_T__Minus_Assign,
Turkixir_T_T__Or,
Turkixir_T_T__Minus,
Turkixir_T_T__Lbrack,
Turkixir_T_T__And_Assign,
Turkixir_T_T_R_Par,
Turkixir_T_T__Global,
Turkixir_T_T__For,
Turkixir_T_T__From,
Turkixir_T_T__Rsh,
Turkixir_T_T__Finally,
Turkixir_T_T__Pass,
Turkixir_T_T__Lsh_Assign,
Turkixir_T_T__Bin_Or,
Turkixir_T_T__Rcurl,
Turkixir_T_T__With,
Turkixir_T_T__Plus,
Turkixir_T_T__Lt,
Turkixir_T_T__Number,
Turkixir_T_T__String,
Turkixir_T_T__Comment,
Turkixir_T_T__Id,
Turkixir_Indent,
Turkixir_Dedent,
Turkixir_Newline
   );
   --  Kind of token: indentifier, string literal, ...

   type Token_Family is
     (Default_Family);
   --  Groups of token kinds, to make the processing of some groups of token
   --  uniform.

   type Indent_Kind is (Indent, Dedent, Nodent, None);
   --  Change of indentation

   Token_Kind_To_Family : array (Token_Kind) of Token_Family :=
     (Turkixir_Termination => Default_Family, Turkixir_Lexing_Failure => Default_Family, Turkixir_T_T__Rsh_Assign => Default_Family, Turkixir_T_T__Is => Default_Family, Turkixir_T_T__Equals => Default_Family, Turkixir_T_T__Def => Default_Family, Turkixir_T_T__Lte => Default_Family, Turkixir_T_T__Raise => Default_Family, Turkixir_T_T__Mod => Default_Family, Turkixir_T_T__Yield => Default_Family, Turkixir_T_T__Xor_Assign => Default_Family, Turkixir_T_T__As => Default_Family, Turkixir_T_T__Lambda => Default_Family, Turkixir_T_T__Backtick => Default_Family, Turkixir_T_T__Try => Default_Family, Turkixir_T_T__Divide => Default_Family, Turkixir_T_T__Invert => Default_Family, Turkixir_T_T__Return => Default_Family, Turkixir_T_T__Assert => Default_Family, Turkixir_T_T__Xor => Default_Family, Turkixir_T_T__Break => Default_Family, Turkixir_T_T__Rbrack => Default_Family, Turkixir_T_T__Power_Assign => Default_Family, Turkixir_T_T__Import => Default_Family, Turkixir_T_T__Exec => Default_Family, Turkixir_T_T__Comma => Default_Family, Turkixir_T_T_L_Par => Default_Family, Turkixir_T_T__Dot => Default_Family, Turkixir_T_T__Gte => Default_Family, Turkixir_T_T__Floordiv_Assign => Default_Family, Turkixir_T_T__Multiply => Default_Family, Turkixir_T_T__Div_Assign => Default_Family, Turkixir_T_T__At => Default_Family, Turkixir_T_T__Assign => Default_Family, Turkixir_T_T__Floordiv => Default_Family, Turkixir_T_T__Notequal => Default_Family, Turkixir_T_T__Mult_Assign => Default_Family, Turkixir_T_T__Mod_Assign => Default_Family, Turkixir_T_T__Gt => Default_Family, Turkixir_T_T__Power => Default_Family, Turkixir_T_T__Amp => Default_Family, Turkixir_T_T__Not => Default_Family, Turkixir_T_T__Colon => Default_Family, Turkixir_T_T__Diamond => Default_Family, Turkixir_T_T__In => Default_Family, Turkixir_T_T_L_Curl => Default_Family, Turkixir_T_T__Class => Default_Family, Turkixir_T_T__Or_Assign => Default_Family, Turkixir_T_T__Elif => Default_Family, Turkixir_T_T__And => Default_Family, Turkixir_T_T__Semicolon => Default_Family, Turkixir_T_T__Add_Asign => Default_Family, Turkixir_T_T__Print => Default_Family, Turkixir_T_T__Lsh => Default_Family, Turkixir_T_T__Continue => Default_Family, Turkixir_T_T__While => Default_Family, Turkixir_T_T__Except => Default_Family, Turkixir_T_T__If => Default_Family, Turkixir_T_T__Else => Default_Family, Turkixir_T_T__Del => Default_Family, Turkixir_T_T__Minus_Assign => Default_Family, Turkixir_T_T__Or => Default_Family, Turkixir_T_T__Minus => Default_Family, Turkixir_T_T__Lbrack => Default_Family, Turkixir_T_T__And_Assign => Default_Family, Turkixir_T_T_R_Par => Default_Family, Turkixir_T_T__Global => Default_Family, Turkixir_T_T__For => Default_Family, Turkixir_T_T__From => Default_Family, Turkixir_T_T__Rsh => Default_Family, Turkixir_T_T__Finally => Default_Family, Turkixir_T_T__Pass => Default_Family, Turkixir_T_T__Lsh_Assign => Default_Family, Turkixir_T_T__Bin_Or => Default_Family, Turkixir_T_T__Rcurl => Default_Family, Turkixir_T_T__With => Default_Family, Turkixir_T_T__Plus => Default_Family, Turkixir_T_T__Lt => Default_Family, Turkixir_T_T__Number => Default_Family, Turkixir_T_T__String => Default_Family, Turkixir_T_T__Comment => Default_Family, Turkixir_T_T__Id => Default_Family, Turkixir_Indent => Default_Family, Turkixir_Dedent => Default_Family, Turkixir_Newline => Default_Family);
   --  Associate a token family to all token kinds
   --
   --% document-value: False

   function Token_Kind_Name (Token_Id : Token_Kind) return String;
   --  Return a human-readable name for a token kind.

   function Token_Kind_Literal (Token_Id : Token_Kind) return Text_Type;
   --  Return the canonical literal corresponding to this token kind, or an
   --  empty string if this token has no literal.

   function Token_Error_Image (Token_Id : Token_Kind) return String;
   --  Return a string representation of ``Token_Id`` that is suitable in error
   --  messages.

   function To_Token_Kind (Raw : Raw_Token_Kind) return Token_Kind
      with Inline;
   function From_Token_Kind (Kind : Token_Kind) return Raw_Token_Kind
      with Inline;

   function Is_Token_Node (Kind : Turkixir_Node_Kind_Type) return Boolean;
   --  Return whether Kind corresponds to a token node

   function Is_List_Node (Kind : Turkixir_Node_Kind_Type) return Boolean;
   --  Return whether Kind corresponds to a list node

   function Is_Error_Node (Kind : Turkixir_Node_Kind_Type) return Boolean;
   --  Return whether Kind corresponds to an error node

   type Visit_Status is (Into, Over, Stop);
   --  Helper type to control the node traversal process. See the
   --  ``Libturkixirlang.Analysis.Traverse`` function.

   -----------------------
   -- Lexical utilities --
   -----------------------

   type Token_Reference is private;
   --  Reference to a token in an analysis unit.

   No_Token : constant Token_Reference;

   type Token_Data_Type is private;

   function "<" (Left, Right : Token_Reference) return Boolean;
   --  Assuming ``Left`` and ``Right`` belong to the same analysis unit, return
   --  whether ``Left`` came before ``Right`` in the source file.

   function Next
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference;
   --  Return a reference to the next token in the corresponding analysis unit.

   function Previous
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference;
   --  Return a reference to the previous token in the corresponding analysis
   --  unit.

   function Data (Token : Token_Reference) return Token_Data_Type;
   --  Return the data associated to ``Token``

   function Is_Equivalent (L, R : Token_Reference) return Boolean;
   --  Return whether ``L`` and ``R`` are structurally equivalent tokens. This
   --  means that their position in the stream won't be taken into account,
   --  only the kind and text of the token.

   function Image (Token : Token_Reference) return String;
   --  Debug helper: return a human-readable text to represent a token

   function Text (Token : Token_Reference) return Text_Type;
   --  Return the text of the token as ``Text_Type``

   function Text (First, Last : Token_Reference) return Text_Type;
   --  Compute the source buffer slice corresponding to the text that spans
   --  between the ``First`` and ``Last`` tokens (both included). This yields
   --  an empty slice if ``Last`` actually appears before ``First``.
   --
   --  This raises a ``Constraint_Error`` if ``First`` and ``Last`` don't
   --  belong to the same analysis unit.

   function Get_Symbol (Token : Token_Reference) return Symbol_Type;
   --  Assuming that ``Token`` refers to a token that contains a symbol, return
   --  the corresponding symbol.

   function Kind (Token_Data : Token_Data_Type) return Token_Kind;
   --  Kind for this token.

   function Is_Trivia (Token : Token_Reference) return Boolean;
   --  Return whether this token is a trivia. If it's not, it's a regular
   --  token.

   function Is_Trivia (Token_Data : Token_Data_Type) return Boolean;
   --  Return whether this token is a trivia. If it's not, it's a regular
   --  token.

   function Index (Token : Token_Reference) return Token_Index;
   --  One-based index for this token/trivia. Tokens and trivias get their own
   --  index space.

   function Index (Token_Data : Token_Data_Type) return Token_Index;
   --  One-based index for this token/trivia. Tokens and trivias get their own
   --  index space.

   function Sloc_Range
     (Token_Data : Token_Data_Type) return Source_Location_Range;
   --  Source location range for this token. Note that the end bound is
   --  exclusive.

   function Origin_Filename (Token : Token_Reference) return String;
   --  Return the name of the file whose content was scanned to create Token.
   --  Return an empty string if the source comes from a memory buffer instead
   --  of a file.

   function Origin_Charset (Token : Token_Reference) return String;
   --  Return the charset used to decode the source that was scanned to create
   --  Token. Return an empty string if the source was already decoded during
   --  the scan.

   function Convert
     (TDH      : Token_Data_Handler;
      Token    : Token_Reference;
      Raw_Data : Stored_Token_Data) return Token_Data_Type;
   --  Turn data from ``TDH`` and ``Raw_Data`` into a user-ready token data
   --  record.

   type Child_Or_Trivia is (Child, Trivia);
   --  Discriminator for the ``Child_Record`` type

   function Raw_Data (T : Token_Reference) return Stored_Token_Data;
   --  Return the raw token data for ``T``

   Invalid_Input : exception renames Langkit_Support.Errors.Invalid_Input;
   --  Raised by lexing functions (``Libturkixirlang.Lexer``) when the input
   --  contains an invalid byte sequence.

   Invalid_Symbol_Error : exception renames Langkit_Support.Errors.Invalid_Symbol_Error;
   --  Exception raise when an invalid symbol is passed to a subprogram.

   Invalid_Unit_Name_Error : exception renames Langkit_Support.Errors.Invalid_Unit_Name_Error;
   --  Raised when an invalid unit name is provided.

   Native_Exception : exception renames Langkit_Support.Errors.Native_Exception;
   --  Exception raised in language bindings when the underlying C API reports
   --  an unexpected error that occurred in the library.
   --
   --  This kind of exception is raised for internal errors: they should never
   --  happen in normal situations and if they are raised at some point, it
   --  means the library state is potentially corrupted.
   --
   --  Nevertheless, the library does its best not to crash the program,
   --  materializing internal errors using this kind of exception.

   Precondition_Failure : exception renames Langkit_Support.Errors.Precondition_Failure;
   --  Exception raised when an API is called while its preconditions are not
   --  satisfied.

   Property_Error : exception renames Langkit_Support.Errors.Property_Error;
   --  Exception that is raised when an error occurs while evaluating any
   --  function whose name starts with ``P_``. This is the only exceptions that
   --  such functions can raise.

   Stale_Reference_Error : exception renames Langkit_Support.Errors.Stale_Reference_Error;
   --  Exception raised while trying to access data that was deallocated. This
   --  happens when one tries to use a node whose unit has been reparsed, for
   --  instance.

   Unknown_Charset : exception renames Langkit_Support.Errors.Unknown_Charset;
   --  Raised by lexing functions (``Libturkixirlang.Lexer``) when the input
   --  charset is not supported.

   -------------------
   -- Introspection --
   -------------------

   Bad_Type_Error : exception renames Langkit_Support.Errors.Introspection.Bad_Type_Error;
   --  Raised when introspection functions (``Libturkixirlang.Introspection``)
   --  are provided mismatching types/values.

   Out_Of_Bounds_Error : exception renames Langkit_Support.Errors.Introspection.Out_Of_Bounds_Error;
   --  Raised when introspection functions (``Libturkixirlang.Introspection``)
   --  are passed an out of bounds index.

   ---------------
   -- Rewriting --
   ---------------

   Template_Args_Error : exception renames Langkit_Support.Errors.Rewriting.Template_Args_Error;
   --  Exception raised when the provided arguments for a template don't match
   --  what the template expects.

   Template_Format_Error : exception renames Langkit_Support.Errors.Rewriting.Template_Format_Error;
   --  Exception raised when a template has an invalid syntax, such as badly
   --  formatted placeholders.

   Template_Instantiation_Error : exception renames Langkit_Support.Errors.Rewriting.Template_Instantiation_Error;
   --  Exception raised when the instantiation of a template cannot be parsed.


   -------------------
   -- Introspection --
   -------------------

   --  Unlike ``Turkixir_Node_Kind_Type``, the following enumeration contains entries
   --  for abstract nodes.

   type Any_Node_Type_Id is (
      None, Turkixir_Node_Type_Id, Arg_Type_Id, Arg_Assoc_Type_Id, Arg_Gen_Type_Id, Kw_Args_Type_Id, Var_Args_Type_Id, As_Name_Node_Type_Id, Comp_If_Type_Id, Comp_Op_Kind_Type_Id, Comp_Op_Kind_Diamond_Type_Id, Comp_Op_Kind_Eq_Type_Id, Comp_Op_Kind_Gt_Type_Id, Comp_Op_Kind_Gte_Type_Id, Comp_Op_Kind_In_Type_Id, Comp_Op_Kind_Is_Type_Id, Comp_Op_Kind_Isnot_Type_Id, Comp_Op_Kind_Lt_Type_Id, Comp_Op_Kind_Lte_Type_Id, Comp_Op_Kind_Noteq_Type_Id, Comp_Op_Kind_Notin_Type_Id, Comprehension_Type_Id, Comp_For_Type_Id, Comp_ForL_Type_Id, Decorator_Type_Id, Dict_Assoc_Type_Id, Else_Part_Type_Id, Except_Part_Type_Id, Expr_Type_Id, And_Expr_Type_Id, And_Op_Type_Id, Bin_Op_Type_Id, Arith_Expr_Type_Id, Shift_Expr_Type_Id, Term_Type_Id, Call_Expr_Type_Id, Comp_Op_Type_Id, Concat_String_Lit_Type_Id, Dict_Comp_Type_Id, Dict_Lit_Type_Id, Dot_Type_Id, Ellipsis_Expr_Type_Id, Factor_Type_Id, If_Expr_Type_Id, Inline_Eval_Type_Id, Lambda_Def_Type_Id, List_Comp_Type_Id, List_Gen_Type_Id, List_Lit_Type_Id, Name_Type_Id, Dotted_Name_Type_Id, Id_Type_Id, Not_Op_Type_Id, Number_Lit_Type_Id, Or_Expr_Type_Id, Or_Op_Type_Id, Power_Type_Id, Set_Comp_Type_Id, Set_Lit_Type_Id, Slice_Expr_Type_Id, Ext_Slice_Expr_Type_Id, String_Lit_Type_Id, Subscript_Expr_Type_Id, Tuple_Lit_Type_Id, Xor_Expr_Type_Id, Yield_Expr_Type_Id, File_Node_Type_Id, Import_Star_Type_Id, Kw_Args_Flag_Type_Id, Kw_Args_Flag_Absent_Type_Id, Kw_Args_Flag_Present_Type_Id, NL_Type_Id, Op_Type_Id, Params_Type_Id, Rel_Name_Type_Id, Single_Param_Type_Id, Stmt_Type_Id, Assert_Stmt_Type_Id, Assign_Stmt_Type_Id, Aug_Assign_Stmt_Type_Id, Break_Stmt_Type_Id, Continue_Stmt_Type_Id, Decorated_Type_Id, Def_Stmt_Type_Id, Class_Def_Type_Id, Func_Def_Type_Id, Del_Stmt_Type_Id, Elif_Branch_Type_Id, Exec_Stmt_Type_Id, For_Stmt_Type_Id, Global_Stmt_Type_Id, If_Stmt_Type_Id, Import_From_Type_Id, Import_Name_Type_Id, Pass_Stmt_Type_Id, Print_Stmt_Type_Id, Raise_Stmt_Type_Id, Return_Stmt_Type_Id, Stream_Print_Stmt_Type_Id, Try_Stmt_Type_Id, While_Stmt_Type_Id, With_Stmt_Type_Id, Turkixir_Node_Base_List_Type_Id, Arg_List_Type_Id, As_Name_Node_List_Type_Id, Decorator_List_Type_Id, Dict_Assoc_List_Type_Id, Dot_List_Type_Id, Elif_Branch_List_Type_Id, Except_Part_List_Type_Id, Expr_List_Type_Id, Id_List_Type_Id, NL_List_Type_Id, Single_Param_List_Type_Id, String_Lit_List_Type_Id, Turkixir_Node_List_Type_Id, Var_Args_Flag_Type_Id, Var_Args_Flag_Absent_Type_Id, Var_Args_Flag_Present_Type_Id
   );

   subtype Node_Type_Id is Any_Node_Type_Id
      range Turkixir_Node_Type_Id
            .. Var_Args_Flag_Present_Type_Id;

   type Node_Type_Id_Array is array (Positive range <>) of Node_Type_Id;

   type Any_Value_Kind is (
      None,
      Boolean_Value,
      Integer_Value,
      Big_Integer_Value,
      Character_Value,
      String_Value,
      Token_Value,
      Unbounded_Text_Value,
      Analysis_Unit_Value,
      Node_Value

      , Analysis_Unit_Kind_Value
      , Lookup_Kind_Value
      , Designated_Env_Kind_Value
      , Grammar_Rule_Value

      , Turkixir_Node_Array_Value
   );
   subtype Value_Kind is
      Any_Value_Kind range Boolean_Value ..  Any_Value_Kind'Last;
   --  Enumeration for all types used to interact with properties

   
   subtype Enum_Value_Kind is Value_Kind with Static_Predicate =>
      Enum_Value_Kind in Analysis_Unit_Kind_Value | Lookup_Kind_Value | Designated_Env_Kind_Value | Grammar_Rule_Value;
   --  Subrange for all enum types

   
   subtype Array_Value_Kind is Value_Kind with Static_Predicate =>
      Array_Value_Kind in Turkixir_Node_Array_Value;
   --  Subrange for all array types

   subtype Struct_Value_Kind is Value_Kind
         range Any_Value_Kind'Last .. Any_Value_Kind'First
   ;
   --  Subrange for all struct types

   type Type_Constraint (Kind : Value_Kind := Value_Kind'First) is record
      case Kind is
         when Node_Value =>
            Node_Type : Node_Type_Id;
            --  Base type for nodes that satisfy this constraint

         when others =>
            null;
      end case;
   end record;
   --  Type constraint for a polymorphic value

   type Type_Constraint_Array is array (Positive range <>) of Type_Constraint;

   

   type Any_Member_Reference is
      (None, Arg_Assoc_F_Name, Arg_Assoc_F_Expr, Arg_Gen_F_Expr, Arg_Gen_F_Comprehension, Kw_Args_F_Expr, Var_Args_F_Expr, As_Name_Node_F_Imported, As_Name_Node_F_As_Name, Comp_If_F_Test, Comp_If_F_Comp, Comp_For_F_Exprs, Comp_For_F_Target, Comp_For_F_Comp, Comp_ForL_F_Exprs, Comp_ForL_F_Target, Comp_ForL_F_Comp, Decorator_F_Dec_Name, Decorator_F_Arg_List, Dict_Assoc_F_Key, Dict_Assoc_F_Value, Else_Part_F_Statements, Except_Part_F_As_Name, Except_Part_F_Statements, And_Expr_F_Left, And_Expr_F_Right, And_Op_F_Left, And_Op_F_Right, Bin_Op_F_Left, Bin_Op_F_Op, Bin_Op_F_Right, Call_Expr_F_Prefix, Call_Expr_F_Suffix, Comp_Op_F_Left, Comp_Op_F_Op, Comp_Op_F_Right, Concat_String_Lit_F_First_Str, Concat_String_Lit_F_Subsequent_Str, Dict_Comp_F_Assoc, Dict_Comp_F_Comprehension, Dict_Lit_F_Assocs, Factor_F_Op, Factor_F_Expr, If_Expr_F_Expr, If_Expr_F_Cond, If_Expr_F_Else_Expr, Inline_Eval_F_Exprs, Lambda_Def_F_Args, Lambda_Def_F_Expr, List_Comp_F_Expr, List_Comp_F_Comprehension, List_Gen_F_Expr, List_Gen_F_Comprehension, List_Lit_F_Exprs, Dotted_Name_F_Prefix, Dotted_Name_F_Suffix, Not_Op_F_Expr, Or_Expr_F_Left, Or_Expr_F_Right, Or_Op_F_Left, Or_Op_F_Right, Power_F_Left, Power_F_Right, Set_Comp_F_Expr, Set_Comp_F_Comprehension, Set_Lit_F_Exprs, Slice_Expr_F_First, Slice_Expr_F_Last, Ext_Slice_Expr_F_Stride, Subscript_Expr_F_Prefix, Subscript_Expr_F_Suffix, Tuple_Lit_F_Exprs, Xor_Expr_F_Left, Xor_Expr_F_Right, Yield_Expr_F_Exprs, File_Node_F_Statements, Params_F_Single_Params, Rel_Name_F_Dots, Rel_Name_F_Name, Single_Param_F_Is_Varargs, Single_Param_F_Is_Kwargs, Single_Param_F_Name, Single_Param_F_Default_Value, Assert_Stmt_F_Test_Expr, Assert_Stmt_F_Msg, Assign_Stmt_F_L_Value, Assign_Stmt_F_R_Values, Aug_Assign_Stmt_F_L_Value, Aug_Assign_Stmt_F_Op, Aug_Assign_Stmt_F_R_Value, Decorated_F_Decorators, Decorated_F_Defn, Class_Def_F_Name, Class_Def_F_Bases, Class_Def_F_Statements, Func_Def_F_Name, Func_Def_F_Parameters, Func_Def_F_Body, Del_Stmt_F_Exprs, Elif_Branch_F_Cond_Test, Elif_Branch_F_Statements, Exec_Stmt_F_Expr, Exec_Stmt_F_In_List, For_Stmt_F_Bindings, For_Stmt_F_Expr, For_Stmt_F_Statements, For_Stmt_F_Else_Part, Global_Stmt_F_Names, If_Stmt_F_Cond_Test, If_Stmt_F_Statements, If_Stmt_F_Elif_Branchs, If_Stmt_F_Else_Part, Import_From_F_Rel_Name, Import_From_F_Imported, Import_Name_F_Imported_Names, Print_Stmt_F_Exprs, Raise_Stmt_F_Exprs, Return_Stmt_F_Exprs, Stream_Print_Stmt_F_Stream_Expr, Stream_Print_Stmt_F_Exprs, Try_Stmt_F_Statements, Try_Stmt_F_Except_Parts, Try_Stmt_F_Else_Part, Try_Stmt_F_Finally_Part, While_Stmt_F_Cond_Test, While_Stmt_F_Statements, While_Stmt_F_Else_Part, With_Stmt_F_Bindings, With_Stmt_F_Statements, Turkixir_Node_Parent, Turkixir_Node_Parents, Turkixir_Node_Children, Turkixir_Node_Token_Start, Turkixir_Node_Token_End, Turkixir_Node_Child_Index, Turkixir_Node_Previous_Sibling, Turkixir_Node_Next_Sibling, Turkixir_Node_Unit, Turkixir_Node_Is_Ghost, Turkixir_Node_Full_Sloc_Image, Kw_Args_Flag_P_As_Bool, Var_Args_Flag_P_As_Bool);
   subtype Member_Reference is Any_Member_Reference range
      Arg_Assoc_F_Name
      ..  Var_Args_Flag_P_As_Bool;
   --  Enumeration of all data attached to structs/nodes (fields and
   --  properties).

   subtype Node_Member_Reference is Member_Reference range
      Arg_Assoc_F_Name
      ..  Var_Args_Flag_P_As_Bool;
   --  Subrange for members of nodes only

   type Member_Reference_Array is
      array (Positive range <>) of Member_Reference;

   subtype Struct_Field_Reference is Member_Reference range
         
      Var_Args_Flag_P_As_Bool
      .. Arg_Assoc_F_Name
   ;

   type Struct_Field_Reference_Array is
      array (Positive range <>) of Struct_Field_Reference;

   subtype Syntax_Field_Reference is Member_Reference range
         
      Arg_Assoc_F_Name
      .. With_Stmt_F_Statements
   ;
   --  Enumeration of all syntax fields for regular nodes

   type Syntax_Field_Reference_Array is
      array (Positive range <>) of Syntax_Field_Reference;

   subtype Property_Reference is Member_Reference
      range Turkixir_Node_Parent
         .. Var_Args_Flag_P_As_Bool;
   --  Enumeration of all available node properties

   type Property_Reference_Array is
      array (Positive range <>) of Property_Reference;

   


private

   type Token_Safety_Net is record
      Context         : Langkit_Support.Internal.Analysis.Internal_Context;
      Context_Version : Version_Number;
      --  Analysis context and version number at the time this safety net was
      --  produced.
      --
      --  TODO: it is not possible to refer to
      --  $.Implementation.Internal_Context from this spec (otherwise we get a
      --  circular dependency). For now, use the generic pointer from
      --  Langkit_Support (hack), but in the future the Token_Reference type
      --  (and this this safety net type) will go to the generic API, so we
      --  will get rid of this hack.

      TDH_Version : Version_Number;
      --  Version of the token data handler at the time this safety net was
      --  produced.
   end record;
   --  Information to embed in public APIs with token references, used to check
   --  before using the references that they are not stale.

   No_Token_Safety_Net : constant Token_Safety_Net :=
     (Langkit_Support.Internal.Analysis.No_Internal_Context, 0, 0);

   type Token_Reference is record
      TDH : Token_Data_Handler_Access;
      --  Token data handler that owns this token

      Index : Token_Or_Trivia_Index;
      --  Identifier for the trivia or the token this refers to

      Safety_Net : Token_Safety_Net;
   end record;

   procedure Check_Safety_Net (Self : Token_Reference);
   --  If ``Self`` is a stale token reference, raise a
   --  ``Stale_Reference_Error`` error.

   No_Token : constant Token_Reference :=
     (null, No_Token_Or_Trivia_Index, No_Token_Safety_Net);

   type Token_Data_Type is record
      Kind : Token_Kind;
      --  See documentation for the Kind accessor

      Is_Trivia : Boolean;
      --  See documentation for the Is_Trivia accessor

      Index : Token_Index;
      --  See documentation for the Index accessor

      Source_Buffer : Text_Cst_Access;
      --  Text for the original source file

      Source_First : Positive;
      Source_Last  : Natural;
      --  Bounds in Source_Buffer for the text corresponding to this token

      Sloc_Range : Source_Location_Range;
      --  See documenation for the Sloc_Range accessor
   end record;

end Libturkixirlang.Common;
