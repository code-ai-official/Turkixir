
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Internal.Introspection;
use Langkit_Support.Internal.Introspection;
with Langkit_Support.Text; use Langkit_Support.Text;

--  This package provides description tables to enable the generic
--  introspection API in Langkit_Support to work with this Langkit-generated
--  library.

private package Libturkixirlang.Generic_Introspection is

   

   --------------------------
   -- Type index constants --
   --------------------------

      Type_Index_For_Analysis_Unit : constant Type_Index := 1;
      Type_Index_For_Big_Integer : constant Type_Index := 2;
      Type_Index_For_Boolean : constant Type_Index := 3;
      Type_Index_For_Character_Type : constant Type_Index := 4;
      Type_Index_For_Env_Rebindings : constant Type_Index := 5;
      Type_Index_For_Integer : constant Type_Index := 6;
      Type_Index_For_Source_Location_Range : constant Type_Index := 7;
      Type_Index_For_Text_Type : constant Type_Index := 8;
      Type_Index_For_Token_Reference : constant Type_Index := 9;
      Type_Index_For_Unbounded_Text_Type : constant Type_Index := 10;
      Type_Index_For_Analysis_Unit_Kind : constant Type_Index := 11;
      Type_Index_For_Lookup_Kind : constant Type_Index := 12;
      Type_Index_For_Designated_Env_Kind : constant Type_Index := 13;
      Type_Index_For_Grammar_Rule : constant Type_Index := 14;
      Type_Index_For_Turkixir_Node_Array : constant Type_Index := 15;
      Type_Index_For_Turkixir_Node : constant Type_Index := 16;
      Type_Index_For_Arg : constant Type_Index := 17;
      Type_Index_For_Arg_Assoc : constant Type_Index := 18;
      Type_Index_For_Arg_Gen : constant Type_Index := 19;
      Type_Index_For_Kw_Args : constant Type_Index := 20;
      Type_Index_For_Var_Args : constant Type_Index := 21;
      Type_Index_For_As_Name_Node : constant Type_Index := 22;
      Type_Index_For_Comp_If : constant Type_Index := 23;
      Type_Index_For_Comp_Op_Kind : constant Type_Index := 24;
      Type_Index_For_Comp_Op_Kind_Diamond : constant Type_Index := 25;
      Type_Index_For_Comp_Op_Kind_Eq : constant Type_Index := 26;
      Type_Index_For_Comp_Op_Kind_Gt : constant Type_Index := 27;
      Type_Index_For_Comp_Op_Kind_Gte : constant Type_Index := 28;
      Type_Index_For_Comp_Op_Kind_In : constant Type_Index := 29;
      Type_Index_For_Comp_Op_Kind_Is : constant Type_Index := 30;
      Type_Index_For_Comp_Op_Kind_Isnot : constant Type_Index := 31;
      Type_Index_For_Comp_Op_Kind_Lt : constant Type_Index := 32;
      Type_Index_For_Comp_Op_Kind_Lte : constant Type_Index := 33;
      Type_Index_For_Comp_Op_Kind_Noteq : constant Type_Index := 34;
      Type_Index_For_Comp_Op_Kind_Notin : constant Type_Index := 35;
      Type_Index_For_Comprehension : constant Type_Index := 36;
      Type_Index_For_Comp_For : constant Type_Index := 37;
      Type_Index_For_Comp_ForL : constant Type_Index := 38;
      Type_Index_For_Decorator : constant Type_Index := 39;
      Type_Index_For_Dict_Assoc : constant Type_Index := 40;
      Type_Index_For_Else_Part : constant Type_Index := 41;
      Type_Index_For_Except_Part : constant Type_Index := 42;
      Type_Index_For_Expr : constant Type_Index := 43;
      Type_Index_For_And_Expr : constant Type_Index := 44;
      Type_Index_For_And_Op : constant Type_Index := 45;
      Type_Index_For_Bin_Op : constant Type_Index := 46;
      Type_Index_For_Arith_Expr : constant Type_Index := 47;
      Type_Index_For_Shift_Expr : constant Type_Index := 48;
      Type_Index_For_Term : constant Type_Index := 49;
      Type_Index_For_Call_Expr : constant Type_Index := 50;
      Type_Index_For_Comp_Op : constant Type_Index := 51;
      Type_Index_For_Concat_String_Lit : constant Type_Index := 52;
      Type_Index_For_Dict_Comp : constant Type_Index := 53;
      Type_Index_For_Dict_Lit : constant Type_Index := 54;
      Type_Index_For_Dot : constant Type_Index := 55;
      Type_Index_For_Ellipsis_Expr : constant Type_Index := 56;
      Type_Index_For_Factor : constant Type_Index := 57;
      Type_Index_For_If_Expr : constant Type_Index := 58;
      Type_Index_For_Inline_Eval : constant Type_Index := 59;
      Type_Index_For_Lambda_Def : constant Type_Index := 60;
      Type_Index_For_List_Comp : constant Type_Index := 61;
      Type_Index_For_List_Gen : constant Type_Index := 62;
      Type_Index_For_List_Lit : constant Type_Index := 63;
      Type_Index_For_Name : constant Type_Index := 64;
      Type_Index_For_Dotted_Name : constant Type_Index := 65;
      Type_Index_For_Id : constant Type_Index := 66;
      Type_Index_For_Not_Op : constant Type_Index := 67;
      Type_Index_For_Number_Lit : constant Type_Index := 68;
      Type_Index_For_Or_Expr : constant Type_Index := 69;
      Type_Index_For_Or_Op : constant Type_Index := 70;
      Type_Index_For_Power : constant Type_Index := 71;
      Type_Index_For_Set_Comp : constant Type_Index := 72;
      Type_Index_For_Set_Lit : constant Type_Index := 73;
      Type_Index_For_Slice_Expr : constant Type_Index := 74;
      Type_Index_For_Ext_Slice_Expr : constant Type_Index := 75;
      Type_Index_For_String_Lit : constant Type_Index := 76;
      Type_Index_For_Subscript_Expr : constant Type_Index := 77;
      Type_Index_For_Tuple_Lit : constant Type_Index := 78;
      Type_Index_For_Xor_Expr : constant Type_Index := 79;
      Type_Index_For_Yield_Expr : constant Type_Index := 80;
      Type_Index_For_File_Node : constant Type_Index := 81;
      Type_Index_For_Import_Star : constant Type_Index := 82;
      Type_Index_For_Kw_Args_Flag : constant Type_Index := 83;
      Type_Index_For_Kw_Args_Flag_Absent : constant Type_Index := 84;
      Type_Index_For_Kw_Args_Flag_Present : constant Type_Index := 85;
      Type_Index_For_NL : constant Type_Index := 86;
      Type_Index_For_Op : constant Type_Index := 87;
      Type_Index_For_Params : constant Type_Index := 88;
      Type_Index_For_Rel_Name : constant Type_Index := 89;
      Type_Index_For_Single_Param : constant Type_Index := 90;
      Type_Index_For_Stmt : constant Type_Index := 91;
      Type_Index_For_Assert_Stmt : constant Type_Index := 92;
      Type_Index_For_Assign_Stmt : constant Type_Index := 93;
      Type_Index_For_Aug_Assign_Stmt : constant Type_Index := 94;
      Type_Index_For_Break_Stmt : constant Type_Index := 95;
      Type_Index_For_Continue_Stmt : constant Type_Index := 96;
      Type_Index_For_Decorated : constant Type_Index := 97;
      Type_Index_For_Def_Stmt : constant Type_Index := 98;
      Type_Index_For_Class_Def : constant Type_Index := 99;
      Type_Index_For_Func_Def : constant Type_Index := 100;
      Type_Index_For_Del_Stmt : constant Type_Index := 101;
      Type_Index_For_Elif_Branch : constant Type_Index := 102;
      Type_Index_For_Exec_Stmt : constant Type_Index := 103;
      Type_Index_For_For_Stmt : constant Type_Index := 104;
      Type_Index_For_Global_Stmt : constant Type_Index := 105;
      Type_Index_For_If_Stmt : constant Type_Index := 106;
      Type_Index_For_Import_From : constant Type_Index := 107;
      Type_Index_For_Import_Name : constant Type_Index := 108;
      Type_Index_For_Pass_Stmt : constant Type_Index := 109;
      Type_Index_For_Print_Stmt : constant Type_Index := 110;
      Type_Index_For_Raise_Stmt : constant Type_Index := 111;
      Type_Index_For_Return_Stmt : constant Type_Index := 112;
      Type_Index_For_Stream_Print_Stmt : constant Type_Index := 113;
      Type_Index_For_Try_Stmt : constant Type_Index := 114;
      Type_Index_For_While_Stmt : constant Type_Index := 115;
      Type_Index_For_With_Stmt : constant Type_Index := 116;
      Type_Index_For_Turkixir_Node_Base_List : constant Type_Index := 117;
      Type_Index_For_Arg_List : constant Type_Index := 118;
      Type_Index_For_As_Name_Node_List : constant Type_Index := 119;
      Type_Index_For_Decorator_List : constant Type_Index := 120;
      Type_Index_For_Dict_Assoc_List : constant Type_Index := 121;
      Type_Index_For_Dot_List : constant Type_Index := 122;
      Type_Index_For_Elif_Branch_List : constant Type_Index := 123;
      Type_Index_For_Except_Part_List : constant Type_Index := 124;
      Type_Index_For_Expr_List : constant Type_Index := 125;
      Type_Index_For_Id_List : constant Type_Index := 126;
      Type_Index_For_NL_List : constant Type_Index := 127;
      Type_Index_For_Single_Param_List : constant Type_Index := 128;
      Type_Index_For_String_Lit_List : constant Type_Index := 129;
      Type_Index_For_Turkixir_Node_List : constant Type_Index := 130;
      Type_Index_For_Var_Args_Flag : constant Type_Index := 131;
      Type_Index_For_Var_Args_Flag_Absent : constant Type_Index := 132;
      Type_Index_For_Var_Args_Flag_Present : constant Type_Index := 133;

   ----------------------------
   -- Member index constants --
   ----------------------------

      Member_Index_For_Arg_Assoc_F_Name : constant Struct_Member_Index := 1;
      Member_Index_For_Arg_Assoc_F_Expr : constant Struct_Member_Index := 2;
      Member_Index_For_Arg_Gen_F_Expr : constant Struct_Member_Index := 3;
      Member_Index_For_Arg_Gen_F_Comprehension : constant Struct_Member_Index := 4;
      Member_Index_For_Kw_Args_F_Expr : constant Struct_Member_Index := 5;
      Member_Index_For_Var_Args_F_Expr : constant Struct_Member_Index := 6;
      Member_Index_For_As_Name_Node_F_Imported : constant Struct_Member_Index := 7;
      Member_Index_For_As_Name_Node_F_As_Name : constant Struct_Member_Index := 8;
      Member_Index_For_Comp_If_F_Test : constant Struct_Member_Index := 9;
      Member_Index_For_Comp_If_F_Comp : constant Struct_Member_Index := 10;
      Member_Index_For_Comp_For_F_Exprs : constant Struct_Member_Index := 11;
      Member_Index_For_Comp_For_F_Target : constant Struct_Member_Index := 12;
      Member_Index_For_Comp_For_F_Comp : constant Struct_Member_Index := 13;
      Member_Index_For_Comp_ForL_F_Exprs : constant Struct_Member_Index := 14;
      Member_Index_For_Comp_ForL_F_Target : constant Struct_Member_Index := 15;
      Member_Index_For_Comp_ForL_F_Comp : constant Struct_Member_Index := 16;
      Member_Index_For_Decorator_F_Dec_Name : constant Struct_Member_Index := 17;
      Member_Index_For_Decorator_F_Arg_List : constant Struct_Member_Index := 18;
      Member_Index_For_Dict_Assoc_F_Key : constant Struct_Member_Index := 19;
      Member_Index_For_Dict_Assoc_F_Value : constant Struct_Member_Index := 20;
      Member_Index_For_Else_Part_F_Statements : constant Struct_Member_Index := 21;
      Member_Index_For_Except_Part_F_As_Name : constant Struct_Member_Index := 22;
      Member_Index_For_Except_Part_F_Statements : constant Struct_Member_Index := 23;
      Member_Index_For_And_Expr_F_Left : constant Struct_Member_Index := 24;
      Member_Index_For_And_Expr_F_Right : constant Struct_Member_Index := 25;
      Member_Index_For_And_Op_F_Left : constant Struct_Member_Index := 26;
      Member_Index_For_And_Op_F_Right : constant Struct_Member_Index := 27;
      Member_Index_For_Bin_Op_F_Left : constant Struct_Member_Index := 28;
      Member_Index_For_Bin_Op_F_Op : constant Struct_Member_Index := 29;
      Member_Index_For_Bin_Op_F_Right : constant Struct_Member_Index := 30;
      Member_Index_For_Call_Expr_F_Prefix : constant Struct_Member_Index := 31;
      Member_Index_For_Call_Expr_F_Suffix : constant Struct_Member_Index := 32;
      Member_Index_For_Comp_Op_F_Left : constant Struct_Member_Index := 33;
      Member_Index_For_Comp_Op_F_Op : constant Struct_Member_Index := 34;
      Member_Index_For_Comp_Op_F_Right : constant Struct_Member_Index := 35;
      Member_Index_For_Concat_String_Lit_F_First_Str : constant Struct_Member_Index := 36;
      Member_Index_For_Concat_String_Lit_F_Subsequent_Str : constant Struct_Member_Index := 37;
      Member_Index_For_Dict_Comp_F_Assoc : constant Struct_Member_Index := 38;
      Member_Index_For_Dict_Comp_F_Comprehension : constant Struct_Member_Index := 39;
      Member_Index_For_Dict_Lit_F_Assocs : constant Struct_Member_Index := 40;
      Member_Index_For_Factor_F_Op : constant Struct_Member_Index := 41;
      Member_Index_For_Factor_F_Expr : constant Struct_Member_Index := 42;
      Member_Index_For_If_Expr_F_Expr : constant Struct_Member_Index := 43;
      Member_Index_For_If_Expr_F_Cond : constant Struct_Member_Index := 44;
      Member_Index_For_If_Expr_F_Else_Expr : constant Struct_Member_Index := 45;
      Member_Index_For_Inline_Eval_F_Exprs : constant Struct_Member_Index := 46;
      Member_Index_For_Lambda_Def_F_Args : constant Struct_Member_Index := 47;
      Member_Index_For_Lambda_Def_F_Expr : constant Struct_Member_Index := 48;
      Member_Index_For_List_Comp_F_Expr : constant Struct_Member_Index := 49;
      Member_Index_For_List_Comp_F_Comprehension : constant Struct_Member_Index := 50;
      Member_Index_For_List_Gen_F_Expr : constant Struct_Member_Index := 51;
      Member_Index_For_List_Gen_F_Comprehension : constant Struct_Member_Index := 52;
      Member_Index_For_List_Lit_F_Exprs : constant Struct_Member_Index := 53;
      Member_Index_For_Dotted_Name_F_Prefix : constant Struct_Member_Index := 54;
      Member_Index_For_Dotted_Name_F_Suffix : constant Struct_Member_Index := 55;
      Member_Index_For_Not_Op_F_Expr : constant Struct_Member_Index := 56;
      Member_Index_For_Or_Expr_F_Left : constant Struct_Member_Index := 57;
      Member_Index_For_Or_Expr_F_Right : constant Struct_Member_Index := 58;
      Member_Index_For_Or_Op_F_Left : constant Struct_Member_Index := 59;
      Member_Index_For_Or_Op_F_Right : constant Struct_Member_Index := 60;
      Member_Index_For_Power_F_Left : constant Struct_Member_Index := 61;
      Member_Index_For_Power_F_Right : constant Struct_Member_Index := 62;
      Member_Index_For_Set_Comp_F_Expr : constant Struct_Member_Index := 63;
      Member_Index_For_Set_Comp_F_Comprehension : constant Struct_Member_Index := 64;
      Member_Index_For_Set_Lit_F_Exprs : constant Struct_Member_Index := 65;
      Member_Index_For_Slice_Expr_F_First : constant Struct_Member_Index := 66;
      Member_Index_For_Slice_Expr_F_Last : constant Struct_Member_Index := 67;
      Member_Index_For_Ext_Slice_Expr_F_Stride : constant Struct_Member_Index := 68;
      Member_Index_For_Subscript_Expr_F_Prefix : constant Struct_Member_Index := 69;
      Member_Index_For_Subscript_Expr_F_Suffix : constant Struct_Member_Index := 70;
      Member_Index_For_Tuple_Lit_F_Exprs : constant Struct_Member_Index := 71;
      Member_Index_For_Xor_Expr_F_Left : constant Struct_Member_Index := 72;
      Member_Index_For_Xor_Expr_F_Right : constant Struct_Member_Index := 73;
      Member_Index_For_Yield_Expr_F_Exprs : constant Struct_Member_Index := 74;
      Member_Index_For_File_Node_F_Statements : constant Struct_Member_Index := 75;
      Member_Index_For_Params_F_Single_Params : constant Struct_Member_Index := 76;
      Member_Index_For_Rel_Name_F_Dots : constant Struct_Member_Index := 77;
      Member_Index_For_Rel_Name_F_Name : constant Struct_Member_Index := 78;
      Member_Index_For_Single_Param_F_Is_Varargs : constant Struct_Member_Index := 79;
      Member_Index_For_Single_Param_F_Is_Kwargs : constant Struct_Member_Index := 80;
      Member_Index_For_Single_Param_F_Name : constant Struct_Member_Index := 81;
      Member_Index_For_Single_Param_F_Default_Value : constant Struct_Member_Index := 82;
      Member_Index_For_Assert_Stmt_F_Test_Expr : constant Struct_Member_Index := 83;
      Member_Index_For_Assert_Stmt_F_Msg : constant Struct_Member_Index := 84;
      Member_Index_For_Assign_Stmt_F_L_Value : constant Struct_Member_Index := 85;
      Member_Index_For_Assign_Stmt_F_R_Values : constant Struct_Member_Index := 86;
      Member_Index_For_Aug_Assign_Stmt_F_L_Value : constant Struct_Member_Index := 87;
      Member_Index_For_Aug_Assign_Stmt_F_Op : constant Struct_Member_Index := 88;
      Member_Index_For_Aug_Assign_Stmt_F_R_Value : constant Struct_Member_Index := 89;
      Member_Index_For_Decorated_F_Decorators : constant Struct_Member_Index := 90;
      Member_Index_For_Decorated_F_Defn : constant Struct_Member_Index := 91;
      Member_Index_For_Class_Def_F_Name : constant Struct_Member_Index := 92;
      Member_Index_For_Class_Def_F_Bases : constant Struct_Member_Index := 93;
      Member_Index_For_Class_Def_F_Statements : constant Struct_Member_Index := 94;
      Member_Index_For_Func_Def_F_Name : constant Struct_Member_Index := 95;
      Member_Index_For_Func_Def_F_Parameters : constant Struct_Member_Index := 96;
      Member_Index_For_Func_Def_F_Body : constant Struct_Member_Index := 97;
      Member_Index_For_Del_Stmt_F_Exprs : constant Struct_Member_Index := 98;
      Member_Index_For_Elif_Branch_F_Cond_Test : constant Struct_Member_Index := 99;
      Member_Index_For_Elif_Branch_F_Statements : constant Struct_Member_Index := 100;
      Member_Index_For_Exec_Stmt_F_Expr : constant Struct_Member_Index := 101;
      Member_Index_For_Exec_Stmt_F_In_List : constant Struct_Member_Index := 102;
      Member_Index_For_For_Stmt_F_Bindings : constant Struct_Member_Index := 103;
      Member_Index_For_For_Stmt_F_Expr : constant Struct_Member_Index := 104;
      Member_Index_For_For_Stmt_F_Statements : constant Struct_Member_Index := 105;
      Member_Index_For_For_Stmt_F_Else_Part : constant Struct_Member_Index := 106;
      Member_Index_For_Global_Stmt_F_Names : constant Struct_Member_Index := 107;
      Member_Index_For_If_Stmt_F_Cond_Test : constant Struct_Member_Index := 108;
      Member_Index_For_If_Stmt_F_Statements : constant Struct_Member_Index := 109;
      Member_Index_For_If_Stmt_F_Elif_Branchs : constant Struct_Member_Index := 110;
      Member_Index_For_If_Stmt_F_Else_Part : constant Struct_Member_Index := 111;
      Member_Index_For_Import_From_F_Rel_Name : constant Struct_Member_Index := 112;
      Member_Index_For_Import_From_F_Imported : constant Struct_Member_Index := 113;
      Member_Index_For_Import_Name_F_Imported_Names : constant Struct_Member_Index := 114;
      Member_Index_For_Print_Stmt_F_Exprs : constant Struct_Member_Index := 115;
      Member_Index_For_Raise_Stmt_F_Exprs : constant Struct_Member_Index := 116;
      Member_Index_For_Return_Stmt_F_Exprs : constant Struct_Member_Index := 117;
      Member_Index_For_Stream_Print_Stmt_F_Stream_Expr : constant Struct_Member_Index := 118;
      Member_Index_For_Stream_Print_Stmt_F_Exprs : constant Struct_Member_Index := 119;
      Member_Index_For_Try_Stmt_F_Statements : constant Struct_Member_Index := 120;
      Member_Index_For_Try_Stmt_F_Except_Parts : constant Struct_Member_Index := 121;
      Member_Index_For_Try_Stmt_F_Else_Part : constant Struct_Member_Index := 122;
      Member_Index_For_Try_Stmt_F_Finally_Part : constant Struct_Member_Index := 123;
      Member_Index_For_While_Stmt_F_Cond_Test : constant Struct_Member_Index := 124;
      Member_Index_For_While_Stmt_F_Statements : constant Struct_Member_Index := 125;
      Member_Index_For_While_Stmt_F_Else_Part : constant Struct_Member_Index := 126;
      Member_Index_For_With_Stmt_F_Bindings : constant Struct_Member_Index := 127;
      Member_Index_For_With_Stmt_F_Statements : constant Struct_Member_Index := 128;
      Member_Index_For_Parent : constant Struct_Member_Index := 129;
      Member_Index_For_Parents : constant Struct_Member_Index := 130;
      Member_Index_For_Children : constant Struct_Member_Index := 131;
      Member_Index_For_Token_Start : constant Struct_Member_Index := 132;
      Member_Index_For_Token_End : constant Struct_Member_Index := 133;
      Member_Index_For_Child_Index : constant Struct_Member_Index := 134;
      Member_Index_For_Previous_Sibling : constant Struct_Member_Index := 135;
      Member_Index_For_Next_Sibling : constant Struct_Member_Index := 136;
      Member_Index_For_Unit : constant Struct_Member_Index := 137;
      Member_Index_For_Is_Ghost : constant Struct_Member_Index := 138;
      Member_Index_For_Full_Sloc_Image : constant Struct_Member_Index := 139;
      Member_Index_For_Dispatcher_Kw_Args_Flag_P_As_Bool : constant Struct_Member_Index := 140;
      Member_Index_For_Dispatcher_Var_Args_Flag_P_As_Bool : constant Struct_Member_Index := 141;

   ------------------------------------
   -- General value type descriptors --
   ------------------------------------

   
      
      Debug_Name_For_Internal_Unit : aliased constant String :=
        "AnalysisUnit";
      Desc_For_Internal_Unit : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Unit'Access);
      
      Debug_Name_For_Big_Integer_Type : aliased constant String :=
        "BigInt";
      Desc_For_Big_Integer_Type : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Big_Integer_Type'Access);
      
      Debug_Name_For_Boolean : aliased constant String :=
        "Bool";
      Desc_For_Boolean : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Boolean'Access);
      
      Debug_Name_For_Character_Type : aliased constant String :=
        "Character";
      Desc_For_Character_Type : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Character_Type'Access);
      
      Debug_Name_For_Env_Rebindings : aliased constant String :=
        "EnvRebindings";
      Desc_For_Env_Rebindings : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Env_Rebindings'Access);
      
      Debug_Name_For_Integer : aliased constant String :=
        "Int";
      Desc_For_Integer : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Integer'Access);
      
      Debug_Name_For_Source_Location_Range : aliased constant String :=
        "SourceLocationRange";
      Desc_For_Source_Location_Range : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Source_Location_Range'Access);
      
      Debug_Name_For_String_Type : aliased constant String :=
        "String";
      Desc_For_String_Type : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_String_Type'Access);
      
      Debug_Name_For_Token_Reference : aliased constant String :=
        "Token";
      Desc_For_Token_Reference : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Token_Reference'Access);
      
      Debug_Name_For_Symbol_Type : aliased constant String :=
        "Symbol";
      Desc_For_Symbol_Type : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Symbol_Type'Access);
      
      Debug_Name_For_Analysis_Unit_Kind : aliased constant String :=
        "AnalysisUnitKind";
      Desc_For_Analysis_Unit_Kind : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Analysis_Unit_Kind'Access);
      
      Debug_Name_For_Lookup_Kind : aliased constant String :=
        "LookupKind";
      Desc_For_Lookup_Kind : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Lookup_Kind'Access);
      
      Debug_Name_For_Designated_Env_Kind : aliased constant String :=
        "DesignatedEnvKind";
      Desc_For_Designated_Env_Kind : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Designated_Env_Kind'Access);
      
      Debug_Name_For_Grammar_Rule : aliased constant String :=
        "GrammarRule";
      Desc_For_Grammar_Rule : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Grammar_Rule'Access);
      
      Debug_Name_For_Internal_Entity_Array_Access : aliased constant String :=
        "TurkixirNode.entity.array";
      Desc_For_Internal_Entity_Array_Access : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Array_Access'Access);
      
      Debug_Name_For_Internal_Entity : aliased constant String :=
        "TurkixirNode.entity";
      Desc_For_Internal_Entity : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity'Access);
      
      Debug_Name_For_Internal_Entity_Arg : aliased constant String :=
        "Arg.entity";
      Desc_For_Internal_Entity_Arg : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Arg'Access);
      
      Debug_Name_For_Internal_Entity_Arg_Assoc : aliased constant String :=
        "ArgAssoc.entity";
      Desc_For_Internal_Entity_Arg_Assoc : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Arg_Assoc'Access);
      
      Debug_Name_For_Internal_Entity_Arg_Gen : aliased constant String :=
        "ArgGen.entity";
      Desc_For_Internal_Entity_Arg_Gen : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Arg_Gen'Access);
      
      Debug_Name_For_Internal_Entity_Kw_Args : aliased constant String :=
        "KwArgs.entity";
      Desc_For_Internal_Entity_Kw_Args : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Kw_Args'Access);
      
      Debug_Name_For_Internal_Entity_Var_Args : aliased constant String :=
        "VarArgs.entity";
      Desc_For_Internal_Entity_Var_Args : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Var_Args'Access);
      
      Debug_Name_For_Internal_Entity_As_Name_Node : aliased constant String :=
        "AsNameNode.entity";
      Desc_For_Internal_Entity_As_Name_Node : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_As_Name_Node'Access);
      
      Debug_Name_For_Internal_Entity_Comp_If : aliased constant String :=
        "CompIf.entity";
      Desc_For_Internal_Entity_Comp_If : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_If'Access);
      
      Debug_Name_For_Internal_Entity_Comp_Op_Kind : aliased constant String :=
        "CompOpKind.entity";
      Desc_For_Internal_Entity_Comp_Op_Kind : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_Op_Kind'Access);
      
      Debug_Name_For_Internal_Entity_Comp_Op_Kind_Diamond : aliased constant String :=
        "CompOpKind.Diamond.entity";
      Desc_For_Internal_Entity_Comp_Op_Kind_Diamond : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_Op_Kind_Diamond'Access);
      
      Debug_Name_For_Internal_Entity_Comp_Op_Kind_Eq : aliased constant String :=
        "CompOpKind.Eq.entity";
      Desc_For_Internal_Entity_Comp_Op_Kind_Eq : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_Op_Kind_Eq'Access);
      
      Debug_Name_For_Internal_Entity_Comp_Op_Kind_Gt : aliased constant String :=
        "CompOpKind.Gt.entity";
      Desc_For_Internal_Entity_Comp_Op_Kind_Gt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_Op_Kind_Gt'Access);
      
      Debug_Name_For_Internal_Entity_Comp_Op_Kind_Gte : aliased constant String :=
        "CompOpKind.Gte.entity";
      Desc_For_Internal_Entity_Comp_Op_Kind_Gte : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_Op_Kind_Gte'Access);
      
      Debug_Name_For_Internal_Entity_Comp_Op_Kind_In : aliased constant String :=
        "CompOpKind.In.entity";
      Desc_For_Internal_Entity_Comp_Op_Kind_In : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_Op_Kind_In'Access);
      
      Debug_Name_For_Internal_Entity_Comp_Op_Kind_Is : aliased constant String :=
        "CompOpKind.Is.entity";
      Desc_For_Internal_Entity_Comp_Op_Kind_Is : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_Op_Kind_Is'Access);
      
      Debug_Name_For_Internal_Entity_Comp_Op_Kind_Isnot : aliased constant String :=
        "CompOpKind.Isnot.entity";
      Desc_For_Internal_Entity_Comp_Op_Kind_Isnot : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_Op_Kind_Isnot'Access);
      
      Debug_Name_For_Internal_Entity_Comp_Op_Kind_Lt : aliased constant String :=
        "CompOpKind.Lt.entity";
      Desc_For_Internal_Entity_Comp_Op_Kind_Lt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_Op_Kind_Lt'Access);
      
      Debug_Name_For_Internal_Entity_Comp_Op_Kind_Lte : aliased constant String :=
        "CompOpKind.Lte.entity";
      Desc_For_Internal_Entity_Comp_Op_Kind_Lte : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_Op_Kind_Lte'Access);
      
      Debug_Name_For_Internal_Entity_Comp_Op_Kind_Noteq : aliased constant String :=
        "CompOpKind.Noteq.entity";
      Desc_For_Internal_Entity_Comp_Op_Kind_Noteq : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_Op_Kind_Noteq'Access);
      
      Debug_Name_For_Internal_Entity_Comp_Op_Kind_Notin : aliased constant String :=
        "CompOpKind.Notin.entity";
      Desc_For_Internal_Entity_Comp_Op_Kind_Notin : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_Op_Kind_Notin'Access);
      
      Debug_Name_For_Internal_Entity_Comprehension : aliased constant String :=
        "Comprehension.entity";
      Desc_For_Internal_Entity_Comprehension : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comprehension'Access);
      
      Debug_Name_For_Internal_Entity_Comp_For : aliased constant String :=
        "CompFor.entity";
      Desc_For_Internal_Entity_Comp_For : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_For'Access);
      
      Debug_Name_For_Internal_Entity_Comp_ForL : aliased constant String :=
        "CompForL.entity";
      Desc_For_Internal_Entity_Comp_ForL : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_ForL'Access);
      
      Debug_Name_For_Internal_Entity_Decorator : aliased constant String :=
        "Decorator.entity";
      Desc_For_Internal_Entity_Decorator : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Decorator'Access);
      
      Debug_Name_For_Internal_Entity_Dict_Assoc : aliased constant String :=
        "DictAssoc.entity";
      Desc_For_Internal_Entity_Dict_Assoc : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Dict_Assoc'Access);
      
      Debug_Name_For_Internal_Entity_Else_Part : aliased constant String :=
        "ElsePart.entity";
      Desc_For_Internal_Entity_Else_Part : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Else_Part'Access);
      
      Debug_Name_For_Internal_Entity_Except_Part : aliased constant String :=
        "ExceptPart.entity";
      Desc_For_Internal_Entity_Except_Part : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Except_Part'Access);
      
      Debug_Name_For_Internal_Entity_Expr : aliased constant String :=
        "Expr.entity";
      Desc_For_Internal_Entity_Expr : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Expr'Access);
      
      Debug_Name_For_Internal_Entity_And_Expr : aliased constant String :=
        "AndExpr.entity";
      Desc_For_Internal_Entity_And_Expr : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_And_Expr'Access);
      
      Debug_Name_For_Internal_Entity_And_Op : aliased constant String :=
        "AndOp.entity";
      Desc_For_Internal_Entity_And_Op : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_And_Op'Access);
      
      Debug_Name_For_Internal_Entity_Bin_Op : aliased constant String :=
        "BinOp.entity";
      Desc_For_Internal_Entity_Bin_Op : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Bin_Op'Access);
      
      Debug_Name_For_Internal_Entity_Arith_Expr : aliased constant String :=
        "ArithExpr.entity";
      Desc_For_Internal_Entity_Arith_Expr : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Arith_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Shift_Expr : aliased constant String :=
        "ShiftExpr.entity";
      Desc_For_Internal_Entity_Shift_Expr : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Shift_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Term : aliased constant String :=
        "Term.entity";
      Desc_For_Internal_Entity_Term : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Term'Access);
      
      Debug_Name_For_Internal_Entity_Call_Expr : aliased constant String :=
        "CallExpr.entity";
      Desc_For_Internal_Entity_Call_Expr : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Call_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Comp_Op : aliased constant String :=
        "CompOp.entity";
      Desc_For_Internal_Entity_Comp_Op : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Comp_Op'Access);
      
      Debug_Name_For_Internal_Entity_Concat_String_Lit : aliased constant String :=
        "ConcatStringLit.entity";
      Desc_For_Internal_Entity_Concat_String_Lit : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Concat_String_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Dict_Comp : aliased constant String :=
        "DictComp.entity";
      Desc_For_Internal_Entity_Dict_Comp : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Dict_Comp'Access);
      
      Debug_Name_For_Internal_Entity_Dict_Lit : aliased constant String :=
        "DictLit.entity";
      Desc_For_Internal_Entity_Dict_Lit : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Dict_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Dot : aliased constant String :=
        "Dot.entity";
      Desc_For_Internal_Entity_Dot : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Dot'Access);
      
      Debug_Name_For_Internal_Entity_Ellipsis_Expr : aliased constant String :=
        "EllipsisExpr.entity";
      Desc_For_Internal_Entity_Ellipsis_Expr : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Ellipsis_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Factor : aliased constant String :=
        "Factor.entity";
      Desc_For_Internal_Entity_Factor : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Factor'Access);
      
      Debug_Name_For_Internal_Entity_If_Expr : aliased constant String :=
        "IfExpr.entity";
      Desc_For_Internal_Entity_If_Expr : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_If_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Inline_Eval : aliased constant String :=
        "InlineEval.entity";
      Desc_For_Internal_Entity_Inline_Eval : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Inline_Eval'Access);
      
      Debug_Name_For_Internal_Entity_Lambda_Def : aliased constant String :=
        "LambdaDef.entity";
      Desc_For_Internal_Entity_Lambda_Def : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Lambda_Def'Access);
      
      Debug_Name_For_Internal_Entity_List_Comp : aliased constant String :=
        "ListComp.entity";
      Desc_For_Internal_Entity_List_Comp : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_List_Comp'Access);
      
      Debug_Name_For_Internal_Entity_List_Gen : aliased constant String :=
        "ListGen.entity";
      Desc_For_Internal_Entity_List_Gen : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_List_Gen'Access);
      
      Debug_Name_For_Internal_Entity_List_Lit : aliased constant String :=
        "ListLit.entity";
      Desc_For_Internal_Entity_List_Lit : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_List_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Name : aliased constant String :=
        "Name.entity";
      Desc_For_Internal_Entity_Name : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Name'Access);
      
      Debug_Name_For_Internal_Entity_Dotted_Name : aliased constant String :=
        "DottedName.entity";
      Desc_For_Internal_Entity_Dotted_Name : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Dotted_Name'Access);
      
      Debug_Name_For_Internal_Entity_Id : aliased constant String :=
        "Id.entity";
      Desc_For_Internal_Entity_Id : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Id'Access);
      
      Debug_Name_For_Internal_Entity_Not_Op : aliased constant String :=
        "NotOp.entity";
      Desc_For_Internal_Entity_Not_Op : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Not_Op'Access);
      
      Debug_Name_For_Internal_Entity_Number_Lit : aliased constant String :=
        "NumberLit.entity";
      Desc_For_Internal_Entity_Number_Lit : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Number_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Or_Expr : aliased constant String :=
        "OrExpr.entity";
      Desc_For_Internal_Entity_Or_Expr : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Or_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Or_Op : aliased constant String :=
        "OrOp.entity";
      Desc_For_Internal_Entity_Or_Op : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Or_Op'Access);
      
      Debug_Name_For_Internal_Entity_Power : aliased constant String :=
        "Power.entity";
      Desc_For_Internal_Entity_Power : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Power'Access);
      
      Debug_Name_For_Internal_Entity_Set_Comp : aliased constant String :=
        "SetComp.entity";
      Desc_For_Internal_Entity_Set_Comp : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Set_Comp'Access);
      
      Debug_Name_For_Internal_Entity_Set_Lit : aliased constant String :=
        "SetLit.entity";
      Desc_For_Internal_Entity_Set_Lit : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Set_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Slice_Expr : aliased constant String :=
        "SliceExpr.entity";
      Desc_For_Internal_Entity_Slice_Expr : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Slice_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Ext_Slice_Expr : aliased constant String :=
        "ExtSliceExpr.entity";
      Desc_For_Internal_Entity_Ext_Slice_Expr : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Ext_Slice_Expr'Access);
      
      Debug_Name_For_Internal_Entity_String_Lit : aliased constant String :=
        "StringLit.entity";
      Desc_For_Internal_Entity_String_Lit : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_String_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Subscript_Expr : aliased constant String :=
        "SubscriptExpr.entity";
      Desc_For_Internal_Entity_Subscript_Expr : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Subscript_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Tuple_Lit : aliased constant String :=
        "TupleLit.entity";
      Desc_For_Internal_Entity_Tuple_Lit : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Tuple_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Xor_Expr : aliased constant String :=
        "XorExpr.entity";
      Desc_For_Internal_Entity_Xor_Expr : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Xor_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Yield_Expr : aliased constant String :=
        "YieldExpr.entity";
      Desc_For_Internal_Entity_Yield_Expr : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Yield_Expr'Access);
      
      Debug_Name_For_Internal_Entity_File_Node : aliased constant String :=
        "FileNode.entity";
      Desc_For_Internal_Entity_File_Node : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_File_Node'Access);
      
      Debug_Name_For_Internal_Entity_Import_Star : aliased constant String :=
        "ImportStar.entity";
      Desc_For_Internal_Entity_Import_Star : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Import_Star'Access);
      
      Debug_Name_For_Internal_Entity_Kw_Args_Flag : aliased constant String :=
        "KwArgsFlag.entity";
      Desc_For_Internal_Entity_Kw_Args_Flag : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Kw_Args_Flag'Access);
      
      Debug_Name_For_Internal_Entity_Kw_Args_Flag_Absent : aliased constant String :=
        "KwArgsFlag.Absent.entity";
      Desc_For_Internal_Entity_Kw_Args_Flag_Absent : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Kw_Args_Flag_Absent'Access);
      
      Debug_Name_For_Internal_Entity_Kw_Args_Flag_Present : aliased constant String :=
        "KwArgsFlag.Present.entity";
      Desc_For_Internal_Entity_Kw_Args_Flag_Present : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Kw_Args_Flag_Present'Access);
      
      Debug_Name_For_Internal_Entity_NL : aliased constant String :=
        "NL.entity";
      Desc_For_Internal_Entity_NL : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_NL'Access);
      
      Debug_Name_For_Internal_Entity_Op : aliased constant String :=
        "Op.entity";
      Desc_For_Internal_Entity_Op : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Op'Access);
      
      Debug_Name_For_Internal_Entity_Params : aliased constant String :=
        "Params.entity";
      Desc_For_Internal_Entity_Params : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Params'Access);
      
      Debug_Name_For_Internal_Entity_Rel_Name : aliased constant String :=
        "RelName.entity";
      Desc_For_Internal_Entity_Rel_Name : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Rel_Name'Access);
      
      Debug_Name_For_Internal_Entity_Single_Param : aliased constant String :=
        "SingleParam.entity";
      Desc_For_Internal_Entity_Single_Param : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Single_Param'Access);
      
      Debug_Name_For_Internal_Entity_Stmt : aliased constant String :=
        "Stmt.entity";
      Desc_For_Internal_Entity_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Assert_Stmt : aliased constant String :=
        "AssertStmt.entity";
      Desc_For_Internal_Entity_Assert_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Assert_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Assign_Stmt : aliased constant String :=
        "AssignStmt.entity";
      Desc_For_Internal_Entity_Assign_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Assign_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Aug_Assign_Stmt : aliased constant String :=
        "AugAssignStmt.entity";
      Desc_For_Internal_Entity_Aug_Assign_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Aug_Assign_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Break_Stmt : aliased constant String :=
        "BreakStmt.entity";
      Desc_For_Internal_Entity_Break_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Break_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Continue_Stmt : aliased constant String :=
        "ContinueStmt.entity";
      Desc_For_Internal_Entity_Continue_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Continue_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Decorated : aliased constant String :=
        "Decorated.entity";
      Desc_For_Internal_Entity_Decorated : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Decorated'Access);
      
      Debug_Name_For_Internal_Entity_Def_Stmt : aliased constant String :=
        "DefStmt.entity";
      Desc_For_Internal_Entity_Def_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Def_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Class_Def : aliased constant String :=
        "ClassDef.entity";
      Desc_For_Internal_Entity_Class_Def : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Class_Def'Access);
      
      Debug_Name_For_Internal_Entity_Func_Def : aliased constant String :=
        "FuncDef.entity";
      Desc_For_Internal_Entity_Func_Def : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Func_Def'Access);
      
      Debug_Name_For_Internal_Entity_Del_Stmt : aliased constant String :=
        "DelStmt.entity";
      Desc_For_Internal_Entity_Del_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Del_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Elif_Branch : aliased constant String :=
        "ElifBranch.entity";
      Desc_For_Internal_Entity_Elif_Branch : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Elif_Branch'Access);
      
      Debug_Name_For_Internal_Entity_Exec_Stmt : aliased constant String :=
        "ExecStmt.entity";
      Desc_For_Internal_Entity_Exec_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Exec_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_For_Stmt : aliased constant String :=
        "ForStmt.entity";
      Desc_For_Internal_Entity_For_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_For_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Global_Stmt : aliased constant String :=
        "GlobalStmt.entity";
      Desc_For_Internal_Entity_Global_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Global_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_If_Stmt : aliased constant String :=
        "IfStmt.entity";
      Desc_For_Internal_Entity_If_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_If_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Import_From : aliased constant String :=
        "ImportFrom.entity";
      Desc_For_Internal_Entity_Import_From : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Import_From'Access);
      
      Debug_Name_For_Internal_Entity_Import_Name : aliased constant String :=
        "ImportName.entity";
      Desc_For_Internal_Entity_Import_Name : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Import_Name'Access);
      
      Debug_Name_For_Internal_Entity_Pass_Stmt : aliased constant String :=
        "PassStmt.entity";
      Desc_For_Internal_Entity_Pass_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Pass_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Print_Stmt : aliased constant String :=
        "PrintStmt.entity";
      Desc_For_Internal_Entity_Print_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Print_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Raise_Stmt : aliased constant String :=
        "RaiseStmt.entity";
      Desc_For_Internal_Entity_Raise_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Raise_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Return_Stmt : aliased constant String :=
        "ReturnStmt.entity";
      Desc_For_Internal_Entity_Return_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Return_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Stream_Print_Stmt : aliased constant String :=
        "StreamPrintStmt.entity";
      Desc_For_Internal_Entity_Stream_Print_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Stream_Print_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Try_Stmt : aliased constant String :=
        "TryStmt.entity";
      Desc_For_Internal_Entity_Try_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Try_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_While_Stmt : aliased constant String :=
        "WhileStmt.entity";
      Desc_For_Internal_Entity_While_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_While_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_With_Stmt : aliased constant String :=
        "WithStmt.entity";
      Desc_For_Internal_Entity_With_Stmt : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_With_Stmt'Access);
      
      Debug_Name_For_Internal_Entity_Turkixir_Node_Base_List : aliased constant String :=
        "TurkixirNodeBaseList.entity";
      Desc_For_Internal_Entity_Turkixir_Node_Base_List : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Turkixir_Node_Base_List'Access);
      
      Debug_Name_For_Internal_Entity_Arg_List : aliased constant String :=
        "Arg.list.entity";
      Desc_For_Internal_Entity_Arg_List : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Arg_List'Access);
      
      Debug_Name_For_Internal_Entity_As_Name_Node_List : aliased constant String :=
        "AsNameNode.list.entity";
      Desc_For_Internal_Entity_As_Name_Node_List : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_As_Name_Node_List'Access);
      
      Debug_Name_For_Internal_Entity_Decorator_List : aliased constant String :=
        "Decorator.list.entity";
      Desc_For_Internal_Entity_Decorator_List : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Decorator_List'Access);
      
      Debug_Name_For_Internal_Entity_Dict_Assoc_List : aliased constant String :=
        "DictAssoc.list.entity";
      Desc_For_Internal_Entity_Dict_Assoc_List : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Dict_Assoc_List'Access);
      
      Debug_Name_For_Internal_Entity_Dot_List : aliased constant String :=
        "Dot.list.entity";
      Desc_For_Internal_Entity_Dot_List : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Dot_List'Access);
      
      Debug_Name_For_Internal_Entity_Elif_Branch_List : aliased constant String :=
        "ElifBranch.list.entity";
      Desc_For_Internal_Entity_Elif_Branch_List : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Elif_Branch_List'Access);
      
      Debug_Name_For_Internal_Entity_Except_Part_List : aliased constant String :=
        "ExceptPart.list.entity";
      Desc_For_Internal_Entity_Except_Part_List : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Except_Part_List'Access);
      
      Debug_Name_For_Internal_Entity_Expr_List : aliased constant String :=
        "Expr.list.entity";
      Desc_For_Internal_Entity_Expr_List : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Expr_List'Access);
      
      Debug_Name_For_Internal_Entity_Id_List : aliased constant String :=
        "Id.list.entity";
      Desc_For_Internal_Entity_Id_List : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Id_List'Access);
      
      Debug_Name_For_Internal_Entity_NL_List : aliased constant String :=
        "NL.list.entity";
      Desc_For_Internal_Entity_NL_List : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_NL_List'Access);
      
      Debug_Name_For_Internal_Entity_Single_Param_List : aliased constant String :=
        "SingleParam.list.entity";
      Desc_For_Internal_Entity_Single_Param_List : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Single_Param_List'Access);
      
      Debug_Name_For_Internal_Entity_String_Lit_List : aliased constant String :=
        "StringLit.list.entity";
      Desc_For_Internal_Entity_String_Lit_List : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_String_Lit_List'Access);
      
      Debug_Name_For_Internal_Entity_Turkixir_Node_List : aliased constant String :=
        "TurkixirNode.list.entity";
      Desc_For_Internal_Entity_Turkixir_Node_List : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Turkixir_Node_List'Access);
      
      Debug_Name_For_Internal_Entity_Var_Args_Flag : aliased constant String :=
        "VarArgsFlag.entity";
      Desc_For_Internal_Entity_Var_Args_Flag : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Var_Args_Flag'Access);
      
      Debug_Name_For_Internal_Entity_Var_Args_Flag_Absent : aliased constant String :=
        "VarArgsFlag.Absent.entity";
      Desc_For_Internal_Entity_Var_Args_Flag_Absent : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Var_Args_Flag_Absent'Access);
      
      Debug_Name_For_Internal_Entity_Var_Args_Flag_Present : aliased constant String :=
        "VarArgsFlag.Present.entity";
      Desc_For_Internal_Entity_Var_Args_Flag_Present : aliased constant Type_Descriptor :=
        (Debug_Name => Debug_Name_For_Internal_Entity_Var_Args_Flag_Present'Access);

   Types : aliased constant Type_Descriptor_Array := (
      Desc_For_Internal_Unit'Access,
Desc_For_Big_Integer_Type'Access,
Desc_For_Boolean'Access,
Desc_For_Character_Type'Access,
Desc_For_Env_Rebindings'Access,
Desc_For_Integer'Access,
Desc_For_Source_Location_Range'Access,
Desc_For_String_Type'Access,
Desc_For_Token_Reference'Access,
Desc_For_Symbol_Type'Access,
Desc_For_Analysis_Unit_Kind'Access,
Desc_For_Lookup_Kind'Access,
Desc_For_Designated_Env_Kind'Access,
Desc_For_Grammar_Rule'Access,
Desc_For_Internal_Entity_Array_Access'Access,
Desc_For_Internal_Entity'Access,
Desc_For_Internal_Entity_Arg'Access,
Desc_For_Internal_Entity_Arg_Assoc'Access,
Desc_For_Internal_Entity_Arg_Gen'Access,
Desc_For_Internal_Entity_Kw_Args'Access,
Desc_For_Internal_Entity_Var_Args'Access,
Desc_For_Internal_Entity_As_Name_Node'Access,
Desc_For_Internal_Entity_Comp_If'Access,
Desc_For_Internal_Entity_Comp_Op_Kind'Access,
Desc_For_Internal_Entity_Comp_Op_Kind_Diamond'Access,
Desc_For_Internal_Entity_Comp_Op_Kind_Eq'Access,
Desc_For_Internal_Entity_Comp_Op_Kind_Gt'Access,
Desc_For_Internal_Entity_Comp_Op_Kind_Gte'Access,
Desc_For_Internal_Entity_Comp_Op_Kind_In'Access,
Desc_For_Internal_Entity_Comp_Op_Kind_Is'Access,
Desc_For_Internal_Entity_Comp_Op_Kind_Isnot'Access,
Desc_For_Internal_Entity_Comp_Op_Kind_Lt'Access,
Desc_For_Internal_Entity_Comp_Op_Kind_Lte'Access,
Desc_For_Internal_Entity_Comp_Op_Kind_Noteq'Access,
Desc_For_Internal_Entity_Comp_Op_Kind_Notin'Access,
Desc_For_Internal_Entity_Comprehension'Access,
Desc_For_Internal_Entity_Comp_For'Access,
Desc_For_Internal_Entity_Comp_ForL'Access,
Desc_For_Internal_Entity_Decorator'Access,
Desc_For_Internal_Entity_Dict_Assoc'Access,
Desc_For_Internal_Entity_Else_Part'Access,
Desc_For_Internal_Entity_Except_Part'Access,
Desc_For_Internal_Entity_Expr'Access,
Desc_For_Internal_Entity_And_Expr'Access,
Desc_For_Internal_Entity_And_Op'Access,
Desc_For_Internal_Entity_Bin_Op'Access,
Desc_For_Internal_Entity_Arith_Expr'Access,
Desc_For_Internal_Entity_Shift_Expr'Access,
Desc_For_Internal_Entity_Term'Access,
Desc_For_Internal_Entity_Call_Expr'Access,
Desc_For_Internal_Entity_Comp_Op'Access,
Desc_For_Internal_Entity_Concat_String_Lit'Access,
Desc_For_Internal_Entity_Dict_Comp'Access,
Desc_For_Internal_Entity_Dict_Lit'Access,
Desc_For_Internal_Entity_Dot'Access,
Desc_For_Internal_Entity_Ellipsis_Expr'Access,
Desc_For_Internal_Entity_Factor'Access,
Desc_For_Internal_Entity_If_Expr'Access,
Desc_For_Internal_Entity_Inline_Eval'Access,
Desc_For_Internal_Entity_Lambda_Def'Access,
Desc_For_Internal_Entity_List_Comp'Access,
Desc_For_Internal_Entity_List_Gen'Access,
Desc_For_Internal_Entity_List_Lit'Access,
Desc_For_Internal_Entity_Name'Access,
Desc_For_Internal_Entity_Dotted_Name'Access,
Desc_For_Internal_Entity_Id'Access,
Desc_For_Internal_Entity_Not_Op'Access,
Desc_For_Internal_Entity_Number_Lit'Access,
Desc_For_Internal_Entity_Or_Expr'Access,
Desc_For_Internal_Entity_Or_Op'Access,
Desc_For_Internal_Entity_Power'Access,
Desc_For_Internal_Entity_Set_Comp'Access,
Desc_For_Internal_Entity_Set_Lit'Access,
Desc_For_Internal_Entity_Slice_Expr'Access,
Desc_For_Internal_Entity_Ext_Slice_Expr'Access,
Desc_For_Internal_Entity_String_Lit'Access,
Desc_For_Internal_Entity_Subscript_Expr'Access,
Desc_For_Internal_Entity_Tuple_Lit'Access,
Desc_For_Internal_Entity_Xor_Expr'Access,
Desc_For_Internal_Entity_Yield_Expr'Access,
Desc_For_Internal_Entity_File_Node'Access,
Desc_For_Internal_Entity_Import_Star'Access,
Desc_For_Internal_Entity_Kw_Args_Flag'Access,
Desc_For_Internal_Entity_Kw_Args_Flag_Absent'Access,
Desc_For_Internal_Entity_Kw_Args_Flag_Present'Access,
Desc_For_Internal_Entity_NL'Access,
Desc_For_Internal_Entity_Op'Access,
Desc_For_Internal_Entity_Params'Access,
Desc_For_Internal_Entity_Rel_Name'Access,
Desc_For_Internal_Entity_Single_Param'Access,
Desc_For_Internal_Entity_Stmt'Access,
Desc_For_Internal_Entity_Assert_Stmt'Access,
Desc_For_Internal_Entity_Assign_Stmt'Access,
Desc_For_Internal_Entity_Aug_Assign_Stmt'Access,
Desc_For_Internal_Entity_Break_Stmt'Access,
Desc_For_Internal_Entity_Continue_Stmt'Access,
Desc_For_Internal_Entity_Decorated'Access,
Desc_For_Internal_Entity_Def_Stmt'Access,
Desc_For_Internal_Entity_Class_Def'Access,
Desc_For_Internal_Entity_Func_Def'Access,
Desc_For_Internal_Entity_Del_Stmt'Access,
Desc_For_Internal_Entity_Elif_Branch'Access,
Desc_For_Internal_Entity_Exec_Stmt'Access,
Desc_For_Internal_Entity_For_Stmt'Access,
Desc_For_Internal_Entity_Global_Stmt'Access,
Desc_For_Internal_Entity_If_Stmt'Access,
Desc_For_Internal_Entity_Import_From'Access,
Desc_For_Internal_Entity_Import_Name'Access,
Desc_For_Internal_Entity_Pass_Stmt'Access,
Desc_For_Internal_Entity_Print_Stmt'Access,
Desc_For_Internal_Entity_Raise_Stmt'Access,
Desc_For_Internal_Entity_Return_Stmt'Access,
Desc_For_Internal_Entity_Stream_Print_Stmt'Access,
Desc_For_Internal_Entity_Try_Stmt'Access,
Desc_For_Internal_Entity_While_Stmt'Access,
Desc_For_Internal_Entity_With_Stmt'Access,
Desc_For_Internal_Entity_Turkixir_Node_Base_List'Access,
Desc_For_Internal_Entity_Arg_List'Access,
Desc_For_Internal_Entity_As_Name_Node_List'Access,
Desc_For_Internal_Entity_Decorator_List'Access,
Desc_For_Internal_Entity_Dict_Assoc_List'Access,
Desc_For_Internal_Entity_Dot_List'Access,
Desc_For_Internal_Entity_Elif_Branch_List'Access,
Desc_For_Internal_Entity_Except_Part_List'Access,
Desc_For_Internal_Entity_Expr_List'Access,
Desc_For_Internal_Entity_Id_List'Access,
Desc_For_Internal_Entity_NL_List'Access,
Desc_For_Internal_Entity_Single_Param_List'Access,
Desc_For_Internal_Entity_String_Lit_List'Access,
Desc_For_Internal_Entity_Turkixir_Node_List'Access,
Desc_For_Internal_Entity_Var_Args_Flag'Access,
Desc_For_Internal_Entity_Var_Args_Flag_Absent'Access,
Desc_For_Internal_Entity_Var_Args_Flag_Present'Access
   );

   ---------------------------
   -- Enum type descriptors --
   ---------------------------

   
      

         Enum_Name_For_Analysis_Unit_Kind_1 : aliased constant Text_Type :=
           "Unit_Specification";
         Enum_Name_For_Analysis_Unit_Kind_2 : aliased constant Text_Type :=
           "Unit_Body";

      Enum_Name_For_Analysis_Unit_Kind : aliased constant Text_Type :=
        "Analysis_Unit_Kind";
      Enum_Desc_For_Analysis_Unit_Kind : aliased constant Enum_Type_Descriptor := (
         Last_Value    => 2,
         Name          => Enum_Name_For_Analysis_Unit_Kind'Access,
         Default_Value => 0,
         Value_Names   => (
            1 => Enum_Name_For_Analysis_Unit_Kind_1'Access,
2 => Enum_Name_For_Analysis_Unit_Kind_2'Access
         )
      );
      

         Enum_Name_For_Lookup_Kind_1 : aliased constant Text_Type :=
           "Recursive";
         Enum_Name_For_Lookup_Kind_2 : aliased constant Text_Type :=
           "Flat";
         Enum_Name_For_Lookup_Kind_3 : aliased constant Text_Type :=
           "Minimal";

      Enum_Name_For_Lookup_Kind : aliased constant Text_Type :=
        "Lookup_Kind";
      Enum_Desc_For_Lookup_Kind : aliased constant Enum_Type_Descriptor := (
         Last_Value    => 3,
         Name          => Enum_Name_For_Lookup_Kind'Access,
         Default_Value => 0,
         Value_Names   => (
            1 => Enum_Name_For_Lookup_Kind_1'Access,
2 => Enum_Name_For_Lookup_Kind_2'Access,
3 => Enum_Name_For_Lookup_Kind_3'Access
         )
      );
      

         Enum_Name_For_Designated_Env_Kind_1 : aliased constant Text_Type :=
           "None";
         Enum_Name_For_Designated_Env_Kind_2 : aliased constant Text_Type :=
           "Current_Env";
         Enum_Name_For_Designated_Env_Kind_3 : aliased constant Text_Type :=
           "Named_Env";
         Enum_Name_For_Designated_Env_Kind_4 : aliased constant Text_Type :=
           "Direct_Env";

      Enum_Name_For_Designated_Env_Kind : aliased constant Text_Type :=
        "Designated_Env_Kind";
      Enum_Desc_For_Designated_Env_Kind : aliased constant Enum_Type_Descriptor := (
         Last_Value    => 4,
         Name          => Enum_Name_For_Designated_Env_Kind'Access,
         Default_Value => 1,
         Value_Names   => (
            1 => Enum_Name_For_Designated_Env_Kind_1'Access,
2 => Enum_Name_For_Designated_Env_Kind_2'Access,
3 => Enum_Name_For_Designated_Env_Kind_3'Access,
4 => Enum_Name_For_Designated_Env_Kind_4'Access
         )
      );
      

         Enum_Name_For_Grammar_Rule_1 : aliased constant Text_Type :=
           "Name_Rule";
         Enum_Name_For_Grammar_Rule_2 : aliased constant Text_Type :=
           "Number_Rule";
         Enum_Name_For_Grammar_Rule_3 : aliased constant Text_Type :=
           "String_Rule";
         Enum_Name_For_Grammar_Rule_4 : aliased constant Text_Type :=
           "Cat_String_Rule";
         Enum_Name_For_Grammar_Rule_5 : aliased constant Text_Type :=
           "Nl_Rule";
         Enum_Name_For_Grammar_Rule_6 : aliased constant Text_Type :=
           "Main_Rule_Rule";
         Enum_Name_For_Grammar_Rule_7 : aliased constant Text_Type :=
           "Decorator_Rule";
         Enum_Name_For_Grammar_Rule_8 : aliased constant Text_Type :=
           "Decorators_Rule";
         Enum_Name_For_Grammar_Rule_9 : aliased constant Text_Type :=
           "Decorated_Rule";
         Enum_Name_For_Grammar_Rule_10 : aliased constant Text_Type :=
           "Func_Def_Rule";
         Enum_Name_For_Grammar_Rule_11 : aliased constant Text_Type :=
           "Parameters_Rule";
         Enum_Name_For_Grammar_Rule_12 : aliased constant Text_Type :=
           "Varargslist_Rule";
         Enum_Name_For_Grammar_Rule_13 : aliased constant Text_Type :=
           "Fpdef_Rule";
         Enum_Name_For_Grammar_Rule_14 : aliased constant Text_Type :=
           "Name_List_Rule";
         Enum_Name_For_Grammar_Rule_15 : aliased constant Text_Type :=
           "Stmt_Rule";
         Enum_Name_For_Grammar_Rule_16 : aliased constant Text_Type :=
           "Simple_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_17 : aliased constant Text_Type :=
           "Small_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_18 : aliased constant Text_Type :=
           "Expr_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_19 : aliased constant Text_Type :=
           "Print_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_20 : aliased constant Text_Type :=
           "Del_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_21 : aliased constant Text_Type :=
           "Pass_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_22 : aliased constant Text_Type :=
           "Flow_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_23 : aliased constant Text_Type :=
           "Break_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_24 : aliased constant Text_Type :=
           "Continue_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_25 : aliased constant Text_Type :=
           "Return_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_26 : aliased constant Text_Type :=
           "Yield_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_27 : aliased constant Text_Type :=
           "Raise_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_28 : aliased constant Text_Type :=
           "Import_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_29 : aliased constant Text_Type :=
           "Import_Name_Rule";
         Enum_Name_For_Grammar_Rule_30 : aliased constant Text_Type :=
           "Dot_Rule";
         Enum_Name_For_Grammar_Rule_31 : aliased constant Text_Type :=
           "Import_From_Rule";
         Enum_Name_For_Grammar_Rule_32 : aliased constant Text_Type :=
           "As_Name_Rule";
         Enum_Name_For_Grammar_Rule_33 : aliased constant Text_Type :=
           "Dotted_As_Name_Rule";
         Enum_Name_For_Grammar_Rule_34 : aliased constant Text_Type :=
           "Import_As_Names_Rule";
         Enum_Name_For_Grammar_Rule_35 : aliased constant Text_Type :=
           "Dotted_As_Names_Rule";
         Enum_Name_For_Grammar_Rule_36 : aliased constant Text_Type :=
           "Dotted_Name_Rule";
         Enum_Name_For_Grammar_Rule_37 : aliased constant Text_Type :=
           "Global_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_38 : aliased constant Text_Type :=
           "Exec_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_39 : aliased constant Text_Type :=
           "Assert_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_40 : aliased constant Text_Type :=
           "Compound_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_41 : aliased constant Text_Type :=
           "Else_Part_Rule";
         Enum_Name_For_Grammar_Rule_42 : aliased constant Text_Type :=
           "If_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_43 : aliased constant Text_Type :=
           "While_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_44 : aliased constant Text_Type :=
           "For_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_45 : aliased constant Text_Type :=
           "Try_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_46 : aliased constant Text_Type :=
           "With_Stmt_Rule";
         Enum_Name_For_Grammar_Rule_47 : aliased constant Text_Type :=
           "With_Item_Rule";
         Enum_Name_For_Grammar_Rule_48 : aliased constant Text_Type :=
           "Suite_Rule";
         Enum_Name_For_Grammar_Rule_49 : aliased constant Text_Type :=
           "Test_Rule";
         Enum_Name_For_Grammar_Rule_50 : aliased constant Text_Type :=
           "Or_Test_Rule";
         Enum_Name_For_Grammar_Rule_51 : aliased constant Text_Type :=
           "And_Test_Rule";
         Enum_Name_For_Grammar_Rule_52 : aliased constant Text_Type :=
           "Not_Test_Rule";
         Enum_Name_For_Grammar_Rule_53 : aliased constant Text_Type :=
           "Comparison_Rule";
         Enum_Name_For_Grammar_Rule_54 : aliased constant Text_Type :=
           "Expr_Rule";
         Enum_Name_For_Grammar_Rule_55 : aliased constant Text_Type :=
           "Xor_Expr_Rule";
         Enum_Name_For_Grammar_Rule_56 : aliased constant Text_Type :=
           "And_Expr_Rule";
         Enum_Name_For_Grammar_Rule_57 : aliased constant Text_Type :=
           "Shift_Expr_Rule";
         Enum_Name_For_Grammar_Rule_58 : aliased constant Text_Type :=
           "Arith_Expr_Rule";
         Enum_Name_For_Grammar_Rule_59 : aliased constant Text_Type :=
           "Term_Rule";
         Enum_Name_For_Grammar_Rule_60 : aliased constant Text_Type :=
           "Factor_Rule";
         Enum_Name_For_Grammar_Rule_61 : aliased constant Text_Type :=
           "Power_Rule";
         Enum_Name_For_Grammar_Rule_62 : aliased constant Text_Type :=
           "Atom_Expr_Rule";
         Enum_Name_For_Grammar_Rule_63 : aliased constant Text_Type :=
           "Dict_Assoc_Rule";
         Enum_Name_For_Grammar_Rule_64 : aliased constant Text_Type :=
           "Yield_Expr_Rule";
         Enum_Name_For_Grammar_Rule_65 : aliased constant Text_Type :=
           "Atom_Rule";
         Enum_Name_For_Grammar_Rule_66 : aliased constant Text_Type :=
           "Set_Lit_Rule";
         Enum_Name_For_Grammar_Rule_67 : aliased constant Text_Type :=
           "Lambdef_Rule";
         Enum_Name_For_Grammar_Rule_68 : aliased constant Text_Type :=
           "Subscript_List_Rule";
         Enum_Name_For_Grammar_Rule_69 : aliased constant Text_Type :=
           "Subscript_Rule";
         Enum_Name_For_Grammar_Rule_70 : aliased constant Text_Type :=
           "Expr_List_Rule";
         Enum_Name_For_Grammar_Rule_71 : aliased constant Text_Type :=
           "Test_List_Rule";
         Enum_Name_For_Grammar_Rule_72 : aliased constant Text_Type :=
           "Empty_Test_List_Rule";
         Enum_Name_For_Grammar_Rule_73 : aliased constant Text_Type :=
           "Class_Def_Rule";
         Enum_Name_For_Grammar_Rule_74 : aliased constant Text_Type :=
           "Arg_List_Rule";
         Enum_Name_For_Grammar_Rule_75 : aliased constant Text_Type :=
           "List_Iter_Rule";
         Enum_Name_For_Grammar_Rule_76 : aliased constant Text_Type :=
           "List_For_Rule";
         Enum_Name_For_Grammar_Rule_77 : aliased constant Text_Type :=
           "List_If_Rule";
         Enum_Name_For_Grammar_Rule_78 : aliased constant Text_Type :=
           "Comp_Iter_Rule";
         Enum_Name_For_Grammar_Rule_79 : aliased constant Text_Type :=
           "Comp_For_Rule";
         Enum_Name_For_Grammar_Rule_80 : aliased constant Text_Type :=
           "Comp_If_Rule";

      Enum_Name_For_Grammar_Rule : aliased constant Text_Type :=
        "Grammar_Rule";
      Enum_Desc_For_Grammar_Rule : aliased constant Enum_Type_Descriptor := (
         Last_Value    => 80,
         Name          => Enum_Name_For_Grammar_Rule'Access,
         Default_Value => 0,
         Value_Names   => (
            1 => Enum_Name_For_Grammar_Rule_1'Access,
2 => Enum_Name_For_Grammar_Rule_2'Access,
3 => Enum_Name_For_Grammar_Rule_3'Access,
4 => Enum_Name_For_Grammar_Rule_4'Access,
5 => Enum_Name_For_Grammar_Rule_5'Access,
6 => Enum_Name_For_Grammar_Rule_6'Access,
7 => Enum_Name_For_Grammar_Rule_7'Access,
8 => Enum_Name_For_Grammar_Rule_8'Access,
9 => Enum_Name_For_Grammar_Rule_9'Access,
10 => Enum_Name_For_Grammar_Rule_10'Access,
11 => Enum_Name_For_Grammar_Rule_11'Access,
12 => Enum_Name_For_Grammar_Rule_12'Access,
13 => Enum_Name_For_Grammar_Rule_13'Access,
14 => Enum_Name_For_Grammar_Rule_14'Access,
15 => Enum_Name_For_Grammar_Rule_15'Access,
16 => Enum_Name_For_Grammar_Rule_16'Access,
17 => Enum_Name_For_Grammar_Rule_17'Access,
18 => Enum_Name_For_Grammar_Rule_18'Access,
19 => Enum_Name_For_Grammar_Rule_19'Access,
20 => Enum_Name_For_Grammar_Rule_20'Access,
21 => Enum_Name_For_Grammar_Rule_21'Access,
22 => Enum_Name_For_Grammar_Rule_22'Access,
23 => Enum_Name_For_Grammar_Rule_23'Access,
24 => Enum_Name_For_Grammar_Rule_24'Access,
25 => Enum_Name_For_Grammar_Rule_25'Access,
26 => Enum_Name_For_Grammar_Rule_26'Access,
27 => Enum_Name_For_Grammar_Rule_27'Access,
28 => Enum_Name_For_Grammar_Rule_28'Access,
29 => Enum_Name_For_Grammar_Rule_29'Access,
30 => Enum_Name_For_Grammar_Rule_30'Access,
31 => Enum_Name_For_Grammar_Rule_31'Access,
32 => Enum_Name_For_Grammar_Rule_32'Access,
33 => Enum_Name_For_Grammar_Rule_33'Access,
34 => Enum_Name_For_Grammar_Rule_34'Access,
35 => Enum_Name_For_Grammar_Rule_35'Access,
36 => Enum_Name_For_Grammar_Rule_36'Access,
37 => Enum_Name_For_Grammar_Rule_37'Access,
38 => Enum_Name_For_Grammar_Rule_38'Access,
39 => Enum_Name_For_Grammar_Rule_39'Access,
40 => Enum_Name_For_Grammar_Rule_40'Access,
41 => Enum_Name_For_Grammar_Rule_41'Access,
42 => Enum_Name_For_Grammar_Rule_42'Access,
43 => Enum_Name_For_Grammar_Rule_43'Access,
44 => Enum_Name_For_Grammar_Rule_44'Access,
45 => Enum_Name_For_Grammar_Rule_45'Access,
46 => Enum_Name_For_Grammar_Rule_46'Access,
47 => Enum_Name_For_Grammar_Rule_47'Access,
48 => Enum_Name_For_Grammar_Rule_48'Access,
49 => Enum_Name_For_Grammar_Rule_49'Access,
50 => Enum_Name_For_Grammar_Rule_50'Access,
51 => Enum_Name_For_Grammar_Rule_51'Access,
52 => Enum_Name_For_Grammar_Rule_52'Access,
53 => Enum_Name_For_Grammar_Rule_53'Access,
54 => Enum_Name_For_Grammar_Rule_54'Access,
55 => Enum_Name_For_Grammar_Rule_55'Access,
56 => Enum_Name_For_Grammar_Rule_56'Access,
57 => Enum_Name_For_Grammar_Rule_57'Access,
58 => Enum_Name_For_Grammar_Rule_58'Access,
59 => Enum_Name_For_Grammar_Rule_59'Access,
60 => Enum_Name_For_Grammar_Rule_60'Access,
61 => Enum_Name_For_Grammar_Rule_61'Access,
62 => Enum_Name_For_Grammar_Rule_62'Access,
63 => Enum_Name_For_Grammar_Rule_63'Access,
64 => Enum_Name_For_Grammar_Rule_64'Access,
65 => Enum_Name_For_Grammar_Rule_65'Access,
66 => Enum_Name_For_Grammar_Rule_66'Access,
67 => Enum_Name_For_Grammar_Rule_67'Access,
68 => Enum_Name_For_Grammar_Rule_68'Access,
69 => Enum_Name_For_Grammar_Rule_69'Access,
70 => Enum_Name_For_Grammar_Rule_70'Access,
71 => Enum_Name_For_Grammar_Rule_71'Access,
72 => Enum_Name_For_Grammar_Rule_72'Access,
73 => Enum_Name_For_Grammar_Rule_73'Access,
74 => Enum_Name_For_Grammar_Rule_74'Access,
75 => Enum_Name_For_Grammar_Rule_75'Access,
76 => Enum_Name_For_Grammar_Rule_76'Access,
77 => Enum_Name_For_Grammar_Rule_77'Access,
78 => Enum_Name_For_Grammar_Rule_78'Access,
79 => Enum_Name_For_Grammar_Rule_79'Access,
80 => Enum_Name_For_Grammar_Rule_80'Access
         )
      );
   Enum_Types : aliased constant Enum_Type_Descriptor_Array := (
      Type_Index_For_Analysis_Unit_Kind => Enum_Desc_For_Analysis_Unit_Kind'Access,
Type_Index_For_Lookup_Kind => Enum_Desc_For_Lookup_Kind'Access,
Type_Index_For_Designated_Env_Kind => Enum_Desc_For_Designated_Env_Kind'Access,
Type_Index_For_Grammar_Rule => Enum_Desc_For_Grammar_Rule'Access
   );

   ----------------------------
   -- Array type descriptors --
   ----------------------------

   
   Array_Types : aliased constant Array_Type_Descriptor_Array := (
      Type_Index_For_Turkixir_Node_Array => (Element_Type => Type_Index_For_Turkixir_Node)
   );

   -------------------------------
   -- Struct member descriptors --
   -------------------------------

   
      


      Member_Name_For_Arg_Assoc_F_Name : aliased constant Text_Type :=
        "Arg_Assoc_F_Name";
      Member_Desc_For_Arg_Assoc_F_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Arg_Assoc_F_Name'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Arg_Assoc_F_Expr : aliased constant Text_Type :=
        "Arg_Assoc_F_Expr";
      Member_Desc_For_Arg_Assoc_F_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Arg_Assoc_F_Expr'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Arg_Gen_F_Expr : aliased constant Text_Type :=
        "Arg_Gen_F_Expr";
      Member_Desc_For_Arg_Gen_F_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Arg_Gen_F_Expr'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Arg_Gen_F_Comprehension : aliased constant Text_Type :=
        "Arg_Gen_F_Comprehension";
      Member_Desc_For_Arg_Gen_F_Comprehension : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Arg_Gen_F_Comprehension'Access,
         Member_Type   => Type_Index_For_Comp_For,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Kw_Args_F_Expr : aliased constant Text_Type :=
        "Kw_Args_F_Expr";
      Member_Desc_For_Kw_Args_F_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Kw_Args_F_Expr'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Var_Args_F_Expr : aliased constant Text_Type :=
        "Var_Args_F_Expr";
      Member_Desc_For_Var_Args_F_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Var_Args_F_Expr'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_As_Name_Node_F_Imported : aliased constant Text_Type :=
        "As_Name_Node_F_Imported";
      Member_Desc_For_As_Name_Node_F_Imported : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_As_Name_Node_F_Imported'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_As_Name_Node_F_As_Name : aliased constant Text_Type :=
        "As_Name_Node_F_As_Name";
      Member_Desc_For_As_Name_Node_F_As_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_As_Name_Node_F_As_Name'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Comp_If_F_Test : aliased constant Text_Type :=
        "Comp_If_F_Test";
      Member_Desc_For_Comp_If_F_Test : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Comp_If_F_Test'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Comp_If_F_Comp : aliased constant Text_Type :=
        "Comp_If_F_Comp";
      Member_Desc_For_Comp_If_F_Comp : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Comp_If_F_Comp'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Comp_For_F_Exprs : aliased constant Text_Type :=
        "Comp_For_F_Exprs";
      Member_Desc_For_Comp_For_F_Exprs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Comp_For_F_Exprs'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Comp_For_F_Target : aliased constant Text_Type :=
        "Comp_For_F_Target";
      Member_Desc_For_Comp_For_F_Target : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Comp_For_F_Target'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Comp_For_F_Comp : aliased constant Text_Type :=
        "Comp_For_F_Comp";
      Member_Desc_For_Comp_For_F_Comp : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Comp_For_F_Comp'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Comp_ForL_F_Exprs : aliased constant Text_Type :=
        "Comp_ForL_F_Exprs";
      Member_Desc_For_Comp_ForL_F_Exprs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Comp_ForL_F_Exprs'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Comp_ForL_F_Target : aliased constant Text_Type :=
        "Comp_ForL_F_Target";
      Member_Desc_For_Comp_ForL_F_Target : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Comp_ForL_F_Target'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Comp_ForL_F_Comp : aliased constant Text_Type :=
        "Comp_ForL_F_Comp";
      Member_Desc_For_Comp_ForL_F_Comp : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Comp_ForL_F_Comp'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Decorator_F_Dec_Name : aliased constant Text_Type :=
        "Decorator_F_Dec_Name";
      Member_Desc_For_Decorator_F_Dec_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decorator_F_Dec_Name'Access,
         Member_Type   => Type_Index_For_Name,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Decorator_F_Arg_List : aliased constant Text_Type :=
        "Decorator_F_Arg_List";
      Member_Desc_For_Decorator_F_Arg_List : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decorator_F_Arg_List'Access,
         Member_Type   => Type_Index_For_Arg_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Dict_Assoc_F_Key : aliased constant Text_Type :=
        "Dict_Assoc_F_Key";
      Member_Desc_For_Dict_Assoc_F_Key : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Dict_Assoc_F_Key'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Dict_Assoc_F_Value : aliased constant Text_Type :=
        "Dict_Assoc_F_Value";
      Member_Desc_For_Dict_Assoc_F_Value : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Dict_Assoc_F_Value'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Else_Part_F_Statements : aliased constant Text_Type :=
        "Else_Part_F_Statements";
      Member_Desc_For_Else_Part_F_Statements : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Else_Part_F_Statements'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Except_Part_F_As_Name : aliased constant Text_Type :=
        "Except_Part_F_As_Name";
      Member_Desc_For_Except_Part_F_As_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Except_Part_F_As_Name'Access,
         Member_Type   => Type_Index_For_As_Name_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Except_Part_F_Statements : aliased constant Text_Type :=
        "Except_Part_F_Statements";
      Member_Desc_For_Except_Part_F_Statements : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Except_Part_F_Statements'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_And_Expr_F_Left : aliased constant Text_Type :=
        "And_Expr_F_Left";
      Member_Desc_For_And_Expr_F_Left : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_And_Expr_F_Left'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_And_Expr_F_Right : aliased constant Text_Type :=
        "And_Expr_F_Right";
      Member_Desc_For_And_Expr_F_Right : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_And_Expr_F_Right'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_And_Op_F_Left : aliased constant Text_Type :=
        "And_Op_F_Left";
      Member_Desc_For_And_Op_F_Left : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_And_Op_F_Left'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_And_Op_F_Right : aliased constant Text_Type :=
        "And_Op_F_Right";
      Member_Desc_For_And_Op_F_Right : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_And_Op_F_Right'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Bin_Op_F_Left : aliased constant Text_Type :=
        "Bin_Op_F_Left";
      Member_Desc_For_Bin_Op_F_Left : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Bin_Op_F_Left'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Bin_Op_F_Op : aliased constant Text_Type :=
        "Bin_Op_F_Op";
      Member_Desc_For_Bin_Op_F_Op : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Bin_Op_F_Op'Access,
         Member_Type   => Type_Index_For_Op,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Bin_Op_F_Right : aliased constant Text_Type :=
        "Bin_Op_F_Right";
      Member_Desc_For_Bin_Op_F_Right : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Bin_Op_F_Right'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Call_Expr_F_Prefix : aliased constant Text_Type :=
        "Call_Expr_F_Prefix";
      Member_Desc_For_Call_Expr_F_Prefix : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Call_Expr_F_Prefix'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Call_Expr_F_Suffix : aliased constant Text_Type :=
        "Call_Expr_F_Suffix";
      Member_Desc_For_Call_Expr_F_Suffix : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Call_Expr_F_Suffix'Access,
         Member_Type   => Type_Index_For_Arg_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Comp_Op_F_Left : aliased constant Text_Type :=
        "Comp_Op_F_Left";
      Member_Desc_For_Comp_Op_F_Left : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Comp_Op_F_Left'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Comp_Op_F_Op : aliased constant Text_Type :=
        "Comp_Op_F_Op";
      Member_Desc_For_Comp_Op_F_Op : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Comp_Op_F_Op'Access,
         Member_Type   => Type_Index_For_Comp_Op_Kind,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Comp_Op_F_Right : aliased constant Text_Type :=
        "Comp_Op_F_Right";
      Member_Desc_For_Comp_Op_F_Right : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Comp_Op_F_Right'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Concat_String_Lit_F_First_Str : aliased constant Text_Type :=
        "Concat_String_Lit_F_First_Str";
      Member_Desc_For_Concat_String_Lit_F_First_Str : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Concat_String_Lit_F_First_Str'Access,
         Member_Type   => Type_Index_For_String_Lit,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Concat_String_Lit_F_Subsequent_Str : aliased constant Text_Type :=
        "Concat_String_Lit_F_Subsequent_Str";
      Member_Desc_For_Concat_String_Lit_F_Subsequent_Str : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Concat_String_Lit_F_Subsequent_Str'Access,
         Member_Type   => Type_Index_For_String_Lit_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Dict_Comp_F_Assoc : aliased constant Text_Type :=
        "Dict_Comp_F_Assoc";
      Member_Desc_For_Dict_Comp_F_Assoc : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Dict_Comp_F_Assoc'Access,
         Member_Type   => Type_Index_For_Dict_Assoc,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Dict_Comp_F_Comprehension : aliased constant Text_Type :=
        "Dict_Comp_F_Comprehension";
      Member_Desc_For_Dict_Comp_F_Comprehension : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Dict_Comp_F_Comprehension'Access,
         Member_Type   => Type_Index_For_Comp_For,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Dict_Lit_F_Assocs : aliased constant Text_Type :=
        "Dict_Lit_F_Assocs";
      Member_Desc_For_Dict_Lit_F_Assocs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Dict_Lit_F_Assocs'Access,
         Member_Type   => Type_Index_For_Dict_Assoc_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Factor_F_Op : aliased constant Text_Type :=
        "Factor_F_Op";
      Member_Desc_For_Factor_F_Op : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Factor_F_Op'Access,
         Member_Type   => Type_Index_For_Op,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Factor_F_Expr : aliased constant Text_Type :=
        "Factor_F_Expr";
      Member_Desc_For_Factor_F_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Factor_F_Expr'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_If_Expr_F_Expr : aliased constant Text_Type :=
        "If_Expr_F_Expr";
      Member_Desc_For_If_Expr_F_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_If_Expr_F_Expr'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_If_Expr_F_Cond : aliased constant Text_Type :=
        "If_Expr_F_Cond";
      Member_Desc_For_If_Expr_F_Cond : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_If_Expr_F_Cond'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_If_Expr_F_Else_Expr : aliased constant Text_Type :=
        "If_Expr_F_Else_Expr";
      Member_Desc_For_If_Expr_F_Else_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_If_Expr_F_Else_Expr'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Inline_Eval_F_Exprs : aliased constant Text_Type :=
        "Inline_Eval_F_Exprs";
      Member_Desc_For_Inline_Eval_F_Exprs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Inline_Eval_F_Exprs'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Lambda_Def_F_Args : aliased constant Text_Type :=
        "Lambda_Def_F_Args";
      Member_Desc_For_Lambda_Def_F_Args : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lambda_Def_F_Args'Access,
         Member_Type   => Type_Index_For_Params,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Lambda_Def_F_Expr : aliased constant Text_Type :=
        "Lambda_Def_F_Expr";
      Member_Desc_For_Lambda_Def_F_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lambda_Def_F_Expr'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_List_Comp_F_Expr : aliased constant Text_Type :=
        "List_Comp_F_Expr";
      Member_Desc_For_List_Comp_F_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_List_Comp_F_Expr'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_List_Comp_F_Comprehension : aliased constant Text_Type :=
        "List_Comp_F_Comprehension";
      Member_Desc_For_List_Comp_F_Comprehension : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_List_Comp_F_Comprehension'Access,
         Member_Type   => Type_Index_For_Comp_ForL,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_List_Gen_F_Expr : aliased constant Text_Type :=
        "List_Gen_F_Expr";
      Member_Desc_For_List_Gen_F_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_List_Gen_F_Expr'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_List_Gen_F_Comprehension : aliased constant Text_Type :=
        "List_Gen_F_Comprehension";
      Member_Desc_For_List_Gen_F_Comprehension : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_List_Gen_F_Comprehension'Access,
         Member_Type   => Type_Index_For_Comp_ForL,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_List_Lit_F_Exprs : aliased constant Text_Type :=
        "List_Lit_F_Exprs";
      Member_Desc_For_List_Lit_F_Exprs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_List_Lit_F_Exprs'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Dotted_Name_F_Prefix : aliased constant Text_Type :=
        "Dotted_Name_F_Prefix";
      Member_Desc_For_Dotted_Name_F_Prefix : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Dotted_Name_F_Prefix'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Dotted_Name_F_Suffix : aliased constant Text_Type :=
        "Dotted_Name_F_Suffix";
      Member_Desc_For_Dotted_Name_F_Suffix : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Dotted_Name_F_Suffix'Access,
         Member_Type   => Type_Index_For_Id,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Not_Op_F_Expr : aliased constant Text_Type :=
        "Not_Op_F_Expr";
      Member_Desc_For_Not_Op_F_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Not_Op_F_Expr'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Or_Expr_F_Left : aliased constant Text_Type :=
        "Or_Expr_F_Left";
      Member_Desc_For_Or_Expr_F_Left : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Or_Expr_F_Left'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Or_Expr_F_Right : aliased constant Text_Type :=
        "Or_Expr_F_Right";
      Member_Desc_For_Or_Expr_F_Right : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Or_Expr_F_Right'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Or_Op_F_Left : aliased constant Text_Type :=
        "Or_Op_F_Left";
      Member_Desc_For_Or_Op_F_Left : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Or_Op_F_Left'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Or_Op_F_Right : aliased constant Text_Type :=
        "Or_Op_F_Right";
      Member_Desc_For_Or_Op_F_Right : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Or_Op_F_Right'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Power_F_Left : aliased constant Text_Type :=
        "Power_F_Left";
      Member_Desc_For_Power_F_Left : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Power_F_Left'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Power_F_Right : aliased constant Text_Type :=
        "Power_F_Right";
      Member_Desc_For_Power_F_Right : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Power_F_Right'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Set_Comp_F_Expr : aliased constant Text_Type :=
        "Set_Comp_F_Expr";
      Member_Desc_For_Set_Comp_F_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Set_Comp_F_Expr'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Set_Comp_F_Comprehension : aliased constant Text_Type :=
        "Set_Comp_F_Comprehension";
      Member_Desc_For_Set_Comp_F_Comprehension : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Set_Comp_F_Comprehension'Access,
         Member_Type   => Type_Index_For_Comp_For,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Set_Lit_F_Exprs : aliased constant Text_Type :=
        "Set_Lit_F_Exprs";
      Member_Desc_For_Set_Lit_F_Exprs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Set_Lit_F_Exprs'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Slice_Expr_F_First : aliased constant Text_Type :=
        "Slice_Expr_F_First";
      Member_Desc_For_Slice_Expr_F_First : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Slice_Expr_F_First'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Slice_Expr_F_Last : aliased constant Text_Type :=
        "Slice_Expr_F_Last";
      Member_Desc_For_Slice_Expr_F_Last : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Slice_Expr_F_Last'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Ext_Slice_Expr_F_Stride : aliased constant Text_Type :=
        "Ext_Slice_Expr_F_Stride";
      Member_Desc_For_Ext_Slice_Expr_F_Stride : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Ext_Slice_Expr_F_Stride'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Subscript_Expr_F_Prefix : aliased constant Text_Type :=
        "Subscript_Expr_F_Prefix";
      Member_Desc_For_Subscript_Expr_F_Prefix : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Subscript_Expr_F_Prefix'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Subscript_Expr_F_Suffix : aliased constant Text_Type :=
        "Subscript_Expr_F_Suffix";
      Member_Desc_For_Subscript_Expr_F_Suffix : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Subscript_Expr_F_Suffix'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Tuple_Lit_F_Exprs : aliased constant Text_Type :=
        "Tuple_Lit_F_Exprs";
      Member_Desc_For_Tuple_Lit_F_Exprs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Tuple_Lit_F_Exprs'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Xor_Expr_F_Left : aliased constant Text_Type :=
        "Xor_Expr_F_Left";
      Member_Desc_For_Xor_Expr_F_Left : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Xor_Expr_F_Left'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Xor_Expr_F_Right : aliased constant Text_Type :=
        "Xor_Expr_F_Right";
      Member_Desc_For_Xor_Expr_F_Right : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Xor_Expr_F_Right'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Yield_Expr_F_Exprs : aliased constant Text_Type :=
        "Yield_Expr_F_Exprs";
      Member_Desc_For_Yield_Expr_F_Exprs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Yield_Expr_F_Exprs'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_File_Node_F_Statements : aliased constant Text_Type :=
        "File_Node_F_Statements";
      Member_Desc_For_File_Node_F_Statements : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_File_Node_F_Statements'Access,
         Member_Type   => Type_Index_For_Turkixir_Node_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Params_F_Single_Params : aliased constant Text_Type :=
        "Params_F_Single_Params";
      Member_Desc_For_Params_F_Single_Params : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Params_F_Single_Params'Access,
         Member_Type   => Type_Index_For_Single_Param_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Rel_Name_F_Dots : aliased constant Text_Type :=
        "Rel_Name_F_Dots";
      Member_Desc_For_Rel_Name_F_Dots : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Rel_Name_F_Dots'Access,
         Member_Type   => Type_Index_For_Dot_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Rel_Name_F_Name : aliased constant Text_Type :=
        "Rel_Name_F_Name";
      Member_Desc_For_Rel_Name_F_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Rel_Name_F_Name'Access,
         Member_Type   => Type_Index_For_Name,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Single_Param_F_Is_Varargs : aliased constant Text_Type :=
        "Single_Param_F_Is_Varargs";
      Member_Desc_For_Single_Param_F_Is_Varargs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Single_Param_F_Is_Varargs'Access,
         Member_Type   => Type_Index_For_Var_Args_Flag,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Single_Param_F_Is_Kwargs : aliased constant Text_Type :=
        "Single_Param_F_Is_Kwargs";
      Member_Desc_For_Single_Param_F_Is_Kwargs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Single_Param_F_Is_Kwargs'Access,
         Member_Type   => Type_Index_For_Kw_Args_Flag,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Single_Param_F_Name : aliased constant Text_Type :=
        "Single_Param_F_Name";
      Member_Desc_For_Single_Param_F_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Single_Param_F_Name'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Single_Param_F_Default_Value : aliased constant Text_Type :=
        "Single_Param_F_Default_Value";
      Member_Desc_For_Single_Param_F_Default_Value : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Single_Param_F_Default_Value'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Assert_Stmt_F_Test_Expr : aliased constant Text_Type :=
        "Assert_Stmt_F_Test_Expr";
      Member_Desc_For_Assert_Stmt_F_Test_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Assert_Stmt_F_Test_Expr'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Assert_Stmt_F_Msg : aliased constant Text_Type :=
        "Assert_Stmt_F_Msg";
      Member_Desc_For_Assert_Stmt_F_Msg : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Assert_Stmt_F_Msg'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Assign_Stmt_F_L_Value : aliased constant Text_Type :=
        "Assign_Stmt_F_L_Value";
      Member_Desc_For_Assign_Stmt_F_L_Value : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Assign_Stmt_F_L_Value'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Assign_Stmt_F_R_Values : aliased constant Text_Type :=
        "Assign_Stmt_F_R_Values";
      Member_Desc_For_Assign_Stmt_F_R_Values : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Assign_Stmt_F_R_Values'Access,
         Member_Type   => Type_Index_For_Turkixir_Node_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Aug_Assign_Stmt_F_L_Value : aliased constant Text_Type :=
        "Aug_Assign_Stmt_F_L_Value";
      Member_Desc_For_Aug_Assign_Stmt_F_L_Value : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Aug_Assign_Stmt_F_L_Value'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Aug_Assign_Stmt_F_Op : aliased constant Text_Type :=
        "Aug_Assign_Stmt_F_Op";
      Member_Desc_For_Aug_Assign_Stmt_F_Op : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Aug_Assign_Stmt_F_Op'Access,
         Member_Type   => Type_Index_For_Op,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Aug_Assign_Stmt_F_R_Value : aliased constant Text_Type :=
        "Aug_Assign_Stmt_F_R_Value";
      Member_Desc_For_Aug_Assign_Stmt_F_R_Value : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Aug_Assign_Stmt_F_R_Value'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Decorated_F_Decorators : aliased constant Text_Type :=
        "Decorated_F_Decorators";
      Member_Desc_For_Decorated_F_Decorators : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decorated_F_Decorators'Access,
         Member_Type   => Type_Index_For_Decorator_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Decorated_F_Defn : aliased constant Text_Type :=
        "Decorated_F_Defn";
      Member_Desc_For_Decorated_F_Defn : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decorated_F_Defn'Access,
         Member_Type   => Type_Index_For_Def_Stmt,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Class_Def_F_Name : aliased constant Text_Type :=
        "Class_Def_F_Name";
      Member_Desc_For_Class_Def_F_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Class_Def_F_Name'Access,
         Member_Type   => Type_Index_For_Id,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Class_Def_F_Bases : aliased constant Text_Type :=
        "Class_Def_F_Bases";
      Member_Desc_For_Class_Def_F_Bases : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Class_Def_F_Bases'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Class_Def_F_Statements : aliased constant Text_Type :=
        "Class_Def_F_Statements";
      Member_Desc_For_Class_Def_F_Statements : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Class_Def_F_Statements'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Func_Def_F_Name : aliased constant Text_Type :=
        "Func_Def_F_Name";
      Member_Desc_For_Func_Def_F_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Func_Def_F_Name'Access,
         Member_Type   => Type_Index_For_Id,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Func_Def_F_Parameters : aliased constant Text_Type :=
        "Func_Def_F_Parameters";
      Member_Desc_For_Func_Def_F_Parameters : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Func_Def_F_Parameters'Access,
         Member_Type   => Type_Index_For_Params,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Func_Def_F_Body : aliased constant Text_Type :=
        "Func_Def_F_Body";
      Member_Desc_For_Func_Def_F_Body : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Func_Def_F_Body'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Del_Stmt_F_Exprs : aliased constant Text_Type :=
        "Del_Stmt_F_Exprs";
      Member_Desc_For_Del_Stmt_F_Exprs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Del_Stmt_F_Exprs'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Elif_Branch_F_Cond_Test : aliased constant Text_Type :=
        "Elif_Branch_F_Cond_Test";
      Member_Desc_For_Elif_Branch_F_Cond_Test : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Elif_Branch_F_Cond_Test'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Elif_Branch_F_Statements : aliased constant Text_Type :=
        "Elif_Branch_F_Statements";
      Member_Desc_For_Elif_Branch_F_Statements : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Elif_Branch_F_Statements'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Exec_Stmt_F_Expr : aliased constant Text_Type :=
        "Exec_Stmt_F_Expr";
      Member_Desc_For_Exec_Stmt_F_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Exec_Stmt_F_Expr'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Exec_Stmt_F_In_List : aliased constant Text_Type :=
        "Exec_Stmt_F_In_List";
      Member_Desc_For_Exec_Stmt_F_In_List : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Exec_Stmt_F_In_List'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_For_Stmt_F_Bindings : aliased constant Text_Type :=
        "For_Stmt_F_Bindings";
      Member_Desc_For_For_Stmt_F_Bindings : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_For_Stmt_F_Bindings'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_For_Stmt_F_Expr : aliased constant Text_Type :=
        "For_Stmt_F_Expr";
      Member_Desc_For_For_Stmt_F_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_For_Stmt_F_Expr'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_For_Stmt_F_Statements : aliased constant Text_Type :=
        "For_Stmt_F_Statements";
      Member_Desc_For_For_Stmt_F_Statements : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_For_Stmt_F_Statements'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_For_Stmt_F_Else_Part : aliased constant Text_Type :=
        "For_Stmt_F_Else_Part";
      Member_Desc_For_For_Stmt_F_Else_Part : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_For_Stmt_F_Else_Part'Access,
         Member_Type   => Type_Index_For_Else_Part,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Global_Stmt_F_Names : aliased constant Text_Type :=
        "Global_Stmt_F_Names";
      Member_Desc_For_Global_Stmt_F_Names : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Global_Stmt_F_Names'Access,
         Member_Type   => Type_Index_For_Id_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_If_Stmt_F_Cond_Test : aliased constant Text_Type :=
        "If_Stmt_F_Cond_Test";
      Member_Desc_For_If_Stmt_F_Cond_Test : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_If_Stmt_F_Cond_Test'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_If_Stmt_F_Statements : aliased constant Text_Type :=
        "If_Stmt_F_Statements";
      Member_Desc_For_If_Stmt_F_Statements : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_If_Stmt_F_Statements'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_If_Stmt_F_Elif_Branchs : aliased constant Text_Type :=
        "If_Stmt_F_Elif_Branchs";
      Member_Desc_For_If_Stmt_F_Elif_Branchs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_If_Stmt_F_Elif_Branchs'Access,
         Member_Type   => Type_Index_For_Elif_Branch_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_If_Stmt_F_Else_Part : aliased constant Text_Type :=
        "If_Stmt_F_Else_Part";
      Member_Desc_For_If_Stmt_F_Else_Part : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_If_Stmt_F_Else_Part'Access,
         Member_Type   => Type_Index_For_Else_Part,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Import_From_F_Rel_Name : aliased constant Text_Type :=
        "Import_From_F_Rel_Name";
      Member_Desc_For_Import_From_F_Rel_Name : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Import_From_F_Rel_Name'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Import_From_F_Imported : aliased constant Text_Type :=
        "Import_From_F_Imported";
      Member_Desc_For_Import_From_F_Imported : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Import_From_F_Imported'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Import_Name_F_Imported_Names : aliased constant Text_Type :=
        "Import_Name_F_Imported_Names";
      Member_Desc_For_Import_Name_F_Imported_Names : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Import_Name_F_Imported_Names'Access,
         Member_Type   => Type_Index_For_Turkixir_Node_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Print_Stmt_F_Exprs : aliased constant Text_Type :=
        "Print_Stmt_F_Exprs";
      Member_Desc_For_Print_Stmt_F_Exprs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Print_Stmt_F_Exprs'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Raise_Stmt_F_Exprs : aliased constant Text_Type :=
        "Raise_Stmt_F_Exprs";
      Member_Desc_For_Raise_Stmt_F_Exprs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Raise_Stmt_F_Exprs'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Return_Stmt_F_Exprs : aliased constant Text_Type :=
        "Return_Stmt_F_Exprs";
      Member_Desc_For_Return_Stmt_F_Exprs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Return_Stmt_F_Exprs'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Stream_Print_Stmt_F_Stream_Expr : aliased constant Text_Type :=
        "Stream_Print_Stmt_F_Stream_Expr";
      Member_Desc_For_Stream_Print_Stmt_F_Stream_Expr : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Stream_Print_Stmt_F_Stream_Expr'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Stream_Print_Stmt_F_Exprs : aliased constant Text_Type :=
        "Stream_Print_Stmt_F_Exprs";
      Member_Desc_For_Stream_Print_Stmt_F_Exprs : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Stream_Print_Stmt_F_Exprs'Access,
         Member_Type   => Type_Index_For_Expr_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Try_Stmt_F_Statements : aliased constant Text_Type :=
        "Try_Stmt_F_Statements";
      Member_Desc_For_Try_Stmt_F_Statements : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Try_Stmt_F_Statements'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Try_Stmt_F_Except_Parts : aliased constant Text_Type :=
        "Try_Stmt_F_Except_Parts";
      Member_Desc_For_Try_Stmt_F_Except_Parts : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Try_Stmt_F_Except_Parts'Access,
         Member_Type   => Type_Index_For_Except_Part_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Try_Stmt_F_Else_Part : aliased constant Text_Type :=
        "Try_Stmt_F_Else_Part";
      Member_Desc_For_Try_Stmt_F_Else_Part : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Try_Stmt_F_Else_Part'Access,
         Member_Type   => Type_Index_For_Else_Part,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Try_Stmt_F_Finally_Part : aliased constant Text_Type :=
        "Try_Stmt_F_Finally_Part";
      Member_Desc_For_Try_Stmt_F_Finally_Part : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Try_Stmt_F_Finally_Part'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_While_Stmt_F_Cond_Test : aliased constant Text_Type :=
        "While_Stmt_F_Cond_Test";
      Member_Desc_For_While_Stmt_F_Cond_Test : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_While_Stmt_F_Cond_Test'Access,
         Member_Type   => Type_Index_For_Expr,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_While_Stmt_F_Statements : aliased constant Text_Type :=
        "While_Stmt_F_Statements";
      Member_Desc_For_While_Stmt_F_Statements : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_While_Stmt_F_Statements'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_While_Stmt_F_Else_Part : aliased constant Text_Type :=
        "While_Stmt_F_Else_Part";
      Member_Desc_For_While_Stmt_F_Else_Part : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_While_Stmt_F_Else_Part'Access,
         Member_Type   => Type_Index_For_Else_Part,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_With_Stmt_F_Bindings : aliased constant Text_Type :=
        "With_Stmt_F_Bindings";
      Member_Desc_For_With_Stmt_F_Bindings : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_With_Stmt_F_Bindings'Access,
         Member_Type   => Type_Index_For_As_Name_Node_List,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_With_Stmt_F_Statements : aliased constant Text_Type :=
        "With_Stmt_F_Statements";
      Member_Desc_For_With_Stmt_F_Statements : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_With_Stmt_F_Statements'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Parent : aliased constant Text_Type :=
        "Parent";
      Member_Desc_For_Parent : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Parent'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      

         Arg_Name_1 : aliased constant Text_Type :=
           "With_Self";
         

      Member_Name_For_Parents : aliased constant Text_Type :=
        "Parents";
      Member_Desc_For_Parents : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 1,
         Name          => Member_Name_For_Parents'Access,
         Member_Type   => Type_Index_For_Turkixir_Node_Array,
         Arguments     => (
               1 => (Name => Arg_Name_1'Access, Argument_Type => Type_Index_For_Boolean)
        ));

      


      Member_Name_For_Children : aliased constant Text_Type :=
        "Children";
      Member_Desc_For_Children : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Children'Access,
         Member_Type   => Type_Index_For_Turkixir_Node_Array,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Token_Start : aliased constant Text_Type :=
        "Token_Start";
      Member_Desc_For_Token_Start : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Token_Start'Access,
         Member_Type   => Type_Index_For_Token_Reference,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Token_End : aliased constant Text_Type :=
        "Token_End";
      Member_Desc_For_Token_End : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Token_End'Access,
         Member_Type   => Type_Index_For_Token_Reference,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Child_Index : aliased constant Text_Type :=
        "Child_Index";
      Member_Desc_For_Child_Index : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Child_Index'Access,
         Member_Type   => Type_Index_For_Integer,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Previous_Sibling : aliased constant Text_Type :=
        "Previous_Sibling";
      Member_Desc_For_Previous_Sibling : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Previous_Sibling'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Next_Sibling : aliased constant Text_Type :=
        "Next_Sibling";
      Member_Desc_For_Next_Sibling : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Next_Sibling'Access,
         Member_Type   => Type_Index_For_Turkixir_Node,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Unit : aliased constant Text_Type :=
        "Unit";
      Member_Desc_For_Unit : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Unit'Access,
         Member_Type   => Type_Index_For_Analysis_Unit,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Is_Ghost : aliased constant Text_Type :=
        "Is_Ghost";
      Member_Desc_For_Is_Ghost : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Is_Ghost'Access,
         Member_Type   => Type_Index_For_Boolean,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Full_Sloc_Image : aliased constant Text_Type :=
        "Full_Sloc_Image";
      Member_Desc_For_Full_Sloc_Image : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Full_Sloc_Image'Access,
         Member_Type   => Type_Index_For_Text_Type,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Dispatcher_Kw_Args_Flag_P_As_Bool : aliased constant Text_Type :=
        "Dispatcher_Kw_Args_Flag_P_As_Bool";
      Member_Desc_For_Dispatcher_Kw_Args_Flag_P_As_Bool : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Dispatcher_Kw_Args_Flag_P_As_Bool'Access,
         Member_Type   => Type_Index_For_Boolean,
         Arguments     => (
               1 .. 0 => <>
        ));

      


      Member_Name_For_Dispatcher_Var_Args_Flag_P_As_Bool : aliased constant Text_Type :=
        "Dispatcher_Var_Args_Flag_P_As_Bool";
      Member_Desc_For_Dispatcher_Var_Args_Flag_P_As_Bool : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Dispatcher_Var_Args_Flag_P_As_Bool'Access,
         Member_Type   => Type_Index_For_Boolean,
         Arguments     => (
               1 .. 0 => <>
        ));


   Struct_Members : aliased constant Struct_Member_Descriptor_Array := (
      Member_Index_For_Arg_Assoc_F_Name => Member_Desc_For_Arg_Assoc_F_Name'Access,
Member_Index_For_Arg_Assoc_F_Expr => Member_Desc_For_Arg_Assoc_F_Expr'Access,
Member_Index_For_Arg_Gen_F_Expr => Member_Desc_For_Arg_Gen_F_Expr'Access,
Member_Index_For_Arg_Gen_F_Comprehension => Member_Desc_For_Arg_Gen_F_Comprehension'Access,
Member_Index_For_Kw_Args_F_Expr => Member_Desc_For_Kw_Args_F_Expr'Access,
Member_Index_For_Var_Args_F_Expr => Member_Desc_For_Var_Args_F_Expr'Access,
Member_Index_For_As_Name_Node_F_Imported => Member_Desc_For_As_Name_Node_F_Imported'Access,
Member_Index_For_As_Name_Node_F_As_Name => Member_Desc_For_As_Name_Node_F_As_Name'Access,
Member_Index_For_Comp_If_F_Test => Member_Desc_For_Comp_If_F_Test'Access,
Member_Index_For_Comp_If_F_Comp => Member_Desc_For_Comp_If_F_Comp'Access,
Member_Index_For_Comp_For_F_Exprs => Member_Desc_For_Comp_For_F_Exprs'Access,
Member_Index_For_Comp_For_F_Target => Member_Desc_For_Comp_For_F_Target'Access,
Member_Index_For_Comp_For_F_Comp => Member_Desc_For_Comp_For_F_Comp'Access,
Member_Index_For_Comp_ForL_F_Exprs => Member_Desc_For_Comp_ForL_F_Exprs'Access,
Member_Index_For_Comp_ForL_F_Target => Member_Desc_For_Comp_ForL_F_Target'Access,
Member_Index_For_Comp_ForL_F_Comp => Member_Desc_For_Comp_ForL_F_Comp'Access,
Member_Index_For_Decorator_F_Dec_Name => Member_Desc_For_Decorator_F_Dec_Name'Access,
Member_Index_For_Decorator_F_Arg_List => Member_Desc_For_Decorator_F_Arg_List'Access,
Member_Index_For_Dict_Assoc_F_Key => Member_Desc_For_Dict_Assoc_F_Key'Access,
Member_Index_For_Dict_Assoc_F_Value => Member_Desc_For_Dict_Assoc_F_Value'Access,
Member_Index_For_Else_Part_F_Statements => Member_Desc_For_Else_Part_F_Statements'Access,
Member_Index_For_Except_Part_F_As_Name => Member_Desc_For_Except_Part_F_As_Name'Access,
Member_Index_For_Except_Part_F_Statements => Member_Desc_For_Except_Part_F_Statements'Access,
Member_Index_For_And_Expr_F_Left => Member_Desc_For_And_Expr_F_Left'Access,
Member_Index_For_And_Expr_F_Right => Member_Desc_For_And_Expr_F_Right'Access,
Member_Index_For_And_Op_F_Left => Member_Desc_For_And_Op_F_Left'Access,
Member_Index_For_And_Op_F_Right => Member_Desc_For_And_Op_F_Right'Access,
Member_Index_For_Bin_Op_F_Left => Member_Desc_For_Bin_Op_F_Left'Access,
Member_Index_For_Bin_Op_F_Op => Member_Desc_For_Bin_Op_F_Op'Access,
Member_Index_For_Bin_Op_F_Right => Member_Desc_For_Bin_Op_F_Right'Access,
Member_Index_For_Call_Expr_F_Prefix => Member_Desc_For_Call_Expr_F_Prefix'Access,
Member_Index_For_Call_Expr_F_Suffix => Member_Desc_For_Call_Expr_F_Suffix'Access,
Member_Index_For_Comp_Op_F_Left => Member_Desc_For_Comp_Op_F_Left'Access,
Member_Index_For_Comp_Op_F_Op => Member_Desc_For_Comp_Op_F_Op'Access,
Member_Index_For_Comp_Op_F_Right => Member_Desc_For_Comp_Op_F_Right'Access,
Member_Index_For_Concat_String_Lit_F_First_Str => Member_Desc_For_Concat_String_Lit_F_First_Str'Access,
Member_Index_For_Concat_String_Lit_F_Subsequent_Str => Member_Desc_For_Concat_String_Lit_F_Subsequent_Str'Access,
Member_Index_For_Dict_Comp_F_Assoc => Member_Desc_For_Dict_Comp_F_Assoc'Access,
Member_Index_For_Dict_Comp_F_Comprehension => Member_Desc_For_Dict_Comp_F_Comprehension'Access,
Member_Index_For_Dict_Lit_F_Assocs => Member_Desc_For_Dict_Lit_F_Assocs'Access,
Member_Index_For_Factor_F_Op => Member_Desc_For_Factor_F_Op'Access,
Member_Index_For_Factor_F_Expr => Member_Desc_For_Factor_F_Expr'Access,
Member_Index_For_If_Expr_F_Expr => Member_Desc_For_If_Expr_F_Expr'Access,
Member_Index_For_If_Expr_F_Cond => Member_Desc_For_If_Expr_F_Cond'Access,
Member_Index_For_If_Expr_F_Else_Expr => Member_Desc_For_If_Expr_F_Else_Expr'Access,
Member_Index_For_Inline_Eval_F_Exprs => Member_Desc_For_Inline_Eval_F_Exprs'Access,
Member_Index_For_Lambda_Def_F_Args => Member_Desc_For_Lambda_Def_F_Args'Access,
Member_Index_For_Lambda_Def_F_Expr => Member_Desc_For_Lambda_Def_F_Expr'Access,
Member_Index_For_List_Comp_F_Expr => Member_Desc_For_List_Comp_F_Expr'Access,
Member_Index_For_List_Comp_F_Comprehension => Member_Desc_For_List_Comp_F_Comprehension'Access,
Member_Index_For_List_Gen_F_Expr => Member_Desc_For_List_Gen_F_Expr'Access,
Member_Index_For_List_Gen_F_Comprehension => Member_Desc_For_List_Gen_F_Comprehension'Access,
Member_Index_For_List_Lit_F_Exprs => Member_Desc_For_List_Lit_F_Exprs'Access,
Member_Index_For_Dotted_Name_F_Prefix => Member_Desc_For_Dotted_Name_F_Prefix'Access,
Member_Index_For_Dotted_Name_F_Suffix => Member_Desc_For_Dotted_Name_F_Suffix'Access,
Member_Index_For_Not_Op_F_Expr => Member_Desc_For_Not_Op_F_Expr'Access,
Member_Index_For_Or_Expr_F_Left => Member_Desc_For_Or_Expr_F_Left'Access,
Member_Index_For_Or_Expr_F_Right => Member_Desc_For_Or_Expr_F_Right'Access,
Member_Index_For_Or_Op_F_Left => Member_Desc_For_Or_Op_F_Left'Access,
Member_Index_For_Or_Op_F_Right => Member_Desc_For_Or_Op_F_Right'Access,
Member_Index_For_Power_F_Left => Member_Desc_For_Power_F_Left'Access,
Member_Index_For_Power_F_Right => Member_Desc_For_Power_F_Right'Access,
Member_Index_For_Set_Comp_F_Expr => Member_Desc_For_Set_Comp_F_Expr'Access,
Member_Index_For_Set_Comp_F_Comprehension => Member_Desc_For_Set_Comp_F_Comprehension'Access,
Member_Index_For_Set_Lit_F_Exprs => Member_Desc_For_Set_Lit_F_Exprs'Access,
Member_Index_For_Slice_Expr_F_First => Member_Desc_For_Slice_Expr_F_First'Access,
Member_Index_For_Slice_Expr_F_Last => Member_Desc_For_Slice_Expr_F_Last'Access,
Member_Index_For_Ext_Slice_Expr_F_Stride => Member_Desc_For_Ext_Slice_Expr_F_Stride'Access,
Member_Index_For_Subscript_Expr_F_Prefix => Member_Desc_For_Subscript_Expr_F_Prefix'Access,
Member_Index_For_Subscript_Expr_F_Suffix => Member_Desc_For_Subscript_Expr_F_Suffix'Access,
Member_Index_For_Tuple_Lit_F_Exprs => Member_Desc_For_Tuple_Lit_F_Exprs'Access,
Member_Index_For_Xor_Expr_F_Left => Member_Desc_For_Xor_Expr_F_Left'Access,
Member_Index_For_Xor_Expr_F_Right => Member_Desc_For_Xor_Expr_F_Right'Access,
Member_Index_For_Yield_Expr_F_Exprs => Member_Desc_For_Yield_Expr_F_Exprs'Access,
Member_Index_For_File_Node_F_Statements => Member_Desc_For_File_Node_F_Statements'Access,
Member_Index_For_Params_F_Single_Params => Member_Desc_For_Params_F_Single_Params'Access,
Member_Index_For_Rel_Name_F_Dots => Member_Desc_For_Rel_Name_F_Dots'Access,
Member_Index_For_Rel_Name_F_Name => Member_Desc_For_Rel_Name_F_Name'Access,
Member_Index_For_Single_Param_F_Is_Varargs => Member_Desc_For_Single_Param_F_Is_Varargs'Access,
Member_Index_For_Single_Param_F_Is_Kwargs => Member_Desc_For_Single_Param_F_Is_Kwargs'Access,
Member_Index_For_Single_Param_F_Name => Member_Desc_For_Single_Param_F_Name'Access,
Member_Index_For_Single_Param_F_Default_Value => Member_Desc_For_Single_Param_F_Default_Value'Access,
Member_Index_For_Assert_Stmt_F_Test_Expr => Member_Desc_For_Assert_Stmt_F_Test_Expr'Access,
Member_Index_For_Assert_Stmt_F_Msg => Member_Desc_For_Assert_Stmt_F_Msg'Access,
Member_Index_For_Assign_Stmt_F_L_Value => Member_Desc_For_Assign_Stmt_F_L_Value'Access,
Member_Index_For_Assign_Stmt_F_R_Values => Member_Desc_For_Assign_Stmt_F_R_Values'Access,
Member_Index_For_Aug_Assign_Stmt_F_L_Value => Member_Desc_For_Aug_Assign_Stmt_F_L_Value'Access,
Member_Index_For_Aug_Assign_Stmt_F_Op => Member_Desc_For_Aug_Assign_Stmt_F_Op'Access,
Member_Index_For_Aug_Assign_Stmt_F_R_Value => Member_Desc_For_Aug_Assign_Stmt_F_R_Value'Access,
Member_Index_For_Decorated_F_Decorators => Member_Desc_For_Decorated_F_Decorators'Access,
Member_Index_For_Decorated_F_Defn => Member_Desc_For_Decorated_F_Defn'Access,
Member_Index_For_Class_Def_F_Name => Member_Desc_For_Class_Def_F_Name'Access,
Member_Index_For_Class_Def_F_Bases => Member_Desc_For_Class_Def_F_Bases'Access,
Member_Index_For_Class_Def_F_Statements => Member_Desc_For_Class_Def_F_Statements'Access,
Member_Index_For_Func_Def_F_Name => Member_Desc_For_Func_Def_F_Name'Access,
Member_Index_For_Func_Def_F_Parameters => Member_Desc_For_Func_Def_F_Parameters'Access,
Member_Index_For_Func_Def_F_Body => Member_Desc_For_Func_Def_F_Body'Access,
Member_Index_For_Del_Stmt_F_Exprs => Member_Desc_For_Del_Stmt_F_Exprs'Access,
Member_Index_For_Elif_Branch_F_Cond_Test => Member_Desc_For_Elif_Branch_F_Cond_Test'Access,
Member_Index_For_Elif_Branch_F_Statements => Member_Desc_For_Elif_Branch_F_Statements'Access,
Member_Index_For_Exec_Stmt_F_Expr => Member_Desc_For_Exec_Stmt_F_Expr'Access,
Member_Index_For_Exec_Stmt_F_In_List => Member_Desc_For_Exec_Stmt_F_In_List'Access,
Member_Index_For_For_Stmt_F_Bindings => Member_Desc_For_For_Stmt_F_Bindings'Access,
Member_Index_For_For_Stmt_F_Expr => Member_Desc_For_For_Stmt_F_Expr'Access,
Member_Index_For_For_Stmt_F_Statements => Member_Desc_For_For_Stmt_F_Statements'Access,
Member_Index_For_For_Stmt_F_Else_Part => Member_Desc_For_For_Stmt_F_Else_Part'Access,
Member_Index_For_Global_Stmt_F_Names => Member_Desc_For_Global_Stmt_F_Names'Access,
Member_Index_For_If_Stmt_F_Cond_Test => Member_Desc_For_If_Stmt_F_Cond_Test'Access,
Member_Index_For_If_Stmt_F_Statements => Member_Desc_For_If_Stmt_F_Statements'Access,
Member_Index_For_If_Stmt_F_Elif_Branchs => Member_Desc_For_If_Stmt_F_Elif_Branchs'Access,
Member_Index_For_If_Stmt_F_Else_Part => Member_Desc_For_If_Stmt_F_Else_Part'Access,
Member_Index_For_Import_From_F_Rel_Name => Member_Desc_For_Import_From_F_Rel_Name'Access,
Member_Index_For_Import_From_F_Imported => Member_Desc_For_Import_From_F_Imported'Access,
Member_Index_For_Import_Name_F_Imported_Names => Member_Desc_For_Import_Name_F_Imported_Names'Access,
Member_Index_For_Print_Stmt_F_Exprs => Member_Desc_For_Print_Stmt_F_Exprs'Access,
Member_Index_For_Raise_Stmt_F_Exprs => Member_Desc_For_Raise_Stmt_F_Exprs'Access,
Member_Index_For_Return_Stmt_F_Exprs => Member_Desc_For_Return_Stmt_F_Exprs'Access,
Member_Index_For_Stream_Print_Stmt_F_Stream_Expr => Member_Desc_For_Stream_Print_Stmt_F_Stream_Expr'Access,
Member_Index_For_Stream_Print_Stmt_F_Exprs => Member_Desc_For_Stream_Print_Stmt_F_Exprs'Access,
Member_Index_For_Try_Stmt_F_Statements => Member_Desc_For_Try_Stmt_F_Statements'Access,
Member_Index_For_Try_Stmt_F_Except_Parts => Member_Desc_For_Try_Stmt_F_Except_Parts'Access,
Member_Index_For_Try_Stmt_F_Else_Part => Member_Desc_For_Try_Stmt_F_Else_Part'Access,
Member_Index_For_Try_Stmt_F_Finally_Part => Member_Desc_For_Try_Stmt_F_Finally_Part'Access,
Member_Index_For_While_Stmt_F_Cond_Test => Member_Desc_For_While_Stmt_F_Cond_Test'Access,
Member_Index_For_While_Stmt_F_Statements => Member_Desc_For_While_Stmt_F_Statements'Access,
Member_Index_For_While_Stmt_F_Else_Part => Member_Desc_For_While_Stmt_F_Else_Part'Access,
Member_Index_For_With_Stmt_F_Bindings => Member_Desc_For_With_Stmt_F_Bindings'Access,
Member_Index_For_With_Stmt_F_Statements => Member_Desc_For_With_Stmt_F_Statements'Access,
Member_Index_For_Parent => Member_Desc_For_Parent'Access,
Member_Index_For_Parents => Member_Desc_For_Parents'Access,
Member_Index_For_Children => Member_Desc_For_Children'Access,
Member_Index_For_Token_Start => Member_Desc_For_Token_Start'Access,
Member_Index_For_Token_End => Member_Desc_For_Token_End'Access,
Member_Index_For_Child_Index => Member_Desc_For_Child_Index'Access,
Member_Index_For_Previous_Sibling => Member_Desc_For_Previous_Sibling'Access,
Member_Index_For_Next_Sibling => Member_Desc_For_Next_Sibling'Access,
Member_Index_For_Unit => Member_Desc_For_Unit'Access,
Member_Index_For_Is_Ghost => Member_Desc_For_Is_Ghost'Access,
Member_Index_For_Full_Sloc_Image => Member_Desc_For_Full_Sloc_Image'Access,
Member_Index_For_Dispatcher_Kw_Args_Flag_P_As_Bool => Member_Desc_For_Dispatcher_Kw_Args_Flag_P_As_Bool'Access,
Member_Index_For_Dispatcher_Var_Args_Flag_P_As_Bool => Member_Desc_For_Dispatcher_Var_Args_Flag_P_As_Bool'Access
   );

   -----------------------------
   -- Struct type descriptors --
   -----------------------------

   
      
      Node_Name_For_Turkixir_Node : aliased constant Text_Type :=
        "Turkixir_Node";
      Node_Desc_For_Turkixir_Node : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 21,
         Member_Count      => 11,
         Base_Type         => No_Type_Index,
         Is_Abstract       => True,
         Name              => Node_Name_For_Turkixir_Node'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 => Type_Index_For_Arg,
2 => Type_Index_For_As_Name_Node,
3 => Type_Index_For_Comp_If,
4 => Type_Index_For_Comp_Op_Kind,
5 => Type_Index_For_Comprehension,
6 => Type_Index_For_Decorator,
7 => Type_Index_For_Dict_Assoc,
8 => Type_Index_For_Else_Part,
9 => Type_Index_For_Except_Part,
10 => Type_Index_For_Expr,
11 => Type_Index_For_File_Node,
12 => Type_Index_For_Import_Star,
13 => Type_Index_For_Kw_Args_Flag,
14 => Type_Index_For_NL,
15 => Type_Index_For_Op,
16 => Type_Index_For_Params,
17 => Type_Index_For_Rel_Name,
18 => Type_Index_For_Single_Param,
19 => Type_Index_For_Stmt,
20 => Type_Index_For_Turkixir_Node_Base_List,
21 => Type_Index_For_Var_Args_Flag
         ),
         Members           => (
              1 => Member_Index_For_Parent,
2 => Member_Index_For_Parents,
3 => Member_Index_For_Children,
4 => Member_Index_For_Token_Start,
5 => Member_Index_For_Token_End,
6 => Member_Index_For_Child_Index,
7 => Member_Index_For_Previous_Sibling,
8 => Member_Index_For_Next_Sibling,
9 => Member_Index_For_Unit,
10 => Member_Index_For_Is_Ghost,
11 => Member_Index_For_Full_Sloc_Image
         ));
      
      Node_Name_For_Arg : aliased constant Text_Type :=
        "Arg";
      Node_Desc_For_Arg : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 4,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => True,
         Name              => Node_Name_For_Arg'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 => Type_Index_For_Arg_Assoc,
2 => Type_Index_For_Arg_Gen,
3 => Type_Index_For_Kw_Args,
4 => Type_Index_For_Var_Args
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Arg_Assoc : aliased constant Text_Type :=
        "Arg_Assoc";
      Node_Desc_For_Arg_Assoc : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Arg,
         Is_Abstract       => False,
         Name              => Node_Name_For_Arg_Assoc'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Arg_Assoc_F_Name,
2 => Member_Index_For_Arg_Assoc_F_Expr
         ));
      
      Node_Name_For_Arg_Gen : aliased constant Text_Type :=
        "Arg_Gen";
      Node_Desc_For_Arg_Gen : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Arg,
         Is_Abstract       => False,
         Name              => Node_Name_For_Arg_Gen'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Arg_Gen_F_Expr,
2 => Member_Index_For_Arg_Gen_F_Comprehension
         ));
      
      Node_Name_For_Kw_Args : aliased constant Text_Type :=
        "Kw_Args";
      Node_Desc_For_Kw_Args : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Arg,
         Is_Abstract       => False,
         Name              => Node_Name_For_Kw_Args'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Kw_Args_F_Expr
         ));
      
      Node_Name_For_Var_Args : aliased constant Text_Type :=
        "Var_Args";
      Node_Desc_For_Var_Args : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Arg,
         Is_Abstract       => False,
         Name              => Node_Name_For_Var_Args'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Var_Args_F_Expr
         ));
      
      Node_Name_For_As_Name_Node : aliased constant Text_Type :=
        "As_Name_Node";
      Node_Desc_For_As_Name_Node : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => False,
         Name              => Node_Name_For_As_Name_Node'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_As_Name_Node_F_Imported,
2 => Member_Index_For_As_Name_Node_F_As_Name
         ));
      
      Node_Name_For_Comp_If : aliased constant Text_Type :=
        "Comp_If";
      Node_Desc_For_Comp_If : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => False,
         Name              => Node_Name_For_Comp_If'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Comp_If_F_Test,
2 => Member_Index_For_Comp_If_F_Comp
         ));
      
      Node_Name_For_Comp_Op_Kind : aliased constant Text_Type :=
        "Comp_Op_Kind";
      Node_Desc_For_Comp_Op_Kind : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 11,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => True,
         Name              => Node_Name_For_Comp_Op_Kind'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 => Type_Index_For_Comp_Op_Kind_Diamond,
2 => Type_Index_For_Comp_Op_Kind_Eq,
3 => Type_Index_For_Comp_Op_Kind_Gt,
4 => Type_Index_For_Comp_Op_Kind_Gte,
5 => Type_Index_For_Comp_Op_Kind_In,
6 => Type_Index_For_Comp_Op_Kind_Is,
7 => Type_Index_For_Comp_Op_Kind_Isnot,
8 => Type_Index_For_Comp_Op_Kind_Lt,
9 => Type_Index_For_Comp_Op_Kind_Lte,
10 => Type_Index_For_Comp_Op_Kind_Noteq,
11 => Type_Index_For_Comp_Op_Kind_Notin
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Comp_Op_Kind_Diamond : aliased constant Text_Type :=
        "Comp_Op_Kind_Diamond";
      Node_Desc_For_Comp_Op_Kind_Diamond : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Comp_Op_Kind,
         Is_Abstract       => False,
         Name              => Node_Name_For_Comp_Op_Kind_Diamond'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Comp_Op_Kind_Eq : aliased constant Text_Type :=
        "Comp_Op_Kind_Eq";
      Node_Desc_For_Comp_Op_Kind_Eq : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Comp_Op_Kind,
         Is_Abstract       => False,
         Name              => Node_Name_For_Comp_Op_Kind_Eq'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Comp_Op_Kind_Gt : aliased constant Text_Type :=
        "Comp_Op_Kind_Gt";
      Node_Desc_For_Comp_Op_Kind_Gt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Comp_Op_Kind,
         Is_Abstract       => False,
         Name              => Node_Name_For_Comp_Op_Kind_Gt'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Comp_Op_Kind_Gte : aliased constant Text_Type :=
        "Comp_Op_Kind_Gte";
      Node_Desc_For_Comp_Op_Kind_Gte : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Comp_Op_Kind,
         Is_Abstract       => False,
         Name              => Node_Name_For_Comp_Op_Kind_Gte'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Comp_Op_Kind_In : aliased constant Text_Type :=
        "Comp_Op_Kind_In";
      Node_Desc_For_Comp_Op_Kind_In : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Comp_Op_Kind,
         Is_Abstract       => False,
         Name              => Node_Name_For_Comp_Op_Kind_In'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Comp_Op_Kind_Is : aliased constant Text_Type :=
        "Comp_Op_Kind_Is";
      Node_Desc_For_Comp_Op_Kind_Is : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Comp_Op_Kind,
         Is_Abstract       => False,
         Name              => Node_Name_For_Comp_Op_Kind_Is'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Comp_Op_Kind_Isnot : aliased constant Text_Type :=
        "Comp_Op_Kind_Isnot";
      Node_Desc_For_Comp_Op_Kind_Isnot : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Comp_Op_Kind,
         Is_Abstract       => False,
         Name              => Node_Name_For_Comp_Op_Kind_Isnot'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Comp_Op_Kind_Lt : aliased constant Text_Type :=
        "Comp_Op_Kind_Lt";
      Node_Desc_For_Comp_Op_Kind_Lt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Comp_Op_Kind,
         Is_Abstract       => False,
         Name              => Node_Name_For_Comp_Op_Kind_Lt'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Comp_Op_Kind_Lte : aliased constant Text_Type :=
        "Comp_Op_Kind_Lte";
      Node_Desc_For_Comp_Op_Kind_Lte : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Comp_Op_Kind,
         Is_Abstract       => False,
         Name              => Node_Name_For_Comp_Op_Kind_Lte'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Comp_Op_Kind_Noteq : aliased constant Text_Type :=
        "Comp_Op_Kind_Noteq";
      Node_Desc_For_Comp_Op_Kind_Noteq : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Comp_Op_Kind,
         Is_Abstract       => False,
         Name              => Node_Name_For_Comp_Op_Kind_Noteq'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Comp_Op_Kind_Notin : aliased constant Text_Type :=
        "Comp_Op_Kind_Notin";
      Node_Desc_For_Comp_Op_Kind_Notin : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Comp_Op_Kind,
         Is_Abstract       => False,
         Name              => Node_Name_For_Comp_Op_Kind_Notin'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Comprehension : aliased constant Text_Type :=
        "Comprehension";
      Node_Desc_For_Comprehension : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => True,
         Name              => Node_Name_For_Comprehension'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 => Type_Index_For_Comp_For,
2 => Type_Index_For_Comp_ForL
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Comp_For : aliased constant Text_Type :=
        "Comp_For";
      Node_Desc_For_Comp_For : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Comprehension,
         Is_Abstract       => False,
         Name              => Node_Name_For_Comp_For'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Comp_For_F_Exprs,
2 => Member_Index_For_Comp_For_F_Target,
3 => Member_Index_For_Comp_For_F_Comp
         ));
      
      Node_Name_For_Comp_ForL : aliased constant Text_Type :=
        "Comp_ForL";
      Node_Desc_For_Comp_ForL : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Comprehension,
         Is_Abstract       => False,
         Name              => Node_Name_For_Comp_ForL'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Comp_ForL_F_Exprs,
2 => Member_Index_For_Comp_ForL_F_Target,
3 => Member_Index_For_Comp_ForL_F_Comp
         ));
      
      Node_Name_For_Decorator : aliased constant Text_Type :=
        "Decorator";
      Node_Desc_For_Decorator : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => False,
         Name              => Node_Name_For_Decorator'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decorator_F_Dec_Name,
2 => Member_Index_For_Decorator_F_Arg_List
         ));
      
      Node_Name_For_Dict_Assoc : aliased constant Text_Type :=
        "Dict_Assoc";
      Node_Desc_For_Dict_Assoc : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => False,
         Name              => Node_Name_For_Dict_Assoc'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Dict_Assoc_F_Key,
2 => Member_Index_For_Dict_Assoc_F_Value
         ));
      
      Node_Name_For_Else_Part : aliased constant Text_Type :=
        "Else_Part";
      Node_Desc_For_Else_Part : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => False,
         Name              => Node_Name_For_Else_Part'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Else_Part_F_Statements
         ));
      
      Node_Name_For_Except_Part : aliased constant Text_Type :=
        "Except_Part";
      Node_Desc_For_Except_Part : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => False,
         Name              => Node_Name_For_Except_Part'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Except_Part_F_As_Name,
2 => Member_Index_For_Except_Part_F_Statements
         ));
      
      Node_Name_For_Expr : aliased constant Text_Type :=
        "Expr";
      Node_Desc_For_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 31,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => True,
         Name              => Node_Name_For_Expr'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 => Type_Index_For_And_Expr,
2 => Type_Index_For_And_Op,
3 => Type_Index_For_Bin_Op,
4 => Type_Index_For_Call_Expr,
5 => Type_Index_For_Comp_Op,
6 => Type_Index_For_Concat_String_Lit,
7 => Type_Index_For_Dict_Comp,
8 => Type_Index_For_Dict_Lit,
9 => Type_Index_For_Dot,
10 => Type_Index_For_Ellipsis_Expr,
11 => Type_Index_For_Factor,
12 => Type_Index_For_If_Expr,
13 => Type_Index_For_Inline_Eval,
14 => Type_Index_For_Lambda_Def,
15 => Type_Index_For_List_Comp,
16 => Type_Index_For_List_Gen,
17 => Type_Index_For_List_Lit,
18 => Type_Index_For_Name,
19 => Type_Index_For_Not_Op,
20 => Type_Index_For_Number_Lit,
21 => Type_Index_For_Or_Expr,
22 => Type_Index_For_Or_Op,
23 => Type_Index_For_Power,
24 => Type_Index_For_Set_Comp,
25 => Type_Index_For_Set_Lit,
26 => Type_Index_For_Slice_Expr,
27 => Type_Index_For_String_Lit,
28 => Type_Index_For_Subscript_Expr,
29 => Type_Index_For_Tuple_Lit,
30 => Type_Index_For_Xor_Expr,
31 => Type_Index_For_Yield_Expr
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_And_Expr : aliased constant Text_Type :=
        "And_Expr";
      Node_Desc_For_And_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_And_Expr'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_And_Expr_F_Left,
2 => Member_Index_For_And_Expr_F_Right
         ));
      
      Node_Name_For_And_Op : aliased constant Text_Type :=
        "And_Op";
      Node_Desc_For_And_Op : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_And_Op'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_And_Op_F_Left,
2 => Member_Index_For_And_Op_F_Right
         ));
      
      Node_Name_For_Bin_Op : aliased constant Text_Type :=
        "Bin_Op";
      Node_Desc_For_Bin_Op : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 3,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => True,
         Name              => Node_Name_For_Bin_Op'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 => Type_Index_For_Arith_Expr,
2 => Type_Index_For_Shift_Expr,
3 => Type_Index_For_Term
         ),
         Members           => (
              1 => Member_Index_For_Bin_Op_F_Left,
2 => Member_Index_For_Bin_Op_F_Op,
3 => Member_Index_For_Bin_Op_F_Right
         ));
      
      Node_Name_For_Arith_Expr : aliased constant Text_Type :=
        "Arith_Expr";
      Node_Desc_For_Arith_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Bin_Op,
         Is_Abstract       => False,
         Name              => Node_Name_For_Arith_Expr'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Shift_Expr : aliased constant Text_Type :=
        "Shift_Expr";
      Node_Desc_For_Shift_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Bin_Op,
         Is_Abstract       => False,
         Name              => Node_Name_For_Shift_Expr'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Term : aliased constant Text_Type :=
        "Term";
      Node_Desc_For_Term : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Bin_Op,
         Is_Abstract       => False,
         Name              => Node_Name_For_Term'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Call_Expr : aliased constant Text_Type :=
        "Call_Expr";
      Node_Desc_For_Call_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Call_Expr'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Call_Expr_F_Prefix,
2 => Member_Index_For_Call_Expr_F_Suffix
         ));
      
      Node_Name_For_Comp_Op : aliased constant Text_Type :=
        "Comp_Op";
      Node_Desc_For_Comp_Op : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Comp_Op'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Comp_Op_F_Left,
2 => Member_Index_For_Comp_Op_F_Op,
3 => Member_Index_For_Comp_Op_F_Right
         ));
      
      Node_Name_For_Concat_String_Lit : aliased constant Text_Type :=
        "Concat_String_Lit";
      Node_Desc_For_Concat_String_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Concat_String_Lit'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Concat_String_Lit_F_First_Str,
2 => Member_Index_For_Concat_String_Lit_F_Subsequent_Str
         ));
      
      Node_Name_For_Dict_Comp : aliased constant Text_Type :=
        "Dict_Comp";
      Node_Desc_For_Dict_Comp : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Dict_Comp'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Dict_Comp_F_Assoc,
2 => Member_Index_For_Dict_Comp_F_Comprehension
         ));
      
      Node_Name_For_Dict_Lit : aliased constant Text_Type :=
        "Dict_Lit";
      Node_Desc_For_Dict_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Dict_Lit'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Dict_Lit_F_Assocs
         ));
      
      Node_Name_For_Dot : aliased constant Text_Type :=
        "Dot";
      Node_Desc_For_Dot : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Dot'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Ellipsis_Expr : aliased constant Text_Type :=
        "Ellipsis_Expr";
      Node_Desc_For_Ellipsis_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Ellipsis_Expr'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Factor : aliased constant Text_Type :=
        "Factor";
      Node_Desc_For_Factor : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Factor'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Factor_F_Op,
2 => Member_Index_For_Factor_F_Expr
         ));
      
      Node_Name_For_If_Expr : aliased constant Text_Type :=
        "If_Expr";
      Node_Desc_For_If_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_If_Expr'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_If_Expr_F_Expr,
2 => Member_Index_For_If_Expr_F_Cond,
3 => Member_Index_For_If_Expr_F_Else_Expr
         ));
      
      Node_Name_For_Inline_Eval : aliased constant Text_Type :=
        "Inline_Eval";
      Node_Desc_For_Inline_Eval : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Inline_Eval'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Inline_Eval_F_Exprs
         ));
      
      Node_Name_For_Lambda_Def : aliased constant Text_Type :=
        "Lambda_Def";
      Node_Desc_For_Lambda_Def : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Lambda_Def'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Lambda_Def_F_Args,
2 => Member_Index_For_Lambda_Def_F_Expr
         ));
      
      Node_Name_For_List_Comp : aliased constant Text_Type :=
        "List_Comp";
      Node_Desc_For_List_Comp : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_List_Comp'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_List_Comp_F_Expr,
2 => Member_Index_For_List_Comp_F_Comprehension
         ));
      
      Node_Name_For_List_Gen : aliased constant Text_Type :=
        "List_Gen";
      Node_Desc_For_List_Gen : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_List_Gen'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_List_Gen_F_Expr,
2 => Member_Index_For_List_Gen_F_Comprehension
         ));
      
      Node_Name_For_List_Lit : aliased constant Text_Type :=
        "List_Lit";
      Node_Desc_For_List_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_List_Lit'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_List_Lit_F_Exprs
         ));
      
      Node_Name_For_Name : aliased constant Text_Type :=
        "Name";
      Node_Desc_For_Name : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => True,
         Name              => Node_Name_For_Name'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 => Type_Index_For_Dotted_Name,
2 => Type_Index_For_Id
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Dotted_Name : aliased constant Text_Type :=
        "Dotted_Name";
      Node_Desc_For_Dotted_Name : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Name,
         Is_Abstract       => False,
         Name              => Node_Name_For_Dotted_Name'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Dotted_Name_F_Prefix,
2 => Member_Index_For_Dotted_Name_F_Suffix
         ));
      
      Node_Name_For_Id : aliased constant Text_Type :=
        "Id";
      Node_Desc_For_Id : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Name,
         Is_Abstract       => False,
         Name              => Node_Name_For_Id'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Not_Op : aliased constant Text_Type :=
        "Not_Op";
      Node_Desc_For_Not_Op : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Not_Op'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Not_Op_F_Expr
         ));
      
      Node_Name_For_Number_Lit : aliased constant Text_Type :=
        "Number_Lit";
      Node_Desc_For_Number_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Number_Lit'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Or_Expr : aliased constant Text_Type :=
        "Or_Expr";
      Node_Desc_For_Or_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Or_Expr'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Or_Expr_F_Left,
2 => Member_Index_For_Or_Expr_F_Right
         ));
      
      Node_Name_For_Or_Op : aliased constant Text_Type :=
        "Or_Op";
      Node_Desc_For_Or_Op : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Or_Op'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Or_Op_F_Left,
2 => Member_Index_For_Or_Op_F_Right
         ));
      
      Node_Name_For_Power : aliased constant Text_Type :=
        "Power";
      Node_Desc_For_Power : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Power'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Power_F_Left,
2 => Member_Index_For_Power_F_Right
         ));
      
      Node_Name_For_Set_Comp : aliased constant Text_Type :=
        "Set_Comp";
      Node_Desc_For_Set_Comp : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Set_Comp'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Set_Comp_F_Expr,
2 => Member_Index_For_Set_Comp_F_Comprehension
         ));
      
      Node_Name_For_Set_Lit : aliased constant Text_Type :=
        "Set_Lit";
      Node_Desc_For_Set_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Set_Lit'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Set_Lit_F_Exprs
         ));
      
      Node_Name_For_Slice_Expr : aliased constant Text_Type :=
        "Slice_Expr";
      Node_Desc_For_Slice_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 1,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Slice_Expr'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 => Type_Index_For_Ext_Slice_Expr
         ),
         Members           => (
              1 => Member_Index_For_Slice_Expr_F_First,
2 => Member_Index_For_Slice_Expr_F_Last
         ));
      
      Node_Name_For_Ext_Slice_Expr : aliased constant Text_Type :=
        "Ext_Slice_Expr";
      Node_Desc_For_Ext_Slice_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Slice_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Ext_Slice_Expr'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Ext_Slice_Expr_F_Stride
         ));
      
      Node_Name_For_String_Lit : aliased constant Text_Type :=
        "String_Lit";
      Node_Desc_For_String_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_String_Lit'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Subscript_Expr : aliased constant Text_Type :=
        "Subscript_Expr";
      Node_Desc_For_Subscript_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Subscript_Expr'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Subscript_Expr_F_Prefix,
2 => Member_Index_For_Subscript_Expr_F_Suffix
         ));
      
      Node_Name_For_Tuple_Lit : aliased constant Text_Type :=
        "Tuple_Lit";
      Node_Desc_For_Tuple_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Tuple_Lit'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Tuple_Lit_F_Exprs
         ));
      
      Node_Name_For_Xor_Expr : aliased constant Text_Type :=
        "Xor_Expr";
      Node_Desc_For_Xor_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Xor_Expr'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Xor_Expr_F_Left,
2 => Member_Index_For_Xor_Expr_F_Right
         ));
      
      Node_Name_For_Yield_Expr : aliased constant Text_Type :=
        "Yield_Expr";
      Node_Desc_For_Yield_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Name              => Node_Name_For_Yield_Expr'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Yield_Expr_F_Exprs
         ));
      
      Node_Name_For_File_Node : aliased constant Text_Type :=
        "File_Node";
      Node_Desc_For_File_Node : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => False,
         Name              => Node_Name_For_File_Node'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_File_Node_F_Statements
         ));
      
      Node_Name_For_Import_Star : aliased constant Text_Type :=
        "Import_Star";
      Node_Desc_For_Import_Star : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => False,
         Name              => Node_Name_For_Import_Star'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Kw_Args_Flag : aliased constant Text_Type :=
        "Kw_Args_Flag";
      Node_Desc_For_Kw_Args_Flag : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => True,
         Name              => Node_Name_For_Kw_Args_Flag'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 => Type_Index_For_Kw_Args_Flag_Absent,
2 => Type_Index_For_Kw_Args_Flag_Present
         ),
         Members           => (
              1 => Member_Index_For_Dispatcher_Kw_Args_Flag_P_As_Bool
         ));
      
      Node_Name_For_Kw_Args_Flag_Absent : aliased constant Text_Type :=
        "Kw_Args_Flag_Absent";
      Node_Desc_For_Kw_Args_Flag_Absent : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Kw_Args_Flag,
         Is_Abstract       => False,
         Name              => Node_Name_For_Kw_Args_Flag_Absent'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Kw_Args_Flag_Present : aliased constant Text_Type :=
        "Kw_Args_Flag_Present";
      Node_Desc_For_Kw_Args_Flag_Present : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Kw_Args_Flag,
         Is_Abstract       => False,
         Name              => Node_Name_For_Kw_Args_Flag_Present'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_NL : aliased constant Text_Type :=
        "NL";
      Node_Desc_For_NL : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => False,
         Name              => Node_Name_For_NL'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op : aliased constant Text_Type :=
        "Op";
      Node_Desc_For_Op : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => False,
         Name              => Node_Name_For_Op'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Params : aliased constant Text_Type :=
        "Params";
      Node_Desc_For_Params : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => False,
         Name              => Node_Name_For_Params'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Params_F_Single_Params
         ));
      
      Node_Name_For_Rel_Name : aliased constant Text_Type :=
        "Rel_Name";
      Node_Desc_For_Rel_Name : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => False,
         Name              => Node_Name_For_Rel_Name'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Rel_Name_F_Dots,
2 => Member_Index_For_Rel_Name_F_Name
         ));
      
      Node_Name_For_Single_Param : aliased constant Text_Type :=
        "Single_Param";
      Node_Desc_For_Single_Param : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => False,
         Name              => Node_Name_For_Single_Param'Access,
         Inherited_Members => 15,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Single_Param_F_Is_Varargs,
2 => Member_Index_For_Single_Param_F_Is_Kwargs,
3 => Member_Index_For_Single_Param_F_Name,
4 => Member_Index_For_Single_Param_F_Default_Value
         ));
      
      Node_Name_For_Stmt : aliased constant Text_Type :=
        "Stmt";
      Node_Desc_For_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 23,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => True,
         Name              => Node_Name_For_Stmt'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 => Type_Index_For_Assert_Stmt,
2 => Type_Index_For_Assign_Stmt,
3 => Type_Index_For_Aug_Assign_Stmt,
4 => Type_Index_For_Break_Stmt,
5 => Type_Index_For_Continue_Stmt,
6 => Type_Index_For_Decorated,
7 => Type_Index_For_Def_Stmt,
8 => Type_Index_For_Del_Stmt,
9 => Type_Index_For_Elif_Branch,
10 => Type_Index_For_Exec_Stmt,
11 => Type_Index_For_For_Stmt,
12 => Type_Index_For_Global_Stmt,
13 => Type_Index_For_If_Stmt,
14 => Type_Index_For_Import_From,
15 => Type_Index_For_Import_Name,
16 => Type_Index_For_Pass_Stmt,
17 => Type_Index_For_Print_Stmt,
18 => Type_Index_For_Raise_Stmt,
19 => Type_Index_For_Return_Stmt,
20 => Type_Index_For_Stream_Print_Stmt,
21 => Type_Index_For_Try_Stmt,
22 => Type_Index_For_While_Stmt,
23 => Type_Index_For_With_Stmt
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Assert_Stmt : aliased constant Text_Type :=
        "Assert_Stmt";
      Node_Desc_For_Assert_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Assert_Stmt'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Assert_Stmt_F_Test_Expr,
2 => Member_Index_For_Assert_Stmt_F_Msg
         ));
      
      Node_Name_For_Assign_Stmt : aliased constant Text_Type :=
        "Assign_Stmt";
      Node_Desc_For_Assign_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Assign_Stmt'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Assign_Stmt_F_L_Value,
2 => Member_Index_For_Assign_Stmt_F_R_Values
         ));
      
      Node_Name_For_Aug_Assign_Stmt : aliased constant Text_Type :=
        "Aug_Assign_Stmt";
      Node_Desc_For_Aug_Assign_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Aug_Assign_Stmt'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Aug_Assign_Stmt_F_L_Value,
2 => Member_Index_For_Aug_Assign_Stmt_F_Op,
3 => Member_Index_For_Aug_Assign_Stmt_F_R_Value
         ));
      
      Node_Name_For_Break_Stmt : aliased constant Text_Type :=
        "Break_Stmt";
      Node_Desc_For_Break_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Break_Stmt'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Continue_Stmt : aliased constant Text_Type :=
        "Continue_Stmt";
      Node_Desc_For_Continue_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Continue_Stmt'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Decorated : aliased constant Text_Type :=
        "Decorated";
      Node_Desc_For_Decorated : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Decorated'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decorated_F_Decorators,
2 => Member_Index_For_Decorated_F_Defn
         ));
      
      Node_Name_For_Def_Stmt : aliased constant Text_Type :=
        "Def_Stmt";
      Node_Desc_For_Def_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => True,
         Name              => Node_Name_For_Def_Stmt'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 => Type_Index_For_Class_Def,
2 => Type_Index_For_Func_Def
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Class_Def : aliased constant Text_Type :=
        "Class_Def";
      Node_Desc_For_Class_Def : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Def_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Class_Def'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Class_Def_F_Name,
2 => Member_Index_For_Class_Def_F_Bases,
3 => Member_Index_For_Class_Def_F_Statements
         ));
      
      Node_Name_For_Func_Def : aliased constant Text_Type :=
        "Func_Def";
      Node_Desc_For_Func_Def : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Def_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Func_Def'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Func_Def_F_Name,
2 => Member_Index_For_Func_Def_F_Parameters,
3 => Member_Index_For_Func_Def_F_Body
         ));
      
      Node_Name_For_Del_Stmt : aliased constant Text_Type :=
        "Del_Stmt";
      Node_Desc_For_Del_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Del_Stmt'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Del_Stmt_F_Exprs
         ));
      
      Node_Name_For_Elif_Branch : aliased constant Text_Type :=
        "Elif_Branch";
      Node_Desc_For_Elif_Branch : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Elif_Branch'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Elif_Branch_F_Cond_Test,
2 => Member_Index_For_Elif_Branch_F_Statements
         ));
      
      Node_Name_For_Exec_Stmt : aliased constant Text_Type :=
        "Exec_Stmt";
      Node_Desc_For_Exec_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Exec_Stmt'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Exec_Stmt_F_Expr,
2 => Member_Index_For_Exec_Stmt_F_In_List
         ));
      
      Node_Name_For_For_Stmt : aliased constant Text_Type :=
        "For_Stmt";
      Node_Desc_For_For_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_For_Stmt'Access,
         Inherited_Members => 15,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_For_Stmt_F_Bindings,
2 => Member_Index_For_For_Stmt_F_Expr,
3 => Member_Index_For_For_Stmt_F_Statements,
4 => Member_Index_For_For_Stmt_F_Else_Part
         ));
      
      Node_Name_For_Global_Stmt : aliased constant Text_Type :=
        "Global_Stmt";
      Node_Desc_For_Global_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Global_Stmt'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Global_Stmt_F_Names
         ));
      
      Node_Name_For_If_Stmt : aliased constant Text_Type :=
        "If_Stmt";
      Node_Desc_For_If_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_If_Stmt'Access,
         Inherited_Members => 15,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_If_Stmt_F_Cond_Test,
2 => Member_Index_For_If_Stmt_F_Statements,
3 => Member_Index_For_If_Stmt_F_Elif_Branchs,
4 => Member_Index_For_If_Stmt_F_Else_Part
         ));
      
      Node_Name_For_Import_From : aliased constant Text_Type :=
        "Import_From";
      Node_Desc_For_Import_From : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Import_From'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Import_From_F_Rel_Name,
2 => Member_Index_For_Import_From_F_Imported
         ));
      
      Node_Name_For_Import_Name : aliased constant Text_Type :=
        "Import_Name";
      Node_Desc_For_Import_Name : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Import_Name'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Import_Name_F_Imported_Names
         ));
      
      Node_Name_For_Pass_Stmt : aliased constant Text_Type :=
        "Pass_Stmt";
      Node_Desc_For_Pass_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Pass_Stmt'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Print_Stmt : aliased constant Text_Type :=
        "Print_Stmt";
      Node_Desc_For_Print_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Print_Stmt'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Print_Stmt_F_Exprs
         ));
      
      Node_Name_For_Raise_Stmt : aliased constant Text_Type :=
        "Raise_Stmt";
      Node_Desc_For_Raise_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Raise_Stmt'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Raise_Stmt_F_Exprs
         ));
      
      Node_Name_For_Return_Stmt : aliased constant Text_Type :=
        "Return_Stmt";
      Node_Desc_For_Return_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Return_Stmt'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Return_Stmt_F_Exprs
         ));
      
      Node_Name_For_Stream_Print_Stmt : aliased constant Text_Type :=
        "Stream_Print_Stmt";
      Node_Desc_For_Stream_Print_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Stream_Print_Stmt'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Stream_Print_Stmt_F_Stream_Expr,
2 => Member_Index_For_Stream_Print_Stmt_F_Exprs
         ));
      
      Node_Name_For_Try_Stmt : aliased constant Text_Type :=
        "Try_Stmt";
      Node_Desc_For_Try_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_Try_Stmt'Access,
         Inherited_Members => 15,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Try_Stmt_F_Statements,
2 => Member_Index_For_Try_Stmt_F_Except_Parts,
3 => Member_Index_For_Try_Stmt_F_Else_Part,
4 => Member_Index_For_Try_Stmt_F_Finally_Part
         ));
      
      Node_Name_For_While_Stmt : aliased constant Text_Type :=
        "While_Stmt";
      Node_Desc_For_While_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_While_Stmt'Access,
         Inherited_Members => 14,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_While_Stmt_F_Cond_Test,
2 => Member_Index_For_While_Stmt_F_Statements,
3 => Member_Index_For_While_Stmt_F_Else_Part
         ));
      
      Node_Name_For_With_Stmt : aliased constant Text_Type :=
        "With_Stmt";
      Node_Desc_For_With_Stmt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Stmt,
         Is_Abstract       => False,
         Name              => Node_Name_For_With_Stmt'Access,
         Inherited_Members => 13,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_With_Stmt_F_Bindings,
2 => Member_Index_For_With_Stmt_F_Statements
         ));
      
      Node_Name_For_Turkixir_Node_Base_List : aliased constant Text_Type :=
        "Turkixir_Node_Base_List";
      Node_Desc_For_Turkixir_Node_Base_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 13,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => True,
         Name              => Node_Name_For_Turkixir_Node_Base_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 => Type_Index_For_Arg_List,
2 => Type_Index_For_As_Name_Node_List,
3 => Type_Index_For_Decorator_List,
4 => Type_Index_For_Dict_Assoc_List,
5 => Type_Index_For_Dot_List,
6 => Type_Index_For_Elif_Branch_List,
7 => Type_Index_For_Except_Part_List,
8 => Type_Index_For_Expr_List,
9 => Type_Index_For_Id_List,
10 => Type_Index_For_NL_List,
11 => Type_Index_For_Single_Param_List,
12 => Type_Index_For_String_Lit_List,
13 => Type_Index_For_Turkixir_Node_List
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Arg_List : aliased constant Text_Type :=
        "Arg_List";
      Node_Desc_For_Arg_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node_Base_List,
         Is_Abstract       => False,
         Name              => Node_Name_For_Arg_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_As_Name_Node_List : aliased constant Text_Type :=
        "As_Name_Node_List";
      Node_Desc_For_As_Name_Node_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node_Base_List,
         Is_Abstract       => False,
         Name              => Node_Name_For_As_Name_Node_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Decorator_List : aliased constant Text_Type :=
        "Decorator_List";
      Node_Desc_For_Decorator_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node_Base_List,
         Is_Abstract       => False,
         Name              => Node_Name_For_Decorator_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Dict_Assoc_List : aliased constant Text_Type :=
        "Dict_Assoc_List";
      Node_Desc_For_Dict_Assoc_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node_Base_List,
         Is_Abstract       => False,
         Name              => Node_Name_For_Dict_Assoc_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Dot_List : aliased constant Text_Type :=
        "Dot_List";
      Node_Desc_For_Dot_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node_Base_List,
         Is_Abstract       => False,
         Name              => Node_Name_For_Dot_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Elif_Branch_List : aliased constant Text_Type :=
        "Elif_Branch_List";
      Node_Desc_For_Elif_Branch_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node_Base_List,
         Is_Abstract       => False,
         Name              => Node_Name_For_Elif_Branch_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Except_Part_List : aliased constant Text_Type :=
        "Except_Part_List";
      Node_Desc_For_Except_Part_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node_Base_List,
         Is_Abstract       => False,
         Name              => Node_Name_For_Except_Part_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Expr_List : aliased constant Text_Type :=
        "Expr_List";
      Node_Desc_For_Expr_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node_Base_List,
         Is_Abstract       => False,
         Name              => Node_Name_For_Expr_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Id_List : aliased constant Text_Type :=
        "Id_List";
      Node_Desc_For_Id_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node_Base_List,
         Is_Abstract       => False,
         Name              => Node_Name_For_Id_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_NL_List : aliased constant Text_Type :=
        "NL_List";
      Node_Desc_For_NL_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node_Base_List,
         Is_Abstract       => False,
         Name              => Node_Name_For_NL_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Single_Param_List : aliased constant Text_Type :=
        "Single_Param_List";
      Node_Desc_For_Single_Param_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node_Base_List,
         Is_Abstract       => False,
         Name              => Node_Name_For_Single_Param_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_String_Lit_List : aliased constant Text_Type :=
        "String_Lit_List";
      Node_Desc_For_String_Lit_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node_Base_List,
         Is_Abstract       => False,
         Name              => Node_Name_For_String_Lit_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Turkixir_Node_List : aliased constant Text_Type :=
        "Turkixir_Node_List";
      Node_Desc_For_Turkixir_Node_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Turkixir_Node_Base_List,
         Is_Abstract       => False,
         Name              => Node_Name_For_Turkixir_Node_List'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Var_Args_Flag : aliased constant Text_Type :=
        "Var_Args_Flag";
      Node_Desc_For_Var_Args_Flag : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Turkixir_Node,
         Is_Abstract       => True,
         Name              => Node_Name_For_Var_Args_Flag'Access,
         Inherited_Members => 12,
         Derivations       => (
             1 => Type_Index_For_Var_Args_Flag_Absent,
2 => Type_Index_For_Var_Args_Flag_Present
         ),
         Members           => (
              1 => Member_Index_For_Dispatcher_Var_Args_Flag_P_As_Bool
         ));
      
      Node_Name_For_Var_Args_Flag_Absent : aliased constant Text_Type :=
        "Var_Args_Flag_Absent";
      Node_Desc_For_Var_Args_Flag_Absent : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Var_Args_Flag,
         Is_Abstract       => False,
         Name              => Node_Name_For_Var_Args_Flag_Absent'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Var_Args_Flag_Present : aliased constant Text_Type :=
        "Var_Args_Flag_Present";
      Node_Desc_For_Var_Args_Flag_Present : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Var_Args_Flag,
         Is_Abstract       => False,
         Name              => Node_Name_For_Var_Args_Flag_Present'Access,
         Inherited_Members => 11,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));

   Struct_Types : aliased constant Struct_Type_Descriptor_Array := (
      Type_Index_For_Turkixir_Node => Node_Desc_For_Turkixir_Node'Access,
Type_Index_For_Arg => Node_Desc_For_Arg'Access,
Type_Index_For_Arg_Assoc => Node_Desc_For_Arg_Assoc'Access,
Type_Index_For_Arg_Gen => Node_Desc_For_Arg_Gen'Access,
Type_Index_For_Kw_Args => Node_Desc_For_Kw_Args'Access,
Type_Index_For_Var_Args => Node_Desc_For_Var_Args'Access,
Type_Index_For_As_Name_Node => Node_Desc_For_As_Name_Node'Access,
Type_Index_For_Comp_If => Node_Desc_For_Comp_If'Access,
Type_Index_For_Comp_Op_Kind => Node_Desc_For_Comp_Op_Kind'Access,
Type_Index_For_Comp_Op_Kind_Diamond => Node_Desc_For_Comp_Op_Kind_Diamond'Access,
Type_Index_For_Comp_Op_Kind_Eq => Node_Desc_For_Comp_Op_Kind_Eq'Access,
Type_Index_For_Comp_Op_Kind_Gt => Node_Desc_For_Comp_Op_Kind_Gt'Access,
Type_Index_For_Comp_Op_Kind_Gte => Node_Desc_For_Comp_Op_Kind_Gte'Access,
Type_Index_For_Comp_Op_Kind_In => Node_Desc_For_Comp_Op_Kind_In'Access,
Type_Index_For_Comp_Op_Kind_Is => Node_Desc_For_Comp_Op_Kind_Is'Access,
Type_Index_For_Comp_Op_Kind_Isnot => Node_Desc_For_Comp_Op_Kind_Isnot'Access,
Type_Index_For_Comp_Op_Kind_Lt => Node_Desc_For_Comp_Op_Kind_Lt'Access,
Type_Index_For_Comp_Op_Kind_Lte => Node_Desc_For_Comp_Op_Kind_Lte'Access,
Type_Index_For_Comp_Op_Kind_Noteq => Node_Desc_For_Comp_Op_Kind_Noteq'Access,
Type_Index_For_Comp_Op_Kind_Notin => Node_Desc_For_Comp_Op_Kind_Notin'Access,
Type_Index_For_Comprehension => Node_Desc_For_Comprehension'Access,
Type_Index_For_Comp_For => Node_Desc_For_Comp_For'Access,
Type_Index_For_Comp_ForL => Node_Desc_For_Comp_ForL'Access,
Type_Index_For_Decorator => Node_Desc_For_Decorator'Access,
Type_Index_For_Dict_Assoc => Node_Desc_For_Dict_Assoc'Access,
Type_Index_For_Else_Part => Node_Desc_For_Else_Part'Access,
Type_Index_For_Except_Part => Node_Desc_For_Except_Part'Access,
Type_Index_For_Expr => Node_Desc_For_Expr'Access,
Type_Index_For_And_Expr => Node_Desc_For_And_Expr'Access,
Type_Index_For_And_Op => Node_Desc_For_And_Op'Access,
Type_Index_For_Bin_Op => Node_Desc_For_Bin_Op'Access,
Type_Index_For_Arith_Expr => Node_Desc_For_Arith_Expr'Access,
Type_Index_For_Shift_Expr => Node_Desc_For_Shift_Expr'Access,
Type_Index_For_Term => Node_Desc_For_Term'Access,
Type_Index_For_Call_Expr => Node_Desc_For_Call_Expr'Access,
Type_Index_For_Comp_Op => Node_Desc_For_Comp_Op'Access,
Type_Index_For_Concat_String_Lit => Node_Desc_For_Concat_String_Lit'Access,
Type_Index_For_Dict_Comp => Node_Desc_For_Dict_Comp'Access,
Type_Index_For_Dict_Lit => Node_Desc_For_Dict_Lit'Access,
Type_Index_For_Dot => Node_Desc_For_Dot'Access,
Type_Index_For_Ellipsis_Expr => Node_Desc_For_Ellipsis_Expr'Access,
Type_Index_For_Factor => Node_Desc_For_Factor'Access,
Type_Index_For_If_Expr => Node_Desc_For_If_Expr'Access,
Type_Index_For_Inline_Eval => Node_Desc_For_Inline_Eval'Access,
Type_Index_For_Lambda_Def => Node_Desc_For_Lambda_Def'Access,
Type_Index_For_List_Comp => Node_Desc_For_List_Comp'Access,
Type_Index_For_List_Gen => Node_Desc_For_List_Gen'Access,
Type_Index_For_List_Lit => Node_Desc_For_List_Lit'Access,
Type_Index_For_Name => Node_Desc_For_Name'Access,
Type_Index_For_Dotted_Name => Node_Desc_For_Dotted_Name'Access,
Type_Index_For_Id => Node_Desc_For_Id'Access,
Type_Index_For_Not_Op => Node_Desc_For_Not_Op'Access,
Type_Index_For_Number_Lit => Node_Desc_For_Number_Lit'Access,
Type_Index_For_Or_Expr => Node_Desc_For_Or_Expr'Access,
Type_Index_For_Or_Op => Node_Desc_For_Or_Op'Access,
Type_Index_For_Power => Node_Desc_For_Power'Access,
Type_Index_For_Set_Comp => Node_Desc_For_Set_Comp'Access,
Type_Index_For_Set_Lit => Node_Desc_For_Set_Lit'Access,
Type_Index_For_Slice_Expr => Node_Desc_For_Slice_Expr'Access,
Type_Index_For_Ext_Slice_Expr => Node_Desc_For_Ext_Slice_Expr'Access,
Type_Index_For_String_Lit => Node_Desc_For_String_Lit'Access,
Type_Index_For_Subscript_Expr => Node_Desc_For_Subscript_Expr'Access,
Type_Index_For_Tuple_Lit => Node_Desc_For_Tuple_Lit'Access,
Type_Index_For_Xor_Expr => Node_Desc_For_Xor_Expr'Access,
Type_Index_For_Yield_Expr => Node_Desc_For_Yield_Expr'Access,
Type_Index_For_File_Node => Node_Desc_For_File_Node'Access,
Type_Index_For_Import_Star => Node_Desc_For_Import_Star'Access,
Type_Index_For_Kw_Args_Flag => Node_Desc_For_Kw_Args_Flag'Access,
Type_Index_For_Kw_Args_Flag_Absent => Node_Desc_For_Kw_Args_Flag_Absent'Access,
Type_Index_For_Kw_Args_Flag_Present => Node_Desc_For_Kw_Args_Flag_Present'Access,
Type_Index_For_NL => Node_Desc_For_NL'Access,
Type_Index_For_Op => Node_Desc_For_Op'Access,
Type_Index_For_Params => Node_Desc_For_Params'Access,
Type_Index_For_Rel_Name => Node_Desc_For_Rel_Name'Access,
Type_Index_For_Single_Param => Node_Desc_For_Single_Param'Access,
Type_Index_For_Stmt => Node_Desc_For_Stmt'Access,
Type_Index_For_Assert_Stmt => Node_Desc_For_Assert_Stmt'Access,
Type_Index_For_Assign_Stmt => Node_Desc_For_Assign_Stmt'Access,
Type_Index_For_Aug_Assign_Stmt => Node_Desc_For_Aug_Assign_Stmt'Access,
Type_Index_For_Break_Stmt => Node_Desc_For_Break_Stmt'Access,
Type_Index_For_Continue_Stmt => Node_Desc_For_Continue_Stmt'Access,
Type_Index_For_Decorated => Node_Desc_For_Decorated'Access,
Type_Index_For_Def_Stmt => Node_Desc_For_Def_Stmt'Access,
Type_Index_For_Class_Def => Node_Desc_For_Class_Def'Access,
Type_Index_For_Func_Def => Node_Desc_For_Func_Def'Access,
Type_Index_For_Del_Stmt => Node_Desc_For_Del_Stmt'Access,
Type_Index_For_Elif_Branch => Node_Desc_For_Elif_Branch'Access,
Type_Index_For_Exec_Stmt => Node_Desc_For_Exec_Stmt'Access,
Type_Index_For_For_Stmt => Node_Desc_For_For_Stmt'Access,
Type_Index_For_Global_Stmt => Node_Desc_For_Global_Stmt'Access,
Type_Index_For_If_Stmt => Node_Desc_For_If_Stmt'Access,
Type_Index_For_Import_From => Node_Desc_For_Import_From'Access,
Type_Index_For_Import_Name => Node_Desc_For_Import_Name'Access,
Type_Index_For_Pass_Stmt => Node_Desc_For_Pass_Stmt'Access,
Type_Index_For_Print_Stmt => Node_Desc_For_Print_Stmt'Access,
Type_Index_For_Raise_Stmt => Node_Desc_For_Raise_Stmt'Access,
Type_Index_For_Return_Stmt => Node_Desc_For_Return_Stmt'Access,
Type_Index_For_Stream_Print_Stmt => Node_Desc_For_Stream_Print_Stmt'Access,
Type_Index_For_Try_Stmt => Node_Desc_For_Try_Stmt'Access,
Type_Index_For_While_Stmt => Node_Desc_For_While_Stmt'Access,
Type_Index_For_With_Stmt => Node_Desc_For_With_Stmt'Access,
Type_Index_For_Turkixir_Node_Base_List => Node_Desc_For_Turkixir_Node_Base_List'Access,
Type_Index_For_Arg_List => Node_Desc_For_Arg_List'Access,
Type_Index_For_As_Name_Node_List => Node_Desc_For_As_Name_Node_List'Access,
Type_Index_For_Decorator_List => Node_Desc_For_Decorator_List'Access,
Type_Index_For_Dict_Assoc_List => Node_Desc_For_Dict_Assoc_List'Access,
Type_Index_For_Dot_List => Node_Desc_For_Dot_List'Access,
Type_Index_For_Elif_Branch_List => Node_Desc_For_Elif_Branch_List'Access,
Type_Index_For_Except_Part_List => Node_Desc_For_Except_Part_List'Access,
Type_Index_For_Expr_List => Node_Desc_For_Expr_List'Access,
Type_Index_For_Id_List => Node_Desc_For_Id_List'Access,
Type_Index_For_NL_List => Node_Desc_For_NL_List'Access,
Type_Index_For_Single_Param_List => Node_Desc_For_Single_Param_List'Access,
Type_Index_For_String_Lit_List => Node_Desc_For_String_Lit_List'Access,
Type_Index_For_Turkixir_Node_List => Node_Desc_For_Turkixir_Node_List'Access,
Type_Index_For_Var_Args_Flag => Node_Desc_For_Var_Args_Flag'Access,
Type_Index_For_Var_Args_Flag_Absent => Node_Desc_For_Var_Args_Flag_Absent'Access,
Type_Index_For_Var_Args_Flag_Present => Node_Desc_For_Var_Args_Flag_Present'Access
   );

   First_Node     : constant Type_Index := Type_Index_For_Turkixir_Node;
   First_Property : constant Struct_Member_Index :=
     Member_Index_For_Parent;

end Libturkixirlang.Generic_Introspection;
