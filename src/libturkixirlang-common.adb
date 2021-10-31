
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with GNATCOLL.Iconv;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Libturkixirlang.Implementation; use Libturkixirlang.Implementation;
with Libturkixirlang.Lexer_Implementation;
use Libturkixirlang.Lexer_Implementation;
with Libturkixirlang.Private_Converters;



package body Libturkixirlang.Common is

   Is_Token_Node_Kind : constant array (Turkixir_Node_Kind_Type) of Boolean :=
     (Turkixir_Arg_Assoc => False, Turkixir_Arg_Gen => False, Turkixir_Kw_Args => False, Turkixir_Var_Args => False, Turkixir_As_Name_Node => False, Turkixir_Comp_If => False, Turkixir_Comp_Op_Kind_Diamond => False, Turkixir_Comp_Op_Kind_Eq => False, Turkixir_Comp_Op_Kind_Gt => False, Turkixir_Comp_Op_Kind_Gte => False, Turkixir_Comp_Op_Kind_In => False, Turkixir_Comp_Op_Kind_Is => False, Turkixir_Comp_Op_Kind_Isnot => False, Turkixir_Comp_Op_Kind_Lt => False, Turkixir_Comp_Op_Kind_Lte => False, Turkixir_Comp_Op_Kind_Noteq => False, Turkixir_Comp_Op_Kind_Notin => False, Turkixir_Comp_For => False, Turkixir_Comp_ForL => False, Turkixir_Decorator => False, Turkixir_Dict_Assoc => False, Turkixir_Else_Part => False, Turkixir_Except_Part => False, Turkixir_And_Expr => False, Turkixir_And_Op => False, Turkixir_Arith_Expr => False, Turkixir_Shift_Expr => False, Turkixir_Term => False, Turkixir_Call_Expr => False, Turkixir_Comp_Op => False, Turkixir_Concat_String_Lit => False, Turkixir_Dict_Comp => False, Turkixir_Dict_Lit => False, Turkixir_Dot => False, Turkixir_Ellipsis_Expr => False, Turkixir_Factor => False, Turkixir_If_Expr => False, Turkixir_Inline_Eval => False, Turkixir_Lambda_Def => False, Turkixir_List_Comp => False, Turkixir_List_Gen => False, Turkixir_List_Lit => False, Turkixir_Dotted_Name => False, Turkixir_Id => True, Turkixir_Not_Op => False, Turkixir_Number_Lit => True, Turkixir_Or_Expr => False, Turkixir_Or_Op => False, Turkixir_Power => False, Turkixir_Set_Comp => False, Turkixir_Set_Lit => False, Turkixir_Slice_Expr => False, Turkixir_Ext_Slice_Expr => False, Turkixir_String_Lit => True, Turkixir_Subscript_Expr => False, Turkixir_Tuple_Lit => False, Turkixir_Xor_Expr => False, Turkixir_Yield_Expr => False, Turkixir_File_Node => False, Turkixir_Import_Star => False, Turkixir_Kw_Args_Flag_Absent => False, Turkixir_Kw_Args_Flag_Present => False, Turkixir_NL => False, Turkixir_Op => True, Turkixir_Params => False, Turkixir_Rel_Name => False, Turkixir_Single_Param => False, Turkixir_Assert_Stmt => False, Turkixir_Assign_Stmt => False, Turkixir_Aug_Assign_Stmt => False, Turkixir_Break_Stmt => False, Turkixir_Continue_Stmt => False, Turkixir_Decorated => False, Turkixir_Class_Def => False, Turkixir_Func_Def => False, Turkixir_Del_Stmt => False, Turkixir_Elif_Branch => False, Turkixir_Exec_Stmt => False, Turkixir_For_Stmt => False, Turkixir_Global_Stmt => False, Turkixir_If_Stmt => False, Turkixir_Import_From => False, Turkixir_Import_Name => False, Turkixir_Pass_Stmt => False, Turkixir_Print_Stmt => False, Turkixir_Raise_Stmt => False, Turkixir_Return_Stmt => False, Turkixir_Stream_Print_Stmt => False, Turkixir_Try_Stmt => False, Turkixir_While_Stmt => False, Turkixir_With_Stmt => False, Turkixir_Arg_List => False, Turkixir_As_Name_Node_List => False, Turkixir_Decorator_List => False, Turkixir_Dict_Assoc_List => False, Turkixir_Dot_List => False, Turkixir_Elif_Branch_List => False, Turkixir_Except_Part_List => False, Turkixir_Expr_List => False, Turkixir_Id_List => False, Turkixir_NL_List => False, Turkixir_Single_Param_List => False, Turkixir_String_Lit_List => False, Turkixir_Turkixir_Node_List => False, Turkixir_Var_Args_Flag_Absent => False, Turkixir_Var_Args_Flag_Present => False);
   --  For each node kind, return whether it is a node that contains only a
   --  single token.

   Is_Error_Node_Kind : constant array (Turkixir_Node_Kind_Type) of Boolean :=
     (Turkixir_Arg_Assoc => False, Turkixir_Arg_Gen => False, Turkixir_Kw_Args => False, Turkixir_Var_Args => False, Turkixir_As_Name_Node => False, Turkixir_Comp_If => False, Turkixir_Comp_Op_Kind_Diamond => False, Turkixir_Comp_Op_Kind_Eq => False, Turkixir_Comp_Op_Kind_Gt => False, Turkixir_Comp_Op_Kind_Gte => False, Turkixir_Comp_Op_Kind_In => False, Turkixir_Comp_Op_Kind_Is => False, Turkixir_Comp_Op_Kind_Isnot => False, Turkixir_Comp_Op_Kind_Lt => False, Turkixir_Comp_Op_Kind_Lte => False, Turkixir_Comp_Op_Kind_Noteq => False, Turkixir_Comp_Op_Kind_Notin => False, Turkixir_Comp_For => False, Turkixir_Comp_ForL => False, Turkixir_Decorator => False, Turkixir_Dict_Assoc => False, Turkixir_Else_Part => False, Turkixir_Except_Part => False, Turkixir_And_Expr => False, Turkixir_And_Op => False, Turkixir_Arith_Expr => False, Turkixir_Shift_Expr => False, Turkixir_Term => False, Turkixir_Call_Expr => False, Turkixir_Comp_Op => False, Turkixir_Concat_String_Lit => False, Turkixir_Dict_Comp => False, Turkixir_Dict_Lit => False, Turkixir_Dot => False, Turkixir_Ellipsis_Expr => False, Turkixir_Factor => False, Turkixir_If_Expr => False, Turkixir_Inline_Eval => False, Turkixir_Lambda_Def => False, Turkixir_List_Comp => False, Turkixir_List_Gen => False, Turkixir_List_Lit => False, Turkixir_Dotted_Name => False, Turkixir_Id => False, Turkixir_Not_Op => False, Turkixir_Number_Lit => False, Turkixir_Or_Expr => False, Turkixir_Or_Op => False, Turkixir_Power => False, Turkixir_Set_Comp => False, Turkixir_Set_Lit => False, Turkixir_Slice_Expr => False, Turkixir_Ext_Slice_Expr => False, Turkixir_String_Lit => False, Turkixir_Subscript_Expr => False, Turkixir_Tuple_Lit => False, Turkixir_Xor_Expr => False, Turkixir_Yield_Expr => False, Turkixir_File_Node => False, Turkixir_Import_Star => False, Turkixir_Kw_Args_Flag_Absent => False, Turkixir_Kw_Args_Flag_Present => False, Turkixir_NL => False, Turkixir_Op => False, Turkixir_Params => False, Turkixir_Rel_Name => False, Turkixir_Single_Param => False, Turkixir_Assert_Stmt => False, Turkixir_Assign_Stmt => False, Turkixir_Aug_Assign_Stmt => False, Turkixir_Break_Stmt => False, Turkixir_Continue_Stmt => False, Turkixir_Decorated => False, Turkixir_Class_Def => False, Turkixir_Func_Def => False, Turkixir_Del_Stmt => False, Turkixir_Elif_Branch => False, Turkixir_Exec_Stmt => False, Turkixir_For_Stmt => False, Turkixir_Global_Stmt => False, Turkixir_If_Stmt => False, Turkixir_Import_From => False, Turkixir_Import_Name => False, Turkixir_Pass_Stmt => False, Turkixir_Print_Stmt => False, Turkixir_Raise_Stmt => False, Turkixir_Return_Stmt => False, Turkixir_Stream_Print_Stmt => False, Turkixir_Try_Stmt => False, Turkixir_While_Stmt => False, Turkixir_With_Stmt => False, Turkixir_Arg_List => False, Turkixir_As_Name_Node_List => False, Turkixir_Decorator_List => False, Turkixir_Dict_Assoc_List => False, Turkixir_Dot_List => False, Turkixir_Elif_Branch_List => False, Turkixir_Except_Part_List => False, Turkixir_Expr_List => False, Turkixir_Id_List => False, Turkixir_NL_List => False, Turkixir_Single_Param_List => False, Turkixir_String_Lit_List => False, Turkixir_Turkixir_Node_List => False, Turkixir_Var_Args_Flag_Absent => False, Turkixir_Var_Args_Flag_Present => False);
   --  For each node kind, return whether it is an error node

   function Wrap_Token_Reference
     (Context : Internal_Context;
      TDH     : Token_Data_Handler_Access;
      Index   : Token_Or_Trivia_Index) return Token_Reference;
   function Get_Token_Context
     (Token : Token_Reference) return Internal_Context;
   function Get_Token_TDH
     (Token : Token_Reference) return Token_Data_Handler_Access;
   function Get_Token_Index
     (Token : Token_Reference) return Token_Or_Trivia_Index;
   procedure Extract_Token_Text
     (Token         : Token_Data_Type;
      Source_Buffer : out Text_Cst_Access;
      First         : out Positive;
      Last          : out Natural);
   --  Implementations for converters soft-links

   function "+" is new Ada.Unchecked_Conversion
     (Langkit_Support.Internal.Analysis.Internal_Context, Internal_Context);
   function "+" is new Ada.Unchecked_Conversion
     (Internal_Context, Langkit_Support.Internal.Analysis.Internal_Context);

   function Rewrap_Token
     (Origin : Token_Reference;
      Index  : Token_Or_Trivia_Index) return Token_Reference;
   --  Create a token reference for ``Index`` using the token data handler
   --  reference from ``Origin``.

   Token_Kind_To_Literals : constant array (Token_Kind) of Text_Access := (
   

         Turkixir_T_T__Rsh_Assign => new Text_Type'(">>="),
         
         Turkixir_T_T__Is => new Text_Type'("is"),
         
         Turkixir_T_T__Equals => new Text_Type'("=="),
         
         Turkixir_T_T__Def => new Text_Type'("def"),
         
         Turkixir_T_T__Lte => new Text_Type'("<="),
         
         Turkixir_T_T__Raise => new Text_Type'("raise"),
         
         Turkixir_T_T__Mod => new Text_Type'("%"),
         
         Turkixir_T_T__Yield => new Text_Type'("yield"),
         
         Turkixir_T_T__Xor_Assign => new Text_Type'("^="),
         
         Turkixir_T_T__As => new Text_Type'("as"),
         
         Turkixir_T_T__Lambda => new Text_Type'("lambda"),
         
         Turkixir_T_T__Backtick => new Text_Type'("`"),
         
         Turkixir_T_T__Try => new Text_Type'("try"),
         
         Turkixir_T_T__Divide => new Text_Type'("/"),
         
         Turkixir_T_T__Invert => new Text_Type'("~"),
         
         Turkixir_T_T__Return => new Text_Type'("return"),
         
         Turkixir_T_T__Assert => new Text_Type'("assert"),
         
         Turkixir_T_T__Xor => new Text_Type'("^"),
         
         Turkixir_T_T__Break => new Text_Type'("break"),
         
         Turkixir_T_T__Rbrack => new Text_Type'("]"),
         
         Turkixir_T_T__Power_Assign => new Text_Type'("**="),
         
         Turkixir_T_T__Import => new Text_Type'("import"),
         
         Turkixir_T_T__Exec => new Text_Type'("exec"),
         
         Turkixir_T_T__Comma => new Text_Type'(","),
         
         Turkixir_T_T_L_Par => new Text_Type'("("),
         
         Turkixir_T_T__Dot => new Text_Type'("."),
         
         Turkixir_T_T__Gte => new Text_Type'(">="),
         
         Turkixir_T_T__Floordiv_Assign => new Text_Type'("//="),
         
         Turkixir_T_T__Multiply => new Text_Type'("*"),
         
         Turkixir_T_T__Div_Assign => new Text_Type'("/="),
         
         Turkixir_T_T__At => new Text_Type'("@"),
         
         Turkixir_T_T__Assign => new Text_Type'("="),
         
         Turkixir_T_T__Floordiv => new Text_Type'("//"),
         
         Turkixir_T_T__Notequal => new Text_Type'("!="),
         
         Turkixir_T_T__Mult_Assign => new Text_Type'("*="),
         
         Turkixir_T_T__Mod_Assign => new Text_Type'("%="),
         
         Turkixir_T_T__Gt => new Text_Type'(">"),
         
         Turkixir_T_T__Power => new Text_Type'("**"),
         
         Turkixir_T_T__Amp => new Text_Type'("&"),
         
         Turkixir_T_T__Not => new Text_Type'("not"),
         
         Turkixir_T_T__Colon => new Text_Type'(":"),
         
         Turkixir_T_T__Diamond => new Text_Type'("<>"),
         
         Turkixir_T_T__In => new Text_Type'("in"),
         
         Turkixir_T_T_L_Curl => new Text_Type'("{"),
         
         Turkixir_T_T__Class => new Text_Type'("class"),
         
         Turkixir_T_T__Or_Assign => new Text_Type'("|="),
         
         Turkixir_T_T__Elif => new Text_Type'("elif"),
         
         Turkixir_T_T__And => new Text_Type'("and"),
         
         Turkixir_T_T__Semicolon => new Text_Type'(";"),
         
         Turkixir_T_T__Add_Asign => new Text_Type'("+="),
         
         Turkixir_T_T__Print => new Text_Type'("print"),
         
         Turkixir_T_T__Lsh => new Text_Type'("<<"),
         
         Turkixir_T_T__Continue => new Text_Type'("continue"),
         
         Turkixir_T_T__While => new Text_Type'("while"),
         
         Turkixir_T_T__Except => new Text_Type'("catch"),
         
         Turkixir_T_T__If => new Text_Type'("if"),
         
         Turkixir_T_T__Else => new Text_Type'("else"),
         
         Turkixir_T_T__Del => new Text_Type'("del"),
         
         Turkixir_T_T__Minus_Assign => new Text_Type'("-="),
         
         Turkixir_T_T__Or => new Text_Type'("or"),
         
         Turkixir_T_T__Minus => new Text_Type'("-"),
         
         Turkixir_T_T__Lbrack => new Text_Type'("["),
         
         Turkixir_T_T__And_Assign => new Text_Type'("&="),
         
         Turkixir_T_T_R_Par => new Text_Type'(")"),
         
         Turkixir_T_T__Global => new Text_Type'("global"),
         
         Turkixir_T_T__For => new Text_Type'("for"),
         
         Turkixir_T_T__From => new Text_Type'("from"),
         
         Turkixir_T_T__Rsh => new Text_Type'(">>"),
         
         Turkixir_T_T__Finally => new Text_Type'("finally"),
         
         Turkixir_T_T__Pass => new Text_Type'("pass"),
         
         Turkixir_T_T__Lsh_Assign => new Text_Type'("<<="),
         
         Turkixir_T_T__Bin_Or => new Text_Type'("|"),
         
         Turkixir_T_T__Rcurl => new Text_Type'("}"),
         
         Turkixir_T_T__With => new Text_Type'("with"),
         
         Turkixir_T_T__Plus => new Text_Type'("+"),
         
         Turkixir_T_T__Lt => new Text_Type'("<"),
         
      others => new Text_Type'("")
   );

   Token_Kind_Names : constant array (Token_Kind) of String_Access := (
          Turkixir_T_T__Rsh_Assign =>
             new String'("T_T__Rsh_Assign")
              ,
          Turkixir_T_T__Is =>
             new String'("T_T__Is")
              ,
          Turkixir_T_T__Equals =>
             new String'("T_T__Equals")
              ,
          Turkixir_T_T__Def =>
             new String'("T_T__Def")
              ,
          Turkixir_T_T__Lte =>
             new String'("T_T__Lte")
              ,
          Turkixir_T_T__Raise =>
             new String'("T_T__Raise")
              ,
          Turkixir_T_T__Mod =>
             new String'("T_T__Mod")
              ,
          Turkixir_T_T__Yield =>
             new String'("T_T__Yield")
              ,
          Turkixir_T_T__Xor_Assign =>
             new String'("T_T__Xor_Assign")
              ,
          Turkixir_T_T__As =>
             new String'("T_T__As")
              ,
          Turkixir_T_T__Lambda =>
             new String'("T_T__Lambda")
              ,
          Turkixir_T_T__Backtick =>
             new String'("T_T__Backtick")
              ,
          Turkixir_T_T__Try =>
             new String'("T_T__Try")
              ,
          Turkixir_T_T__Divide =>
             new String'("T_T__Divide")
              ,
          Turkixir_T_T__Invert =>
             new String'("T_T__Invert")
              ,
          Turkixir_T_T__Return =>
             new String'("T_T__Return")
              ,
          Turkixir_T_T__Assert =>
             new String'("T_T__Assert")
              ,
          Turkixir_T_T__Xor =>
             new String'("T_T__Xor")
              ,
          Turkixir_T_T__Break =>
             new String'("T_T__Break")
              ,
          Turkixir_T_T__Rbrack =>
             new String'("T_T__Rbrack")
              ,
          Turkixir_T_T__Power_Assign =>
             new String'("T_T__Power_Assign")
              ,
          Turkixir_T_T__Import =>
             new String'("T_T__Import")
              ,
          Turkixir_T_T__Exec =>
             new String'("T_T__Exec")
              ,
          Turkixir_T_T__Comma =>
             new String'("T_T__Comma")
              ,
          Turkixir_T_T_L_Par =>
             new String'("T_T_L_Par")
              ,
          Turkixir_T_T__Dot =>
             new String'("T_T__Dot")
              ,
          Turkixir_T_T__Gte =>
             new String'("T_T__Gte")
              ,
          Turkixir_T_T__Floordiv_Assign =>
             new String'("T_T__Floordiv_Assign")
              ,
          Turkixir_T_T__Multiply =>
             new String'("T_T__Multiply")
              ,
          Turkixir_T_T__Div_Assign =>
             new String'("T_T__Div_Assign")
              ,
          Turkixir_T_T__At =>
             new String'("T_T__At")
              ,
          Turkixir_T_T__Assign =>
             new String'("T_T__Assign")
              ,
          Turkixir_T_T__Floordiv =>
             new String'("T_T__Floordiv")
              ,
          Turkixir_T_T__Notequal =>
             new String'("T_T__Notequal")
              ,
          Turkixir_T_T__Mult_Assign =>
             new String'("T_T__Mult_Assign")
              ,
          Turkixir_T_T__Mod_Assign =>
             new String'("T_T__Mod_Assign")
              ,
          Turkixir_T_T__Gt =>
             new String'("T_T__Gt")
              ,
          Turkixir_T_T__Power =>
             new String'("T_T__Power")
              ,
          Turkixir_T_T__Amp =>
             new String'("T_T__Amp")
              ,
          Turkixir_T_T__Not =>
             new String'("T_T__Not")
              ,
          Turkixir_T_T__Colon =>
             new String'("T_T__Colon")
              ,
          Turkixir_T_T__Diamond =>
             new String'("T_T__Diamond")
              ,
          Turkixir_T_T__In =>
             new String'("T_T__In")
              ,
          Turkixir_T_T_L_Curl =>
             new String'("T_T_L_Curl")
              ,
          Turkixir_T_T__Class =>
             new String'("T_T__Class")
              ,
          Turkixir_T_T__Or_Assign =>
             new String'("T_T__Or_Assign")
              ,
          Turkixir_T_T__Elif =>
             new String'("T_T__Elif")
              ,
          Turkixir_T_T__And =>
             new String'("T_T__And")
              ,
          Turkixir_T_T__Semicolon =>
             new String'("T_T__Semicolon")
              ,
          Turkixir_T_T__Add_Asign =>
             new String'("T_T__Add_Asign")
              ,
          Turkixir_T_T__Print =>
             new String'("T_T__Print")
              ,
          Turkixir_T_T__Lsh =>
             new String'("T_T__Lsh")
              ,
          Turkixir_T_T__Continue =>
             new String'("T_T__Continue")
              ,
          Turkixir_T_T__While =>
             new String'("T_T__While")
              ,
          Turkixir_T_T__Except =>
             new String'("T_T__Except")
              ,
          Turkixir_T_T__If =>
             new String'("T_T__If")
              ,
          Turkixir_T_T__Else =>
             new String'("T_T__Else")
              ,
          Turkixir_T_T__Del =>
             new String'("T_T__Del")
              ,
          Turkixir_T_T__Minus_Assign =>
             new String'("T_T__Minus_Assign")
              ,
          Turkixir_T_T__Or =>
             new String'("T_T__Or")
              ,
          Turkixir_T_T__Minus =>
             new String'("T_T__Minus")
              ,
          Turkixir_T_T__Lbrack =>
             new String'("T_T__Lbrack")
              ,
          Turkixir_T_T__And_Assign =>
             new String'("T_T__And_Assign")
              ,
          Turkixir_T_T_R_Par =>
             new String'("T_T_R_Par")
              ,
          Turkixir_T_T__Global =>
             new String'("T_T__Global")
              ,
          Turkixir_T_T__For =>
             new String'("T_T__For")
              ,
          Turkixir_T_T__From =>
             new String'("T_T__From")
              ,
          Turkixir_T_T__Rsh =>
             new String'("T_T__Rsh")
              ,
          Turkixir_T_T__Finally =>
             new String'("T_T__Finally")
              ,
          Turkixir_T_T__Pass =>
             new String'("T_T__Pass")
              ,
          Turkixir_T_T__Lsh_Assign =>
             new String'("T_T__Lsh_Assign")
              ,
          Turkixir_T_T__Bin_Or =>
             new String'("T_T__Bin_Or")
              ,
          Turkixir_T_T__Rcurl =>
             new String'("T_T__Rcurl")
              ,
          Turkixir_T_T__With =>
             new String'("T_T__With")
              ,
          Turkixir_T_T__Plus =>
             new String'("T_T__Plus")
              ,
          Turkixir_T_T__Lt =>
             new String'("T_T__Lt")
              ,
          Turkixir_T_T__Number =>
             new String'("T_T__Number")
              ,
          Turkixir_T_T__String =>
             new String'("T_T__String")
              ,
          Turkixir_T_T__Comment =>
             new String'("T_T__Comment")
              ,
          Turkixir_T_T__Id =>
             new String'("T_T__Id")
              ,
          Turkixir_Indent =>
             new String'("Indent")
              ,
          Turkixir_Dedent =>
             new String'("Dedent")
              ,
          Turkixir_Newline =>
             new String'("Newline")
              ,
          Turkixir_Termination =>
             new String'("Termination")
              ,
          Turkixir_Lexing_Failure =>
             new String'("Lexing_Failure")
   );

   ------------------------
   -- Precomputed_Symbol --
   ------------------------

   pragma Warnings (Off, "referenced");
   function Precomputed_Symbol
     (Index : Precomputed_Symbol_Index) return Text_Type is
   pragma Warnings (On, "referenced");
   begin
         return (raise Program_Error);
   end Precomputed_Symbol;

   ---------------------
   -- Token_Kind_Name --
   ---------------------

   function Token_Kind_Name (Token_Id : Token_Kind) return String is
     (Token_Kind_Names (Token_Id).all);

   ------------------------
   -- Token_Kind_Literal --
   ------------------------

   function Token_Kind_Literal (Token_Id : Token_Kind) return Text_Type is
     (Token_Kind_To_Literals (Token_Id).all);

   -----------------------
   -- Token_Error_Image --
   -----------------------

   function Token_Error_Image (Token_Id : Token_Kind) return String is
      Literal : constant Text_Type := Token_Kind_Literal (Token_Id);
   begin
      return (if Literal /= ""
              then "'" & Image (Literal) & "'"
              else Token_Kind_Name (Token_Id));
   end Token_Error_Image;

   function To_Token_Kind (Raw : Raw_Token_Kind) return Token_Kind
   is (Token_Kind'Val (Raw));

   function From_Token_Kind (Kind : Token_Kind) return Raw_Token_Kind
   is (Token_Kind'Pos (Kind));

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Kind : Turkixir_Node_Kind_Type) return Boolean is
   begin
      return Is_Token_Node_Kind (Kind);
   end Is_Token_Node;

   -------------------
   -- Is_Error_Node --
   -------------------

   function Is_Error_Node (Kind : Turkixir_Node_Kind_Type) return Boolean is
   begin
      return Is_Error_Node_Kind (Kind);
   end Is_Error_Node;

   ------------------
   -- Is_List_Node --
   ------------------

   function Is_List_Node (Kind : Turkixir_Node_Kind_Type) return Boolean is
   begin
      return Kind in Turkixir_Turkixir_Node_Base_List;
   end Is_List_Node;

   ------------------
   -- Rewrap_Token --
   ------------------

   function Rewrap_Token
     (Origin : Token_Reference;
      Index  : Token_Or_Trivia_Index) return Token_Reference is
   begin
      return (if Index = No_Token_Or_Trivia_Index
              then No_Token
              else (Origin.TDH, Index, Origin.Safety_Net));
   end Rewrap_Token;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Self : Token_Reference) is
      SN  : Token_Safety_Net renames Self.Safety_Net;
      Ctx : constant Internal_Context := +SN.Context;
   begin
      if Self.TDH /= null
         and then (Ctx.Serial_Number /= SN.Context_Version
                   or else Self.TDH.Version /= SN.TDH_Version)
      then
         raise Stale_Reference_Error;
      end if;
   end Check_Safety_Net;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Token_Reference) return Boolean is
      pragma Assert (Left.TDH = Right.TDH);
   begin
      Check_Safety_Net (Left);
      Check_Safety_Net (Right);
      if Left.Index.Token < Right.Index.Token then
         return True;

      elsif Left.Index.Token = Right.Index.Token then
         return Left.Index.Trivia < Right.Index.Trivia;

      else
         return False;
      end if;
   end "<";

   ----------
   -- Next --
   ----------

   function Next
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference is
   begin
      Check_Safety_Net (Token);
      return (if Token.TDH = null
              then No_Token
              else Rewrap_Token (Token,
                                 Next (Token.Index, Token.TDH.all,
                                       Exclude_Trivia)));
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference is
   begin
      Check_Safety_Net (Token);
      return (if Token.TDH = null
              then No_Token
              else Rewrap_Token (Token,
                                 Previous (Token.Index, Token.TDH.all,
                                           Exclude_Trivia)));
   end Previous;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol (Token : Token_Reference) return Symbol_Type is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return Get_Symbol (Token.Index, Token.TDH.all);
   end Get_Symbol;

   ----------
   -- Data --
   ----------

   function Data (Token : Token_Reference) return Token_Data_Type is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return Convert (Token.TDH.all, Token, Raw_Data (Token));
   end Data;

   ----------
   -- Text --
   ----------

   function Text (Token : Token_Reference) return Text_Type is
      RD : constant Stored_Token_Data := Raw_Data (Token);
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return Token.TDH.Source_Buffer (RD.Source_First .. RD.Source_Last);
   end Text;

   ----------
   -- Text --
   ----------

   function Text (First, Last : Token_Reference) return Text_Type is
      FD, LD : Token_Data_Type;
   begin
      Check_Safety_Net (First);
      Check_Safety_Net (Last);
      if First.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      if First.TDH /= Last.TDH then
         raise Precondition_Failure with
            "token arguments must belong to the same source";
      end if;
      FD := Data (First);
      LD := Data (Last);
      return FD.Source_Buffer.all (FD.Source_First .. LD.Source_Last);
   end Text;

   ----------
   -- Kind --
   ----------

   function Kind (Token_Data : Token_Data_Type) return Token_Kind is
   begin
      return Token_Data.Kind;
   end Kind;

   ---------------
   -- Is_Trivia --
   ---------------

   function Is_Trivia (Token : Token_Reference) return Boolean is
   begin
      Check_Safety_Net (Token);
      return Token.Index.Trivia /= No_Token_Index;
   end Is_Trivia;

   ---------------
   -- Is_Trivia --
   ---------------

   function Is_Trivia (Token_Data : Token_Data_Type) return Boolean is
   begin
      return Token_Data.Is_Trivia;
   end Is_Trivia;

   -----------
   -- Index --
   -----------

   function Index (Token : Token_Reference) return Token_Index is
   begin
      Check_Safety_Net (Token);
      return (if Token.Index.Trivia = No_Token_Index
              then Token.Index.Token
              else Token.Index.Trivia);
   end Index;

   -----------
   -- Index --
   -----------

   function Index (Token_Data : Token_Data_Type) return Token_Index is
   begin
      return Token_Data.Index;
   end Index;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Token_Data : Token_Data_Type) return Source_Location_Range
   is
   begin
      return Token_Data.Sloc_Range;
   end Sloc_Range;

   ---------------------
   -- Origin_Filename --
   ---------------------

   function Origin_Filename (Token : Token_Reference) return String is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return +Token.TDH.Filename.Full_Name;
   end Origin_Filename;

   --------------------
   -- Origin_Charset --
   --------------------

   function Origin_Charset (Token : Token_Reference) return String is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return To_String (Token.TDH.Charset);
   end Origin_Charset;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent (L, R : Token_Reference) return Boolean is
      DL : constant Token_Data_Type := Data (L);
      DR : constant Token_Data_Type := Data (R);
      TL : constant Text_Type := Text (L);
      TR : constant Text_Type := Text (R);
   begin
      return DL.Kind = DR.Kind and then TL = TR;
   end Is_Equivalent;

   -----------
   -- Image --
   -----------

   function Image (Token : Token_Reference) return String is
      D : constant Token_Data_Type := Data (Token);
   begin
      return ("<Token Kind=" & Token_Kind_Name (D.Kind) &
              " Text=" & Image (Text (Token), With_Quotes => True) & ">");
   end Image;

   --------------
   -- Raw_Data --
   --------------

   function Raw_Data (T : Token_Reference) return Stored_Token_Data is
   begin
      Check_Safety_Net (T);
      if T.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return
        (if T.Index.Trivia = No_Token_Index
         then Token_Vectors.Get (T.TDH.Tokens, Natural (T.Index.Token))
         else Trivia_Vectors.Get (T.TDH.Trivias, Natural (T.Index.Trivia)).T);
   end Raw_Data;

   -------------
   -- Convert --
   -------------

   function Convert
     (TDH      : Token_Data_Handler;
      Token    : Token_Reference;
      Raw_Data : Stored_Token_Data) return Token_Data_Type is
   begin
      Check_Safety_Net (Token);
      return (Kind          => To_Token_Kind (Raw_Data.Kind),
              Is_Trivia     => Token.Index.Trivia /= No_Token_Index,
              Index         => (if Token.Index.Trivia = No_Token_Index
                                then Token.Index.Token
                                else Token.Index.Trivia),
              Source_Buffer => Text_Cst_Access (TDH.Source_Buffer),
              Source_First  => Raw_Data.Source_First,
              Source_Last   => Raw_Data.Source_Last,
              Sloc_Range    => Sloc_Range (TDH, Raw_Data));
   end Convert;

   --------------------------
   -- Wrap_Token_Reference --
   --------------------------

   function Wrap_Token_Reference
     (Context : Internal_Context;
      TDH     : Token_Data_Handler_Access;
      Index   : Token_Or_Trivia_Index) return Token_Reference is
   begin
      if Index = No_Token_Or_Trivia_Index then
         return No_Token;
      end if;

      declare
         SN : constant Token_Safety_Net :=
           (Context         => +Context,
            Context_Version => Context.Serial_Number,
            TDH_Version     => TDH.Version);
      begin
        return (TDH, Index, SN);
      end;
   end Wrap_Token_Reference;

   -----------------------
   -- Get_Token_Context --
   -----------------------

   function Get_Token_Context
     (Token : Token_Reference) return Internal_Context is
   begin
      return +Token.Safety_Net.Context;
   end Get_Token_Context;

   -------------------
   -- Get_Token_TDH --
   -------------------

   function Get_Token_TDH
     (Token : Token_Reference) return Token_Data_Handler_Access is
   begin
      return Token.TDH;
   end Get_Token_TDH;

   ---------------------
   -- Get_Token_Index --
   ---------------------

   function Get_Token_Index
     (Token : Token_Reference) return Token_Or_Trivia_Index is
   begin
      return Token.Index;
   end Get_Token_Index;

   ------------------------
   -- Extract_Token_Text --
   ------------------------

   procedure Extract_Token_Text
     (Token         : Token_Data_Type;
      Source_Buffer : out Text_Cst_Access;
      First         : out Positive;
      Last          : out Natural) is
   begin
      Source_Buffer := Token.Source_Buffer;
      First := Token.Source_First;
      Last := Token.Source_Last;
   end Extract_Token_Text;


begin
   --  Check that we actually have full Libiconv support: as nothing works
   --  without it, we explicitly check support here instead of letting
   --  user-unfriendly errors happen during lexing.

   if not GNATCOLL.Iconv.Has_Iconv then
      raise Program_Error with "Libiconv is not available";
   end if;


   Private_Converters.Wrap_Token_Reference := Wrap_Token_Reference'Access;
   Private_Converters.Get_Token_Context := Get_Token_Context'Access;
   Private_Converters.Get_Token_TDH := Get_Token_TDH'Access;
   Private_Converters.Get_Token_Index := Get_Token_Index'Access;
   Private_Converters.Extract_Token_Text := Extract_Token_Text'Access;
end Libturkixirlang.Common;
