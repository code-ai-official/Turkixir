








with Ada.Finalization;
pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
use Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");

with System.Memory;
use type System.Address;

with GNATCOLL.Iconv;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Text;        use Langkit_Support.Text;

with Libturkixirlang.Private_Converters;
use Libturkixirlang.Private_Converters;


          with Langkit_Support.Errors;


package body Libturkixirlang.Implementation.C is

   --  Avoid hiding from $.Lexer
   subtype Token_Data_Type is Common.Token_Data_Type;

   --------------------
   -- Event handlers --
   --------------------

   type C_Event_Handler is limited new
      Ada.Finalization.Limited_Controlled
      and Internal_Event_Handler
   with record
      Ref_Count           : Natural;
      Data                : System.Address;
      Destroy_Func        : turkixir_event_handler_destroy_callback;
      Unit_Requested_Func : turkixir_event_handler_unit_requested_callback;
      Unit_Parsed_Func    : turkixir_event_handler_unit_parsed_callback;
   end record;

   overriding procedure Finalize (Self : in out C_Event_Handler);
   overriding procedure Inc_Ref (Self : in out C_Event_Handler);
   overriding function Dec_Ref (Self : in out C_Event_Handler) return Boolean;

   overriding procedure Unit_Requested_Callback
     (Self               : in out C_Event_Handler;
      Context            : Internal_Context;
      Name               : Text_Type;
      From               : Internal_Unit;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean);

   overriding procedure Unit_Parsed_Callback
     (Self     : in out C_Event_Handler;
      Context  : Internal_Context;
      Unit     : Internal_Unit;
      Reparsed : Boolean);

   ------------------
   -- File readers --
   ------------------

   type C_File_Reader is limited new
      Ada.Finalization.Limited_Controlled
      and Internal_File_Reader
   with record
      Ref_Count    : Natural;
      Data         : System.Address;
      Destroy_Func : turkixir_file_reader_destroy_callback;
      Read_Func    : turkixir_file_reader_read_callback;
   end record;

   type C_File_Reader_Access is access all C_File_Reader;

   overriding procedure Finalize (Self : in out C_File_Reader);
   overriding procedure Inc_Ref (Self : in out C_File_Reader);
   overriding function Dec_Ref (Self : in out C_File_Reader) return Boolean;
   overriding procedure Read
     (Self        : C_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);

   --------------------
   -- Unit providers --
   --------------------

   type C_Unit_Provider is limited new
      Ada.Finalization.Limited_Controlled
      and Internal_Unit_Provider
   with record
      Ref_Count               : Natural;
      Data                    : System.Address;
      Destroy_Func            : turkixir_unit_provider_destroy_callback;
      Get_Unit_Filename_Func  : turkixir_unit_provider_get_unit_filename_callback;
      Get_Unit_From_Name_Func : turkixir_unit_provider_get_unit_from_name_callback;
   end record;

   type C_Unit_Provider_Access is access all C_Unit_Provider;

   overriding procedure Finalize (Provider : in out C_Unit_Provider);
   overriding procedure Inc_Ref (Provider : in out C_Unit_Provider);
   overriding function Dec_Ref
     (Provider : in out C_Unit_Provider) return Boolean;

   overriding function Get_Unit_Filename
     (Provider : C_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String;

   overriding function Get_Unit
     (Provider : C_Unit_Provider;
      Context  : Internal_Context;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind;
      Charset  : String := "";
      Reparse  : Boolean := False) return Internal_Unit;

   function Value_Or_Empty (S : chars_ptr) return String
   --  If S is null, return an empty string. Return Value (S) otherwise.
   is (if S = Null_Ptr
       then ""
       else Value (S));

   Last_Exception : turkixir_exception_Ptr := null;

   ----------
   -- Free --
   ----------

   procedure Free (Address : System.Address) is
      procedure C_Free (Address : System.Address)
        with Import        => True,
             Convention    => C,
             External_Name => "free";
   begin
      C_Free (Address);
   end Free;

   -------------------------
   -- Analysis primitives --
   -------------------------

   function turkixir_create_analysis_context
     (Charset       : chars_ptr;
      File_Reader   : turkixir_file_reader;
      Unit_Provider : turkixir_unit_provider;
      Event_Handler : turkixir_event_handler;
      With_Trivia   : int;
      Tab_Stop      : int) return turkixir_analysis_context is
   begin
      Clear_Last_Exception;

      declare
         C : constant String :=
           (if Charset = Null_Ptr
            then "utf-8"
            else Value (Charset));
      begin
         return Create_Context
            (Charset       => C,
             File_Reader   => Unwrap_Private_File_Reader (File_Reader),
             Unit_Provider => Unwrap_Private_Provider (Unit_Provider),
             Event_Handler => Unwrap_Private_Event_Handler (Event_Handler),
             With_Trivia   => With_Trivia /= 0,
             Tab_Stop      => Natural (Tab_Stop));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   function turkixir_context_incref
     (Context : turkixir_analysis_context) return turkixir_analysis_context is
   begin
      Clear_Last_Exception;
      Inc_Ref (Context);
      return Context;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   procedure turkixir_context_decref
     (Context : turkixir_analysis_context)
   is
      Context_Var : Internal_Context := Context;
   begin
      Clear_Last_Exception;
      Dec_Ref (Context_Var);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function turkixir_context_symbol
     (Context : turkixir_analysis_context;
      Text    : access turkixir_text;
      Symbol  : access turkixir_symbol_type) return int
   is
      Raw_Text : Text_Type (1 .. Natural (Text.Length))
         with Import, Address => Text.Chars;
   begin
      Clear_Last_Exception;
      Symbol.all := Wrap_Symbol (Lookup_Symbol (Context, Raw_Text));
      return 1;
   exception
      when Invalid_Symbol_Error =>
         return 0;
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   procedure turkixir_context_discard_errors_in_populate_lexical_env
     (Context : turkixir_analysis_context;
      Discard : int) is
   begin
      Clear_Last_Exception;
      Discard_Errors_In_Populate_Lexical_Env (Context, Discard /= 0);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function turkixir_get_analysis_unit_from_file
     (Context           : turkixir_analysis_context;
      Filename, Charset : chars_ptr;
      Reparse           : int;
      Rule              : turkixir_grammar_rule) return turkixir_analysis_unit is
   begin
      Clear_Last_Exception;

      return Get_From_File
        (Context,
         Value (Filename),
         Value_Or_Empty (Charset),
         Reparse /= 0,
         Rule);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   function turkixir_get_analysis_unit_from_buffer
     (Context           : turkixir_analysis_context;
      Filename, Charset : chars_ptr;
      Buffer            : chars_ptr;
      Buffer_Size       : size_t;
      Rule              : turkixir_grammar_rule) return turkixir_analysis_unit is
   begin
      Clear_Last_Exception;

      declare
         Buffer_Str : String (1 .. Natural (Buffer_Size))
            with Import, Address => Convert (Buffer);
      begin
         return Get_From_Buffer
           (Context,
            Value (Filename),
            Value_Or_Empty (Charset),
            Buffer_Str,
            Rule);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;


   procedure turkixir_unit_root
     (Unit     : turkixir_analysis_unit;
      Result_P : turkixir_base_entity_Ptr) is
   begin
      Clear_Last_Exception;

      Result_P.all := (Unit.AST_Root, No_Entity_Info);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure turkixir_unit_first_token
     (Unit  : turkixir_analysis_unit;
      Token : access turkixir_token) is
   begin
      Clear_Last_Exception;

      declare
         T : constant Token_Reference := First_Token (Unit);
      begin
         Token.all := Wrap (T);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure turkixir_unit_last_token
     (Unit  : turkixir_analysis_unit;
      Token : access turkixir_token) is
   begin
      Clear_Last_Exception;

      declare
         T : constant Token_Reference := Last_Token (Unit);
      begin
         Token.all := Wrap (T);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function turkixir_unit_token_count
     (Unit : turkixir_analysis_unit) return int is
   begin
      Clear_Last_Exception;

      return int (Token_Count (Unit));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return -1;
   end;

   function turkixir_unit_trivia_count
     (Unit : turkixir_analysis_unit) return int is
   begin
      Clear_Last_Exception;

      return int (Trivia_Count (Unit));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return -1;
   end;

   procedure turkixir_unit_lookup_token
     (Unit   : turkixir_analysis_unit;
      Sloc   : access turkixir_source_location;
      Result : access turkixir_token) is
   begin
      Clear_Last_Exception;

      declare
         S   : constant Source_Location := Unwrap (Sloc.all);
         Tok : constant Token_Reference := Lookup_Token (Unit, S);
      begin
         Result.all := Wrap (Tok);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure turkixir_unit_dump_lexical_env
     (Unit : turkixir_analysis_unit) is
   begin
      Clear_Last_Exception;
      Dump_Lexical_Env (Unit);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function turkixir_unit_filename
     (Unit : turkixir_analysis_unit) return chars_ptr is
   begin
      Clear_Last_Exception;

      return New_String (Get_Filename (Unit));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return Null_Ptr;
   end;

   function turkixir_unit_diagnostic_count
     (Unit : turkixir_analysis_unit) return unsigned is
   begin
      Clear_Last_Exception;

      return unsigned (Unit.Diagnostics.Length);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function turkixir_unit_diagnostic
     (Unit         : turkixir_analysis_unit;
      N            : unsigned;
      Diagnostic_P : access turkixir_diagnostic) return int
   is
   begin
      Clear_Last_Exception;

      if N < unsigned (Unit.Diagnostics.Length) then
         declare
            D_In  : Diagnostic renames Unit.Diagnostics (Natural (N) + 1);
            D_Out : turkixir_diagnostic renames Diagnostic_P.all;
         begin
            D_Out.Sloc_Range := Wrap (D_In.Sloc_Range);
            D_Out.Message := Wrap (D_In.Message);
            return 1;
         end;
      else
         return 0;
      end if;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function turkixir_unit_context
     (Unit : turkixir_analysis_unit) return turkixir_analysis_context is
   begin
      Clear_Last_Exception;
      return Unit.Context;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   procedure turkixir_unit_reparse_from_file
     (Unit : turkixir_analysis_unit; Charset : chars_ptr) is
   begin
      Clear_Last_Exception;

      Reparse (Unit, Value_Or_Empty (Charset));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure turkixir_unit_reparse_from_buffer
     (Unit        : turkixir_analysis_unit;
      Charset     : chars_ptr;
      Buffer      : chars_ptr;
      Buffer_Size : size_t) is
   begin
      Clear_Last_Exception;

      declare
         Buffer_Str : String (1 .. Natural (Buffer_Size))
            with Import, Address => Convert (Buffer);
      begin
         Reparse (Unit, Value_Or_Empty (Charset), Buffer_Str);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function turkixir_unit_populate_lexical_env
     (Unit : turkixir_analysis_unit) return int is
   begin
      Clear_Last_Exception;
      Populate_Lexical_Env (Unit);
      return 1;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

   Node_Kind_Names : constant array (Turkixir_Node_Kind_Type) of Text_Access :=
     (Turkixir_Arg_Assoc => new Text_Type'(To_Text ("ArgAssoc")), Turkixir_Arg_Gen => new Text_Type'(To_Text ("ArgGen")), Turkixir_Kw_Args => new Text_Type'(To_Text ("KwArgs")), Turkixir_Var_Args => new Text_Type'(To_Text ("VarArgs")), Turkixir_As_Name_Node => new Text_Type'(To_Text ("AsNameNode")), Turkixir_Comp_If => new Text_Type'(To_Text ("CompIf")), Turkixir_Comp_Op_Kind_Diamond => new Text_Type'(To_Text ("CompOpKindDiamond")), Turkixir_Comp_Op_Kind_Eq => new Text_Type'(To_Text ("CompOpKindEq")), Turkixir_Comp_Op_Kind_Gt => new Text_Type'(To_Text ("CompOpKindGt")), Turkixir_Comp_Op_Kind_Gte => new Text_Type'(To_Text ("CompOpKindGte")), Turkixir_Comp_Op_Kind_In => new Text_Type'(To_Text ("CompOpKindIn")), Turkixir_Comp_Op_Kind_Is => new Text_Type'(To_Text ("CompOpKindIs")), Turkixir_Comp_Op_Kind_Isnot => new Text_Type'(To_Text ("CompOpKindIsnot")), Turkixir_Comp_Op_Kind_Lt => new Text_Type'(To_Text ("CompOpKindLt")), Turkixir_Comp_Op_Kind_Lte => new Text_Type'(To_Text ("CompOpKindLte")), Turkixir_Comp_Op_Kind_Noteq => new Text_Type'(To_Text ("CompOpKindNoteq")), Turkixir_Comp_Op_Kind_Notin => new Text_Type'(To_Text ("CompOpKindNotin")), Turkixir_Comp_For => new Text_Type'(To_Text ("CompFor")), Turkixir_Comp_ForL => new Text_Type'(To_Text ("CompForL")), Turkixir_Decorator => new Text_Type'(To_Text ("Decorator")), Turkixir_Dict_Assoc => new Text_Type'(To_Text ("DictAssoc")), Turkixir_Else_Part => new Text_Type'(To_Text ("ElsePart")), Turkixir_Except_Part => new Text_Type'(To_Text ("ExceptPart")), Turkixir_And_Expr => new Text_Type'(To_Text ("AndExpr")), Turkixir_And_Op => new Text_Type'(To_Text ("AndOp")), Turkixir_Arith_Expr => new Text_Type'(To_Text ("ArithExpr")), Turkixir_Shift_Expr => new Text_Type'(To_Text ("ShiftExpr")), Turkixir_Term => new Text_Type'(To_Text ("Term")), Turkixir_Call_Expr => new Text_Type'(To_Text ("CallExpr")), Turkixir_Comp_Op => new Text_Type'(To_Text ("CompOp")), Turkixir_Concat_String_Lit => new Text_Type'(To_Text ("ConcatStringLit")), Turkixir_Dict_Comp => new Text_Type'(To_Text ("DictComp")), Turkixir_Dict_Lit => new Text_Type'(To_Text ("DictLit")), Turkixir_Dot => new Text_Type'(To_Text ("Dot")), Turkixir_Ellipsis_Expr => new Text_Type'(To_Text ("EllipsisExpr")), Turkixir_Factor => new Text_Type'(To_Text ("Factor")), Turkixir_If_Expr => new Text_Type'(To_Text ("IfExpr")), Turkixir_Inline_Eval => new Text_Type'(To_Text ("InlineEval")), Turkixir_Lambda_Def => new Text_Type'(To_Text ("LambdaDef")), Turkixir_List_Comp => new Text_Type'(To_Text ("ListComp")), Turkixir_List_Gen => new Text_Type'(To_Text ("ListGen")), Turkixir_List_Lit => new Text_Type'(To_Text ("ListLit")), Turkixir_Dotted_Name => new Text_Type'(To_Text ("DottedName")), Turkixir_Id => new Text_Type'(To_Text ("Id")), Turkixir_Not_Op => new Text_Type'(To_Text ("NotOp")), Turkixir_Number_Lit => new Text_Type'(To_Text ("NumberLit")), Turkixir_Or_Expr => new Text_Type'(To_Text ("OrExpr")), Turkixir_Or_Op => new Text_Type'(To_Text ("OrOp")), Turkixir_Power => new Text_Type'(To_Text ("Power")), Turkixir_Set_Comp => new Text_Type'(To_Text ("SetComp")), Turkixir_Set_Lit => new Text_Type'(To_Text ("SetLit")), Turkixir_Slice_Expr => new Text_Type'(To_Text ("SliceExpr")), Turkixir_Ext_Slice_Expr => new Text_Type'(To_Text ("ExtSliceExpr")), Turkixir_String_Lit => new Text_Type'(To_Text ("StringLit")), Turkixir_Subscript_Expr => new Text_Type'(To_Text ("SubscriptExpr")), Turkixir_Tuple_Lit => new Text_Type'(To_Text ("TupleLit")), Turkixir_Xor_Expr => new Text_Type'(To_Text ("XorExpr")), Turkixir_Yield_Expr => new Text_Type'(To_Text ("YieldExpr")), Turkixir_File_Node => new Text_Type'(To_Text ("FileNode")), Turkixir_Import_Star => new Text_Type'(To_Text ("ImportStar")), Turkixir_Kw_Args_Flag_Absent => new Text_Type'(To_Text ("KwArgsFlagAbsent")), Turkixir_Kw_Args_Flag_Present => new Text_Type'(To_Text ("KwArgsFlagPresent")), Turkixir_NL => new Text_Type'(To_Text ("NL")), Turkixir_Op => new Text_Type'(To_Text ("Op")), Turkixir_Params => new Text_Type'(To_Text ("Params")), Turkixir_Rel_Name => new Text_Type'(To_Text ("RelName")), Turkixir_Single_Param => new Text_Type'(To_Text ("SingleParam")), Turkixir_Assert_Stmt => new Text_Type'(To_Text ("AssertStmt")), Turkixir_Assign_Stmt => new Text_Type'(To_Text ("AssignStmt")), Turkixir_Aug_Assign_Stmt => new Text_Type'(To_Text ("AugAssignStmt")), Turkixir_Break_Stmt => new Text_Type'(To_Text ("BreakStmt")), Turkixir_Continue_Stmt => new Text_Type'(To_Text ("ContinueStmt")), Turkixir_Decorated => new Text_Type'(To_Text ("Decorated")), Turkixir_Class_Def => new Text_Type'(To_Text ("ClassDef")), Turkixir_Func_Def => new Text_Type'(To_Text ("FuncDef")), Turkixir_Del_Stmt => new Text_Type'(To_Text ("DelStmt")), Turkixir_Elif_Branch => new Text_Type'(To_Text ("ElifBranch")), Turkixir_Exec_Stmt => new Text_Type'(To_Text ("ExecStmt")), Turkixir_For_Stmt => new Text_Type'(To_Text ("ForStmt")), Turkixir_Global_Stmt => new Text_Type'(To_Text ("GlobalStmt")), Turkixir_If_Stmt => new Text_Type'(To_Text ("IfStmt")), Turkixir_Import_From => new Text_Type'(To_Text ("ImportFrom")), Turkixir_Import_Name => new Text_Type'(To_Text ("ImportName")), Turkixir_Pass_Stmt => new Text_Type'(To_Text ("PassStmt")), Turkixir_Print_Stmt => new Text_Type'(To_Text ("PrintStmt")), Turkixir_Raise_Stmt => new Text_Type'(To_Text ("RaiseStmt")), Turkixir_Return_Stmt => new Text_Type'(To_Text ("ReturnStmt")), Turkixir_Stream_Print_Stmt => new Text_Type'(To_Text ("StreamPrintStmt")), Turkixir_Try_Stmt => new Text_Type'(To_Text ("TryStmt")), Turkixir_While_Stmt => new Text_Type'(To_Text ("WhileStmt")), Turkixir_With_Stmt => new Text_Type'(To_Text ("WithStmt")), Turkixir_Arg_List => new Text_Type'(To_Text ("ArgList")), Turkixir_As_Name_Node_List => new Text_Type'(To_Text ("AsNameNodeList")), Turkixir_Decorator_List => new Text_Type'(To_Text ("DecoratorList")), Turkixir_Dict_Assoc_List => new Text_Type'(To_Text ("DictAssocList")), Turkixir_Dot_List => new Text_Type'(To_Text ("DotList")), Turkixir_Elif_Branch_List => new Text_Type'(To_Text ("ElifBranchList")), Turkixir_Except_Part_List => new Text_Type'(To_Text ("ExceptPartList")), Turkixir_Expr_List => new Text_Type'(To_Text ("ExprList")), Turkixir_Id_List => new Text_Type'(To_Text ("IdList")), Turkixir_NL_List => new Text_Type'(To_Text ("NLList")), Turkixir_Single_Param_List => new Text_Type'(To_Text ("SingleParamList")), Turkixir_String_Lit_List => new Text_Type'(To_Text ("StringLitList")), Turkixir_Turkixir_Node_List => new Text_Type'(To_Text ("TurkixirNodeList")), Turkixir_Var_Args_Flag_Absent => new Text_Type'(To_Text ("VarArgsFlagAbsent")), Turkixir_Var_Args_Flag_Present => new Text_Type'(To_Text ("VarArgsFlagPresent")));

   function turkixir_node_kind
     (Node : turkixir_base_entity_Ptr) return turkixir_node_kind_enum is
   begin
      Clear_Last_Exception;

      declare
         K : constant Turkixir_Node_Kind_Type := Node.Node.Kind;
      begin
         return turkixir_node_kind_enum (K'Enum_Rep);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return turkixir_node_kind_enum'First;
   end;

   procedure turkixir_kind_name
     (Kind : turkixir_node_kind_enum; Result : access turkixir_text) is
   begin
      Clear_Last_Exception;

      declare
         K    : constant Turkixir_Node_Kind_Type := Turkixir_Node_Kind_Type'Enum_Val (Kind);
         Name : Text_Access renames Node_Kind_Names (K);
      begin
         Result.all := (Chars        => Name.all'Address,
                        Length       => Name'Length,
                        Is_Allocated => 0);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function turkixir_node_unit
     (Node : turkixir_base_entity_Ptr) return turkixir_analysis_unit is
   begin
      Clear_Last_Exception;
      return Node.Node.Unit;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   function turkixir_is_token_node
     (Node : turkixir_base_entity_Ptr) return int is
   begin
      Clear_Last_Exception;
      return Boolean'Pos (Is_Token_Node (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function turkixir_is_synthetic
     (Node : turkixir_base_entity_Ptr) return int is
   begin
      Clear_Last_Exception;
      return Boolean'Pos (Is_Synthetic (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   procedure turkixir_node_image
     (Node : turkixir_base_entity_Ptr; Result : access turkixir_text) is
   begin
      Clear_Last_Exception;
      declare
         Img : constant Text_Type := Short_Text_Image (Node.Node);
      begin
         Result.all := Wrap_Alloc (Img);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure turkixir_node_text
     (Node : turkixir_base_entity_Ptr;
      Text : access turkixir_text) is
   begin
      Clear_Last_Exception;
      Text.all := Wrap_Alloc (Implementation.Text (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure turkixir_node_sloc_range
     (Node         : turkixir_base_entity_Ptr;
      Sloc_Range_P : access turkixir_source_location_range) is
   begin
      Clear_Last_Exception;

      Sloc_Range_P.all := Wrap (Sloc_Range (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure turkixir_lookup_in_node
     (Node   : turkixir_base_entity_Ptr;
      Sloc   : turkixir_source_location;
      Result : turkixir_base_entity_Ptr) is
   begin
      Clear_Last_Exception;

      declare
         S : constant Source_Location := Unwrap (Sloc);
      begin
         Result.all := (Lookup (Node.Node, S), Node.Info);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function turkixir_node_children_count
     (Node : turkixir_base_entity_Ptr) return unsigned is
   begin
      Clear_Last_Exception;
      return unsigned (Children_Count (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function turkixir_node_child
     (Node    : turkixir_base_entity_Ptr;
      N       : unsigned;
      Child_P : turkixir_base_entity_Ptr) return int is
   begin
      Clear_Last_Exception;

      declare
         Result : Bare_Turkixir_Node;
         Exists : Boolean;
      begin
         if N > unsigned (Natural'Last) then
            return 0;
         end if;
         Get_Child (Node.Node, Natural (N) + 1, Exists, Result);
         if Exists then
            Child_P.all := (Result, Node.Info);
            return 1;
         else
            return 0;
         end if;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function turkixir_text_to_locale_string
     (Text : turkixir_text) return System.Address is
   begin
      Clear_Last_Exception;

      declare
         use GNATCOLL.Iconv;

         Input_Byte_Size : constant size_t := 4 * Text.Length;

         Output_Byte_Size : constant size_t := Input_Byte_Size + 1;
         --  Assuming no encoding will take more than 4 bytes per character, 4
         --  times the size of the input text plus one null byte should be
         --  enough to hold the result. This is a development helper anyway, so
         --  we don't have performance concerns.

         Result : constant System.Address := System.Memory.Alloc
           (System.Memory.size_t (Output_Byte_Size));
         --  Buffer we are going to return to the caller. We use
         --  System.Memory.Alloc so that users can call C's "free" function in
         --  order to free it.

         Input : String (1 .. Natural (Input_Byte_Size));
         for Input'Address use Text.Chars;

         Output : String (1 .. Natural (Output_Byte_Size));
         for Output'Address use Result;

         State                     : Iconv_T;
         Input_Index, Output_Index : Positive := 1;
         Status                    : Iconv_Result;

         From_Code : constant String :=
           (if System."=" (System.Default_Bit_Order, System.Low_Order_First)
            then UTF32LE
            else UTF32BE);

      begin
         --  GNATCOLL.Iconv raises Constraint_Error exceptions for empty
         --  strings, so handle them ourselves.

         if Input_Byte_Size = 0 then
            Output (1) := ASCII.NUL;
         end if;

         --  Encode to the locale. Don't bother with error checking...

         Set_Locale;
         State := Iconv_Open
           (To_Code         => Locale,
            From_Code       => From_Code,
            Transliteration => True,
            Ignore          => True);
         Iconv (State, Input, Input_Index, Output, Output_Index, Status);
         Iconv_Close (State);

         --  Don't forget the trailing NULL character to keep C programs happy
         Output (Output_Index) := ASCII.NUL;

         return Result;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return System.Null_Address;
   end;

   ----------
   -- Wrap --
   ----------

   function Wrap (S : Unbounded_Wide_Wide_String) return turkixir_text is
      Chars  : Big_Wide_Wide_String_Access;
      Length : Natural;
   begin
      Get_Wide_Wide_String (S, Chars, Length);
      return (Chars.all'Address, size_t (Length), 0);
   end Wrap;

   ------------------------
   -- Set_Last_Exception --
   ------------------------

   procedure Set_Last_Exception (Exc : Exception_Occurrence) is
   begin
      --  If it's the first time, allocate room for the exception information

      if Last_Exception = null then
         Last_Exception := new turkixir_exception;

      --  If it is not the first time, free memory allocated for the last
      --  exception.

      elsif Last_Exception.Information /= Null_Ptr then
         Free (Last_Exception.Information);
      end if;

      --  Get the kind corresponding to Exc

      declare
         Id : constant Exception_Id := Exception_Identity (Exc);
      begin
         if Id = Langkit_Support.Errors.Introspection.Bad_Type_Error'Identity then
            Last_Exception.Kind := Exception_Bad_Type_Error;
            Last_Exception.Information :=
               New_String (Exception_Message (Exc));
         elsif Id = Langkit_Support.Errors.Introspection.Out_Of_Bounds_Error'Identity then
            Last_Exception.Kind := Exception_Out_Of_Bounds_Error;
            Last_Exception.Information :=
               New_String (Exception_Message (Exc));
         elsif Id = Langkit_Support.Errors.Invalid_Input'Identity then
            Last_Exception.Kind := Exception_Invalid_Input;
            Last_Exception.Information :=
               New_String (Exception_Message (Exc));
         elsif Id = Langkit_Support.Errors.Invalid_Symbol_Error'Identity then
            Last_Exception.Kind := Exception_Invalid_Symbol_Error;
            Last_Exception.Information :=
               New_String (Exception_Message (Exc));
         elsif Id = Langkit_Support.Errors.Invalid_Unit_Name_Error'Identity then
            Last_Exception.Kind := Exception_Invalid_Unit_Name_Error;
            Last_Exception.Information :=
               New_String (Exception_Message (Exc));
         elsif Id = Langkit_Support.Errors.Native_Exception'Identity then
            Last_Exception.Kind := Exception_Native_Exception;
            Last_Exception.Information :=
               New_String (Exception_Message (Exc));
         elsif Id = Langkit_Support.Errors.Precondition_Failure'Identity then
            Last_Exception.Kind := Exception_Precondition_Failure;
            Last_Exception.Information :=
               New_String (Exception_Message (Exc));
         elsif Id = Langkit_Support.Errors.Property_Error'Identity then
            Last_Exception.Kind := Exception_Property_Error;
            Last_Exception.Information :=
               New_String (Exception_Message (Exc));
         elsif Id = Langkit_Support.Errors.Rewriting.Template_Args_Error'Identity then
            Last_Exception.Kind := Exception_Template_Args_Error;
            Last_Exception.Information :=
               New_String (Exception_Message (Exc));
         elsif Id = Langkit_Support.Errors.Rewriting.Template_Format_Error'Identity then
            Last_Exception.Kind := Exception_Template_Format_Error;
            Last_Exception.Information :=
               New_String (Exception_Message (Exc));
         elsif Id = Langkit_Support.Errors.Rewriting.Template_Instantiation_Error'Identity then
            Last_Exception.Kind := Exception_Template_Instantiation_Error;
            Last_Exception.Information :=
               New_String (Exception_Message (Exc));
         elsif Id = Langkit_Support.Errors.Stale_Reference_Error'Identity then
            Last_Exception.Kind := Exception_Stale_Reference_Error;
            Last_Exception.Information :=
               New_String (Exception_Message (Exc));
         elsif Id = Langkit_Support.Errors.Unknown_Charset'Identity then
            Last_Exception.Kind := Exception_Unknown_Charset;
            Last_Exception.Information :=
               New_String (Exception_Message (Exc));
         else
            Last_Exception.Kind := Exception_Native_Exception;
            Last_Exception.Information :=
               New_String (Exception_Information (Exc));
         end if;
      end;

   end Set_Last_Exception;

   --------------------------
   -- Clear_Last_Exception --
   --------------------------

   procedure Clear_Last_Exception is
   begin
      if Last_Exception /= null then
         Free (Last_Exception.Information);
      end if;
   end Clear_Last_Exception;

   function turkixir_get_last_exception return turkixir_exception_Ptr
   is
   begin
      if Last_Exception = null
         or else Last_Exception.Information = Null_Ptr
      then
         return null;
      else
         return Last_Exception;
      end if;
   end;

   function turkixir_token_kind_name (Kind : int) return chars_ptr
   is
      K : Token_Kind;
   begin
      begin
         K := Token_Kind'Enum_Val (Kind);
      exception
         when Exc : Constraint_Error =>
            Set_Last_Exception (Exc);
            return Null_Ptr;
      end;

      return New_String (Token_Kind_Name (K));
   end;

   procedure turkixir_token_next
     (Token      : turkixir_token;
      Next_Token : access turkixir_token)
   is
   begin
      Clear_Last_Exception;
      declare
         T  : constant Token_Reference := Unwrap (Token);
         NT : constant Token_Reference := Next (T);
      begin
         Next_Token.all := Wrap (NT);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure turkixir_token_previous
     (Token          : turkixir_token;
      Previous_Token : access turkixir_token)
   is
   begin
      Clear_Last_Exception;
      declare
         T  : constant Token_Reference := Unwrap (Token);
         PT : constant Token_Reference := Previous (T);
      begin
         Previous_Token.all := Wrap (PT);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function turkixir_token_range_text
     (First, Last : turkixir_token;
      Text        : access turkixir_text) return int
   is
   begin
      Clear_Last_Exception;
      declare
         FD : constant Token_Data_Type := Data (Unwrap (First));
         LD : constant Token_Data_Type := Data (Unwrap (Last));

         First_Source_Buffer, Last_Source_Buffer : Text_Cst_Access;
         First_Index, Ignored_First              : Positive;
         Last_Index, Ignored_Last                : Natural;
      begin
         Extract_Token_Text
           (FD, First_Source_Buffer, First_Index, Ignored_Last);
         Extract_Token_Text
           (LD, Last_Source_Buffer, Ignored_First, Last_Index);
         if First_Source_Buffer /= Last_Source_Buffer then
            return 0;
         end if;
         Text.all := Wrap (First_Source_Buffer, First_Index, Last_Index);
         return 1;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function turkixir_token_is_equivalent
     (Left  : turkixir_token;
      Right : turkixir_token) return turkixir_bool
   is
   begin
      Clear_Last_Exception;
         declare
         L  : constant Token_Reference := Unwrap (Left);
         R  : constant Token_Reference := Unwrap (Right);
      begin
         return turkixir_bool (Boolean'Pos (Is_Equivalent (L, R)));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   procedure turkixir_entity_image
     (Ent : turkixir_base_entity_Ptr; Result : access turkixir_text) is
   begin
      Clear_Last_Exception;
      declare
         Img : constant Text_Type := Text_Image (Ent.all);
      begin
         Result.all := Wrap_Alloc (Img);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   ----------------
   -- Wrap_Alloc --
   ----------------

   function Wrap_Alloc (S : Text_Type) return turkixir_text is
      T : Text_Access := new Text_Type'(S);
   begin
      return turkixir_text'(T.all'Address, T.all'Length, Is_Allocated => 1);
   end Wrap_Alloc;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (S     : Text_Cst_Access;
      First : Positive;
      Last  : Natural) return turkixir_text
   is
      Substring : Text_Type renames S (First .. Last);
   begin
      return (if First > Last
              then (Chars        => System.Null_Address,
                    Length       => 0,
                    Is_Allocated => 0)
              else (Chars        => S (First)'Address,
                    Length       => Substring'Length,
                    Is_Allocated => 0));
   end Wrap;

   procedure turkixir_destroy_text (T : access turkixir_text) is
   begin
      Clear_Last_Exception;
      declare
         use System;
      begin
         if T.Is_Allocated /= 0 and then T.Chars /= System.Null_Address then
            declare
               TT : Text_Type (1 .. Natural (T.Length));
               for TT'Address use T.Chars;
               TA : Text_Access := TT'Unrestricted_Access;
            begin
               Free (TA);
            end;
            T.Chars := System.Null_Address;
         end if;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure turkixir_symbol_text
     (Symbol : access turkixir_symbol_type; Text : access turkixir_text) is
   begin
      Clear_Last_Exception;
      declare
         Sym    : constant Symbol_Type := Unwrap_Symbol (Symbol.all);
         Result : constant Text_Type :=
           (if Sym = null then "" else Image (Sym));
      begin
         Text.all := Wrap_Alloc (Result);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function turkixir_create_big_integer
     (Text : access turkixir_text) return turkixir_big_integer is
   begin
      Clear_Last_Exception;
      declare
         T      : Text_Type (1 .. Natural (Text.Length))
            with Import, Address => Text.Chars;
         Image  : constant String := Langkit_Support.Text.Image (T);
         Result : constant Big_Integer_Type := Create_Big_Integer (Image);
      begin
         return Wrap_Big_Integer (Result);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return turkixir_big_integer (System.Null_Address);
   end turkixir_create_big_integer;

   procedure turkixir_big_integer_text
     (Bigint : turkixir_big_integer; Text : access turkixir_text) is
   begin
      Clear_Last_Exception;
      declare
         BI    : constant Big_Integer_Type := Unwrap_Big_Integer (Bigint);
         Image : constant String := BI.Value.Image;
      begin
         Text.all := Wrap_Alloc (To_Text (Image));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure turkixir_big_integer_decref
     (Bigint : turkixir_big_integer) is
   begin
      Clear_Last_Exception;
      declare
         BI : Big_Integer_Type := Unwrap_Big_Integer (Bigint);
      begin
         Dec_Ref (BI);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure turkixir_get_versions
     (Version, Build_Date : access chars_ptr)
   is
   begin
      Clear_Last_Exception;
      Version.all := New_String (Libturkixirlang.Version);
      Build_Date.all := New_String (Libturkixirlang.Build_Date);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function turkixir_create_string
     (Content : System.Address; Length : int) return turkixir_string_type
   is
      Value : Text_Type (1 .. Integer (Length))
        with Import, Address => Content;
   begin
      Clear_Last_Exception;
      return Create_String (Value);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   procedure turkixir_string_dec_ref (Self : turkixir_string_type) is
   begin
      Clear_Last_Exception;
      declare
         Self_Var : String_Type := Self;
      begin
         Dec_Ref (Self_Var);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function turkixir_create_unit_provider
     (Data                    : System.Address;
      Destroy_Func            : turkixir_unit_provider_destroy_callback;
      Get_Unit_Filename_Func  : turkixir_unit_provider_get_unit_filename_callback;
      Get_Unit_From_Name_Func : turkixir_unit_provider_get_unit_from_name_callback)
      return turkixir_unit_provider is
   begin
      Clear_Last_Exception;
      declare
         Result : constant C_Unit_Provider_Access := new C_Unit_Provider'
           (Ada.Finalization.Limited_Controlled with
            Ref_Count               => 1,
            Data                    => Data,
            Destroy_Func            => Destroy_Func,
            Get_Unit_Filename_Func  => Get_Unit_Filename_Func,
            Get_Unit_From_Name_Func => Get_Unit_From_Name_Func);
      begin
         return Wrap_Private_Provider (Internal_Unit_Provider_Access (Result));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return turkixir_unit_provider (System.Null_Address);
   end;

   procedure turkixir_dec_ref_unit_provider
     (Provider : turkixir_unit_provider) is
   begin
      Clear_Last_Exception;
      declare
         P : Internal_Unit_Provider_Access :=
            Unwrap_Private_Provider (Provider);
      begin
         Dec_Ref (P);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function turkixir_create_event_handler
     (Data                : System.Address;
      Destroy_Func        : turkixir_event_handler_destroy_callback;
      Unit_Requested_Func : turkixir_event_handler_unit_requested_callback;
      Unit_Parsed_Func    : turkixir_event_handler_unit_parsed_callback)
      return turkixir_event_handler is
   begin
      Clear_Last_Exception;
      declare
         Result : constant Internal_Event_Handler_Access :=
           new C_Event_Handler'
             (Ada.Finalization.Limited_Controlled with
              Ref_Count           => 1,
              Data                => Data,
              Destroy_Func        => Destroy_Func,
              Unit_Requested_Func => Unit_Requested_Func,
              Unit_Parsed_Func    => Unit_Parsed_Func);
      begin
         return Wrap_Private_Event_Handler (Result);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return turkixir_event_handler (System.Null_Address);
   end;

   procedure turkixir_dec_ref_event_handler
     (Handler : turkixir_event_handler) is
   begin
      Clear_Last_Exception;
      declare
         P : Internal_Event_Handler_Access :=
            Unwrap_Private_Event_Handler (Handler);
      begin
         Dec_Ref (P);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out C_File_Reader) is
   begin
      Self.Destroy_Func (Self.Data);
   end Finalize;

   -------------
   -- Inc_Ref --
   -------------

   overriding procedure Inc_Ref (Self : in out C_File_Reader) is
   begin
      Self.Ref_Count := Self.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   overriding function Dec_Ref (Self : in out C_File_Reader) return Boolean is
   begin
      Self.Ref_Count := Self.Ref_Count - 1;
      if Self.Ref_Count = 0 then
         return True;
      else
         return False;
      end if;
   end Dec_Ref;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self        : C_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      C_Filename : chars_ptr := New_String (Filename);
      C_Charset  : chars_ptr := New_String (Charset);
      C_Read_BOM : constant int := (if Read_BOM then 1 else 0);

      C_Contents   : aliased turkixir_text;
      C_Diagnostic : aliased turkixir_diagnostic :=
        (Sloc_Range => <>,
         Message    => (Chars        => Null_Address,
                        Length       => 0,
                        Is_Allocated => 0));
   begin
      Self.Read_Func.all
        (Self.Data, C_Filename, C_Charset, C_Read_BOM, C_Contents'Access,
         C_Diagnostic'Access);

      if C_Diagnostic.Message.Chars = Null_Address then

         --  If there is a diagnostic (an error), there is no content to return

         declare
            Message : Text_Type (1 .. Natural (C_Diagnostic.Message.Length))
               with Import,
                    Convention => Ada,
                    Address    => C_Diagnostic.Message.Chars;
         begin
            Append (Diagnostics,
                    Unwrap (C_Diagnostic.Sloc_Range),
                    Message);
         end;

      else
         --  Otherwise, create a copy of the buffer

         declare
            Buffer : Text_Type (1 .. Natural (C_Contents.Length))
               with Import, Convention => Ada, Address => C_Contents.Chars;
         begin
            Contents.Buffer := new Text_Type (Buffer'Range);
            Contents.First := Buffer'First;
            Contents.Last := Buffer'Last;
            Contents.Buffer.all := Buffer;
         end;
      end if;

      Free (C_Filename);
      Free (C_Charset);
   end Read;

   function turkixir_create_file_reader
     (Data         : System.Address;
      Destroy_Func : turkixir_file_reader_destroy_callback;
      Read_Func    : turkixir_file_reader_read_callback) return turkixir_file_reader
   is
   begin
      Clear_Last_Exception;
      declare
         Result : constant C_File_Reader_Access := new C_File_Reader'
           (Ada.Finalization.Limited_Controlled with
            Ref_Count    => 1,
            Data         => Data,
            Destroy_Func => Destroy_Func,
            Read_Func    => Read_Func);
      begin
         return Wrap_Private_File_Reader (Internal_File_Reader_Access (Result));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return turkixir_file_reader (System.Null_Address);
   end;

   procedure turkixir_dec_ref_file_reader
     (File_Reader : turkixir_file_reader) is
   begin
      Clear_Last_Exception;
      declare
         P : Internal_File_Reader_Access :=
            Unwrap_Private_File_Reader (File_Reader);
      begin
         Dec_Ref (P);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   


   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out C_Event_Handler) is
   begin
      Self.Destroy_Func (Self.Data);
   end Finalize;

   -------------
   -- Inc_Ref --
   -------------

   overriding procedure Inc_Ref (Self : in out C_Event_Handler) is
   begin
      Self.Ref_Count := Self.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   overriding function Dec_Ref (Self : in out C_Event_Handler) return Boolean
   is
   begin
      Self.Ref_Count := Self.Ref_Count - 1;
      if Self.Ref_Count = 0 then
         return True;
      else
         return False;
      end if;
   end Dec_Ref;

   -----------------------------
   -- Unit_Requested_Callback --
   -----------------------------

   overriding procedure Unit_Requested_Callback
     (Self               : in out C_Event_Handler;
      Context            : Internal_Context;
      Name               : Text_Type;
      From               : Internal_Unit;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean)
   is
      Name_Access : constant Text_Cst_Access
        := Name'Unrestricted_Access;
   begin
      Self.Unit_Requested_Func
        (Self.Data,
         Context,
         Wrap (Name_Access),
         From,
         (if Found then 1 else 0),
         (if Is_Not_Found_Error then 1 else 0));
   end Unit_Requested_Callback;

   --------------------------
   -- Unit_Parsed_Callback --
   --------------------------

   overriding procedure Unit_Parsed_Callback
     (Self     : in out C_Event_Handler;
      Context  : Internal_Context;
      Unit     : Internal_Unit;
      Reparsed : Boolean)
   is
   begin
      Self.Unit_Parsed_Func
        (Self.Data, Context, Unit, (if Reparsed then 1 else 0));
   end Unit_Parsed_Callback;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Provider : in out C_Unit_Provider) is
   begin
      Provider.Destroy_Func (Provider.Data);
   end Finalize;

   -------------
   -- Inc_Ref --
   -------------

   overriding procedure Inc_Ref (Provider : in out C_Unit_Provider) is
   begin
      Provider.Ref_Count := Provider.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   overriding function Dec_Ref
     (Provider : in out C_Unit_Provider) return Boolean is
   begin
      Provider.Ref_Count := Provider.Ref_Count - 1;
      if Provider.Ref_Count = 0 then
         return True;
      else
         return False;
      end if;
   end Dec_Ref;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : C_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String
   is
      Name_Access : constant Text_Cst_Access := Name'Unrestricted_Access;

      C_Result : chars_ptr := Provider.Get_Unit_Filename_Func
        (Provider.Data, Wrap (Name_Access), Kind);
   begin
      if C_Result = Null_Ptr then
         raise Property_Error with "invalid AST node for unit name";
      else
         declare
            Result : constant String := Value (C_Result);
         begin
            Free (C_Result);
            return Result;
         end;
      end if;
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider : C_Unit_Provider;
      Context  : Internal_Context;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind;
      Charset  : String := "";
      Reparse  : Boolean := False) return Internal_Unit
   is
      Name_Access : constant Text_Cst_Access := Name'Unrestricted_Access;
      C_Charset   : chars_ptr := (if Charset'Length = 0
                                  then Null_Ptr
                                  else New_String (Charset));
   begin
      return C_Result : constant turkixir_analysis_unit :=
         Provider.Get_Unit_From_Name_Func
           (Provider.Data, Context, Wrap (Name_Access), Kind,
            C_Charset, Boolean'Pos (Reparse))
      do
         Free (C_Charset);
         if C_Result = null then
            raise Property_Error with "invalid AST node for unit name";
         end if;
      end return;
   end Get_Unit;

   


   ----------
   -- Wrap --
   ----------

   function Wrap (Token : Token_Reference) return turkixir_token is
   begin
      if Token = No_Token then
         return (Token_Data   => null,
                 Token_Index  => -1,
                 Trivia_Index => -1,
                 others       => <>);
      end if;

      declare
         D : constant Token_Data_Type := Data (Token);
         K : constant Token_Kind := Kind (D);

         Index : constant Token_Or_Trivia_Index := Get_Token_Index (Token);

         Source_Buffer : Text_Cst_Access;
         First         : Positive;
         Last          : Natural;
      begin
         Extract_Token_Text (D, Source_Buffer, First, Last);
         return (Context         => Get_Token_Context (Token),
                 Token_Data      => Get_Token_TDH (Token),
                 Token_Index     => int (Index.Token),
                 Trivia_Index    => int (Index.Trivia),
                 Kind            => K'Enum_Rep,
                 Text            => Wrap (Source_Buffer, First, Last),
                 Sloc_Range      => Wrap (Sloc_Range (D)));
      end;
   end Wrap;

   ------------
   -- Unwrap --
   ------------

   function Unwrap (Token : turkixir_token) return Token_Reference is
   begin
      return (if Token.Token_Data = null
              then No_Token
              else Wrap_Token_Reference
                     (Token.Context,
                      Token.Token_Data,
                      (Token  => Token_Index (Token.Token_Index),
                       Trivia => Token_Index (Token.Trivia_Index))));
   end Unwrap;

   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

           

   

   
   

   function turkixir_turkixir_node_parent
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity;
         begin
            Result := Libturkixirlang.Implementation.Parent (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_turkixir_node_parent;


           

   

   
   

   function turkixir_turkixir_node_parents
     (Node : turkixir_base_entity_Ptr;

         With_Self :
            
            turkixir_bool;

      Value_P : access turkixir_turkixir_node_array) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
         
         Unwrapped_With_Self : constant Boolean :=
               With_Self /= 0
         ;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Array_Access;
         begin
            Result := Libturkixirlang.Implementation.Parents (Unwrapped_Node, With_Self => Unwrapped_With_Self, E_Info => Node.Info);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_turkixir_node_parents;


           

   

   
   

   function turkixir_turkixir_node_children
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_turkixir_node_array) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Array_Access;
         begin
            Result := Libturkixirlang.Implementation.Children (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_turkixir_node_children;


           

   

   
   

   function turkixir_turkixir_node_token_start
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_token) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Token_Reference;
         begin
            Result := Libturkixirlang.Implementation.Token_Start (Unwrapped_Node);

            Value_P.all :=
                   Wrap (Result)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_turkixir_node_token_start;


           

   

   
   

   function turkixir_turkixir_node_token_end
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_token) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Token_Reference;
         begin
            Result := Libturkixirlang.Implementation.Token_End (Unwrapped_Node);

            Value_P.all :=
                   Wrap (Result)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_turkixir_node_token_end;


           

   

   
   

   function turkixir_turkixir_node_child_index
     (Node : turkixir_base_entity_Ptr;


      Value_P : access int) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Integer;
         begin
            Result := Libturkixirlang.Implementation.Child_Index (Unwrapped_Node);

            Value_P.all :=
                   int (Result)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_turkixir_node_child_index;


           

   

   
   

   function turkixir_turkixir_node_previous_sibling
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity;
         begin
            Result := Libturkixirlang.Implementation.Previous_Sibling (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_turkixir_node_previous_sibling;


           

   

   
   

   function turkixir_turkixir_node_next_sibling
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity;
         begin
            Result := Libturkixirlang.Implementation.Next_Sibling (Unwrapped_Node, E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_turkixir_node_next_sibling;


           

   

   
   

   function turkixir_turkixir_node_unit
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_analysis_unit) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Unit;
         begin
            Result := Libturkixirlang.Implementation.Unit (Unwrapped_Node);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_turkixir_node_unit;


           

   

   
   

   function turkixir_turkixir_node_is_ghost
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_bool) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Boolean;
         begin
            Result := Libturkixirlang.Implementation.Is_Ghost (Unwrapped_Node);

            Value_P.all :=
                   turkixir_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_turkixir_node_is_ghost;


           

   

   
   

   function turkixir_turkixir_node_full_sloc_image
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_string_type) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : String_Type;
         begin
            Result := Libturkixirlang.Implementation.Full_Sloc_Image (Unwrapped_Node);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_turkixir_node_full_sloc_image;


           

   

   
   

   function turkixir_arg_assoc_f_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Arg_Assoc_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Arg_Assoc_F_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_arg_assoc_f_name;


           

   

   
   

   function turkixir_arg_assoc_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Arg_Assoc_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Arg_Assoc_F_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_arg_assoc_f_expr;


           

   

   
   

   function turkixir_arg_gen_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Arg_Gen_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Arg_Gen_F_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_arg_gen_f_expr;


           

   

   
   

   function turkixir_arg_gen_f_comprehension
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Arg_Gen_Range then

         declare
            

            Result : Bare_Comp_For;
         begin
            Result := Arg_Gen_F_Comprehension (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_arg_gen_f_comprehension;


           

   

   
   

   function turkixir_kw_args_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Kw_Args_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Kw_Args_F_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_kw_args_f_expr;


           

   

   
   

   function turkixir_var_args_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Var_Args_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Var_Args_F_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_var_args_f_expr;


           

   

   
   

   function turkixir_as_name_node_f_imported
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_As_Name_Node_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := As_Name_Node_F_Imported (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_as_name_node_f_imported;


           

   

   
   

   function turkixir_as_name_node_f_as_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_As_Name_Node_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := As_Name_Node_F_As_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_as_name_node_f_as_name;


           

   

   
   

   function turkixir_comp_if_f_test
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Comp_If_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Comp_If_F_Test (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_comp_if_f_test;


           

   

   
   

   function turkixir_comp_if_f_comp
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Comp_If_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := Comp_If_F_Comp (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_comp_if_f_comp;


           

   

   
   

   function turkixir_comp_for_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Comp_For_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Comp_For_F_Exprs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_comp_for_f_exprs;


           

   

   
   

   function turkixir_comp_for_f_target
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Comp_For_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Comp_For_F_Target (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_comp_for_f_target;


           

   

   
   

   function turkixir_comp_for_f_comp
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Comp_For_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := Comp_For_F_Comp (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_comp_for_f_comp;


           

   

   
   

   function turkixir_comp_forl_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Comp_ForL_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Comp_ForL_F_Exprs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_comp_forl_f_exprs;


           

   

   
   

   function turkixir_comp_forl_f_target
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Comp_ForL_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Comp_ForL_F_Target (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_comp_forl_f_target;


           

   

   
   

   function turkixir_comp_forl_f_comp
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Comp_ForL_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := Comp_ForL_F_Comp (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_comp_forl_f_comp;


           

   

   
   

   function turkixir_decorator_f_dec_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Decorator_Range then

         declare
            

            Result : Bare_Name;
         begin
            Result := Decorator_F_Dec_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_decorator_f_dec_name;


           

   

   
   

   function turkixir_decorator_f_arg_list
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Decorator_Range then

         declare
            

            Result : Bare_Arg_List;
         begin
            Result := Decorator_F_Arg_List (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_decorator_f_arg_list;


           

   

   
   

   function turkixir_dict_assoc_f_key
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Dict_Assoc_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Dict_Assoc_F_Key (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_dict_assoc_f_key;


           

   

   
   

   function turkixir_dict_assoc_f_value
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Dict_Assoc_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Dict_Assoc_F_Value (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_dict_assoc_f_value;


           

   

   
   

   function turkixir_else_part_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Else_Part_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := Else_Part_F_Statements (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_else_part_f_statements;


           

   

   
   

   function turkixir_except_part_f_as_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Except_Part_Range then

         declare
            

            Result : Bare_As_Name_Node;
         begin
            Result := Except_Part_F_As_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_except_part_f_as_name;


           

   

   
   

   function turkixir_except_part_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Except_Part_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := Except_Part_F_Statements (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_except_part_f_statements;


           

   

   
   

   function turkixir_and_expr_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_And_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := And_Expr_F_Left (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_and_expr_f_left;


           

   

   
   

   function turkixir_and_expr_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_And_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := And_Expr_F_Right (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_and_expr_f_right;


           

   

   
   

   function turkixir_and_op_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_And_Op_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := And_Op_F_Left (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_and_op_f_left;


           

   

   
   

   function turkixir_and_op_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_And_Op_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := And_Op_F_Right (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_and_op_f_right;


           

   

   
   

   function turkixir_bin_op_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Bin_Op then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Bin_Op_F_Left (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_bin_op_f_left;


           

   

   
   

   function turkixir_bin_op_f_op
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Bin_Op then

         declare
            

            Result : Bare_Op;
         begin
            Result := Bin_Op_F_Op (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_bin_op_f_op;


           

   

   
   

   function turkixir_bin_op_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Bin_Op then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Bin_Op_F_Right (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_bin_op_f_right;


           

   

   
   

   function turkixir_call_expr_f_prefix
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Call_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Call_Expr_F_Prefix (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_call_expr_f_prefix;


           

   

   
   

   function turkixir_call_expr_f_suffix
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Call_Expr_Range then

         declare
            

            Result : Bare_Arg_List;
         begin
            Result := Call_Expr_F_Suffix (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_call_expr_f_suffix;


           

   

   
   

   function turkixir_comp_op_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Comp_Op_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Comp_Op_F_Left (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_comp_op_f_left;


           

   

   
   

   function turkixir_comp_op_f_op
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Comp_Op_Range then

         declare
            

            Result : Bare_Comp_Op_Kind;
         begin
            Result := Comp_Op_F_Op (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_comp_op_f_op;


           

   

   
   

   function turkixir_comp_op_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Comp_Op_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Comp_Op_F_Right (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_comp_op_f_right;


           

   

   
   

   function turkixir_concat_string_lit_f_first_str
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Concat_String_Lit_Range then

         declare
            

            Result : Bare_String_Lit;
         begin
            Result := Concat_String_Lit_F_First_Str (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_concat_string_lit_f_first_str;


           

   

   
   

   function turkixir_concat_string_lit_f_subsequent_str
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Concat_String_Lit_Range then

         declare
            

            Result : Bare_String_Lit_List;
         begin
            Result := Concat_String_Lit_F_Subsequent_Str (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_concat_string_lit_f_subsequent_str;


           

   

   
   

   function turkixir_dict_comp_f_assoc
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Dict_Comp_Range then

         declare
            

            Result : Bare_Dict_Assoc;
         begin
            Result := Dict_Comp_F_Assoc (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_dict_comp_f_assoc;


           

   

   
   

   function turkixir_dict_comp_f_comprehension
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Dict_Comp_Range then

         declare
            

            Result : Bare_Comp_For;
         begin
            Result := Dict_Comp_F_Comprehension (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_dict_comp_f_comprehension;


           

   

   
   

   function turkixir_dict_lit_f_assocs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Dict_Lit_Range then

         declare
            

            Result : Bare_Dict_Assoc_List;
         begin
            Result := Dict_Lit_F_Assocs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_dict_lit_f_assocs;


           

   

   
   

   function turkixir_factor_f_op
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Factor_Range then

         declare
            

            Result : Bare_Op;
         begin
            Result := Factor_F_Op (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_factor_f_op;


           

   

   
   

   function turkixir_factor_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Factor_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Factor_F_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_factor_f_expr;


           

   

   
   

   function turkixir_if_expr_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_If_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := If_Expr_F_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_if_expr_f_expr;


           

   

   
   

   function turkixir_if_expr_f_cond
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_If_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := If_Expr_F_Cond (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_if_expr_f_cond;


           

   

   
   

   function turkixir_if_expr_f_else_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_If_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := If_Expr_F_Else_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_if_expr_f_else_expr;


           

   

   
   

   function turkixir_inline_eval_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Inline_Eval_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Inline_Eval_F_Exprs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_inline_eval_f_exprs;


           

   

   
   

   function turkixir_lambda_def_f_args
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Lambda_Def_Range then

         declare
            

            Result : Bare_Params;
         begin
            Result := Lambda_Def_F_Args (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_lambda_def_f_args;


           

   

   
   

   function turkixir_lambda_def_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Lambda_Def_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Lambda_Def_F_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_lambda_def_f_expr;


           

   

   
   

   function turkixir_list_comp_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_List_Comp_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := List_Comp_F_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_list_comp_f_expr;


           

   

   
   

   function turkixir_list_comp_f_comprehension
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_List_Comp_Range then

         declare
            

            Result : Bare_Comp_ForL;
         begin
            Result := List_Comp_F_Comprehension (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_list_comp_f_comprehension;


           

   

   
   

   function turkixir_list_gen_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_List_Gen_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := List_Gen_F_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_list_gen_f_expr;


           

   

   
   

   function turkixir_list_gen_f_comprehension
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_List_Gen_Range then

         declare
            

            Result : Bare_Comp_ForL;
         begin
            Result := List_Gen_F_Comprehension (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_list_gen_f_comprehension;


           

   

   
   

   function turkixir_list_lit_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_List_Lit_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := List_Lit_F_Exprs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_list_lit_f_exprs;


           

   

   
   

   function turkixir_dotted_name_f_prefix
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Dotted_Name_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Dotted_Name_F_Prefix (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_dotted_name_f_prefix;


           

   

   
   

   function turkixir_dotted_name_f_suffix
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Dotted_Name_Range then

         declare
            

            Result : Bare_Id;
         begin
            Result := Dotted_Name_F_Suffix (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_dotted_name_f_suffix;


           

   

   
   

   function turkixir_not_op_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Not_Op_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Not_Op_F_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_not_op_f_expr;


           

   

   
   

   function turkixir_or_expr_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Or_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Or_Expr_F_Left (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_or_expr_f_left;


           

   

   
   

   function turkixir_or_expr_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Or_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Or_Expr_F_Right (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_or_expr_f_right;


           

   

   
   

   function turkixir_or_op_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Or_Op_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Or_Op_F_Left (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_or_op_f_left;


           

   

   
   

   function turkixir_or_op_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Or_Op_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Or_Op_F_Right (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_or_op_f_right;


           

   

   
   

   function turkixir_power_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Power_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Power_F_Left (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_power_f_left;


           

   

   
   

   function turkixir_power_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Power_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Power_F_Right (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_power_f_right;


           

   

   
   

   function turkixir_set_comp_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Set_Comp_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Set_Comp_F_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_set_comp_f_expr;


           

   

   
   

   function turkixir_set_comp_f_comprehension
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Set_Comp_Range then

         declare
            

            Result : Bare_Comp_For;
         begin
            Result := Set_Comp_F_Comprehension (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_set_comp_f_comprehension;


           

   

   
   

   function turkixir_set_lit_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Set_Lit_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Set_Lit_F_Exprs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_set_lit_f_exprs;


           

   

   
   

   function turkixir_slice_expr_f_first
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Slice_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Slice_Expr_F_First (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_slice_expr_f_first;


           

   

   
   

   function turkixir_slice_expr_f_last
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Slice_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Slice_Expr_F_Last (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_slice_expr_f_last;


           

   

   
   

   function turkixir_ext_slice_expr_f_stride
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Ext_Slice_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Ext_Slice_Expr_F_Stride (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_ext_slice_expr_f_stride;


           

   

   
   

   function turkixir_subscript_expr_f_prefix
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Subscript_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Subscript_Expr_F_Prefix (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_subscript_expr_f_prefix;


           

   

   
   

   function turkixir_subscript_expr_f_suffix
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Subscript_Expr_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Subscript_Expr_F_Suffix (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_subscript_expr_f_suffix;


           

   

   
   

   function turkixir_tuple_lit_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Tuple_Lit_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Tuple_Lit_F_Exprs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_tuple_lit_f_exprs;


           

   

   
   

   function turkixir_xor_expr_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Xor_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Xor_Expr_F_Left (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_xor_expr_f_left;


           

   

   
   

   function turkixir_xor_expr_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Xor_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Xor_Expr_F_Right (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_xor_expr_f_right;


           

   

   
   

   function turkixir_yield_expr_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Yield_Expr_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Yield_Expr_F_Exprs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_yield_expr_f_exprs;


           

   

   
   

   function turkixir_file_node_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_File_Node_Range then

         declare
            

            Result : Bare_Turkixir_Node_List;
         begin
            Result := File_Node_F_Statements (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_file_node_f_statements;


           

   

   
   

   function turkixir_kw_args_flag_p_as_bool
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_bool) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Kw_Args_Flag then

         declare
            

            Result : Boolean;
         begin
            Result := Libturkixirlang.Implementation.Dispatcher_Kw_Args_Flag_P_As_Bool (Unwrapped_Node);

            Value_P.all :=
                   turkixir_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_kw_args_flag_p_as_bool;


           

   

   
   

   function turkixir_params_f_single_params
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Params_Range then

         declare
            

            Result : Bare_Single_Param_List;
         begin
            Result := Params_F_Single_Params (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_params_f_single_params;


           

   

   
   

   function turkixir_rel_name_f_dots
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Rel_Name_Range then

         declare
            

            Result : Bare_Dot_List;
         begin
            Result := Rel_Name_F_Dots (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_rel_name_f_dots;


           

   

   
   

   function turkixir_rel_name_f_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Rel_Name_Range then

         declare
            

            Result : Bare_Name;
         begin
            Result := Rel_Name_F_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_rel_name_f_name;


           

   

   
   

   function turkixir_single_param_f_is_varargs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Single_Param_Range then

         declare
            

            Result : Bare_Var_Args_Flag;
         begin
            Result := Single_Param_F_Is_Varargs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_single_param_f_is_varargs;


           

   

   
   

   function turkixir_single_param_f_is_kwargs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Single_Param_Range then

         declare
            

            Result : Bare_Kw_Args_Flag;
         begin
            Result := Single_Param_F_Is_Kwargs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_single_param_f_is_kwargs;


           

   

   
   

   function turkixir_single_param_f_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Single_Param_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := Single_Param_F_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_single_param_f_name;


           

   

   
   

   function turkixir_single_param_f_default_value
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Single_Param_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Single_Param_F_Default_Value (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_single_param_f_default_value;


           

   

   
   

   function turkixir_assert_stmt_f_test_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Assert_Stmt_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Assert_Stmt_F_Test_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_assert_stmt_f_test_expr;


           

   

   
   

   function turkixir_assert_stmt_f_msg
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Assert_Stmt_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Assert_Stmt_F_Msg (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_assert_stmt_f_msg;


           

   

   
   

   function turkixir_assign_stmt_f_l_value
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Assign_Stmt_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Assign_Stmt_F_L_Value (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_assign_stmt_f_l_value;


           

   

   
   

   function turkixir_assign_stmt_f_r_values
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Assign_Stmt_Range then

         declare
            

            Result : Bare_Turkixir_Node_List;
         begin
            Result := Assign_Stmt_F_R_Values (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_assign_stmt_f_r_values;


           

   

   
   

   function turkixir_aug_assign_stmt_f_l_value
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Aug_Assign_Stmt_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Aug_Assign_Stmt_F_L_Value (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_aug_assign_stmt_f_l_value;


           

   

   
   

   function turkixir_aug_assign_stmt_f_op
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Aug_Assign_Stmt_Range then

         declare
            

            Result : Bare_Op;
         begin
            Result := Aug_Assign_Stmt_F_Op (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_aug_assign_stmt_f_op;


           

   

   
   

   function turkixir_aug_assign_stmt_f_r_value
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Aug_Assign_Stmt_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := Aug_Assign_Stmt_F_R_Value (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_aug_assign_stmt_f_r_value;


           

   

   
   

   function turkixir_decorated_f_decorators
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Decorated_Range then

         declare
            

            Result : Bare_Decorator_List;
         begin
            Result := Decorated_F_Decorators (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_decorated_f_decorators;


           

   

   
   

   function turkixir_decorated_f_defn
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Decorated_Range then

         declare
            

            Result : Bare_Def_Stmt;
         begin
            Result := Decorated_F_Defn (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_decorated_f_defn;


           

   

   
   

   function turkixir_class_def_f_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Class_Def_Range then

         declare
            

            Result : Bare_Id;
         begin
            Result := Class_Def_F_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_class_def_f_name;


           

   

   
   

   function turkixir_class_def_f_bases
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Class_Def_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Class_Def_F_Bases (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_class_def_f_bases;


           

   

   
   

   function turkixir_class_def_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Class_Def_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := Class_Def_F_Statements (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_class_def_f_statements;


           

   

   
   

   function turkixir_func_def_f_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Func_Def_Range then

         declare
            

            Result : Bare_Id;
         begin
            Result := Func_Def_F_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_func_def_f_name;


           

   

   
   

   function turkixir_func_def_f_parameters
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Func_Def_Range then

         declare
            

            Result : Bare_Params;
         begin
            Result := Func_Def_F_Parameters (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_func_def_f_parameters;


           

   

   
   

   function turkixir_func_def_f_body
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Func_Def_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := Func_Def_F_Body (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_func_def_f_body;


           

   

   
   

   function turkixir_del_stmt_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Del_Stmt_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Del_Stmt_F_Exprs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_del_stmt_f_exprs;


           

   

   
   

   function turkixir_elif_branch_f_cond_test
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Elif_Branch_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Elif_Branch_F_Cond_Test (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_elif_branch_f_cond_test;


           

   

   
   

   function turkixir_elif_branch_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Elif_Branch_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := Elif_Branch_F_Statements (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_elif_branch_f_statements;


           

   

   
   

   function turkixir_exec_stmt_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Exec_Stmt_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Exec_Stmt_F_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_exec_stmt_f_expr;


           

   

   
   

   function turkixir_exec_stmt_f_in_list
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Exec_Stmt_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Exec_Stmt_F_In_List (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_exec_stmt_f_in_list;


           

   

   
   

   function turkixir_for_stmt_f_bindings
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_For_Stmt_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := For_Stmt_F_Bindings (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_for_stmt_f_bindings;


           

   

   
   

   function turkixir_for_stmt_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_For_Stmt_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := For_Stmt_F_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_for_stmt_f_expr;


           

   

   
   

   function turkixir_for_stmt_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_For_Stmt_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := For_Stmt_F_Statements (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_for_stmt_f_statements;


           

   

   
   

   function turkixir_for_stmt_f_else_part
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_For_Stmt_Range then

         declare
            

            Result : Bare_Else_Part;
         begin
            Result := For_Stmt_F_Else_Part (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_for_stmt_f_else_part;


           

   

   
   

   function turkixir_global_stmt_f_names
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Global_Stmt_Range then

         declare
            

            Result : Bare_Id_List;
         begin
            Result := Global_Stmt_F_Names (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_global_stmt_f_names;


           

   

   
   

   function turkixir_if_stmt_f_cond_test
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_If_Stmt_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := If_Stmt_F_Cond_Test (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_if_stmt_f_cond_test;


           

   

   
   

   function turkixir_if_stmt_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_If_Stmt_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := If_Stmt_F_Statements (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_if_stmt_f_statements;


           

   

   
   

   function turkixir_if_stmt_f_elif_branchs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_If_Stmt_Range then

         declare
            

            Result : Bare_Elif_Branch_List;
         begin
            Result := If_Stmt_F_Elif_Branchs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_if_stmt_f_elif_branchs;


           

   

   
   

   function turkixir_if_stmt_f_else_part
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_If_Stmt_Range then

         declare
            

            Result : Bare_Else_Part;
         begin
            Result := If_Stmt_F_Else_Part (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_if_stmt_f_else_part;


           

   

   
   

   function turkixir_import_from_f_rel_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Import_From_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := Import_From_F_Rel_Name (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_import_from_f_rel_name;


           

   

   
   

   function turkixir_import_from_f_imported
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Import_From_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := Import_From_F_Imported (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_import_from_f_imported;


           

   

   
   

   function turkixir_import_name_f_imported_names
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Import_Name_Range then

         declare
            

            Result : Bare_Turkixir_Node_List;
         begin
            Result := Import_Name_F_Imported_Names (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_import_name_f_imported_names;


           

   

   
   

   function turkixir_print_stmt_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Print_Stmt_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Print_Stmt_F_Exprs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_print_stmt_f_exprs;


           

   

   
   

   function turkixir_raise_stmt_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Raise_Stmt_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Raise_Stmt_F_Exprs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_raise_stmt_f_exprs;


           

   

   
   

   function turkixir_return_stmt_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Return_Stmt_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Return_Stmt_F_Exprs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_return_stmt_f_exprs;


           

   

   
   

   function turkixir_stream_print_stmt_f_stream_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Stream_Print_Stmt_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Stream_Print_Stmt_F_Stream_Expr (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_stream_print_stmt_f_stream_expr;


           

   

   
   

   function turkixir_stream_print_stmt_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Stream_Print_Stmt_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Stream_Print_Stmt_F_Exprs (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_stream_print_stmt_f_exprs;


           

   

   
   

   function turkixir_try_stmt_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Try_Stmt_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := Try_Stmt_F_Statements (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_try_stmt_f_statements;


           

   

   
   

   function turkixir_try_stmt_f_except_parts
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Try_Stmt_Range then

         declare
            

            Result : Bare_Except_Part_List;
         begin
            Result := Try_Stmt_F_Except_Parts (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_try_stmt_f_except_parts;


           

   

   
   

   function turkixir_try_stmt_f_else_part
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Try_Stmt_Range then

         declare
            

            Result : Bare_Else_Part;
         begin
            Result := Try_Stmt_F_Else_Part (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_try_stmt_f_else_part;


           

   

   
   

   function turkixir_try_stmt_f_finally_part
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Try_Stmt_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := Try_Stmt_F_Finally_Part (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_try_stmt_f_finally_part;


           

   

   
   

   function turkixir_while_stmt_f_cond_test
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_While_Stmt_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := While_Stmt_F_Cond_Test (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_while_stmt_f_cond_test;


           

   

   
   

   function turkixir_while_stmt_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_While_Stmt_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := While_Stmt_F_Statements (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_while_stmt_f_statements;


           

   

   
   

   function turkixir_while_stmt_f_else_part
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_While_Stmt_Range then

         declare
            

            Result : Bare_Else_Part;
         begin
            Result := While_Stmt_F_Else_Part (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_while_stmt_f_else_part;


           

   

   
   

   function turkixir_with_stmt_f_bindings
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_With_Stmt_Range then

         declare
            

            Result : Bare_As_Name_Node_List;
         begin
            Result := With_Stmt_F_Bindings (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_with_stmt_f_bindings;


           

   

   
   

   function turkixir_with_stmt_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_With_Stmt_Range then

         declare
            

            Result : Bare_Turkixir_Node;
         begin
            Result := With_Stmt_F_Statements (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_with_stmt_f_statements;


           

   

   
   

   function turkixir_var_args_flag_p_as_bool
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_bool) return int

   is
      Unwrapped_Node : constant Bare_Turkixir_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Turkixir_Var_Args_Flag then

         declare
            

            Result : Boolean;
         begin
            Result := Libturkixirlang.Implementation.Dispatcher_Var_Args_Flag_P_As_Bool (Unwrapped_Node);

            Value_P.all :=
                   turkixir_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end turkixir_var_args_flag_p_as_bool;



         






         



function turkixir_turkixir_node_array_create (Length : int) return Internal_Entity_Array_Access is
begin
   Clear_Last_Exception;
   return Create_Internal_Entity_Array (Natural (Length));
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
      return null;
end turkixir_turkixir_node_array_create;

procedure turkixir_turkixir_node_array_inc_ref (A : Internal_Entity_Array_Access) is
begin
   Clear_Last_Exception;
   Inc_Ref (A);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end;

procedure turkixir_turkixir_node_array_dec_ref (A : Internal_Entity_Array_Access) is
begin
   Clear_Last_Exception;
   declare
      A_Var : Internal_Entity_Array_Access := A;
   begin
      Dec_Ref (A_Var);
   end;
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end;




end Libturkixirlang.Implementation.C;
