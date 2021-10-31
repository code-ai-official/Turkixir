







with Ada.Containers;            use Ada.Containers;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");
with Ada.Unchecked_Conversion;

with GNATCOLL.Traces;

pragma Warnings (Off, "referenced");
with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Debug;
use Langkit_Support.Adalog.Debug;
with Langkit_Support.Adalog.Operations;
use Langkit_Support.Adalog.Operations;
with Langkit_Support.Adalog.Predicates;
use Langkit_Support.Adalog.Predicates;
with Langkit_Support.Adalog.Pure_Relations;
use Langkit_Support.Adalog.Pure_Relations;
with Langkit_Support.Symbols;      use Langkit_Support.Symbols;
pragma Warnings (On, "referenced");

with Langkit_Support.Types;        use Langkit_Support.Types;

with Libturkixirlang.Common;
with Libturkixirlang.Private_Converters;
use Libturkixirlang.Private_Converters;
with Libturkixirlang.Public_Converters; use Libturkixirlang.Public_Converters;




package body Libturkixirlang.Analysis is

   use Libturkixirlang.Implementation;
   use AST_Envs;

      


      
      function To_Public_Turkixir_Node_Array
         (Value : Internal_Entity_Array_Access) return Turkixir_Node_Array;


      


      


      


      



      

      

      

      

      

      



   ----------------
   -- Do_Release --
   ----------------

   procedure Do_Release (Self : in out Event_Handler_Interface'Class) is
   begin
      Self.Release;
   end Do_Release;

   ----------------
   -- Do_Release --
   ----------------

   procedure Do_Release (Provider : in out Unit_Provider_Interface'Class) is
   begin
      Provider.Release;
   end Do_Release;

   ------------------------------------
   -- Create_Unit_Provider_Reference --
   ------------------------------------

   function Create_Unit_Provider_Reference
     (Provider : Unit_Provider_Interface'Class) return Unit_Provider_Reference
   is
   begin
      return Result : Unit_Provider_Reference do
         Result.Set (Provider);
      end return;
   end Create_Unit_Provider_Reference;

   ------------------------------------
   -- Create_Event_Handler_Reference --
   ------------------------------------

   function Create_Event_Handler_Reference
     (Handler : Event_Handler_Interface'Class) return Event_Handler_Reference
   is
   begin
      return Result : Event_Handler_Reference do
         Result.Set (Handler);
      end return;
   end Create_Event_Handler_Reference;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Charset       : String := Default_Charset;
      File_Reader   : File_Reader_Reference := No_File_Reader_Reference;
      Unit_Provider : Unit_Provider_Reference := No_Unit_Provider_Reference;
      Event_Handler : Event_Handler_Reference := No_Event_Handler_Ref;
      With_Trivia   : Boolean := True;
      Tab_Stop      : Positive := 8)
      return Analysis_Context
   is
      use Unit_Provider_References;

      FR     : Internal_File_Reader_Access :=
         Wrap_Public_File_Reader (File_Reader);
      UP     : Internal_Unit_Provider_Access :=
         Wrap_Public_Provider (Unit_Provider);
      EH     : Internal_Event_Handler_Access :=
         Wrap_Public_Event_Handler (Event_Handler);
      Result : Internal_Context := Create_Context
        (Charset, FR, UP, EH, With_Trivia, Tab_Stop);
   begin
      --  Create_Context created ownership shares for itself, so don't forget
      --  to remove the shares on FR and UP.
      Dec_Ref (FR);
      Dec_Ref (UP);
      Dec_Ref (EH);

      return Context : constant Analysis_Context := Wrap_Context (Result)
      do
         --  Result has one ownership share and the call to Wrap_Context
         --  creates a new one, so don't forget to dec-ref before returning.
         Dec_Ref (Result);
      end return;
   end Create_Context;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit
     (Context       : Analysis_Context'Class;
      Unit_Filename : String) return Boolean is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      return Has_Unit (Unwrap_Context (Context), Unit_Filename);
   end Has_Unit;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      return Wrap_Unit
        (Get_From_File (Unwrap_Context (Context), Filename, Charset,
                        Reparse, Rule));
   end Get_From_File;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      return Wrap_Unit
        (Get_From_Buffer (Unwrap_Context (Context), Filename, Charset,
                          Buffer, Rule));
   end Get_From_Buffer;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : Ada.Strings.Unbounded.Unbounded_String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit
   is
      Bytes       : Big_String_Access;
      Bytes_Count : Natural;
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      Get_String (Buffer, Bytes, Bytes_Count);
      return Wrap_Unit
        (Get_From_Buffer (Unwrap_Context (Context), Filename, Charset,
                          Bytes (1 .. Bytes_Count), Rule));
   end Get_From_Buffer;

   --------------------
   -- Get_With_Error --
   --------------------

   function Get_With_Error
     (Context  : Analysis_Context'Class;
      Filename : String;
      Error    : Text_Type;
      Charset  : String := "";
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit
   is
      Result : Internal_Unit;
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      Result := Implementation.Get_With_Error
        (Unwrap_Context (Context), Filename, Error, Charset, Rule);
      return Wrap_Unit (Result);
   end Get_With_Error;


   -------------------
   -- Unit_Provider --
   -------------------

   function Unit_Provider
     (Context : Analysis_Context'Class) return Unit_Provider_Reference
   is
      Provider : Internal_Unit_Provider_Access;
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      --  By design, Unit_Provider_Wrapper is supposed to be the only
      --  implementation of the Internal_Unit_Provider interface.
      Provider := Unit_Provider (Unwrap_Context (Context));
      if Provider.all not in Unit_Provider_Wrapper'Class then
         raise Program_Error;
      end if;

      return Unit_Provider_Wrapper (Provider.all).Internal;
   end Unit_Provider;

   ----------
   -- Hash --
   ----------

   function Hash (Context : Analysis_Context) return Ada.Containers.Hash_Type
   is
   begin
      return Hash (Unwrap_Context (Context));
   end Hash;

   ---------------------
   -- Has_With_Trivia --
   ---------------------

   function Has_With_Trivia (Context : Analysis_Context'Class) return Boolean
   is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      return Has_With_Trivia (Unwrap_Context (Context));
   end Has_With_Trivia;

   --------------------------------------------
   -- Discard_Errors_In_Populate_Lexical_Env --
   --------------------------------------------

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Analysis_Context'Class; Discard : Boolean) is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      Discard_Errors_In_Populate_Lexical_Env
        (Unwrap_Context (Context), Discard);
   end Discard_Errors_In_Populate_Lexical_Env;

   ----------------------------------
   -- Set_Logic_Resolution_Timeout --
   ----------------------------------

   procedure Set_Logic_Resolution_Timeout
     (Context : Analysis_Context'Class; Timeout : Natural) is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      Set_Logic_Resolution_Timeout (Unwrap_Context (Context), Timeout);
   end Set_Logic_Resolution_Timeout;

   ---------------------------
   -- Set_Lookup_Cache_Mode --
   ---------------------------

   procedure Set_Lookup_Cache_Mode (Mode : Lookup_Cache_Kind) is
   begin
      Lookup_Cache_Mode := Mode;
   end Set_Lookup_Cache_Mode;

   --------------------------
   -- Has_Rewriting_Handle --
   --------------------------

   function Has_Rewriting_Handle
     (Context : Analysis_Context'Class) return Boolean is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      return Has_Rewriting_Handle (Unwrap_Context (Context));
   end Has_Rewriting_Handle;

   ----------------------
   -- Get_Symbol_Table --
   ----------------------

   function Get_Symbol_Table
     (Context : Analysis_Context'Class) return Symbol_Table
   is
   begin
      return Context.Internal.Symbols;
   end Get_Symbol_Table;

   -------------
   -- Context --
   -------------

   function Context (Unit : Analysis_Unit'Class) return Analysis_Context is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Wrap_Context (Context (Unwrap_Unit (Unit)));
   end Context;

   ----------
   -- Hash --
   ----------

   function Hash (Unit : Analysis_Unit) return Ada.Containers.Hash_Type
   is
   begin
      return Hash (Unwrap_Unit (Unit));
   end Hash;

   -------------
   -- Reparse --
   -------------

   procedure Reparse (Unit : Analysis_Unit'Class; Charset : String := "") is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Reparse (Unwrap_Unit (Unit), Charset);
   end Reparse;

   -------------
   -- Reparse --
   -------------

   procedure Reparse
     (Unit : Analysis_Unit'Class; Charset : String := ""; Buffer  : String) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Reparse (Unwrap_Unit (Unit), Charset, Buffer);
   end Reparse;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env (Unit : Analysis_Unit'Class) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Populate_Lexical_Env (Unwrap_Unit (Unit));
   end Populate_Lexical_Env;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Unit : Analysis_Unit'Class) return String is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Get_Filename (Unwrap_Unit (Unit));
   end Get_Filename;

   -----------------
   -- Get_Charset --
   -----------------

   function Get_Charset (Unit : Analysis_Unit'Class) return String is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Get_Charset (Unwrap_Unit (Unit));
   end Get_Charset;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics (Unit : Analysis_Unit'Class) return Boolean is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Has_Diagnostics (Unwrap_Unit (Unit));
   end Has_Diagnostics;

   -----------------
   -- Diagnostics --
   -----------------

   function Diagnostics (Unit : Analysis_Unit'Class) return Diagnostics_Array
   is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Implementation.Diagnostics (Unwrap_Unit (Unit));
   end Diagnostics;

   ---------------------------
   -- Format_GNU_Diagnostic --
   ---------------------------

   function Format_GNU_Diagnostic
     (Unit : Analysis_Unit'Class; D : Diagnostic) return String is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Format_GNU_Diagnostic (Unwrap_Unit (Unit), D);
   end Format_GNU_Diagnostic;

   ----------
   -- Root --
   ----------

   function Root (Unit : Analysis_Unit'Class) return Turkixir_Node is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Wrap_Node (Root (Unwrap_Unit (Unit)));
   end Root;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Unit : Analysis_Unit'Class) return Token_Reference is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return First_Token (Unwrap_Unit (Unit));
   end First_Token;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Unit : Analysis_Unit'Class) return Token_Reference is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Last_Token (Unwrap_Unit (Unit));
   end Last_Token;

   -----------------
   -- Token_Count --
   -----------------

   function Token_Count (Unit : Analysis_Unit'Class) return Natural is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Token_Count (Unwrap_Unit (Unit));
   end Token_Count;

   ------------------
   -- Trivia_Count --
   ------------------

   function Trivia_Count (Unit : Analysis_Unit'Class) return Natural is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Trivia_Count (Unwrap_Unit (Unit));
   end Trivia_Count;

   ----------
   -- Text --
   ----------

   function Text (Unit : Analysis_Unit'Class) return Text_Type is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Implementation.Text (Unwrap_Unit (Unit));
   end Text;

   ------------------
   -- Lookup_Token --
   ------------------

   function Lookup_Token
     (Unit : Analysis_Unit'Class; Sloc : Source_Location)
      return Token_Reference is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Lookup_Token (Unwrap_Unit (Unit), Sloc);
   end Lookup_Token;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Unit : Analysis_Unit; Line_Number : Positive) return Text_Type is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Get_Line (Unwrap_Unit (Unit), Line_Number);
   end Get_Line;

   ----------------------
   -- Dump_Lexical_Env --
   ----------------------

   procedure Dump_Lexical_Env (Unit : Analysis_Unit'Class) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Dump_Lexical_Env (Unwrap_Unit (Unit));
   end Dump_Lexical_Env;

   ------------------------
   -- Trigger_Envs_Debug --
   ------------------------

   procedure Trigger_Envs_Debug (Is_Active : Boolean) is
   begin
      Langkit_Support.Lexical_Envs.Me.Set_Active (Is_Active);
   end Trigger_Envs_Debug;

   -----------
   -- Print --
   -----------

   procedure Print (Unit : Analysis_Unit'Class; Show_Slocs : Boolean := True)
   is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Print (Unwrap_Unit (Unit), Show_Slocs);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia (Unit : Analysis_Unit'Class) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      PP_Trivia (Unwrap_Unit (Unit));
   end PP_Trivia;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Node : Turkixir_Node'Class) return Boolean is
     (Node.Internal.Node = null);

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Node : Turkixir_Node'Class) return Boolean
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Is_Token_Node (Node.Internal.Node);
   end Is_Token_Node;

   ------------------
   -- Is_Synthetic --
   ------------------

   function Is_Synthetic (Node : Turkixir_Node'Class) return Boolean
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Is_Synthetic (Node.Internal.Node);
   end Is_Synthetic;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Turkixir_Node'Class) return Boolean is
   begin
      Check_Safety_Net (L);
      Check_Safety_Net (R);
      return Compare_Entity (L.Internal, R.Internal);
   end "=";

   -----------
   -- Image --
   -----------

   function Image (Node : Turkixir_Node'Class) return String is
   begin
      Check_Safety_Net (Node);
      return Image (Node.Internal);
   end Image;

   -----------------------
   -- Entity converters --
   -----------------------

      function As_Turkixir_Node
        (Node : Turkixir_Node'Class) return Turkixir_Node
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Turkixir_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         

      end;
      function As_Expr
        (Node : Turkixir_Node'Class) return Expr
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Expr then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Expr";
         
            end if;
      end;
      function As_And_Expr
        (Node : Turkixir_Node'Class) return And_Expr
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_And_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_And_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to AndExpr";
         
            end if;
      end;
      function As_And_Op
        (Node : Turkixir_Node'Class) return And_Op
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_And_Op;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_And_Op_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to AndOp";
         
            end if;
      end;
      function As_Arg
        (Node : Turkixir_Node'Class) return Arg
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Arg;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Arg then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Arg";
         
            end if;
      end;
      function As_Arg_Assoc
        (Node : Turkixir_Node'Class) return Arg_Assoc
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Arg_Assoc;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Arg_Assoc_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ArgAssoc";
         
            end if;
      end;
      function As_Arg_Gen
        (Node : Turkixir_Node'Class) return Arg_Gen
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Arg_Gen;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Arg_Gen_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ArgGen";
         
            end if;
      end;
      function As_Turkixir_Node_Base_List
        (Node : Turkixir_Node'Class) return Turkixir_Node_Base_List
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Turkixir_Node_Base_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Turkixir_Node_Base_List then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to TurkixirNodeBaseList";
         
            end if;
      end;
      function As_Arg_List
        (Node : Turkixir_Node'Class) return Arg_List
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Arg_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Arg_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Arg.list";
         
            end if;
      end;
      function As_Bin_Op
        (Node : Turkixir_Node'Class) return Bin_Op
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Bin_Op;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Bin_Op then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BinOp";
         
            end if;
      end;
      function As_Arith_Expr
        (Node : Turkixir_Node'Class) return Arith_Expr
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Arith_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Arith_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ArithExpr";
         
            end if;
      end;
      function As_As_Name_Node
        (Node : Turkixir_Node'Class) return As_Name_Node
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_As_Name_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_As_Name_Node_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to AsNameNode";
         
            end if;
      end;
      function As_As_Name_Node_List
        (Node : Turkixir_Node'Class) return As_Name_Node_List
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_As_Name_Node_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_As_Name_Node_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to AsNameNode.list";
         
            end if;
      end;
      function As_Stmt
        (Node : Turkixir_Node'Class) return Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Stmt then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Stmt";
         
            end if;
      end;
      function As_Assert_Stmt
        (Node : Turkixir_Node'Class) return Assert_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Assert_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Assert_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to AssertStmt";
         
            end if;
      end;
      function As_Assign_Stmt
        (Node : Turkixir_Node'Class) return Assign_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Assign_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Assign_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to AssignStmt";
         
            end if;
      end;
      function As_Aug_Assign_Stmt
        (Node : Turkixir_Node'Class) return Aug_Assign_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Aug_Assign_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Aug_Assign_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to AugAssignStmt";
         
            end if;
      end;
      function As_Break_Stmt
        (Node : Turkixir_Node'Class) return Break_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Break_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Break_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to BreakStmt";
         
            end if;
      end;
      function As_Call_Expr
        (Node : Turkixir_Node'Class) return Call_Expr
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Call_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Call_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CallExpr";
         
            end if;
      end;
      function As_Def_Stmt
        (Node : Turkixir_Node'Class) return Def_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Def_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Def_Stmt then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to DefStmt";
         
            end if;
      end;
      function As_Class_Def
        (Node : Turkixir_Node'Class) return Class_Def
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Class_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Class_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ClassDef";
         
            end if;
      end;
      function As_Comprehension
        (Node : Turkixir_Node'Class) return Comprehension
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comprehension;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comprehension then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Comprehension";
         
            end if;
      end;
      function As_Comp_For
        (Node : Turkixir_Node'Class) return Comp_For
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_For;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_For_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompFor";
         
            end if;
      end;
      function As_Comp_ForL
        (Node : Turkixir_Node'Class) return Comp_ForL
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_ForL;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_ForL_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompForL";
         
            end if;
      end;
      function As_Comp_If
        (Node : Turkixir_Node'Class) return Comp_If
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_If;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_If_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompIf";
         
            end if;
      end;
      function As_Comp_Op
        (Node : Turkixir_Node'Class) return Comp_Op
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_Op;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_Op_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompOp";
         
            end if;
      end;
      function As_Comp_Op_Kind
        (Node : Turkixir_Node'Class) return Comp_Op_Kind
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_Op_Kind;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_Op_Kind then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompOpKind";
         
            end if;
      end;
      function As_Comp_Op_Kind_Diamond
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Diamond
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_Op_Kind_Diamond;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_Op_Kind_Diamond_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompOpKind.Diamond";
         
            end if;
      end;
      function As_Comp_Op_Kind_Eq
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Eq
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_Op_Kind_Eq;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_Op_Kind_Eq_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompOpKind.Eq";
         
            end if;
      end;
      function As_Comp_Op_Kind_Gt
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Gt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_Op_Kind_Gt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_Op_Kind_Gt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompOpKind.Gt";
         
            end if;
      end;
      function As_Comp_Op_Kind_Gte
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Gte
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_Op_Kind_Gte;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_Op_Kind_Gte_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompOpKind.Gte";
         
            end if;
      end;
      function As_Comp_Op_Kind_In
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_In
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_Op_Kind_In;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_Op_Kind_In_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompOpKind.In";
         
            end if;
      end;
      function As_Comp_Op_Kind_Is
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Is
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_Op_Kind_Is;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_Op_Kind_Is_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompOpKind.Is";
         
            end if;
      end;
      function As_Comp_Op_Kind_Isnot
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Isnot
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_Op_Kind_Isnot;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_Op_Kind_Isnot_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompOpKind.Isnot";
         
            end if;
      end;
      function As_Comp_Op_Kind_Lt
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Lt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_Op_Kind_Lt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_Op_Kind_Lt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompOpKind.Lt";
         
            end if;
      end;
      function As_Comp_Op_Kind_Lte
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Lte
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_Op_Kind_Lte;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_Op_Kind_Lte_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompOpKind.Lte";
         
            end if;
      end;
      function As_Comp_Op_Kind_Noteq
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Noteq
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_Op_Kind_Noteq;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_Op_Kind_Noteq_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompOpKind.Noteq";
         
            end if;
      end;
      function As_Comp_Op_Kind_Notin
        (Node : Turkixir_Node'Class) return Comp_Op_Kind_Notin
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Comp_Op_Kind_Notin;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Comp_Op_Kind_Notin_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompOpKind.Notin";
         
            end if;
      end;
      function As_Concat_String_Lit
        (Node : Turkixir_Node'Class) return Concat_String_Lit
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Concat_String_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Concat_String_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ConcatStringLit";
         
            end if;
      end;
      function As_Continue_Stmt
        (Node : Turkixir_Node'Class) return Continue_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Continue_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Continue_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ContinueStmt";
         
            end if;
      end;
      function As_Decorated
        (Node : Turkixir_Node'Class) return Decorated
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Decorated;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Decorated_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Decorated";
         
            end if;
      end;
      function As_Decorator
        (Node : Turkixir_Node'Class) return Decorator
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Decorator;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Decorator_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Decorator";
         
            end if;
      end;
      function As_Decorator_List
        (Node : Turkixir_Node'Class) return Decorator_List
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Decorator_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Decorator_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Decorator.list";
         
            end if;
      end;
      function As_Del_Stmt
        (Node : Turkixir_Node'Class) return Del_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Del_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Del_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to DelStmt";
         
            end if;
      end;
      function As_Dict_Assoc
        (Node : Turkixir_Node'Class) return Dict_Assoc
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Dict_Assoc;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Dict_Assoc_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to DictAssoc";
         
            end if;
      end;
      function As_Dict_Assoc_List
        (Node : Turkixir_Node'Class) return Dict_Assoc_List
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Dict_Assoc_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Dict_Assoc_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to DictAssoc.list";
         
            end if;
      end;
      function As_Dict_Comp
        (Node : Turkixir_Node'Class) return Dict_Comp
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Dict_Comp;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Dict_Comp_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to DictComp";
         
            end if;
      end;
      function As_Dict_Lit
        (Node : Turkixir_Node'Class) return Dict_Lit
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Dict_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Dict_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to DictLit";
         
            end if;
      end;
      function As_Dot
        (Node : Turkixir_Node'Class) return Dot
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Dot;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Dot_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Dot";
         
            end if;
      end;
      function As_Dot_List
        (Node : Turkixir_Node'Class) return Dot_List
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Dot_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Dot_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Dot.list";
         
            end if;
      end;
      function As_Name
        (Node : Turkixir_Node'Class) return Name
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Name;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Name then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Name";
         
            end if;
      end;
      function As_Dotted_Name
        (Node : Turkixir_Node'Class) return Dotted_Name
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Dotted_Name;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Dotted_Name_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to DottedName";
         
            end if;
      end;
      function As_Elif_Branch
        (Node : Turkixir_Node'Class) return Elif_Branch
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Elif_Branch;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Elif_Branch_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ElifBranch";
         
            end if;
      end;
      function As_Elif_Branch_List
        (Node : Turkixir_Node'Class) return Elif_Branch_List
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Elif_Branch_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Elif_Branch_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ElifBranch.list";
         
            end if;
      end;
      function As_Ellipsis_Expr
        (Node : Turkixir_Node'Class) return Ellipsis_Expr
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ellipsis_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Ellipsis_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to EllipsisExpr";
         
            end if;
      end;
      function As_Else_Part
        (Node : Turkixir_Node'Class) return Else_Part
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Else_Part;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Else_Part_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ElsePart";
         
            end if;
      end;
      function As_Except_Part
        (Node : Turkixir_Node'Class) return Except_Part
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Except_Part;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Except_Part_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExceptPart";
         
            end if;
      end;
      function As_Except_Part_List
        (Node : Turkixir_Node'Class) return Except_Part_List
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Except_Part_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Except_Part_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExceptPart.list";
         
            end if;
      end;
      function As_Exec_Stmt
        (Node : Turkixir_Node'Class) return Exec_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Exec_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Exec_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExecStmt";
         
            end if;
      end;
      function As_Expr_List
        (Node : Turkixir_Node'Class) return Expr_List
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Expr_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Expr_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Expr.list";
         
            end if;
      end;
      function As_Slice_Expr
        (Node : Turkixir_Node'Class) return Slice_Expr
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Slice_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Slice_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SliceExpr";
         
            end if;
      end;
      function As_Ext_Slice_Expr
        (Node : Turkixir_Node'Class) return Ext_Slice_Expr
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ext_Slice_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Ext_Slice_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExtSliceExpr";
         
            end if;
      end;
      function As_Factor
        (Node : Turkixir_Node'Class) return Factor
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Factor;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Factor_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Factor";
         
            end if;
      end;
      function As_File_Node
        (Node : Turkixir_Node'Class) return File_Node
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_File_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_File_Node_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to FileNode";
         
            end if;
      end;
      function As_For_Stmt
        (Node : Turkixir_Node'Class) return For_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_For_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_For_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ForStmt";
         
            end if;
      end;
      function As_Func_Def
        (Node : Turkixir_Node'Class) return Func_Def
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Func_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Func_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to FuncDef";
         
            end if;
      end;
      function As_Global_Stmt
        (Node : Turkixir_Node'Class) return Global_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Global_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Global_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to GlobalStmt";
         
            end if;
      end;
      function As_Id
        (Node : Turkixir_Node'Class) return Id
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Id;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Id_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Id";
         
            end if;
      end;
      function As_Id_List
        (Node : Turkixir_Node'Class) return Id_List
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Id_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Id_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Id.list";
         
            end if;
      end;
      function As_If_Expr
        (Node : Turkixir_Node'Class) return If_Expr
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_If_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_If_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to IfExpr";
         
            end if;
      end;
      function As_If_Stmt
        (Node : Turkixir_Node'Class) return If_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_If_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_If_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to IfStmt";
         
            end if;
      end;
      function As_Import_From
        (Node : Turkixir_Node'Class) return Import_From
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Import_From;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Import_From_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ImportFrom";
         
            end if;
      end;
      function As_Import_Name
        (Node : Turkixir_Node'Class) return Import_Name
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Import_Name;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Import_Name_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ImportName";
         
            end if;
      end;
      function As_Import_Star
        (Node : Turkixir_Node'Class) return Import_Star
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Import_Star;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Import_Star_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ImportStar";
         
            end if;
      end;
      function As_Inline_Eval
        (Node : Turkixir_Node'Class) return Inline_Eval
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Inline_Eval;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Inline_Eval_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to InlineEval";
         
            end if;
      end;
      function As_Kw_Args
        (Node : Turkixir_Node'Class) return Kw_Args
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Kw_Args;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Kw_Args_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to KwArgs";
         
            end if;
      end;
      function As_Kw_Args_Flag
        (Node : Turkixir_Node'Class) return Kw_Args_Flag
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Kw_Args_Flag;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Kw_Args_Flag then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to KwArgsFlag";
         
            end if;
      end;
      function As_Kw_Args_Flag_Absent
        (Node : Turkixir_Node'Class) return Kw_Args_Flag_Absent
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Kw_Args_Flag_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Kw_Args_Flag_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to KwArgsFlag.Absent";
         
            end if;
      end;
      function As_Kw_Args_Flag_Present
        (Node : Turkixir_Node'Class) return Kw_Args_Flag_Present
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Kw_Args_Flag_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Kw_Args_Flag_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to KwArgsFlag.Present";
         
            end if;
      end;
      function As_Lambda_Def
        (Node : Turkixir_Node'Class) return Lambda_Def
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Lambda_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Lambda_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to LambdaDef";
         
            end if;
      end;
      function As_List_Comp
        (Node : Turkixir_Node'Class) return List_Comp
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_List_Comp;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_List_Comp_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ListComp";
         
            end if;
      end;
      function As_List_Gen
        (Node : Turkixir_Node'Class) return List_Gen
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_List_Gen;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_List_Gen_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ListGen";
         
            end if;
      end;
      function As_List_Lit
        (Node : Turkixir_Node'Class) return List_Lit
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_List_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_List_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ListLit";
         
            end if;
      end;
      function As_NL
        (Node : Turkixir_Node'Class) return NL
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_NL;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_NL_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NL";
         
            end if;
      end;
      function As_NL_List
        (Node : Turkixir_Node'Class) return NL_List
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_NL_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_NL_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NL.list";
         
            end if;
      end;
      function As_Not_Op
        (Node : Turkixir_Node'Class) return Not_Op
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Not_Op;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Not_Op_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NotOp";
         
            end if;
      end;
      function As_Number_Lit
        (Node : Turkixir_Node'Class) return Number_Lit
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Number_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Number_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to NumberLit";
         
            end if;
      end;
      function As_Op
        (Node : Turkixir_Node'Class) return Op
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Op_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op";
         
            end if;
      end;
      function As_Or_Expr
        (Node : Turkixir_Node'Class) return Or_Expr
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Or_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Or_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to OrExpr";
         
            end if;
      end;
      function As_Or_Op
        (Node : Turkixir_Node'Class) return Or_Op
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Or_Op;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Or_Op_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to OrOp";
         
            end if;
      end;
      function As_Params
        (Node : Turkixir_Node'Class) return Params
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Params;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Params_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Params";
         
            end if;
      end;
      function As_Pass_Stmt
        (Node : Turkixir_Node'Class) return Pass_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Pass_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Pass_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to PassStmt";
         
            end if;
      end;
      function As_Power
        (Node : Turkixir_Node'Class) return Power
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Power;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Power_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Power";
         
            end if;
      end;
      function As_Print_Stmt
        (Node : Turkixir_Node'Class) return Print_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Print_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Print_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to PrintStmt";
         
            end if;
      end;
      function As_Raise_Stmt
        (Node : Turkixir_Node'Class) return Raise_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Raise_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Raise_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to RaiseStmt";
         
            end if;
      end;
      function As_Rel_Name
        (Node : Turkixir_Node'Class) return Rel_Name
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Rel_Name;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Rel_Name_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to RelName";
         
            end if;
      end;
      function As_Return_Stmt
        (Node : Turkixir_Node'Class) return Return_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Return_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Return_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ReturnStmt";
         
            end if;
      end;
      function As_Set_Comp
        (Node : Turkixir_Node'Class) return Set_Comp
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Set_Comp;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Set_Comp_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SetComp";
         
            end if;
      end;
      function As_Set_Lit
        (Node : Turkixir_Node'Class) return Set_Lit
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Set_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Set_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SetLit";
         
            end if;
      end;
      function As_Shift_Expr
        (Node : Turkixir_Node'Class) return Shift_Expr
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Shift_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Shift_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to ShiftExpr";
         
            end if;
      end;
      function As_Single_Param
        (Node : Turkixir_Node'Class) return Single_Param
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Single_Param;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Single_Param_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SingleParam";
         
            end if;
      end;
      function As_Single_Param_List
        (Node : Turkixir_Node'Class) return Single_Param_List
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Single_Param_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Single_Param_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SingleParam.list";
         
            end if;
      end;
      function As_Stream_Print_Stmt
        (Node : Turkixir_Node'Class) return Stream_Print_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Stream_Print_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Stream_Print_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to StreamPrintStmt";
         
            end if;
      end;
      function As_String_Lit
        (Node : Turkixir_Node'Class) return String_Lit
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_String_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_String_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to StringLit";
         
            end if;
      end;
      function As_String_Lit_List
        (Node : Turkixir_Node'Class) return String_Lit_List
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_String_Lit_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_String_Lit_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to StringLit.list";
         
            end if;
      end;
      function As_Subscript_Expr
        (Node : Turkixir_Node'Class) return Subscript_Expr
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Subscript_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Subscript_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to SubscriptExpr";
         
            end if;
      end;
      function As_Term
        (Node : Turkixir_Node'Class) return Term
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Term;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Term_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to Term";
         
            end if;
      end;
      function As_Try_Stmt
        (Node : Turkixir_Node'Class) return Try_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Try_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Try_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to TryStmt";
         
            end if;
      end;
      function As_Tuple_Lit
        (Node : Turkixir_Node'Class) return Tuple_Lit
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Tuple_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Tuple_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to TupleLit";
         
            end if;
      end;
      function As_Turkixir_Node_List
        (Node : Turkixir_Node'Class) return Turkixir_Node_List
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Turkixir_Node_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Turkixir_Node_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to TurkixirNode.list";
         
            end if;
      end;
      function As_Var_Args
        (Node : Turkixir_Node'Class) return Var_Args
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Var_Args;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Var_Args_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to VarArgs";
         
            end if;
      end;
      function As_Var_Args_Flag
        (Node : Turkixir_Node'Class) return Var_Args_Flag
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Var_Args_Flag;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Var_Args_Flag then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to VarArgsFlag";
         
            end if;
      end;
      function As_Var_Args_Flag_Absent
        (Node : Turkixir_Node'Class) return Var_Args_Flag_Absent
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Var_Args_Flag_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Var_Args_Flag_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to VarArgsFlag.Absent";
         
            end if;
      end;
      function As_Var_Args_Flag_Present
        (Node : Turkixir_Node'Class) return Var_Args_Flag_Present
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Var_Args_Flag_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Var_Args_Flag_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to VarArgsFlag.Present";
         
            end if;
      end;
      function As_While_Stmt
        (Node : Turkixir_Node'Class) return While_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_While_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_While_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to WhileStmt";
         
            end if;
      end;
      function As_With_Stmt
        (Node : Turkixir_Node'Class) return With_Stmt
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_With_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_With_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to WithStmt";
         
            end if;
      end;
      function As_Xor_Expr
        (Node : Turkixir_Node'Class) return Xor_Expr
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Xor_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Xor_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to XorExpr";
         
            end if;
      end;
      function As_Yield_Expr
        (Node : Turkixir_Node'Class) return Yield_Expr
      is
         N : constant Bare_Turkixir_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Yield_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Turkixir_Yield_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libturkixirlang: invalid type conversion from "
              & Node.Kind_Name
              & " to YieldExpr";
         
            end if;
      end;

   -----------------------
   -- Entity primitives --
   -----------------------

   ----------
   -- Hash --
   ----------

   function Hash
     (Node : Turkixir_Node) return Ada.Containers.Hash_Type is
   begin
      Check_Safety_Net (Node);
      return Hash_Entity (Node.Internal);
   end Hash;

   ----------
   -- Kind --
   ----------

   function Kind (Node : Turkixir_Node'Class) return Turkixir_Node_Kind_Type
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Node.Internal.Node.Kind;
   end Kind;

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Node : Turkixir_Node'Class) return String is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Kind_Name (Node.Internal.Node);
   end Kind_Name;

      


      
      function To_Public_Turkixir_Node_Array
         (Value : Internal_Entity_Array_Access) return Turkixir_Node_Array is
      begin
         return Result : Turkixir_Node_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Node (Value.Items (I).Node, Value.Items (I).Info)
               ;
            end loop;
         end return;
      end;


      


      


      


      









         
   function Parent
     (Node : Turkixir_Node'Class) return Turkixir_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libturkixirlang.Implementation.Parent
            (Bare_Turkixir_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info);

   end;

         
   function Parents
     (Node : Turkixir_Node'Class;
      With_Self : Boolean := True) return Turkixir_Node_Array is
      


         Internal_Arg_With_Self : Boolean;
      Property_Result : Internal_Entity_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_With_Self :=
            With_Self;

      
      Property_Result :=
         Libturkixirlang.Implementation.Parents
            (Bare_Turkixir_Node (Node.Internal.Node), Internal_Arg_With_Self, E_Info => Node.Internal.Info);

         return Result : constant Turkixir_Node_Array :=
            To_Public_Turkixir_Node_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function Children
     (Node : Turkixir_Node'Class) return Turkixir_Node_Array is
      


      Property_Result : Internal_Entity_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libturkixirlang.Implementation.Children
            (Bare_Turkixir_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Turkixir_Node_Array :=
            To_Public_Turkixir_Node_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function Token_Start
     (Node : Turkixir_Node'Class) return Token_Reference is
      


      Property_Result : Token_Reference;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libturkixirlang.Implementation.Token_Start
            (Bare_Turkixir_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function Token_End
     (Node : Turkixir_Node'Class) return Token_Reference is
      


      Property_Result : Token_Reference;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libturkixirlang.Implementation.Token_End
            (Bare_Turkixir_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function Child_Index
     (Node : Turkixir_Node'Class) return Integer is
      


      Property_Result : Integer;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libturkixirlang.Implementation.Child_Index
            (Bare_Turkixir_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function Previous_Sibling
     (Node : Turkixir_Node'Class) return Turkixir_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libturkixirlang.Implementation.Previous_Sibling
            (Bare_Turkixir_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info);

   end;

         
   function Next_Sibling
     (Node : Turkixir_Node'Class) return Turkixir_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libturkixirlang.Implementation.Next_Sibling
            (Bare_Turkixir_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info);

   end;

         
   function Unit
     (Node : Turkixir_Node'Class) return Analysis_Unit is
      


      Property_Result : Internal_Unit;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libturkixirlang.Implementation.Unit
            (Bare_Turkixir_Node (Node.Internal.Node));

         return Wrap_Unit (Property_Result);

   end;

         
   function Is_Ghost
     (Node : Turkixir_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libturkixirlang.Implementation.Is_Ghost
            (Bare_Turkixir_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function Full_Sloc_Image
     (Node : Turkixir_Node'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libturkixirlang.Implementation.Full_Sloc_Image
            (Bare_Turkixir_Node (Node.Internal.Node));

         return Result : constant Text_Type :=
            Property_Result.Content
         do
            Free_Internal;
         end return;

      exception
         when Property_Error =>
            Free_Internal;
            raise;
   end;










         
   

   function F_Left
     (Node : And_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.And_Expr_F_Left (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Left;


         
   

   function F_Right
     (Node : And_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.And_Expr_F_Right (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Right;







         
   

   function F_Left
     (Node : And_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.And_Op_F_Left (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Left;


         
   

   function F_Right
     (Node : And_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.And_Op_F_Right (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Right;












         
   

   function F_Name
     (Node : Arg_Assoc'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Arg_Assoc_F_Name (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Name;


         
   

   function F_Expr
     (Node : Arg_Assoc'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Arg_Assoc_F_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Expr;







         
   

   function F_Expr
     (Node : Arg_Gen'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Arg_Gen_F_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Expr;


         
   

   function F_Comprehension
     (Node : Arg_Gen'Class) return Comp_For
   is
      Result : Bare_Comp_For;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Arg_Gen_F_Comprehension (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Comprehension;










         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Arg_List'Class; Index : Positive) return Arg
         is
            Result : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Arg;
         end List_Child;

         

         function Arg_List_First (Node : Arg_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Arg_List_Next
           (Node : Arg_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Arg_List_Has_Element
           (Node : Arg_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Arg_List_Element
           (Node : Arg_List; Cursor : Positive) return Arg'Class
         is
            Child : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Arg'(Child.As_Arg);
         end;






         
   

   function F_Left
     (Node : Bin_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Bin_Op_F_Left (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Left;


         
   

   function F_Op
     (Node : Bin_Op'Class) return Op
   is
      Result : Bare_Op;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Bin_Op_F_Op (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Op;


         
   

   function F_Right
     (Node : Bin_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Bin_Op_F_Right (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Right;












         
   

   function F_Imported
     (Node : As_Name_Node'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.As_Name_Node_F_Imported (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Imported;


         
   

   function F_As_Name
     (Node : As_Name_Node'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.As_Name_Node_F_As_Name (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_As_Name;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : As_Name_Node_List'Class; Index : Positive) return As_Name_Node
         is
            Result : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_As_Name_Node;
         end List_Child;

         

         function As_Name_Node_List_First (Node : As_Name_Node_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function As_Name_Node_List_Next
           (Node : As_Name_Node_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function As_Name_Node_List_Has_Element
           (Node : As_Name_Node_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function As_Name_Node_List_Element
           (Node : As_Name_Node_List; Cursor : Positive) return As_Name_Node'Class
         is
            Child : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return As_Name_Node'(Child.As_As_Name_Node);
         end;











         
   

   function F_Test_Expr
     (Node : Assert_Stmt'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Assert_Stmt_F_Test_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Test_Expr;


         
   

   function F_Msg
     (Node : Assert_Stmt'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Assert_Stmt_F_Msg (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Msg;







         
   

   function F_L_Value
     (Node : Assign_Stmt'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Assign_Stmt_F_L_Value (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_L_Value;


         
   

   function F_R_Values
     (Node : Assign_Stmt'Class) return Turkixir_Node_List
   is
      Result : Bare_Turkixir_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Assign_Stmt_F_R_Values (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_R_Values;







         
   

   function F_L_Value
     (Node : Aug_Assign_Stmt'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Aug_Assign_Stmt_F_L_Value (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_L_Value;


         
   

   function F_Op
     (Node : Aug_Assign_Stmt'Class) return Op
   is
      Result : Bare_Op;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Aug_Assign_Stmt_F_Op (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Op;


         
   

   function F_R_Value
     (Node : Aug_Assign_Stmt'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Aug_Assign_Stmt_F_R_Value (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_R_Value;












         
   

   function F_Prefix
     (Node : Call_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Call_Expr_F_Prefix (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Prefix;


         
   

   function F_Suffix
     (Node : Call_Expr'Class) return Arg_List
   is
      Result : Bare_Arg_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Call_Expr_F_Suffix (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Suffix;












         
   

   function F_Name
     (Node : Class_Def'Class) return Id
   is
      Result : Bare_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Class_Def_F_Name (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Name;


         
   

   function F_Bases
     (Node : Class_Def'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Class_Def_F_Bases (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Bases;


         
   

   function F_Statements
     (Node : Class_Def'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Class_Def_F_Statements (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Statements;












         
   

   function F_Exprs
     (Node : Comp_For'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Comp_For_F_Exprs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Exprs;


         
   

   function F_Target
     (Node : Comp_For'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Comp_For_F_Target (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Target;


         
   

   function F_Comp
     (Node : Comp_For'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Comp_For_F_Comp (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Comp;







         
   

   function F_Exprs
     (Node : Comp_ForL'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Comp_ForL_F_Exprs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Exprs;


         
   

   function F_Target
     (Node : Comp_ForL'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Comp_ForL_F_Target (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Target;


         
   

   function F_Comp
     (Node : Comp_ForL'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Comp_ForL_F_Comp (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Comp;







         
   

   function F_Test
     (Node : Comp_If'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Comp_If_F_Test (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Test;


         
   

   function F_Comp
     (Node : Comp_If'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Comp_If_F_Comp (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Comp;







         
   

   function F_Left
     (Node : Comp_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Comp_Op_F_Left (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Left;


         
   

   function F_Op
     (Node : Comp_Op'Class) return Comp_Op_Kind
   is
      Result : Bare_Comp_Op_Kind;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Comp_Op_F_Op (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Op;

         function F_Op
           (Node : Comp_Op'Class) return Turkixir_Comp_Op_Kind
         is (Comp_Op_Kind'(Node.F_Op).Kind);

         
   

   function F_Right
     (Node : Comp_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Comp_Op_F_Right (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Right;



































































         
   

   function F_First_Str
     (Node : Concat_String_Lit'Class) return String_Lit
   is
      Result : Bare_String_Lit;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Concat_String_Lit_F_First_Str (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_First_Str;


         
   

   function F_Subsequent_Str
     (Node : Concat_String_Lit'Class) return String_Lit_List
   is
      Result : Bare_String_Lit_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Concat_String_Lit_F_Subsequent_Str (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Subsequent_Str;












         
   

   function F_Decorators
     (Node : Decorated'Class) return Decorator_List
   is
      Result : Bare_Decorator_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decorated_F_Decorators (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Decorators;


         
   

   function F_Defn
     (Node : Decorated'Class) return Def_Stmt
   is
      Result : Bare_Def_Stmt;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decorated_F_Defn (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Defn;







         
   

   function F_Dec_Name
     (Node : Decorator'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decorator_F_Dec_Name (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Dec_Name;


         
   

   function F_Arg_List
     (Node : Decorator'Class) return Arg_List
   is
      Result : Bare_Arg_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decorator_F_Arg_List (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Arg_List;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Decorator_List'Class; Index : Positive) return Decorator
         is
            Result : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Decorator;
         end List_Child;

         

         function Decorator_List_First (Node : Decorator_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Decorator_List_Next
           (Node : Decorator_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Decorator_List_Has_Element
           (Node : Decorator_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Decorator_List_Element
           (Node : Decorator_List; Cursor : Positive) return Decorator'Class
         is
            Child : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Decorator'(Child.As_Decorator);
         end;






         
   

   function F_Exprs
     (Node : Del_Stmt'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Del_Stmt_F_Exprs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Exprs;







         
   

   function F_Key
     (Node : Dict_Assoc'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Dict_Assoc_F_Key (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Key;


         
   

   function F_Value
     (Node : Dict_Assoc'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Dict_Assoc_F_Value (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Value;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Dict_Assoc_List'Class; Index : Positive) return Dict_Assoc
         is
            Result : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Dict_Assoc;
         end List_Child;

         

         function Dict_Assoc_List_First (Node : Dict_Assoc_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Dict_Assoc_List_Next
           (Node : Dict_Assoc_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Dict_Assoc_List_Has_Element
           (Node : Dict_Assoc_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Dict_Assoc_List_Element
           (Node : Dict_Assoc_List; Cursor : Positive) return Dict_Assoc'Class
         is
            Child : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Dict_Assoc'(Child.As_Dict_Assoc);
         end;






         
   

   function F_Assoc
     (Node : Dict_Comp'Class) return Dict_Assoc
   is
      Result : Bare_Dict_Assoc;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Dict_Comp_F_Assoc (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Assoc;


         
   

   function F_Comprehension
     (Node : Dict_Comp'Class) return Comp_For
   is
      Result : Bare_Comp_For;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Dict_Comp_F_Comprehension (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Comprehension;







         
   

   function F_Assocs
     (Node : Dict_Lit'Class) return Dict_Assoc_List
   is
      Result : Bare_Dict_Assoc_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Dict_Lit_F_Assocs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Assocs;










         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Dot_List'Class; Index : Positive) return Dot
         is
            Result : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Dot;
         end List_Child;

         

         function Dot_List_First (Node : Dot_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Dot_List_Next
           (Node : Dot_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Dot_List_Has_Element
           (Node : Dot_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Dot_List_Element
           (Node : Dot_List; Cursor : Positive) return Dot'Class
         is
            Child : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Dot'(Child.As_Dot);
         end;











         
   

   function F_Prefix
     (Node : Dotted_Name'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Dotted_Name_F_Prefix (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Prefix;


         
   

   function F_Suffix
     (Node : Dotted_Name'Class) return Id
   is
      Result : Bare_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Dotted_Name_F_Suffix (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Suffix;







         
   

   function F_Cond_Test
     (Node : Elif_Branch'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Elif_Branch_F_Cond_Test (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Cond_Test;


         
   

   function F_Statements
     (Node : Elif_Branch'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Elif_Branch_F_Statements (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Statements;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Elif_Branch_List'Class; Index : Positive) return Elif_Branch
         is
            Result : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Elif_Branch;
         end List_Child;

         

         function Elif_Branch_List_First (Node : Elif_Branch_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Elif_Branch_List_Next
           (Node : Elif_Branch_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Elif_Branch_List_Has_Element
           (Node : Elif_Branch_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Elif_Branch_List_Element
           (Node : Elif_Branch_List; Cursor : Positive) return Elif_Branch'Class
         is
            Child : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Elif_Branch'(Child.As_Elif_Branch);
         end;











         
   

   function F_Statements
     (Node : Else_Part'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Else_Part_F_Statements (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Statements;







         
   

   function F_As_Name
     (Node : Except_Part'Class) return As_Name_Node
   is
      Result : Bare_As_Name_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Except_Part_F_As_Name (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_As_Name;


         
   

   function F_Statements
     (Node : Except_Part'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Except_Part_F_Statements (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Statements;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Except_Part_List'Class; Index : Positive) return Except_Part
         is
            Result : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Except_Part;
         end List_Child;

         

         function Except_Part_List_First (Node : Except_Part_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Except_Part_List_Next
           (Node : Except_Part_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Except_Part_List_Has_Element
           (Node : Except_Part_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Except_Part_List_Element
           (Node : Except_Part_List; Cursor : Positive) return Except_Part'Class
         is
            Child : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Except_Part'(Child.As_Except_Part);
         end;






         
   

   function F_Expr
     (Node : Exec_Stmt'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Exec_Stmt_F_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Expr;


         
   

   function F_In_List
     (Node : Exec_Stmt'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Exec_Stmt_F_In_List (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_In_List;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Expr_List'Class; Index : Positive) return Expr
         is
            Result : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Expr;
         end List_Child;

         

         function Expr_List_First (Node : Expr_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Expr_List_Next
           (Node : Expr_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Expr_List_Has_Element
           (Node : Expr_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Expr_List_Element
           (Node : Expr_List; Cursor : Positive) return Expr'Class
         is
            Child : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Expr'(Child.As_Expr);
         end;






         
   

   function F_First
     (Node : Slice_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Slice_Expr_F_First (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_First;


         
   

   function F_Last
     (Node : Slice_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Slice_Expr_F_Last (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Last;







         
   

   function F_Stride
     (Node : Ext_Slice_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ext_Slice_Expr_F_Stride (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Stride;







         
   

   function F_Op
     (Node : Factor'Class) return Op
   is
      Result : Bare_Op;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Factor_F_Op (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Op;


         
   

   function F_Expr
     (Node : Factor'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Factor_F_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Expr;







         
   

   function F_Statements
     (Node : File_Node'Class) return Turkixir_Node_List
   is
      Result : Bare_Turkixir_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.File_Node_F_Statements (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Statements;







         
   

   function F_Bindings
     (Node : For_Stmt'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.For_Stmt_F_Bindings (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Bindings;


         
   

   function F_Expr
     (Node : For_Stmt'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.For_Stmt_F_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Expr;


         
   

   function F_Statements
     (Node : For_Stmt'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.For_Stmt_F_Statements (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Statements;


         
   

   function F_Else_Part
     (Node : For_Stmt'Class) return Else_Part
   is
      Result : Bare_Else_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.For_Stmt_F_Else_Part (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Else_Part;







         
   

   function F_Name
     (Node : Func_Def'Class) return Id
   is
      Result : Bare_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Func_Def_F_Name (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Name;


         
   

   function F_Parameters
     (Node : Func_Def'Class) return Params
   is
      Result : Bare_Params;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Func_Def_F_Parameters (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Parameters;


         
   

   function F_Body
     (Node : Func_Def'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Func_Def_F_Body (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Body;







         
   

   function F_Names
     (Node : Global_Stmt'Class) return Id_List
   is
      Result : Bare_Id_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Global_Stmt_F_Names (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Names;










         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Id_List'Class; Index : Positive) return Id
         is
            Result : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Id;
         end List_Child;

         

         function Id_List_First (Node : Id_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Id_List_Next
           (Node : Id_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Id_List_Has_Element
           (Node : Id_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Id_List_Element
           (Node : Id_List; Cursor : Positive) return Id'Class
         is
            Child : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Id'(Child.As_Id);
         end;






         
   

   function F_Expr
     (Node : If_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Expr_F_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Expr;


         
   

   function F_Cond
     (Node : If_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Expr_F_Cond (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Cond;


         
   

   function F_Else_Expr
     (Node : If_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Expr_F_Else_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Else_Expr;







         
   

   function F_Cond_Test
     (Node : If_Stmt'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Stmt_F_Cond_Test (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Cond_Test;


         
   

   function F_Statements
     (Node : If_Stmt'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Stmt_F_Statements (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Statements;


         
   

   function F_Elif_Branchs
     (Node : If_Stmt'Class) return Elif_Branch_List
   is
      Result : Bare_Elif_Branch_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Stmt_F_Elif_Branchs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Elif_Branchs;


         
   

   function F_Else_Part
     (Node : If_Stmt'Class) return Else_Part
   is
      Result : Bare_Else_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Stmt_F_Else_Part (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Else_Part;







         
   

   function F_Rel_Name
     (Node : Import_From'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Import_From_F_Rel_Name (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Rel_Name;


         
   

   function F_Imported
     (Node : Import_From'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Import_From_F_Imported (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Imported;







         
   

   function F_Imported_Names
     (Node : Import_Name'Class) return Turkixir_Node_List
   is
      Result : Bare_Turkixir_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Import_Name_F_Imported_Names (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Imported_Names;












         
   

   function F_Exprs
     (Node : Inline_Eval'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Inline_Eval_F_Exprs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Exprs;







         
   

   function F_Expr
     (Node : Kw_Args'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Kw_Args_F_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Expr;








         
   function P_As_Bool
     (Node : Kw_Args_Flag'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libturkixirlang.Implementation.Dispatcher_Kw_Args_Flag_P_As_Bool
            (Bare_Turkixir_Node (Node.Internal.Node));

         return Property_Result;

   end;















         
   

   function F_Args
     (Node : Lambda_Def'Class) return Params
   is
      Result : Bare_Params;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Lambda_Def_F_Args (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Args;


         
   

   function F_Expr
     (Node : Lambda_Def'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Lambda_Def_F_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Expr;







         
   

   function F_Expr
     (Node : List_Comp'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.List_Comp_F_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Expr;


         
   

   function F_Comprehension
     (Node : List_Comp'Class) return Comp_ForL
   is
      Result : Bare_Comp_ForL;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.List_Comp_F_Comprehension (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Comprehension;







         
   

   function F_Expr
     (Node : List_Gen'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.List_Gen_F_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Expr;


         
   

   function F_Comprehension
     (Node : List_Gen'Class) return Comp_ForL
   is
      Result : Bare_Comp_ForL;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.List_Gen_F_Comprehension (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Comprehension;







         
   

   function F_Exprs
     (Node : List_Lit'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.List_Lit_F_Exprs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Exprs;










         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : NL_List'Class; Index : Positive) return NL
         is
            Result : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_NL;
         end List_Child;

         

         function NL_List_First (Node : NL_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function NL_List_Next
           (Node : NL_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function NL_List_Has_Element
           (Node : NL_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function NL_List_Element
           (Node : NL_List; Cursor : Positive) return NL'Class
         is
            Child : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return NL'(Child.As_NL);
         end;






         
   

   function F_Expr
     (Node : Not_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Not_Op_F_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Expr;

















         
   

   function F_Left
     (Node : Or_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Or_Expr_F_Left (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Left;


         
   

   function F_Right
     (Node : Or_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Or_Expr_F_Right (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Right;







         
   

   function F_Left
     (Node : Or_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Or_Op_F_Left (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Left;


         
   

   function F_Right
     (Node : Or_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Or_Op_F_Right (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Right;







         
   

   function F_Single_Params
     (Node : Params'Class) return Single_Param_List
   is
      Result : Bare_Single_Param_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Params_F_Single_Params (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Single_Params;












         
   

   function F_Left
     (Node : Power'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Power_F_Left (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Left;


         
   

   function F_Right
     (Node : Power'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Power_F_Right (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Right;







         
   

   function F_Exprs
     (Node : Print_Stmt'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Print_Stmt_F_Exprs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Exprs;







         
   

   function F_Exprs
     (Node : Raise_Stmt'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Raise_Stmt_F_Exprs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Exprs;







         
   

   function F_Dots
     (Node : Rel_Name'Class) return Dot_List
   is
      Result : Bare_Dot_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Rel_Name_F_Dots (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Dots;


         
   

   function F_Name
     (Node : Rel_Name'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Rel_Name_F_Name (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Name;







         
   

   function F_Exprs
     (Node : Return_Stmt'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Return_Stmt_F_Exprs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Exprs;







         
   

   function F_Expr
     (Node : Set_Comp'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Set_Comp_F_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Expr;


         
   

   function F_Comprehension
     (Node : Set_Comp'Class) return Comp_For
   is
      Result : Bare_Comp_For;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Set_Comp_F_Comprehension (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Comprehension;







         
   

   function F_Exprs
     (Node : Set_Lit'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Set_Lit_F_Exprs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Exprs;












         
   

   function F_Is_Varargs
     (Node : Single_Param'Class) return Var_Args_Flag
   is
      Result : Bare_Var_Args_Flag;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Single_Param_F_Is_Varargs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Is_Varargs;

         function F_Is_Varargs (Node : Single_Param'Class) return Boolean
         is (Var_Args_Flag'(Node.F_Is_Varargs).Kind
             = Turkixir_Var_Args_Flag_Present);


         
   

   function F_Is_Kwargs
     (Node : Single_Param'Class) return Kw_Args_Flag
   is
      Result : Bare_Kw_Args_Flag;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Single_Param_F_Is_Kwargs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Is_Kwargs;

         function F_Is_Kwargs (Node : Single_Param'Class) return Boolean
         is (Kw_Args_Flag'(Node.F_Is_Kwargs).Kind
             = Turkixir_Kw_Args_Flag_Present);


         
   

   function F_Name
     (Node : Single_Param'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Single_Param_F_Name (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Name;


         
   

   function F_Default_Value
     (Node : Single_Param'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Single_Param_F_Default_Value (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Default_Value;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Single_Param_List'Class; Index : Positive) return Single_Param
         is
            Result : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Single_Param;
         end List_Child;

         

         function Single_Param_List_First (Node : Single_Param_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Single_Param_List_Next
           (Node : Single_Param_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Single_Param_List_Has_Element
           (Node : Single_Param_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Single_Param_List_Element
           (Node : Single_Param_List; Cursor : Positive) return Single_Param'Class
         is
            Child : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Single_Param'(Child.As_Single_Param);
         end;






         
   

   function F_Stream_Expr
     (Node : Stream_Print_Stmt'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Stream_Print_Stmt_F_Stream_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Stream_Expr;


         
   

   function F_Exprs
     (Node : Stream_Print_Stmt'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Stream_Print_Stmt_F_Exprs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Exprs;










         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : String_Lit_List'Class; Index : Positive) return String_Lit
         is
            Result : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_String_Lit;
         end List_Child;

         

         function String_Lit_List_First (Node : String_Lit_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function String_Lit_List_Next
           (Node : String_Lit_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function String_Lit_List_Has_Element
           (Node : String_Lit_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function String_Lit_List_Element
           (Node : String_Lit_List; Cursor : Positive) return String_Lit'Class
         is
            Child : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return String_Lit'(Child.As_String_Lit);
         end;






         
   

   function F_Prefix
     (Node : Subscript_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subscript_Expr_F_Prefix (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Prefix;


         
   

   function F_Suffix
     (Node : Subscript_Expr'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subscript_Expr_F_Suffix (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Suffix;












         
   

   function F_Statements
     (Node : Try_Stmt'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Try_Stmt_F_Statements (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Statements;


         
   

   function F_Except_Parts
     (Node : Try_Stmt'Class) return Except_Part_List
   is
      Result : Bare_Except_Part_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Try_Stmt_F_Except_Parts (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Except_Parts;


         
   

   function F_Else_Part
     (Node : Try_Stmt'Class) return Else_Part
   is
      Result : Bare_Else_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Try_Stmt_F_Else_Part (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Else_Part;


         
   

   function F_Finally_Part
     (Node : Try_Stmt'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Try_Stmt_F_Finally_Part (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Finally_Part;







         
   

   function F_Exprs
     (Node : Tuple_Lit'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Tuple_Lit_F_Exprs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Exprs;






         

         function Turkixir_Node_List_First (Node : Turkixir_Node_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Turkixir_Node_List_Next
           (Node : Turkixir_Node_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Turkixir_Node_List_Has_Element
           (Node : Turkixir_Node_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Turkixir_Node_List_Element
           (Node : Turkixir_Node_List; Cursor : Positive) return Turkixir_Node'Class
         is
            Child : Turkixir_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Turkixir_Node'(Child.As_Turkixir_Node);
         end;






         
   

   function F_Expr
     (Node : Var_Args'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Var_Args_F_Expr (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Expr;








         
   function P_As_Bool
     (Node : Var_Args_Flag'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libturkixirlang.Implementation.Dispatcher_Var_Args_Flag_P_As_Bool
            (Bare_Turkixir_Node (Node.Internal.Node));

         return Property_Result;

   end;















         
   

   function F_Cond_Test
     (Node : While_Stmt'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.While_Stmt_F_Cond_Test (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Cond_Test;


         
   

   function F_Statements
     (Node : While_Stmt'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.While_Stmt_F_Statements (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Statements;


         
   

   function F_Else_Part
     (Node : While_Stmt'Class) return Else_Part
   is
      Result : Bare_Else_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.While_Stmt_F_Else_Part (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Else_Part;







         
   

   function F_Bindings
     (Node : With_Stmt'Class) return As_Name_Node_List
   is
      Result : Bare_As_Name_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.With_Stmt_F_Bindings (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Bindings;


         
   

   function F_Statements
     (Node : With_Stmt'Class) return Turkixir_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.With_Stmt_F_Statements (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Statements;







         
   

   function F_Left
     (Node : Xor_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Xor_Expr_F_Left (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Left;


         
   

   function F_Right
     (Node : Xor_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Xor_Expr_F_Right (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Right;







         
   

   function F_Exprs
     (Node : Yield_Expr'Class) return Expr_List
   is
      Result : Bare_Expr_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Yield_Expr_F_Exprs (Node.Internal.Node);
         return (Internal   => (Result, Node.Internal.Info),
                 Safety_Net => Node.Safety_Net);
   end F_Exprs;





   --------------------
   -- Children_Count --
   --------------------

   function Children_Count
     (Node : Turkixir_Node'Class) return Natural is
   begin
      Check_Safety_Net (Node);
      return Children_Count (Node.Internal.Node);
   end Children_Count;

   -----------------------
   -- First_Child_Index --
   -----------------------

   function First_Child_Index
     (Node : Turkixir_Node'Class) return Natural is
   begin
      Check_Safety_Net (Node);
      return First_Child_Index (Node.Internal.Node);
   end First_Child_Index;

   ----------------------
   -- Last_Child_Index --
   ----------------------

   function Last_Child_Index
     (Node : Turkixir_Node'Class) return Natural is
   begin
      Check_Safety_Net (Node);
      return Last_Child_Index (Node.Internal.Node);
   end Last_Child_Index;

   -----------------
   -- First_Child --
   -----------------

   function First_Child
     (Node : Turkixir_Node'Class) return Turkixir_Node is
   begin
      Check_Safety_Net (Node);

      return Node.Child (First_Child_Index (Node.Internal.Node));
   end First_Child;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child
     (Node : Turkixir_Node'Class) return Turkixir_Node is
   begin
      Check_Safety_Net (Node);

      return Node.Child (Last_Child_Index (Node.Internal.Node));
   end Last_Child;

   ---------------
   -- Get_Child --
   ---------------

   procedure Get_Child
     (Node            : Turkixir_Node'Class;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Turkixir_Node)
   is
      N : Bare_Turkixir_Node;
   begin
      Check_Safety_Net (Node);
      Get_Child (Node.Internal.Node, Index, Index_In_Bounds, N);
      Result := Wrap_Node (N, Node.Internal.Info);
   end Get_Child;

   -----------
   -- Child --
   -----------

   function Child
     (Node  : Turkixir_Node'Class;
      Index : Positive) return Turkixir_Node
   is
   begin
      Check_Safety_Net (Node);
      return Wrap_Node (Child (Node.Internal.Node, Index), Node.Internal.Info);
   end Child;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Node : Turkixir_Node'Class) return Source_Location_Range is
   begin
      Check_Safety_Net (Node);
      return Sloc_Range (Node.Internal.Node);
   end Sloc_Range;

   -------------
   -- Compare --
   -------------

   function Compare
     (Node : Turkixir_Node'Class;
      Sloc : Source_Location) return Relative_Position is
   begin
      Check_Safety_Net (Node);
      return Compare (Node.Internal.Node, Sloc);
   end Compare;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Node : Turkixir_Node'Class;
      Sloc : Source_Location) return Turkixir_Node is
   begin
      Check_Safety_Net (Node);
      return Wrap_Node (Lookup (Node.Internal.Node, Sloc));
   end Lookup;

   ----------
   -- Text --
   ----------

   function Text (Node : Turkixir_Node'Class) return Text_Type is
   begin
      Check_Safety_Net (Node);
      return Implementation.Text (Node.Internal.Node);
   end Text;

   -----------------
   -- Token_Range --
   -----------------

   function Token_Range
     (Node : Turkixir_Node'Class) return Token_Iterator is
   begin
      Check_Safety_Net (Node);
      return Token_Iterator'(Node.As_Turkixir_Node,
                             Node.Internal.Node.Token_End_Index);
   end Token_Range;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : Turkixir_Node'Class;
      Show_Slocs  : Boolean := True;
      Line_Prefix : String := "") is
   begin
      Check_Safety_Net (Node);
      Print (Node.Internal.Node, Show_Slocs, Line_Prefix);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia
     (Node : Turkixir_Node'Class; Line_Prefix : String := "") is
   begin
      Check_Safety_Net (Node);
      PP_Trivia (Node.Internal.Node, Line_Prefix);
   end PP_Trivia;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Node  : Turkixir_Node'Class;
      Visit : access function (Node : Turkixir_Node'Class)
              return Visit_Status)
      return Visit_Status
   is
      Info : constant Internal_Entity_Info := Node.Internal.Info;

      -------------
      -- Wrapper --
      -------------

      function Wrapper (Node : Bare_Turkixir_Node) return Visit_Status
      is
         Public_Node : constant Turkixir_Node :=
           Wrap_Node (Bare_Turkixir_Node (Node), Info);
      begin
         return Visit (Public_Node);
      end Wrapper;

   begin
      Check_Safety_Net (Node);
      return Traverse (Node.Internal.Node, Wrapper'Access);
   end Traverse;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (Node  : Turkixir_Node'Class;
      Visit : access function (Node : Turkixir_Node'Class)
                               return Visit_Status)
   is
      Result_Status : Visit_Status;
      pragma Unreferenced (Result_Status);
   begin
      Result_Status := Traverse (Node, Visit);
   end Traverse;

   --------------------------------
   -- Assign_Names_To_Logic_Vars --
   --------------------------------

   procedure Assign_Names_To_Logic_Vars (Node : Turkixir_Node'Class)
   is
   begin
      Check_Safety_Net (Node);
      Assign_Names_To_Logic_Vars (Node.Internal.Node);
   end Assign_Names_To_Logic_Vars;

   -------------------------
   -- Children_And_Trivia --
   -------------------------

   function Children_And_Trivia
     (Node : Turkixir_Node'Class) return Children_Array
   is
   begin
      Check_Safety_Net (Node);
      declare
         Bare_Result : constant Bare_Children_Array :=
            Children_And_Trivia (Unwrap_Node (Node));
         Result      : Children_Array (Bare_Result'Range);
      begin
         for I in Bare_Result'Range loop
            declare
               BR : Bare_Child_Record renames Bare_Result (I);
               R  : Child_Record renames Result (I);
            begin
               case BR.Kind is
                  when Child =>
                     R := (Child, Wrap_Node (BR.Node));
                  when Trivia =>
                     R := (Trivia, BR.Trivia);
               end case;
            end;
         end loop;
         return Result;
      end;
   end Children_And_Trivia;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Self : Token_Iterator) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node);
      return Token_Start (Self.Node);
   end First_Token;

   ----------------
   -- Next_Token --
   ----------------

   function Next_Token
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node);
      return Next (Tok);
   end Next_Token;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Self : Token_Iterator; Tok : Token_Reference) return Boolean is
   begin
      Check_Safety_Net (Self.Node);
      return Get_Token_Index (Tok).Token <= Self.Last;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node);
      return Tok;
   end Element;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Context : in out Analysis_Context) is
   begin
      Context.Internal := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Context : in out Analysis_Context) is
   begin
      Inc_Ref (Unwrap_Context (Context));
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Context : in out Analysis_Context) is
      Ctx : Internal_Context := Unwrap_Context (Context);
   begin
      Dec_Ref (Ctx);
      Context.Internal := null;
   end Finalize;

   ----------------------------------------------------
   -- Soft links for public/internal type converters --
   ----------------------------------------------------

   function Wrap_Context (Context : Internal_Context) return Analysis_Context;
   function Unwrap_Context
     (Context : Analysis_Context'Class) return Internal_Context;

   function Wrap_Unit (Unit : Internal_Unit) return Analysis_Unit;
   function Unwrap_Unit (Unit : Analysis_Unit'Class) return Internal_Unit;

   function Wrap_Node
     (Node : Bare_Turkixir_Node;
      Info : Internal_Entity_Info := No_Entity_Info)
      return Turkixir_Node;
   function Unwrap_Node
     (Node : Turkixir_Node'Class) return Bare_Turkixir_Node;
   function Unwrap_Entity
     (Entity : Turkixir_Node'Class) return Internal_Entity;

   ------------------
   -- Wrap_Context --
   ------------------

   function Wrap_Context (Context : Internal_Context) return Analysis_Context
   is
   begin
      Inc_Ref (Context);
      return (Ada.Finalization.Controlled with
              Internal => Internal_Context_Access (Context));
   end Wrap_Context;

   --------------------
   -- Unwrap_Context --
   --------------------

   function Unwrap_Context
     (Context : Analysis_Context'Class) return Internal_Context
   is (Internal_Context (Context.Internal));

   ---------------
   -- Wrap_Unit --
   ---------------

   function Wrap_Unit (Unit : Internal_Unit) return Analysis_Unit
   is ((Internal => Internal_Unit_Access (Unit),
        Context  => Wrap_Context (Context (Unit))));

   -----------------
   -- Unwrap_Unit --
   -----------------

   function Unwrap_Unit (Unit : Analysis_Unit'Class) return Internal_Unit
   is (Internal_Unit (Unit.Internal));

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Self : Turkixir_Node'Class) is
      R  : Env_Rebindings renames Self.Internal.Info.Rebindings;
      SN : Node_Safety_Net renames Self.Safety_Net;
   begin
      if SN.Context = null then
         return;
      end if;

      --  Check that SN's context has not been released (see the Context_Pool)
      if SN.Context.Serial_Number /= SN.Context_Serial then
         raise Stale_Reference_Error with "context was released";

      --  Then check that the unit version is the same
      elsif SN.Unit.Unit_Version /= SN.Unit_Version then
         raise Stale_Reference_Error with "unit was reparsed";

      --  Then check that the R rebindings reference, if not-null, is not stale
      elsif R /= null and then R.Version /= SN.Rebindings_Version then
         raise Stale_Reference_Error with "related unit was reparsed";
      end if;
   end Check_Safety_Net;

   ---------------
   -- Wrap_Node --
   ---------------

   function Wrap_Node
     (Node : Bare_Turkixir_Node;
      Info : Internal_Entity_Info := No_Entity_Info)
      return Turkixir_Node is
   begin
      if Node = null then
         return No_Turkixir_Node;
      end if;

      declare
         Unit               : constant Internal_Unit := Node.Unit;
         Context            : constant Internal_Context := Unit.Context;
         Rebindings_Version : constant Version_Number :=
           (if Info.Rebindings = null
            then 0
            else Info.Rebindings.Version);
      begin
         return ((Internal   => (Node, Info),
                  Safety_Net => (Context            => Context,
                                 Context_Serial     => Context.Serial_Number,
                                 Unit               => Unit,
                                 Unit_Version       => Unit.Unit_Version,
                                 Rebindings_Version => Rebindings_Version)));
      end;
   end;

   -----------------
   -- Unwrap_Node --
   -----------------

   function Unwrap_Node
     (Node : Turkixir_Node'Class) return Bare_Turkixir_Node
   is (Node.Internal.Node);

   -------------------
   -- Unwrap_Entity --
   -------------------

   function Unwrap_Entity
     (Entity : Turkixir_Node'Class) return Internal_Entity
   is ((Entity.Internal));

   


begin
   Public_Converters.Wrap_Context := Wrap_Context'Access;
   Public_Converters.Unwrap_Context := Unwrap_Context'Access;
   Public_Converters.Wrap_Unit := Wrap_Unit'Access;
   Public_Converters.Unwrap_Unit := Unwrap_Unit'Access;
   Public_Converters.Wrap_Node := Wrap_Node'Access;
   Public_Converters.Unwrap_Node := Unwrap_Node'Access;
   Public_Converters.Unwrap_Entity := Unwrap_Entity'Access;
end Libturkixirlang.Analysis;
