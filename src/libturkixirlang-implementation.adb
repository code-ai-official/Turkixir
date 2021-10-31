









with Ada.Containers;                  use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

pragma Warnings (Off, "internal");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
use Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "internal");

with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with GNAT.Traceback.Symbolic;

with GNATCOLL.Traces;

with Langkit_Support.Hashes;  use Langkit_Support.Hashes;
with Langkit_Support.Images;  use Langkit_Support.Images;
with Langkit_Support.Relative_Get;

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
pragma Warnings (On, "referenced");

with Libturkixirlang.Private_Converters;
use Libturkixirlang.Private_Converters;
with Libturkixirlang.Introspection_Implementation;

pragma Warnings (Off, "referenced");


pragma Warnings (On, "referenced");



package body Libturkixirlang.Implementation is

   use Libturkixirlang.Common.Precomputed_Symbols;

   package Context_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Internal_Context);

   type Contexts_Destructor is limited
      new Ada.Finalization.Limited_Controlled with null record;
   overriding procedure Finalize (CD : in out Contexts_Destructor);
   --  Helper to destroy all contexts when terminating the process

   protected Context_Pool is

      procedure Acquire (Context : out Internal_Context)
         with Post => Context /= null;
      --  If a context is free for reuse, increment its serial number and
      --  return it. Otherwise, allocate a new one. In any case, this does not
      --  initialize it, except for the Serial_Number field.

      procedure Release (Context : in out Internal_Context)
         with Pre  => Context /= null,
              Post => Context = null;
      --  Tag Context as free for reuse and set it to null

      procedure Free;
      --  Free all contexts in this pool. Intended to be called only when the
      --  process is terminating, to avoid reported memory leaks.

   private

      Available : Context_Vectors.Vector;
      --  List of allocated contexts that can be re-used right now

      CD : Contexts_Destructor with Unreferenced;
      --  Singleton whose only purpose is to free all contexts in Available
      --  when finalized.

   end Context_Pool;

   procedure Register_Destroyable_Helper
     (Unit    : Internal_Unit;
      Object  : System.Address;
      Destroy : Destroy_Procedure);
   --  Common underlying implementation for Register_Destroyable_Gen

   pragma Warnings (Off, "referenced");
   function Construct_Entity_Array
     (V : AST_Envs.Entity_Vectors.Vector) return Internal_Entity_Array_Access;
   pragma Warnings (On, "referenced");

   procedure Destroy (Env : in out Lexical_Env_Access);

   function Snaps_At_Start (Self : Bare_Turkixir_Node) return Boolean;
   function Snaps_At_End (Self : Bare_Turkixir_Node) return Boolean;

   --  Those maps are used to give unique ids to lexical envs while pretty
   --  printing them.

   package Address_To_Id_Maps is new Ada.Containers.Hashed_Maps
     (Lexical_Env, Integer, Hash, "=");

   type Dump_Lexical_Env_State is record
      Env_Ids : Address_To_Id_Maps.Map;
      --  Mapping: Lexical_Env -> Integer, used to remember which unique Ids we
      --  assigned to the lexical environments we found.

      Next_Id : Positive := 1;
      --  Id to assign to the next unknown lexical environment

      Root_Env : Lexical_Env;
      --  Lexical environment we consider a root (this is the Root_Scope from
      --  the current analysis context), or null if unknown.
   end record;
   --  Holder for the state of lexical environment dumpers

   function Get_Env_Id
     (E : Lexical_Env; State : in out Dump_Lexical_Env_State) return String;
   --  If E is known, return its unique Id from State. Otherwise, assign it a
   --  new unique Id and return it.

   ----------------------------
   -- Construct_Entity_Array --
   ----------------------------

   function Construct_Entity_Array
     (V : AST_Envs.Entity_Vectors.Vector) return Internal_Entity_Array_Access
   is
      Ret : Internal_Entity_Array_Access :=
        Create_Internal_Entity_Array (V.Length);
   begin
      for J in V.First_Index .. V.Last_Index loop
         Ret.Items (J) := V.Get (J);
      end loop;

      declare
         Tmp : AST_Envs.Entity_Vectors.Vector := V;
      begin
         Tmp.Destroy;
      end;

      return Ret;
   end Construct_Entity_Array;

   ----------------
   -- Enter_Call --
   ----------------

   procedure Enter_Call
     (Context : Internal_Context; Call_Depth : access Natural)
   is
      Max             : Natural renames Context.Max_Call_Depth;
      Current         : Natural renames Context.Current_Call_Depth;
      High_Water_Mark : Natural renames Context.Call_Depth_High_Water_Mark;
   begin
      Current := Current + 1;
      High_Water_Mark := Natural'Max (High_Water_Mark, Current);
      Call_Depth.all := Current;
      if Current > Max then
         raise Property_Error with "stack overflow";
      end if;
   end Enter_Call;

   ---------------
   -- Exit_Call --
   ---------------

   procedure Exit_Call (Context : Internal_Context; Call_Depth : Natural) is
      Current : Natural renames Context.Current_Call_Depth;
   begin
      if Call_Depth /= Current then
         raise Unexpected_Call_Depth with
            "Langkit code generation bug for call depth handling detected";
      end if;
      Current := Current - 1;
   end Exit_Call;

   -----------
   -- Image --
   -----------

   function Image (Self : Symbol_Type) return String_Type is
   begin
      return Create_String (Image (Self));
   end Image;

   ------------------
   -- Context_Pool --
   ------------------

   protected body Context_Pool is

      -------------
      -- Acquire --
      -------------

      procedure Acquire (Context : out Internal_Context) is
      begin
         if Available.Is_Empty then
            Context := new Analysis_Context_Type;
            Context.Serial_Number := 1;
         else
            Context := Available.Last_Element;
            Available.Delete_Last;
         end if;
      end Acquire;

      -------------
      -- Release --
      -------------

      procedure Release (Context : in out Internal_Context) is
      begin
         Available.Append (Context);
         Context.Serial_Number := Context.Serial_Number + 1;
         Context := null;
      end Release;

      ----------
      -- Free --
      ----------

      procedure Free is
      begin
         for C of Available loop
            Free (C);
         end loop;
      end Free;

   end Context_Pool;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (CD : in out Contexts_Destructor) is
      pragma Unreferenced (CD);
   begin
      Context_Pool.Free;
   end Finalize;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (File_Reader : in out Internal_File_Reader_Access) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Internal_File_Reader'Class, Internal_File_Reader_Access);
   begin
      if File_Reader /= null and then File_Reader.all.Dec_Ref then
         Destroy (File_Reader);
      end if;
   end Dec_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Internal_Event_Handler_Access) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Internal_Event_Handler'Class, Internal_Event_Handler_Access);
   begin
      if Self /= null and then Self.all.Dec_Ref then
         Destroy (Self);
      end if;
   end Dec_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Provider : in out Internal_Unit_Provider_Access) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Internal_Unit_Provider'Class, Internal_Unit_Provider_Access);
   begin
      if Provider /= null and then Provider.all.Dec_Ref then
         Destroy (Provider);
      end if;
   end Dec_Ref;

   ----------------
   -- Get_Env_Id --
   ----------------

   function Get_Env_Id
     (E : Lexical_Env; State : in out Dump_Lexical_Env_State) return String
   is
      C        : Address_To_Id_Maps.Cursor;
      Inserted : Boolean;
   begin
      if E = Null_Lexical_Env then
         return "$null";

      elsif E = State.Root_Env then
         --  Insert root env with a special Id so that we only print it once
         State.Env_Ids.Insert (E, -1, C, Inserted);
         return "$root";
      end if;

      State.Env_Ids.Insert (E, State.Next_Id, C, Inserted);
      if Inserted then
         State.Next_Id := State.Next_Id + 1;
      end if;

      return '@' & Stripped_Image (Address_To_Id_Maps.Element (C));
   end Get_Env_Id;

   pragma Warnings (Off, "referenced");
   function To_Lookup_Kind_Type (K : Lookup_Kind) return Lookup_Kind_Type
   is
     (Lookup_Kind_Type'Val (Lookup_Kind'Pos (K)));
   pragma Warnings (On, "referenced");

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Charset        : String;
      File_Reader    : Internal_File_Reader_Access;
      Unit_Provider  : Internal_Unit_Provider_Access;
      Event_Handler  : Internal_Event_Handler_Access;
      With_Trivia    : Boolean;
      Tab_Stop       : Positive;
      Max_Call_Depth : Natural := 1000)
      return Internal_Context
   is
      Actual_Charset : constant String :=
        (if Charset = "" then Default_Charset else Charset);
      Symbols        : constant Precomputed_Symbol_Table
        := Create_Symbol_Table;
      Context        : Internal_Context;
   begin
      Context_Pool.Acquire (Context);
      Context.Ref_Count := 1;
      Context.Symbols := Symbol_Table (Symbols);
      Context.Charset := To_Unbounded_String (Actual_Charset);
      Context.Tab_Stop := Tab_Stop;
      Context.With_Trivia := With_Trivia;
      Context.Root_Scope := Create_Static_Lexical_Env
        (Parent => Null_Lexical_Env,
         Node   => null);

      --  Create a new ownership share for Event_Handler so that it lives at
      --  least as long as this analysis context.
      Context.Event_Handler := Event_Handler;
      if Context.Event_Handler /= null then
         Context.Event_Handler.Inc_Ref;
      end if;

      --  Create a new ownership share for File_Reader so that it lives at
      --  least as long as this analysis context.
      Context.File_Reader := File_Reader;
      if Context.File_Reader /= null then
         Context.File_Reader.Inc_Ref;
      end if;

      --  Create a new ownership share for Unit_Provider so that it lives at
      --  least as long as this analysis context.
      Context.Unit_Provider := Unit_Provider;
      if Context.Unit_Provider /= null then
         Context.Unit_Provider.Inc_Ref;
      end if;


      Initialize (Context.Parser);

      Context.Discard_Errors_In_Populate_Lexical_Env := True;
      Context.Logic_Resolution_Timeout :=
        Langkit_Support.Adalog.Default_Timeout_Ticks_Number;
      Context.In_Populate_Lexical_Env := False;
      Context.Cache_Version := 0;
      Context.Reparse_Cache_Version := 0;

      Context.Rewriting_Handle := No_Rewriting_Handle_Pointer;
      Context.Templates_Unit := No_Analysis_Unit;

      Context.Max_Call_Depth := Max_Call_Depth;

      Context.Available_Rebindings := Env_Rebindings_Vectors.Empty_Vector;

      


      return Context;
   end Create_Context;

   -----------------
   -- Create_Unit --
   -----------------

   function Create_Unit
     (Context             : Internal_Context;
      Normalized_Filename : Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Internal_Unit
   is
      use Units_Maps;

      Unit : Internal_Unit;
   begin
      Unit := Create_Special_Unit
        (Context, Normalized_Filename, Charset, Rule);
      Context.Units.Insert (Normalized_Filename, Unit);
      return Unit;
   end Create_Unit;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Context           : Internal_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Input             : Internal_Lexer_Input;
      Rule              : Grammar_Rule;
      Is_Internal       : Boolean := False) return Internal_Unit
   is
      use Units_Maps;

      Normalized_Filename : constant GNATCOLL.VFS.Virtual_File :=
         Normalized_Unit_Filename (Context, Filename);

      Cur     : constant Cursor :=
         Context.Units.Find (Normalized_Filename);
      Created : constant Boolean := Cur = No_Element;
      Unit    : Internal_Unit;

      Actual_Charset : Unbounded_String;
      Refined_Input  : Internal_Lexer_Input := Input;

   begin
      --  Determine which encoding to use. Use the Charset parameter (if
      --  provided), otherwise use the context-wide default.

      Actual_Charset := (if Charset'Length /= 0
                         then To_Unbounded_String (Charset)
                         else Context.Charset);

      if Refined_Input.Kind = File then
         Refined_Input.Filename := Normalized_Filename;
      end if;

      if Refined_Input.Kind in File | Bytes_Buffer then
         Refined_Input.Charset := Actual_Charset;

         --  Unless the caller requested a specific charset for this unit,
         --  allow the lexer to automatically discover the source file encoding
         --  before defaulting to the context-specific one. We do this trying
         --  to match a byte order mark.

         Refined_Input.Read_BOM := Charset'Length = 0;
      end if;

      --  Create the Internal_Unit if needed

      Unit :=
        (if Created
         then Create_Unit (Context, Normalized_Filename,
                           To_String (Actual_Charset), Rule)
         else Element (Cur));

      --  If an internal unit is requested, set the corresponding flag.
      --  Otherwise, make sure that the unit we return isn't internal.

      if Is_Internal then
         Unit.Is_Internal := True;
      end if;

      --  (Re)parse it if needed

      if Created or else Reparse then

         --  It is illegal to reparse an internal unit for public API users.
         --  Since public APIs do not allow to pass True to Is_Internal, we can
         --  check here that only the implementation can ask to reparse an
         --  internal unit.

         if Unit.Is_Internal and then not Is_Internal then
            raise Precondition_Failure with "cannot reparse an internal unit";
         end if;

         declare
            Reparsed : Reparsed_Unit;
         begin
            Do_Parsing (Unit, Refined_Input, Reparsed);
            Update_After_Reparse (Unit, Reparsed);
         end;

         --  Now that we have removed reparsed the unit, update its current
         --  charset.

         Unit.Charset := Actual_Charset;
      end if;

      if Context.Event_Handler /= null then
         Context.Event_Handler.Unit_Parsed_Callback (Context, Unit, Reparse);
      end if;

      return Unit;
   end Get_Unit;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit
     (Context : Internal_Context; Unit_Filename : String) return Boolean is
   begin
      return Context.Units.Contains
        (Normalized_Unit_Filename (Context, Unit_Filename));
   end Has_Unit;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Reparse  : Boolean;
      Rule     : Grammar_Rule) return Internal_Unit
   is
      Input : constant Internal_Lexer_Input :=
        (Kind     => File,
         Charset  => <>,
         Read_BOM => False,
         Filename => <>);
   begin
      if Reparse and then Has_Rewriting_Handle (Context) then
         raise Precondition_Failure with
            "cannot reparse during tree rewriting";
      end if;

      return Get_Unit (Context, Filename, Charset, Reparse, Input, Rule);
   end Get_From_File;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Buffer   : String;
      Rule     : Grammar_Rule) return Internal_Unit
   is
      Input : constant Internal_Lexer_Input :=
        (Kind        => Bytes_Buffer,
         Charset     => <>,
         Read_BOM    => False,
         Bytes       => Buffer'Address,
         Bytes_Count => Buffer'Length);
   begin
      if Has_Rewriting_Handle (Context) then
         raise Precondition_Failure with
            "cannot parse from buffer during tree rewriting";

      elsif Context.File_Reader /= null then
         raise Precondition_Failure with
            "cannot parse from buffer with a file reader";
      end if;

      return Get_Unit (Context, Filename, Charset, True, Input, Rule);
   end Get_From_Buffer;

   --------------------
   -- Get_With_Error --
   --------------------

   function Get_With_Error
     (Context  : Internal_Context;
      Filename : String;
      Error    : Text_Type;
      Charset  : String;
      Rule     : Grammar_Rule) return Internal_Unit
   is
      use Units_Maps;

      Normalized_Filename : constant Virtual_File :=
         Normalized_Unit_Filename (Context, Filename);
      Cur                 : constant Cursor :=
         Context.Units.Find (Normalized_Filename);
   begin
      if Cur = No_Element then
         declare
            Unit : constant Internal_Unit := Create_Unit
              (Context, Normalized_Filename, Charset, Rule);
         begin
            Append (Unit.Diagnostics, No_Source_Location_Range, Error);
            return Unit;
         end;
      else
         return Element (Cur);
      end if;
   end Get_With_Error;


   -------------------
   -- Unit_Provider --
   -------------------

   function Unit_Provider
     (Context : Internal_Context) return Internal_Unit_Provider_Access
   is (Context.Unit_Provider);

   ----------
   -- Hash --
   ----------

   function Hash (Context : Internal_Context) return Hash_Type is
      function H is new Hash_Access (Analysis_Context_Type, Internal_Context);
   begin
      return H (Context);
   end Hash;

   ---------------------
   -- Has_With_Trivia --
   ---------------------

   function Has_With_Trivia (Context : Internal_Context) return Boolean is
   begin
      return Context.With_Trivia;
   end Has_With_Trivia;

   --------------------------------------------
   -- Discard_Errors_In_Populate_Lexical_Env --
   --------------------------------------------

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Internal_Context; Discard : Boolean) is
   begin
      Context.Discard_Errors_In_Populate_Lexical_Env := Discard;
   end Discard_Errors_In_Populate_Lexical_Env;

   ----------------------------------
   -- Set_Logic_Resolution_Timeout --
   ----------------------------------

   procedure Set_Logic_Resolution_Timeout
     (Context : Internal_Context; Timeout : Natural) is
   begin
      Context.Logic_Resolution_Timeout := Timeout;
   end Set_Logic_Resolution_Timeout;

   --------------------------
   -- Has_Rewriting_Handle --
   --------------------------

   function Has_Rewriting_Handle (Context : Internal_Context) return Boolean is
   begin
      return Context.Rewriting_Handle /= No_Rewriting_Handle_Pointer;
   end Has_Rewriting_Handle;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Context : Internal_Context) is
   begin
      if Context /= null then
         Context.Ref_Count := Context.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Context : in out Internal_Context) is
   begin
      if Context /= null then
         Context.Ref_Count := Context.Ref_Count - 1;
         if Context.Ref_Count = 0 then
            Destroy (Context);
         end if;
      end if;
   end Dec_Ref;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out Internal_Context) is
   begin
      --  Destroy all named environment data structures
      for Desc of Context.Named_Envs loop
         for V of Desc.Foreign_Nodes loop
            V.Destroy;
         end loop;
         Destroy (Desc);
      end loop;
      Context.Named_Envs.Clear;

      --  If we are asked to free this context, it means that no one else have
      --  references to its analysis units, so it's safe to destroy these.
      for Unit of Context.Units loop
         Destroy (Unit);
      end loop;
      Context.Units := Units_Maps.Empty_Map;
      Context.Filenames := Virtual_File_Maps.Empty_Map;

      declare
         procedure Destroy is new Ada.Unchecked_Deallocation
           (Env_Rebindings_Type, Env_Rebindings);

         AR : Env_Rebindings_Vectors.Vector renames
            Context.Available_Rebindings;
         R  : Env_Rebindings;
      begin
         for I in AR.First_Index .. AR.Last_Index loop
            R := AR.Get (I);
            Destroy (R);
         end loop;
         AR.Destroy;
      end;

      Destroy (Context.Templates_Unit);
      AST_Envs.Destroy (Context.Root_Scope);
      Destroy (Context.Symbols);
      Destroy (Context.Parser);
      Dec_Ref (Context.File_Reader);
      Dec_Ref (Context.Unit_Provider);
      Dec_Ref (Context.Event_Handler);
      Context_Pool.Release (Context);
   end Destroy;

   -------------
   -- Context --
   -------------

   function Context (Unit : Internal_Unit) return Internal_Context is
   begin
      return Unit.Context;
   end Context;

   ----------
   -- Hash --
   ----------

   function Hash (Unit : Internal_Unit) return Hash_Type is
      function H is new Hash_Access (Analysis_Unit_Type, Internal_Unit);
   begin
      return H (Unit);
   end Hash;

   -------------
   -- Reparse --
   -------------

   procedure Reparse (Unit : Internal_Unit; Charset : String) is
      Dummy : constant Internal_Unit := Get_From_File
        (Unit.Context, +Unit.Filename.Full_Name, Charset,
         Reparse => True,
         Rule    => Unit.Rule);
   begin
      null;
   end Reparse;

   -------------
   -- Reparse --
   -------------

   procedure Reparse (Unit : Internal_Unit; Charset : String; Buffer : String)
   is
      Dummy : constant Internal_Unit := Get_From_Buffer
        (Unit.Context, +Unit.Filename.Full_Name, Charset, Buffer, Unit.Rule);
   begin
      null;
   end Reparse;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env (Unit : Internal_Unit) is
      Context : constant Internal_Context := Unit.Context;

      Has_Errors : Boolean := False;
      --  Whether at least one Property_Error occurred during this PLE pass

      Saved_In_Populate_Lexical_Env : constant Boolean :=
         Unit.Context.In_Populate_Lexical_Env;

      procedure Reset_Envs_Caches (Unit : Internal_Unit) is
         procedure Internal (Node : Bare_Turkixir_Node) is
         begin
            if Node = null then
               return;
            end if;
            Reset_Caches (Node.Self_Env);
            for I in 1 .. Children_Count (Node) loop
               Internal (Child (Node, I));
            end loop;
         end Internal;
      begin
         Internal (Unit.AST_Root);
      end Reset_Envs_Caches;

   begin
      --  TODO??? Handle env invalidation when reparsing a unit and when a
      --  previous call raised a Property_Error.
      if Unit.Is_Env_Populated then
         return;
      end if;
      Unit.Is_Env_Populated := True;

      if Unit.AST_Root = null then
         return;
      end if;

      GNATCOLL.Traces.Trace (Main_Trace, "Populating lexical envs for unit: "
                                         & Basename (Unit));
      GNATCOLL.Traces.Increase_Indent (Main_Trace);

      Context.In_Populate_Lexical_Env := True;
      Has_Errors := Populate_Lexical_Env (Unit.AST_Root);
      Context.In_Populate_Lexical_Env := Saved_In_Populate_Lexical_Env;

      GNATCOLL.Traces.Decrease_Indent (Main_Trace);
      GNATCOLL.Traces.Trace
        (Main_Trace,
         "Finished populating lexical envs for unit: " & Basename (Unit));

      Reset_Envs_Caches (Unit);

      if Has_Errors and then not Context.Discard_Errors_In_Populate_Lexical_Env
      then
         raise Property_Error with
            "errors occurred in Populate_Lexical_Env";
      end if;
   end Populate_Lexical_Env;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Unit : Internal_Unit) return String is
     (+Unit.Filename.Full_Name);

   -----------------
   -- Get_Charset --
   -----------------

   function Get_Charset (Unit : Internal_Unit) return String is
   begin
      return To_String (Unit.Charset);
   end Get_Charset;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics (Unit : Internal_Unit) return Boolean is
   begin
      return not Unit.Diagnostics.Is_Empty;
   end Has_Diagnostics;

   -----------------
   -- Diagnostics --
   -----------------

   function Diagnostics (Unit : Internal_Unit) return Diagnostics_Array is
      Result : Diagnostics_Array (1 .. Natural (Unit.Diagnostics.Length));
      I      : Natural := 1;
   begin
      for D of Unit.Diagnostics loop
         Result (I) := D;
         I := I + 1;
      end loop;
      return Result;
   end Diagnostics;

   ---------------------------
   -- Format_GNU_Diagnostic --
   ---------------------------

   function Format_GNU_Diagnostic
     (Unit : Internal_Unit; D : Diagnostic) return String
   is
      Filename : constant String := Basename (Unit);
      Sloc     : constant Source_Location := Start_Sloc (D.Sloc_Range);
      Msg      : constant String :=
         Image
           (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (D.Message));
   begin
      return (Filename
              & (if Sloc = No_Source_Location then "" else ":" & Image (Sloc))
              & ": " & Msg);
   end Format_GNU_Diagnostic;

   ----------
   -- Root --
   ----------

   function Root (Unit : Internal_Unit) return Bare_Turkixir_Node is
     (Unit.AST_Root);

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Unit : Internal_Unit) return Token_Reference is
     (Wrap_Token_Reference (Unit.Context,
                            Unit.TDH'Access,
                            First_Token_Or_Trivia (Unit.TDH)));

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Unit : Internal_Unit) return Token_Reference is
     (Wrap_Token_Reference (Unit.Context,
                            Unit.TDH'Access,
                            Last_Token_Or_Trivia (Unit.TDH)));

   -----------------
   -- Token_Count --
   -----------------

   function Token_Count (Unit : Internal_Unit) return Natural is
     (Unit.TDH.Tokens.Length);

   ------------------
   -- Trivia_Count --
   ------------------

   function Trivia_Count (Unit : Internal_Unit) return Natural is
     (Unit.TDH.Trivias.Length);

   ----------
   -- Text --
   ----------

   function Text (Unit : Internal_Unit) return Text_Type is
   begin
      return Text (First_Token (Unit), Last_Token (Unit));
   end Text;

   ------------------
   -- Lookup_Token --
   ------------------

   function Lookup_Token
     (Unit : Internal_Unit; Sloc : Source_Location) return Token_Reference
   is
      Result : constant Token_Or_Trivia_Index := Lookup_Token (Unit.TDH, Sloc);
   begin
      return Wrap_Token_Reference (Unit.Context, Unit.TDH'Access, Result);
   end Lookup_Token;

   ----------------------
   -- Dump_Lexical_Env --
   ----------------------

   procedure Dump_Lexical_Env (Unit : Internal_Unit) is
      Node     : constant Bare_Turkixir_Node := Unit.AST_Root;
      Root_Env : constant Lexical_Env := Unit.Context.Root_Scope;
      State    : Dump_Lexical_Env_State := (Root_Env => Root_Env, others => <>);

      function Get_Parent (Env : Lexical_Env) return Lexical_Env
      is (Unwrap (Env).Parent);

      --------------------------
      -- Explore_Parent_Chain --
      --------------------------

      procedure Explore_Parent_Chain (Env : Lexical_Env) is
         P : Lexical_Env;
      begin
         if Env /= Null_Lexical_Env then
            P := Get_Parent (Env);
            Dump_One_Lexical_Env
              (Env, Get_Env_Id (Env, State), Get_Env_Id (P, State));
            Explore_Parent_Chain (P);
         end if;
      end Explore_Parent_Chain;

      --------------
      -- Internal --
      --------------

      procedure Internal (Current : Bare_Turkixir_Node) is
         Explore_Parent : Boolean := False;
         Env, Parent    : Lexical_Env;
      begin
         if Current = null then
            return;
         end if;

         --  We only dump environments that we haven't dumped before. This way
         --  we'll only dump environments at the site of their creation, and
         --  not in any subsequent link. We use the Env_Ids map to check which
         --  envs we have already seen or not.
         if not State.Env_Ids.Contains (Current.Self_Env) then
            Env := Current.Self_Env;
            Parent := Get_Parent (Env);
            Explore_Parent := not State.Env_Ids.Contains (Parent);

            Dump_One_Lexical_Env
              (Env, Get_Env_Id (Env, State), Get_Env_Id (Parent, State));

            if Explore_Parent then
               Explore_Parent_Chain (Parent);
            end if;
         end if;

         for Child of Internal_Bare_Turkixir_Node_Array'(Children (Current))
         loop
            Internal (Child);
         end loop;
      end Internal;
      --  This procedure implements the main recursive logic of dumping the
      --  environments.
   begin
      Internal (Bare_Turkixir_Node (Node));
   end Dump_Lexical_Env;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type
   is
   begin
      return Get_Line (Unit.TDH, Line_Number);
   end Get_Line;

   -----------
   -- Print --
   -----------

   procedure Print (Unit : Internal_Unit; Show_Slocs : Boolean) is
   begin
      if Unit.AST_Root = null then
         Put_Line ("<empty analysis unit>");
      else
         Print (Unit.AST_Root, Show_Slocs);
      end if;
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia (Unit : Internal_Unit) is

      procedure Process (Trivia : Token_Index) is
         Data : constant Stored_Token_Data :=
            Unit.TDH.Trivias.Get (Natural (Trivia)).T;
      begin
         Put_Line (Image (Text (Unit.TDH, Data)));
      end Process;

      Last_Token : constant Token_Index :=
         Token_Index (Token_Vectors.Last_Index (Unit.TDH.Tokens) - 1);
      --  Index for the last token in Unit excluding the Termination token
      --  (hence the -1).
   begin
      for Tok of Get_Leading_Trivias (Unit.TDH) loop
         Process (Tok);
      end loop;

      PP_Trivia (Unit.AST_Root);

      for Tok of Get_Trivias (Unit.TDH, Last_Token) loop
         Process (Tok);
      end loop;
   end PP_Trivia;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Unit : in out Internal_Unit) is
   begin
      if Unit = No_Analysis_Unit then
         return;
      end if;

      Unit.Exiled_Entries.Destroy;
      Unit.Foreign_Nodes.Destroy;
      Unit.Exiled_Entries_In_NED.Destroy;
      Unit.Exiled_Envs.Destroy;
      Unit.Named_Envs.Destroy;
      Analysis_Unit_Sets.Destroy (Unit.Referenced_Units);


      Destroy_Rebindings (Unit.Rebindings'Access);
      Unit.Rebindings.Destroy;

      if Unit.AST_Root /= null then
         Destroy (Unit.AST_Root);
      end if;

      Free (Unit.TDH);
      Free (Unit.AST_Mem_Pool);
      Destroy_Unit_Destroyables (Unit);
      Destroyable_Vectors.Destroy (Unit.Destroyables);
      

      Free (Unit);
   end Destroy;

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Node : Bare_Turkixir_Node) return Boolean is
   begin
      return Is_Token_Node (Node.Kind);
   end Is_Token_Node;

   ------------------
   -- Is_Synthetic --
   ------------------

   function Is_Synthetic (Node : Bare_Turkixir_Node) return Boolean is
   begin
      return Node.Kind in Synthetic_Nodes;
   end Is_Synthetic;

   ------------------------------
   -- Register_Destroyable_Gen --
   ------------------------------

   procedure Register_Destroyable_Gen
     (Unit : Internal_Unit; Object : T_Access)
   is
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Destroy_Procedure);
      procedure Destroy_Procedure (Object : in out T_Access) renames Destroy;
   begin
      Register_Destroyable_Helper
        (Unit,
         Object.all'Address,
         Convert (Destroy_Procedure'Address));
   end Register_Destroyable_Gen;

      

   


   ---------
   -- Get --
   ---------

   function Get
     (T       : Bare_Turkixir_Node_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Bare_Turkixir_Node
   is
      function Absolute_Get
        (T : Bare_Turkixir_Node_Array_Access; Index : Integer)
         return Bare_Turkixir_Node
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => Bare_Turkixir_Node,
         Sequence_Type => Bare_Turkixir_Node_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Bare_Turkixir_Node;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Bare_Turkixir_Node;
      else
         raise Property_Error with "out-of-bounds array access";
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Bare_Turkixir_Node_Array_Access) return Bare_Turkixir_Node_Array_Access is
      Ret : Bare_Turkixir_Node_Array_Access := Create_Bare_Turkixir_Node_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Bare_Turkixir_Node_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Bare_Turkixir_Node_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Bare_Turkixir_Node_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Bare_Turkixir_Node_Array (Items_Count : Natural) return Bare_Turkixir_Node_Array_Access
   is (if Items_Count = 0
       then No_Bare_Turkixir_Node_Array_Type
       else new Bare_Turkixir_Node_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Bare_Turkixir_Node_Array
     (Items : Internal_Bare_Turkixir_Node_Array) return Bare_Turkixir_Node_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Bare_Turkixir_Node_Array_Type;
      end if;

      return new Bare_Turkixir_Node_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Bare_Turkixir_Node_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if
               L.Items (I) /= R.Items (I)
         then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Bare_Turkixir_Node_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   


   ---------
   -- Get --
   ---------

   function Get
     (T       : Internal_Entity_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity
   is
      function Absolute_Get
        (T : Internal_Entity_Array_Access; Index : Integer)
         return Internal_Entity
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => Internal_Entity,
         Sequence_Type => Internal_Entity_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Entity;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Entity;
      else
         raise Property_Error with "out-of-bounds array access";
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Entity_Array_Access) return Internal_Entity_Array_Access is
      Ret : Internal_Entity_Array_Access := Create_Internal_Entity_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Entity_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Entity_Array (Items_Count : Natural) return Internal_Entity_Array_Access
   is (if Items_Count = 0
       then No_Internal_Entity_Array_Type
       else new Internal_Entity_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));

   function Create_Internal_Entity_Array
     (Items : AST_Envs.Entity_Array) return Internal_Entity_Array_Access
   is (if Items'Length = 0
       then No_Internal_Entity_Array_Type
       else new Internal_Entity_Array_Record'
         (N         => Items'Length,
          Items     => Implementation.Internal_Internal_Entity_Array (Items),
          Ref_Count => 1));

   function Create_Internal_Entity_Array
     (Items : Internal_Internal_Entity_Array) return Internal_Entity_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Entity_Array_Type;
      end if;

      return new Internal_Entity_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Entity_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if
               L.Items (I) /= R.Items (I)
         then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

      package Internal_Env_Assoc_Vectors is new Langkit_Support.Vectors (Internal_Env_Assoc);

   ---------
   -- Get --
   ---------

   function Get
     (T       : Internal_Env_Assoc_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Env_Assoc
   is
      function Absolute_Get
        (T : Internal_Env_Assoc_Array_Access; Index : Integer)
         return Internal_Env_Assoc
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => Internal_Env_Assoc,
         Sequence_Type => Internal_Env_Assoc_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Env_Assoc;
   begin
      if Relative_Get (T, Index, Result) then
            Inc_Ref (Result);
         return Result;
      elsif Or_Null then
         return No_Env_Assoc;
      else
         raise Property_Error with "out-of-bounds array access";
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Env_Assoc_Array_Access) return Internal_Env_Assoc_Array_Access is
      Ret : Internal_Env_Assoc_Array_Access := Create_Internal_Env_Assoc_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
         for Item of Ret.Items loop
            Inc_Ref (Item);
         end loop;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Env_Assoc_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Env_Assoc_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Env_Assoc_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
            for Item of T.Items loop
               Dec_Ref (Item);
            end loop;
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Env_Assoc_Array (Items_Count : Natural) return Internal_Env_Assoc_Array_Access
   is (if Items_Count = 0
       then No_Internal_Env_Assoc_Array_Type
       else new Internal_Env_Assoc_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Env_Assoc_Array
     (Items : Internal_Internal_Env_Assoc_Array) return Internal_Env_Assoc_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Env_Assoc_Array_Type;
      end if;

         for El of Items loop
            Inc_Ref (El);
         end loop;
      return new Internal_Env_Assoc_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Env_Assoc_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if
               not Equivalent (L.Items (I), R.Items (I))
         then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Env_Assoc_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   


   ---------
   -- Get --
   ---------

   function Get
     (T       : Internal_Inner_Env_Assoc_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Inner_Env_Assoc
   is
      function Absolute_Get
        (T : Internal_Inner_Env_Assoc_Array_Access; Index : Integer)
         return Internal_Inner_Env_Assoc
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => Internal_Inner_Env_Assoc,
         Sequence_Type => Internal_Inner_Env_Assoc_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Inner_Env_Assoc;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Inner_Env_Assoc;
      else
         raise Property_Error with "out-of-bounds array access";
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Inner_Env_Assoc_Array_Access) return Internal_Inner_Env_Assoc_Array_Access is
      Ret : Internal_Inner_Env_Assoc_Array_Access := Create_Internal_Inner_Env_Assoc_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Inner_Env_Assoc_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Inner_Env_Assoc_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Inner_Env_Assoc_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Inner_Env_Assoc_Array (Items_Count : Natural) return Internal_Inner_Env_Assoc_Array_Access
   is (if Items_Count = 0
       then No_Internal_Inner_Env_Assoc_Array_Type
       else new Internal_Inner_Env_Assoc_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Inner_Env_Assoc_Array
     (Items : Internal_Internal_Inner_Env_Assoc_Array) return Internal_Inner_Env_Assoc_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Inner_Env_Assoc_Array_Type;
      end if;

      return new Internal_Inner_Env_Assoc_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Inner_Env_Assoc_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if
               L.Items (I) /= R.Items (I)
         then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Inner_Env_Assoc_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   


   ---------
   -- Get --
   ---------

   function Get
     (T       : Lexical_Env_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Lexical_Env
   is
      function Absolute_Get
        (T : Lexical_Env_Array_Access; Index : Integer)
         return Lexical_Env
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => Lexical_Env,
         Sequence_Type => Lexical_Env_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Lexical_Env;
   begin
      if Relative_Get (T, Index, Result) then
            Inc_Ref (Result);
         return Result;
      elsif Or_Null then
         return Empty_Env;
      else
         raise Property_Error with "out-of-bounds array access";
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Lexical_Env_Array_Access) return Lexical_Env_Array_Access is
      Ret : Lexical_Env_Array_Access := Create_Lexical_Env_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
         for Item of Ret.Items loop
            Inc_Ref (Item);
         end loop;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Lexical_Env_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Lexical_Env_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Lexical_Env_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
            for Item of T.Items loop
               Dec_Ref (Item);
            end loop;
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Lexical_Env_Array (Items_Count : Natural) return Lexical_Env_Array_Access
   is (if Items_Count = 0
       then No_Lexical_Env_Array_Type
       else new Lexical_Env_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Lexical_Env_Array
     (Items : Internal_Lexical_Env_Array) return Lexical_Env_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Lexical_Env_Array_Type;
      end if;

         for El of Items loop
            Inc_Ref (El);
         end loop;
      return new Lexical_Env_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Lexical_Env_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if
               not Equivalent (L.Items (I), R.Items (I))
         then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Lexical_Env_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   


   ---------
   -- Get --
   ---------

   function Get
     (T       : Symbol_Type_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Symbol_Type
   is
      function Absolute_Get
        (T : Symbol_Type_Array_Access; Index : Integer)
         return Symbol_Type
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => Symbol_Type,
         Sequence_Type => Symbol_Type_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Symbol_Type;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return null;
      else
         raise Property_Error with "out-of-bounds array access";
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Symbol_Type_Array_Access) return Symbol_Type_Array_Access is
      Ret : Symbol_Type_Array_Access := Create_Symbol_Type_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Symbol_Type_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Symbol_Type_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Symbol_Type_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Symbol_Type_Array (Items_Count : Natural) return Symbol_Type_Array_Access
   is (if Items_Count = 0
       then No_Symbol_Type_Array_Type
       else new Symbol_Type_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Symbol_Type_Array
     (Items : Internal_Symbol_Type_Array) return Symbol_Type_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Symbol_Type_Array_Type;
      end if;

      return new Symbol_Type_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Symbol_Type_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if
               L.Items (I) /= R.Items (I)
         then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Symbol_Type_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;





         

   

   ----------
   -- Next --
   ----------

   function Next
     (T       : Bare_Turkixir_Node_Iterator_Access;
      Element : out Bare_Turkixir_Node) return Boolean is
   begin
      if T = null then
         raise Property_Error with "null access dereference";
      end if;
      Check_Safety_Net (T.Safety_Net);

      if T.Index > T.Elements.Items'Last then
         return False;
      else
         Element := T.Elements.Items (T.Index);
         T.Index := T.Index + 1;
         return True;
      end if;
   end Next;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Bare_Turkixir_Node_Iterator_Access) is
   begin
      if T /= null and then T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Bare_Turkixir_Node_Iterator_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Dec_Ref (T.Elements);
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Bare_Turkixir_Node_Iterator_Access) return String is
      begin
         return "<Iterator of TurkixirNode, index="
                & A.Index'Image & ">";
      end Trace_Image;


         

   

   ----------
   -- Next --
   ----------

   function Next
     (T       : Internal_Entity_Iterator_Access;
      Element : out Internal_Entity) return Boolean is
   begin
      if T = null then
         raise Property_Error with "null access dereference";
      end if;
      Check_Safety_Net (T.Safety_Net);

      if T.Index > T.Elements.Items'Last then
         return False;
      else
         Element := T.Elements.Items (T.Index);
         T.Index := T.Index + 1;
         return True;
      end if;
   end Next;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Iterator_Access) is
   begin
      if T /= null and then T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Iterator_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Dec_Ref (T.Elements);
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Iterator_Access) return String is
      begin
         return "<Iterator of TurkixirNode.entity, index="
                & A.Index'Image & ">";
      end Trace_Image;


         

   

   ----------
   -- Next --
   ----------

   function Next
     (T       : Internal_Inner_Env_Assoc_Iterator_Access;
      Element : out Internal_Inner_Env_Assoc) return Boolean is
   begin
      if T = null then
         raise Property_Error with "null access dereference";
      end if;
      Check_Safety_Net (T.Safety_Net);

      if T.Index > T.Elements.Items'Last then
         return False;
      else
         Element := T.Elements.Items (T.Index);
         T.Index := T.Index + 1;
         return True;
      end if;
   end Next;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Inner_Env_Assoc_Iterator_Access) is
   begin
      if T /= null and then T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Inner_Env_Assoc_Iterator_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Dec_Ref (T.Elements);
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Inner_Env_Assoc_Iterator_Access) return String is
      begin
         return "<Iterator of InnerEnvAssoc, index="
                & A.Index'Image & ">";
      end Trace_Image;



   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Internal_Unit) return Boolean is
   begin
      return Left.Filename < Right.Filename;
   end "<";


   -------------------
   -- Solve_Wrapper --
   -------------------

   function Solve_Wrapper
     (R            : Relation;
      Context_Node : Bare_Turkixir_Node) return Boolean is
   begin
      if Context_Node /= null and then Langkit_Support.Adalog.Debug.Debug then
         Assign_Names_To_Logic_Vars (Context_Node);
      end if;

      begin
         return Solve (R, Context_Node.Unit.Context.Logic_Resolution_Timeout);
      exception
         when Langkit_Support.Adalog.Early_Binding_Error =>
            raise Property_Error with "invalid equation for logic resolution";
         when Langkit_Support.Adalog.Timeout_Error =>
            raise Property_Error with "logic resolution timed out";
      end;
   end Solve_Wrapper;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Env : in out Lexical_Env_Access) is
      Mutable_Env : Lexical_Env :=
        (Wrap (Env), 0, Env.Kind, No_Generic_Unit, 0);
   begin
      Destroy (Mutable_Env);
      Env := null;
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self              : Bare_Turkixir_Node;
      Kind              : Turkixir_Node_Kind_Type;
      Unit              : Internal_Unit;
      Token_Start_Index : Token_Index;
      Token_End_Index   : Token_Index;
      Parent            : Bare_Turkixir_Node := null;
      Self_Env          : Lexical_Env := AST_Envs.Empty_Env) is
   begin
      pragma Unreferenced (Kind);
      Self.Parent := Parent;
      Self.Unit := Unit;

      Self.Token_Start_Index := Token_Start_Index;
      Self.Token_End_Index := Token_End_Index;

      Self.Self_Env := Self_Env;
      Self.Last_Attempted_Child := -1;

      

   end Initialize;

   --------------------
   -- Use_Direct_Env --
   --------------------

   procedure Use_Direct_Env (State : in out PLE_Node_State; Env : Lexical_Env)
   is
   begin
      State.Current_Env := Env;
      State.Current_NED := null;
   end Use_Direct_Env;

   -------------------
   -- Use_Named_Env --
   -------------------

   procedure Use_Named_Env
     (State   : in out PLE_Node_State;
      Context : Internal_Context;
      Name    : Symbol_Type) is
   begin
      State.Current_NED := Get_Named_Env_Descriptor (Context, Name);
      State.Current_Env := State.Current_NED.Env_With_Precedence;
   end Use_Named_Env;

   ---------------------
   -- Set_Initial_Env --
   ---------------------

   procedure Set_Initial_Env
     (Self         : Bare_Turkixir_Node;
      State        : in out PLE_Node_State;
      Env          : Internal_DesignatedEnv;
      DSL_Location : String) is
   begin
      case Env.Kind is
         when None =>
            Use_Direct_Env (State, Empty_Env);

         when Current_Env =>
            null;

         when Named_Env =>
            Use_Named_Env (State, Self.Unit.Context, Env.Env_Name);

         when Direct_Env =>

            --  Sanitize this environment value: make sure it's a non-foreign
            --  and primary environment.

            if Env.Direct_Env.Kind /= Static_Primary then
               raise Property_Error with
                  "Cannot set an env that is not static-primary as the"
                  & " initial env";

            elsif Is_Foreign_Strict (Env.Direct_Env, Self) then
               raise Property_Error with
                  "unsound foreign environment in SetInitialEnv ("
                  & DSL_Location & ")";
            end if;
            Use_Direct_Env (State, Env.Direct_Env);
      end case;
   end Set_Initial_Env;

   ----------------
   -- Add_To_Env --
   ----------------

   procedure Add_To_Env
     (Self         : Bare_Turkixir_Node;
      State        : PLE_Node_State;
      Key          : Symbol_Type;
      Value        : Bare_Turkixir_Node;
      MD           : Internal_Metadata;
      Resolver     : Entity_Resolver;
      Dest_Env     : Internal_DesignatedEnv;
      DSL_Location : String)
   is
      Context    : constant Internal_Context := Self.Unit.Context;
      Root_Scope : Lexical_Env renames Context.Root_Scope;
      --  Shortcuts

      Actual_Dest_Env : Lexical_Env;
      Dest_NED        : Named_Env_Descriptor_Access;
      --  Description for the destination environment
   begin
      --  Skip the env addition if explicitly requested

      if Key = null
         or else Value = null
         or else (case Dest_Env.Kind is
                  when None        => True,
                  when Current_Env => False,
                  when Named_Env   => Dest_Env.Env_Name = null,
                  when Direct_Env  => Dest_Env.Direct_Env = Empty_Env)
      then
         return;
      end if;

      if Value.Unit /= Self.Unit then
         raise Property_Error with "Cannot add_to_env an AST node that comes"
                                   & " from another analysis unit";
      end if;

      

      --  Then determine the destination environment

      case Dest_Env.Kind is
         when None =>
            raise Program_Error with "unreachable code";

         when Current_Env =>
            --  Just use the current environment
            Dest_NED := State.Current_NED;
            Actual_Dest_Env := State.Current_Env;

         when Named_Env =>
            --  There is an environment name: just lookup the corresponding
            --  NED/env.
            Dest_NED := Get_Named_Env_Descriptor (Context, Dest_Env.Env_Name);
            Actual_Dest_Env := Dest_NED.Env_With_Precedence;

         when Direct_Env =>
            --  There is an explicit destination environment
            Dest_NED := null;
            Actual_Dest_Env := Dest_Env.Direct_Env;
      end case;

      --  Sanitize it

      if Actual_Dest_Env.Kind /= Static_Primary then
         raise Property_Error with
            "Cannot add elements to a lexical env that is not static-primary";

      elsif
         --  Since lexical envs need to sort the foreign nodes they contain,
         --  and that the total order on nodes is not defined for synthetic
         --  nodes, it is not possible to add a synthetic node to a foreign
         --  lexical environment.
         --
         --  This reasoning applies to environments that belong to foreign
         --  units, but also to the root environment.
         Is_Foreign (Actual_Dest_Env, Self) and then Is_Synthetic (Value)
      then
         raise Property_Error with
            "Cannot add a synthetic node to a lexical env from another"
            & " analysis unit";

      elsif
         --  Reject direct references to foreign destination environments.
         --
         --  This is an attempt at identifying uses of the unsound relocation
         --  mechanism (as opposed to named environments), so this applies to
         --  all foreign environments (root scope included).
         DSL_Location'Length > 0
         and then Dest_Env.Kind = Direct_Env
         and then Is_Foreign_Strict (Actual_Dest_Env, Self)
      then
         raise Property_Error with
            "unsound foreign environment in AddToEnv (" & DSL_Location & ")";
      end if;

      --  Now that everything is sanitized, we can proceed with the actual
      --  key/value pair addition. Note that this does nothing if
      --  Actual_Dest_Env ended up empty.
      Add (Actual_Dest_Env, Key, Value, MD, Resolver);

      --  If we're adding the element to an environment by env name, we must
      --  register this association in two places: in the target named env
      --  entry, and in Value's unit.
      if Dest_NED /= null then
         declare
            use NED_Assoc_Maps;

            FN    : Map renames Dest_NED.Foreign_Nodes;
            Dummy : Boolean;
            Cur   : Cursor;
         begin
            FN.Insert (Key      => Key,
                       New_Item => Internal_Map_Node_Vectors.Empty_Vector,
                       Position => Cur,
                       Inserted => Dummy);
            declare
               V : Internal_Map_Node_Vectors.Vector renames
                  FN.Reference (Cur);
            begin
               V.Append ((Value, MD, Resolver));
            end;
         end;
         Value.Unit.Exiled_Entries_In_NED.Append ((Dest_NED, Key, Value));

      --  Otherwise, if we're adding the element to an environment that belongs
      --  to a different unit, or to the root scope, then...
      elsif Is_Foreign_Not_Empty (Actual_Dest_Env, Self) then
         --  Add the Key/Value association to the list of entries contained in
         --  other units, so we can remove them when reparsing Value's unit.
         Value.Unit.Exiled_Entries.Append ((Actual_Dest_Env, Key, Value));

         if Actual_Dest_Env /= Root_Scope then
            --  Add Val to the list of foreign nodes that Actual_Dest_Env's
            --  unit contains, so that when that unit is reparsed, we can call
            --  Add_To_Env again on those nodes.
            Convert_Unit (Actual_Dest_Env.Owner).Foreign_Nodes.Append
              ((Value, Self.Unit));
         end if;
      end if;
   end Add_To_Env;

   -------------
   -- Ref_Env --
   -------------

   procedure Ref_Env
     (Self                : Bare_Turkixir_Node;
      Dest_Env            : Lexical_Env;
      Ref_Env_Nodes       : in out Bare_Turkixir_Node_Array_Access;
      Resolver            : Lexical_Env_Resolver;
      Kind                : Ref_Kind;
      Cats                : Ref_Categories;
      Shed_Rebindings     : Boolean) is
   begin
      for N of Ref_Env_Nodes.Items loop
         if N /= null then
            if N.Unit /= Self.Unit then
               raise Property_Error with
                  "attempt to add a referenced environment to a foreign unit";
            end if;
            Reference (Dest_Env, N, Resolver, Kind, Cats, Shed_Rebindings);
         end if;
      end loop;
      Dec_Ref (Ref_Env_Nodes);
   end Ref_Env;

   -------------
   -- Add_Env --
   -------------

   procedure Add_Env
     (Self              : Bare_Turkixir_Node;
      State             : in out PLE_Node_State;
      No_Parent         : Boolean;
      Transitive_Parent : Boolean;
      Names             : in out Symbol_Type_Array_Access)
   is
      Parent_From_Name : constant Boolean := State.Current_NED /= null;
      --  Does the parent environment comes from a named environment lookup?

      --  Determine the parent of this new environment:
      --
      --  (1) no parent if requested;
      --  (2) the current environment as the static parent if it comes from a
      --      named env lookup or if it is not foreign (or is the empty/root
      --      environment).
      Parent : constant Lexical_Env :=
        (if No_Parent
         then Null_Lexical_Env
         else State.Current_Env);
   begin
      --  Create the environment itself
      Self.Self_Env := Create_Static_Lexical_Env
        (Parent            => Parent,
         Node              => Self,
         Transitive_Parent => Transitive_Parent);

      --  If the parent of this new environment comes from a named environment
      --  lookup, register this new environment so that its parent is updated
      --  when the precence for this named environment changes.
      if Parent_From_Name then
         declare
            NED : constant Named_Env_Descriptor_Access := State.Current_NED;
         begin
            Self.Unit.Exiled_Envs.Append ((NED, Self.Self_Env));
            NED.Foreign_Envs.Insert (Self, Self.Self_Env);
         end;
      end if;

      --  From now on, the current environment is Self.Self_Env, with a direct
      --  access to it. It does not go through the env naming scheme, since
      --  only this node and its children (i.e. non-foreign nodes) will access
      --  it as a "current" environment during PLE.
      Use_Direct_Env (State, Self.Self_Env);

      --  Register the environment we just created on all the requested names
      if Names /= null then
         declare
            Context   : constant Internal_Context := Self.Unit.Context;
            Env       : constant Lexical_Env := Self.Self_Env;
            NENU      : NED_Maps.Map renames
               State.Unit_State.Named_Envs_Needing_Update;
         begin
            for N of Names.Items loop
               Register_Named_Env (Context, N, Env, NENU);
            end loop;
            Dec_Ref (Names);
         end;
      end if;
   end Add_Env;

   ---------------------
   -- Pre_Env_Actions --
   ---------------------

   procedure Pre_Env_Actions
     (Self            : Bare_Turkixir_Node;
      State           : in out PLE_Node_State;
      Add_To_Env_Only : Boolean := False) is
   begin

      
   

   case Self.Kind is
            when Turkixir_Class_Def .. Turkixir_Func_Def =>
            
            Def_Stmt_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Turkixir_Assign_Stmt =>
            
            Assign_Stmt_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Turkixir_Single_Param =>
            
            Single_Param_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
      when others =>  null; 
   end case;


   end Pre_Env_Actions;

   ----------------------
   -- Post_Env_Actions --
   ----------------------

   pragma Warnings (Off, "referenced");
   procedure Post_Env_Actions
     (Self : Bare_Turkixir_Node; State : in out PLE_Node_State)
   is
      pragma Warnings (On, "referenced");
   begin
      
   

   case Self.Kind is
            when Turkixir_Class_Def .. Turkixir_Func_Def =>
            
            null;
      
            when Turkixir_Assign_Stmt =>
            
            null;
      
            when Turkixir_Single_Param =>
            
            null;
      
      when others =>  null; 
   end case;

   end Post_Env_Actions;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Node : Bare_Turkixir_Node) return Symbol_Type is
   begin
      if Node = null then
         raise Property_Error with "cannot get the symbol of a null node";
      end if;
      return Get_Symbol (Token (Node, Node.Token_Start_Index));
   end Get_Symbol;

   ----------
   -- Text --
   ----------

   function Text
     (Node : Bare_Turkixir_Node) return Text_Type
   is
   begin
      if Node = null then
         raise Property_Error with "cannot get the text of a null node";
      end if;

      declare
         Start_T : constant Token_Reference :=
            Token (Node, Node.Token_Start_Index);
         End_T   : constant Token_Reference :=
            Token (Node, Node.Token_End_Index);
      begin
         --  No text is associated to synthetic and ghost nodes

         if Is_Synthetic (Node) then
            return "";
         end if;

         if Is_Ghost (Node) then
            return "";
         end if;

         return Text (Start_T, End_T);
      end;
   end Text;

   ---------------------
   -- Is_Visible_From --
   ---------------------

   function Is_Visible_From
     (Referenced_Env, Base_Env : Lexical_Env) return Boolean
   is
      Referenced_Unit : constant Internal_Unit :=
         Convert_Unit (Referenced_Env.Owner);
      Base_Unit       : constant Internal_Unit :=
         Convert_Unit (Base_Env.Owner);
   begin
      if Referenced_Unit = null then
         raise Property_Error with
            "referenced environment does not belong to any analysis unit";
      elsif Base_Unit = null then
         raise Property_Error with
            "base environment does not belong to any analysis unit";
      end if;
      return Is_Referenced_From (Referenced_Unit, Base_Unit);
   end Is_Visible_From;

   ----------
   -- Unit --
   ----------

   function Unit (Node : Bare_Turkixir_Node) return Internal_Unit is
   begin
      return Node.Unit;
   end Unit;

   function Lookup_Internal
     (Node : Bare_Turkixir_Node;
      Sloc : Source_Location) return Bare_Turkixir_Node;
   procedure Lookup_Relative
     (Node       : Bare_Turkixir_Node;
      Sloc       : Source_Location;
      Position   : out Relative_Position;
      Node_Found : out Bare_Turkixir_Node);
   --  Implementation helpers for the looking up process

   -----------------
   -- Set_Parents --
   -----------------

   procedure Set_Parents
     (Node, Parent : Bare_Turkixir_Node)
   is
   begin
      if Node = null then
         return;
      end if;

      Node.Parent := Bare_Turkixir_Node (Parent);

      for I in 1 .. Children_Count (Node) loop
         Set_Parents (Child (Node, I), Node);
      end loop;
   end Set_Parents;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Node : Bare_Turkixir_Node) is
   begin
      if Node = null then
         return;
      end if;

      Free_User_Fields (Node);
      for I in 1 .. Children_Count (Node) loop
         Destroy (Child (Node, I));
      end loop;
   end Destroy;

   -----------
   -- Child --
   -----------

   function Child (Node  : Bare_Turkixir_Node;
                   Index : Positive) return Bare_Turkixir_Node
   is
      Result          : Bare_Turkixir_Node;
      Index_In_Bounds : Boolean;
   begin
      Get_Child (Node, Index, Index_In_Bounds, Result);
      return Result;
   end Child;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Node  : Bare_Turkixir_Node;
      Visit : access function (Node : Bare_Turkixir_Node)
              return Visit_Status)
     return Visit_Status
   is
      Status : Visit_Status := Into;

   begin
      if Node /= null then
         Status := Visit (Node);

         --  Skip processing the child nodes if the returned status is Over
         --  or Stop. In the former case the previous call to Visit has taken
         --  care of processing the needed childs, and in the latter case we
         --  must immediately stop processing the tree.

         if Status = Into then
            for I in 1 .. Children_Count (Node) loop
               declare
                  Cur_Child : constant Bare_Turkixir_Node :=
                     Child (Node, I);

               begin
                  if Cur_Child /= null then
                     Status := Traverse (Cur_Child, Visit);
                     exit when Status /= Into;
                  end if;
               end;
            end loop;
         end if;
      end if;

      if Status = Stop then
         return Stop;

      --  At this stage the Over status has no sense and we just continue
      --  processing the tree.

      else
         return Into;
      end if;
   end Traverse;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (Node  : Bare_Turkixir_Node;
      Visit : access function (Node : Bare_Turkixir_Node)
                               return Visit_Status)
   is
      Result_Status : Visit_Status;
      pragma Unreferenced (Result_Status);
   begin
      Result_Status := Traverse (Node, Visit);
   end Traverse;

   ------------------------
   -- Traverse_With_Data --
   ------------------------

   function Traverse_With_Data
     (Node  : Bare_Turkixir_Node;
      Visit : access function (Node : Bare_Turkixir_Node;
                               Data : in out Data_Type)
                               return Visit_Status;
      Data  : in out Data_Type)
      return Visit_Status
   is
      function Helper (Node : Bare_Turkixir_Node) return Visit_Status;

      ------------
      -- Helper --
      ------------

      function Helper (Node : Bare_Turkixir_Node) return Visit_Status is
      begin
         return Visit (Node, Data);
      end Helper;

      Saved_Data : Data_Type;
      Result     : Visit_Status;

   begin
      if Reset_After_Traversal then
         Saved_Data := Data;
      end if;
      Result := Traverse (Node, Helper'Access);
      if Reset_After_Traversal then
         Data := Saved_Data;
      end if;
      return Result;
   end Traverse_With_Data;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Node : Bare_Turkixir_Node) return Source_Location_Range
   is
      type Token_Anchor is (T_Start, T_End);
      type Token_Pos is record
         Pos    : Token_Index;
         Anchor : Token_Anchor;
      end record;

      TDH                    : Token_Data_Handler renames Node.Unit.TDH;
      Token_Start, Token_End : Token_Pos;

      function Get (Index : Token_Index) return Stored_Token_Data is
        (Get_Token (TDH, Index));

      function Sloc (T : Token_Pos) return Source_Location is
        (if T.Anchor = T_Start
         then Sloc_Start (TDH, Get (T.Pos))
         else Sloc_End (TDH, Get (T.Pos)));

   begin
      if Is_Synthetic (Node) then
         return Sloc_Range (Node.Parent);
      end if;

      if Is_Ghost (Node) then
         Token_Start := (if Node.Token_Start_Index = 1
                         then (1, T_Start)
                         else (Node.Token_Start_Index - 1, T_End));
         Token_End := Token_Start;
      else
         Token_Start := (Node.Token_Start_Index, T_Start);
         Token_End := (Node.Token_End_Index, T_End);
      end if;

      if Snaps_At_Start (Node)
         and then not Is_Ghost (Node)
         and then Token_Start.Pos /= 1
      then
         Token_Start := (Token_Start.Pos - 1, T_End);
      end if;

      if Snaps_At_End (Node) and then Token_End.Pos /= Last_Token (TDH) then
         Token_End := (Token_End.Pos + 1, T_Start);
      end if;

      return Make_Range (Sloc (Token_Start), Sloc (Token_End));
   end Sloc_Range;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Node : Bare_Turkixir_Node;
      Sloc : Source_Location) return Bare_Turkixir_Node
   is
      Position : Relative_Position;
      Result   : Bare_Turkixir_Node;
   begin
      if Sloc = No_Source_Location then
         return null;
      end if;

      Lookup_Relative
        (Bare_Turkixir_Node (Node), Sloc, Position, Result);
      return Result;
   end Lookup;

   ---------------------
   -- Lookup_Internal --
   ---------------------

   function Lookup_Internal
     (Node : Bare_Turkixir_Node;
      Sloc : Source_Location) return Bare_Turkixir_Node
   is
      --  For this implementation helper (i.e. internal primitive), we can
      --  assume that all lookups fall into this node's sloc range.
      pragma Assert (Compare (Sloc_Range (Node), Sloc) = Inside);

      Children : constant Internal_Bare_Turkixir_Node_Array :=
         Implementation.Children (Node);
      Pos      : Relative_Position;
      Result   : Bare_Turkixir_Node;
   begin
      --  Look for a child node that contains Sloc (i.e. return the most
      --  precise result).

      for Child of Children loop
         --  Note that we assume here that child nodes are ordered so that the
         --  first one has a sloc range that is before the sloc range of the
         --  second child node, etc.

         if Child /= null then
            Lookup_Relative (Child, Sloc, Pos, Result);
            case Pos is
               when Before =>
                   --  If this is the first node, Sloc is before it, so we can
                   --  stop here.  Otherwise, Sloc is between the previous
                   --  child node and the next one...  so we can stop here,
                   --  too.
                   return Node;

               when Inside =>
                   return Result;

               when After =>
                   --  Sloc is after the current child node, so see with the
                   --  next one.
                   null;
            end case;
         end if;
      end loop;

      --  If we reach this point, we found no children that covers Sloc, but
      --  Node still covers it (see the assertion).
      return Node;
   end Lookup_Internal;

   -------------
   -- Compare --
   -------------

   function Compare
     (Node : Bare_Turkixir_Node;
      Sloc : Source_Location) return Relative_Position is
   begin
      return Compare (Sloc_Range (Node), Sloc);
   end Compare;

   ---------------------
   -- Lookup_Relative --
   ---------------------

   procedure Lookup_Relative
     (Node       : Bare_Turkixir_Node;
      Sloc       : Source_Location;
      Position   : out Relative_Position;
      Node_Found : out Bare_Turkixir_Node)
   is
      Result : constant Relative_Position :=
        Compare (Node, Sloc);
   begin
      Position := Result;
      Node_Found := (if Result = Inside
                     then Lookup_Internal (Node, Sloc)
                     else null);
   end Lookup_Relative;

   -------------
   -- Compare --
   -------------

   function Compare
     (Left, Right : Bare_Turkixir_Node;
      Relation    : Comparison_Relation) return Boolean
   is
      LS, RS : Source_Location;
   begin
      if Left = null or else Right = null or else Left.Unit /= Right.Unit then
         raise Property_Error with "invalid node comparison";
      end if;

      LS := Start_Sloc (Sloc_Range (Left));
      RS := Start_Sloc (Sloc_Range (Right));
      return (case Relation is
              when Langkit_Support.Types.Less_Than        => LS < RS,
              when Langkit_Support.Types.Less_Or_Equal    => LS <= RS,
              when Langkit_Support.Types.Greater_Than     => LS > RS,
              when Langkit_Support.Types.Greater_Or_Equal => LS >= RS);
   end Compare;

   --------------
   -- Children --
   --------------

   function Children
     (Node : Bare_Turkixir_Node) return Internal_Bare_Turkixir_Node_Array
   is
      First : constant Integer := Bare_Turkixir_Node_Vectors.Index_Type'First;
      Last  : constant Integer := First + Children_Count (Node) - 1;
   begin
      return A : Internal_Bare_Turkixir_Node_Array (First .. Last)
      do
         for I in First .. Last loop
            A (I) := Child (Node, I);
         end loop;
      end return;
   end Children;

   function Children
     (Node : Bare_Turkixir_Node) return Bare_Turkixir_Node_Array_Access
   is
      C : Internal_Bare_Turkixir_Node_Array := Children (Node);
   begin
      return Ret : Bare_Turkixir_Node_Array_Access :=
         Create_Bare_Turkixir_Node_Array (C'Length)
      do
         Ret.Items := C;
      end return;
   end Children;

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Turkixir_Node_Base_List;
      Index   : Integer;
      Or_Null : Boolean := False) return Bare_Turkixir_Node
   is
      function Length (Node : Bare_Turkixir_Node_Base_List) return Natural
      is (Node.Count);
      --  Wrapper around the Length primitive to get the compiler happy for the
      --  the package instantiation below.

      function Absolute_Get
        (L     : Bare_Turkixir_Node_Base_List;
         Index : Integer) return Bare_Turkixir_Node
      is (L.Nodes.all (Index + 1));
      --  L.Nodes is 1-based but Index is 0-based

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => Bare_Turkixir_Node,
         Sequence_Type => Bare_Turkixir_Node_Base_List,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Bare_Turkixir_Node;
   begin
      if Node = null and then Or_Null then
         return null;
      elsif Relative_Get (Node, Index, Result) then
         return Result;
      elsif Or_Null then
         return null;
      else
         raise Property_Error with "out-of-bounds AST list access";
      end if;
   end Get;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia
     (Node        : Bare_Turkixir_Node;
      Line_Prefix : String := "")
   is
      Children_Prefix : constant String := Line_Prefix & "|  ";
   begin
      Put_Line (Line_Prefix & Kind_Name (Node));
      for C of Children_And_Trivia (Node) loop
         case C.Kind is
            when Trivia =>
               Put_Line (Children_Prefix & Image (Text (C.Trivia)));
            when Child =>
               PP_Trivia (C.Node, Children_Prefix);
         end case;
      end loop;
   end PP_Trivia;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   function Populate_Lexical_Env (Node : Bare_Turkixir_Node) return Boolean is

      Context    : constant Internal_Context := Node.Unit.Context;
      Unit_State : aliased PLE_Unit_State := (Named_Envs_Needing_Update => <>);
      Root_State : constant PLE_Node_State :=
        (Unit_State  => Unit_State'Unchecked_Access,
         Current_Env => Context.Root_Scope,
         Current_NED => null);

      function Populate_Internal
        (Node         : Bare_Turkixir_Node;
         Parent_State : PLE_Node_State) return Boolean;
      --  Do the lexical env population on Node and recurse on its children

      procedure Register_Foreign_Env
        (Node : Bare_Turkixir_Node; State : PLE_Node_State);
      --  Given a node and its PLE state, register Node.Self_Env as being
      --  initialized through the named environment mechanism, if that's indeed
      --  the case. Do nothing otherwise.

      -----------------------
      -- Populate_Internal --
      -----------------------

      function Populate_Internal
        (Node         : Bare_Turkixir_Node;
         Parent_State : PLE_Node_State) return Boolean
      is
         Result : Boolean := False;
         State  : PLE_Node_State := Parent_State;
      begin
         if Node = null then
            return Result;
         end if;

         --  By default (i.e. unless env actions add a new env), the
         --  environment we store in Node is the current one.
         Node.Self_Env := State.Current_Env;

         --  Run pre/post actions, and run PLE on children in between. Make
         --  sure we register the potential foreign Node.Self_Env environment
         --  at the end, even when an exception interrupts PLE to keep the
         --  state consistent.
         begin
            Pre_Env_Actions (Node, State);
            if State.Current_Env /= Null_Lexical_Env then
               Node.Self_Env := State.Current_Env;
               Register_Foreign_Env (Node, State);
            end if;

            --  Call recursively on children
            for I in First_Child_Index (Node) .. Last_Child_Index (Node) loop
               Result := Populate_Internal
                 (Child (Node, I), State) or else Result;
            end loop;

            Post_Env_Actions (Node, State);
         exception
            when Exc : Property_Error =>
               if PLE_Errors_Trace.Is_Active then
                   GNATCOLL.Traces.Trace
                     (PLE_Errors_Trace,
                      "Exception raised during PLE "
                      & Ada.Exceptions.Exception_Name (Exc) & " : "
                      & Ada.Exceptions.Exception_Message (Exc));
                   GNATCOLL.Traces.Trace
                     (PLE_Errors_Trace,
                      GNAT.Traceback.Symbolic.Symbolic_Traceback (Exc));
               end if;
               Register_Foreign_Env (Node, State);
               return True;
         end;

         return Result;
      end Populate_Internal;

      --------------------------
      -- Register_Foreign_Env --
      --------------------------

      procedure Register_Foreign_Env
        (Node : Bare_Turkixir_Node; State : PLE_Node_State) is
      begin
         if State.Current_NED /= null then
            State.Current_NED.Nodes_With_Foreign_Env.Insert (Node);
            Node.Unit.Nodes_With_Foreign_Env.Insert (Node, State.Current_NED);
         end if;
      end Register_Foreign_Env;

   begin
      --  This function is meant to be called during an existing PLE pass. If
      --  if is called outside of this context, run the PLE pass on Node's
      --  analysis unit. Likewise, if PLE has not run on the unit that owns
      --  this PLE unit yet, do a full run, which will in the end trigger the
      --  PLE on this PLE unit.
      --
      --  We do this so that as soon as PLE is required on a PLE unit: the
      --  whole unit end up with its lexical environments populated.
      if not Context.In_Populate_Lexical_Env then
         begin
            Populate_Lexical_Env (Node.Unit);
            return False;
         exception
            when Property_Error =>
               return True;
         end;
      end if;

      --  This is intended to be called on the root node only
      if Node.Parent /= null then
         raise Program_Error;
      end if;

      return Result : constant Boolean :=
         Populate_Internal (Node, Root_State)
      do
         Update_Named_Envs (Unit_State.Named_Envs_Needing_Update);
      end return;
   end Populate_Lexical_Env;

   ------------------------------
   -- AST_Envs_Node_Text_Image --
   ------------------------------

   function AST_Envs_Node_Text_Image
     (Node  : Bare_Turkixir_Node;
      Short : Boolean := True) return Text_Type is
   begin
      if Short then
         if Node = null then
            return "null";
         end if;
         return To_Text (Basename (Node.Unit))
           & ":" & To_Text (Image (Start_Sloc (Sloc_Range (Node))));
      else
         return Short_Text_Image (Node);
      end if;
   end AST_Envs_Node_Text_Image;

   -------------------
   -- Is_Rebindable --
   -------------------

   function Is_Rebindable (Node : Bare_Turkixir_Node) return Boolean is
   begin
      
         pragma Unreferenced (Node);
         return True;
   end Is_Rebindable;

   -----------------------
   -- Acquire_Rebinding --
   -----------------------

   function Acquire_Rebinding
     (Node             : Bare_Turkixir_Node;
      Parent           : Env_Rebindings;
      Old_Env, New_Env : Lexical_Env) return Env_Rebindings
   is
      Result    : Env_Rebindings;
      Available : Env_Rebindings_Vectors.Vector renames
         Node.Unit.Context.Available_Rebindings;
   begin
      --  Use an existing and available Env_Rebindings_Type record for Node's
      --  Context, otherwise allocate a new rebinding.
      Result := (if Available.Is_Empty
                 then new Env_Rebindings_Type'(Version => 0, others => <>)
                 else Available.Pop);

      Result.Parent := Parent;
      Result.Old_Env := Old_Env;
      Result.New_Env := New_Env;
      Result.Children := Env_Rebindings_Vectors.Empty_Vector;
      return Result;
   end Acquire_Rebinding;

   -----------------------
   -- Release_Rebinding --
   -----------------------

   procedure Release_Rebinding (Self : in out Env_Rebindings) is
      Available : Env_Rebindings_Vectors.Vector renames
         Unwrap (Self.Old_Env).Node.Unit.Context.Available_Rebindings;
   begin
      --  Bumping the version number, to invalidate existing references to
      --  Self.
      Self.Version := Self.Version + 1;

      Self.Children.Destroy;
      Available.Append (Self);
      Self := null;
   end Release_Rebinding;

   ------------------------
   -- Register_Rebinding --
   ------------------------

   procedure Register_Rebinding
     (Node : Bare_Turkixir_Node; Rebinding : Env_Rebindings) is
   begin
      Node.Unit.Rebindings.Append (Rebinding);
   end Register_Rebinding;

   --------------------
   -- Element_Parent --
   --------------------

   function Element_Parent
     (Node : Bare_Turkixir_Node) return Bare_Turkixir_Node
   is (Node.Parent);

   ---------------
   -- Node_Unit --
   ---------------

   function Node_Unit (Node : Bare_Turkixir_Node) return Generic_Unit_Ptr is
   begin
      return Convert_Unit (Node.Unit);
   end Node_Unit;

   ----------
   -- Hash --
   ----------

   function Hash (Node : Bare_Turkixir_Node) return Hash_Type
   is
      function H is new Hash_Access
        (Root_Node_Record, Bare_Turkixir_Node);
   begin
      return H (Node);
   end Hash;

      function Hash (B : Boolean) return Hash_Type is (Boolean'Pos (B));




   ------------------------
   -- Named environments --
   ------------------------

   ---------
   -- Add --
   ---------

   procedure Add
     (Self : in out NED_Assoc_Maps.Map;
      Key  : Symbol_Type;
      Node : AST_Envs.Internal_Map_Node)
   is
      use NED_Assoc_Maps;

      Pos   : Cursor;
      Dummy : Boolean;
   begin
      --  Make sure there is a vector entry for Key
      Self.Insert (Key, Internal_Map_Node_Vectors.Empty_Vector, Pos, Dummy);

      --  Append Node to that vector
      declare
         V : Internal_Map_Node_Vectors.Vector renames Self.Reference (Pos);
      begin
         V.Append (Node);
      end;
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self : in out NED_Assoc_Maps.Map;
      Key  : Symbol_Type;
      Node : Bare_Turkixir_Node)
   is
      use NED_Assoc_Maps;

      V : Internal_Map_Node_Vectors.Vector renames Self.Reference (Key);
   begin
      --  Remove the (assumed unique) entry in V whose node is Node. The order
      --  of items in V is not significant, so we can use Pop for efficient
      --  removal. Do the traversal in reverse order for correctness.
      for I in reverse 1 .. V.Length loop
         if V.Get_Access (I).Node = Node then
            V.Pop (I);
            exit;
         end if;
      end loop;
   end Remove;

   ------------------------------
   -- Get_Named_Env_Descriptor --
   ------------------------------

   function Get_Named_Env_Descriptor
     (Context : Internal_Context;
      Name    : Symbol_Type) return Named_Env_Descriptor_Access
   is
      use NED_Maps;

      --  Look for an existing entry for Name
      Pos : constant Cursor := Context.Named_Envs.Find (Name);
   begin
      if Has_Element (Pos) then
         return Element (Pos);
      end if;

      --  There is no such entry: create one
      return Result : constant Named_Env_Descriptor_Access :=
         new Named_Env_Descriptor'
           (Name                   => Name,
            Envs                   => <>,
            Env_With_Precedence    => Empty_Env,
            Foreign_Nodes          => <>,
            Foreign_Envs           => <>,
            Nodes_With_Foreign_Env => <>)
      do
         Context.Named_Envs.Insert (Name, Result);
      end return;
   end Get_Named_Env_Descriptor;

   ------------------------
   -- Register_Named_Env --
   ------------------------

   procedure Register_Named_Env
     (Context                   : Internal_Context;
      Name                      : Symbol_Type;
      Env                       : Lexical_Env;
      Named_Envs_Needing_Update : in out NED_Maps.Map)
   is
      NED_Access : constant Named_Env_Descriptor_Access :=
         Get_Named_Env_Descriptor (Context, Name);
      NED        : Named_Env_Descriptor renames NED_Access.all;
      Node       : constant Bare_Turkixir_Node := Env_Node (Env);
   begin
      NED.Envs.Insert (Node, Env);
      Node.Unit.Named_Envs.Append ((Name, Env));

      --  If that insertion must change the env that has precedence, signal
      --  that NED requires an update.

      if NED.Envs.First_Element /= NED.Env_With_Precedence then
         Named_Envs_Needing_Update.Include (Name, NED_Access);
      end if;
   end Register_Named_Env;

   ----------------------
   -- Update_Named_Env --
   ----------------------

   procedure Update_Named_Envs (Named_Envs : NED_Maps.Map) is
   begin
      for Cur in Named_Envs.Iterate loop
         declare
            NE      : Named_Env_Descriptor renames NED_Maps.Element (Cur).all;
            New_Env : constant Lexical_Env :=
              (if NE.Envs.Is_Empty
               then Empty_Env
               else NE.Envs.First_Element);
         begin
            --  If there was an environment with precedence, remove its foreign
            --  nodes.
            if NE.Env_With_Precedence /= Empty_Env then
               for Cur in NE.Foreign_Nodes.Iterate loop
                  declare
                     Key   : constant Symbol_Type :=
                        NED_Assoc_Maps.Key (Cur);
                     Nodes : Internal_Map_Node_Vectors.Vector renames
                        NE.Foreign_Nodes.Reference (Cur);
                  begin
                     for N of Nodes loop
                        Remove (NE.Env_With_Precedence, Key, N.Node);
                     end loop;
                  end;
               end loop;
            end if;

            --  Now, set the new environment that has precedence
            NE.Env_With_Precedence := New_Env;

            --  Add the foreign nodes to the new environment with precedence,
            --  if any.
            for Cur in NE.Foreign_Nodes.Iterate loop
               declare
                  Key   : constant Symbol_Type :=
                     NED_Assoc_Maps.Key (Cur);
                  Nodes : Internal_Map_Node_Vectors.Vector renames
                     NE.Foreign_Nodes.Reference (Cur);
               begin
                  for N of Nodes loop
                     Add (New_Env, Key, N.Node, N.MD, N.Resolver);
                  end loop;
               end;
            end loop;

            --  Set the parent environment of all foreign environments
            for Cur in NE.Foreign_Envs.Iterate loop
               declare
                  Env : Lexical_Env_Record renames
                     Unwrap (Sorted_Env_Maps.Element (Cur)).all;
               begin
                  Env.Parent := New_Env;
               end;
            end loop;

            --  Update nodes whose environment was the old env with precedence
            for N of NE.Nodes_With_Foreign_Env loop
               N.Self_Env := New_Env;
            end loop;
         end;
      end loop;
   end Update_Named_Envs;

   --------------------------
   -- Big integers wrapper --
   --------------------------

   ------------------------
   -- Create_Big_Integer --
   ------------------------

   function Create_Big_Integer
     (Image : String; Base : Integer := 10) return Big_Integer_Type
   is
      use GNATCOLL.GMP;
      use GNATCOLL.GMP.Integers;
   begin
      return new Big_Integer_Record'(Value     => Make (Image, Int (Base)),
                                     Ref_Count => 1);
   end Create_Big_Integer;

   ------------------------
   -- Create_Big_Integer --
   ------------------------

   function Create_Big_Integer
     (Big_Int : GNATCOLL.GMP.Integers.Big_Integer) return Big_Integer_Type
   is
      Result : constant Big_Integer_Type :=
         new Big_Integer_Record'(Value     => <>,
                                 Ref_Count => 1);
   begin
      Result.Value.Set (Big_Int);
      return Result;
   end Create_Big_Integer;

   ------------------------
   -- Create_Big_Integer --
   ------------------------

   function Create_Big_Integer (Int : Integer) return Big_Integer_Type is
      Result : constant Big_Integer_Type :=
         new Big_Integer_Record'(Value     => <>,
                                 Ref_Count => 1);
   begin
      Result.Value.Set (GNATCOLL.GMP.Long (Int));
      return Result;
   end Create_Big_Integer;

   -------------------------------
   -- Create_Public_Big_Integer --
   -------------------------------

   function Create_Public_Big_Integer
     (Big_Int : Big_Integer_Type) return GNATCOLL.GMP.Integers.Big_Integer is
   begin
      return Result : GNATCOLL.GMP.Integers.Big_Integer do
         Result.Set (Big_Int.Value);
      end return;
   end Create_Public_Big_Integer;

   -----------------
   -- Trace_Image --
   -----------------

   function Trace_Image (I : Big_Integer_Type) return String is
   begin
      return GNATCOLL.GMP.Integers.Image (I.Value);
   end Trace_Image;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Big_Int : Big_Integer_Type) return Integer is
      Image : constant String := Big_Int.Value.Image;
   begin
      return Integer'Value (Image);
   exception
      when Constraint_Error =>
         raise Property_Error with "out of range big integer";
   end To_Integer;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Big_Int : Big_Integer_Type) is
   begin
      if Big_Int.Ref_Count /= -1 then
         Big_Int.Ref_Count := Big_Int.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Big_Int : in out Big_Integer_Type) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Big_Integer_Record, Big_Integer_Type);
   begin
      if Big_Int = null or else Big_Int.Ref_Count = -1 then
         return;
      end if;

      Big_Int.Ref_Count := Big_Int.Ref_Count - 1;
      if Big_Int.Ref_Count = 0 then
         Destroy (Big_Int);
      end if;
   end Dec_Ref;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (Left, Right : Big_Integer_Type) return Boolean is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Left.Value = Right.Value;
   end Equivalent;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Big_Integer_Type) return Boolean is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Left.Value < Right.Value;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Big_Integer_Type) return Boolean is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Left.Value <= Right.Value;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Big_Integer_Type) return Boolean is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Left.Value > Right.Value;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Big_Integer_Type) return Boolean is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Left.Value >= Right.Value;
   end ">=";

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Big_Integer_Type) return Big_Integer_Type is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Create_Big_Integer (Left.Value + Right.Value);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Big_Integer_Type) return Big_Integer_Type is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Create_Big_Integer (Left.Value - Right.Value);
   end "-";

   ------------------
   -- Unit_Version --
   ------------------

   function Unit_Version (Unit : Generic_Unit_Ptr) return Version_Number is
   begin
      return Convert_Unit (Unit).Unit_Version;
   end Unit_Version;

   -------------------------
   -- Get_Context_Version --
   -------------------------

   function Get_Context_Version
     (Node : Bare_Turkixir_Node) return Version_Number is
   begin
      return Node.Unit.Context.Cache_Version;
   end Get_Context_Version;

   ----------------------
   -- Short_Text_Image --
   ----------------------

   function Short_Text_Image (Self : Bare_Turkixir_Node) return Text_Type
   is
   begin
      if Self = null then
         return "None";
      end if;

      
   

   case Self.Kind is
      when others => 
         return "<" & To_Text (Kind_Name (Self))
                & " "
                & To_Text
                  (Ada.Directories.Simple_Name
                     (Get_Filename (Unit (Self))))
                & ":" & To_Text (Image (Sloc_Range (Self))) & ">";
      
   end case;

   end Short_Text_Image;

   --------------------
   -- Snaps_At_Start --
   --------------------

   function Snaps_At_Start (Self : Bare_Turkixir_Node) return Boolean is
   begin
      
   

   case Self.Kind is
      when others => 
         return False;
      
   end case;

   end Snaps_At_Start;

   ------------------
   -- Snaps_At_End --
   ------------------

   function Snaps_At_End (Self : Bare_Turkixir_Node) return Boolean is
   begin
      
   

   case Self.Kind is
      when others => 
         return Is_Incomplete (Self);
      
   end case;

   end Snaps_At_End;

   -------------
   -- Parents --
   -------------

   function Parents
     (Node      : Bare_Turkixir_Node;
      With_Self : Boolean := True)
      return Bare_Turkixir_Node_Array_Access
   is
      Count : Natural := 0;
      Start : Bare_Turkixir_Node :=
        (if With_Self then Node else Node.Parent);
      Cur   : Bare_Turkixir_Node := Start;
   begin
      while Cur /= null loop
         Count := Count + 1;
         Cur := Cur.Parent;
      end loop;

      declare
         Result : constant Bare_Turkixir_Node_Array_Access :=
            Create_Bare_Turkixir_Node_Array (Count);
      begin
         Cur := Start;
         for I in Result.Items'Range loop
            Result.Items (I) := Cur;
            Cur := Cur.Parent;
         end loop;
         return Result;
      end;
   end Parents;

   -----------------------
   -- First_Child_Index --
   -----------------------

   function First_Child_Index (Node : Bare_Turkixir_Node) return Natural
   is (1);

   ----------------------
   -- Last_Child_Index --
   ----------------------

   function Last_Child_Index (Node : Bare_Turkixir_Node) return Natural
   is (Children_Count (Node));

   ---------------
   -- Get_Child --
   ---------------

   procedure Get_Child
     (Node            : Bare_Turkixir_Node;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Bare_Turkixir_Node)
   is
      K : constant Turkixir_Node_Kind_Type := Node.Kind;
   begin
      

      Index_In_Bounds := True;
      Result := null;
      case Turkixir_Turkixir_Node (K) is
when Turkixir_Arg_Assoc_Range =>
declare
N_Bare_Arg_Assoc : constant Bare_Arg_Assoc := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Arg_Assoc.Arg_Assoc_F_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Arg_Assoc.Arg_Assoc_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Arg_Gen_Range =>
declare
N_Bare_Arg_Gen : constant Bare_Arg_Gen := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Arg_Gen.Arg_Gen_F_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Arg_Gen.Arg_Gen_F_Comprehension;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Kw_Args_Range =>
declare
N_Bare_Kw_Args : constant Bare_Kw_Args := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Kw_Args.Kw_Args_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Var_Args_Range =>
declare
N_Bare_Var_Args : constant Bare_Var_Args := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Var_Args.Var_Args_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_As_Name_Node_Range =>
declare
N_Bare_As_Name_Node : constant Bare_As_Name_Node := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_As_Name_Node.As_Name_Node_F_Imported;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_As_Name_Node.As_Name_Node_F_As_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Comp_If_Range =>
declare
N_Bare_Comp_If : constant Bare_Comp_If := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Comp_If.Comp_If_F_Test;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Comp_If.Comp_If_F_Comp;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Comp_For_Range =>
declare
N_Bare_Comp_For : constant Bare_Comp_For := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Comp_For.Comp_For_F_Exprs;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Comp_For.Comp_For_F_Target;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Comp_For.Comp_For_F_Comp;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Comp_ForL_Range =>
declare
N_Bare_Comp_ForL : constant Bare_Comp_ForL := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Comp_ForL.Comp_ForL_F_Exprs;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Comp_ForL.Comp_ForL_F_Target;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Comp_ForL.Comp_ForL_F_Comp;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Decorator_Range =>
declare
N_Bare_Decorator : constant Bare_Decorator := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Decorator.Decorator_F_Dec_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Decorator.Decorator_F_Arg_List;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Dict_Assoc_Range =>
declare
N_Bare_Dict_Assoc : constant Bare_Dict_Assoc := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Dict_Assoc.Dict_Assoc_F_Key;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Dict_Assoc.Dict_Assoc_F_Value;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Else_Part_Range =>
declare
N_Bare_Else_Part : constant Bare_Else_Part := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Else_Part.Else_Part_F_Statements;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Except_Part_Range =>
declare
N_Bare_Except_Part : constant Bare_Except_Part := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Except_Part.Except_Part_F_As_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Except_Part.Except_Part_F_Statements;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_And_Expr_Range =>
declare
N_Bare_And_Expr : constant Bare_And_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_And_Expr.And_Expr_F_Left;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_And_Expr.And_Expr_F_Right;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_And_Op_Range =>
declare
N_Bare_And_Op : constant Bare_And_Op := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_And_Op.And_Op_F_Left;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_And_Op.And_Op_F_Right;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Bin_Op =>
declare
N_Bare_Bin_Op : constant Bare_Bin_Op := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Bin_Op.Bin_Op_F_Left;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Bin_Op.Bin_Op_F_Op;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Bin_Op.Bin_Op_F_Right;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Call_Expr_Range =>
declare
N_Bare_Call_Expr : constant Bare_Call_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Call_Expr.Call_Expr_F_Prefix;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Call_Expr.Call_Expr_F_Suffix;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Comp_Op_Range =>
declare
N_Bare_Comp_Op : constant Bare_Comp_Op := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Comp_Op.Comp_Op_F_Left;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Comp_Op.Comp_Op_F_Op;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Comp_Op.Comp_Op_F_Right;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Concat_String_Lit_Range =>
declare
N_Bare_Concat_String_Lit : constant Bare_Concat_String_Lit := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Concat_String_Lit.Concat_String_Lit_F_First_Str;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Concat_String_Lit.Concat_String_Lit_F_Subsequent_Str;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Dict_Comp_Range =>
declare
N_Bare_Dict_Comp : constant Bare_Dict_Comp := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Dict_Comp.Dict_Comp_F_Assoc;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Dict_Comp.Dict_Comp_F_Comprehension;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Dict_Lit_Range =>
declare
N_Bare_Dict_Lit : constant Bare_Dict_Lit := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Dict_Lit.Dict_Lit_F_Assocs;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Factor_Range =>
declare
N_Bare_Factor : constant Bare_Factor := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Factor.Factor_F_Op;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Factor.Factor_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_If_Expr_Range =>
declare
N_Bare_If_Expr : constant Bare_If_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_If_Expr.If_Expr_F_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_If_Expr.If_Expr_F_Cond;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_If_Expr.If_Expr_F_Else_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Inline_Eval_Range =>
declare
N_Bare_Inline_Eval : constant Bare_Inline_Eval := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Inline_Eval.Inline_Eval_F_Exprs;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Lambda_Def_Range =>
declare
N_Bare_Lambda_Def : constant Bare_Lambda_Def := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Lambda_Def.Lambda_Def_F_Args;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Lambda_Def.Lambda_Def_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_List_Comp_Range =>
declare
N_Bare_List_Comp : constant Bare_List_Comp := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_List_Comp.List_Comp_F_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_List_Comp.List_Comp_F_Comprehension;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_List_Gen_Range =>
declare
N_Bare_List_Gen : constant Bare_List_Gen := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_List_Gen.List_Gen_F_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_List_Gen.List_Gen_F_Comprehension;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_List_Lit_Range =>
declare
N_Bare_List_Lit : constant Bare_List_Lit := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_List_Lit.List_Lit_F_Exprs;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Dotted_Name_Range =>
declare
N_Bare_Dotted_Name : constant Bare_Dotted_Name := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Dotted_Name.Dotted_Name_F_Prefix;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Dotted_Name.Dotted_Name_F_Suffix;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Not_Op_Range =>
declare
N_Bare_Not_Op : constant Bare_Not_Op := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Not_Op.Not_Op_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Or_Expr_Range =>
declare
N_Bare_Or_Expr : constant Bare_Or_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Or_Expr.Or_Expr_F_Left;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Or_Expr.Or_Expr_F_Right;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Or_Op_Range =>
declare
N_Bare_Or_Op : constant Bare_Or_Op := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Or_Op.Or_Op_F_Left;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Or_Op.Or_Op_F_Right;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Power_Range =>
declare
N_Bare_Power : constant Bare_Power := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Power.Power_F_Left;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Power.Power_F_Right;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Set_Comp_Range =>
declare
N_Bare_Set_Comp : constant Bare_Set_Comp := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Set_Comp.Set_Comp_F_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Set_Comp.Set_Comp_F_Comprehension;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Set_Lit_Range =>
declare
N_Bare_Set_Lit : constant Bare_Set_Lit := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Set_Lit.Set_Lit_F_Exprs;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Slice_Expr_Range =>
declare
N_Bare_Slice_Expr : constant Bare_Slice_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Slice_Expr.Slice_Expr_F_First;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Slice_Expr.Slice_Expr_F_Last;
                            return;
                    

                        when others => null;
                    end case;
                
case Turkixir_Slice_Expr_Range (K) is
when Turkixir_Ext_Slice_Expr_Range =>
declare
N_Bare_Ext_Slice_Expr : constant Bare_Ext_Slice_Expr := N_Bare_Slice_Expr;
begin
case Index is

                        when 3 =>
                            Result := N_Bare_Ext_Slice_Expr.Ext_Slice_Expr_F_Stride;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when others => null;
end case;
end;
when Turkixir_Subscript_Expr_Range =>
declare
N_Bare_Subscript_Expr : constant Bare_Subscript_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Subscript_Expr.Subscript_Expr_F_Prefix;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Subscript_Expr.Subscript_Expr_F_Suffix;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Tuple_Lit_Range =>
declare
N_Bare_Tuple_Lit : constant Bare_Tuple_Lit := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Tuple_Lit.Tuple_Lit_F_Exprs;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Xor_Expr_Range =>
declare
N_Bare_Xor_Expr : constant Bare_Xor_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Xor_Expr.Xor_Expr_F_Left;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Xor_Expr.Xor_Expr_F_Right;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Yield_Expr_Range =>
declare
N_Bare_Yield_Expr : constant Bare_Yield_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Yield_Expr.Yield_Expr_F_Exprs;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_File_Node_Range =>
declare
N_Bare_File_Node : constant Bare_File_Node := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_File_Node.File_Node_F_Statements;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Params_Range =>
declare
N_Bare_Params : constant Bare_Params := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Params.Params_F_Single_Params;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Rel_Name_Range =>
declare
N_Bare_Rel_Name : constant Bare_Rel_Name := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Rel_Name.Rel_Name_F_Dots;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Rel_Name.Rel_Name_F_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Single_Param_Range =>
declare
N_Bare_Single_Param : constant Bare_Single_Param := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Single_Param.Single_Param_F_Is_Varargs;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Single_Param.Single_Param_F_Is_Kwargs;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Single_Param.Single_Param_F_Name;
                            return;
                    

                        when 4 =>
                            Result := N_Bare_Single_Param.Single_Param_F_Default_Value;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Assert_Stmt_Range =>
declare
N_Bare_Assert_Stmt : constant Bare_Assert_Stmt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Assert_Stmt.Assert_Stmt_F_Test_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Assert_Stmt.Assert_Stmt_F_Msg;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Assign_Stmt_Range =>
declare
N_Bare_Assign_Stmt : constant Bare_Assign_Stmt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Assign_Stmt.Assign_Stmt_F_L_Value;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Assign_Stmt.Assign_Stmt_F_R_Values;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Aug_Assign_Stmt_Range =>
declare
N_Bare_Aug_Assign_Stmt : constant Bare_Aug_Assign_Stmt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Aug_Assign_Stmt.Aug_Assign_Stmt_F_L_Value;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Aug_Assign_Stmt.Aug_Assign_Stmt_F_Op;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Aug_Assign_Stmt.Aug_Assign_Stmt_F_R_Value;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Decorated_Range =>
declare
N_Bare_Decorated : constant Bare_Decorated := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Decorated.Decorated_F_Decorators;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Decorated.Decorated_F_Defn;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Class_Def_Range =>
declare
N_Bare_Class_Def : constant Bare_Class_Def := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Class_Def.Class_Def_F_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Class_Def.Class_Def_F_Bases;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Class_Def.Class_Def_F_Statements;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Func_Def_Range =>
declare
N_Bare_Func_Def : constant Bare_Func_Def := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Func_Def.Func_Def_F_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Func_Def.Func_Def_F_Parameters;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Func_Def.Func_Def_F_Body;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Del_Stmt_Range =>
declare
N_Bare_Del_Stmt : constant Bare_Del_Stmt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Del_Stmt.Del_Stmt_F_Exprs;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Elif_Branch_Range =>
declare
N_Bare_Elif_Branch : constant Bare_Elif_Branch := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Elif_Branch.Elif_Branch_F_Cond_Test;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Elif_Branch.Elif_Branch_F_Statements;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Exec_Stmt_Range =>
declare
N_Bare_Exec_Stmt : constant Bare_Exec_Stmt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Exec_Stmt.Exec_Stmt_F_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Exec_Stmt.Exec_Stmt_F_In_List;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_For_Stmt_Range =>
declare
N_Bare_For_Stmt : constant Bare_For_Stmt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_For_Stmt.For_Stmt_F_Bindings;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_For_Stmt.For_Stmt_F_Expr;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_For_Stmt.For_Stmt_F_Statements;
                            return;
                    

                        when 4 =>
                            Result := N_Bare_For_Stmt.For_Stmt_F_Else_Part;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Global_Stmt_Range =>
declare
N_Bare_Global_Stmt : constant Bare_Global_Stmt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Global_Stmt.Global_Stmt_F_Names;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_If_Stmt_Range =>
declare
N_Bare_If_Stmt : constant Bare_If_Stmt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_If_Stmt.If_Stmt_F_Cond_Test;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_If_Stmt.If_Stmt_F_Statements;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_If_Stmt.If_Stmt_F_Elif_Branchs;
                            return;
                    

                        when 4 =>
                            Result := N_Bare_If_Stmt.If_Stmt_F_Else_Part;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Import_From_Range =>
declare
N_Bare_Import_From : constant Bare_Import_From := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Import_From.Import_From_F_Rel_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Import_From.Import_From_F_Imported;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Import_Name_Range =>
declare
N_Bare_Import_Name : constant Bare_Import_Name := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Import_Name.Import_Name_F_Imported_Names;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Print_Stmt_Range =>
declare
N_Bare_Print_Stmt : constant Bare_Print_Stmt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Print_Stmt.Print_Stmt_F_Exprs;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Raise_Stmt_Range =>
declare
N_Bare_Raise_Stmt : constant Bare_Raise_Stmt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Raise_Stmt.Raise_Stmt_F_Exprs;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Return_Stmt_Range =>
declare
N_Bare_Return_Stmt : constant Bare_Return_Stmt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Return_Stmt.Return_Stmt_F_Exprs;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Stream_Print_Stmt_Range =>
declare
N_Bare_Stream_Print_Stmt : constant Bare_Stream_Print_Stmt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Stream_Print_Stmt.Stream_Print_Stmt_F_Stream_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Stream_Print_Stmt.Stream_Print_Stmt_F_Exprs;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Try_Stmt_Range =>
declare
N_Bare_Try_Stmt : constant Bare_Try_Stmt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Try_Stmt.Try_Stmt_F_Statements;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Try_Stmt.Try_Stmt_F_Except_Parts;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Try_Stmt.Try_Stmt_F_Else_Part;
                            return;
                    

                        when 4 =>
                            Result := N_Bare_Try_Stmt.Try_Stmt_F_Finally_Part;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_While_Stmt_Range =>
declare
N_Bare_While_Stmt : constant Bare_While_Stmt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_While_Stmt.While_Stmt_F_Cond_Test;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_While_Stmt.While_Stmt_F_Statements;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_While_Stmt.While_Stmt_F_Else_Part;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_With_Stmt_Range =>
declare
N_Bare_With_Stmt : constant Bare_With_Stmt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_With_Stmt.With_Stmt_F_Bindings;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_With_Stmt.With_Stmt_F_Statements;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Turkixir_Turkixir_Node_Base_List =>
declare
N_Bare_Turkixir_Node_Base_List : constant Bare_Turkixir_Node_Base_List := Node;
begin

                    if Index > N_Bare_Turkixir_Node_Base_List.Count then
                        Index_In_Bounds := False;
                    else
                        Result := N_Bare_Turkixir_Node_Base_List.Nodes (Index);
                    end if;
                    return;
                
end;
when others => null;
end case;

      --  Execution should reach this point iff nothing matched this index, so
      --  we must be out of bounds.
      Index_In_Bounds := False;
   end Get_Child;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : Bare_Turkixir_Node;
      Show_Slocs  : Boolean;
      Line_Prefix : String := "")
   is
      K               : Turkixir_Node_Kind_Type;
      Attr_Prefix     : constant String := Line_Prefix & "|";
      Children_Prefix : constant String := Line_Prefix & "|  ";

   begin
      if Node = null then
         Put_Line ("None");
         return;
      end if;
      K := Node.Kind;

      Put (Line_Prefix & Kind_Name (Node));
      if Show_Slocs then
         Put ("[" & Image (Sloc_Range (Node)) & "]");
      end if;

      if Is_Incomplete (Node) then
         Put (" <<INCOMPLETE>>");
      end if;

      if Is_Token_Node (Node.Kind) then
         Put_Line (": " & Image (Text (Node)));

      elsif Node.Kind not in Turkixir_Turkixir_Node_Base_List then
         New_Line;

      end if;

         --  List nodes are displayed in a special way (they have no field)
         if K in Turkixir_Turkixir_Node_Base_List then
            if Node.Count = 0 then
               Put_Line (": <empty list>");
               return;
            end if;

            New_Line;
            for Child of Node.Nodes (1 .. Node.Count) loop
               if Child /= null then
                  Print (Child, Show_Slocs, Line_Prefix & "|  ");
               end if;
            end loop;
            return;
         end if;

         --  This is for regular nodes: display each field
         declare
            use Libturkixirlang.Introspection_Implementation;
            Field_List : constant Syntax_Field_Reference_Array :=
               Syntax_Fields (K);
         begin
            for I in Field_List'Range loop
               declare
                  Child : constant Bare_Turkixir_Node :=
                     Implementation.Child (Node, I);
               begin
                  Put
                    (Attr_Prefix
                     & Image (Syntax_Field_Name (Field_List (I)))
                     & ":");
                  if Child /= null then
                     New_Line;
                     Print (Child, Show_Slocs, Children_Prefix);
                  else
                     Put_Line (" <null>");
                  end if;
               end;
            end loop;
         end;
   end Print;

   ------------
   -- Parent --
   ------------

   function Parent (Node : Bare_Turkixir_Node) return Bare_Turkixir_Node is
   begin
      return Node.Parent;
   end Parent;

   ------------------
   -- Stored_Token --
   ------------------

   function Stored_Token
     (Node  : Bare_Turkixir_Node;
      Token : Token_Reference) return Token_Index
   is
      Index : constant Token_Or_Trivia_Index := Get_Token_Index (Token);
   begin
      if Node.Unit.TDH'Access /= Get_Token_TDH (Token) then
         raise Property_Error with
           ("Cannot associate a token and a node from different analysis"
            & " units");
      elsif Index.Trivia /= No_Token_Index then
         raise Property_Error with
           ("A node cannot hold trivia");
      end if;

      return Index.Token;
   end Stored_Token;

   -------------------------
   -- Children_And_Trivia --
   -------------------------

   function Children_And_Trivia
     (Node : Bare_Turkixir_Node) return Bare_Children_Array
   is
      package Children_Vectors is new Ada.Containers.Vectors
        (Positive, Bare_Child_Record);
      use Children_Vectors;

      Ret_Vec : Vector;
      Ctx     : Internal_Context renames Node.Unit.Context;
      TDH     : Token_Data_Handler renames Node.Unit.TDH;

      procedure Append_Trivias (First, Last : Token_Index);
      --  Append all the trivias of tokens between indices First and Last to
      --  the returned vector.

      function Filter_Children
        (Parent : Bare_Turkixir_Node)
         return Internal_Bare_Turkixir_Node_Array;
      --  Return an array for all children in Parent that are not null

      --------------------
      -- Append_Trivias --
      --------------------

      procedure Append_Trivias (First, Last : Token_Index) is
      begin
         for I in First .. Last loop
            for D of Get_Trivias (TDH, I) loop
               Ret_Vec.Append
                 (Bare_Child_Record'
                    (Kind   => Trivia,
                     Trivia => Wrap_Token_Reference
                                 (Ctx, TDH'Access, (I, D))));
            end loop;
         end loop;
      end Append_Trivias;

      ---------------------
      -- Filter_Children --
      ---------------------

      function Filter_Children
        (Parent : Bare_Turkixir_Node)
         return Internal_Bare_Turkixir_Node_Array
      is
         Children : constant Internal_Bare_Turkixir_Node_Array :=
            Implementation.Children (Parent);
         Result   : Internal_Bare_Turkixir_Node_Array (Children'Range);
         Next     : Integer := Result'First;
      begin
         for I in Children'Range loop
            if Children (I) /= null then
               Result (Next) := Children (I);
               Next := Next + 1;
            end if;
         end loop;
         return Result (Result'First .. Next - 1);
      end Filter_Children;

      First_Child : constant Positive := 1;
      N_Children  : constant Internal_Bare_Turkixir_Node_Array :=
         Filter_Children (Node);
   begin
      if N_Children'Length > 0
        and then (Node.Token_Start_Index
                    /= N_Children (First_Child).Token_Start_Index)
      then
         Append_Trivias (Node.Token_Start_Index,
                         N_Children (First_Child).Token_Start_Index - 1);
      end if;

      --  Append each node to Ret_Vec, and append trivia that follow after each
      --  non-ghost nodes.
      for I in N_Children'Range loop
         Ret_Vec.Append (Bare_Child_Record'(Child, N_Children (I)));
         if not Is_Ghost (N_Children (I)) then
            Append_Trivias (N_Children (I).Token_End_Index,
                            (if I = N_Children'Last
                             then Node.Token_End_Index - 1
                             else N_Children (I + 1).Token_Start_Index - 1));
         end if;
      end loop;

      declare
         A : Bare_Children_Array (1 .. Natural (Ret_Vec.Length));
      begin
         for I in A'Range loop
            A (I) := Ret_Vec.Element (I);
         end loop;
         return A;
      end;
   end Children_And_Trivia;

   --------------
   -- Is_Ghost --
   --------------

   function Is_Ghost (Node : Bare_Turkixir_Node) return Boolean
   is (Node.Token_End_Index = No_Token_Index);

   -------------------
   -- Is_Incomplete --
   -------------------

   function Is_Incomplete (Node : Bare_Turkixir_Node) return Boolean
   is
      LGC : Bare_Turkixir_Node;
   begin
     if Is_List_Node (Node.Kind) then
        LGC := (if Last_Child_Index (Node) /= 0
                then Child (Node, Last_Child_Index (Node))
                else null);
        return LGC /= null and then Is_Incomplete (LGC);
      else
         return Node.Last_Attempted_Child > -1;
      end if;
   end;

   -----------------
   -- Token_Start --
   -----------------

   function Token_Start (Node : Bare_Turkixir_Node) return Token_Reference
   is (Token (Node, Node.Token_Start_Index));

   ---------------
   -- Token_End --
   ---------------

   function Token_End (Node : Bare_Turkixir_Node) return Token_Reference
   is
     (if Node.Token_End_Index = No_Token_Index
      then Token_Start (Node)
      else Token (Node, Node.Token_End_Index));

   -----------
   -- Token --
   -----------

   function Token
     (Node  : Bare_Turkixir_Node;
      Index : Token_Index) return Token_Reference
   is
      Unit    : constant Internal_Unit := Node.Unit;
      Context : constant Internal_Context := Unit.Context;
   begin
      return Wrap_Token_Reference
        (Context, Token_Data (Unit), (Index, No_Token_Index));
   end Token;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Bare_Turkixir_Node) return Boolean is
   begin
      --  Reject invalid inputs
      if Left /= null and Is_Synthetic (Left) then
         raise Property_Error with "left node is synthetic";
      elsif Right /= null and Is_Synthetic (Right) then
         raise Property_Error with "right node is synthetic";
      end if;

      --  Null nodes come first
      if Left = null then
         return Right /= null;
      elsif Right = null then
         return False;
      end if;

      --  So we have two non-null nodes. Sort by unit filename
      if Left.Unit < Right.Unit then
         return True;
      elsif Left.Unit /= Right.Unit then
         return False;
      end if;

      --  Both nodes come from the same unit: compare their token indexes
      if Left.Token_Start_Index < Right.Token_Start_Index then
         return True;
      elsif Left.Token_Start_Index > Right.Token_Start_Index then
         return False;
      else
         return Left.Token_End_Index < Right.Token_End_Index;
      end if;
   end "<";

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Node : Bare_Turkixir_Node) return Boolean
   is (Node = null);

   ----------
   -- Kind --
   ----------

   function Kind (Node : Bare_Turkixir_Node) return Turkixir_Node_Kind_Type
   is (Node.Kind);

   -----------------
   -- Child_Index --
   -----------------

   function Child_Index (Node : Bare_Turkixir_Node) return Integer
   is
      N : Bare_Turkixir_Node := null;
   begin
      if Node.Parent = null then
         raise Property_Error with
            "Trying to get the child index of a root node";
      end if;

      for I in First_Child_Index (Node.Parent)
            .. Last_Child_Index (Node.Parent)
      loop
         N := Child (Node.Parent, I);
         if N = Node then
            return I - 1;
         end if;
      end loop;

      --  If we reach this point, then Node isn't a Child of Node.Parent. This
      --  is not supposed to happen.
      raise Program_Error;
   end Child_Index;

   -------------------
   -- Fetch_Sibling --
   -------------------

   function Fetch_Sibling
     (Node   : Bare_Turkixir_Node;
      Offset : Integer) return Bare_Turkixir_Node is
   begin
      --  Root nodes have no sibling: handle them now to avoid invalid requests
      --  in the code below.
      if Node.Parent = null then
         return null;
      end if;

      declare
         Node_Index : constant Positive := Child_Index (Node) + 1;
         --  Child_Index is 0-based, but the Child primitive expects a 1-based
         --  index.

         Sibling_Index : constant Integer := Node_Index + Offset;
      begin
         --  Child returns null for out-of-bound indexes

         return (if Sibling_Index >= 1
                 then Child (Node.Parent, Sibling_Index)
                 else null);
      end;
   end Fetch_Sibling;

   -------------------
   -- Fetch_Sibling --
   -------------------

   function Fetch_Sibling
     (Node   : Bare_Turkixir_Node;
      E_Info : Internal_Entity_Info;
      Offset : Integer) return Internal_Entity
   is
      Sibling : constant Bare_Turkixir_Node := Fetch_Sibling (Node, Offset);
   begin
      --  Don't forget to clear entity info if the result is nul

      return (if Sibling = null
              then No_Entity
              else (Sibling, E_Info));
   end Fetch_Sibling;

   ----------------------
   -- Previous_Sibling --
   ----------------------

   function Previous_Sibling
     (Node   : Bare_Turkixir_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Internal_Entity is
   begin
      return Fetch_Sibling (Node, E_Info, -1);
   end Previous_Sibling;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling
     (Node   : Bare_Turkixir_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Internal_Entity is
   begin
      return Fetch_Sibling (Node, E_Info, 1);
   end Next_Sibling;


   -------------
   -- Combine --
   -------------

   function Combine
     (L, R : Internal_Metadata) return Internal_Metadata
   is
      pragma Unreferenced (L, R);
      Ret : Internal_Metadata := No_Metadata;
   begin
      return Ret;
   end Combine;

   -------------------------------
   -- Create_Static_Lexical_Env --
   -------------------------------

   function Create_Static_Lexical_Env
     (Parent            : Lexical_Env;
      Node              : Bare_Turkixir_Node;
      Transitive_Parent : Boolean := False) return Lexical_Env
   is
      Unit : constant Internal_Unit :=
        (if Node = null then null else Node.Unit);
   begin
      return Result : Lexical_Env := Create_Lexical_Env
        (Parent, Node, Transitive_Parent, Convert_Unit (Unit))
      do
         if Unit /= null then
            Register_Destroyable (Unit, Unwrap (Result.Env));
         end if;
      end return;
   end Create_Static_Lexical_Env;

   ---------
   -- Get --
   ---------

   function Get
     (A     : AST_Envs.Entity_Array;
      Index : Integer) return Internal_Entity
   is
      function Length (A : AST_Envs.Entity_Array) return Natural
      is (A'Length);

      function Get
        (A     : AST_Envs.Entity_Array;
         Index : Integer) return Internal_Entity
      is (A (Index + 1)); --  A is 1-based but Index is 0-based

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => Entity,
         Sequence_Type => AST_Envs.Entity_Array,
         Length        => Length,
         Get           => Get);
      Result : Internal_Entity;
   begin
      if Relative_Get (A, Index, Result) then
         return Result;
      else
         raise Property_Error with "out-of-bounds array access";
      end if;
   end Get;

   -----------
   -- Group --
   -----------

   function Group
     (Envs   : Lexical_Env_Array_Access;
      Env_Md : Internal_Metadata := No_Metadata) return Lexical_Env
   is (Group (Lexical_Env_Array (Envs.Items), Env_Md));

       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   




   ------------------
   -- Children_Env --
   ------------------

   function Children_Env
     (Node   : Bare_Turkixir_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Lexical_Env
   is (Rebind_Env (Node.Self_Env, E_Info));

   --------------
   -- Node_Env --
   --------------

   function Node_Env
     (Node   : Bare_Turkixir_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Lexical_Env
   is
      function Get_Base_Env return Lexical_Env;
      --  Return the environment that we need to rebind before returning

      ------------------
      -- Get_Base_Env --
      ------------------

      function Get_Base_Env return Lexical_Env is
         pragma Warnings (Off, "referenced");
         function Get_Parent_Env return Lexical_Env;
         pragma Warnings (On, "referenced");

         --------------------
         -- Get_Parent_Env --
         --------------------

         function Get_Parent_Env return Lexical_Env is
            Parent : constant Lexical_Env := AST_Envs.Parent (Node.Self_Env);
         begin
            --  If Node is the root scope or the empty environment, Parent can
            --  be a wrapper around the null node. Turn this into the
            --  Empty_Env, as null envs are erroneous values in properties.
            return (if Unwrap (Parent) = null
                    then Empty_Env
                    else Parent);
         end Get_Parent_Env;

      begin
         
         return
           (if Node.Kind in Turkixir_Class_Def | Turkixir_Func_Def
            then Get_Parent_Env
            else Node.Self_Env);
      end Get_Base_Env;

      Base_Env : Lexical_Env := Get_Base_Env;
      Result   : constant Lexical_Env := Rebind_Env (Base_Env, E_Info);
   begin
      Dec_Ref (Base_Env);
      return Result;
   end Node_Env;

   ------------
   -- Parent --
   ------------

   function Parent
     (Node   : Bare_Turkixir_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Internal_Entity is
   begin
      --  TODO: shed entity information as appropriate
      return (Node.Parent, E_Info);
   end Parent;

   -------------
   -- Parents --
   -------------

   function Parents
     (Node      : Bare_Turkixir_Node;
      With_Self : Boolean := True;
      E_Info    : Internal_Entity_Info := No_Entity_Info)
      return Internal_Entity_Array_Access
   is
      Bare_Parents : Bare_Turkixir_Node_Array_Access := Parents (Node, With_Self);
      Result       : Internal_Entity_Array_Access :=
         Create_Internal_Entity_Array (Bare_Parents.N);
   begin
      --  TODO: shed entity information as appropriate
      for I in Bare_Parents.Items'Range loop
         Result.Items (I) := (Bare_Parents.Items (I), E_Info);
      end loop;
      Dec_Ref (Bare_Parents);
      return Result;
   end Parents;

   --------------
   -- Children --
   --------------

   function Children
     (Node   : Bare_Turkixir_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Internal_Entity_Array_Access
   is
      Bare_Children : Bare_Turkixir_Node_Array_Access := Children (Node);
      Result        : Internal_Entity_Array_Access :=
         Create_Internal_Entity_Array (Bare_Children.N);
   begin
      --  TODO: shed entity information as appropriate
      for I in Bare_Children.Items'Range loop
         Result.Items (I) := (Bare_Children.Items (I), E_Info);
      end loop;
      Dec_Ref (Bare_Children);
      return Result;
   end Children;

   ---------------------
   -- New_Unit_String --
   ---------------------

   function New_Unit_String
     (Unit : Internal_Unit; Str : String) return String_Access
   is
      procedure Register_Destroyable_String is new Register_Destroyable_Gen
        (String, String_Access, Free);
   begin
      return Ret : String_Access := new String'(Str) do
         Register_Destroyable_String (Unit, Ret);
      end return;
   end New_Unit_String;

   --------------------------------
   -- Assign_Names_To_Logic_Vars --
   --------------------------------

   procedure Assign_Names_To_Logic_Vars (Node : Bare_Turkixir_Node) is

      pragma Warnings (Off, "referenced");

      procedure Assign
        (Node  : Bare_Turkixir_Node;
         LV    : in out Logic_Var_Record;
         Field : String);
      --  Assign a name to the LV logic variable. Node must be the node that
      --  owns LV, and Field must be the name of the field in Node that holds
      --  LV.

      ------------
      -- Assign --
      ------------

      procedure Assign
        (Node  : Bare_Turkixir_Node;
         LV    : in out Logic_Var_Record;
         Field : String) is
      begin
         LV.Dbg_Name :=
           New_Unit_String
             (Node.Unit, Image (Short_Text_Image (Node)) & "." & Field);
      end Assign;

      K : constant Turkixir_Node_Kind_Type := Node.Kind;

      pragma Warnings (On, "referenced");

   begin
      
      null;
      for Child of Internal_Bare_Turkixir_Node_Array'(Children (Node)) loop
         if Child /= null then
            Assign_Names_To_Logic_Vars (Child);
         end if;
      end loop;
   end Assign_Names_To_Logic_Vars;

   ----------------
   -- Text_Image --
   ----------------

   function Text_Image (Ent : Internal_Entity) return Text_Type is
   begin
      if Ent.Node /= null then
         declare
            Node_Image : constant Text_Type := Short_Text_Image (Ent.Node);
         begin
            return
            (if Ent.Info.Rebindings /= null
             then "<| "
             & Node_Image (Node_Image'First + 1 .. Node_Image'Last - 1) & " "
             & AST_Envs.Text_Image (Ent.Info.Rebindings) & " |>"
             else Node_Image);
         end;
      else
         return "None";
      end if;
   end Text_Image;

   ---------------------
   -- Full_Sloc_Image --
   ---------------------

   function Full_Sloc_Image (Node : Bare_Turkixir_Node) return String_Type
   is
      Res      : constant Text_Type :=
        To_Text
          (Ada.Directories.Simple_Name
             (Get_Filename (Unit (Node))))
           & ":" & To_Text (Image (Start_Sloc (Sloc_Range (Node)))) & ": ";
   begin
      return Create_String (Res);
   end Full_Sloc_Image;

   -----------
   -- Image --
   -----------

   function Image (Ent : Internal_Entity) return String is
      Result : constant Text_Type := Text_Image (Ent);
   begin
      return Image (Result);
   end Image;

   ---------------
   -- Can_Reach --
   ---------------

   function Can_Reach (El, From : Bare_Turkixir_Node) return Boolean is
   begin
      --  Since this function is only used to implement sequential semantics in
      --  envs, we consider that elements coming from different units are
      --  always visible for each other, and let the user implement language
      --  specific visibility rules in the DSL.
      if El = null or else From = null or else El.Unit /= From.Unit then
         return True;
      end if;

      return Compare
        (Start_Sloc (Sloc_Range (El)),
         Start_Sloc (Sloc_Range (From))) = After;
   end Can_Reach;

   -----------------
   -- Hash_Entity --
   -----------------

   function Hash_Entity (Self : Internal_Entity) return Hash_Type is
   begin
      return Combine (Hash (Self.Node), Hash (Self.Info.Rebindings));
   end Hash_Entity;

   --------------------
   -- Compare_Entity --
   --------------------

   function Compare_Entity (Left, Right : Internal_Entity) return Boolean
   is
   begin
      return (Left.Node = Right.Node
              and then Left.Info.Rebindings = Right.Info.Rebindings);
   end Compare_Entity;

   --------------------------------
   -- Create_Dynamic_Lexical_Env --
   --------------------------------

   function Create_Dynamic_Lexical_Env
     (Self              : Bare_Turkixir_Node;
      Assocs_Getter     : Inner_Env_Assocs_Resolver;
      Assoc_Resolver    : Entity_Resolver;
      Transitive_Parent : Boolean) return Lexical_Env
   is
      Unit : constant Internal_Unit := Self.Unit;
   begin
      --  This restriction is necessary to avoid relocation issues when
      --  Self.Self_Env is terminated.
      if Is_Foreign_Strict (Self.Self_Env, Self) then
         raise Property_Error with
           ("cannot create a dynamic lexical env when Self.Self_Env is"
            & " foreign");
      end if;

      return Result : constant Lexical_Env := Create_Dynamic_Lexical_Env
        (Parent            => Null_Lexical_Env,
         Node              => Self,
         Transitive_Parent => Transitive_Parent,
         Owner             => Convert_Unit (Unit),
         Assocs_Getter     => Assocs_Getter,
         Assoc_Resolver    => Assoc_Resolver)
      do
         --  Since dynamic lexical environments can only be created in lazy
         --  field initializers, it is fine to tie Result's lifetime to the
         --  its owning unit's lifetime.
         Register_Destroyable (Unit, Unwrap (Result));
      end return;
   end Create_Dynamic_Lexical_Env;

   procedure Destroy_Synthetic_Node (Node : in out Bare_Turkixir_Node);
   --  Helper for the Register_Destroyable above

   ------------
   -- Length --
   ------------

   function Length (Node : Bare_Turkixir_Node_Base_List) return Natural
   is (if Node = null then 0 else Children_Count (Node));


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (B : Boolean) return String is
      begin
         return (if B then "True" else "False");
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (I : Integer) return String is
      begin
         return Integer'Image (I);
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (S : Symbol_Type) return String is
      begin
         return (if S = null
                 then "None"
                 else Image (S.all, With_Quotes => True));
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (C : Character_Type) return String is
         C_Str : constant Text_Type := (1 => C);
      begin
         return "'" & Image (C_Str) & "'";
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (S : String_Type) return String is
      begin
         return Image (S.Content, With_Quotes => True);
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Env : Lexical_Env) return String is
      begin
         case Env.Kind is
         when Static_Primary =>
            return "<LexicalEnv static-primary for "
                   & Trace_Image (Env_Node (Env)) & ">";
         when others =>
            return "<LexicalEnv synthetic>";
         end case;
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (R : Env_Rebindings) return String is
      begin
         return Image (Text_Image (R));
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Unit : Internal_Unit) return String is
      begin
         return "Internal_Unit (""" & Basename (Unit) & """)";
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Eq : Logic_Equation) return String is
         pragma Unreferenced (Eq);
      begin
         return "<LogicEquation>";
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Var : Logic_Var) return String is
         pragma Unreferenced (Var);
      begin
         return "<LogicVariable>";
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (K : Analysis_Unit_Kind) return String is
      begin
         return Analysis_Unit_Kind'Image (K);
      end Trace_Image;


   

   


      -------------
      -- Inc_Ref --
      -------------

      procedure Inc_Ref (R : Internal_DesignatedEnv) is
      begin
               Inc_Ref (R.Direct_Env);
      end Inc_Ref;

      -------------
      -- Dec_Ref --
      -------------

      procedure Dec_Ref (R : in out Internal_DesignatedEnv) is
      begin
               Dec_Ref (R.Direct_Env);
      end Dec_Ref;




      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_DesignatedEnv) return Boolean is
      begin
         return L.Kind = R.Kind and then L.Env_Name = R.Env_Name and then Equivalent (L.Direct_Env, R.Direct_Env);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_DesignatedEnv) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Kind => " & Trace_Image (R.Kind)
                        & ", "
                     & "Env_Name => " & Trace_Image (R.Env_Name)
                        & ", "
                     & "Direct_Env => " & Trace_Image (R.Direct_Env)
               & ")");
      end Trace_Image;


   

   




   

      ----------
      -- Hash --
      ----------

      pragma Warnings (Off, "referenced");
      function Hash (R : Internal_Metadata) return Hash_Type is
         pragma Warnings (On, "referenced");
      begin
         
            return Initial_Hash;
      end Hash;


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Metadata) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                  & "null record"
               & ")");
      end Trace_Image;


   

   




   

      ----------
      -- Hash --
      ----------

      pragma Warnings (Off, "referenced");
      function Hash (R : Internal_Entity_Info) return Hash_Type is
         pragma Warnings (On, "referenced");
      begin
         
            return Combine ((Hash (R.MD), Hash (R.Rebindings), Hash (R.From_Rebound)));
      end Hash;


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Info) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "MD => " & Trace_Image (R.MD)
                        & ", "
                     & "Rebindings => " & Trace_Image (R.Rebindings)
                        & ", "
                     & "From_Rebound => " & Trace_Image (R.From_Rebound)
               & ")");
      end Trace_Image;


   

   



      function Create_Internal_Entity
        (Node : Bare_Turkixir_Node; Info : Internal_Entity_Info)
         return Internal_Entity is
      begin
         if Node = null then
            return No_Entity;
         end if;
         return (Node => Node, Info => Info);
      end;



   

      ----------
      -- Hash --
      ----------

      pragma Warnings (Off, "referenced");
      function Hash (R : Internal_Entity) return Hash_Type is
         pragma Warnings (On, "referenced");
      begin
         
            return Combine
              (Hash (R.Node), Hash (R.Info));
      end Hash;


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Expr
        (Node : Bare_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Expr is
      begin
         if Node = null then
            return No_Entity_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_And_Expr
        (Node : Bare_And_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_And_Expr is
      begin
         if Node = null then
            return No_Entity_And_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_And_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_And_Op
        (Node : Bare_And_Op; Info : Internal_Entity_Info)
         return Internal_Entity_And_Op is
      begin
         if Node = null then
            return No_Entity_And_Op;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_And_Op) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Arg
        (Node : Bare_Arg; Info : Internal_Entity_Info)
         return Internal_Entity_Arg is
      begin
         if Node = null then
            return No_Entity_Arg;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Arg) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Arg_Assoc
        (Node : Bare_Arg_Assoc; Info : Internal_Entity_Info)
         return Internal_Entity_Arg_Assoc is
      begin
         if Node = null then
            return No_Entity_Arg_Assoc;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Arg_Assoc) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Arg_Gen
        (Node : Bare_Arg_Gen; Info : Internal_Entity_Info)
         return Internal_Entity_Arg_Gen is
      begin
         if Node = null then
            return No_Entity_Arg_Gen;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Arg_Gen) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Turkixir_Node_Base_List
        (Node : Bare_Turkixir_Node_Base_List; Info : Internal_Entity_Info)
         return Internal_Entity_Turkixir_Node_Base_List is
      begin
         if Node = null then
            return No_Entity_Turkixir_Node_Base_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Turkixir_Node_Base_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Arg_List
        (Node : Bare_Arg_List; Info : Internal_Entity_Info)
         return Internal_Entity_Arg_List is
      begin
         if Node = null then
            return No_Entity_Arg_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Arg_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Bin_Op
        (Node : Bare_Bin_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Bin_Op is
      begin
         if Node = null then
            return No_Entity_Bin_Op;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Bin_Op) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Arith_Expr
        (Node : Bare_Arith_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Arith_Expr is
      begin
         if Node = null then
            return No_Entity_Arith_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Arith_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_As_Name_Node
        (Node : Bare_As_Name_Node; Info : Internal_Entity_Info)
         return Internal_Entity_As_Name_Node is
      begin
         if Node = null then
            return No_Entity_As_Name_Node;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_As_Name_Node) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_As_Name_Node_List
        (Node : Bare_As_Name_Node_List; Info : Internal_Entity_Info)
         return Internal_Entity_As_Name_Node_List is
      begin
         if Node = null then
            return No_Entity_As_Name_Node_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_As_Name_Node_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Stmt
        (Node : Bare_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Stmt is
      begin
         if Node = null then
            return No_Entity_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Assert_Stmt
        (Node : Bare_Assert_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Assert_Stmt is
      begin
         if Node = null then
            return No_Entity_Assert_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Assert_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Assign_Stmt
        (Node : Bare_Assign_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Assign_Stmt is
      begin
         if Node = null then
            return No_Entity_Assign_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Assign_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Aug_Assign_Stmt
        (Node : Bare_Aug_Assign_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Aug_Assign_Stmt is
      begin
         if Node = null then
            return No_Entity_Aug_Assign_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Aug_Assign_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Break_Stmt
        (Node : Bare_Break_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Break_Stmt is
      begin
         if Node = null then
            return No_Entity_Break_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Break_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Call_Expr
        (Node : Bare_Call_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Call_Expr is
      begin
         if Node = null then
            return No_Entity_Call_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Call_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Def_Stmt
        (Node : Bare_Def_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Def_Stmt is
      begin
         if Node = null then
            return No_Entity_Def_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Def_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Class_Def
        (Node : Bare_Class_Def; Info : Internal_Entity_Info)
         return Internal_Entity_Class_Def is
      begin
         if Node = null then
            return No_Entity_Class_Def;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Class_Def) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comprehension
        (Node : Bare_Comprehension; Info : Internal_Entity_Info)
         return Internal_Entity_Comprehension is
      begin
         if Node = null then
            return No_Entity_Comprehension;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comprehension) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_For
        (Node : Bare_Comp_For; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_For is
      begin
         if Node = null then
            return No_Entity_Comp_For;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_For) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_ForL
        (Node : Bare_Comp_ForL; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_ForL is
      begin
         if Node = null then
            return No_Entity_Comp_ForL;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_ForL) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_If
        (Node : Bare_Comp_If; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_If is
      begin
         if Node = null then
            return No_Entity_Comp_If;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_If) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_Op
        (Node : Bare_Comp_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op is
      begin
         if Node = null then
            return No_Entity_Comp_Op;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_Op) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_Op_Kind
        (Node : Bare_Comp_Op_Kind; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind is
      begin
         if Node = null then
            return No_Entity_Comp_Op_Kind;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_Op_Kind) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_Op_Kind_Diamond
        (Node : Bare_Comp_Op_Kind_Diamond; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Diamond is
      begin
         if Node = null then
            return No_Entity_Comp_Op_Kind_Diamond;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Diamond) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_Op_Kind_Eq
        (Node : Bare_Comp_Op_Kind_Eq; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Eq is
      begin
         if Node = null then
            return No_Entity_Comp_Op_Kind_Eq;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Eq) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_Op_Kind_Gt
        (Node : Bare_Comp_Op_Kind_Gt; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Gt is
      begin
         if Node = null then
            return No_Entity_Comp_Op_Kind_Gt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Gt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_Op_Kind_Gte
        (Node : Bare_Comp_Op_Kind_Gte; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Gte is
      begin
         if Node = null then
            return No_Entity_Comp_Op_Kind_Gte;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Gte) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_Op_Kind_In
        (Node : Bare_Comp_Op_Kind_In; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_In is
      begin
         if Node = null then
            return No_Entity_Comp_Op_Kind_In;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_In) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_Op_Kind_Is
        (Node : Bare_Comp_Op_Kind_Is; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Is is
      begin
         if Node = null then
            return No_Entity_Comp_Op_Kind_Is;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Is) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_Op_Kind_Isnot
        (Node : Bare_Comp_Op_Kind_Isnot; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Isnot is
      begin
         if Node = null then
            return No_Entity_Comp_Op_Kind_Isnot;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Isnot) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_Op_Kind_Lt
        (Node : Bare_Comp_Op_Kind_Lt; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Lt is
      begin
         if Node = null then
            return No_Entity_Comp_Op_Kind_Lt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Lt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_Op_Kind_Lte
        (Node : Bare_Comp_Op_Kind_Lte; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Lte is
      begin
         if Node = null then
            return No_Entity_Comp_Op_Kind_Lte;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Lte) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_Op_Kind_Noteq
        (Node : Bare_Comp_Op_Kind_Noteq; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Noteq is
      begin
         if Node = null then
            return No_Entity_Comp_Op_Kind_Noteq;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Noteq) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Comp_Op_Kind_Notin
        (Node : Bare_Comp_Op_Kind_Notin; Info : Internal_Entity_Info)
         return Internal_Entity_Comp_Op_Kind_Notin is
      begin
         if Node = null then
            return No_Entity_Comp_Op_Kind_Notin;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Comp_Op_Kind_Notin) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Concat_String_Lit
        (Node : Bare_Concat_String_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Concat_String_Lit is
      begin
         if Node = null then
            return No_Entity_Concat_String_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Concat_String_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Continue_Stmt
        (Node : Bare_Continue_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Continue_Stmt is
      begin
         if Node = null then
            return No_Entity_Continue_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Continue_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Decorated
        (Node : Bare_Decorated; Info : Internal_Entity_Info)
         return Internal_Entity_Decorated is
      begin
         if Node = null then
            return No_Entity_Decorated;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Decorated) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Decorator
        (Node : Bare_Decorator; Info : Internal_Entity_Info)
         return Internal_Entity_Decorator is
      begin
         if Node = null then
            return No_Entity_Decorator;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Decorator) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Decorator_List
        (Node : Bare_Decorator_List; Info : Internal_Entity_Info)
         return Internal_Entity_Decorator_List is
      begin
         if Node = null then
            return No_Entity_Decorator_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Decorator_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Del_Stmt
        (Node : Bare_Del_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Del_Stmt is
      begin
         if Node = null then
            return No_Entity_Del_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Del_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Dict_Assoc
        (Node : Bare_Dict_Assoc; Info : Internal_Entity_Info)
         return Internal_Entity_Dict_Assoc is
      begin
         if Node = null then
            return No_Entity_Dict_Assoc;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Dict_Assoc) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Dict_Assoc_List
        (Node : Bare_Dict_Assoc_List; Info : Internal_Entity_Info)
         return Internal_Entity_Dict_Assoc_List is
      begin
         if Node = null then
            return No_Entity_Dict_Assoc_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Dict_Assoc_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Dict_Comp
        (Node : Bare_Dict_Comp; Info : Internal_Entity_Info)
         return Internal_Entity_Dict_Comp is
      begin
         if Node = null then
            return No_Entity_Dict_Comp;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Dict_Comp) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Dict_Lit
        (Node : Bare_Dict_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Dict_Lit is
      begin
         if Node = null then
            return No_Entity_Dict_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Dict_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Dot
        (Node : Bare_Dot; Info : Internal_Entity_Info)
         return Internal_Entity_Dot is
      begin
         if Node = null then
            return No_Entity_Dot;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Dot) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Dot_List
        (Node : Bare_Dot_List; Info : Internal_Entity_Info)
         return Internal_Entity_Dot_List is
      begin
         if Node = null then
            return No_Entity_Dot_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Dot_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Name
        (Node : Bare_Name; Info : Internal_Entity_Info)
         return Internal_Entity_Name is
      begin
         if Node = null then
            return No_Entity_Name;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Name) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Dotted_Name
        (Node : Bare_Dotted_Name; Info : Internal_Entity_Info)
         return Internal_Entity_Dotted_Name is
      begin
         if Node = null then
            return No_Entity_Dotted_Name;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Dotted_Name) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Elif_Branch
        (Node : Bare_Elif_Branch; Info : Internal_Entity_Info)
         return Internal_Entity_Elif_Branch is
      begin
         if Node = null then
            return No_Entity_Elif_Branch;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Elif_Branch) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Elif_Branch_List
        (Node : Bare_Elif_Branch_List; Info : Internal_Entity_Info)
         return Internal_Entity_Elif_Branch_List is
      begin
         if Node = null then
            return No_Entity_Elif_Branch_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Elif_Branch_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ellipsis_Expr
        (Node : Bare_Ellipsis_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Ellipsis_Expr is
      begin
         if Node = null then
            return No_Entity_Ellipsis_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ellipsis_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Else_Part
        (Node : Bare_Else_Part; Info : Internal_Entity_Info)
         return Internal_Entity_Else_Part is
      begin
         if Node = null then
            return No_Entity_Else_Part;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Else_Part) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Except_Part
        (Node : Bare_Except_Part; Info : Internal_Entity_Info)
         return Internal_Entity_Except_Part is
      begin
         if Node = null then
            return No_Entity_Except_Part;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Except_Part) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Except_Part_List
        (Node : Bare_Except_Part_List; Info : Internal_Entity_Info)
         return Internal_Entity_Except_Part_List is
      begin
         if Node = null then
            return No_Entity_Except_Part_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Except_Part_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Exec_Stmt
        (Node : Bare_Exec_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Exec_Stmt is
      begin
         if Node = null then
            return No_Entity_Exec_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Exec_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Expr_List
        (Node : Bare_Expr_List; Info : Internal_Entity_Info)
         return Internal_Entity_Expr_List is
      begin
         if Node = null then
            return No_Entity_Expr_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Expr_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Slice_Expr
        (Node : Bare_Slice_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Slice_Expr is
      begin
         if Node = null then
            return No_Entity_Slice_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Slice_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ext_Slice_Expr
        (Node : Bare_Ext_Slice_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Ext_Slice_Expr is
      begin
         if Node = null then
            return No_Entity_Ext_Slice_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ext_Slice_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Factor
        (Node : Bare_Factor; Info : Internal_Entity_Info)
         return Internal_Entity_Factor is
      begin
         if Node = null then
            return No_Entity_Factor;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Factor) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_File_Node
        (Node : Bare_File_Node; Info : Internal_Entity_Info)
         return Internal_Entity_File_Node is
      begin
         if Node = null then
            return No_Entity_File_Node;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_File_Node) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_For_Stmt
        (Node : Bare_For_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_For_Stmt is
      begin
         if Node = null then
            return No_Entity_For_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_For_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Func_Def
        (Node : Bare_Func_Def; Info : Internal_Entity_Info)
         return Internal_Entity_Func_Def is
      begin
         if Node = null then
            return No_Entity_Func_Def;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Func_Def) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Global_Stmt
        (Node : Bare_Global_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Global_Stmt is
      begin
         if Node = null then
            return No_Entity_Global_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Global_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Id
        (Node : Bare_Id; Info : Internal_Entity_Info)
         return Internal_Entity_Id is
      begin
         if Node = null then
            return No_Entity_Id;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Id) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Id_List
        (Node : Bare_Id_List; Info : Internal_Entity_Info)
         return Internal_Entity_Id_List is
      begin
         if Node = null then
            return No_Entity_Id_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Id_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_If_Expr
        (Node : Bare_If_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_If_Expr is
      begin
         if Node = null then
            return No_Entity_If_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_If_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_If_Stmt
        (Node : Bare_If_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_If_Stmt is
      begin
         if Node = null then
            return No_Entity_If_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_If_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Import_From
        (Node : Bare_Import_From; Info : Internal_Entity_Info)
         return Internal_Entity_Import_From is
      begin
         if Node = null then
            return No_Entity_Import_From;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Import_From) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Import_Name
        (Node : Bare_Import_Name; Info : Internal_Entity_Info)
         return Internal_Entity_Import_Name is
      begin
         if Node = null then
            return No_Entity_Import_Name;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Import_Name) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Import_Star
        (Node : Bare_Import_Star; Info : Internal_Entity_Info)
         return Internal_Entity_Import_Star is
      begin
         if Node = null then
            return No_Entity_Import_Star;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Import_Star) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Inline_Eval
        (Node : Bare_Inline_Eval; Info : Internal_Entity_Info)
         return Internal_Entity_Inline_Eval is
      begin
         if Node = null then
            return No_Entity_Inline_Eval;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Inline_Eval) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Kw_Args
        (Node : Bare_Kw_Args; Info : Internal_Entity_Info)
         return Internal_Entity_Kw_Args is
      begin
         if Node = null then
            return No_Entity_Kw_Args;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Kw_Args) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Kw_Args_Flag
        (Node : Bare_Kw_Args_Flag; Info : Internal_Entity_Info)
         return Internal_Entity_Kw_Args_Flag is
      begin
         if Node = null then
            return No_Entity_Kw_Args_Flag;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Kw_Args_Flag) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Kw_Args_Flag_Absent
        (Node : Bare_Kw_Args_Flag_Absent; Info : Internal_Entity_Info)
         return Internal_Entity_Kw_Args_Flag_Absent is
      begin
         if Node = null then
            return No_Entity_Kw_Args_Flag_Absent;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Kw_Args_Flag_Absent) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Kw_Args_Flag_Present
        (Node : Bare_Kw_Args_Flag_Present; Info : Internal_Entity_Info)
         return Internal_Entity_Kw_Args_Flag_Present is
      begin
         if Node = null then
            return No_Entity_Kw_Args_Flag_Present;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Kw_Args_Flag_Present) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Lambda_Def
        (Node : Bare_Lambda_Def; Info : Internal_Entity_Info)
         return Internal_Entity_Lambda_Def is
      begin
         if Node = null then
            return No_Entity_Lambda_Def;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Lambda_Def) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_List_Comp
        (Node : Bare_List_Comp; Info : Internal_Entity_Info)
         return Internal_Entity_List_Comp is
      begin
         if Node = null then
            return No_Entity_List_Comp;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_List_Comp) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_List_Gen
        (Node : Bare_List_Gen; Info : Internal_Entity_Info)
         return Internal_Entity_List_Gen is
      begin
         if Node = null then
            return No_Entity_List_Gen;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_List_Gen) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_List_Lit
        (Node : Bare_List_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_List_Lit is
      begin
         if Node = null then
            return No_Entity_List_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_List_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_NL
        (Node : Bare_NL; Info : Internal_Entity_Info)
         return Internal_Entity_NL is
      begin
         if Node = null then
            return No_Entity_NL;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_NL) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_NL_List
        (Node : Bare_NL_List; Info : Internal_Entity_Info)
         return Internal_Entity_NL_List is
      begin
         if Node = null then
            return No_Entity_NL_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_NL_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Not_Op
        (Node : Bare_Not_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Not_Op is
      begin
         if Node = null then
            return No_Entity_Not_Op;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Not_Op) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Number_Lit
        (Node : Bare_Number_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Number_Lit is
      begin
         if Node = null then
            return No_Entity_Number_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Number_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op
        (Node : Bare_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Op is
      begin
         if Node = null then
            return No_Entity_Op;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Or_Expr
        (Node : Bare_Or_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Or_Expr is
      begin
         if Node = null then
            return No_Entity_Or_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Or_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Or_Op
        (Node : Bare_Or_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Or_Op is
      begin
         if Node = null then
            return No_Entity_Or_Op;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Or_Op) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Params
        (Node : Bare_Params; Info : Internal_Entity_Info)
         return Internal_Entity_Params is
      begin
         if Node = null then
            return No_Entity_Params;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Params) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Pass_Stmt
        (Node : Bare_Pass_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Pass_Stmt is
      begin
         if Node = null then
            return No_Entity_Pass_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Pass_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Power
        (Node : Bare_Power; Info : Internal_Entity_Info)
         return Internal_Entity_Power is
      begin
         if Node = null then
            return No_Entity_Power;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Power) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Print_Stmt
        (Node : Bare_Print_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Print_Stmt is
      begin
         if Node = null then
            return No_Entity_Print_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Print_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Raise_Stmt
        (Node : Bare_Raise_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Raise_Stmt is
      begin
         if Node = null then
            return No_Entity_Raise_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Raise_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Rel_Name
        (Node : Bare_Rel_Name; Info : Internal_Entity_Info)
         return Internal_Entity_Rel_Name is
      begin
         if Node = null then
            return No_Entity_Rel_Name;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Rel_Name) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Return_Stmt
        (Node : Bare_Return_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Return_Stmt is
      begin
         if Node = null then
            return No_Entity_Return_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Return_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Set_Comp
        (Node : Bare_Set_Comp; Info : Internal_Entity_Info)
         return Internal_Entity_Set_Comp is
      begin
         if Node = null then
            return No_Entity_Set_Comp;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Set_Comp) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Set_Lit
        (Node : Bare_Set_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Set_Lit is
      begin
         if Node = null then
            return No_Entity_Set_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Set_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Shift_Expr
        (Node : Bare_Shift_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Shift_Expr is
      begin
         if Node = null then
            return No_Entity_Shift_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Shift_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Single_Param
        (Node : Bare_Single_Param; Info : Internal_Entity_Info)
         return Internal_Entity_Single_Param is
      begin
         if Node = null then
            return No_Entity_Single_Param;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Single_Param) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Single_Param_List
        (Node : Bare_Single_Param_List; Info : Internal_Entity_Info)
         return Internal_Entity_Single_Param_List is
      begin
         if Node = null then
            return No_Entity_Single_Param_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Single_Param_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Stream_Print_Stmt
        (Node : Bare_Stream_Print_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Stream_Print_Stmt is
      begin
         if Node = null then
            return No_Entity_Stream_Print_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Stream_Print_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_String_Lit
        (Node : Bare_String_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_String_Lit is
      begin
         if Node = null then
            return No_Entity_String_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_String_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_String_Lit_List
        (Node : Bare_String_Lit_List; Info : Internal_Entity_Info)
         return Internal_Entity_String_Lit_List is
      begin
         if Node = null then
            return No_Entity_String_Lit_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_String_Lit_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Subscript_Expr
        (Node : Bare_Subscript_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Subscript_Expr is
      begin
         if Node = null then
            return No_Entity_Subscript_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Subscript_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Term
        (Node : Bare_Term; Info : Internal_Entity_Info)
         return Internal_Entity_Term is
      begin
         if Node = null then
            return No_Entity_Term;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Term) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Try_Stmt
        (Node : Bare_Try_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_Try_Stmt is
      begin
         if Node = null then
            return No_Entity_Try_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Try_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Tuple_Lit
        (Node : Bare_Tuple_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Tuple_Lit is
      begin
         if Node = null then
            return No_Entity_Tuple_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Tuple_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Turkixir_Node_List
        (Node : Bare_Turkixir_Node_List; Info : Internal_Entity_Info)
         return Internal_Entity_Turkixir_Node_List is
      begin
         if Node = null then
            return No_Entity_Turkixir_Node_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Turkixir_Node_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Var_Args
        (Node : Bare_Var_Args; Info : Internal_Entity_Info)
         return Internal_Entity_Var_Args is
      begin
         if Node = null then
            return No_Entity_Var_Args;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Var_Args) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Var_Args_Flag
        (Node : Bare_Var_Args_Flag; Info : Internal_Entity_Info)
         return Internal_Entity_Var_Args_Flag is
      begin
         if Node = null then
            return No_Entity_Var_Args_Flag;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Var_Args_Flag) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Var_Args_Flag_Absent
        (Node : Bare_Var_Args_Flag_Absent; Info : Internal_Entity_Info)
         return Internal_Entity_Var_Args_Flag_Absent is
      begin
         if Node = null then
            return No_Entity_Var_Args_Flag_Absent;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Var_Args_Flag_Absent) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Var_Args_Flag_Present
        (Node : Bare_Var_Args_Flag_Present; Info : Internal_Entity_Info)
         return Internal_Entity_Var_Args_Flag_Present is
      begin
         if Node = null then
            return No_Entity_Var_Args_Flag_Present;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Var_Args_Flag_Present) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_While_Stmt
        (Node : Bare_While_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_While_Stmt is
      begin
         if Node = null then
            return No_Entity_While_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_While_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_With_Stmt
        (Node : Bare_With_Stmt; Info : Internal_Entity_Info)
         return Internal_Entity_With_Stmt is
      begin
         if Node = null then
            return No_Entity_With_Stmt;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_With_Stmt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Xor_Expr
        (Node : Bare_Xor_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Xor_Expr is
      begin
         if Node = null then
            return No_Entity_Xor_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Xor_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Yield_Expr
        (Node : Bare_Yield_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Yield_Expr is
      begin
         if Node = null then
            return No_Entity_Yield_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Yield_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   


      -------------
      -- Inc_Ref --
      -------------

      procedure Inc_Ref (R : Internal_Env_Assoc) is
      begin
               Inc_Ref (R.Dest_Env);
      end Inc_Ref;

      -------------
      -- Dec_Ref --
      -------------

      procedure Dec_Ref (R : in out Internal_Env_Assoc) is
      begin
               Dec_Ref (R.Dest_Env);
      end Dec_Ref;




      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Env_Assoc) return Boolean is
      begin
         return L.Key = R.Key and then L.Val = R.Val and then Equivalent (L.Dest_Env, R.Dest_Env) and then L.Metadata = R.Metadata;
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Env_Assoc) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Key => " & Trace_Image (R.Key)
                        & ", "
                     & "Val => " & Trace_Image (R.Val)
                        & ", "
                     & "Dest_Env => " & Trace_Image (R.Dest_Env)
                        & ", "
                     & "Metadata => " & Trace_Image (R.Metadata)
               & ")");
      end Trace_Image;


   

   




   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Inner_Env_Assoc) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Key => " & Trace_Image (R.Key)
                        & ", "
                     & "Val => " & Trace_Image (R.Val)
                        & ", "
                     & "Metadata => " & Trace_Image (R.Metadata)
               & ")");
      end Trace_Image;



   

   pragma Warnings (Off, "referenced");
   type Logic_Converter_Default is null record;
   No_Logic_Converter_Default : constant Logic_Converter_Default :=
     (null record);

   function Convert
     (Self : Logic_Converter_Default;
      From : Internal_Entity) return Internal_Entity
   is
      pragma Unreferenced (Self);
   begin
      return From;
   end Convert;

   type Equals_Data_Default is null record;
   No_Equals_Data_Default : constant Equals_Data_Default := (null record);

   function Eq_Default
     (Data : Equals_Data_Default; L, R : Internal_Entity) return Boolean
   is (Equivalent (L, R))
      with Inline;
   pragma Warnings (On, "referenced");


         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         


   


      

   --
   --  Primitives for Bare_Turkixir_Node
   --

   







   


      

   --
   --  Primitives for Bare_Arg
   --

   







   


      

   --
   --  Primitives for Bare_Arg_Assoc
   --

   



      
      procedure Initialize_Fields_For_Arg_Assoc
        (Self : Bare_Arg_Assoc
         ; Arg_Assoc_F_Name : Bare_Expr
         ; Arg_Assoc_F_Expr : Bare_Expr
        ) is
      begin

            Self.Arg_Assoc_F_Name := Arg_Assoc_F_Name;
            Self.Arg_Assoc_F_Expr := Arg_Assoc_F_Expr;
         

      end Initialize_Fields_For_Arg_Assoc;

      
   function Arg_Assoc_F_Name
     (Node : Bare_Arg_Assoc) return Bare_Expr
   is
      

   begin
         
         return Node.Arg_Assoc_F_Name;
      
   end;

      
   function Arg_Assoc_F_Expr
     (Node : Bare_Arg_Assoc) return Bare_Expr
   is
      

   begin
         
         return Node.Arg_Assoc_F_Expr;
      
   end;




   


      

   --
   --  Primitives for Bare_Arg_Gen
   --

   



      
      procedure Initialize_Fields_For_Arg_Gen
        (Self : Bare_Arg_Gen
         ; Arg_Gen_F_Expr : Bare_Expr
         ; Arg_Gen_F_Comprehension : Bare_Comp_For
        ) is
      begin

            Self.Arg_Gen_F_Expr := Arg_Gen_F_Expr;
            Self.Arg_Gen_F_Comprehension := Arg_Gen_F_Comprehension;
         

      end Initialize_Fields_For_Arg_Gen;

      
   function Arg_Gen_F_Expr
     (Node : Bare_Arg_Gen) return Bare_Expr
   is
      

   begin
         
         return Node.Arg_Gen_F_Expr;
      
   end;

      
   function Arg_Gen_F_Comprehension
     (Node : Bare_Arg_Gen) return Bare_Comp_For
   is
      

   begin
         
         return Node.Arg_Gen_F_Comprehension;
      
   end;




   


      

   --
   --  Primitives for Bare_Kw_Args
   --

   



      
      procedure Initialize_Fields_For_Kw_Args
        (Self : Bare_Kw_Args
         ; Kw_Args_F_Expr : Bare_Expr
        ) is
      begin

            Self.Kw_Args_F_Expr := Kw_Args_F_Expr;
         

      end Initialize_Fields_For_Kw_Args;

      
   function Kw_Args_F_Expr
     (Node : Bare_Kw_Args) return Bare_Expr
   is
      

   begin
         
         return Node.Kw_Args_F_Expr;
      
   end;




   


      

   --
   --  Primitives for Bare_Var_Args
   --

   



      
      procedure Initialize_Fields_For_Var_Args
        (Self : Bare_Var_Args
         ; Var_Args_F_Expr : Bare_Expr
        ) is
      begin

            Self.Var_Args_F_Expr := Var_Args_F_Expr;
         

      end Initialize_Fields_For_Var_Args;

      
   function Var_Args_F_Expr
     (Node : Bare_Var_Args) return Bare_Expr
   is
      

   begin
         
         return Node.Var_Args_F_Expr;
      
   end;




   


      

   --
   --  Primitives for Bare_As_Name_Node
   --

   



      
      procedure Initialize_Fields_For_As_Name_Node
        (Self : Bare_As_Name_Node
         ; As_Name_Node_F_Imported : Bare_Expr
         ; As_Name_Node_F_As_Name : Bare_Expr
        ) is
      begin

            Self.As_Name_Node_F_Imported := As_Name_Node_F_Imported;
            Self.As_Name_Node_F_As_Name := As_Name_Node_F_As_Name;
         

      end Initialize_Fields_For_As_Name_Node;

      
   function As_Name_Node_F_Imported
     (Node : Bare_As_Name_Node) return Bare_Expr
   is
      

   begin
         
         return Node.As_Name_Node_F_Imported;
      
   end;

      
   function As_Name_Node_F_As_Name
     (Node : Bare_As_Name_Node) return Bare_Expr
   is
      

   begin
         
         return Node.As_Name_Node_F_As_Name;
      
   end;




   


      

   --
   --  Primitives for Bare_Comp_If
   --

   



      
      procedure Initialize_Fields_For_Comp_If
        (Self : Bare_Comp_If
         ; Comp_If_F_Test : Bare_Expr
         ; Comp_If_F_Comp : Bare_Turkixir_Node
        ) is
      begin

            Self.Comp_If_F_Test := Comp_If_F_Test;
            Self.Comp_If_F_Comp := Comp_If_F_Comp;
         

      end Initialize_Fields_For_Comp_If;

      
   function Comp_If_F_Test
     (Node : Bare_Comp_If) return Bare_Expr
   is
      

   begin
         
         return Node.Comp_If_F_Test;
      
   end;

      
   function Comp_If_F_Comp
     (Node : Bare_Comp_If) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.Comp_If_F_Comp;
      
   end;




   


      

   --
   --  Primitives for Bare_Comp_Op_Kind
   --

   







   


      

   --
   --  Primitives for Bare_Comp_Op_Kind_Diamond
   --

   







   


      

   --
   --  Primitives for Bare_Comp_Op_Kind_Eq
   --

   







   


      

   --
   --  Primitives for Bare_Comp_Op_Kind_Gt
   --

   







   


      

   --
   --  Primitives for Bare_Comp_Op_Kind_Gte
   --

   







   


      

   --
   --  Primitives for Bare_Comp_Op_Kind_In
   --

   







   


      

   --
   --  Primitives for Bare_Comp_Op_Kind_Is
   --

   







   


      

   --
   --  Primitives for Bare_Comp_Op_Kind_Isnot
   --

   







   


      

   --
   --  Primitives for Bare_Comp_Op_Kind_Lt
   --

   







   


      

   --
   --  Primitives for Bare_Comp_Op_Kind_Lte
   --

   







   


      

   --
   --  Primitives for Bare_Comp_Op_Kind_Noteq
   --

   







   


      

   --
   --  Primitives for Bare_Comp_Op_Kind_Notin
   --

   







   


      

   --
   --  Primitives for Bare_Comprehension
   --

   







   


      

   --
   --  Primitives for Bare_Comp_For
   --

   



      
      procedure Initialize_Fields_For_Comp_For
        (Self : Bare_Comp_For
         ; Comp_For_F_Exprs : Bare_Expr_List
         ; Comp_For_F_Target : Bare_Expr
         ; Comp_For_F_Comp : Bare_Turkixir_Node
        ) is
      begin

            Self.Comp_For_F_Exprs := Comp_For_F_Exprs;
            Self.Comp_For_F_Target := Comp_For_F_Target;
            Self.Comp_For_F_Comp := Comp_For_F_Comp;
         

      end Initialize_Fields_For_Comp_For;

      
   function Comp_For_F_Exprs
     (Node : Bare_Comp_For) return Bare_Expr_List
   is
      

   begin
         
         return Node.Comp_For_F_Exprs;
      
   end;

      
   function Comp_For_F_Target
     (Node : Bare_Comp_For) return Bare_Expr
   is
      

   begin
         
         return Node.Comp_For_F_Target;
      
   end;

      
   function Comp_For_F_Comp
     (Node : Bare_Comp_For) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.Comp_For_F_Comp;
      
   end;




   


      

   --
   --  Primitives for Bare_Comp_ForL
   --

   



      
      procedure Initialize_Fields_For_Comp_ForL
        (Self : Bare_Comp_ForL
         ; Comp_ForL_F_Exprs : Bare_Expr_List
         ; Comp_ForL_F_Target : Bare_Expr_List
         ; Comp_ForL_F_Comp : Bare_Turkixir_Node
        ) is
      begin

            Self.Comp_ForL_F_Exprs := Comp_ForL_F_Exprs;
            Self.Comp_ForL_F_Target := Comp_ForL_F_Target;
            Self.Comp_ForL_F_Comp := Comp_ForL_F_Comp;
         

      end Initialize_Fields_For_Comp_ForL;

      
   function Comp_ForL_F_Exprs
     (Node : Bare_Comp_ForL) return Bare_Expr_List
   is
      

   begin
         
         return Node.Comp_ForL_F_Exprs;
      
   end;

      
   function Comp_ForL_F_Target
     (Node : Bare_Comp_ForL) return Bare_Expr_List
   is
      

   begin
         
         return Node.Comp_ForL_F_Target;
      
   end;

      
   function Comp_ForL_F_Comp
     (Node : Bare_Comp_ForL) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.Comp_ForL_F_Comp;
      
   end;




   


      

   --
   --  Primitives for Bare_Decorator
   --

   



      
      procedure Initialize_Fields_For_Decorator
        (Self : Bare_Decorator
         ; Decorator_F_Dec_Name : Bare_Name
         ; Decorator_F_Arg_List : Bare_Arg_List
        ) is
      begin

            Self.Decorator_F_Dec_Name := Decorator_F_Dec_Name;
            Self.Decorator_F_Arg_List := Decorator_F_Arg_List;
         

      end Initialize_Fields_For_Decorator;

      
   function Decorator_F_Dec_Name
     (Node : Bare_Decorator) return Bare_Name
   is
      

   begin
         
         return Node.Decorator_F_Dec_Name;
      
   end;

      
   function Decorator_F_Arg_List
     (Node : Bare_Decorator) return Bare_Arg_List
   is
      

   begin
         
         return Node.Decorator_F_Arg_List;
      
   end;




   


      

   --
   --  Primitives for Bare_Dict_Assoc
   --

   



      
      procedure Initialize_Fields_For_Dict_Assoc
        (Self : Bare_Dict_Assoc
         ; Dict_Assoc_F_Key : Bare_Expr
         ; Dict_Assoc_F_Value : Bare_Expr
        ) is
      begin

            Self.Dict_Assoc_F_Key := Dict_Assoc_F_Key;
            Self.Dict_Assoc_F_Value := Dict_Assoc_F_Value;
         

      end Initialize_Fields_For_Dict_Assoc;

      
   function Dict_Assoc_F_Key
     (Node : Bare_Dict_Assoc) return Bare_Expr
   is
      

   begin
         
         return Node.Dict_Assoc_F_Key;
      
   end;

      
   function Dict_Assoc_F_Value
     (Node : Bare_Dict_Assoc) return Bare_Expr
   is
      

   begin
         
         return Node.Dict_Assoc_F_Value;
      
   end;




   


      

   --
   --  Primitives for Bare_Else_Part
   --

   



      
      procedure Initialize_Fields_For_Else_Part
        (Self : Bare_Else_Part
         ; Else_Part_F_Statements : Bare_Turkixir_Node
        ) is
      begin

            Self.Else_Part_F_Statements := Else_Part_F_Statements;
         

      end Initialize_Fields_For_Else_Part;

      
   function Else_Part_F_Statements
     (Node : Bare_Else_Part) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.Else_Part_F_Statements;
      
   end;




   


      

   --
   --  Primitives for Bare_Except_Part
   --

   



      
      procedure Initialize_Fields_For_Except_Part
        (Self : Bare_Except_Part
         ; Except_Part_F_As_Name : Bare_As_Name_Node
         ; Except_Part_F_Statements : Bare_Turkixir_Node
        ) is
      begin

            Self.Except_Part_F_As_Name := Except_Part_F_As_Name;
            Self.Except_Part_F_Statements := Except_Part_F_Statements;
         

      end Initialize_Fields_For_Except_Part;

      
   function Except_Part_F_As_Name
     (Node : Bare_Except_Part) return Bare_As_Name_Node
   is
      

   begin
         
         return Node.Except_Part_F_As_Name;
      
   end;

      
   function Except_Part_F_Statements
     (Node : Bare_Except_Part) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.Except_Part_F_Statements;
      
   end;




   


      

   --
   --  Primitives for Bare_Expr
   --

   







   


      

   --
   --  Primitives for Bare_And_Expr
   --

   



      
      procedure Initialize_Fields_For_And_Expr
        (Self : Bare_And_Expr
         ; And_Expr_F_Left : Bare_Expr
         ; And_Expr_F_Right : Bare_Expr
        ) is
      begin

            Self.And_Expr_F_Left := And_Expr_F_Left;
            Self.And_Expr_F_Right := And_Expr_F_Right;
         

      end Initialize_Fields_For_And_Expr;

      
   function And_Expr_F_Left
     (Node : Bare_And_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.And_Expr_F_Left;
      
   end;

      
   function And_Expr_F_Right
     (Node : Bare_And_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.And_Expr_F_Right;
      
   end;




   


      

   --
   --  Primitives for Bare_And_Op
   --

   



      
      procedure Initialize_Fields_For_And_Op
        (Self : Bare_And_Op
         ; And_Op_F_Left : Bare_Expr
         ; And_Op_F_Right : Bare_Expr
        ) is
      begin

            Self.And_Op_F_Left := And_Op_F_Left;
            Self.And_Op_F_Right := And_Op_F_Right;
         

      end Initialize_Fields_For_And_Op;

      
   function And_Op_F_Left
     (Node : Bare_And_Op) return Bare_Expr
   is
      

   begin
         
         return Node.And_Op_F_Left;
      
   end;

      
   function And_Op_F_Right
     (Node : Bare_And_Op) return Bare_Expr
   is
      

   begin
         
         return Node.And_Op_F_Right;
      
   end;




   


      

   --
   --  Primitives for Bare_Bin_Op
   --

   



      
      procedure Initialize_Fields_For_Bin_Op
        (Self : Bare_Bin_Op
         ; Bin_Op_F_Left : Bare_Expr
         ; Bin_Op_F_Op : Bare_Op
         ; Bin_Op_F_Right : Bare_Expr
        ) is
      begin

            Self.Bin_Op_F_Left := Bin_Op_F_Left;
            Self.Bin_Op_F_Op := Bin_Op_F_Op;
            Self.Bin_Op_F_Right := Bin_Op_F_Right;
         

      end Initialize_Fields_For_Bin_Op;

      
   function Bin_Op_F_Left
     (Node : Bare_Bin_Op) return Bare_Expr
   is
      

   begin
         
         return Node.Bin_Op_F_Left;
      
   end;

      
   function Bin_Op_F_Op
     (Node : Bare_Bin_Op) return Bare_Op
   is
      

   begin
         
         return Node.Bin_Op_F_Op;
      
   end;

      
   function Bin_Op_F_Right
     (Node : Bare_Bin_Op) return Bare_Expr
   is
      

   begin
         
         return Node.Bin_Op_F_Right;
      
   end;




   


      

   --
   --  Primitives for Bare_Arith_Expr
   --

   



      
      procedure Initialize_Fields_For_Arith_Expr
        (Self : Bare_Arith_Expr
         ; Bin_Op_F_Left : Bare_Expr
         ; Bin_Op_F_Op : Bare_Op
         ; Bin_Op_F_Right : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Bin_Op
              (Self, Bin_Op_F_Left, Bin_Op_F_Op, Bin_Op_F_Right);

         

      end Initialize_Fields_For_Arith_Expr;




   


      

   --
   --  Primitives for Bare_Shift_Expr
   --

   



      
      procedure Initialize_Fields_For_Shift_Expr
        (Self : Bare_Shift_Expr
         ; Bin_Op_F_Left : Bare_Expr
         ; Bin_Op_F_Op : Bare_Op
         ; Bin_Op_F_Right : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Bin_Op
              (Self, Bin_Op_F_Left, Bin_Op_F_Op, Bin_Op_F_Right);

         

      end Initialize_Fields_For_Shift_Expr;




   


      

   --
   --  Primitives for Bare_Term
   --

   



      
      procedure Initialize_Fields_For_Term
        (Self : Bare_Term
         ; Bin_Op_F_Left : Bare_Expr
         ; Bin_Op_F_Op : Bare_Op
         ; Bin_Op_F_Right : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Bin_Op
              (Self, Bin_Op_F_Left, Bin_Op_F_Op, Bin_Op_F_Right);

         

      end Initialize_Fields_For_Term;




   


      

   --
   --  Primitives for Bare_Call_Expr
   --

   



      
      procedure Initialize_Fields_For_Call_Expr
        (Self : Bare_Call_Expr
         ; Call_Expr_F_Prefix : Bare_Expr
         ; Call_Expr_F_Suffix : Bare_Arg_List
        ) is
      begin

            Self.Call_Expr_F_Prefix := Call_Expr_F_Prefix;
            Self.Call_Expr_F_Suffix := Call_Expr_F_Suffix;
         

      end Initialize_Fields_For_Call_Expr;

      
   function Call_Expr_F_Prefix
     (Node : Bare_Call_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Call_Expr_F_Prefix;
      
   end;

      
   function Call_Expr_F_Suffix
     (Node : Bare_Call_Expr) return Bare_Arg_List
   is
      

   begin
         
         return Node.Call_Expr_F_Suffix;
      
   end;




   


      

   --
   --  Primitives for Bare_Comp_Op
   --

   



      
      procedure Initialize_Fields_For_Comp_Op
        (Self : Bare_Comp_Op
         ; Comp_Op_F_Left : Bare_Expr
         ; Comp_Op_F_Op : Bare_Comp_Op_Kind
         ; Comp_Op_F_Right : Bare_Expr
        ) is
      begin

            Self.Comp_Op_F_Left := Comp_Op_F_Left;
            Self.Comp_Op_F_Op := Comp_Op_F_Op;
            Self.Comp_Op_F_Right := Comp_Op_F_Right;
         

      end Initialize_Fields_For_Comp_Op;

      
   function Comp_Op_F_Left
     (Node : Bare_Comp_Op) return Bare_Expr
   is
      

   begin
         
         return Node.Comp_Op_F_Left;
      
   end;

      
   function Comp_Op_F_Op
     (Node : Bare_Comp_Op) return Bare_Comp_Op_Kind
   is
      

   begin
         
         return Node.Comp_Op_F_Op;
      
   end;

      
   function Comp_Op_F_Right
     (Node : Bare_Comp_Op) return Bare_Expr
   is
      

   begin
         
         return Node.Comp_Op_F_Right;
      
   end;




   


      

   --
   --  Primitives for Bare_Concat_String_Lit
   --

   



      
      procedure Initialize_Fields_For_Concat_String_Lit
        (Self : Bare_Concat_String_Lit
         ; Concat_String_Lit_F_First_Str : Bare_String_Lit
         ; Concat_String_Lit_F_Subsequent_Str : Bare_String_Lit_List
        ) is
      begin

            Self.Concat_String_Lit_F_First_Str := Concat_String_Lit_F_First_Str;
            Self.Concat_String_Lit_F_Subsequent_Str := Concat_String_Lit_F_Subsequent_Str;
         

      end Initialize_Fields_For_Concat_String_Lit;

      
   function Concat_String_Lit_F_First_Str
     (Node : Bare_Concat_String_Lit) return Bare_String_Lit
   is
      

   begin
         
         return Node.Concat_String_Lit_F_First_Str;
      
   end;

      
   function Concat_String_Lit_F_Subsequent_Str
     (Node : Bare_Concat_String_Lit) return Bare_String_Lit_List
   is
      

   begin
         
         return Node.Concat_String_Lit_F_Subsequent_Str;
      
   end;




   


      

   --
   --  Primitives for Bare_Dict_Comp
   --

   



      
      procedure Initialize_Fields_For_Dict_Comp
        (Self : Bare_Dict_Comp
         ; Dict_Comp_F_Assoc : Bare_Dict_Assoc
         ; Dict_Comp_F_Comprehension : Bare_Comp_For
        ) is
      begin

            Self.Dict_Comp_F_Assoc := Dict_Comp_F_Assoc;
            Self.Dict_Comp_F_Comprehension := Dict_Comp_F_Comprehension;
         

      end Initialize_Fields_For_Dict_Comp;

      
   function Dict_Comp_F_Assoc
     (Node : Bare_Dict_Comp) return Bare_Dict_Assoc
   is
      

   begin
         
         return Node.Dict_Comp_F_Assoc;
      
   end;

      
   function Dict_Comp_F_Comprehension
     (Node : Bare_Dict_Comp) return Bare_Comp_For
   is
      

   begin
         
         return Node.Dict_Comp_F_Comprehension;
      
   end;




   


      

   --
   --  Primitives for Bare_Dict_Lit
   --

   



      
      procedure Initialize_Fields_For_Dict_Lit
        (Self : Bare_Dict_Lit
         ; Dict_Lit_F_Assocs : Bare_Dict_Assoc_List
        ) is
      begin

            Self.Dict_Lit_F_Assocs := Dict_Lit_F_Assocs;
         

      end Initialize_Fields_For_Dict_Lit;

      
   function Dict_Lit_F_Assocs
     (Node : Bare_Dict_Lit) return Bare_Dict_Assoc_List
   is
      

   begin
         
         return Node.Dict_Lit_F_Assocs;
      
   end;




   


      

   --
   --  Primitives for Bare_Dot
   --

   







   


      

   --
   --  Primitives for Bare_Ellipsis_Expr
   --

   







   


      

   --
   --  Primitives for Bare_Factor
   --

   



      
      procedure Initialize_Fields_For_Factor
        (Self : Bare_Factor
         ; Factor_F_Op : Bare_Op
         ; Factor_F_Expr : Bare_Expr
        ) is
      begin

            Self.Factor_F_Op := Factor_F_Op;
            Self.Factor_F_Expr := Factor_F_Expr;
         

      end Initialize_Fields_For_Factor;

      
   function Factor_F_Op
     (Node : Bare_Factor) return Bare_Op
   is
      

   begin
         
         return Node.Factor_F_Op;
      
   end;

      
   function Factor_F_Expr
     (Node : Bare_Factor) return Bare_Expr
   is
      

   begin
         
         return Node.Factor_F_Expr;
      
   end;




   


      

   --
   --  Primitives for Bare_If_Expr
   --

   



      
      procedure Initialize_Fields_For_If_Expr
        (Self : Bare_If_Expr
         ; If_Expr_F_Expr : Bare_Expr
         ; If_Expr_F_Cond : Bare_Expr
         ; If_Expr_F_Else_Expr : Bare_Expr
        ) is
      begin

            Self.If_Expr_F_Expr := If_Expr_F_Expr;
            Self.If_Expr_F_Cond := If_Expr_F_Cond;
            Self.If_Expr_F_Else_Expr := If_Expr_F_Else_Expr;
         

      end Initialize_Fields_For_If_Expr;

      
   function If_Expr_F_Expr
     (Node : Bare_If_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.If_Expr_F_Expr;
      
   end;

      
   function If_Expr_F_Cond
     (Node : Bare_If_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.If_Expr_F_Cond;
      
   end;

      
   function If_Expr_F_Else_Expr
     (Node : Bare_If_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.If_Expr_F_Else_Expr;
      
   end;




   


      

   --
   --  Primitives for Bare_Inline_Eval
   --

   



      
      procedure Initialize_Fields_For_Inline_Eval
        (Self : Bare_Inline_Eval
         ; Inline_Eval_F_Exprs : Bare_Expr_List
        ) is
      begin

            Self.Inline_Eval_F_Exprs := Inline_Eval_F_Exprs;
         

      end Initialize_Fields_For_Inline_Eval;

      
   function Inline_Eval_F_Exprs
     (Node : Bare_Inline_Eval) return Bare_Expr_List
   is
      

   begin
         
         return Node.Inline_Eval_F_Exprs;
      
   end;




   


      

   --
   --  Primitives for Bare_Lambda_Def
   --

   



      
      procedure Initialize_Fields_For_Lambda_Def
        (Self : Bare_Lambda_Def
         ; Lambda_Def_F_Args : Bare_Params
         ; Lambda_Def_F_Expr : Bare_Expr
        ) is
      begin

            Self.Lambda_Def_F_Args := Lambda_Def_F_Args;
            Self.Lambda_Def_F_Expr := Lambda_Def_F_Expr;
         

      end Initialize_Fields_For_Lambda_Def;

      
   function Lambda_Def_F_Args
     (Node : Bare_Lambda_Def) return Bare_Params
   is
      

   begin
         
         return Node.Lambda_Def_F_Args;
      
   end;

      
   function Lambda_Def_F_Expr
     (Node : Bare_Lambda_Def) return Bare_Expr
   is
      

   begin
         
         return Node.Lambda_Def_F_Expr;
      
   end;




   


      

   --
   --  Primitives for Bare_List_Comp
   --

   



      
      procedure Initialize_Fields_For_List_Comp
        (Self : Bare_List_Comp
         ; List_Comp_F_Expr : Bare_Expr
         ; List_Comp_F_Comprehension : Bare_Comp_ForL
        ) is
      begin

            Self.List_Comp_F_Expr := List_Comp_F_Expr;
            Self.List_Comp_F_Comprehension := List_Comp_F_Comprehension;
         

      end Initialize_Fields_For_List_Comp;

      
   function List_Comp_F_Expr
     (Node : Bare_List_Comp) return Bare_Expr
   is
      

   begin
         
         return Node.List_Comp_F_Expr;
      
   end;

      
   function List_Comp_F_Comprehension
     (Node : Bare_List_Comp) return Bare_Comp_ForL
   is
      

   begin
         
         return Node.List_Comp_F_Comprehension;
      
   end;




   


      

   --
   --  Primitives for Bare_List_Gen
   --

   



      
      procedure Initialize_Fields_For_List_Gen
        (Self : Bare_List_Gen
         ; List_Gen_F_Expr : Bare_Expr
         ; List_Gen_F_Comprehension : Bare_Comp_ForL
        ) is
      begin

            Self.List_Gen_F_Expr := List_Gen_F_Expr;
            Self.List_Gen_F_Comprehension := List_Gen_F_Comprehension;
         

      end Initialize_Fields_For_List_Gen;

      
   function List_Gen_F_Expr
     (Node : Bare_List_Gen) return Bare_Expr
   is
      

   begin
         
         return Node.List_Gen_F_Expr;
      
   end;

      
   function List_Gen_F_Comprehension
     (Node : Bare_List_Gen) return Bare_Comp_ForL
   is
      

   begin
         
         return Node.List_Gen_F_Comprehension;
      
   end;




   


      

   --
   --  Primitives for Bare_List_Lit
   --

   



      
      procedure Initialize_Fields_For_List_Lit
        (Self : Bare_List_Lit
         ; List_Lit_F_Exprs : Bare_Expr_List
        ) is
      begin

            Self.List_Lit_F_Exprs := List_Lit_F_Exprs;
         

      end Initialize_Fields_For_List_Lit;

      
   function List_Lit_F_Exprs
     (Node : Bare_List_Lit) return Bare_Expr_List
   is
      

   begin
         
         return Node.List_Lit_F_Exprs;
      
   end;




   


      

   --
   --  Primitives for Bare_Name
   --

   







   


      

   --
   --  Primitives for Bare_Dotted_Name
   --

   



      
      procedure Initialize_Fields_For_Dotted_Name
        (Self : Bare_Dotted_Name
         ; Dotted_Name_F_Prefix : Bare_Expr
         ; Dotted_Name_F_Suffix : Bare_Id
        ) is
      begin

            Self.Dotted_Name_F_Prefix := Dotted_Name_F_Prefix;
            Self.Dotted_Name_F_Suffix := Dotted_Name_F_Suffix;
         

      end Initialize_Fields_For_Dotted_Name;

      
   function Dotted_Name_F_Prefix
     (Node : Bare_Dotted_Name) return Bare_Expr
   is
      

   begin
         
         return Node.Dotted_Name_F_Prefix;
      
   end;

      
   function Dotted_Name_F_Suffix
     (Node : Bare_Dotted_Name) return Bare_Id
   is
      

   begin
         
         return Node.Dotted_Name_F_Suffix;
      
   end;




   


      

   --
   --  Primitives for Bare_Id
   --

   





   







--# property-start Id.sym 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:441'
pragma Warnings (Off, "is not referenced");
 function Id_P_Sym
  
  (Node : Bare_Id
  )

   return Symbol_Type
is
   Self : Bare_Id := Bare_Id (Node);

   Call_Depth : aliased Natural;

   

   --# bind self Self


   Property_Result : Symbol_Type;

      

      Sym : Symbol_Type;



begin
   --# property-body-start


   if Self /= null then
      Enter_Call (Self.Unit.Context, Call_Depth'Access);
   end if;




      
   --# scope-start

      --# expr-start 1 '<GetSymbol>' Sym 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:442'

Sym := Get_Symbol (Self); 
--# expr-done 1

      Property_Result := Sym;
      
   --# end




   if Self /= null then
      Exit_Call (Self.Unit.Context, Call_Depth);
   end if;
   return Property_Result;

exception


   when others =>
      if Self /= null then
         Exit_Call (Self.Unit.Context, Call_Depth);
      end if;
      raise;

end Id_P_Sym;
--# end



   


      

   --
   --  Primitives for Bare_Not_Op
   --

   



      
      procedure Initialize_Fields_For_Not_Op
        (Self : Bare_Not_Op
         ; Not_Op_F_Expr : Bare_Expr
        ) is
      begin

            Self.Not_Op_F_Expr := Not_Op_F_Expr;
         

      end Initialize_Fields_For_Not_Op;

      
   function Not_Op_F_Expr
     (Node : Bare_Not_Op) return Bare_Expr
   is
      

   begin
         
         return Node.Not_Op_F_Expr;
      
   end;




   


      

   --
   --  Primitives for Bare_Number_Lit
   --

   







   


      

   --
   --  Primitives for Bare_Or_Expr
   --

   



      
      procedure Initialize_Fields_For_Or_Expr
        (Self : Bare_Or_Expr
         ; Or_Expr_F_Left : Bare_Expr
         ; Or_Expr_F_Right : Bare_Expr
        ) is
      begin

            Self.Or_Expr_F_Left := Or_Expr_F_Left;
            Self.Or_Expr_F_Right := Or_Expr_F_Right;
         

      end Initialize_Fields_For_Or_Expr;

      
   function Or_Expr_F_Left
     (Node : Bare_Or_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Or_Expr_F_Left;
      
   end;

      
   function Or_Expr_F_Right
     (Node : Bare_Or_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Or_Expr_F_Right;
      
   end;




   


      

   --
   --  Primitives for Bare_Or_Op
   --

   



      
      procedure Initialize_Fields_For_Or_Op
        (Self : Bare_Or_Op
         ; Or_Op_F_Left : Bare_Expr
         ; Or_Op_F_Right : Bare_Expr
        ) is
      begin

            Self.Or_Op_F_Left := Or_Op_F_Left;
            Self.Or_Op_F_Right := Or_Op_F_Right;
         

      end Initialize_Fields_For_Or_Op;

      
   function Or_Op_F_Left
     (Node : Bare_Or_Op) return Bare_Expr
   is
      

   begin
         
         return Node.Or_Op_F_Left;
      
   end;

      
   function Or_Op_F_Right
     (Node : Bare_Or_Op) return Bare_Expr
   is
      

   begin
         
         return Node.Or_Op_F_Right;
      
   end;




   


      

   --
   --  Primitives for Bare_Power
   --

   



      
      procedure Initialize_Fields_For_Power
        (Self : Bare_Power
         ; Power_F_Left : Bare_Expr
         ; Power_F_Right : Bare_Expr
        ) is
      begin

            Self.Power_F_Left := Power_F_Left;
            Self.Power_F_Right := Power_F_Right;
         

      end Initialize_Fields_For_Power;

      
   function Power_F_Left
     (Node : Bare_Power) return Bare_Expr
   is
      

   begin
         
         return Node.Power_F_Left;
      
   end;

      
   function Power_F_Right
     (Node : Bare_Power) return Bare_Expr
   is
      

   begin
         
         return Node.Power_F_Right;
      
   end;




   


      

   --
   --  Primitives for Bare_Set_Comp
   --

   



      
      procedure Initialize_Fields_For_Set_Comp
        (Self : Bare_Set_Comp
         ; Set_Comp_F_Expr : Bare_Expr
         ; Set_Comp_F_Comprehension : Bare_Comp_For
        ) is
      begin

            Self.Set_Comp_F_Expr := Set_Comp_F_Expr;
            Self.Set_Comp_F_Comprehension := Set_Comp_F_Comprehension;
         

      end Initialize_Fields_For_Set_Comp;

      
   function Set_Comp_F_Expr
     (Node : Bare_Set_Comp) return Bare_Expr
   is
      

   begin
         
         return Node.Set_Comp_F_Expr;
      
   end;

      
   function Set_Comp_F_Comprehension
     (Node : Bare_Set_Comp) return Bare_Comp_For
   is
      

   begin
         
         return Node.Set_Comp_F_Comprehension;
      
   end;




   


      

   --
   --  Primitives for Bare_Set_Lit
   --

   



      
      procedure Initialize_Fields_For_Set_Lit
        (Self : Bare_Set_Lit
         ; Set_Lit_F_Exprs : Bare_Expr_List
        ) is
      begin

            Self.Set_Lit_F_Exprs := Set_Lit_F_Exprs;
         

      end Initialize_Fields_For_Set_Lit;

      
   function Set_Lit_F_Exprs
     (Node : Bare_Set_Lit) return Bare_Expr_List
   is
      

   begin
         
         return Node.Set_Lit_F_Exprs;
      
   end;




   


      

   --
   --  Primitives for Bare_Slice_Expr
   --

   



      
      procedure Initialize_Fields_For_Slice_Expr
        (Self : Bare_Slice_Expr
         ; Slice_Expr_F_First : Bare_Expr
         ; Slice_Expr_F_Last : Bare_Expr
        ) is
      begin

            Self.Slice_Expr_F_First := Slice_Expr_F_First;
            Self.Slice_Expr_F_Last := Slice_Expr_F_Last;
         

      end Initialize_Fields_For_Slice_Expr;

      
   function Slice_Expr_F_First
     (Node : Bare_Slice_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Slice_Expr_F_First;
      
   end;

      
   function Slice_Expr_F_Last
     (Node : Bare_Slice_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Slice_Expr_F_Last;
      
   end;




   


      

   --
   --  Primitives for Bare_Ext_Slice_Expr
   --

   



      
      procedure Initialize_Fields_For_Ext_Slice_Expr
        (Self : Bare_Ext_Slice_Expr
         ; Slice_Expr_F_First : Bare_Expr
         ; Slice_Expr_F_Last : Bare_Expr
         ; Ext_Slice_Expr_F_Stride : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Slice_Expr
              (Self, Slice_Expr_F_First, Slice_Expr_F_Last);

            Self.Ext_Slice_Expr_F_Stride := Ext_Slice_Expr_F_Stride;
         

      end Initialize_Fields_For_Ext_Slice_Expr;

      
   function Ext_Slice_Expr_F_Stride
     (Node : Bare_Ext_Slice_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Ext_Slice_Expr_F_Stride;
      
   end;




   


      

   --
   --  Primitives for Bare_String_Lit
   --

   







   


      

   --
   --  Primitives for Bare_Subscript_Expr
   --

   



      
      procedure Initialize_Fields_For_Subscript_Expr
        (Self : Bare_Subscript_Expr
         ; Subscript_Expr_F_Prefix : Bare_Expr
         ; Subscript_Expr_F_Suffix : Bare_Expr_List
        ) is
      begin

            Self.Subscript_Expr_F_Prefix := Subscript_Expr_F_Prefix;
            Self.Subscript_Expr_F_Suffix := Subscript_Expr_F_Suffix;
         

      end Initialize_Fields_For_Subscript_Expr;

      
   function Subscript_Expr_F_Prefix
     (Node : Bare_Subscript_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Subscript_Expr_F_Prefix;
      
   end;

      
   function Subscript_Expr_F_Suffix
     (Node : Bare_Subscript_Expr) return Bare_Expr_List
   is
      

   begin
         
         return Node.Subscript_Expr_F_Suffix;
      
   end;




   


      

   --
   --  Primitives for Bare_Tuple_Lit
   --

   



      
      procedure Initialize_Fields_For_Tuple_Lit
        (Self : Bare_Tuple_Lit
         ; Tuple_Lit_F_Exprs : Bare_Expr_List
        ) is
      begin

            Self.Tuple_Lit_F_Exprs := Tuple_Lit_F_Exprs;
         

      end Initialize_Fields_For_Tuple_Lit;

      
   function Tuple_Lit_F_Exprs
     (Node : Bare_Tuple_Lit) return Bare_Expr_List
   is
      

   begin
         
         return Node.Tuple_Lit_F_Exprs;
      
   end;




   


      

   --
   --  Primitives for Bare_Xor_Expr
   --

   



      
      procedure Initialize_Fields_For_Xor_Expr
        (Self : Bare_Xor_Expr
         ; Xor_Expr_F_Left : Bare_Expr
         ; Xor_Expr_F_Right : Bare_Expr
        ) is
      begin

            Self.Xor_Expr_F_Left := Xor_Expr_F_Left;
            Self.Xor_Expr_F_Right := Xor_Expr_F_Right;
         

      end Initialize_Fields_For_Xor_Expr;

      
   function Xor_Expr_F_Left
     (Node : Bare_Xor_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Xor_Expr_F_Left;
      
   end;

      
   function Xor_Expr_F_Right
     (Node : Bare_Xor_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Xor_Expr_F_Right;
      
   end;




   


      

   --
   --  Primitives for Bare_Yield_Expr
   --

   



      
      procedure Initialize_Fields_For_Yield_Expr
        (Self : Bare_Yield_Expr
         ; Yield_Expr_F_Exprs : Bare_Expr_List
        ) is
      begin

            Self.Yield_Expr_F_Exprs := Yield_Expr_F_Exprs;
         

      end Initialize_Fields_For_Yield_Expr;

      
   function Yield_Expr_F_Exprs
     (Node : Bare_Yield_Expr) return Bare_Expr_List
   is
      

   begin
         
         return Node.Yield_Expr_F_Exprs;
      
   end;




   


      

   --
   --  Primitives for Bare_File_Node
   --

   



      
      procedure Initialize_Fields_For_File_Node
        (Self : Bare_File_Node
         ; File_Node_F_Statements : Bare_Turkixir_Node_List
        ) is
      begin

            Self.File_Node_F_Statements := File_Node_F_Statements;
         

      end Initialize_Fields_For_File_Node;

      
   function File_Node_F_Statements
     (Node : Bare_File_Node) return Bare_Turkixir_Node_List
   is
      

   begin
         
         return Node.File_Node_F_Statements;
      
   end;




   


      

   --
   --  Primitives for Bare_Import_Star
   --

   







   


      

   --
   --  Primitives for Bare_Kw_Args_Flag
   --

   





   







--# property-start '[dispatcher]KwArgsFlag.as_bool' dispatcher
pragma Warnings (Off, "is not referenced");
 function Dispatcher_Kw_Args_Flag_P_As_Bool
  
  (Node : Bare_Kw_Args_Flag
  )

   return Boolean
is
   Self : Bare_Kw_Args_Flag := Bare_Kw_Args_Flag (Node);

   Call_Depth : aliased Natural;

   

   --# bind self Self


   Property_Result : Boolean;



begin
   --# property-body-start


   if Self /= null then
      Enter_Call (Self.Unit.Context, Call_Depth'Access);
   end if;




      if Self = null then
         raise Property_Error with "dispatching on null node";
      end if;

      case Turkixir_Kw_Args_Flag (Self.Kind) is
               when Turkixir_Kw_Args_Flag_Absent =>
                  --# property-call-start KwArgsFlag.Absent.as_bool
                  Property_Result := Kw_Args_Flag_Absent_P_As_Bool
                    (Self
                    );
                  --# end
               when Turkixir_Kw_Args_Flag_Present =>
                  --# property-call-start KwArgsFlag.Present.as_bool
                  Property_Result := Kw_Args_Flag_Present_P_As_Bool
                    (Self
                    );
                  --# end
      end case;




   if Self /= null then
      Exit_Call (Self.Unit.Context, Call_Depth);
   end if;
   return Property_Result;

exception


   when others =>
      if Self /= null then
         Exit_Call (Self.Unit.Context, Call_Depth);
      end if;
      raise;

end Dispatcher_Kw_Args_Flag_P_As_Bool;
--# end



   


      

   --
   --  Primitives for Bare_Kw_Args_Flag_Absent
   --

   





   







--# property-start KwArgsFlag.Absent.as_bool 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:19'
pragma Warnings (Off, "is not referenced");
 function Kw_Args_Flag_Absent_P_As_Bool
  
  (Node : Bare_Kw_Args_Flag_Absent
  )

   return Boolean
is
   Self : Bare_Kw_Args_Flag_Absent := Bare_Kw_Args_Flag_Absent (Node);

   Call_Depth : aliased Natural;

   

   --# bind self Self


   Property_Result : Boolean;

      

      



begin
   --# property-body-start


   if Self /= null then
      Enter_Call (Self.Unit.Context, Call_Depth'Access);
   end if;




      
   --# scope-start

      

      Property_Result := False;
      
   --# end




   if Self /= null then
      Exit_Call (Self.Unit.Context, Call_Depth);
   end if;
   return Property_Result;

exception


   when others =>
      if Self /= null then
         Exit_Call (Self.Unit.Context, Call_Depth);
      end if;
      raise;

end Kw_Args_Flag_Absent_P_As_Bool;
--# end



   


      

   --
   --  Primitives for Bare_Kw_Args_Flag_Present
   --

   





   







--# property-start KwArgsFlag.Present.as_bool 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:19'
pragma Warnings (Off, "is not referenced");
 function Kw_Args_Flag_Present_P_As_Bool
  
  (Node : Bare_Kw_Args_Flag_Present
  )

   return Boolean
is
   Self : Bare_Kw_Args_Flag_Present := Bare_Kw_Args_Flag_Present (Node);

   Call_Depth : aliased Natural;

   

   --# bind self Self


   Property_Result : Boolean;

      

      



begin
   --# property-body-start


   if Self /= null then
      Enter_Call (Self.Unit.Context, Call_Depth'Access);
   end if;




      
   --# scope-start

      

      Property_Result := True;
      
   --# end




   if Self /= null then
      Exit_Call (Self.Unit.Context, Call_Depth);
   end if;
   return Property_Result;

exception


   when others =>
      if Self /= null then
         Exit_Call (Self.Unit.Context, Call_Depth);
      end if;
      raise;

end Kw_Args_Flag_Present_P_As_Bool;
--# end



   


      

   --
   --  Primitives for Bare_NL
   --

   







   


      

   --
   --  Primitives for Bare_Op
   --

   







   


      

   --
   --  Primitives for Bare_Params
   --

   



      
      procedure Initialize_Fields_For_Params
        (Self : Bare_Params
         ; Params_F_Single_Params : Bare_Single_Param_List
        ) is
      begin

            Self.Params_F_Single_Params := Params_F_Single_Params;
         

      end Initialize_Fields_For_Params;

      
   function Params_F_Single_Params
     (Node : Bare_Params) return Bare_Single_Param_List
   is
      

   begin
         
         return Node.Params_F_Single_Params;
      
   end;




   


      

   --
   --  Primitives for Bare_Rel_Name
   --

   



      
      procedure Initialize_Fields_For_Rel_Name
        (Self : Bare_Rel_Name
         ; Rel_Name_F_Dots : Bare_Dot_List
         ; Rel_Name_F_Name : Bare_Name
        ) is
      begin

            Self.Rel_Name_F_Dots := Rel_Name_F_Dots;
            Self.Rel_Name_F_Name := Rel_Name_F_Name;
         

      end Initialize_Fields_For_Rel_Name;

      
   function Rel_Name_F_Dots
     (Node : Bare_Rel_Name) return Bare_Dot_List
   is
      

   begin
         
         return Node.Rel_Name_F_Dots;
      
   end;

      
   function Rel_Name_F_Name
     (Node : Bare_Rel_Name) return Bare_Name
   is
      

   begin
         
         return Node.Rel_Name_F_Name;
      
   end;




   


      

   --
   --  Primitives for Bare_Single_Param
   --

   



   

   

   

   

   

   

   


      procedure Single_Param_Pre_Env_Actions
        (Self            : Bare_Single_Param;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False) is
      begin
            


   

      declare
         Resolver : constant Entity_Resolver :=
            null;

         

         Mappings : Internal_Env_Assoc_Array_Access :=
            Libturkixirlang.Implementation.Env_Mappings_1 (Node => 
Self);
      begin
         for Mapping of Mappings.Items loop

         Add_To_Env
           (Self,
            State,
            Mapping.Key,
            Mapping.Val,
            Mapping.Metadata,
            Resolver,
            Mapping.Dest_Env,
            DSL_Location => "parser.py:65");

         end loop;
         Dec_Ref (Mappings);
      end;
   
   
      end;



      
      procedure Initialize_Fields_For_Single_Param
        (Self : Bare_Single_Param
         ; Single_Param_F_Is_Varargs : Bare_Var_Args_Flag
         ; Single_Param_F_Is_Kwargs : Bare_Kw_Args_Flag
         ; Single_Param_F_Name : Bare_Turkixir_Node
         ; Single_Param_F_Default_Value : Bare_Expr
        ) is
      begin

            Self.Single_Param_F_Is_Varargs := Single_Param_F_Is_Varargs;
            Self.Single_Param_F_Is_Kwargs := Single_Param_F_Is_Kwargs;
            Self.Single_Param_F_Name := Single_Param_F_Name;
            Self.Single_Param_F_Default_Value := Single_Param_F_Default_Value;
         

      end Initialize_Fields_For_Single_Param;

      
   function Single_Param_F_Is_Varargs
     (Node : Bare_Single_Param) return Bare_Var_Args_Flag
   is
      

   begin
         
         return Node.Single_Param_F_Is_Varargs;
      
   end;

      
   function Single_Param_F_Is_Kwargs
     (Node : Bare_Single_Param) return Bare_Kw_Args_Flag
   is
      

   begin
         
         return Node.Single_Param_F_Is_Kwargs;
      
   end;

      
   function Single_Param_F_Name
     (Node : Bare_Single_Param) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.Single_Param_F_Name;
      
   end;

      
   function Single_Param_F_Default_Value
     (Node : Bare_Single_Param) return Bare_Expr
   is
      

   begin
         
         return Node.Single_Param_F_Default_Value;
      
   end;


   







--# property-start SingleParam._env_mappings_1 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:65'
pragma Warnings (Off, "is not referenced");
 function Env_Mappings_1
  
  (Node : Bare_Single_Param
  )

   return Internal_Env_Assoc_Array_Access
is
   Self : Bare_Single_Param := Bare_Single_Param (Node);

   Call_Depth : aliased Natural;

   

   --# bind self Self


   Property_Result : Internal_Env_Assoc_Array_Access;

      
            procedure Finalizer_Scope_26;
            procedure Finalizer_Scope_32;
            procedure Finalizer_Scope_33;
            procedure Finalizer_Scope_34;

      Match_0 : Bare_Id;
Match_1 : Bare_Id_List;
Match_2 : Bare_Turkixir_Node;
Fld : Bare_Turkixir_Node;
Fld_1 : Symbol_Type;
New_Struct : Internal_DesignatedEnv := No_DesignatedEnv;
Cast_Expr : Bare_Single_Param;
Cast_Result : Bare_Turkixir_Node;
New_Struct_1 : Internal_Env_Assoc := No_Env_Assoc;
Singleton : Internal_Env_Assoc_Array_Access;
Fld_2 : Symbol_Type;
New_Struct_2 : Internal_DesignatedEnv := No_DesignatedEnv;
Cast_Expr_1 : Bare_Single_Param;
Cast_Result_1 : Bare_Turkixir_Node;
New_Struct_3 : Internal_Env_Assoc := No_Env_Assoc;
Item_0 : Bare_Id;
Map_Result : Internal_Env_Assoc_Array_Access;
Match_Prefix : Bare_Turkixir_Node;
Cast_Result_2 : Bare_Id;
Let_Result : Internal_Env_Assoc_Array_Access;
Scope_Result : Internal_Env_Assoc_Array_Access;
Cast_Result_3 : Bare_Id_List;
Let_Result_1 : Internal_Env_Assoc_Array_Access;
Scope_Result_1 : Internal_Env_Assoc_Array_Access;
Cast_Result_4 : Bare_Turkixir_Node;
Let_Result_2 : Internal_Env_Assoc_Array_Access;
Scope_Result_2 : Internal_Env_Assoc_Array_Access;
Match_Result : Internal_Env_Assoc_Array_Access;

            procedure Finalizer_Scope_26 is
            begin
                     Dec_Ref (Let_Result);
                     Dec_Ref (Scope_Result);
                     Dec_Ref (Let_Result_1);
                     Dec_Ref (Scope_Result_1);
                     Dec_Ref (Let_Result_2);
                     Dec_Ref (Scope_Result_2);
                     Dec_Ref (Match_Result);
            end Finalizer_Scope_26;
            procedure Finalizer_Scope_32 is
            begin
                     Dec_Ref (New_Struct);
                     Dec_Ref (New_Struct_1);
                     Dec_Ref (Singleton);
            end Finalizer_Scope_32;
            procedure Finalizer_Scope_33 is
            begin
                     Dec_Ref (Map_Result);
            end Finalizer_Scope_33;
            procedure Finalizer_Scope_34 is
            begin
                     Dec_Ref (New_Struct_2);
                     Dec_Ref (New_Struct_3);
            end Finalizer_Scope_34;


begin
   --# property-body-start


   if Self /= null then
      Enter_Call (Self.Unit.Context, Call_Depth'Access);
   end if;




      
   --# scope-start

      --# expr-start 10 '<Match>' Match_Result 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:65'






--# expr-start 2 '<FieldAccess .name>' Fld 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:65'






   

      if Self = null then
         raise Property_Error with "dereferencing a null access";
      end if;



Fld := Self.Single_Param_F_Name;
--# expr-done 2


   

      if Fld = null then
         raise Property_Error with "dereferencing a null access";
      end if;



Match_Prefix := Fld; 

case Turkixir_Turkixir_Node (Match_Prefix.Kind) is
         when Turkixir_Id =>
            



   --# scope-start



--# scope-start









   
      Cast_Result_2 := Match_Prefix;


Match_0 := Cast_Result_2; 
--# bind i Match_0
--# expr-start 5 '<New InternalEnvAssoc>' New_Struct_1 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:66'
--# expr-start 3 '<New InternalDesignatedEnv>' New_Struct 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:66'



Inc_Ref (Empty_Env);
New_Struct := (Direct_Env => Empty_Env, Env_Name => null, Kind => Current_Env); 
--# expr-done 3
--# expr-start 4 '<FieldAccess .sym>' Fld_1 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:66'






   

      if Match_0 = null then
         raise Property_Error with "dereferencing a null access";
      end if;



--# property-call-start Id.sym
Fld_1 := Libturkixirlang.Implementation.Id_P_Sym (Node => Match_0);
--# end
--# expr-done 4







Cast_Expr := Self; 



   
      Cast_Result := Cast_Expr;


Inc_Ref (New_Struct);
New_Struct_1 := (Dest_Env => New_Struct, Key => Fld_1, Metadata => No_Metadata, Val => Cast_Result); 
--# expr-done 5
                Singleton := Create_Internal_Env_Assoc_Array (Items_Count => 1);
                Singleton.Items (1) := New_Struct_1;
                Inc_Ref (Singleton.Items (1));
            
Let_Result := Singleton; Inc_Ref (Let_Result);
--# end
Scope_Result := Let_Result;
   Inc_Ref (Scope_Result);


   --# end
      Finalizer_Scope_32;


            Match_Result := Scope_Result; Inc_Ref (Match_Result);
         when Turkixir_Id_List =>
            



   --# scope-start



--# scope-start









   
      Cast_Result_3 := Match_Prefix;


Match_1 := Cast_Result_3; 
--# bind l Match_1
--# expr-start 9 '<map>' Map_Result 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:67'







declare
   Map_Result_Vec : Internal_Env_Assoc_Vectors.Vector;
begin

   

   


      if Match_1 = null then
         Map_Result := Create_Internal_Env_Assoc_Array (0);
      else
         

      
      declare
         Collection : constant Bare_Id_List := Match_1;
      begin
         for Untyped_Item_0 of
               Collection.Nodes (1 .. Children_Count (Collection))
         loop

                  
                  Item_0 := Untyped_Item_0; 

            
   --# scope-start


               --# bind i Item_0

               

      --# expr-start 8 '<New InternalEnvAssoc>' New_Struct_3 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:68'
--# expr-start 6 '<New InternalDesignatedEnv>' New_Struct_2 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:68'



Inc_Ref (Empty_Env);
New_Struct_2 := (Direct_Env => Empty_Env, Env_Name => null, Kind => Current_Env); 
--# expr-done 6
--# expr-start 7 '<FieldAccess .sym>' Fld_2 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:68'






   

      if Item_0 = null then
         raise Property_Error with "dereferencing a null access";
      end if;



--# property-call-start Id.sym
Fld_2 := Libturkixirlang.Implementation.Id_P_Sym (Node => Item_0);
--# end
--# expr-done 7







Cast_Expr_1 := Self; 



   
      Cast_Result_1 := Cast_Expr_1;


Inc_Ref (New_Struct_2);
New_Struct_3 := (Dest_Env => New_Struct_2, Key => Fld_2, Metadata => No_Metadata, Val => Cast_Result_1); 
--# expr-done 8
         declare
            Item_To_Append : constant Internal_Env_Assoc :=
               New_Struct_3;
         begin
               Inc_Ref (Item_To_Append);
            Internal_Env_Assoc_Vectors.Append (Map_Result_Vec, Item_To_Append);
         end;
   

            
   --# end
      Finalizer_Scope_34;

         end loop;
      end;

      Map_Result := Create_Internal_Env_Assoc_Array
        (Items_Count => Natural (Internal_Env_Assoc_Vectors.Length (Map_Result_Vec)));
      for I in Map_Result.Items'Range loop
         Map_Result.Items (I) := Internal_Env_Assoc_Vectors.Get
           (Map_Result_Vec,
            I + Internal_Env_Assoc_Vectors.Index_Type'First - Map_Result.Items'First);
      end loop;
      Internal_Env_Assoc_Vectors.Destroy (Map_Result_Vec);
   
      end if;


end;

--# expr-done 9
Let_Result_1 := Map_Result; Inc_Ref (Let_Result_1);
--# end
Scope_Result_1 := Let_Result_1;
   Inc_Ref (Scope_Result_1);


   --# end
      Finalizer_Scope_33;


            Match_Result := Scope_Result_1; Inc_Ref (Match_Result);
         when Turkixir_Arg_Assoc .. Turkixir_Dotted_Name | Turkixir_Not_Op .. Turkixir_Expr_List | Turkixir_NL_List .. Turkixir_Var_Args_Flag_Present =>
            



   --# scope-start



--# scope-start









   
      Cast_Result_4 := Match_Prefix;


Match_2 := Cast_Result_4; 
--# bind _ Match_2

Let_Result_2 := No_Internal_Env_Assoc_Array_Type; Inc_Ref (Let_Result_2);
--# end
Scope_Result_2 := Let_Result_2;
   Inc_Ref (Scope_Result_2);


   --# end


            Match_Result := Scope_Result_2; Inc_Ref (Match_Result);
end case;

--# expr-done 10

      Property_Result := Match_Result;
         Inc_Ref (Property_Result);
      
   --# end
      Finalizer_Scope_26;




   if Self /= null then
      Exit_Call (Self.Unit.Context, Call_Depth);
   end if;
   return Property_Result;

exception

   when Property_Error =>
               Finalizer_Scope_26;
               Finalizer_Scope_32;
               Finalizer_Scope_33;
               Finalizer_Scope_34;



      if Self /= null then
         Exit_Call (Self.Unit.Context, Call_Depth);
      end if;
      raise;

   when others =>
      if Self /= null then
         Exit_Call (Self.Unit.Context, Call_Depth);
      end if;
      raise;

end Env_Mappings_1;
--# end



   


      

   --
   --  Primitives for Bare_Stmt
   --

   







   


      

   --
   --  Primitives for Bare_Assert_Stmt
   --

   



      
      procedure Initialize_Fields_For_Assert_Stmt
        (Self : Bare_Assert_Stmt
         ; Assert_Stmt_F_Test_Expr : Bare_Expr
         ; Assert_Stmt_F_Msg : Bare_Expr
        ) is
      begin

            Self.Assert_Stmt_F_Test_Expr := Assert_Stmt_F_Test_Expr;
            Self.Assert_Stmt_F_Msg := Assert_Stmt_F_Msg;
         

      end Initialize_Fields_For_Assert_Stmt;

      
   function Assert_Stmt_F_Test_Expr
     (Node : Bare_Assert_Stmt) return Bare_Expr
   is
      

   begin
         
         return Node.Assert_Stmt_F_Test_Expr;
      
   end;

      
   function Assert_Stmt_F_Msg
     (Node : Bare_Assert_Stmt) return Bare_Expr
   is
      

   begin
         
         return Node.Assert_Stmt_F_Msg;
      
   end;




   


      

   --
   --  Primitives for Bare_Assign_Stmt
   --

   



   

   

   

   

   

   

   


      procedure Assign_Stmt_Pre_Env_Actions
        (Self            : Bare_Assign_Stmt;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False) is
      begin
            


   

      declare
         Resolver : constant Entity_Resolver :=
            null;

         

         Mappings : Internal_Env_Assoc_Array_Access :=
            Libturkixirlang.Implementation.Env_Mappings_2 (Node => 
Self);
      begin
         for Mapping of Mappings.Items loop

         Add_To_Env
           (Self,
            State,
            Mapping.Key,
            Mapping.Val,
            Mapping.Metadata,
            Resolver,
            Mapping.Dest_Env,
            DSL_Location => "parser.py:86");

         end loop;
         Dec_Ref (Mappings);
      end;
   
   
      end;



      
      procedure Initialize_Fields_For_Assign_Stmt
        (Self : Bare_Assign_Stmt
         ; Assign_Stmt_F_L_Value : Bare_Expr_List
         ; Assign_Stmt_F_R_Values : Bare_Turkixir_Node_List
        ) is
      begin

            Self.Assign_Stmt_F_L_Value := Assign_Stmt_F_L_Value;
            Self.Assign_Stmt_F_R_Values := Assign_Stmt_F_R_Values;
         

      end Initialize_Fields_For_Assign_Stmt;

      
   function Assign_Stmt_F_L_Value
     (Node : Bare_Assign_Stmt) return Bare_Expr_List
   is
      

   begin
         
         return Node.Assign_Stmt_F_L_Value;
      
   end;

      
   function Assign_Stmt_F_R_Values
     (Node : Bare_Assign_Stmt) return Bare_Turkixir_Node_List
   is
      

   begin
         
         return Node.Assign_Stmt_F_R_Values;
      
   end;


   







--# property-start AssignStmt._env_mappings_2 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:86'
pragma Warnings (Off, "is not referenced");
 function Env_Mappings_2
  
  (Node : Bare_Assign_Stmt
  )

   return Internal_Env_Assoc_Array_Access
is
   Self : Bare_Assign_Stmt := Bare_Assign_Stmt (Node);

   Call_Depth : aliased Natural;

   

   --# bind self Self


   Property_Result : Internal_Env_Assoc_Array_Access;

      
            procedure Finalizer_Scope_27;
            procedure Finalizer_Scope_36;

      Fld : Bare_Expr_List;
Cast_Expr : Bare_Expr;
Cast_Result : Bare_Id;
Fld_1 : Symbol_Type;
New_Struct : Internal_DesignatedEnv := No_DesignatedEnv;
Cast_Expr_1 : Bare_Assign_Stmt;
Cast_Result_1 : Bare_Turkixir_Node;
New_Struct_1 : Internal_Env_Assoc := No_Env_Assoc;
Item_1 : Bare_Expr;
Is_A : Boolean;
Map_Result : Internal_Env_Assoc_Array_Access;

            procedure Finalizer_Scope_27 is
            begin
                     Dec_Ref (Map_Result);
            end Finalizer_Scope_27;
            procedure Finalizer_Scope_36 is
            begin
                     Dec_Ref (New_Struct);
                     Dec_Ref (New_Struct_1);
            end Finalizer_Scope_36;


begin
   --# property-body-start


   if Self /= null then
      Enter_Call (Self.Unit.Context, Call_Depth'Access);
   end if;




      
   --# scope-start

      --# expr-start 17 '<filter_map>' Map_Result 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:86'





--# expr-start 11 '<FieldAccess .l_value>' Fld 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:86'






   

      if Self = null then
         raise Property_Error with "dereferencing a null access";
      end if;



Fld := Self.Assign_Stmt_F_L_Value;
--# expr-done 11

declare
   Map_Result_Vec : Internal_Env_Assoc_Vectors.Vector;
begin

   

   


      if Fld = null then
         Map_Result := Create_Internal_Env_Assoc_Array (0);
      else
         

      
      declare
         Collection : constant Bare_Expr_List := Fld;
      begin
         for Untyped_Item_1 of
               Collection.Nodes (1 .. Children_Count (Collection))
         loop

                  
                  Item_1 := Untyped_Item_1; 

            
   --# scope-start


               --# bind e Item_1

               --# expr-start 12 '<IsA Id>' Is_A 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:88'

Is_A := Item_1 /= null 
and then Item_1.Kind in Turkixir_Id_Range; 
--# expr-done 12
               if Is_A then
                  

      --# expr-start 16 '<New InternalEnvAssoc>' New_Struct_1 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:87'
--# expr-start 13 '<New InternalDesignatedEnv>' New_Struct 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:87'



Inc_Ref (Empty_Env);
New_Struct := (Direct_Env => Empty_Env, Env_Name => null, Kind => Current_Env); 
--# expr-done 13
--# expr-start 15 '<FieldAccess .sym>' Fld_1 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:87'



--# expr-start 14 '<Cast to Id>' Cast_Result 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:87'






Cast_Expr := Item_1; 



   if Cast_Expr = null
      or else Cast_Expr.Kind in Turkixir_Id_Range
   then
      
      Cast_Result := Cast_Expr;

   else
         raise Property_Error with "invalid object cast";
   end if;


--# expr-done 14


   

      if Cast_Result = null then
         raise Property_Error with "dereferencing a null access";
      end if;



--# property-call-start Id.sym
Fld_1 := Libturkixirlang.Implementation.Id_P_Sym (Node => Cast_Result);
--# end
--# expr-done 15







Cast_Expr_1 := Self; 



   
      Cast_Result_1 := Cast_Expr_1;


Inc_Ref (New_Struct);
New_Struct_1 := (Dest_Env => New_Struct, Key => Fld_1, Metadata => No_Metadata, Val => Cast_Result_1); 
--# expr-done 16
         declare
            Item_To_Append : constant Internal_Env_Assoc :=
               New_Struct_1;
         begin
               Inc_Ref (Item_To_Append);
            Internal_Env_Assoc_Vectors.Append (Map_Result_Vec, Item_To_Append);
         end;
   
               end if;

            
   --# end
      Finalizer_Scope_36;

         end loop;
      end;

      Map_Result := Create_Internal_Env_Assoc_Array
        (Items_Count => Natural (Internal_Env_Assoc_Vectors.Length (Map_Result_Vec)));
      for I in Map_Result.Items'Range loop
         Map_Result.Items (I) := Internal_Env_Assoc_Vectors.Get
           (Map_Result_Vec,
            I + Internal_Env_Assoc_Vectors.Index_Type'First - Map_Result.Items'First);
      end loop;
      Internal_Env_Assoc_Vectors.Destroy (Map_Result_Vec);
   
      end if;


end;

--# expr-done 17

      Property_Result := Map_Result;
         Inc_Ref (Property_Result);
      
   --# end
      Finalizer_Scope_27;




   if Self /= null then
      Exit_Call (Self.Unit.Context, Call_Depth);
   end if;
   return Property_Result;

exception

   when Property_Error =>
               Finalizer_Scope_27;
               Finalizer_Scope_36;



      if Self /= null then
         Exit_Call (Self.Unit.Context, Call_Depth);
      end if;
      raise;

   when others =>
      if Self /= null then
         Exit_Call (Self.Unit.Context, Call_Depth);
      end if;
      raise;

end Env_Mappings_2;
--# end



   


      

   --
   --  Primitives for Bare_Aug_Assign_Stmt
   --

   



      
      procedure Initialize_Fields_For_Aug_Assign_Stmt
        (Self : Bare_Aug_Assign_Stmt
         ; Aug_Assign_Stmt_F_L_Value : Bare_Expr_List
         ; Aug_Assign_Stmt_F_Op : Bare_Op
         ; Aug_Assign_Stmt_F_R_Value : Bare_Turkixir_Node
        ) is
      begin

            Self.Aug_Assign_Stmt_F_L_Value := Aug_Assign_Stmt_F_L_Value;
            Self.Aug_Assign_Stmt_F_Op := Aug_Assign_Stmt_F_Op;
            Self.Aug_Assign_Stmt_F_R_Value := Aug_Assign_Stmt_F_R_Value;
         

      end Initialize_Fields_For_Aug_Assign_Stmt;

      
   function Aug_Assign_Stmt_F_L_Value
     (Node : Bare_Aug_Assign_Stmt) return Bare_Expr_List
   is
      

   begin
         
         return Node.Aug_Assign_Stmt_F_L_Value;
      
   end;

      
   function Aug_Assign_Stmt_F_Op
     (Node : Bare_Aug_Assign_Stmt) return Bare_Op
   is
      

   begin
         
         return Node.Aug_Assign_Stmt_F_Op;
      
   end;

      
   function Aug_Assign_Stmt_F_R_Value
     (Node : Bare_Aug_Assign_Stmt) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.Aug_Assign_Stmt_F_R_Value;
      
   end;




   


      

   --
   --  Primitives for Bare_Break_Stmt
   --

   







   


      

   --
   --  Primitives for Bare_Continue_Stmt
   --

   







   


      

   --
   --  Primitives for Bare_Decorated
   --

   



      
      procedure Initialize_Fields_For_Decorated
        (Self : Bare_Decorated
         ; Decorated_F_Decorators : Bare_Decorator_List
         ; Decorated_F_Defn : Bare_Def_Stmt
        ) is
      begin

            Self.Decorated_F_Decorators := Decorated_F_Decorators;
            Self.Decorated_F_Defn := Decorated_F_Defn;
         

      end Initialize_Fields_For_Decorated;

      
   function Decorated_F_Decorators
     (Node : Bare_Decorated) return Bare_Decorator_List
   is
      

   begin
         
         return Node.Decorated_F_Decorators;
      
   end;

      
   function Decorated_F_Defn
     (Node : Bare_Decorated) return Bare_Def_Stmt
   is
      

   begin
         
         return Node.Decorated_F_Defn;
      
   end;




   


      

   --
   --  Primitives for Bare_Def_Stmt
   --

   



   

   

   

   

   

   

   


      procedure Def_Stmt_Pre_Env_Actions
        (Self            : Bare_Def_Stmt;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False) is
      begin
            


   
      if Add_To_Env_Only then
         return;
      end if;

      declare
         No_Parent         : constant Boolean :=
            False;
         Transitive_Parent : constant Boolean :=
            Libturkixirlang.Implementation.Env_Trans_Parent_0 (Node => 
Self);
         Names             : Symbol_Type_Array_Access :=
            null;
      begin
         Add_Env (Self, State, No_Parent, Transitive_Parent, Names);
      end;
   
   
      end;





   







--# property-start DefStmt._env_trans_parent_0 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:45'
pragma Warnings (Off, "is not referenced");
 function Env_Trans_Parent_0
  
  (Node : Bare_Def_Stmt
  )

   return Boolean
is
   Self : Bare_Def_Stmt := Bare_Def_Stmt (Node);

   Call_Depth : aliased Natural;

   

   --# bind self Self


   Property_Result : Boolean;

      

      



begin
   --# property-body-start


   if Self /= null then
      Enter_Call (Self.Unit.Context, Call_Depth'Access);
   end if;




      
   --# scope-start

      

      Property_Result := False;
      
   --# end




   if Self /= null then
      Exit_Call (Self.Unit.Context, Call_Depth);
   end if;
   return Property_Result;

exception


   when others =>
      if Self /= null then
         Exit_Call (Self.Unit.Context, Call_Depth);
      end if;
      raise;

end Env_Trans_Parent_0;
--# end



   


      

   --
   --  Primitives for Bare_Class_Def
   --

   



      
      procedure Initialize_Fields_For_Class_Def
        (Self : Bare_Class_Def
         ; Class_Def_F_Name : Bare_Id
         ; Class_Def_F_Bases : Bare_Expr_List
         ; Class_Def_F_Statements : Bare_Turkixir_Node
        ) is
      begin

            Self.Class_Def_F_Name := Class_Def_F_Name;
            Self.Class_Def_F_Bases := Class_Def_F_Bases;
            Self.Class_Def_F_Statements := Class_Def_F_Statements;
         

      end Initialize_Fields_For_Class_Def;

      
   function Class_Def_F_Name
     (Node : Bare_Class_Def) return Bare_Id
   is
      

   begin
         
         return Node.Class_Def_F_Name;
      
   end;

      
   function Class_Def_F_Bases
     (Node : Bare_Class_Def) return Bare_Expr_List
   is
      

   begin
         
         return Node.Class_Def_F_Bases;
      
   end;

      
   function Class_Def_F_Statements
     (Node : Bare_Class_Def) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.Class_Def_F_Statements;
      
   end;




   


      

   --
   --  Primitives for Bare_Func_Def
   --

   



      
      procedure Initialize_Fields_For_Func_Def
        (Self : Bare_Func_Def
         ; Func_Def_F_Name : Bare_Id
         ; Func_Def_F_Parameters : Bare_Params
         ; Func_Def_F_Body : Bare_Turkixir_Node
        ) is
      begin

            Self.Func_Def_F_Name := Func_Def_F_Name;
            Self.Func_Def_F_Parameters := Func_Def_F_Parameters;
            Self.Func_Def_F_Body := Func_Def_F_Body;
         

      end Initialize_Fields_For_Func_Def;

      
   function Func_Def_F_Name
     (Node : Bare_Func_Def) return Bare_Id
   is
      

   begin
         
         return Node.Func_Def_F_Name;
      
   end;

      
   function Func_Def_F_Parameters
     (Node : Bare_Func_Def) return Bare_Params
   is
      

   begin
         
         return Node.Func_Def_F_Parameters;
      
   end;

      
   function Func_Def_F_Body
     (Node : Bare_Func_Def) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.Func_Def_F_Body;
      
   end;




   


      

   --
   --  Primitives for Bare_Del_Stmt
   --

   



      
      procedure Initialize_Fields_For_Del_Stmt
        (Self : Bare_Del_Stmt
         ; Del_Stmt_F_Exprs : Bare_Expr_List
        ) is
      begin

            Self.Del_Stmt_F_Exprs := Del_Stmt_F_Exprs;
         

      end Initialize_Fields_For_Del_Stmt;

      
   function Del_Stmt_F_Exprs
     (Node : Bare_Del_Stmt) return Bare_Expr_List
   is
      

   begin
         
         return Node.Del_Stmt_F_Exprs;
      
   end;




   


      

   --
   --  Primitives for Bare_Elif_Branch
   --

   



      
      procedure Initialize_Fields_For_Elif_Branch
        (Self : Bare_Elif_Branch
         ; Elif_Branch_F_Cond_Test : Bare_Expr
         ; Elif_Branch_F_Statements : Bare_Turkixir_Node
        ) is
      begin

            Self.Elif_Branch_F_Cond_Test := Elif_Branch_F_Cond_Test;
            Self.Elif_Branch_F_Statements := Elif_Branch_F_Statements;
         

      end Initialize_Fields_For_Elif_Branch;

      
   function Elif_Branch_F_Cond_Test
     (Node : Bare_Elif_Branch) return Bare_Expr
   is
      

   begin
         
         return Node.Elif_Branch_F_Cond_Test;
      
   end;

      
   function Elif_Branch_F_Statements
     (Node : Bare_Elif_Branch) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.Elif_Branch_F_Statements;
      
   end;




   


      

   --
   --  Primitives for Bare_Exec_Stmt
   --

   



      
      procedure Initialize_Fields_For_Exec_Stmt
        (Self : Bare_Exec_Stmt
         ; Exec_Stmt_F_Expr : Bare_Expr
         ; Exec_Stmt_F_In_List : Bare_Expr_List
        ) is
      begin

            Self.Exec_Stmt_F_Expr := Exec_Stmt_F_Expr;
            Self.Exec_Stmt_F_In_List := Exec_Stmt_F_In_List;
         

      end Initialize_Fields_For_Exec_Stmt;

      
   function Exec_Stmt_F_Expr
     (Node : Bare_Exec_Stmt) return Bare_Expr
   is
      

   begin
         
         return Node.Exec_Stmt_F_Expr;
      
   end;

      
   function Exec_Stmt_F_In_List
     (Node : Bare_Exec_Stmt) return Bare_Expr_List
   is
      

   begin
         
         return Node.Exec_Stmt_F_In_List;
      
   end;




   


      

   --
   --  Primitives for Bare_For_Stmt
   --

   



      
      procedure Initialize_Fields_For_For_Stmt
        (Self : Bare_For_Stmt
         ; For_Stmt_F_Bindings : Bare_Expr_List
         ; For_Stmt_F_Expr : Bare_Expr_List
         ; For_Stmt_F_Statements : Bare_Turkixir_Node
         ; For_Stmt_F_Else_Part : Bare_Else_Part
        ) is
      begin

            Self.For_Stmt_F_Bindings := For_Stmt_F_Bindings;
            Self.For_Stmt_F_Expr := For_Stmt_F_Expr;
            Self.For_Stmt_F_Statements := For_Stmt_F_Statements;
            Self.For_Stmt_F_Else_Part := For_Stmt_F_Else_Part;
         

      end Initialize_Fields_For_For_Stmt;

      
   function For_Stmt_F_Bindings
     (Node : Bare_For_Stmt) return Bare_Expr_List
   is
      

   begin
         
         return Node.For_Stmt_F_Bindings;
      
   end;

      
   function For_Stmt_F_Expr
     (Node : Bare_For_Stmt) return Bare_Expr_List
   is
      

   begin
         
         return Node.For_Stmt_F_Expr;
      
   end;

      
   function For_Stmt_F_Statements
     (Node : Bare_For_Stmt) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.For_Stmt_F_Statements;
      
   end;

      
   function For_Stmt_F_Else_Part
     (Node : Bare_For_Stmt) return Bare_Else_Part
   is
      

   begin
         
         return Node.For_Stmt_F_Else_Part;
      
   end;




   


      

   --
   --  Primitives for Bare_Global_Stmt
   --

   



      
      procedure Initialize_Fields_For_Global_Stmt
        (Self : Bare_Global_Stmt
         ; Global_Stmt_F_Names : Bare_Id_List
        ) is
      begin

            Self.Global_Stmt_F_Names := Global_Stmt_F_Names;
         

      end Initialize_Fields_For_Global_Stmt;

      
   function Global_Stmt_F_Names
     (Node : Bare_Global_Stmt) return Bare_Id_List
   is
      

   begin
         
         return Node.Global_Stmt_F_Names;
      
   end;




   


      

   --
   --  Primitives for Bare_If_Stmt
   --

   



      
      procedure Initialize_Fields_For_If_Stmt
        (Self : Bare_If_Stmt
         ; If_Stmt_F_Cond_Test : Bare_Expr
         ; If_Stmt_F_Statements : Bare_Turkixir_Node
         ; If_Stmt_F_Elif_Branchs : Bare_Elif_Branch_List
         ; If_Stmt_F_Else_Part : Bare_Else_Part
        ) is
      begin

            Self.If_Stmt_F_Cond_Test := If_Stmt_F_Cond_Test;
            Self.If_Stmt_F_Statements := If_Stmt_F_Statements;
            Self.If_Stmt_F_Elif_Branchs := If_Stmt_F_Elif_Branchs;
            Self.If_Stmt_F_Else_Part := If_Stmt_F_Else_Part;
         

      end Initialize_Fields_For_If_Stmt;

      
   function If_Stmt_F_Cond_Test
     (Node : Bare_If_Stmt) return Bare_Expr
   is
      

   begin
         
         return Node.If_Stmt_F_Cond_Test;
      
   end;

      
   function If_Stmt_F_Statements
     (Node : Bare_If_Stmt) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.If_Stmt_F_Statements;
      
   end;

      
   function If_Stmt_F_Elif_Branchs
     (Node : Bare_If_Stmt) return Bare_Elif_Branch_List
   is
      

   begin
         
         return Node.If_Stmt_F_Elif_Branchs;
      
   end;

      
   function If_Stmt_F_Else_Part
     (Node : Bare_If_Stmt) return Bare_Else_Part
   is
      

   begin
         
         return Node.If_Stmt_F_Else_Part;
      
   end;




   


      

   --
   --  Primitives for Bare_Import_From
   --

   



      
      procedure Initialize_Fields_For_Import_From
        (Self : Bare_Import_From
         ; Import_From_F_Rel_Name : Bare_Turkixir_Node
         ; Import_From_F_Imported : Bare_Turkixir_Node
        ) is
      begin

            Self.Import_From_F_Rel_Name := Import_From_F_Rel_Name;
            Self.Import_From_F_Imported := Import_From_F_Imported;
         

      end Initialize_Fields_For_Import_From;

      
   function Import_From_F_Rel_Name
     (Node : Bare_Import_From) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.Import_From_F_Rel_Name;
      
   end;

      
   function Import_From_F_Imported
     (Node : Bare_Import_From) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.Import_From_F_Imported;
      
   end;




   


      

   --
   --  Primitives for Bare_Import_Name
   --

   



      
      procedure Initialize_Fields_For_Import_Name
        (Self : Bare_Import_Name
         ; Import_Name_F_Imported_Names : Bare_Turkixir_Node_List
        ) is
      begin

            Self.Import_Name_F_Imported_Names := Import_Name_F_Imported_Names;
         

      end Initialize_Fields_For_Import_Name;

      
   function Import_Name_F_Imported_Names
     (Node : Bare_Import_Name) return Bare_Turkixir_Node_List
   is
      

   begin
         
         return Node.Import_Name_F_Imported_Names;
      
   end;




   


      

   --
   --  Primitives for Bare_Pass_Stmt
   --

   







   


      

   --
   --  Primitives for Bare_Print_Stmt
   --

   



      
      procedure Initialize_Fields_For_Print_Stmt
        (Self : Bare_Print_Stmt
         ; Print_Stmt_F_Exprs : Bare_Expr_List
        ) is
      begin

            Self.Print_Stmt_F_Exprs := Print_Stmt_F_Exprs;
         

      end Initialize_Fields_For_Print_Stmt;

      
   function Print_Stmt_F_Exprs
     (Node : Bare_Print_Stmt) return Bare_Expr_List
   is
      

   begin
         
         return Node.Print_Stmt_F_Exprs;
      
   end;




   


      

   --
   --  Primitives for Bare_Raise_Stmt
   --

   



      
      procedure Initialize_Fields_For_Raise_Stmt
        (Self : Bare_Raise_Stmt
         ; Raise_Stmt_F_Exprs : Bare_Expr_List
        ) is
      begin

            Self.Raise_Stmt_F_Exprs := Raise_Stmt_F_Exprs;
         

      end Initialize_Fields_For_Raise_Stmt;

      
   function Raise_Stmt_F_Exprs
     (Node : Bare_Raise_Stmt) return Bare_Expr_List
   is
      

   begin
         
         return Node.Raise_Stmt_F_Exprs;
      
   end;




   


      

   --
   --  Primitives for Bare_Return_Stmt
   --

   



      
      procedure Initialize_Fields_For_Return_Stmt
        (Self : Bare_Return_Stmt
         ; Return_Stmt_F_Exprs : Bare_Expr_List
        ) is
      begin

            Self.Return_Stmt_F_Exprs := Return_Stmt_F_Exprs;
         

      end Initialize_Fields_For_Return_Stmt;

      
   function Return_Stmt_F_Exprs
     (Node : Bare_Return_Stmt) return Bare_Expr_List
   is
      

   begin
         
         return Node.Return_Stmt_F_Exprs;
      
   end;




   


      

   --
   --  Primitives for Bare_Stream_Print_Stmt
   --

   



      
      procedure Initialize_Fields_For_Stream_Print_Stmt
        (Self : Bare_Stream_Print_Stmt
         ; Stream_Print_Stmt_F_Stream_Expr : Bare_Expr
         ; Stream_Print_Stmt_F_Exprs : Bare_Expr_List
        ) is
      begin

            Self.Stream_Print_Stmt_F_Stream_Expr := Stream_Print_Stmt_F_Stream_Expr;
            Self.Stream_Print_Stmt_F_Exprs := Stream_Print_Stmt_F_Exprs;
         

      end Initialize_Fields_For_Stream_Print_Stmt;

      
   function Stream_Print_Stmt_F_Stream_Expr
     (Node : Bare_Stream_Print_Stmt) return Bare_Expr
   is
      

   begin
         
         return Node.Stream_Print_Stmt_F_Stream_Expr;
      
   end;

      
   function Stream_Print_Stmt_F_Exprs
     (Node : Bare_Stream_Print_Stmt) return Bare_Expr_List
   is
      

   begin
         
         return Node.Stream_Print_Stmt_F_Exprs;
      
   end;




   


      

   --
   --  Primitives for Bare_Try_Stmt
   --

   



      
      procedure Initialize_Fields_For_Try_Stmt
        (Self : Bare_Try_Stmt
         ; Try_Stmt_F_Statements : Bare_Turkixir_Node
         ; Try_Stmt_F_Except_Parts : Bare_Except_Part_List
         ; Try_Stmt_F_Else_Part : Bare_Else_Part
         ; Try_Stmt_F_Finally_Part : Bare_Turkixir_Node
        ) is
      begin

            Self.Try_Stmt_F_Statements := Try_Stmt_F_Statements;
            Self.Try_Stmt_F_Except_Parts := Try_Stmt_F_Except_Parts;
            Self.Try_Stmt_F_Else_Part := Try_Stmt_F_Else_Part;
            Self.Try_Stmt_F_Finally_Part := Try_Stmt_F_Finally_Part;
         

      end Initialize_Fields_For_Try_Stmt;

      
   function Try_Stmt_F_Statements
     (Node : Bare_Try_Stmt) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.Try_Stmt_F_Statements;
      
   end;

      
   function Try_Stmt_F_Except_Parts
     (Node : Bare_Try_Stmt) return Bare_Except_Part_List
   is
      

   begin
         
         return Node.Try_Stmt_F_Except_Parts;
      
   end;

      
   function Try_Stmt_F_Else_Part
     (Node : Bare_Try_Stmt) return Bare_Else_Part
   is
      

   begin
         
         return Node.Try_Stmt_F_Else_Part;
      
   end;

      
   function Try_Stmt_F_Finally_Part
     (Node : Bare_Try_Stmt) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.Try_Stmt_F_Finally_Part;
      
   end;




   


      

   --
   --  Primitives for Bare_While_Stmt
   --

   



      
      procedure Initialize_Fields_For_While_Stmt
        (Self : Bare_While_Stmt
         ; While_Stmt_F_Cond_Test : Bare_Expr
         ; While_Stmt_F_Statements : Bare_Turkixir_Node
         ; While_Stmt_F_Else_Part : Bare_Else_Part
        ) is
      begin

            Self.While_Stmt_F_Cond_Test := While_Stmt_F_Cond_Test;
            Self.While_Stmt_F_Statements := While_Stmt_F_Statements;
            Self.While_Stmt_F_Else_Part := While_Stmt_F_Else_Part;
         

      end Initialize_Fields_For_While_Stmt;

      
   function While_Stmt_F_Cond_Test
     (Node : Bare_While_Stmt) return Bare_Expr
   is
      

   begin
         
         return Node.While_Stmt_F_Cond_Test;
      
   end;

      
   function While_Stmt_F_Statements
     (Node : Bare_While_Stmt) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.While_Stmt_F_Statements;
      
   end;

      
   function While_Stmt_F_Else_Part
     (Node : Bare_While_Stmt) return Bare_Else_Part
   is
      

   begin
         
         return Node.While_Stmt_F_Else_Part;
      
   end;




   


      

   --
   --  Primitives for Bare_With_Stmt
   --

   



      
      procedure Initialize_Fields_For_With_Stmt
        (Self : Bare_With_Stmt
         ; With_Stmt_F_Bindings : Bare_As_Name_Node_List
         ; With_Stmt_F_Statements : Bare_Turkixir_Node
        ) is
      begin

            Self.With_Stmt_F_Bindings := With_Stmt_F_Bindings;
            Self.With_Stmt_F_Statements := With_Stmt_F_Statements;
         

      end Initialize_Fields_For_With_Stmt;

      
   function With_Stmt_F_Bindings
     (Node : Bare_With_Stmt) return Bare_As_Name_Node_List
   is
      

   begin
         
         return Node.With_Stmt_F_Bindings;
      
   end;

      
   function With_Stmt_F_Statements
     (Node : Bare_With_Stmt) return Bare_Turkixir_Node
   is
      

   begin
         
         return Node.With_Stmt_F_Statements;
      
   end;




   


      

   --
   --  Primitives for Bare_Turkixir_Node_Base_List
   --

   







   


      

   --
   --  Primitives for Bare_Arg_List
   --

   







   


      

   --
   --  Primitives for Bare_As_Name_Node_List
   --

   







   


      

   --
   --  Primitives for Bare_Decorator_List
   --

   







   


      

   --
   --  Primitives for Bare_Dict_Assoc_List
   --

   







   


      

   --
   --  Primitives for Bare_Dot_List
   --

   







   


      

   --
   --  Primitives for Bare_Elif_Branch_List
   --

   







   


      

   --
   --  Primitives for Bare_Except_Part_List
   --

   







   


      

   --
   --  Primitives for Bare_Expr_List
   --

   







   


      

   --
   --  Primitives for Bare_Id_List
   --

   







   


      

   --
   --  Primitives for Bare_NL_List
   --

   







   


      

   --
   --  Primitives for Bare_Single_Param_List
   --

   







   


      

   --
   --  Primitives for Bare_String_Lit_List
   --

   







   


      

   --
   --  Primitives for Bare_Turkixir_Node_List
   --

   







   


      

   --
   --  Primitives for Bare_Var_Args_Flag
   --

   





   







--# property-start '[dispatcher]VarArgsFlag.as_bool' dispatcher
pragma Warnings (Off, "is not referenced");
 function Dispatcher_Var_Args_Flag_P_As_Bool
  
  (Node : Bare_Var_Args_Flag
  )

   return Boolean
is
   Self : Bare_Var_Args_Flag := Bare_Var_Args_Flag (Node);

   Call_Depth : aliased Natural;

   

   --# bind self Self


   Property_Result : Boolean;



begin
   --# property-body-start


   if Self /= null then
      Enter_Call (Self.Unit.Context, Call_Depth'Access);
   end if;




      if Self = null then
         raise Property_Error with "dispatching on null node";
      end if;

      case Turkixir_Var_Args_Flag (Self.Kind) is
               when Turkixir_Var_Args_Flag_Absent =>
                  --# property-call-start VarArgsFlag.Absent.as_bool
                  Property_Result := Var_Args_Flag_Absent_P_As_Bool
                    (Self
                    );
                  --# end
               when Turkixir_Var_Args_Flag_Present =>
                  --# property-call-start VarArgsFlag.Present.as_bool
                  Property_Result := Var_Args_Flag_Present_P_As_Bool
                    (Self
                    );
                  --# end
      end case;




   if Self /= null then
      Exit_Call (Self.Unit.Context, Call_Depth);
   end if;
   return Property_Result;

exception


   when others =>
      if Self /= null then
         Exit_Call (Self.Unit.Context, Call_Depth);
      end if;
      raise;

end Dispatcher_Var_Args_Flag_P_As_Bool;
--# end



   


      

   --
   --  Primitives for Bare_Var_Args_Flag_Absent
   --

   





   







--# property-start VarArgsFlag.Absent.as_bool 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:14'
pragma Warnings (Off, "is not referenced");
 function Var_Args_Flag_Absent_P_As_Bool
  
  (Node : Bare_Var_Args_Flag_Absent
  )

   return Boolean
is
   Self : Bare_Var_Args_Flag_Absent := Bare_Var_Args_Flag_Absent (Node);

   Call_Depth : aliased Natural;

   

   --# bind self Self


   Property_Result : Boolean;

      

      



begin
   --# property-body-start


   if Self /= null then
      Enter_Call (Self.Unit.Context, Call_Depth'Access);
   end if;




      
   --# scope-start

      

      Property_Result := False;
      
   --# end




   if Self /= null then
      Exit_Call (Self.Unit.Context, Call_Depth);
   end if;
   return Property_Result;

exception


   when others =>
      if Self /= null then
         Exit_Call (Self.Unit.Context, Call_Depth);
      end if;
      raise;

end Var_Args_Flag_Absent_P_As_Bool;
--# end



   


      

   --
   --  Primitives for Bare_Var_Args_Flag_Present
   --

   





   







--# property-start VarArgsFlag.Present.as_bool 'C:\Users\Kerem Akman\Masaüstü\langkit-master\turkixir\language\parser.py:14'
pragma Warnings (Off, "is not referenced");
 function Var_Args_Flag_Present_P_As_Bool
  
  (Node : Bare_Var_Args_Flag_Present
  )

   return Boolean
is
   Self : Bare_Var_Args_Flag_Present := Bare_Var_Args_Flag_Present (Node);

   Call_Depth : aliased Natural;

   

   --# bind self Self


   Property_Result : Boolean;

      

      



begin
   --# property-body-start


   if Self /= null then
      Enter_Call (Self.Unit.Context, Call_Depth'Access);
   end if;




      
   --# scope-start

      

      Property_Result := True;
      
   --# end




   if Self /= null then
      Exit_Call (Self.Unit.Context, Call_Depth);
   end if;
   return Property_Result;

exception


   when others =>
      if Self /= null then
         Exit_Call (Self.Unit.Context, Call_Depth);
      end if;
      raise;

end Var_Args_Flag_Present_P_As_Bool;
--# end



   



   ----------------------------
   -- Destroy_Synthetic_Node --
   ----------------------------

   procedure Destroy_Synthetic_Node (Node : in out Bare_Turkixir_Node) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Root_Node_Record, Bare_Turkixir_Node);
   begin
      --  Don't call Node.Destroy, as Node's children may be gone already: they
      --  have their own destructor and there is no specified order for the
      --  call of these destructors.
      Free_User_Fields (Node);
      Free (Node);
   end Destroy_Synthetic_Node;

   -----------
   -- Image --
   -----------

   function Image (Value : Boolean) return String
   is (if Value then "True" else "False");

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image
        (Node       : Bare_Turkixir_Node;
         Decoration : Boolean := True) return String is
      begin
         if Node = null then
            return "None";
         else
            declare
               Result : constant String :=
                 (Kind_Name (Node) & " "
                  & Basename (Node.Unit) & ":"
                  & Image (Sloc_Range (Node)));
            begin
               return (if Decoration then "<" & Result & ">" else Result);
            end;
         end if;
      end Trace_Image;

   Kind_Names : array (Turkixir_Node_Kind_Type) of Unbounded_String :=
     (Turkixir_Arg_Assoc => To_Unbounded_String ("ArgAssoc"), 
Turkixir_Arg_Gen => To_Unbounded_String ("ArgGen"), 
Turkixir_Kw_Args => To_Unbounded_String ("KwArgs"), 
Turkixir_Var_Args => To_Unbounded_String ("VarArgs"), 
Turkixir_As_Name_Node => To_Unbounded_String ("AsNameNode"), 
Turkixir_Comp_If => To_Unbounded_String ("CompIf"), 
Turkixir_Comp_Op_Kind_Diamond => To_Unbounded_String ("CompOpKindDiamond"), 
Turkixir_Comp_Op_Kind_Eq => To_Unbounded_String ("CompOpKindEq"), 
Turkixir_Comp_Op_Kind_Gt => To_Unbounded_String ("CompOpKindGt"), 
Turkixir_Comp_Op_Kind_Gte => To_Unbounded_String ("CompOpKindGte"), 
Turkixir_Comp_Op_Kind_In => To_Unbounded_String ("CompOpKindIn"), 
Turkixir_Comp_Op_Kind_Is => To_Unbounded_String ("CompOpKindIs"), 
Turkixir_Comp_Op_Kind_Isnot => To_Unbounded_String ("CompOpKindIsnot"), 
Turkixir_Comp_Op_Kind_Lt => To_Unbounded_String ("CompOpKindLt"), 
Turkixir_Comp_Op_Kind_Lte => To_Unbounded_String ("CompOpKindLte"), 
Turkixir_Comp_Op_Kind_Noteq => To_Unbounded_String ("CompOpKindNoteq"), 
Turkixir_Comp_Op_Kind_Notin => To_Unbounded_String ("CompOpKindNotin"), 
Turkixir_Comp_For => To_Unbounded_String ("CompFor"), 
Turkixir_Comp_ForL => To_Unbounded_String ("CompForL"), 
Turkixir_Decorator => To_Unbounded_String ("Decorator"), 
Turkixir_Dict_Assoc => To_Unbounded_String ("DictAssoc"), 
Turkixir_Else_Part => To_Unbounded_String ("ElsePart"), 
Turkixir_Except_Part => To_Unbounded_String ("ExceptPart"), 
Turkixir_And_Expr => To_Unbounded_String ("AndExpr"), 
Turkixir_And_Op => To_Unbounded_String ("AndOp"), 
Turkixir_Arith_Expr => To_Unbounded_String ("ArithExpr"), 
Turkixir_Shift_Expr => To_Unbounded_String ("ShiftExpr"), 
Turkixir_Term => To_Unbounded_String ("Term"), 
Turkixir_Call_Expr => To_Unbounded_String ("CallExpr"), 
Turkixir_Comp_Op => To_Unbounded_String ("CompOp"), 
Turkixir_Concat_String_Lit => To_Unbounded_String ("ConcatStringLit"), 
Turkixir_Dict_Comp => To_Unbounded_String ("DictComp"), 
Turkixir_Dict_Lit => To_Unbounded_String ("DictLit"), 
Turkixir_Dot => To_Unbounded_String ("Dot"), 
Turkixir_Ellipsis_Expr => To_Unbounded_String ("EllipsisExpr"), 
Turkixir_Factor => To_Unbounded_String ("Factor"), 
Turkixir_If_Expr => To_Unbounded_String ("IfExpr"), 
Turkixir_Inline_Eval => To_Unbounded_String ("InlineEval"), 
Turkixir_Lambda_Def => To_Unbounded_String ("LambdaDef"), 
Turkixir_List_Comp => To_Unbounded_String ("ListComp"), 
Turkixir_List_Gen => To_Unbounded_String ("ListGen"), 
Turkixir_List_Lit => To_Unbounded_String ("ListLit"), 
Turkixir_Dotted_Name => To_Unbounded_String ("DottedName"), 
Turkixir_Id => To_Unbounded_String ("Id"), 
Turkixir_Not_Op => To_Unbounded_String ("NotOp"), 
Turkixir_Number_Lit => To_Unbounded_String ("NumberLit"), 
Turkixir_Or_Expr => To_Unbounded_String ("OrExpr"), 
Turkixir_Or_Op => To_Unbounded_String ("OrOp"), 
Turkixir_Power => To_Unbounded_String ("Power"), 
Turkixir_Set_Comp => To_Unbounded_String ("SetComp"), 
Turkixir_Set_Lit => To_Unbounded_String ("SetLit"), 
Turkixir_Slice_Expr => To_Unbounded_String ("SliceExpr"), 
Turkixir_Ext_Slice_Expr => To_Unbounded_String ("ExtSliceExpr"), 
Turkixir_String_Lit => To_Unbounded_String ("StringLit"), 
Turkixir_Subscript_Expr => To_Unbounded_String ("SubscriptExpr"), 
Turkixir_Tuple_Lit => To_Unbounded_String ("TupleLit"), 
Turkixir_Xor_Expr => To_Unbounded_String ("XorExpr"), 
Turkixir_Yield_Expr => To_Unbounded_String ("YieldExpr"), 
Turkixir_File_Node => To_Unbounded_String ("FileNode"), 
Turkixir_Import_Star => To_Unbounded_String ("ImportStar"), 
Turkixir_Kw_Args_Flag_Absent => To_Unbounded_String ("KwArgsFlagAbsent"), 
Turkixir_Kw_Args_Flag_Present => To_Unbounded_String ("KwArgsFlagPresent"), 
Turkixir_NL => To_Unbounded_String ("NL"), 
Turkixir_Op => To_Unbounded_String ("Op"), 
Turkixir_Params => To_Unbounded_String ("Params"), 
Turkixir_Rel_Name => To_Unbounded_String ("RelName"), 
Turkixir_Single_Param => To_Unbounded_String ("SingleParam"), 
Turkixir_Assert_Stmt => To_Unbounded_String ("AssertStmt"), 
Turkixir_Assign_Stmt => To_Unbounded_String ("AssignStmt"), 
Turkixir_Aug_Assign_Stmt => To_Unbounded_String ("AugAssignStmt"), 
Turkixir_Break_Stmt => To_Unbounded_String ("BreakStmt"), 
Turkixir_Continue_Stmt => To_Unbounded_String ("ContinueStmt"), 
Turkixir_Decorated => To_Unbounded_String ("Decorated"), 
Turkixir_Class_Def => To_Unbounded_String ("ClassDef"), 
Turkixir_Func_Def => To_Unbounded_String ("FuncDef"), 
Turkixir_Del_Stmt => To_Unbounded_String ("DelStmt"), 
Turkixir_Elif_Branch => To_Unbounded_String ("ElifBranch"), 
Turkixir_Exec_Stmt => To_Unbounded_String ("ExecStmt"), 
Turkixir_For_Stmt => To_Unbounded_String ("ForStmt"), 
Turkixir_Global_Stmt => To_Unbounded_String ("GlobalStmt"), 
Turkixir_If_Stmt => To_Unbounded_String ("IfStmt"), 
Turkixir_Import_From => To_Unbounded_String ("ImportFrom"), 
Turkixir_Import_Name => To_Unbounded_String ("ImportName"), 
Turkixir_Pass_Stmt => To_Unbounded_String ("PassStmt"), 
Turkixir_Print_Stmt => To_Unbounded_String ("PrintStmt"), 
Turkixir_Raise_Stmt => To_Unbounded_String ("RaiseStmt"), 
Turkixir_Return_Stmt => To_Unbounded_String ("ReturnStmt"), 
Turkixir_Stream_Print_Stmt => To_Unbounded_String ("StreamPrintStmt"), 
Turkixir_Try_Stmt => To_Unbounded_String ("TryStmt"), 
Turkixir_While_Stmt => To_Unbounded_String ("WhileStmt"), 
Turkixir_With_Stmt => To_Unbounded_String ("WithStmt"), 
Turkixir_Arg_List => To_Unbounded_String ("ArgList"), 
Turkixir_As_Name_Node_List => To_Unbounded_String ("AsNameNodeList"), 
Turkixir_Decorator_List => To_Unbounded_String ("DecoratorList"), 
Turkixir_Dict_Assoc_List => To_Unbounded_String ("DictAssocList"), 
Turkixir_Dot_List => To_Unbounded_String ("DotList"), 
Turkixir_Elif_Branch_List => To_Unbounded_String ("ElifBranchList"), 
Turkixir_Except_Part_List => To_Unbounded_String ("ExceptPartList"), 
Turkixir_Expr_List => To_Unbounded_String ("ExprList"), 
Turkixir_Id_List => To_Unbounded_String ("IdList"), 
Turkixir_NL_List => To_Unbounded_String ("NLList"), 
Turkixir_Single_Param_List => To_Unbounded_String ("SingleParamList"), 
Turkixir_String_Lit_List => To_Unbounded_String ("StringLitList"), 
Turkixir_Turkixir_Node_List => To_Unbounded_String ("TurkixirNodeList"), 
Turkixir_Var_Args_Flag_Absent => To_Unbounded_String ("VarArgsFlagAbsent"), 
Turkixir_Var_Args_Flag_Present => To_Unbounded_String ("VarArgsFlagPresent"));

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Node : Bare_Turkixir_Node) return String is
   begin
      return To_String (Kind_Names (Node.Kind));
   end Kind_Name;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Node : Bare_Turkixir_Node) return Natural is
      C : Integer := Kind_To_Node_Children_Count (Node.Kind);
   begin
      if C = -1 then
         return Node.Count;
      else
         return C;
      end if;
   end Children_Count;

   ----------------------
   -- Free_User_Fields --
   ----------------------

   procedure Free_User_Fields (Node : Bare_Turkixir_Node) is

      procedure Reset_Logic_Var (LV : in out Logic_Var_Record);
      --  Reset the LV logic variable, clearing the value it stores

      ---------------------
      -- Reset_Logic_Var --
      ---------------------

      procedure Reset_Logic_Var (LV : in out Logic_Var_Record) is
      begin
         LV.Value := No_Entity;
         Eq_Node.Refs.Reset (LV);
         Eq_Node.Refs.Destroy (LV);
      end Reset_Logic_Var;

      K : constant Turkixir_Node_Kind_Type := Node.Kind;

   begin
      
      null;
   end Free_User_Fields;

   ----------------
   -- Token_Data --
   ----------------

   function Token_Data (Unit : Internal_Unit) return Token_Data_Handler_Access
   is (Unit.TDH'Access);

   -------------------
   -- Lookup_Symbol --
   -------------------

   function Lookup_Symbol
     (Context : Internal_Context; Symbol : Text_Type) return Symbol_Type
   is
      Canon_Symbol : constant Symbolization_Result :=
            Create_Symbol (Symbol)
      ;
   begin
      if Canon_Symbol.Success then
         return Get_Symbol
           (Context.Symbols, Find (Context.Symbols, Canon_Symbol.Symbol));
      else
         raise Invalid_Symbol_Error with Image (Canon_Symbol.Error_Message);
      end if;
   end Lookup_Symbol;

   -------------------------
   -- Create_Special_Unit --
   -------------------------

   function Create_Special_Unit
     (Context             : Internal_Context;
      Normalized_Filename : Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Internal_Unit
   is
      Unit : Internal_Unit := new Analysis_Unit_Type'
        (Context                      => Context,
         Is_Internal                  => False,
         AST_Root                     => null,
         Filename                     => Normalized_Filename,
         Charset                      => To_Unbounded_String (Charset),
         TDH                          => <>,
         Diagnostics                  => <>,
         Is_Env_Populated             => False,
         Rule                         => Rule,
         AST_Mem_Pool                 => No_Pool,
         Destroyables                 => Destroyable_Vectors.Empty_Vector,
         Referenced_Units             => <>,
         Exiled_Entries               => Exiled_Entry_Vectors.Empty_Vector,
         Foreign_Nodes                =>
            Foreign_Node_Entry_Vectors.Empty_Vector,
         Exiled_Entries_In_NED        =>
            Exiled_Entry_In_NED_Vectors.Empty_Vector,
         Exiled_Envs                  => Exiled_Env_Vectors.Empty_Vector,
         Named_Envs                   => Named_Env_Vectors.Empty_Vector,
         Nodes_With_Foreign_Env       => <>,
         Rebindings                   => Env_Rebindings_Vectors.Empty_Vector,
         Cache_Version                => <>,
         Unit_Version                 => <>,
         others => <>
      );
   begin
      Initialize (Unit.TDH, Context.Symbols,
                  Context.Tab_Stop);
      return Unit;
   end Create_Special_Unit;

   --------------------
   -- Templates_Unit --
   --------------------

   function Templates_Unit (Context : Internal_Context) return Internal_Unit is
   begin
      if Context.Templates_Unit = No_Analysis_Unit then
         Context.Templates_Unit := Create_Special_Unit
           (Context             => Context,
            Normalized_Filename => No_File,
            Charset             => Default_Charset,
            Rule                => Main_Rule_Rule);
      end if;
      return Context.Templates_Unit;
   end Templates_Unit;

   --------------
   -- Set_Rule --
   --------------

   procedure Set_Rule (Unit : Internal_Unit; Rule : Grammar_Rule) is
   begin
      Unit.Rule := Rule;
   end Set_Rule;

   ------------------------------
   -- Normalized_Unit_Filename --
   ------------------------------

   function Normalized_Unit_Filename
     (Context : Internal_Context; Filename : String) return Virtual_File
   is
      use Virtual_File_Maps;
      Key : constant Unbounded_String := To_Unbounded_String (Filename);
      Cur : Cursor := Context.Filenames.Find (Key);
   begin
      if Cur = No_Element then
         declare
            F : constant Virtual_File := Create
              (Create_From_Base (+Filename).Full_Name,
               Normalize => True);
         begin
            Context.Filenames.Insert (Key, F);
            return F;
         end;
      else
         return Element (Cur);
      end if;
   end Normalized_Unit_Filename;

   --------------------------
   -- Register_Destroyable --
   --------------------------

   procedure Register_Destroyable_Helper
     (Unit    : Internal_Unit;
      Object  : System.Address;
      Destroy : Destroy_Procedure)
   is
   begin
      Destroyable_Vectors.Append (Unit.Destroyables, (Object, Destroy));
   end Register_Destroyable_Helper;

   --------------------------
   -- Register_Destroyable --
   --------------------------

   procedure Register_Destroyable
     (Unit : Internal_Unit; Node : Bare_Turkixir_Node)
   is
      procedure Helper is new Register_Destroyable_Gen
        (Root_Node_Record,
         Bare_Turkixir_Node,
         Destroy_Synthetic_Node);
   begin
      Helper (Unit, Node);
   end Register_Destroyable;

   --------------------------
   -- Register_Destroyable --
   --------------------------

   procedure Register_Destroyable
     (Unit : Internal_Unit; Env : AST_Envs.Lexical_Env_Access)
   is
      procedure Helper is new Register_Destroyable_Gen
        (AST_Envs.Lexical_Env_Record, AST_Envs.Lexical_Env_Access, Destroy);
   begin
      Helper (Unit, Env);
   end Register_Destroyable;

   -----------------------
   -- Invalidate_Caches --
   -----------------------

   procedure Invalidate_Caches
     (Context : Internal_Context; Invalidate_Envs : Boolean) is
   begin
      --  Increase Context's version number. If we are about to overflow, reset
      --  all version numbers from analysis units.
      if Context.Cache_Version = Version_Number'Last then
         Context.Cache_Version := 1;
         for Unit of Context.Units loop
            Unit.Cache_Version := 0;
         end loop;
      else
         Context.Cache_Version := Context.Cache_Version + 1;
      end if;

      if Invalidate_Envs then
         Context.Reparse_Cache_Version := Context.Cache_Version;
      end if;
   end Invalidate_Caches;

   ------------------
   --  Reset_Envs  --
   ------------------

   procedure Reset_Envs (Unit : Internal_Unit) is

      procedure Deactivate_Refd_Envs (Node : Bare_Turkixir_Node);
      procedure Recompute_Refd_Envs (Node : Bare_Turkixir_Node);

      --------------------------
      -- Deactivate_Refd_Envs --
      --------------------------

      procedure Deactivate_Refd_Envs (Node : Bare_Turkixir_Node) is
      begin
         if Node = null then
            return;
         end if;

         Deactivate_Referenced_Envs (Node.Self_Env);
         for I in 1 .. Children_Count (Node) loop
            Deactivate_Refd_Envs (Child (Node, I));
         end loop;
      end Deactivate_Refd_Envs;

      -------------------------
      -- Recompute_Refd_Envs --
      -------------------------

      procedure Recompute_Refd_Envs (Node : Bare_Turkixir_Node) is
      begin
         if Node = null then
            return;
         end if;
         Recompute_Referenced_Envs (Node.Self_Env);
         for I in 1 .. Children_Count (Node) loop
            Recompute_Refd_Envs (Child (Node, I));
         end loop;
      end Recompute_Refd_Envs;

   begin
      --  First pass will deactivate every referenced envs that Unit possesses
      Deactivate_Refd_Envs (Unit.AST_Root);

      --  Second pass will recompute the env they are pointing to
      Recompute_Refd_Envs (Unit.AST_Root);
   end Reset_Envs;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Reparsed : in out Reparsed_Unit) is
   begin
      Free (Reparsed.TDH);
      Reparsed.Diagnostics := Diagnostics_Vectors.Empty_Vector;
      Free (Reparsed.AST_Mem_Pool);
      Reparsed.AST_Root := null;
   end Destroy;

   --------------
   -- Basename --
   --------------

   function Basename (Filename : String) return String is
   begin
      return +Create (+Filename).Base_Name;
   end Basename;

   --------------
   -- Basename --
   --------------

   function Basename (Unit : Internal_Unit) return String is
   begin
      return +Unit.Filename.Base_Name;
   end Basename;

   ------------------
   -- Reset_Caches --
   ------------------

   procedure Reset_Caches (Unit : Internal_Unit) is
      Cache_Version : constant Version_Number := Unit.Cache_Version;
   begin
      if Cache_Version < Unit.Context.Reparse_Cache_Version then
         Unit.Cache_Version := Unit.Context.Reparse_Cache_Version;
         Reset_Envs (Unit);
      end if;

      if Cache_Version < Unit.Context.Cache_Version then
         Unit.Cache_Version := Unit.Context.Cache_Version;
      end if;
   end Reset_Caches;

   --------------------
   -- Reference_Unit --
   --------------------

   procedure Reference_Unit (From, Referenced : Internal_Unit) is
      Dummy : Boolean;
   begin
      Dummy := Analysis_Unit_Sets.Add (From.Referenced_Units, Referenced);
   end Reference_Unit;

   ------------------------
   -- Is_Referenced_From --
   ------------------------

   function Is_Referenced_From
     (Self, Unit : Internal_Unit) return Boolean is
   begin
      if Unit = null or else Self = null then
         return False;
      elsif Unit = Self then
         return True;
      else
         return Analysis_Unit_Sets.Has (Unit.Referenced_Units, Self);
      end if;
   end Is_Referenced_From;

   ----------------
   -- Do_Parsing --
   ----------------

   procedure Do_Parsing
     (Unit   : Internal_Unit;
      Input  : Internal_Lexer_Input;
      Result : out Reparsed_Unit)
   is
      Context  : constant Internal_Context := Unit.Context;
      Unit_TDH : constant Token_Data_Handler_Access := Token_Data (Unit);

      Saved_TDH : Token_Data_Handler;
      --  Holder to save tokens data in Unit.
      --
      --  By design, parsing is required to bind the nodes it creates to an
      --  analysis unit. However, this procedure is supposed to preserve the
      --  Unit itself and return its parsing result in Result.
      --
      --  In order to implement this, we first move "old" token data in this
      --  variable, then we do parsing. Only then, we can move "new" token data
      --  from the unit to Result, and restore the "old" token data to Unit.
      --  This last step is what Rotate_TDH (see below) is above.

      procedure Rotate_TDH;
      --  Move token data from Unit to Result and restore data in Saved_TDH to
      --  Unit.

      ----------------
      -- Rotate_TDH --
      ----------------

      procedure Rotate_TDH is
      begin
         Move (Result.TDH, Unit_TDH.all);
         Move (Unit_TDH.all, Saved_TDH);
      end Rotate_TDH;

   begin
      GNATCOLL.Traces.Trace (Main_Trace, "Parsing unit " & Basename (Unit));

      Result.AST_Root := null;

      Move (Saved_TDH, Unit_TDH.all);
      Initialize (Unit_TDH.all, Saved_TDH.Symbols,
                  Unit.Context.Tab_Stop);

      --  This is where lexing occurs, so this is where we get most "setup"
      --  issues: missing input file, bad charset, etc. If we have such an
      --  error, catch it, turn it into diagnostics and abort parsing.
      --
      --  As it is quite common, first check if the file is readable: if not,
      --  don't bother opening it and directly emit a diagnostic. This avoid
      --  pointless exceptions which harm debugging. Note that this
      --  optimization is valid only when there is no file reader, which can
      --  work even when there is no real source file.

      if Context.File_Reader = null
         and then Input.Kind = File
         and then (Input.Filename.Is_Directory
                   or else (not Input.Filename.Is_Readable))
      then
         declare
            Name : constant String := Basename (Unit);
         begin
            GNATCOLL.Traces.Trace
              (Main_Trace, "WARNING: File is not readable: " & Name);
            Append
              (Result.Diagnostics,
               No_Source_Location_Range,
               "Cannot read " & To_Text (Name));
            Rotate_TDH;
            return;
         end;
      end if;

      --  Initialize the parser, which fetches the source buffer and extract
      --  all tokens.

      Init_Parser
        (Input, Context.With_Trivia, Unit, Unit_TDH, Unit.Context.Parser);

      --  If we could run the lexer, run the parser and get the root node

      if Unit_TDH.Source_Buffer /= null then
         Result.AST_Mem_Pool := Create;
         Unit.Context.Parser.Mem_Pool := Result.AST_Mem_Pool;
         Result.AST_Root := Bare_Turkixir_Node
           (Parse (Unit.Context.Parser, Rule => Unit.Rule));
      end if;

      --  Forward token data and diagnostics to the returned unit

      Rotate_TDH;

      --  TODO we use a for loop rt. than the new ``Append_Vector`` here for
      --  compatibility with old compilers, but someday we'll be able to get
      --  rid of it.
      for Diag of Unit.Context.Parser.Diagnostics loop
         Result.Diagnostics.Append (Diag);
      end loop;
   end Do_Parsing;

   --------------------------
   -- Update_After_Reparse --
   --------------------------

   procedure Update_After_Reparse
     (Unit : Internal_Unit; Reparsed : in out Reparsed_Unit) is
   begin
      --  Remove the `symbol -> AST node` associations for Unit's nodes in
      --  foreign lexical environments.
      Remove_Exiled_Entries (Unit);

      --  Remove the named envs that Unit created
      declare
         Named_Envs_Needing_Update : NED_Maps.Map;
      begin
         Remove_Named_Envs (Unit, Named_Envs_Needing_Update);
         Update_Named_Envs (Named_Envs_Needing_Update);
      end;

      --  At this point, envs and nodes that don't belong to this unit no
      --  longer reference this unit's envs and nodes. It is thus now safe to
      --  deallocate this unit's obsolete data.

      --  Replace Unit's diagnostics by Reparsed's
      Unit.Diagnostics := Reparsed.Diagnostics;
      Reparsed.Diagnostics.Clear;

      --  As (re-)loading a unit can change how any AST node property in the
      --  whole analysis context behaves, we have to invalidate caches. This
      --  is likely overkill, but kill all caches here as it's easy to do.
      --
      --  As an optimization, invalidate referenced envs cache only if this is
      --  not the first time we parse Unit.
      Invalidate_Caches
        (Unit.Context, Invalidate_Envs => Unit.AST_Root /= null);

      --  Likewise for token data
      Free (Unit.TDH);
      Move (Unit.TDH, Reparsed.TDH);

      --  Reparsing will invalidate all lexical environments related to this
      --  unit, so destroy all related rebindings as well. This browses AST
      --  nodes, so we have to do this before destroying the old AST nodes
      --  pool.
      Destroy_Rebindings (Unit.Rebindings'Access);

      --  Destroy the old AST node and replace it by the new one
      if Unit.AST_Root /= null then
         Destroy (Unit.AST_Root);
      end if;
      Unit.AST_Root := Reparsed.AST_Root;

      --  Likewise for memory pools
      Free (Unit.AST_Mem_Pool);
      Unit.AST_Mem_Pool := Reparsed.AST_Mem_Pool;
      Reparsed.AST_Mem_Pool := No_Pool;

      --  Increment unit version number to invalidate caches and stale node
      --  reference. Also propagate it to the TDH.
      Unit.Unit_Version := Unit.Unit_Version + 1;
      Unit.TDH.Version := Unit.Unit_Version;


      --  If Unit had its lexical environments populated, re-populate them
      if not Unit.Is_Env_Populated then
         return;
      end if;

      declare
         Unit_Name     : constant String := +Unit.Filename.Base_Name;
         Context       : constant Internal_Context := Unit.Context;
         Foreign_Nodes : Bare_Turkixir_Node_Vectors.Vector :=
            Bare_Turkixir_Node_Vectors.Empty_Vector;

         Saved_In_Populate_Lexical_Env : constant Boolean :=
            Context.In_Populate_Lexical_Env;
      begin
         GNATCOLL.Traces.Trace
           (Main_Trace, "Updating lexical envs for " & Unit_Name
                        & " after reparse");
         GNATCOLL.Traces.Increase_Indent (Main_Trace);

         Context.In_Populate_Lexical_Env := True;

         --  Collect all nodes that are foreign in this Unit's lexical envs.
         --  Exclude them from the corresponding lists of exiled entries.
         Extract_Foreign_Nodes (Unit, Foreign_Nodes);

         --  Reset the flag so that the call to Populate_Lexical_Env below does
         --  its work.
         Unit.Is_Env_Populated := False;

         --  Now that Unit has been reparsed, we can destroy all its
         --  destroyables, which refer to the old tree (i.e. dangling
         --  pointers).
         Destroy_Unit_Destroyables (Unit);

         for FN of Foreign_Nodes loop
            declare
               Node_Image : constant String := Image (Short_Text_Image (FN));
               Unit_Name  : constant String := +FN.Unit.Filename.Base_Name;
            begin
               GNATCOLL.Traces.Trace
                 (Main_Trace, "Rerooting: " & Node_Image
                              & " (from " & Unit_Name & ")");
            end;
            Reroot_Foreign_Node (FN);
         end loop;
         Foreign_Nodes.Destroy;

         Populate_Lexical_Env (Unit);
         Context.In_Populate_Lexical_Env := Saved_In_Populate_Lexical_Env;
         GNATCOLL.Traces.Decrease_Indent (Main_Trace);
      end;
   end Update_After_Reparse;

   -------------------------------
   -- Destroy_Unit_Destroyables --
   -------------------------------

   procedure Destroy_Unit_Destroyables (Unit : Internal_Unit) is
   begin
      for D of Unit.Destroyables loop
         D.Destroy (D.Object);
      end loop;
      Destroyable_Vectors.Clear (Unit.Destroyables);
   end Destroy_Unit_Destroyables;

   ---------------------------
   -- Remove_Exiled_Entries --
   ---------------------------

   procedure Remove_Exiled_Entries (Unit : Internal_Unit) is
   begin
      for EE of Unit.Exiled_Entries loop
         AST_Envs.Remove (EE.Env, EE.Key, EE.Node);

         --  Also strip foreign nodes information from "outer" units so that it
         --  does not contain stale information (i.e. dangling pointers to
         --  nodes that belong to the units in the queue).
         if EE.Env.Owner /= No_Generic_Unit then
            declare
               Foreign_Nodes : Foreign_Node_Entry_Vectors.Vector renames
                  Convert_Unit (EE.Env.Owner).Foreign_Nodes;
               Current       : Positive := Foreign_Nodes.First_Index;
            begin
               while Current <= Foreign_Nodes.Last_Index loop
                  if Foreign_Nodes.Get (Current).Node = EE.Node then
                     Foreign_Nodes.Pop (Current);
                  else
                     Current := Current + 1;
                  end if;
               end loop;
            end;
         end if;
      end loop;

      Unit.Exiled_Entries.Clear;
   end Remove_Exiled_Entries;

   -----------------------
   -- Remove_Named_Envs --
   -----------------------

   procedure Remove_Named_Envs
     (Unit                      : Internal_Unit;
      Named_Envs_Needing_Update : in out NED_Maps.Map) is
   begin
      --  Remove nodes in this unit from the Named_Env_Descriptor.Foreign_Nodes
      --  components in which they are registered and from the foreign
      --  environments themselves.
      for EE of Unit.Exiled_Entries_In_NED loop
         Remove (EE.Named_Env.Foreign_Nodes, EE.Key, EE.Node);
         Remove (EE.Named_Env.Env_With_Precedence, EE.Key, EE.Node);
      end loop;
      Unit.Exiled_Entries_In_NED.Clear;

      --  Remove nodes in this unit from the
      --  Named_Env_Descriptor.Nodes_With_Foreign_Env components in which they
      --  are registered.
      for Cur in Unit.Nodes_With_Foreign_Env.Iterate loop
         declare
            use Node_To_Named_Env_Maps;
            Node : constant Bare_Turkixir_Node := Key (Cur);
            NE   : constant Named_Env_Descriptor_Access := Element (Cur);
         begin
            NE.Nodes_With_Foreign_Env.Delete (Node);
         end;
      end loop;
      Unit.Nodes_With_Foreign_Env.Clear;

      --  Remove ends in this unit from the Named_Env_Descriptor.Foreign_Envs
      --  components in which they are registered.
      for EE of Unit.Exiled_Envs loop
         EE.Named_Env.Foreign_Envs.Delete (Env_Node (EE.Env));
      end loop;
      Unit.Exiled_Envs.Clear;

      --  Remove named envs that this unit created
      for NE of Unit.Named_Envs loop
         declare
            NED_Access : constant Named_Env_Descriptor_Access :=
               Unit.Context.Named_Envs.Element (NE.Name);
            NED        : Named_Env_Descriptor renames NED_Access.all;
         begin
            NED.Envs.Delete (Env_Node (NE.Env));

            --  If this named environment had precedence, we must schedule an
            --  update for this name environment entry.
            if NE.Env = NED.Env_With_Precedence then
               Named_Envs_Needing_Update.Include (NE.Name, NED_Access);
               NED.Env_With_Precedence := Empty_Env;
            end if;
         end;
      end loop;
      Unit.Named_Envs.Clear;
   end Remove_Named_Envs;

   ---------------------------
   -- Extract_Foreign_Nodes --
   ---------------------------

   procedure Extract_Foreign_Nodes
     (Unit          : Internal_Unit;
      Foreign_Nodes : in out Bare_Turkixir_Node_Vectors.Vector) is
   begin
      --  Go through all foreign nodes registered in Unit's lexical
      --  environments.
      for FN of Unit.Foreign_Nodes loop
         --  Collect them
         Foreign_Nodes.Append (FN.Node);

         --  For each foreign node, remove the corresponding exiled entry in
         --  that foreign unit (each foreign node in unit A has a corresponding
         --  exiled entry in unit B).
         declare
            Exiled_Entries : Exiled_Entry_Vectors.Vector renames
               FN.Unit.Exiled_Entries;
            Current        : Positive := Exiled_Entries.First_Index;
         begin
            while Current <= Exiled_Entries.Last_Index loop
               if Exiled_Entries.Get (Current).Node = FN.Node then
                  Exiled_Entries.Pop (Current);
               else
                  Current := Current + 1;
               end if;
            end loop;
         end;
      end loop;
      Unit.Foreign_Nodes.Clear;
   end Extract_Foreign_Nodes;

   --------------------------
   -- Reroot_Foreign_Nodes --
   --------------------------

   procedure Reroot_Foreign_Node (Node : Bare_Turkixir_Node) is
      Unit : constant Internal_Unit := Node.Unit;
   begin
      --  First, filter the exiled entries in foreign units so that they don't
      --  contain references to this unit's lexical environments.  We need to
      --  do that before running the partial Populate_Lexical_Env pass so that
      --  we don't remove exiled entries that this pass will produce.
      declare
         Exiled_Entries : Exiled_Entry_Vectors.Vector renames
            Unit.Exiled_Entries;
         Current        : Positive := Exiled_Entries.First_Index;
      begin
         while Current <= Exiled_Entries.Last_Index loop
            if Exiled_Entries.Get (Current).Node = Node then
               Exiled_Entries.Pop (Current);
            else
               Current := Current + 1;
            end if;
         end loop;
      end;

      --  Re-do a partial Populate_Lexical_Env pass for each foreign node that
      --  this unit contains so that they are relocated in our new lexical
      --  environments.
      declare
         Unit_State : aliased PLE_Unit_State :=
           (Named_Envs_Needing_Update => <>);
         State      : PLE_Node_State :=
           (Unit_State  => Unit_State'Unchecked_Access,
            Current_Env => Node.Self_Env,
            Current_NED => null);
      begin
         Pre_Env_Actions (Node, State, Add_To_Env_Only => True);
         Post_Env_Actions (Node, State);
      end;
   end Reroot_Foreign_Node;

   ----------
   -- Text --
   ----------

   function Text (Node : Bare_Turkixir_Node) return String_Type is
   begin
      return Create_String (Text (Node));
   end Text;

   ------------------------
   -- Destroy_Rebindings --
   ------------------------

   procedure Destroy_Rebindings
     (Rebindings : access Env_Rebindings_Vectors.Vector)
   is
      procedure Recurse (R : in out Env_Rebindings);
      --  Destroy R's children and then destroy R. It is up to the caller to
      --  remove R from its parent's Children vector.

      procedure Unregister
        (R          : Env_Rebindings;
         Rebindings : in out Env_Rebindings_Vectors.Vector);
      --  Remove R from Rebindings

      -------------
      -- Recurse --
      -------------

      procedure Recurse (R : in out Env_Rebindings) is
      begin
         for C of R.Children loop
            declare
               C_Var : Env_Rebindings := C;
            begin
               Recurse (C_Var);
            end;
         end loop;
         R.Children.Destroy;

         Unregister (R, Convert_Unit (R.Old_Env.Owner).Rebindings);
         Unregister (R, Convert_Unit (R.New_Env.Owner).Rebindings);

         Release_Rebinding (R);
      end Recurse;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister
        (R          : Env_Rebindings;
         Rebindings : in out Env_Rebindings_Vectors.Vector) is
      begin
         for I in 1 .. Rebindings.Length loop
            if Rebindings.Get (I) = R then
               Rebindings.Pop (I);
               return;
            end if;
         end loop;

         --  We are always supposed to find R in Rebindings, so this should be
         --  unreachable.
         raise Program_Error;
      end Unregister;

   begin
      while Rebindings.Length > 0 loop
         declare
            R : Env_Rebindings := Rebindings.Get (1);
         begin
            --  Here, we basically undo what has been done in AST_Envs.Append

            --  If this rebinding has no parent, then during its creation we
            --  registered it in its Old_Env. Otherwise, it is registered
            --  in its Parent's Children list.
            if R.Parent = null then
               Unwrap (R.Old_Env).Rebindings_Pool.Delete (R.New_Env);
            else
               Unregister (R, R.Parent.Children);
            end if;

            --  In all cases it's registered in Old_Env's and New_Env's units
            Recurse (R);
         end;
      end loop;
   end Destroy_Rebindings;

   --------------------------
   -- Get_Rewriting_Handle --
   --------------------------

   function Get_Rewriting_Handle
     (Context : Internal_Context) return Rewriting_Handle_Pointer is
   begin
      return Context.Rewriting_Handle;
   end Get_Rewriting_Handle;

   --------------------------
   -- Set_Rewriting_Handle --
   --------------------------

   procedure Set_Rewriting_Handle
     (Context : Internal_Context; Handle : Rewriting_Handle_Pointer) is
   begin
      Context.Rewriting_Handle := Handle;
   end Set_Rewriting_Handle;

   -----------------------
   -- Create_Safety_Net --
   -----------------------

   function Create_Safety_Net
     (Context : Internal_Context) return Iterator_Safety_Net
   is
   begin
      return (Context         => Context,
              Context_Serial  => Context.Serial_Number,
              Context_Version => Context.Cache_Version);
   end Create_Safety_Net;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Self : Iterator_Safety_Net) is
   begin
      if Self.Context = null then
         return;
      end if;

      --  Check that the context is still the same (not released nor reused)
      if Self.Context.Serial_Number /= Self.Context_Serial
         or else Self.Context.Cache_Version /= Self.Context_Version
      then
         raise Stale_Reference_Error;
      end if;
   end Check_Safety_Net;

   ----------------------
   -- String_To_Symbol --
   ----------------------

   function String_To_Symbol
     (Context : Internal_Context; S : String_Type) return Symbol_Type is
   begin
      return (if S.Length > 0
              then Lookup_Symbol (Context, S.Content)
              else null);
   exception
      when Exc : Invalid_Symbol_Error =>
         raise Property_Error with Ada.Exceptions.Exception_Message (Exc);
   end String_To_Symbol;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : String_Type) is
   begin
      if Self.Ref_Count >= 0 then
         Self.Ref_Count := Self.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out String_Type) is
   begin
      if Self = null or else Self.Ref_Count < 0 then
         return;
      end if;

      if Self.Ref_Count = 1 then
         Free (Self);
      else
         Self.Ref_Count := Self.Ref_Count - 1;
         Self := null;
      end if;
   end Dec_Ref;

   -------------------
   -- Create_String --
   -------------------

   function Create_String (Content : Text_Type) return String_Type is
   begin
      return Result : constant String_Type := new String_Record'
        (Length    => Content'Length,
         Ref_Count => 1,
         Content   => Content);
   end Create_String;

   -------------------
   -- Create_String --
   -------------------

   function Create_String (Content : Unbounded_Text_Type) return String_Type is
      S : Big_Wide_Wide_String_Access;
      L : Natural;
   begin
      Get_Wide_Wide_String (Content, S, L);
      return Create_String (S.all (1 .. L));
   end Create_String;

   -------------------
   -- Concat_String --
   -------------------

   function Concat_String (Left, Right : String_Type) return String_Type is
   begin
      return Result : constant String_Type :=
        new String_Record (Length => Left.Length + Right.Length)
      do
         Result.Ref_Count := 1;
         Result.Content (1 .. Left.Length) := Left.Content;
         Result.Content (Left.Length + 1 .. Result.Length) := Right.Content;
      end return;
   end Concat_String;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (Left, Right : String_Type) return Boolean is
   begin
      return Left.Content = Right.Content;
   end Equivalent;

begin
   No_Big_Integer.Value.Set (0);
end Libturkixirlang.Implementation;
