


with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded;



with Libturkixirlang.Introspection; use Libturkixirlang.Introspection;




package body Libturkixirlang.Iterators is

   package Predicate_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Turkixir_Node_Predicate,
      "="          => Turkixir_Node_Predicate_References."=");

   function To_Array
     (Predicates : Predicate_Vectors.Vector) return Turkixir_Node_Predicate_Array;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Predicates : Predicate_Vectors.Vector) return Turkixir_Node_Predicate_Array
   is
   begin
      return Result : Turkixir_Node_Predicate_Array (1 .. Natural (Predicates.Length)) do
         for I in Result'Range loop
            Result (I) := Predicates.Element (I);
         end loop;
      end return;
   end To_Array;

   --------------
   -- Traverse --
   --------------

   function Traverse (Root : Turkixir_Node'Class) return Traverse_Iterator'Class is
   begin
      return Result : Traverse_Iterator do
         Traversal_Iterators.Create_Tree_Iterator (Root.As_Turkixir_Node, Result);
      end return;
   end Traverse;

   -----------
   -- "not" --
   -----------

   function "not" (Predicate : Turkixir_Node_Predicate) return Turkixir_Node_Predicate is
   begin
      return Result : Turkixir_Node_Predicate do
         Result.Set
           (Not_Predicate'(Turkixir_Node_Predicate_Interface with Predicate => Predicate));
      end return;
   end "not";

   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : Turkixir_Node_Predicate) return Turkixir_Node_Predicate is
   begin
      return For_All ((Left, Right));
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Turkixir_Node_Predicate) return Turkixir_Node_Predicate is
   begin
      return For_Some ((Left, Right));
   end "or";

   -------------
   -- For_All --
   -------------

   function For_All (Predicates : Turkixir_Node_Predicate_Array) return Turkixir_Node_Predicate is
      Preds : Predicate_Vectors.Vector;
   begin
      --  Flatten sub-predicates that are themselves For_All predicates in
      --  Predicates.
      for P of Predicates loop
         if P.Unchecked_Get.all in For_All_Predicate'Class then
            for Sub_P of For_All_Predicate (P.Unchecked_Get.all).Predicates
            loop
               Preds.Append (Sub_P);
            end loop;
         else
            Preds.Append (P);
         end if;
      end loop;

      return Result : Turkixir_Node_Predicate do
         Result.Set (For_All_Predicate'
           (Turkixir_Node_Predicate_Interface with
            N          => Natural (Preds.Length),
            Predicates => To_Array (Preds)));
      end return;
   end For_All;

   --------------
   -- For_Some --
   --------------

   function For_Some (Predicates : Turkixir_Node_Predicate_Array) return Turkixir_Node_Predicate is
      Preds : Predicate_Vectors.Vector;
   begin
      --  Flatten sub-predicates that are themselves For_Some predicates in
      --  Predicates.
      for P of Predicates loop
         if P.Unchecked_Get.all in For_Some_Predicate'Class then
            for Sub_P of For_Some_Predicate (P.Unchecked_Get.all).Predicates
            loop
               Preds.Append (Sub_P);
            end loop;
         else
            Preds.Append (P);
         end if;
      end loop;

      return Result : Turkixir_Node_Predicate do
         Result.Set (For_Some_Predicate'
           (Turkixir_Node_Predicate_Interface with
            N          => Natural (Preds.Length),
            Predicates => To_Array (Preds)));
      end return;
   end For_Some;

   ----------------------
   -- For_All_Children --
   ----------------------

   function For_All_Children
     (Predicate : Turkixir_Node_Predicate; Skip_Null : Boolean := True) return Turkixir_Node_Predicate
   is
   begin
      return Result : Turkixir_Node_Predicate do
         Result.Set (For_All_Children_Predicate'
           (Turkixir_Node_Predicate_Interface with
            Predicate => Predicate,
            Skip_Null => Skip_Null));
      end return;
   end For_All_Children;

   -----------------------
   -- For_Some_Children --
   -----------------------

   function For_Some_Children
     (Predicate : Turkixir_Node_Predicate; Skip_Null : Boolean := True) return Turkixir_Node_Predicate
   is
   begin
      return Result : Turkixir_Node_Predicate do
         Result.Set (For_Some_Children_Predicate'
           (Turkixir_Node_Predicate_Interface with
            Predicate => Predicate,
            Skip_Null => Skip_Null));
      end return;
   end For_Some_Children;

   ----------------
   -- Child_With --
   ----------------

   function Child_With
     (Field     : Syntax_Field_Reference;
      Predicate : Turkixir_Node_Predicate) return Turkixir_Node_Predicate is
   begin
         return Result : Turkixir_Node_Predicate do
            Result.Set (Child_With_Predicate'
              (Turkixir_Node_Predicate_Interface with
               Field     => Field,
               Predicate => Predicate));
         end return;
   end Child_With;

   -------------
   -- Kind_Is --
   -------------

   function Kind_Is (Kind : Turkixir_Node_Kind_Type) return Turkixir_Node_Predicate is
   begin
      return Kind_In (Kind, Kind);
   end Kind_Is;

   -------------
   -- Kind_In --
   -------------

   function Kind_In (First, Last : Turkixir_Node_Kind_Type) return Turkixir_Node_Predicate is
   begin
      return Result : Turkixir_Node_Predicate do
         Result.Set (Kind_Predicate'(Turkixir_Node_Predicate_Interface with
                                     First => First,
                                     Last  => Last));
      end return;
   end Kind_In;

   -------------
   -- Text_Is --
   -------------

   function Text_Is (Text : Text_Type) return Turkixir_Node_Predicate is
   begin
      return Result : Turkixir_Node_Predicate do
         Result.Set (Text_Predicate'(Turkixir_Node_Predicate_Interface
                     with Text => To_Unbounded_Text (Text)));
      end return;
   end Text_Is;

   ------------------
   -- Node_Is_Null --
   ------------------

   function Node_Is_Null return Turkixir_Node_Predicate is
   begin
      return Result : Turkixir_Node_Predicate do
         Result.Set (Node_Is_Null_Predicate'(Turkixir_Node_Predicate_Interface with null record));
      end return;
   end Node_Is_Null;

   ----------
   -- Next --
   ----------

   function Next
     (It : in out Find_Iterator; Element : out Turkixir_Node) return Boolean
   is
      Parent : Traverse_Iterator := Traverse_Iterator (It);
   begin
      while Next (Parent, Element) loop
         if It.Predicate.Unchecked_Get.Evaluate (Element) then
            return True;
         end if;
      end loop;
      return False;
   end Next;

   ----------
   -- Next --
   ----------

   overriding function Next
     (It : in out Local_Find_Iterator; Element : out Turkixir_Node) return Boolean
   is
      Parent : Traverse_Iterator := Traverse_Iterator (It);
   begin
      while Next (Parent, Element) loop
         if It.Predicate = null or else It.Predicate (Element) then
            return True;
         end if;
      end loop;
      return False;
   end Next;

   ----------
   -- Find --
   ----------

   function Find
     (Root      : Turkixir_Node'Class;
      Predicate : access function (N : Turkixir_Node) return Boolean := null)
     return Traverse_Iterator'Class is
   begin
      return Ret : Local_Find_Iterator do
         Traversal_Iterators.Create_Tree_Iterator (Root.As_Turkixir_Node, Ret);

         --  We still want to provide this functionality, even though it is
         --  unsafe. TODO: We might be able to make a safe version of this
         --  using generics. Still would be more verbose though.
         Ret.Predicate := Predicate'Unrestricted_Access.all;
      end return;
   end Find;

   ----------------
   -- Find_First --
   ----------------

   function Find_First
     (Root      : Turkixir_Node'Class;
      Predicate : access function (N : Turkixir_Node) return Boolean := null)
      return Turkixir_Node
   is
      I      : Traverse_Iterator'Class := Find (Root, Predicate);
      Result : Turkixir_Node;
      Ignore : Boolean;
   begin
      if not I.Next (Result) then
         Result := No_Turkixir_Node;
      end if;
      return Result;
   end Find_First;

   ----------
   -- Find --
   ----------

   function Find
     (Root : Turkixir_Node'Class; Predicate : Turkixir_Node_Predicate'Class)
      return Traverse_Iterator'Class is
   begin
      return Ret : Find_Iterator do
         Traversal_Iterators.Create_Tree_Iterator
           (Root.As_Turkixir_Node, Ret);

         --  We still want to provide this functionality, even though it is
         --  unsafe. TODO: We might be able to make a safe version of this
         --  using generics. Still would be more verbose though.
         Ret.Predicate := Turkixir_Node_Predicate (Predicate);
      end return;
   end Find;

   ----------------
   -- Find_First --
   ----------------

   function Find_First
     (Root : Turkixir_Node'Class; Predicate : Turkixir_Node_Predicate'Class)
      return Turkixir_Node
   is
      I      : Traverse_Iterator'Class := Find (Root, Predicate);
      Result : Turkixir_Node;
      Ignore : Boolean;
   begin
      if not I.Next (Result) then
         Result := No_Turkixir_Node;
      end if;
      return Result;
   end Find_First;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (N : Turkixir_Node) return Turkixir_Node
   is (Parent (N));

   ------------------------------------
   -- First_Child_Index_For_Traverse --
   ------------------------------------

   function First_Child_Index_For_Traverse (N : Turkixir_Node) return Natural
   is (First_Child_Index (N));

   -----------------------------------
   -- Last_Child_Index_For_Traverse --
   -----------------------------------

   function Last_Child_Index_For_Traverse (N : Turkixir_Node) return Natural
   is (Last_Child_Index (N));

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (N : Turkixir_Node; I : Natural) return Turkixir_Node
   is (Child (N, I));

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Not_Predicate; N : Turkixir_Node) return Boolean is
   begin
      return not P.Predicate.Unchecked_Get.Evaluate (N);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out For_All_Predicate; N : Turkixir_Node) return Boolean is
   begin
      for Predicate of P.Predicates loop
         if not Predicate.Unchecked_Get.Evaluate (N) then
            return False;
         end if;
      end loop;
      return True;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out For_Some_Predicate; N : Turkixir_Node) return Boolean is
   begin
      for Predicate of P.Predicates loop
         if Predicate.Unchecked_Get.Evaluate (N) then
            return True;
         end if;
      end loop;
      return False;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out For_All_Children_Predicate; N : Turkixir_Node) return Boolean
   is
      Child_Pred : Turkixir_Node_Predicate_Interface'Class renames P.Predicate.Unchecked_Get.all;
   begin
      for I in 1 .. N.Children_Count loop
         declare
            Child : constant Turkixir_Node := N.Child (I);
         begin
            if (not P.Skip_Null or else not Child.Is_Null)
               and then not Child_Pred.Evaluate (Child)
            then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out For_Some_Children_Predicate; N : Turkixir_Node) return Boolean
   is
      Child_Pred : Turkixir_Node_Predicate_Interface'Class renames P.Predicate.Unchecked_Get.all;
   begin
      for I in 1 .. N.Children_Count loop
         declare
            Child : constant Turkixir_Node := N.Child (I);
         begin
            if (not P.Skip_Null or else not Child.Is_Null)
               and then Child_Pred.Evaluate (Child)
            then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Child_With_Predicate; N : Turkixir_Node) return Boolean is
   begin
      if N.Is_Null then
         return False;
      end if;

         declare
            Field_Index : Positive;
         begin
            --  First check that N has the requested field
            begin
               Field_Index := Index (N.Kind, P.Field);
            exception
               when Bad_Type_Error =>
                  return False;
            end;

            return P.Predicate.Unchecked_Get.Evaluate (N.Child (Field_Index));
         end;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Kind_Predicate; N : Turkixir_Node) return Boolean is
   begin
      return N.Kind in P.First .. P.Last;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Text_Predicate; N : Turkixir_Node) return Boolean
   is
      use Ada.Strings.Wide_Wide_Unbounded;
   begin
      return (if N.Is_Null
              then P.Text = ""
              else N.Text = P.Text);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Node_Is_Null_Predicate; N : Turkixir_Node) return Boolean
   is
      pragma Unreferenced (P);
   begin
      return N.Is_Null;
   end Evaluate;

   


end Libturkixirlang.Iterators;
