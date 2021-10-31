
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Packrat;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;

pragma Warnings (Off, "referenced");
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
pragma Warnings (On, "referenced");

with Langkit_Support.Text;        use Langkit_Support.Text;

with Libturkixirlang.Common;             use Libturkixirlang.Common;
use Libturkixirlang.Common.Precomputed_Symbols;

with Libturkixirlang.Implementation;     use Libturkixirlang.Implementation;

pragma Warnings (Off, "referenced");
with Libturkixirlang.Private_Converters; use Libturkixirlang.Private_Converters;
pragma Warnings (On, "referenced");



package body Libturkixirlang.Parsers is
   pragma Warnings (Off, "use clause");
   use all type Langkit_Support.Symbols.Symbol_Type;
   pragma Warnings (On, "use clause");

   --  Prepare packrat instantiations: one per enum type and onefor each kind
   --  of node (including lists). Likewise for bump ptr. allocators, except
   --  we need them only for non-abstract AST nodes.
   --
   --  In the Alloc instanciations, there are unchecked conversions to wrap
   --  System.Address values from a low-level allocator. All read/writes for
   --  the pointed values are made through values of the same access types
   --  (i.e. AST node access). Thus, strict aliasing issues should not arise
   --  for these.
   --
   --  See <https://gcc.gnu.org/onlinedocs/gnat_ugn/
   --       Optimization-and-Strict-Aliasing.html>.

   pragma Warnings (Off, "is not referenced");
   pragma Warnings (Off, "possible aliasing problem for type");
      package Bare_Turkixir_Node_Memos is new Langkit_Support.Packrat
        (Bare_Turkixir_Node, Token_Index);

      package Bare_Arg_Memos is new Langkit_Support.Packrat
        (Bare_Arg, Token_Index);

      package Bare_Arg_Assoc_Memos is new Langkit_Support.Packrat
        (Bare_Arg_Assoc, Token_Index);

         
         subtype Subtype_For_Arg_Assoc is
            Root_Node_Record (Turkixir_Arg_Assoc);
         type Access_To_Subtype_For_Arg_Assoc is access all Subtype_For_Arg_Assoc;
         package Bare_Arg_Assoc_Alloc is new Alloc
           (Subtype_For_Arg_Assoc, Access_To_Subtype_For_Arg_Assoc);

         function Allocate_Arg_Assoc
           (Pool : Bump_Ptr_Pool) return Bare_Arg_Assoc;

         function Allocate_Arg_Assoc
           (Pool : Bump_Ptr_Pool) return Bare_Arg_Assoc
         is
            Result      : constant Access_To_Subtype_For_Arg_Assoc := Bare_Arg_Assoc_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Arg_Assoc;
            return Bare_Arg_Assoc (Result);
         end Allocate_Arg_Assoc;

      package Bare_Arg_Gen_Memos is new Langkit_Support.Packrat
        (Bare_Arg_Gen, Token_Index);

         
         subtype Subtype_For_Arg_Gen is
            Root_Node_Record (Turkixir_Arg_Gen);
         type Access_To_Subtype_For_Arg_Gen is access all Subtype_For_Arg_Gen;
         package Bare_Arg_Gen_Alloc is new Alloc
           (Subtype_For_Arg_Gen, Access_To_Subtype_For_Arg_Gen);

         function Allocate_Arg_Gen
           (Pool : Bump_Ptr_Pool) return Bare_Arg_Gen;

         function Allocate_Arg_Gen
           (Pool : Bump_Ptr_Pool) return Bare_Arg_Gen
         is
            Result      : constant Access_To_Subtype_For_Arg_Gen := Bare_Arg_Gen_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Arg_Gen;
            return Bare_Arg_Gen (Result);
         end Allocate_Arg_Gen;

      package Bare_Kw_Args_Memos is new Langkit_Support.Packrat
        (Bare_Kw_Args, Token_Index);

         
         subtype Subtype_For_Kw_Args is
            Root_Node_Record (Turkixir_Kw_Args);
         type Access_To_Subtype_For_Kw_Args is access all Subtype_For_Kw_Args;
         package Bare_Kw_Args_Alloc is new Alloc
           (Subtype_For_Kw_Args, Access_To_Subtype_For_Kw_Args);

         function Allocate_Kw_Args
           (Pool : Bump_Ptr_Pool) return Bare_Kw_Args;

         function Allocate_Kw_Args
           (Pool : Bump_Ptr_Pool) return Bare_Kw_Args
         is
            Result      : constant Access_To_Subtype_For_Kw_Args := Bare_Kw_Args_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Kw_Args;
            return Bare_Kw_Args (Result);
         end Allocate_Kw_Args;

      package Bare_Var_Args_Memos is new Langkit_Support.Packrat
        (Bare_Var_Args, Token_Index);

         
         subtype Subtype_For_Var_Args is
            Root_Node_Record (Turkixir_Var_Args);
         type Access_To_Subtype_For_Var_Args is access all Subtype_For_Var_Args;
         package Bare_Var_Args_Alloc is new Alloc
           (Subtype_For_Var_Args, Access_To_Subtype_For_Var_Args);

         function Allocate_Var_Args
           (Pool : Bump_Ptr_Pool) return Bare_Var_Args;

         function Allocate_Var_Args
           (Pool : Bump_Ptr_Pool) return Bare_Var_Args
         is
            Result      : constant Access_To_Subtype_For_Var_Args := Bare_Var_Args_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Var_Args;
            return Bare_Var_Args (Result);
         end Allocate_Var_Args;

      package Bare_As_Name_Node_Memos is new Langkit_Support.Packrat
        (Bare_As_Name_Node, Token_Index);

         
         subtype Subtype_For_As_Name_Node is
            Root_Node_Record (Turkixir_As_Name_Node);
         type Access_To_Subtype_For_As_Name_Node is access all Subtype_For_As_Name_Node;
         package Bare_As_Name_Node_Alloc is new Alloc
           (Subtype_For_As_Name_Node, Access_To_Subtype_For_As_Name_Node);

         function Allocate_As_Name_Node
           (Pool : Bump_Ptr_Pool) return Bare_As_Name_Node;

         function Allocate_As_Name_Node
           (Pool : Bump_Ptr_Pool) return Bare_As_Name_Node
         is
            Result      : constant Access_To_Subtype_For_As_Name_Node := Bare_As_Name_Node_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_As_Name_Node;
            return Bare_As_Name_Node (Result);
         end Allocate_As_Name_Node;

      package Bare_Comp_If_Memos is new Langkit_Support.Packrat
        (Bare_Comp_If, Token_Index);

         
         subtype Subtype_For_Comp_If is
            Root_Node_Record (Turkixir_Comp_If);
         type Access_To_Subtype_For_Comp_If is access all Subtype_For_Comp_If;
         package Bare_Comp_If_Alloc is new Alloc
           (Subtype_For_Comp_If, Access_To_Subtype_For_Comp_If);

         function Allocate_Comp_If
           (Pool : Bump_Ptr_Pool) return Bare_Comp_If;

         function Allocate_Comp_If
           (Pool : Bump_Ptr_Pool) return Bare_Comp_If
         is
            Result      : constant Access_To_Subtype_For_Comp_If := Bare_Comp_If_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Comp_If;
            return Bare_Comp_If (Result);
         end Allocate_Comp_If;

      package Bare_Comp_Op_Kind_Memos is new Langkit_Support.Packrat
        (Bare_Comp_Op_Kind, Token_Index);

      package Bare_Comp_Op_Kind_Diamond_Memos is new Langkit_Support.Packrat
        (Bare_Comp_Op_Kind_Diamond, Token_Index);

         
         subtype Subtype_For_Comp_Op_Kind_Diamond is
            Root_Node_Record (Turkixir_Comp_Op_Kind_Diamond);
         type Access_To_Subtype_For_Comp_Op_Kind_Diamond is access all Subtype_For_Comp_Op_Kind_Diamond;
         package Bare_Comp_Op_Kind_Diamond_Alloc is new Alloc
           (Subtype_For_Comp_Op_Kind_Diamond, Access_To_Subtype_For_Comp_Op_Kind_Diamond);

         function Allocate_Comp_Op_Kind_Diamond
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Diamond;

         function Allocate_Comp_Op_Kind_Diamond
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Diamond
         is
            Result      : constant Access_To_Subtype_For_Comp_Op_Kind_Diamond := Bare_Comp_Op_Kind_Diamond_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Comp_Op_Kind_Diamond;
            return Bare_Comp_Op_Kind_Diamond (Result);
         end Allocate_Comp_Op_Kind_Diamond;

      package Bare_Comp_Op_Kind_Eq_Memos is new Langkit_Support.Packrat
        (Bare_Comp_Op_Kind_Eq, Token_Index);

         
         subtype Subtype_For_Comp_Op_Kind_Eq is
            Root_Node_Record (Turkixir_Comp_Op_Kind_Eq);
         type Access_To_Subtype_For_Comp_Op_Kind_Eq is access all Subtype_For_Comp_Op_Kind_Eq;
         package Bare_Comp_Op_Kind_Eq_Alloc is new Alloc
           (Subtype_For_Comp_Op_Kind_Eq, Access_To_Subtype_For_Comp_Op_Kind_Eq);

         function Allocate_Comp_Op_Kind_Eq
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Eq;

         function Allocate_Comp_Op_Kind_Eq
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Eq
         is
            Result      : constant Access_To_Subtype_For_Comp_Op_Kind_Eq := Bare_Comp_Op_Kind_Eq_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Comp_Op_Kind_Eq;
            return Bare_Comp_Op_Kind_Eq (Result);
         end Allocate_Comp_Op_Kind_Eq;

      package Bare_Comp_Op_Kind_Gt_Memos is new Langkit_Support.Packrat
        (Bare_Comp_Op_Kind_Gt, Token_Index);

         
         subtype Subtype_For_Comp_Op_Kind_Gt is
            Root_Node_Record (Turkixir_Comp_Op_Kind_Gt);
         type Access_To_Subtype_For_Comp_Op_Kind_Gt is access all Subtype_For_Comp_Op_Kind_Gt;
         package Bare_Comp_Op_Kind_Gt_Alloc is new Alloc
           (Subtype_For_Comp_Op_Kind_Gt, Access_To_Subtype_For_Comp_Op_Kind_Gt);

         function Allocate_Comp_Op_Kind_Gt
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Gt;

         function Allocate_Comp_Op_Kind_Gt
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Gt
         is
            Result      : constant Access_To_Subtype_For_Comp_Op_Kind_Gt := Bare_Comp_Op_Kind_Gt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Comp_Op_Kind_Gt;
            return Bare_Comp_Op_Kind_Gt (Result);
         end Allocate_Comp_Op_Kind_Gt;

      package Bare_Comp_Op_Kind_Gte_Memos is new Langkit_Support.Packrat
        (Bare_Comp_Op_Kind_Gte, Token_Index);

         
         subtype Subtype_For_Comp_Op_Kind_Gte is
            Root_Node_Record (Turkixir_Comp_Op_Kind_Gte);
         type Access_To_Subtype_For_Comp_Op_Kind_Gte is access all Subtype_For_Comp_Op_Kind_Gte;
         package Bare_Comp_Op_Kind_Gte_Alloc is new Alloc
           (Subtype_For_Comp_Op_Kind_Gte, Access_To_Subtype_For_Comp_Op_Kind_Gte);

         function Allocate_Comp_Op_Kind_Gte
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Gte;

         function Allocate_Comp_Op_Kind_Gte
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Gte
         is
            Result      : constant Access_To_Subtype_For_Comp_Op_Kind_Gte := Bare_Comp_Op_Kind_Gte_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Comp_Op_Kind_Gte;
            return Bare_Comp_Op_Kind_Gte (Result);
         end Allocate_Comp_Op_Kind_Gte;

      package Bare_Comp_Op_Kind_In_Memos is new Langkit_Support.Packrat
        (Bare_Comp_Op_Kind_In, Token_Index);

         
         subtype Subtype_For_Comp_Op_Kind_In is
            Root_Node_Record (Turkixir_Comp_Op_Kind_In);
         type Access_To_Subtype_For_Comp_Op_Kind_In is access all Subtype_For_Comp_Op_Kind_In;
         package Bare_Comp_Op_Kind_In_Alloc is new Alloc
           (Subtype_For_Comp_Op_Kind_In, Access_To_Subtype_For_Comp_Op_Kind_In);

         function Allocate_Comp_Op_Kind_In
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_In;

         function Allocate_Comp_Op_Kind_In
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_In
         is
            Result      : constant Access_To_Subtype_For_Comp_Op_Kind_In := Bare_Comp_Op_Kind_In_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Comp_Op_Kind_In;
            return Bare_Comp_Op_Kind_In (Result);
         end Allocate_Comp_Op_Kind_In;

      package Bare_Comp_Op_Kind_Is_Memos is new Langkit_Support.Packrat
        (Bare_Comp_Op_Kind_Is, Token_Index);

         
         subtype Subtype_For_Comp_Op_Kind_Is is
            Root_Node_Record (Turkixir_Comp_Op_Kind_Is);
         type Access_To_Subtype_For_Comp_Op_Kind_Is is access all Subtype_For_Comp_Op_Kind_Is;
         package Bare_Comp_Op_Kind_Is_Alloc is new Alloc
           (Subtype_For_Comp_Op_Kind_Is, Access_To_Subtype_For_Comp_Op_Kind_Is);

         function Allocate_Comp_Op_Kind_Is
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Is;

         function Allocate_Comp_Op_Kind_Is
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Is
         is
            Result      : constant Access_To_Subtype_For_Comp_Op_Kind_Is := Bare_Comp_Op_Kind_Is_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Comp_Op_Kind_Is;
            return Bare_Comp_Op_Kind_Is (Result);
         end Allocate_Comp_Op_Kind_Is;

      package Bare_Comp_Op_Kind_Isnot_Memos is new Langkit_Support.Packrat
        (Bare_Comp_Op_Kind_Isnot, Token_Index);

         
         subtype Subtype_For_Comp_Op_Kind_Isnot is
            Root_Node_Record (Turkixir_Comp_Op_Kind_Isnot);
         type Access_To_Subtype_For_Comp_Op_Kind_Isnot is access all Subtype_For_Comp_Op_Kind_Isnot;
         package Bare_Comp_Op_Kind_Isnot_Alloc is new Alloc
           (Subtype_For_Comp_Op_Kind_Isnot, Access_To_Subtype_For_Comp_Op_Kind_Isnot);

         function Allocate_Comp_Op_Kind_Isnot
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Isnot;

         function Allocate_Comp_Op_Kind_Isnot
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Isnot
         is
            Result      : constant Access_To_Subtype_For_Comp_Op_Kind_Isnot := Bare_Comp_Op_Kind_Isnot_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Comp_Op_Kind_Isnot;
            return Bare_Comp_Op_Kind_Isnot (Result);
         end Allocate_Comp_Op_Kind_Isnot;

      package Bare_Comp_Op_Kind_Lt_Memos is new Langkit_Support.Packrat
        (Bare_Comp_Op_Kind_Lt, Token_Index);

         
         subtype Subtype_For_Comp_Op_Kind_Lt is
            Root_Node_Record (Turkixir_Comp_Op_Kind_Lt);
         type Access_To_Subtype_For_Comp_Op_Kind_Lt is access all Subtype_For_Comp_Op_Kind_Lt;
         package Bare_Comp_Op_Kind_Lt_Alloc is new Alloc
           (Subtype_For_Comp_Op_Kind_Lt, Access_To_Subtype_For_Comp_Op_Kind_Lt);

         function Allocate_Comp_Op_Kind_Lt
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Lt;

         function Allocate_Comp_Op_Kind_Lt
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Lt
         is
            Result      : constant Access_To_Subtype_For_Comp_Op_Kind_Lt := Bare_Comp_Op_Kind_Lt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Comp_Op_Kind_Lt;
            return Bare_Comp_Op_Kind_Lt (Result);
         end Allocate_Comp_Op_Kind_Lt;

      package Bare_Comp_Op_Kind_Lte_Memos is new Langkit_Support.Packrat
        (Bare_Comp_Op_Kind_Lte, Token_Index);

         
         subtype Subtype_For_Comp_Op_Kind_Lte is
            Root_Node_Record (Turkixir_Comp_Op_Kind_Lte);
         type Access_To_Subtype_For_Comp_Op_Kind_Lte is access all Subtype_For_Comp_Op_Kind_Lte;
         package Bare_Comp_Op_Kind_Lte_Alloc is new Alloc
           (Subtype_For_Comp_Op_Kind_Lte, Access_To_Subtype_For_Comp_Op_Kind_Lte);

         function Allocate_Comp_Op_Kind_Lte
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Lte;

         function Allocate_Comp_Op_Kind_Lte
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Lte
         is
            Result      : constant Access_To_Subtype_For_Comp_Op_Kind_Lte := Bare_Comp_Op_Kind_Lte_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Comp_Op_Kind_Lte;
            return Bare_Comp_Op_Kind_Lte (Result);
         end Allocate_Comp_Op_Kind_Lte;

      package Bare_Comp_Op_Kind_Noteq_Memos is new Langkit_Support.Packrat
        (Bare_Comp_Op_Kind_Noteq, Token_Index);

         
         subtype Subtype_For_Comp_Op_Kind_Noteq is
            Root_Node_Record (Turkixir_Comp_Op_Kind_Noteq);
         type Access_To_Subtype_For_Comp_Op_Kind_Noteq is access all Subtype_For_Comp_Op_Kind_Noteq;
         package Bare_Comp_Op_Kind_Noteq_Alloc is new Alloc
           (Subtype_For_Comp_Op_Kind_Noteq, Access_To_Subtype_For_Comp_Op_Kind_Noteq);

         function Allocate_Comp_Op_Kind_Noteq
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Noteq;

         function Allocate_Comp_Op_Kind_Noteq
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Noteq
         is
            Result      : constant Access_To_Subtype_For_Comp_Op_Kind_Noteq := Bare_Comp_Op_Kind_Noteq_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Comp_Op_Kind_Noteq;
            return Bare_Comp_Op_Kind_Noteq (Result);
         end Allocate_Comp_Op_Kind_Noteq;

      package Bare_Comp_Op_Kind_Notin_Memos is new Langkit_Support.Packrat
        (Bare_Comp_Op_Kind_Notin, Token_Index);

         
         subtype Subtype_For_Comp_Op_Kind_Notin is
            Root_Node_Record (Turkixir_Comp_Op_Kind_Notin);
         type Access_To_Subtype_For_Comp_Op_Kind_Notin is access all Subtype_For_Comp_Op_Kind_Notin;
         package Bare_Comp_Op_Kind_Notin_Alloc is new Alloc
           (Subtype_For_Comp_Op_Kind_Notin, Access_To_Subtype_For_Comp_Op_Kind_Notin);

         function Allocate_Comp_Op_Kind_Notin
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Notin;

         function Allocate_Comp_Op_Kind_Notin
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op_Kind_Notin
         is
            Result      : constant Access_To_Subtype_For_Comp_Op_Kind_Notin := Bare_Comp_Op_Kind_Notin_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Comp_Op_Kind_Notin;
            return Bare_Comp_Op_Kind_Notin (Result);
         end Allocate_Comp_Op_Kind_Notin;

      package Bare_Comprehension_Memos is new Langkit_Support.Packrat
        (Bare_Comprehension, Token_Index);

      package Bare_Comp_For_Memos is new Langkit_Support.Packrat
        (Bare_Comp_For, Token_Index);

         
         subtype Subtype_For_Comp_For is
            Root_Node_Record (Turkixir_Comp_For);
         type Access_To_Subtype_For_Comp_For is access all Subtype_For_Comp_For;
         package Bare_Comp_For_Alloc is new Alloc
           (Subtype_For_Comp_For, Access_To_Subtype_For_Comp_For);

         function Allocate_Comp_For
           (Pool : Bump_Ptr_Pool) return Bare_Comp_For;

         function Allocate_Comp_For
           (Pool : Bump_Ptr_Pool) return Bare_Comp_For
         is
            Result      : constant Access_To_Subtype_For_Comp_For := Bare_Comp_For_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Comp_For;
            return Bare_Comp_For (Result);
         end Allocate_Comp_For;

      package Bare_Comp_ForL_Memos is new Langkit_Support.Packrat
        (Bare_Comp_ForL, Token_Index);

         
         subtype Subtype_For_Comp_ForL is
            Root_Node_Record (Turkixir_Comp_ForL);
         type Access_To_Subtype_For_Comp_ForL is access all Subtype_For_Comp_ForL;
         package Bare_Comp_ForL_Alloc is new Alloc
           (Subtype_For_Comp_ForL, Access_To_Subtype_For_Comp_ForL);

         function Allocate_Comp_ForL
           (Pool : Bump_Ptr_Pool) return Bare_Comp_ForL;

         function Allocate_Comp_ForL
           (Pool : Bump_Ptr_Pool) return Bare_Comp_ForL
         is
            Result      : constant Access_To_Subtype_For_Comp_ForL := Bare_Comp_ForL_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Comp_ForL;
            return Bare_Comp_ForL (Result);
         end Allocate_Comp_ForL;

      package Bare_Decorator_Memos is new Langkit_Support.Packrat
        (Bare_Decorator, Token_Index);

         
         subtype Subtype_For_Decorator is
            Root_Node_Record (Turkixir_Decorator);
         type Access_To_Subtype_For_Decorator is access all Subtype_For_Decorator;
         package Bare_Decorator_Alloc is new Alloc
           (Subtype_For_Decorator, Access_To_Subtype_For_Decorator);

         function Allocate_Decorator
           (Pool : Bump_Ptr_Pool) return Bare_Decorator;

         function Allocate_Decorator
           (Pool : Bump_Ptr_Pool) return Bare_Decorator
         is
            Result      : constant Access_To_Subtype_For_Decorator := Bare_Decorator_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Decorator;
            return Bare_Decorator (Result);
         end Allocate_Decorator;

      package Bare_Dict_Assoc_Memos is new Langkit_Support.Packrat
        (Bare_Dict_Assoc, Token_Index);

         
         subtype Subtype_For_Dict_Assoc is
            Root_Node_Record (Turkixir_Dict_Assoc);
         type Access_To_Subtype_For_Dict_Assoc is access all Subtype_For_Dict_Assoc;
         package Bare_Dict_Assoc_Alloc is new Alloc
           (Subtype_For_Dict_Assoc, Access_To_Subtype_For_Dict_Assoc);

         function Allocate_Dict_Assoc
           (Pool : Bump_Ptr_Pool) return Bare_Dict_Assoc;

         function Allocate_Dict_Assoc
           (Pool : Bump_Ptr_Pool) return Bare_Dict_Assoc
         is
            Result      : constant Access_To_Subtype_For_Dict_Assoc := Bare_Dict_Assoc_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Dict_Assoc;
            return Bare_Dict_Assoc (Result);
         end Allocate_Dict_Assoc;

      package Bare_Else_Part_Memos is new Langkit_Support.Packrat
        (Bare_Else_Part, Token_Index);

         
         subtype Subtype_For_Else_Part is
            Root_Node_Record (Turkixir_Else_Part);
         type Access_To_Subtype_For_Else_Part is access all Subtype_For_Else_Part;
         package Bare_Else_Part_Alloc is new Alloc
           (Subtype_For_Else_Part, Access_To_Subtype_For_Else_Part);

         function Allocate_Else_Part
           (Pool : Bump_Ptr_Pool) return Bare_Else_Part;

         function Allocate_Else_Part
           (Pool : Bump_Ptr_Pool) return Bare_Else_Part
         is
            Result      : constant Access_To_Subtype_For_Else_Part := Bare_Else_Part_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Else_Part;
            return Bare_Else_Part (Result);
         end Allocate_Else_Part;

      package Bare_Except_Part_Memos is new Langkit_Support.Packrat
        (Bare_Except_Part, Token_Index);

         
         subtype Subtype_For_Except_Part is
            Root_Node_Record (Turkixir_Except_Part);
         type Access_To_Subtype_For_Except_Part is access all Subtype_For_Except_Part;
         package Bare_Except_Part_Alloc is new Alloc
           (Subtype_For_Except_Part, Access_To_Subtype_For_Except_Part);

         function Allocate_Except_Part
           (Pool : Bump_Ptr_Pool) return Bare_Except_Part;

         function Allocate_Except_Part
           (Pool : Bump_Ptr_Pool) return Bare_Except_Part
         is
            Result      : constant Access_To_Subtype_For_Except_Part := Bare_Except_Part_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Except_Part;
            return Bare_Except_Part (Result);
         end Allocate_Except_Part;

      package Bare_Expr_Memos is new Langkit_Support.Packrat
        (Bare_Expr, Token_Index);

      package Bare_And_Expr_Memos is new Langkit_Support.Packrat
        (Bare_And_Expr, Token_Index);

         
         subtype Subtype_For_And_Expr is
            Root_Node_Record (Turkixir_And_Expr);
         type Access_To_Subtype_For_And_Expr is access all Subtype_For_And_Expr;
         package Bare_And_Expr_Alloc is new Alloc
           (Subtype_For_And_Expr, Access_To_Subtype_For_And_Expr);

         function Allocate_And_Expr
           (Pool : Bump_Ptr_Pool) return Bare_And_Expr;

         function Allocate_And_Expr
           (Pool : Bump_Ptr_Pool) return Bare_And_Expr
         is
            Result      : constant Access_To_Subtype_For_And_Expr := Bare_And_Expr_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_And_Expr;
            return Bare_And_Expr (Result);
         end Allocate_And_Expr;

      package Bare_And_Op_Memos is new Langkit_Support.Packrat
        (Bare_And_Op, Token_Index);

         
         subtype Subtype_For_And_Op is
            Root_Node_Record (Turkixir_And_Op);
         type Access_To_Subtype_For_And_Op is access all Subtype_For_And_Op;
         package Bare_And_Op_Alloc is new Alloc
           (Subtype_For_And_Op, Access_To_Subtype_For_And_Op);

         function Allocate_And_Op
           (Pool : Bump_Ptr_Pool) return Bare_And_Op;

         function Allocate_And_Op
           (Pool : Bump_Ptr_Pool) return Bare_And_Op
         is
            Result      : constant Access_To_Subtype_For_And_Op := Bare_And_Op_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_And_Op;
            return Bare_And_Op (Result);
         end Allocate_And_Op;

      package Bare_Bin_Op_Memos is new Langkit_Support.Packrat
        (Bare_Bin_Op, Token_Index);

      package Bare_Arith_Expr_Memos is new Langkit_Support.Packrat
        (Bare_Arith_Expr, Token_Index);

         
         subtype Subtype_For_Arith_Expr is
            Root_Node_Record (Turkixir_Arith_Expr);
         type Access_To_Subtype_For_Arith_Expr is access all Subtype_For_Arith_Expr;
         package Bare_Arith_Expr_Alloc is new Alloc
           (Subtype_For_Arith_Expr, Access_To_Subtype_For_Arith_Expr);

         function Allocate_Arith_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Arith_Expr;

         function Allocate_Arith_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Arith_Expr
         is
            Result      : constant Access_To_Subtype_For_Arith_Expr := Bare_Arith_Expr_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Arith_Expr;
            return Bare_Arith_Expr (Result);
         end Allocate_Arith_Expr;

      package Bare_Shift_Expr_Memos is new Langkit_Support.Packrat
        (Bare_Shift_Expr, Token_Index);

         
         subtype Subtype_For_Shift_Expr is
            Root_Node_Record (Turkixir_Shift_Expr);
         type Access_To_Subtype_For_Shift_Expr is access all Subtype_For_Shift_Expr;
         package Bare_Shift_Expr_Alloc is new Alloc
           (Subtype_For_Shift_Expr, Access_To_Subtype_For_Shift_Expr);

         function Allocate_Shift_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Shift_Expr;

         function Allocate_Shift_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Shift_Expr
         is
            Result      : constant Access_To_Subtype_For_Shift_Expr := Bare_Shift_Expr_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Shift_Expr;
            return Bare_Shift_Expr (Result);
         end Allocate_Shift_Expr;

      package Bare_Term_Memos is new Langkit_Support.Packrat
        (Bare_Term, Token_Index);

         
         subtype Subtype_For_Term is
            Root_Node_Record (Turkixir_Term);
         type Access_To_Subtype_For_Term is access all Subtype_For_Term;
         package Bare_Term_Alloc is new Alloc
           (Subtype_For_Term, Access_To_Subtype_For_Term);

         function Allocate_Term
           (Pool : Bump_Ptr_Pool) return Bare_Term;

         function Allocate_Term
           (Pool : Bump_Ptr_Pool) return Bare_Term
         is
            Result      : constant Access_To_Subtype_For_Term := Bare_Term_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Term;
            return Bare_Term (Result);
         end Allocate_Term;

      package Bare_Call_Expr_Memos is new Langkit_Support.Packrat
        (Bare_Call_Expr, Token_Index);

         
         subtype Subtype_For_Call_Expr is
            Root_Node_Record (Turkixir_Call_Expr);
         type Access_To_Subtype_For_Call_Expr is access all Subtype_For_Call_Expr;
         package Bare_Call_Expr_Alloc is new Alloc
           (Subtype_For_Call_Expr, Access_To_Subtype_For_Call_Expr);

         function Allocate_Call_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Call_Expr;

         function Allocate_Call_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Call_Expr
         is
            Result      : constant Access_To_Subtype_For_Call_Expr := Bare_Call_Expr_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Call_Expr;
            return Bare_Call_Expr (Result);
         end Allocate_Call_Expr;

      package Bare_Comp_Op_Memos is new Langkit_Support.Packrat
        (Bare_Comp_Op, Token_Index);

         
         subtype Subtype_For_Comp_Op is
            Root_Node_Record (Turkixir_Comp_Op);
         type Access_To_Subtype_For_Comp_Op is access all Subtype_For_Comp_Op;
         package Bare_Comp_Op_Alloc is new Alloc
           (Subtype_For_Comp_Op, Access_To_Subtype_For_Comp_Op);

         function Allocate_Comp_Op
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op;

         function Allocate_Comp_Op
           (Pool : Bump_Ptr_Pool) return Bare_Comp_Op
         is
            Result      : constant Access_To_Subtype_For_Comp_Op := Bare_Comp_Op_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Comp_Op;
            return Bare_Comp_Op (Result);
         end Allocate_Comp_Op;

      package Bare_Concat_String_Lit_Memos is new Langkit_Support.Packrat
        (Bare_Concat_String_Lit, Token_Index);

         
         subtype Subtype_For_Concat_String_Lit is
            Root_Node_Record (Turkixir_Concat_String_Lit);
         type Access_To_Subtype_For_Concat_String_Lit is access all Subtype_For_Concat_String_Lit;
         package Bare_Concat_String_Lit_Alloc is new Alloc
           (Subtype_For_Concat_String_Lit, Access_To_Subtype_For_Concat_String_Lit);

         function Allocate_Concat_String_Lit
           (Pool : Bump_Ptr_Pool) return Bare_Concat_String_Lit;

         function Allocate_Concat_String_Lit
           (Pool : Bump_Ptr_Pool) return Bare_Concat_String_Lit
         is
            Result      : constant Access_To_Subtype_For_Concat_String_Lit := Bare_Concat_String_Lit_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Concat_String_Lit;
            return Bare_Concat_String_Lit (Result);
         end Allocate_Concat_String_Lit;

      package Bare_Dict_Comp_Memos is new Langkit_Support.Packrat
        (Bare_Dict_Comp, Token_Index);

         
         subtype Subtype_For_Dict_Comp is
            Root_Node_Record (Turkixir_Dict_Comp);
         type Access_To_Subtype_For_Dict_Comp is access all Subtype_For_Dict_Comp;
         package Bare_Dict_Comp_Alloc is new Alloc
           (Subtype_For_Dict_Comp, Access_To_Subtype_For_Dict_Comp);

         function Allocate_Dict_Comp
           (Pool : Bump_Ptr_Pool) return Bare_Dict_Comp;

         function Allocate_Dict_Comp
           (Pool : Bump_Ptr_Pool) return Bare_Dict_Comp
         is
            Result      : constant Access_To_Subtype_For_Dict_Comp := Bare_Dict_Comp_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Dict_Comp;
            return Bare_Dict_Comp (Result);
         end Allocate_Dict_Comp;

      package Bare_Dict_Lit_Memos is new Langkit_Support.Packrat
        (Bare_Dict_Lit, Token_Index);

         
         subtype Subtype_For_Dict_Lit is
            Root_Node_Record (Turkixir_Dict_Lit);
         type Access_To_Subtype_For_Dict_Lit is access all Subtype_For_Dict_Lit;
         package Bare_Dict_Lit_Alloc is new Alloc
           (Subtype_For_Dict_Lit, Access_To_Subtype_For_Dict_Lit);

         function Allocate_Dict_Lit
           (Pool : Bump_Ptr_Pool) return Bare_Dict_Lit;

         function Allocate_Dict_Lit
           (Pool : Bump_Ptr_Pool) return Bare_Dict_Lit
         is
            Result      : constant Access_To_Subtype_For_Dict_Lit := Bare_Dict_Lit_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Dict_Lit;
            return Bare_Dict_Lit (Result);
         end Allocate_Dict_Lit;

      package Bare_Dot_Memos is new Langkit_Support.Packrat
        (Bare_Dot, Token_Index);

         
         subtype Subtype_For_Dot is
            Root_Node_Record (Turkixir_Dot);
         type Access_To_Subtype_For_Dot is access all Subtype_For_Dot;
         package Bare_Dot_Alloc is new Alloc
           (Subtype_For_Dot, Access_To_Subtype_For_Dot);

         function Allocate_Dot
           (Pool : Bump_Ptr_Pool) return Bare_Dot;

         function Allocate_Dot
           (Pool : Bump_Ptr_Pool) return Bare_Dot
         is
            Result      : constant Access_To_Subtype_For_Dot := Bare_Dot_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Dot;
            return Bare_Dot (Result);
         end Allocate_Dot;

      package Bare_Ellipsis_Expr_Memos is new Langkit_Support.Packrat
        (Bare_Ellipsis_Expr, Token_Index);

         
         subtype Subtype_For_Ellipsis_Expr is
            Root_Node_Record (Turkixir_Ellipsis_Expr);
         type Access_To_Subtype_For_Ellipsis_Expr is access all Subtype_For_Ellipsis_Expr;
         package Bare_Ellipsis_Expr_Alloc is new Alloc
           (Subtype_For_Ellipsis_Expr, Access_To_Subtype_For_Ellipsis_Expr);

         function Allocate_Ellipsis_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Ellipsis_Expr;

         function Allocate_Ellipsis_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Ellipsis_Expr
         is
            Result      : constant Access_To_Subtype_For_Ellipsis_Expr := Bare_Ellipsis_Expr_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Ellipsis_Expr;
            return Bare_Ellipsis_Expr (Result);
         end Allocate_Ellipsis_Expr;

      package Bare_Factor_Memos is new Langkit_Support.Packrat
        (Bare_Factor, Token_Index);

         
         subtype Subtype_For_Factor is
            Root_Node_Record (Turkixir_Factor);
         type Access_To_Subtype_For_Factor is access all Subtype_For_Factor;
         package Bare_Factor_Alloc is new Alloc
           (Subtype_For_Factor, Access_To_Subtype_For_Factor);

         function Allocate_Factor
           (Pool : Bump_Ptr_Pool) return Bare_Factor;

         function Allocate_Factor
           (Pool : Bump_Ptr_Pool) return Bare_Factor
         is
            Result      : constant Access_To_Subtype_For_Factor := Bare_Factor_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Factor;
            return Bare_Factor (Result);
         end Allocate_Factor;

      package Bare_If_Expr_Memos is new Langkit_Support.Packrat
        (Bare_If_Expr, Token_Index);

         
         subtype Subtype_For_If_Expr is
            Root_Node_Record (Turkixir_If_Expr);
         type Access_To_Subtype_For_If_Expr is access all Subtype_For_If_Expr;
         package Bare_If_Expr_Alloc is new Alloc
           (Subtype_For_If_Expr, Access_To_Subtype_For_If_Expr);

         function Allocate_If_Expr
           (Pool : Bump_Ptr_Pool) return Bare_If_Expr;

         function Allocate_If_Expr
           (Pool : Bump_Ptr_Pool) return Bare_If_Expr
         is
            Result      : constant Access_To_Subtype_For_If_Expr := Bare_If_Expr_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_If_Expr;
            return Bare_If_Expr (Result);
         end Allocate_If_Expr;

      package Bare_Inline_Eval_Memos is new Langkit_Support.Packrat
        (Bare_Inline_Eval, Token_Index);

         
         subtype Subtype_For_Inline_Eval is
            Root_Node_Record (Turkixir_Inline_Eval);
         type Access_To_Subtype_For_Inline_Eval is access all Subtype_For_Inline_Eval;
         package Bare_Inline_Eval_Alloc is new Alloc
           (Subtype_For_Inline_Eval, Access_To_Subtype_For_Inline_Eval);

         function Allocate_Inline_Eval
           (Pool : Bump_Ptr_Pool) return Bare_Inline_Eval;

         function Allocate_Inline_Eval
           (Pool : Bump_Ptr_Pool) return Bare_Inline_Eval
         is
            Result      : constant Access_To_Subtype_For_Inline_Eval := Bare_Inline_Eval_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Inline_Eval;
            return Bare_Inline_Eval (Result);
         end Allocate_Inline_Eval;

      package Bare_Lambda_Def_Memos is new Langkit_Support.Packrat
        (Bare_Lambda_Def, Token_Index);

         
         subtype Subtype_For_Lambda_Def is
            Root_Node_Record (Turkixir_Lambda_Def);
         type Access_To_Subtype_For_Lambda_Def is access all Subtype_For_Lambda_Def;
         package Bare_Lambda_Def_Alloc is new Alloc
           (Subtype_For_Lambda_Def, Access_To_Subtype_For_Lambda_Def);

         function Allocate_Lambda_Def
           (Pool : Bump_Ptr_Pool) return Bare_Lambda_Def;

         function Allocate_Lambda_Def
           (Pool : Bump_Ptr_Pool) return Bare_Lambda_Def
         is
            Result      : constant Access_To_Subtype_For_Lambda_Def := Bare_Lambda_Def_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Lambda_Def;
            return Bare_Lambda_Def (Result);
         end Allocate_Lambda_Def;

      package Bare_List_Comp_Memos is new Langkit_Support.Packrat
        (Bare_List_Comp, Token_Index);

         
         subtype Subtype_For_List_Comp is
            Root_Node_Record (Turkixir_List_Comp);
         type Access_To_Subtype_For_List_Comp is access all Subtype_For_List_Comp;
         package Bare_List_Comp_Alloc is new Alloc
           (Subtype_For_List_Comp, Access_To_Subtype_For_List_Comp);

         function Allocate_List_Comp
           (Pool : Bump_Ptr_Pool) return Bare_List_Comp;

         function Allocate_List_Comp
           (Pool : Bump_Ptr_Pool) return Bare_List_Comp
         is
            Result      : constant Access_To_Subtype_For_List_Comp := Bare_List_Comp_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_List_Comp;
            return Bare_List_Comp (Result);
         end Allocate_List_Comp;

      package Bare_List_Gen_Memos is new Langkit_Support.Packrat
        (Bare_List_Gen, Token_Index);

         
         subtype Subtype_For_List_Gen is
            Root_Node_Record (Turkixir_List_Gen);
         type Access_To_Subtype_For_List_Gen is access all Subtype_For_List_Gen;
         package Bare_List_Gen_Alloc is new Alloc
           (Subtype_For_List_Gen, Access_To_Subtype_For_List_Gen);

         function Allocate_List_Gen
           (Pool : Bump_Ptr_Pool) return Bare_List_Gen;

         function Allocate_List_Gen
           (Pool : Bump_Ptr_Pool) return Bare_List_Gen
         is
            Result      : constant Access_To_Subtype_For_List_Gen := Bare_List_Gen_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_List_Gen;
            return Bare_List_Gen (Result);
         end Allocate_List_Gen;

      package Bare_List_Lit_Memos is new Langkit_Support.Packrat
        (Bare_List_Lit, Token_Index);

         
         subtype Subtype_For_List_Lit is
            Root_Node_Record (Turkixir_List_Lit);
         type Access_To_Subtype_For_List_Lit is access all Subtype_For_List_Lit;
         package Bare_List_Lit_Alloc is new Alloc
           (Subtype_For_List_Lit, Access_To_Subtype_For_List_Lit);

         function Allocate_List_Lit
           (Pool : Bump_Ptr_Pool) return Bare_List_Lit;

         function Allocate_List_Lit
           (Pool : Bump_Ptr_Pool) return Bare_List_Lit
         is
            Result      : constant Access_To_Subtype_For_List_Lit := Bare_List_Lit_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_List_Lit;
            return Bare_List_Lit (Result);
         end Allocate_List_Lit;

      package Bare_Name_Memos is new Langkit_Support.Packrat
        (Bare_Name, Token_Index);

      package Bare_Dotted_Name_Memos is new Langkit_Support.Packrat
        (Bare_Dotted_Name, Token_Index);

         
         subtype Subtype_For_Dotted_Name is
            Root_Node_Record (Turkixir_Dotted_Name);
         type Access_To_Subtype_For_Dotted_Name is access all Subtype_For_Dotted_Name;
         package Bare_Dotted_Name_Alloc is new Alloc
           (Subtype_For_Dotted_Name, Access_To_Subtype_For_Dotted_Name);

         function Allocate_Dotted_Name
           (Pool : Bump_Ptr_Pool) return Bare_Dotted_Name;

         function Allocate_Dotted_Name
           (Pool : Bump_Ptr_Pool) return Bare_Dotted_Name
         is
            Result      : constant Access_To_Subtype_For_Dotted_Name := Bare_Dotted_Name_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Dotted_Name;
            return Bare_Dotted_Name (Result);
         end Allocate_Dotted_Name;

      package Bare_Id_Memos is new Langkit_Support.Packrat
        (Bare_Id, Token_Index);

         
         subtype Subtype_For_Id is
            Root_Node_Record (Turkixir_Id);
         type Access_To_Subtype_For_Id is access all Subtype_For_Id;
         package Bare_Id_Alloc is new Alloc
           (Subtype_For_Id, Access_To_Subtype_For_Id);

         function Allocate_Id
           (Pool : Bump_Ptr_Pool) return Bare_Id;

         function Allocate_Id
           (Pool : Bump_Ptr_Pool) return Bare_Id
         is
            Result      : constant Access_To_Subtype_For_Id := Bare_Id_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Id;
            return Bare_Id (Result);
         end Allocate_Id;

      package Bare_Not_Op_Memos is new Langkit_Support.Packrat
        (Bare_Not_Op, Token_Index);

         
         subtype Subtype_For_Not_Op is
            Root_Node_Record (Turkixir_Not_Op);
         type Access_To_Subtype_For_Not_Op is access all Subtype_For_Not_Op;
         package Bare_Not_Op_Alloc is new Alloc
           (Subtype_For_Not_Op, Access_To_Subtype_For_Not_Op);

         function Allocate_Not_Op
           (Pool : Bump_Ptr_Pool) return Bare_Not_Op;

         function Allocate_Not_Op
           (Pool : Bump_Ptr_Pool) return Bare_Not_Op
         is
            Result      : constant Access_To_Subtype_For_Not_Op := Bare_Not_Op_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Not_Op;
            return Bare_Not_Op (Result);
         end Allocate_Not_Op;

      package Bare_Number_Lit_Memos is new Langkit_Support.Packrat
        (Bare_Number_Lit, Token_Index);

         
         subtype Subtype_For_Number_Lit is
            Root_Node_Record (Turkixir_Number_Lit);
         type Access_To_Subtype_For_Number_Lit is access all Subtype_For_Number_Lit;
         package Bare_Number_Lit_Alloc is new Alloc
           (Subtype_For_Number_Lit, Access_To_Subtype_For_Number_Lit);

         function Allocate_Number_Lit
           (Pool : Bump_Ptr_Pool) return Bare_Number_Lit;

         function Allocate_Number_Lit
           (Pool : Bump_Ptr_Pool) return Bare_Number_Lit
         is
            Result      : constant Access_To_Subtype_For_Number_Lit := Bare_Number_Lit_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Number_Lit;
            return Bare_Number_Lit (Result);
         end Allocate_Number_Lit;

      package Bare_Or_Expr_Memos is new Langkit_Support.Packrat
        (Bare_Or_Expr, Token_Index);

         
         subtype Subtype_For_Or_Expr is
            Root_Node_Record (Turkixir_Or_Expr);
         type Access_To_Subtype_For_Or_Expr is access all Subtype_For_Or_Expr;
         package Bare_Or_Expr_Alloc is new Alloc
           (Subtype_For_Or_Expr, Access_To_Subtype_For_Or_Expr);

         function Allocate_Or_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Or_Expr;

         function Allocate_Or_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Or_Expr
         is
            Result      : constant Access_To_Subtype_For_Or_Expr := Bare_Or_Expr_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Or_Expr;
            return Bare_Or_Expr (Result);
         end Allocate_Or_Expr;

      package Bare_Or_Op_Memos is new Langkit_Support.Packrat
        (Bare_Or_Op, Token_Index);

         
         subtype Subtype_For_Or_Op is
            Root_Node_Record (Turkixir_Or_Op);
         type Access_To_Subtype_For_Or_Op is access all Subtype_For_Or_Op;
         package Bare_Or_Op_Alloc is new Alloc
           (Subtype_For_Or_Op, Access_To_Subtype_For_Or_Op);

         function Allocate_Or_Op
           (Pool : Bump_Ptr_Pool) return Bare_Or_Op;

         function Allocate_Or_Op
           (Pool : Bump_Ptr_Pool) return Bare_Or_Op
         is
            Result      : constant Access_To_Subtype_For_Or_Op := Bare_Or_Op_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Or_Op;
            return Bare_Or_Op (Result);
         end Allocate_Or_Op;

      package Bare_Power_Memos is new Langkit_Support.Packrat
        (Bare_Power, Token_Index);

         
         subtype Subtype_For_Power is
            Root_Node_Record (Turkixir_Power);
         type Access_To_Subtype_For_Power is access all Subtype_For_Power;
         package Bare_Power_Alloc is new Alloc
           (Subtype_For_Power, Access_To_Subtype_For_Power);

         function Allocate_Power
           (Pool : Bump_Ptr_Pool) return Bare_Power;

         function Allocate_Power
           (Pool : Bump_Ptr_Pool) return Bare_Power
         is
            Result      : constant Access_To_Subtype_For_Power := Bare_Power_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Power;
            return Bare_Power (Result);
         end Allocate_Power;

      package Bare_Set_Comp_Memos is new Langkit_Support.Packrat
        (Bare_Set_Comp, Token_Index);

         
         subtype Subtype_For_Set_Comp is
            Root_Node_Record (Turkixir_Set_Comp);
         type Access_To_Subtype_For_Set_Comp is access all Subtype_For_Set_Comp;
         package Bare_Set_Comp_Alloc is new Alloc
           (Subtype_For_Set_Comp, Access_To_Subtype_For_Set_Comp);

         function Allocate_Set_Comp
           (Pool : Bump_Ptr_Pool) return Bare_Set_Comp;

         function Allocate_Set_Comp
           (Pool : Bump_Ptr_Pool) return Bare_Set_Comp
         is
            Result      : constant Access_To_Subtype_For_Set_Comp := Bare_Set_Comp_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Set_Comp;
            return Bare_Set_Comp (Result);
         end Allocate_Set_Comp;

      package Bare_Set_Lit_Memos is new Langkit_Support.Packrat
        (Bare_Set_Lit, Token_Index);

         
         subtype Subtype_For_Set_Lit is
            Root_Node_Record (Turkixir_Set_Lit);
         type Access_To_Subtype_For_Set_Lit is access all Subtype_For_Set_Lit;
         package Bare_Set_Lit_Alloc is new Alloc
           (Subtype_For_Set_Lit, Access_To_Subtype_For_Set_Lit);

         function Allocate_Set_Lit
           (Pool : Bump_Ptr_Pool) return Bare_Set_Lit;

         function Allocate_Set_Lit
           (Pool : Bump_Ptr_Pool) return Bare_Set_Lit
         is
            Result      : constant Access_To_Subtype_For_Set_Lit := Bare_Set_Lit_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Set_Lit;
            return Bare_Set_Lit (Result);
         end Allocate_Set_Lit;

      package Bare_Slice_Expr_Memos is new Langkit_Support.Packrat
        (Bare_Slice_Expr, Token_Index);

         
         subtype Subtype_For_Slice_Expr is
            Root_Node_Record (Turkixir_Slice_Expr);
         type Access_To_Subtype_For_Slice_Expr is access all Subtype_For_Slice_Expr;
         package Bare_Slice_Expr_Alloc is new Alloc
           (Subtype_For_Slice_Expr, Access_To_Subtype_For_Slice_Expr);

         function Allocate_Slice_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Slice_Expr;

         function Allocate_Slice_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Slice_Expr
         is
            Result      : constant Access_To_Subtype_For_Slice_Expr := Bare_Slice_Expr_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Slice_Expr;
            return Bare_Slice_Expr (Result);
         end Allocate_Slice_Expr;

      package Bare_Ext_Slice_Expr_Memos is new Langkit_Support.Packrat
        (Bare_Ext_Slice_Expr, Token_Index);

         
         subtype Subtype_For_Ext_Slice_Expr is
            Root_Node_Record (Turkixir_Ext_Slice_Expr);
         type Access_To_Subtype_For_Ext_Slice_Expr is access all Subtype_For_Ext_Slice_Expr;
         package Bare_Ext_Slice_Expr_Alloc is new Alloc
           (Subtype_For_Ext_Slice_Expr, Access_To_Subtype_For_Ext_Slice_Expr);

         function Allocate_Ext_Slice_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Ext_Slice_Expr;

         function Allocate_Ext_Slice_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Ext_Slice_Expr
         is
            Result      : constant Access_To_Subtype_For_Ext_Slice_Expr := Bare_Ext_Slice_Expr_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Ext_Slice_Expr;
            return Bare_Ext_Slice_Expr (Result);
         end Allocate_Ext_Slice_Expr;

      package Bare_String_Lit_Memos is new Langkit_Support.Packrat
        (Bare_String_Lit, Token_Index);

         
         subtype Subtype_For_String_Lit is
            Root_Node_Record (Turkixir_String_Lit);
         type Access_To_Subtype_For_String_Lit is access all Subtype_For_String_Lit;
         package Bare_String_Lit_Alloc is new Alloc
           (Subtype_For_String_Lit, Access_To_Subtype_For_String_Lit);

         function Allocate_String_Lit
           (Pool : Bump_Ptr_Pool) return Bare_String_Lit;

         function Allocate_String_Lit
           (Pool : Bump_Ptr_Pool) return Bare_String_Lit
         is
            Result      : constant Access_To_Subtype_For_String_Lit := Bare_String_Lit_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_String_Lit;
            return Bare_String_Lit (Result);
         end Allocate_String_Lit;

      package Bare_Subscript_Expr_Memos is new Langkit_Support.Packrat
        (Bare_Subscript_Expr, Token_Index);

         
         subtype Subtype_For_Subscript_Expr is
            Root_Node_Record (Turkixir_Subscript_Expr);
         type Access_To_Subtype_For_Subscript_Expr is access all Subtype_For_Subscript_Expr;
         package Bare_Subscript_Expr_Alloc is new Alloc
           (Subtype_For_Subscript_Expr, Access_To_Subtype_For_Subscript_Expr);

         function Allocate_Subscript_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Subscript_Expr;

         function Allocate_Subscript_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Subscript_Expr
         is
            Result      : constant Access_To_Subtype_For_Subscript_Expr := Bare_Subscript_Expr_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Subscript_Expr;
            return Bare_Subscript_Expr (Result);
         end Allocate_Subscript_Expr;

      package Bare_Tuple_Lit_Memos is new Langkit_Support.Packrat
        (Bare_Tuple_Lit, Token_Index);

         
         subtype Subtype_For_Tuple_Lit is
            Root_Node_Record (Turkixir_Tuple_Lit);
         type Access_To_Subtype_For_Tuple_Lit is access all Subtype_For_Tuple_Lit;
         package Bare_Tuple_Lit_Alloc is new Alloc
           (Subtype_For_Tuple_Lit, Access_To_Subtype_For_Tuple_Lit);

         function Allocate_Tuple_Lit
           (Pool : Bump_Ptr_Pool) return Bare_Tuple_Lit;

         function Allocate_Tuple_Lit
           (Pool : Bump_Ptr_Pool) return Bare_Tuple_Lit
         is
            Result      : constant Access_To_Subtype_For_Tuple_Lit := Bare_Tuple_Lit_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Tuple_Lit;
            return Bare_Tuple_Lit (Result);
         end Allocate_Tuple_Lit;

      package Bare_Xor_Expr_Memos is new Langkit_Support.Packrat
        (Bare_Xor_Expr, Token_Index);

         
         subtype Subtype_For_Xor_Expr is
            Root_Node_Record (Turkixir_Xor_Expr);
         type Access_To_Subtype_For_Xor_Expr is access all Subtype_For_Xor_Expr;
         package Bare_Xor_Expr_Alloc is new Alloc
           (Subtype_For_Xor_Expr, Access_To_Subtype_For_Xor_Expr);

         function Allocate_Xor_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Xor_Expr;

         function Allocate_Xor_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Xor_Expr
         is
            Result      : constant Access_To_Subtype_For_Xor_Expr := Bare_Xor_Expr_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Xor_Expr;
            return Bare_Xor_Expr (Result);
         end Allocate_Xor_Expr;

      package Bare_Yield_Expr_Memos is new Langkit_Support.Packrat
        (Bare_Yield_Expr, Token_Index);

         
         subtype Subtype_For_Yield_Expr is
            Root_Node_Record (Turkixir_Yield_Expr);
         type Access_To_Subtype_For_Yield_Expr is access all Subtype_For_Yield_Expr;
         package Bare_Yield_Expr_Alloc is new Alloc
           (Subtype_For_Yield_Expr, Access_To_Subtype_For_Yield_Expr);

         function Allocate_Yield_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Yield_Expr;

         function Allocate_Yield_Expr
           (Pool : Bump_Ptr_Pool) return Bare_Yield_Expr
         is
            Result      : constant Access_To_Subtype_For_Yield_Expr := Bare_Yield_Expr_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Yield_Expr;
            return Bare_Yield_Expr (Result);
         end Allocate_Yield_Expr;

      package Bare_File_Node_Memos is new Langkit_Support.Packrat
        (Bare_File_Node, Token_Index);

         
         subtype Subtype_For_File_Node is
            Root_Node_Record (Turkixir_File_Node);
         type Access_To_Subtype_For_File_Node is access all Subtype_For_File_Node;
         package Bare_File_Node_Alloc is new Alloc
           (Subtype_For_File_Node, Access_To_Subtype_For_File_Node);

         function Allocate_File_Node
           (Pool : Bump_Ptr_Pool) return Bare_File_Node;

         function Allocate_File_Node
           (Pool : Bump_Ptr_Pool) return Bare_File_Node
         is
            Result      : constant Access_To_Subtype_For_File_Node := Bare_File_Node_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_File_Node;
            return Bare_File_Node (Result);
         end Allocate_File_Node;

      package Bare_Import_Star_Memos is new Langkit_Support.Packrat
        (Bare_Import_Star, Token_Index);

         
         subtype Subtype_For_Import_Star is
            Root_Node_Record (Turkixir_Import_Star);
         type Access_To_Subtype_For_Import_Star is access all Subtype_For_Import_Star;
         package Bare_Import_Star_Alloc is new Alloc
           (Subtype_For_Import_Star, Access_To_Subtype_For_Import_Star);

         function Allocate_Import_Star
           (Pool : Bump_Ptr_Pool) return Bare_Import_Star;

         function Allocate_Import_Star
           (Pool : Bump_Ptr_Pool) return Bare_Import_Star
         is
            Result      : constant Access_To_Subtype_For_Import_Star := Bare_Import_Star_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Import_Star;
            return Bare_Import_Star (Result);
         end Allocate_Import_Star;

      package Bare_Kw_Args_Flag_Memos is new Langkit_Support.Packrat
        (Bare_Kw_Args_Flag, Token_Index);

      package Bare_Kw_Args_Flag_Absent_Memos is new Langkit_Support.Packrat
        (Bare_Kw_Args_Flag_Absent, Token_Index);

         
         subtype Subtype_For_Kw_Args_Flag_Absent is
            Root_Node_Record (Turkixir_Kw_Args_Flag_Absent);
         type Access_To_Subtype_For_Kw_Args_Flag_Absent is access all Subtype_For_Kw_Args_Flag_Absent;
         package Bare_Kw_Args_Flag_Absent_Alloc is new Alloc
           (Subtype_For_Kw_Args_Flag_Absent, Access_To_Subtype_For_Kw_Args_Flag_Absent);

         function Allocate_Kw_Args_Flag_Absent
           (Pool : Bump_Ptr_Pool) return Bare_Kw_Args_Flag_Absent;

         function Allocate_Kw_Args_Flag_Absent
           (Pool : Bump_Ptr_Pool) return Bare_Kw_Args_Flag_Absent
         is
            Result      : constant Access_To_Subtype_For_Kw_Args_Flag_Absent := Bare_Kw_Args_Flag_Absent_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Kw_Args_Flag_Absent;
            return Bare_Kw_Args_Flag_Absent (Result);
         end Allocate_Kw_Args_Flag_Absent;

      package Bare_Kw_Args_Flag_Present_Memos is new Langkit_Support.Packrat
        (Bare_Kw_Args_Flag_Present, Token_Index);

         
         subtype Subtype_For_Kw_Args_Flag_Present is
            Root_Node_Record (Turkixir_Kw_Args_Flag_Present);
         type Access_To_Subtype_For_Kw_Args_Flag_Present is access all Subtype_For_Kw_Args_Flag_Present;
         package Bare_Kw_Args_Flag_Present_Alloc is new Alloc
           (Subtype_For_Kw_Args_Flag_Present, Access_To_Subtype_For_Kw_Args_Flag_Present);

         function Allocate_Kw_Args_Flag_Present
           (Pool : Bump_Ptr_Pool) return Bare_Kw_Args_Flag_Present;

         function Allocate_Kw_Args_Flag_Present
           (Pool : Bump_Ptr_Pool) return Bare_Kw_Args_Flag_Present
         is
            Result      : constant Access_To_Subtype_For_Kw_Args_Flag_Present := Bare_Kw_Args_Flag_Present_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Kw_Args_Flag_Present;
            return Bare_Kw_Args_Flag_Present (Result);
         end Allocate_Kw_Args_Flag_Present;

      package Bare_NL_Memos is new Langkit_Support.Packrat
        (Bare_NL, Token_Index);

         
         subtype Subtype_For_NL is
            Root_Node_Record (Turkixir_NL);
         type Access_To_Subtype_For_NL is access all Subtype_For_NL;
         package Bare_NL_Alloc is new Alloc
           (Subtype_For_NL, Access_To_Subtype_For_NL);

         function Allocate_NL
           (Pool : Bump_Ptr_Pool) return Bare_NL;

         function Allocate_NL
           (Pool : Bump_Ptr_Pool) return Bare_NL
         is
            Result      : constant Access_To_Subtype_For_NL := Bare_NL_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_NL;
            return Bare_NL (Result);
         end Allocate_NL;

      package Bare_Op_Memos is new Langkit_Support.Packrat
        (Bare_Op, Token_Index);

         
         subtype Subtype_For_Op is
            Root_Node_Record (Turkixir_Op);
         type Access_To_Subtype_For_Op is access all Subtype_For_Op;
         package Bare_Op_Alloc is new Alloc
           (Subtype_For_Op, Access_To_Subtype_For_Op);

         function Allocate_Op
           (Pool : Bump_Ptr_Pool) return Bare_Op;

         function Allocate_Op
           (Pool : Bump_Ptr_Pool) return Bare_Op
         is
            Result      : constant Access_To_Subtype_For_Op := Bare_Op_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Op;
            return Bare_Op (Result);
         end Allocate_Op;

      package Bare_Params_Memos is new Langkit_Support.Packrat
        (Bare_Params, Token_Index);

         
         subtype Subtype_For_Params is
            Root_Node_Record (Turkixir_Params);
         type Access_To_Subtype_For_Params is access all Subtype_For_Params;
         package Bare_Params_Alloc is new Alloc
           (Subtype_For_Params, Access_To_Subtype_For_Params);

         function Allocate_Params
           (Pool : Bump_Ptr_Pool) return Bare_Params;

         function Allocate_Params
           (Pool : Bump_Ptr_Pool) return Bare_Params
         is
            Result      : constant Access_To_Subtype_For_Params := Bare_Params_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Params;
            return Bare_Params (Result);
         end Allocate_Params;

      package Bare_Rel_Name_Memos is new Langkit_Support.Packrat
        (Bare_Rel_Name, Token_Index);

         
         subtype Subtype_For_Rel_Name is
            Root_Node_Record (Turkixir_Rel_Name);
         type Access_To_Subtype_For_Rel_Name is access all Subtype_For_Rel_Name;
         package Bare_Rel_Name_Alloc is new Alloc
           (Subtype_For_Rel_Name, Access_To_Subtype_For_Rel_Name);

         function Allocate_Rel_Name
           (Pool : Bump_Ptr_Pool) return Bare_Rel_Name;

         function Allocate_Rel_Name
           (Pool : Bump_Ptr_Pool) return Bare_Rel_Name
         is
            Result      : constant Access_To_Subtype_For_Rel_Name := Bare_Rel_Name_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Rel_Name;
            return Bare_Rel_Name (Result);
         end Allocate_Rel_Name;

      package Bare_Single_Param_Memos is new Langkit_Support.Packrat
        (Bare_Single_Param, Token_Index);

         
         subtype Subtype_For_Single_Param is
            Root_Node_Record (Turkixir_Single_Param);
         type Access_To_Subtype_For_Single_Param is access all Subtype_For_Single_Param;
         package Bare_Single_Param_Alloc is new Alloc
           (Subtype_For_Single_Param, Access_To_Subtype_For_Single_Param);

         function Allocate_Single_Param
           (Pool : Bump_Ptr_Pool) return Bare_Single_Param;

         function Allocate_Single_Param
           (Pool : Bump_Ptr_Pool) return Bare_Single_Param
         is
            Result      : constant Access_To_Subtype_For_Single_Param := Bare_Single_Param_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Single_Param;
            return Bare_Single_Param (Result);
         end Allocate_Single_Param;

      package Bare_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Stmt, Token_Index);

      package Bare_Assert_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Assert_Stmt, Token_Index);

         
         subtype Subtype_For_Assert_Stmt is
            Root_Node_Record (Turkixir_Assert_Stmt);
         type Access_To_Subtype_For_Assert_Stmt is access all Subtype_For_Assert_Stmt;
         package Bare_Assert_Stmt_Alloc is new Alloc
           (Subtype_For_Assert_Stmt, Access_To_Subtype_For_Assert_Stmt);

         function Allocate_Assert_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Assert_Stmt;

         function Allocate_Assert_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Assert_Stmt
         is
            Result      : constant Access_To_Subtype_For_Assert_Stmt := Bare_Assert_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Assert_Stmt;
            return Bare_Assert_Stmt (Result);
         end Allocate_Assert_Stmt;

      package Bare_Assign_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Assign_Stmt, Token_Index);

         
         subtype Subtype_For_Assign_Stmt is
            Root_Node_Record (Turkixir_Assign_Stmt);
         type Access_To_Subtype_For_Assign_Stmt is access all Subtype_For_Assign_Stmt;
         package Bare_Assign_Stmt_Alloc is new Alloc
           (Subtype_For_Assign_Stmt, Access_To_Subtype_For_Assign_Stmt);

         function Allocate_Assign_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Assign_Stmt;

         function Allocate_Assign_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Assign_Stmt
         is
            Result      : constant Access_To_Subtype_For_Assign_Stmt := Bare_Assign_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Assign_Stmt;
            return Bare_Assign_Stmt (Result);
         end Allocate_Assign_Stmt;

      package Bare_Aug_Assign_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Aug_Assign_Stmt, Token_Index);

         
         subtype Subtype_For_Aug_Assign_Stmt is
            Root_Node_Record (Turkixir_Aug_Assign_Stmt);
         type Access_To_Subtype_For_Aug_Assign_Stmt is access all Subtype_For_Aug_Assign_Stmt;
         package Bare_Aug_Assign_Stmt_Alloc is new Alloc
           (Subtype_For_Aug_Assign_Stmt, Access_To_Subtype_For_Aug_Assign_Stmt);

         function Allocate_Aug_Assign_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Aug_Assign_Stmt;

         function Allocate_Aug_Assign_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Aug_Assign_Stmt
         is
            Result      : constant Access_To_Subtype_For_Aug_Assign_Stmt := Bare_Aug_Assign_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Aug_Assign_Stmt;
            return Bare_Aug_Assign_Stmt (Result);
         end Allocate_Aug_Assign_Stmt;

      package Bare_Break_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Break_Stmt, Token_Index);

         
         subtype Subtype_For_Break_Stmt is
            Root_Node_Record (Turkixir_Break_Stmt);
         type Access_To_Subtype_For_Break_Stmt is access all Subtype_For_Break_Stmt;
         package Bare_Break_Stmt_Alloc is new Alloc
           (Subtype_For_Break_Stmt, Access_To_Subtype_For_Break_Stmt);

         function Allocate_Break_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Break_Stmt;

         function Allocate_Break_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Break_Stmt
         is
            Result      : constant Access_To_Subtype_For_Break_Stmt := Bare_Break_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Break_Stmt;
            return Bare_Break_Stmt (Result);
         end Allocate_Break_Stmt;

      package Bare_Continue_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Continue_Stmt, Token_Index);

         
         subtype Subtype_For_Continue_Stmt is
            Root_Node_Record (Turkixir_Continue_Stmt);
         type Access_To_Subtype_For_Continue_Stmt is access all Subtype_For_Continue_Stmt;
         package Bare_Continue_Stmt_Alloc is new Alloc
           (Subtype_For_Continue_Stmt, Access_To_Subtype_For_Continue_Stmt);

         function Allocate_Continue_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Continue_Stmt;

         function Allocate_Continue_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Continue_Stmt
         is
            Result      : constant Access_To_Subtype_For_Continue_Stmt := Bare_Continue_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Continue_Stmt;
            return Bare_Continue_Stmt (Result);
         end Allocate_Continue_Stmt;

      package Bare_Decorated_Memos is new Langkit_Support.Packrat
        (Bare_Decorated, Token_Index);

         
         subtype Subtype_For_Decorated is
            Root_Node_Record (Turkixir_Decorated);
         type Access_To_Subtype_For_Decorated is access all Subtype_For_Decorated;
         package Bare_Decorated_Alloc is new Alloc
           (Subtype_For_Decorated, Access_To_Subtype_For_Decorated);

         function Allocate_Decorated
           (Pool : Bump_Ptr_Pool) return Bare_Decorated;

         function Allocate_Decorated
           (Pool : Bump_Ptr_Pool) return Bare_Decorated
         is
            Result      : constant Access_To_Subtype_For_Decorated := Bare_Decorated_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Decorated;
            return Bare_Decorated (Result);
         end Allocate_Decorated;

      package Bare_Def_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Def_Stmt, Token_Index);

      package Bare_Class_Def_Memos is new Langkit_Support.Packrat
        (Bare_Class_Def, Token_Index);

         
         subtype Subtype_For_Class_Def is
            Root_Node_Record (Turkixir_Class_Def);
         type Access_To_Subtype_For_Class_Def is access all Subtype_For_Class_Def;
         package Bare_Class_Def_Alloc is new Alloc
           (Subtype_For_Class_Def, Access_To_Subtype_For_Class_Def);

         function Allocate_Class_Def
           (Pool : Bump_Ptr_Pool) return Bare_Class_Def;

         function Allocate_Class_Def
           (Pool : Bump_Ptr_Pool) return Bare_Class_Def
         is
            Result      : constant Access_To_Subtype_For_Class_Def := Bare_Class_Def_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Class_Def;
            return Bare_Class_Def (Result);
         end Allocate_Class_Def;

      package Bare_Func_Def_Memos is new Langkit_Support.Packrat
        (Bare_Func_Def, Token_Index);

         
         subtype Subtype_For_Func_Def is
            Root_Node_Record (Turkixir_Func_Def);
         type Access_To_Subtype_For_Func_Def is access all Subtype_For_Func_Def;
         package Bare_Func_Def_Alloc is new Alloc
           (Subtype_For_Func_Def, Access_To_Subtype_For_Func_Def);

         function Allocate_Func_Def
           (Pool : Bump_Ptr_Pool) return Bare_Func_Def;

         function Allocate_Func_Def
           (Pool : Bump_Ptr_Pool) return Bare_Func_Def
         is
            Result      : constant Access_To_Subtype_For_Func_Def := Bare_Func_Def_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Func_Def;
            return Bare_Func_Def (Result);
         end Allocate_Func_Def;

      package Bare_Del_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Del_Stmt, Token_Index);

         
         subtype Subtype_For_Del_Stmt is
            Root_Node_Record (Turkixir_Del_Stmt);
         type Access_To_Subtype_For_Del_Stmt is access all Subtype_For_Del_Stmt;
         package Bare_Del_Stmt_Alloc is new Alloc
           (Subtype_For_Del_Stmt, Access_To_Subtype_For_Del_Stmt);

         function Allocate_Del_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Del_Stmt;

         function Allocate_Del_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Del_Stmt
         is
            Result      : constant Access_To_Subtype_For_Del_Stmt := Bare_Del_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Del_Stmt;
            return Bare_Del_Stmt (Result);
         end Allocate_Del_Stmt;

      package Bare_Elif_Branch_Memos is new Langkit_Support.Packrat
        (Bare_Elif_Branch, Token_Index);

         
         subtype Subtype_For_Elif_Branch is
            Root_Node_Record (Turkixir_Elif_Branch);
         type Access_To_Subtype_For_Elif_Branch is access all Subtype_For_Elif_Branch;
         package Bare_Elif_Branch_Alloc is new Alloc
           (Subtype_For_Elif_Branch, Access_To_Subtype_For_Elif_Branch);

         function Allocate_Elif_Branch
           (Pool : Bump_Ptr_Pool) return Bare_Elif_Branch;

         function Allocate_Elif_Branch
           (Pool : Bump_Ptr_Pool) return Bare_Elif_Branch
         is
            Result      : constant Access_To_Subtype_For_Elif_Branch := Bare_Elif_Branch_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Elif_Branch;
            return Bare_Elif_Branch (Result);
         end Allocate_Elif_Branch;

      package Bare_Exec_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Exec_Stmt, Token_Index);

         
         subtype Subtype_For_Exec_Stmt is
            Root_Node_Record (Turkixir_Exec_Stmt);
         type Access_To_Subtype_For_Exec_Stmt is access all Subtype_For_Exec_Stmt;
         package Bare_Exec_Stmt_Alloc is new Alloc
           (Subtype_For_Exec_Stmt, Access_To_Subtype_For_Exec_Stmt);

         function Allocate_Exec_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Exec_Stmt;

         function Allocate_Exec_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Exec_Stmt
         is
            Result      : constant Access_To_Subtype_For_Exec_Stmt := Bare_Exec_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Exec_Stmt;
            return Bare_Exec_Stmt (Result);
         end Allocate_Exec_Stmt;

      package Bare_For_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_For_Stmt, Token_Index);

         
         subtype Subtype_For_For_Stmt is
            Root_Node_Record (Turkixir_For_Stmt);
         type Access_To_Subtype_For_For_Stmt is access all Subtype_For_For_Stmt;
         package Bare_For_Stmt_Alloc is new Alloc
           (Subtype_For_For_Stmt, Access_To_Subtype_For_For_Stmt);

         function Allocate_For_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_For_Stmt;

         function Allocate_For_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_For_Stmt
         is
            Result      : constant Access_To_Subtype_For_For_Stmt := Bare_For_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_For_Stmt;
            return Bare_For_Stmt (Result);
         end Allocate_For_Stmt;

      package Bare_Global_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Global_Stmt, Token_Index);

         
         subtype Subtype_For_Global_Stmt is
            Root_Node_Record (Turkixir_Global_Stmt);
         type Access_To_Subtype_For_Global_Stmt is access all Subtype_For_Global_Stmt;
         package Bare_Global_Stmt_Alloc is new Alloc
           (Subtype_For_Global_Stmt, Access_To_Subtype_For_Global_Stmt);

         function Allocate_Global_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Global_Stmt;

         function Allocate_Global_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Global_Stmt
         is
            Result      : constant Access_To_Subtype_For_Global_Stmt := Bare_Global_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Global_Stmt;
            return Bare_Global_Stmt (Result);
         end Allocate_Global_Stmt;

      package Bare_If_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_If_Stmt, Token_Index);

         
         subtype Subtype_For_If_Stmt is
            Root_Node_Record (Turkixir_If_Stmt);
         type Access_To_Subtype_For_If_Stmt is access all Subtype_For_If_Stmt;
         package Bare_If_Stmt_Alloc is new Alloc
           (Subtype_For_If_Stmt, Access_To_Subtype_For_If_Stmt);

         function Allocate_If_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_If_Stmt;

         function Allocate_If_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_If_Stmt
         is
            Result      : constant Access_To_Subtype_For_If_Stmt := Bare_If_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_If_Stmt;
            return Bare_If_Stmt (Result);
         end Allocate_If_Stmt;

      package Bare_Import_From_Memos is new Langkit_Support.Packrat
        (Bare_Import_From, Token_Index);

         
         subtype Subtype_For_Import_From is
            Root_Node_Record (Turkixir_Import_From);
         type Access_To_Subtype_For_Import_From is access all Subtype_For_Import_From;
         package Bare_Import_From_Alloc is new Alloc
           (Subtype_For_Import_From, Access_To_Subtype_For_Import_From);

         function Allocate_Import_From
           (Pool : Bump_Ptr_Pool) return Bare_Import_From;

         function Allocate_Import_From
           (Pool : Bump_Ptr_Pool) return Bare_Import_From
         is
            Result      : constant Access_To_Subtype_For_Import_From := Bare_Import_From_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Import_From;
            return Bare_Import_From (Result);
         end Allocate_Import_From;

      package Bare_Import_Name_Memos is new Langkit_Support.Packrat
        (Bare_Import_Name, Token_Index);

         
         subtype Subtype_For_Import_Name is
            Root_Node_Record (Turkixir_Import_Name);
         type Access_To_Subtype_For_Import_Name is access all Subtype_For_Import_Name;
         package Bare_Import_Name_Alloc is new Alloc
           (Subtype_For_Import_Name, Access_To_Subtype_For_Import_Name);

         function Allocate_Import_Name
           (Pool : Bump_Ptr_Pool) return Bare_Import_Name;

         function Allocate_Import_Name
           (Pool : Bump_Ptr_Pool) return Bare_Import_Name
         is
            Result      : constant Access_To_Subtype_For_Import_Name := Bare_Import_Name_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Import_Name;
            return Bare_Import_Name (Result);
         end Allocate_Import_Name;

      package Bare_Pass_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Pass_Stmt, Token_Index);

         
         subtype Subtype_For_Pass_Stmt is
            Root_Node_Record (Turkixir_Pass_Stmt);
         type Access_To_Subtype_For_Pass_Stmt is access all Subtype_For_Pass_Stmt;
         package Bare_Pass_Stmt_Alloc is new Alloc
           (Subtype_For_Pass_Stmt, Access_To_Subtype_For_Pass_Stmt);

         function Allocate_Pass_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Pass_Stmt;

         function Allocate_Pass_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Pass_Stmt
         is
            Result      : constant Access_To_Subtype_For_Pass_Stmt := Bare_Pass_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Pass_Stmt;
            return Bare_Pass_Stmt (Result);
         end Allocate_Pass_Stmt;

      package Bare_Print_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Print_Stmt, Token_Index);

         
         subtype Subtype_For_Print_Stmt is
            Root_Node_Record (Turkixir_Print_Stmt);
         type Access_To_Subtype_For_Print_Stmt is access all Subtype_For_Print_Stmt;
         package Bare_Print_Stmt_Alloc is new Alloc
           (Subtype_For_Print_Stmt, Access_To_Subtype_For_Print_Stmt);

         function Allocate_Print_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Print_Stmt;

         function Allocate_Print_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Print_Stmt
         is
            Result      : constant Access_To_Subtype_For_Print_Stmt := Bare_Print_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Print_Stmt;
            return Bare_Print_Stmt (Result);
         end Allocate_Print_Stmt;

      package Bare_Raise_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Raise_Stmt, Token_Index);

         
         subtype Subtype_For_Raise_Stmt is
            Root_Node_Record (Turkixir_Raise_Stmt);
         type Access_To_Subtype_For_Raise_Stmt is access all Subtype_For_Raise_Stmt;
         package Bare_Raise_Stmt_Alloc is new Alloc
           (Subtype_For_Raise_Stmt, Access_To_Subtype_For_Raise_Stmt);

         function Allocate_Raise_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Raise_Stmt;

         function Allocate_Raise_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Raise_Stmt
         is
            Result      : constant Access_To_Subtype_For_Raise_Stmt := Bare_Raise_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Raise_Stmt;
            return Bare_Raise_Stmt (Result);
         end Allocate_Raise_Stmt;

      package Bare_Return_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Return_Stmt, Token_Index);

         
         subtype Subtype_For_Return_Stmt is
            Root_Node_Record (Turkixir_Return_Stmt);
         type Access_To_Subtype_For_Return_Stmt is access all Subtype_For_Return_Stmt;
         package Bare_Return_Stmt_Alloc is new Alloc
           (Subtype_For_Return_Stmt, Access_To_Subtype_For_Return_Stmt);

         function Allocate_Return_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Return_Stmt;

         function Allocate_Return_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Return_Stmt
         is
            Result      : constant Access_To_Subtype_For_Return_Stmt := Bare_Return_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Return_Stmt;
            return Bare_Return_Stmt (Result);
         end Allocate_Return_Stmt;

      package Bare_Stream_Print_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Stream_Print_Stmt, Token_Index);

         
         subtype Subtype_For_Stream_Print_Stmt is
            Root_Node_Record (Turkixir_Stream_Print_Stmt);
         type Access_To_Subtype_For_Stream_Print_Stmt is access all Subtype_For_Stream_Print_Stmt;
         package Bare_Stream_Print_Stmt_Alloc is new Alloc
           (Subtype_For_Stream_Print_Stmt, Access_To_Subtype_For_Stream_Print_Stmt);

         function Allocate_Stream_Print_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Stream_Print_Stmt;

         function Allocate_Stream_Print_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Stream_Print_Stmt
         is
            Result      : constant Access_To_Subtype_For_Stream_Print_Stmt := Bare_Stream_Print_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Stream_Print_Stmt;
            return Bare_Stream_Print_Stmt (Result);
         end Allocate_Stream_Print_Stmt;

      package Bare_Try_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_Try_Stmt, Token_Index);

         
         subtype Subtype_For_Try_Stmt is
            Root_Node_Record (Turkixir_Try_Stmt);
         type Access_To_Subtype_For_Try_Stmt is access all Subtype_For_Try_Stmt;
         package Bare_Try_Stmt_Alloc is new Alloc
           (Subtype_For_Try_Stmt, Access_To_Subtype_For_Try_Stmt);

         function Allocate_Try_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Try_Stmt;

         function Allocate_Try_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_Try_Stmt
         is
            Result      : constant Access_To_Subtype_For_Try_Stmt := Bare_Try_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Try_Stmt;
            return Bare_Try_Stmt (Result);
         end Allocate_Try_Stmt;

      package Bare_While_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_While_Stmt, Token_Index);

         
         subtype Subtype_For_While_Stmt is
            Root_Node_Record (Turkixir_While_Stmt);
         type Access_To_Subtype_For_While_Stmt is access all Subtype_For_While_Stmt;
         package Bare_While_Stmt_Alloc is new Alloc
           (Subtype_For_While_Stmt, Access_To_Subtype_For_While_Stmt);

         function Allocate_While_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_While_Stmt;

         function Allocate_While_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_While_Stmt
         is
            Result      : constant Access_To_Subtype_For_While_Stmt := Bare_While_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_While_Stmt;
            return Bare_While_Stmt (Result);
         end Allocate_While_Stmt;

      package Bare_With_Stmt_Memos is new Langkit_Support.Packrat
        (Bare_With_Stmt, Token_Index);

         
         subtype Subtype_For_With_Stmt is
            Root_Node_Record (Turkixir_With_Stmt);
         type Access_To_Subtype_For_With_Stmt is access all Subtype_For_With_Stmt;
         package Bare_With_Stmt_Alloc is new Alloc
           (Subtype_For_With_Stmt, Access_To_Subtype_For_With_Stmt);

         function Allocate_With_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_With_Stmt;

         function Allocate_With_Stmt
           (Pool : Bump_Ptr_Pool) return Bare_With_Stmt
         is
            Result      : constant Access_To_Subtype_For_With_Stmt := Bare_With_Stmt_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_With_Stmt;
            return Bare_With_Stmt (Result);
         end Allocate_With_Stmt;

      package Bare_Turkixir_Node_Base_List_Memos is new Langkit_Support.Packrat
        (Bare_Turkixir_Node_Base_List, Token_Index);

      package Bare_Arg_List_Memos is new Langkit_Support.Packrat
        (Bare_Arg_List, Token_Index);

         
         subtype Subtype_For_Arg_List is
            Root_Node_Record (Turkixir_Arg_List);
         type Access_To_Subtype_For_Arg_List is access all Subtype_For_Arg_List;
         package Bare_Arg_List_Alloc is new Alloc
           (Subtype_For_Arg_List, Access_To_Subtype_For_Arg_List);

         function Allocate_Arg_List
           (Pool : Bump_Ptr_Pool) return Bare_Arg_List;

         function Allocate_Arg_List
           (Pool : Bump_Ptr_Pool) return Bare_Arg_List
         is
            Result      : constant Access_To_Subtype_For_Arg_List := Bare_Arg_List_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Arg_List;
            return Bare_Arg_List (Result);
         end Allocate_Arg_List;

      package Bare_As_Name_Node_List_Memos is new Langkit_Support.Packrat
        (Bare_As_Name_Node_List, Token_Index);

         
         subtype Subtype_For_As_Name_Node_List is
            Root_Node_Record (Turkixir_As_Name_Node_List);
         type Access_To_Subtype_For_As_Name_Node_List is access all Subtype_For_As_Name_Node_List;
         package Bare_As_Name_Node_List_Alloc is new Alloc
           (Subtype_For_As_Name_Node_List, Access_To_Subtype_For_As_Name_Node_List);

         function Allocate_As_Name_Node_List
           (Pool : Bump_Ptr_Pool) return Bare_As_Name_Node_List;

         function Allocate_As_Name_Node_List
           (Pool : Bump_Ptr_Pool) return Bare_As_Name_Node_List
         is
            Result      : constant Access_To_Subtype_For_As_Name_Node_List := Bare_As_Name_Node_List_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_As_Name_Node_List;
            return Bare_As_Name_Node_List (Result);
         end Allocate_As_Name_Node_List;

      package Bare_Decorator_List_Memos is new Langkit_Support.Packrat
        (Bare_Decorator_List, Token_Index);

         
         subtype Subtype_For_Decorator_List is
            Root_Node_Record (Turkixir_Decorator_List);
         type Access_To_Subtype_For_Decorator_List is access all Subtype_For_Decorator_List;
         package Bare_Decorator_List_Alloc is new Alloc
           (Subtype_For_Decorator_List, Access_To_Subtype_For_Decorator_List);

         function Allocate_Decorator_List
           (Pool : Bump_Ptr_Pool) return Bare_Decorator_List;

         function Allocate_Decorator_List
           (Pool : Bump_Ptr_Pool) return Bare_Decorator_List
         is
            Result      : constant Access_To_Subtype_For_Decorator_List := Bare_Decorator_List_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Decorator_List;
            return Bare_Decorator_List (Result);
         end Allocate_Decorator_List;

      package Bare_Dict_Assoc_List_Memos is new Langkit_Support.Packrat
        (Bare_Dict_Assoc_List, Token_Index);

         
         subtype Subtype_For_Dict_Assoc_List is
            Root_Node_Record (Turkixir_Dict_Assoc_List);
         type Access_To_Subtype_For_Dict_Assoc_List is access all Subtype_For_Dict_Assoc_List;
         package Bare_Dict_Assoc_List_Alloc is new Alloc
           (Subtype_For_Dict_Assoc_List, Access_To_Subtype_For_Dict_Assoc_List);

         function Allocate_Dict_Assoc_List
           (Pool : Bump_Ptr_Pool) return Bare_Dict_Assoc_List;

         function Allocate_Dict_Assoc_List
           (Pool : Bump_Ptr_Pool) return Bare_Dict_Assoc_List
         is
            Result      : constant Access_To_Subtype_For_Dict_Assoc_List := Bare_Dict_Assoc_List_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Dict_Assoc_List;
            return Bare_Dict_Assoc_List (Result);
         end Allocate_Dict_Assoc_List;

      package Bare_Dot_List_Memos is new Langkit_Support.Packrat
        (Bare_Dot_List, Token_Index);

         
         subtype Subtype_For_Dot_List is
            Root_Node_Record (Turkixir_Dot_List);
         type Access_To_Subtype_For_Dot_List is access all Subtype_For_Dot_List;
         package Bare_Dot_List_Alloc is new Alloc
           (Subtype_For_Dot_List, Access_To_Subtype_For_Dot_List);

         function Allocate_Dot_List
           (Pool : Bump_Ptr_Pool) return Bare_Dot_List;

         function Allocate_Dot_List
           (Pool : Bump_Ptr_Pool) return Bare_Dot_List
         is
            Result      : constant Access_To_Subtype_For_Dot_List := Bare_Dot_List_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Dot_List;
            return Bare_Dot_List (Result);
         end Allocate_Dot_List;

      package Bare_Elif_Branch_List_Memos is new Langkit_Support.Packrat
        (Bare_Elif_Branch_List, Token_Index);

         
         subtype Subtype_For_Elif_Branch_List is
            Root_Node_Record (Turkixir_Elif_Branch_List);
         type Access_To_Subtype_For_Elif_Branch_List is access all Subtype_For_Elif_Branch_List;
         package Bare_Elif_Branch_List_Alloc is new Alloc
           (Subtype_For_Elif_Branch_List, Access_To_Subtype_For_Elif_Branch_List);

         function Allocate_Elif_Branch_List
           (Pool : Bump_Ptr_Pool) return Bare_Elif_Branch_List;

         function Allocate_Elif_Branch_List
           (Pool : Bump_Ptr_Pool) return Bare_Elif_Branch_List
         is
            Result      : constant Access_To_Subtype_For_Elif_Branch_List := Bare_Elif_Branch_List_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Elif_Branch_List;
            return Bare_Elif_Branch_List (Result);
         end Allocate_Elif_Branch_List;

      package Bare_Except_Part_List_Memos is new Langkit_Support.Packrat
        (Bare_Except_Part_List, Token_Index);

         
         subtype Subtype_For_Except_Part_List is
            Root_Node_Record (Turkixir_Except_Part_List);
         type Access_To_Subtype_For_Except_Part_List is access all Subtype_For_Except_Part_List;
         package Bare_Except_Part_List_Alloc is new Alloc
           (Subtype_For_Except_Part_List, Access_To_Subtype_For_Except_Part_List);

         function Allocate_Except_Part_List
           (Pool : Bump_Ptr_Pool) return Bare_Except_Part_List;

         function Allocate_Except_Part_List
           (Pool : Bump_Ptr_Pool) return Bare_Except_Part_List
         is
            Result      : constant Access_To_Subtype_For_Except_Part_List := Bare_Except_Part_List_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Except_Part_List;
            return Bare_Except_Part_List (Result);
         end Allocate_Except_Part_List;

      package Bare_Expr_List_Memos is new Langkit_Support.Packrat
        (Bare_Expr_List, Token_Index);

         
         subtype Subtype_For_Expr_List is
            Root_Node_Record (Turkixir_Expr_List);
         type Access_To_Subtype_For_Expr_List is access all Subtype_For_Expr_List;
         package Bare_Expr_List_Alloc is new Alloc
           (Subtype_For_Expr_List, Access_To_Subtype_For_Expr_List);

         function Allocate_Expr_List
           (Pool : Bump_Ptr_Pool) return Bare_Expr_List;

         function Allocate_Expr_List
           (Pool : Bump_Ptr_Pool) return Bare_Expr_List
         is
            Result      : constant Access_To_Subtype_For_Expr_List := Bare_Expr_List_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Expr_List;
            return Bare_Expr_List (Result);
         end Allocate_Expr_List;

      package Bare_Id_List_Memos is new Langkit_Support.Packrat
        (Bare_Id_List, Token_Index);

         
         subtype Subtype_For_Id_List is
            Root_Node_Record (Turkixir_Id_List);
         type Access_To_Subtype_For_Id_List is access all Subtype_For_Id_List;
         package Bare_Id_List_Alloc is new Alloc
           (Subtype_For_Id_List, Access_To_Subtype_For_Id_List);

         function Allocate_Id_List
           (Pool : Bump_Ptr_Pool) return Bare_Id_List;

         function Allocate_Id_List
           (Pool : Bump_Ptr_Pool) return Bare_Id_List
         is
            Result      : constant Access_To_Subtype_For_Id_List := Bare_Id_List_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Id_List;
            return Bare_Id_List (Result);
         end Allocate_Id_List;

      package Bare_NL_List_Memos is new Langkit_Support.Packrat
        (Bare_NL_List, Token_Index);

         
         subtype Subtype_For_NL_List is
            Root_Node_Record (Turkixir_NL_List);
         type Access_To_Subtype_For_NL_List is access all Subtype_For_NL_List;
         package Bare_NL_List_Alloc is new Alloc
           (Subtype_For_NL_List, Access_To_Subtype_For_NL_List);

         function Allocate_NL_List
           (Pool : Bump_Ptr_Pool) return Bare_NL_List;

         function Allocate_NL_List
           (Pool : Bump_Ptr_Pool) return Bare_NL_List
         is
            Result      : constant Access_To_Subtype_For_NL_List := Bare_NL_List_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_NL_List;
            return Bare_NL_List (Result);
         end Allocate_NL_List;

      package Bare_Single_Param_List_Memos is new Langkit_Support.Packrat
        (Bare_Single_Param_List, Token_Index);

         
         subtype Subtype_For_Single_Param_List is
            Root_Node_Record (Turkixir_Single_Param_List);
         type Access_To_Subtype_For_Single_Param_List is access all Subtype_For_Single_Param_List;
         package Bare_Single_Param_List_Alloc is new Alloc
           (Subtype_For_Single_Param_List, Access_To_Subtype_For_Single_Param_List);

         function Allocate_Single_Param_List
           (Pool : Bump_Ptr_Pool) return Bare_Single_Param_List;

         function Allocate_Single_Param_List
           (Pool : Bump_Ptr_Pool) return Bare_Single_Param_List
         is
            Result      : constant Access_To_Subtype_For_Single_Param_List := Bare_Single_Param_List_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Single_Param_List;
            return Bare_Single_Param_List (Result);
         end Allocate_Single_Param_List;

      package Bare_String_Lit_List_Memos is new Langkit_Support.Packrat
        (Bare_String_Lit_List, Token_Index);

         
         subtype Subtype_For_String_Lit_List is
            Root_Node_Record (Turkixir_String_Lit_List);
         type Access_To_Subtype_For_String_Lit_List is access all Subtype_For_String_Lit_List;
         package Bare_String_Lit_List_Alloc is new Alloc
           (Subtype_For_String_Lit_List, Access_To_Subtype_For_String_Lit_List);

         function Allocate_String_Lit_List
           (Pool : Bump_Ptr_Pool) return Bare_String_Lit_List;

         function Allocate_String_Lit_List
           (Pool : Bump_Ptr_Pool) return Bare_String_Lit_List
         is
            Result      : constant Access_To_Subtype_For_String_Lit_List := Bare_String_Lit_List_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_String_Lit_List;
            return Bare_String_Lit_List (Result);
         end Allocate_String_Lit_List;

      package Bare_Turkixir_Node_List_Memos is new Langkit_Support.Packrat
        (Bare_Turkixir_Node_List, Token_Index);

         
         subtype Subtype_For_Turkixir_Node_List is
            Root_Node_Record (Turkixir_Turkixir_Node_List);
         type Access_To_Subtype_For_Turkixir_Node_List is access all Subtype_For_Turkixir_Node_List;
         package Bare_Turkixir_Node_List_Alloc is new Alloc
           (Subtype_For_Turkixir_Node_List, Access_To_Subtype_For_Turkixir_Node_List);

         function Allocate_Turkixir_Node_List
           (Pool : Bump_Ptr_Pool) return Bare_Turkixir_Node_List;

         function Allocate_Turkixir_Node_List
           (Pool : Bump_Ptr_Pool) return Bare_Turkixir_Node_List
         is
            Result      : constant Access_To_Subtype_For_Turkixir_Node_List := Bare_Turkixir_Node_List_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Turkixir_Node_List;
            return Bare_Turkixir_Node_List (Result);
         end Allocate_Turkixir_Node_List;

      package Bare_Var_Args_Flag_Memos is new Langkit_Support.Packrat
        (Bare_Var_Args_Flag, Token_Index);

      package Bare_Var_Args_Flag_Absent_Memos is new Langkit_Support.Packrat
        (Bare_Var_Args_Flag_Absent, Token_Index);

         
         subtype Subtype_For_Var_Args_Flag_Absent is
            Root_Node_Record (Turkixir_Var_Args_Flag_Absent);
         type Access_To_Subtype_For_Var_Args_Flag_Absent is access all Subtype_For_Var_Args_Flag_Absent;
         package Bare_Var_Args_Flag_Absent_Alloc is new Alloc
           (Subtype_For_Var_Args_Flag_Absent, Access_To_Subtype_For_Var_Args_Flag_Absent);

         function Allocate_Var_Args_Flag_Absent
           (Pool : Bump_Ptr_Pool) return Bare_Var_Args_Flag_Absent;

         function Allocate_Var_Args_Flag_Absent
           (Pool : Bump_Ptr_Pool) return Bare_Var_Args_Flag_Absent
         is
            Result      : constant Access_To_Subtype_For_Var_Args_Flag_Absent := Bare_Var_Args_Flag_Absent_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Var_Args_Flag_Absent;
            return Bare_Var_Args_Flag_Absent (Result);
         end Allocate_Var_Args_Flag_Absent;

      package Bare_Var_Args_Flag_Present_Memos is new Langkit_Support.Packrat
        (Bare_Var_Args_Flag_Present, Token_Index);

         
         subtype Subtype_For_Var_Args_Flag_Present is
            Root_Node_Record (Turkixir_Var_Args_Flag_Present);
         type Access_To_Subtype_For_Var_Args_Flag_Present is access all Subtype_For_Var_Args_Flag_Present;
         package Bare_Var_Args_Flag_Present_Alloc is new Alloc
           (Subtype_For_Var_Args_Flag_Present, Access_To_Subtype_For_Var_Args_Flag_Present);

         function Allocate_Var_Args_Flag_Present
           (Pool : Bump_Ptr_Pool) return Bare_Var_Args_Flag_Present;

         function Allocate_Var_Args_Flag_Present
           (Pool : Bump_Ptr_Pool) return Bare_Var_Args_Flag_Present
         is
            Result      : constant Access_To_Subtype_For_Var_Args_Flag_Present := Bare_Var_Args_Flag_Present_Alloc.Alloc (Pool);
            Result_Kind : Turkixir_Node_Kind_Type
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := Turkixir_Var_Args_Flag_Present;
            return Bare_Var_Args_Flag_Present (Result);
         end Allocate_Var_Args_Flag_Present;

   pragma Warnings (On, "is not referenced");
   pragma Warnings (On, "possible aliasing problem for type");

   procedure Initialize_List
     (Self   : Bare_Turkixir_Node_Base_List;
      Parser : Parser_Type;
      Count  : Natural);
   --  Helper for parsers, to initialize the list of children in a freshly
   --  allocated list node.

   type Dontskip_Parser_Function is access function
     (Parser : in out Parser_Type;
      Pos    : Token_Index)
      return Bare_Turkixir_Node;

   package Dont_Skip_Fn_Vectors
   is new Ada.Containers.Vectors (Natural, Dontskip_Parser_Function);

   type Free_Parse_List_Record;
   type Free_Parse_List is access all Free_Parse_List_Record;
   --  Cache of temporary lists of AST nodes used in List parsers

   type Free_Parse_List_Record is record
      Nodes : Bare_Turkixir_Node_Vectors.Vector;
      Next  : Free_Parse_List;
   end record;

   type Parser_Private_Part_Type is record
      Parse_Lists : Free_Parse_List;

      
      And_Expr_Or_Parse_0_Memo : Bare_Expr_Memos.Memo_Type;
      
      And_Test_Or_Parse_0_Memo : Bare_Expr_Memos.Memo_Type;
      
      Arg_List_Extract_Parse_1_Memo : Bare_Arg_List_Memos.Memo_Type;
      
      Arith_Expr_Or_Parse_1_Memo : Bare_Expr_Memos.Memo_Type;
      
      As_Name_Transform_Parse_0_Memo : Bare_As_Name_Node_Memos.Memo_Type;
      
      Assert_Stmt_Transform_Parse_0_Memo : Bare_Assert_Stmt_Memos.Memo_Type;
      
      Atom_Expr_Or_Parse_0_Memo : Bare_Expr_Memos.Memo_Type;
      
      Atom_Or_Parse_0_Memo : Bare_Expr_Memos.Memo_Type;
      
      Break_Stmt_Transform_Parse_0_Memo : Bare_Break_Stmt_Memos.Memo_Type;
      
      Cat_String_Transform_Parse_0_Memo : Bare_Concat_String_Lit_Memos.Memo_Type;
      
      Class_Def_Transform_Parse_0_Memo : Bare_Class_Def_Memos.Memo_Type;
      
      Comp_For_Transform_Parse_0_Memo : Bare_Comp_For_Memos.Memo_Type;
      
      Comp_If_Transform_Parse_0_Memo : Bare_Comp_If_Memos.Memo_Type;
      
      Comp_Iter_Or_Parse_0_Memo : Bare_Turkixir_Node_Memos.Memo_Type;
      
      Comparison_Or_Parse_1_Memo : Bare_Expr_Memos.Memo_Type;
      
      Compound_Stmt_Or_Parse_0_Memo : Bare_Stmt_Memos.Memo_Type;
      
      Continue_Stmt_Transform_Parse_0_Memo : Bare_Continue_Stmt_Memos.Memo_Type;
      
      Decorated_Transform_Parse_0_Memo : Bare_Decorated_Memos.Memo_Type;
      
      Decorator_Transform_Parse_0_Memo : Bare_Decorator_Memos.Memo_Type;
      
      Decorators_List_Parse_0_Memo : Bare_Decorator_List_Memos.Memo_Type;
      
      Del_Stmt_Transform_Parse_0_Memo : Bare_Del_Stmt_Memos.Memo_Type;
      
      Dict_Assoc_Transform_Parse_0_Memo : Bare_Dict_Assoc_Memos.Memo_Type;
      
      Dot_Transform_Parse_0_Memo : Bare_Dot_Memos.Memo_Type;
      
      Dotted_As_Name_Transform_Parse_0_Memo : Bare_As_Name_Node_Memos.Memo_Type;
      
      Dotted_As_Names_Extract_Parse_0_Memo : Bare_Turkixir_Node_List_Memos.Memo_Type;
      
      Dotted_Name_Or_Parse_0_Memo : Bare_Name_Memos.Memo_Type;
      
      Else_Part_Transform_Parse_0_Memo : Bare_Else_Part_Memos.Memo_Type;
      
      Empty_Test_List_Extract_Parse_0_Memo : Bare_Expr_List_Memos.Memo_Type;
      
      Exec_Stmt_Transform_Parse_0_Memo : Bare_Exec_Stmt_Memos.Memo_Type;
      
      Expr_List_Extract_Parse_0_Memo : Bare_Expr_List_Memos.Memo_Type;
      
      Expr_Or_Parse_0_Memo : Bare_Expr_Memos.Memo_Type;
      
      Expr_Stmt_Or_Parse_3_Memo : Bare_Turkixir_Node_Memos.Memo_Type;
      
      Factor_Or_Parse_1_Memo : Bare_Expr_Memos.Memo_Type;
      
      Flow_Stmt_Or_Parse_0_Memo : Bare_Turkixir_Node_Memos.Memo_Type;
      
      For_Stmt_Transform_Parse_0_Memo : Bare_For_Stmt_Memos.Memo_Type;
      
      Fpdef_Or_Parse_0_Memo : Bare_Turkixir_Node_Memos.Memo_Type;
      
      Func_Def_Transform_Parse_0_Memo : Bare_Func_Def_Memos.Memo_Type;
      
      Global_Stmt_Transform_Parse_0_Memo : Bare_Global_Stmt_Memos.Memo_Type;
      
      If_Stmt_Transform_Parse_1_Memo : Bare_If_Stmt_Memos.Memo_Type;
      
      Import_As_Names_Extract_Parse_0_Memo : Bare_Turkixir_Node_List_Memos.Memo_Type;
      
      Import_From_Transform_Parse_2_Memo : Bare_Import_From_Memos.Memo_Type;
      
      Import_Name_Transform_Parse_0_Memo : Bare_Import_Name_Memos.Memo_Type;
      
      Import_Stmt_Or_Parse_0_Memo : Bare_Stmt_Memos.Memo_Type;
      
      Lambdef_Transform_Parse_0_Memo : Bare_Lambda_Def_Memos.Memo_Type;
      
      List_For_Transform_Parse_0_Memo : Bare_Comp_ForL_Memos.Memo_Type;
      
      List_If_Transform_Parse_0_Memo : Bare_Comp_If_Memos.Memo_Type;
      
      List_Iter_Or_Parse_0_Memo : Bare_Turkixir_Node_Memos.Memo_Type;
      
      Main_Rule_Transform_Parse_0_Memo : Bare_File_Node_Memos.Memo_Type;
      
      Name_List_Extract_Parse_0_Memo : Bare_Id_List_Memos.Memo_Type;
      
      Name_Transform_Parse_0_Memo : Bare_Id_Memos.Memo_Type;
      
      Nl_Transform_Parse_0_Memo : Bare_NL_Memos.Memo_Type;
      
      Not_Test_Or_Parse_0_Memo : Bare_Expr_Memos.Memo_Type;
      
      Number_Transform_Parse_0_Memo : Bare_Number_Lit_Memos.Memo_Type;
      
      Or_Test_Or_Parse_0_Memo : Bare_Expr_Memos.Memo_Type;
      
      Parameters_Extract_Parse_0_Memo : Bare_Params_Memos.Memo_Type;
      
      Pass_Stmt_Transform_Parse_0_Memo : Bare_Pass_Stmt_Memos.Memo_Type;
      
      Power_Or_Parse_0_Memo : Bare_Expr_Memos.Memo_Type;
      
      Print_Stmt_Or_Parse_0_Memo : Bare_Stmt_Memos.Memo_Type;
      
      Raise_Stmt_Transform_Parse_0_Memo : Bare_Raise_Stmt_Memos.Memo_Type;
      
      Return_Stmt_Transform_Parse_0_Memo : Bare_Return_Stmt_Memos.Memo_Type;
      
      Set_Lit_Transform_Parse_0_Memo : Bare_Set_Lit_Memos.Memo_Type;
      
      Shift_Expr_Or_Parse_1_Memo : Bare_Expr_Memos.Memo_Type;
      
      Simple_Stmt_Extract_Parse_1_Memo : Bare_Turkixir_Node_Memos.Memo_Type;
      
      Small_Stmt_Or_Parse_0_Memo : Bare_Turkixir_Node_Memos.Memo_Type;
      
      Stmt_Or_Parse_0_Memo : Bare_Turkixir_Node_Memos.Memo_Type;
      
      String_Transform_Parse_0_Memo : Bare_String_Lit_Memos.Memo_Type;
      
      Subscript_List_Extract_Parse_0_Memo : Bare_Expr_List_Memos.Memo_Type;
      
      Subscript_Or_Parse_0_Memo : Bare_Expr_Memos.Memo_Type;
      
      Suite_Or_Parse_0_Memo : Bare_Turkixir_Node_Memos.Memo_Type;
      
      Term_Or_Parse_1_Memo : Bare_Expr_Memos.Memo_Type;
      
      Test_List_Extract_Parse_0_Memo : Bare_Expr_List_Memos.Memo_Type;
      
      Test_Or_Parse_0_Memo : Bare_Expr_Memos.Memo_Type;
      
      Try_Stmt_Transform_Parse_2_Memo : Bare_Try_Stmt_Memos.Memo_Type;
      
      Varargslist_Transform_Parse_1_Memo : Bare_Params_Memos.Memo_Type;
      
      While_Stmt_Transform_Parse_0_Memo : Bare_While_Stmt_Memos.Memo_Type;
      
      With_Item_Transform_Parse_0_Memo : Bare_As_Name_Node_Memos.Memo_Type;
      
      With_Stmt_Transform_Parse_0_Memo : Bare_With_Stmt_Memos.Memo_Type;
      
      Xor_Expr_Or_Parse_0_Memo : Bare_Expr_Memos.Memo_Type;
      
      Yield_Expr_Transform_Parse_0_Memo : Bare_Yield_Expr_Memos.Memo_Type;
      
      Yield_Stmt_Defer_Parse_0_Memo : Bare_Yield_Expr_Memos.Memo_Type;

      Dont_Skip : Dont_Skip_Fn_Vectors.Vector;
   end record;

   
function Name_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Id;

   
function Name_List_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Id_List;

   
function Fpdef_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node;

   
function Varargslist_Transform_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Params;

   
function Lambdef_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Lambda_Def;

   
function Xor_Expr_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function Expr_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function Comparison_Or_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function Not_Test_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function And_Test_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function Or_Test_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function Test_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function Expr_List_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr_List;

   
function Comp_If_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Comp_If;

   
function Comp_Iter_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node;

   
function Comp_For_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Comp_For;

   
function Arg_List_Extract_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Arg_List;

   
function Subscript_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function Subscript_List_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr_List;

   
function Test_List_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr_List;

   
function Yield_Expr_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Yield_Expr;

   
function List_If_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Comp_If;

   
function List_Iter_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node;

   
function List_For_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Comp_ForL;

   
function Empty_Test_List_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr_List;

   
function Set_Lit_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Set_Lit;

   
function Dict_Assoc_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Dict_Assoc;

   
function Number_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Number_Lit;

   
function String_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_String_Lit;

   
function Cat_String_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Concat_String_Lit;

   
function Atom_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function Atom_Expr_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function Power_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function Factor_Or_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function Term_Or_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function Arith_Expr_Or_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function Shift_Expr_Or_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function And_Expr_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr;

   
function As_Name_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_As_Name_Node;

   
function Assert_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Assert_Stmt;

   
function Break_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Break_Stmt;

   
function Nl_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_NL;

   
function Expr_Stmt_Or_Parse_3
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node;

   
function Print_Stmt_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Stmt;

   
function Del_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Del_Stmt;

   
function Pass_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Pass_Stmt;

   
function Continue_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Continue_Stmt;

   
function Return_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Return_Stmt;

   
function Raise_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Raise_Stmt;

   
function Yield_Stmt_Defer_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Yield_Expr;

   
function Flow_Stmt_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node;

   
function Dotted_Name_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Name;

   
function Dotted_As_Name_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_As_Name_Node;

   
function Dotted_As_Names_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node_List;

   
function Import_Name_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Import_Name;

   
function Dot_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Dot;

   
function Import_As_Names_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node_List;

   
function Import_From_Transform_Parse_2
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Import_From;

   
function Import_Stmt_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Stmt;

   
function Global_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Global_Stmt;

   
function Exec_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Exec_Stmt;

   
function Small_Stmt_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node;

   
function Simple_Stmt_Extract_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node;

   
function Else_Part_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Else_Part;

   
function If_Stmt_Transform_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_If_Stmt;

   
function While_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_While_Stmt;

   
function For_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_For_Stmt;

   
function Try_Stmt_Transform_Parse_2
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Try_Stmt;

   
function With_Item_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_As_Name_Node;

   
function With_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_With_Stmt;

   
function Parameters_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Params;

   
function Func_Def_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Func_Def;

   
function Decorator_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Decorator;

   
function Decorators_List_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Decorator_List;

   
function Decorated_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Decorated;

   
function Compound_Stmt_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Stmt;

   
function Stmt_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node;

   
function Suite_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node;

   
function Class_Def_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Class_Def;

   
function Main_Rule_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_File_Node;


   procedure Process_Parsing_Error
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True);
   --  Helper for the user parsing function, to be called after a low-level
   --  parsing function. Check_Complete has the same semantics as in Parse. If
   --  the parsing failed (Parser.Current_Pos = No_Token_Index), append
   --  corresponding diagnostics to Parser.Diagnostics, do nothing instead.

   procedure Add_Last_Fail_Diagnostic (Parser : in out Parser_Type);
   --  Add a diagnostic for the last fail position of the parser

   function Get_Parse_List (Parser : Parser_Type) return Free_Parse_List;
   --  Get a free parse list, or allocate one if there is no free parse list in
   --  Parser. When done with the result, the caller must invoke
   --  Release_Parse_List.

   procedure Release_Parse_List
     (Parser : Parser_Type; List : in out Free_Parse_List);
   --  Release a parse list, putting it in Parsers' free list. Set List to
   --  null.

   procedure Enter_Call (Parser : Parser_Type; Call_Depth : access Natural);
   procedure Exit_Call (Parser : Parser_Type; Call_Depth : Natural);
   --  Shortcuts to forward Parser's context to the eponym procedures in
   --  the Implementation package.

   ---------------------
   -- Initialize_List --
   ---------------------

   procedure Initialize_List
     (Self   : Bare_Turkixir_Node_Base_List;
      Parser : Parser_Type;
      Count  : Natural) is
   begin
      Self.Count := Count;
      Self.Nodes := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, 0);
   end Initialize_List;

   -----------------
   -- Init_Parser --
   -----------------

   procedure Init_Parser
     (Input       : Internal_Lexer_Input;
      With_Trivia : Boolean;
      Unit        : access Implementation.Analysis_Unit_Type;
      TDH         : Token_Data_Handler_Access;
      Parser      : in out Parser_Type)
   is
      --  Never try to use file readers for internal units: these are generally
      --  not actual source files, and file readers, which are external users
      --  of the generated library, have no reason to be aware of them.

      FR : constant Internal_File_Reader_Access :=
        (if Unit.Is_Internal
         then null
         else Unit.Context.File_Reader);
   begin
      Reset (Parser);
      Extract_Tokens (Input, With_Trivia, FR, TDH.all, Parser.Diagnostics);
      Parser.Unit := Unit;
      Parser.TDH := TDH;
   end Init_Parser;

   ------------------------------
   -- Add_Last_Fail_Diagnostic --
   ------------------------------

   procedure Add_Last_Fail_Diagnostic (Parser : in out Parser_Type)
   is
      Last_Token : Stored_Token_Data renames
         Get_Token (Parser.TDH.all, Parser.Last_Fail.Pos);
      D : constant Diagnostic :=
        (if Parser.Last_Fail.Kind = Token_Fail then
          Create (Sloc_Range (Parser.TDH.all, Last_Token), To_Text
            ("Expected "
             & Token_Error_Image (Parser.Last_Fail.Expected_Token_Id)
             & ", got "
             & Token_Error_Image (Parser.Last_Fail.Found_Token_Id)))
         else
           Create (Sloc_Range (Parser.TDH.all, Last_Token),
                   To_Text (Parser.Last_Fail.Custom_Message.all)));
   begin
      Parser.Diagnostics.Append (D);
   end Add_Last_Fail_Diagnostic;

   ---------------------------
   -- Process_Parsing_Error --
   ---------------------------

   procedure Process_Parsing_Error
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True) is
   begin

      if Parser.Current_Pos = No_Token_Index then
         Add_Last_Fail_Diagnostic (Parser);
      elsif Check_Complete
        and then Parser.Current_Pos /= Last_Token (Parser.TDH.all)
      then
         --  If the fail pos is the current position of the parser or after,
         --  it means that the longest parse is the correct result, and that we
         --  have some garbage afterwards.
         if Parser.Current_Pos >= Parser.Last_Fail.Pos then
            declare
               First_Garbage_Token : Stored_Token_Data renames
                  Get_Token (Parser.TDH.all, Parser.Current_Pos);
            begin
               Append
                 (Parser.Diagnostics,
                  Sloc_Range (Parser.TDH.all, First_Garbage_Token),
                  To_Text
                    ("End of input expected, got """
                     & Token_Kind_Name
                         (To_Token_Kind (First_Garbage_Token.Kind))
                     & """"));
            end;

         --  Else, the last fail pos is further down the line, and we want to
         --  have the diagnostic of what exactly failed.
         else
            Add_Last_Fail_Diagnostic (Parser);
         end if;
      end if;

   end Process_Parsing_Error;

   -----------
   -- Parse --
   -----------

   function Parse
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True;
      Rule           : Grammar_Rule) return Parsed_Node
   is
      Result : Bare_Turkixir_Node;
   begin
      case Rule is
         when Name_Rule =>
            Result := Name_Transform_Parse_0
              (Parser, First_Token_Index);
         when Number_Rule =>
            Result := Number_Transform_Parse_0
              (Parser, First_Token_Index);
         when String_Rule =>
            Result := String_Transform_Parse_0
              (Parser, First_Token_Index);
         when Cat_String_Rule =>
            Result := Cat_String_Transform_Parse_0
              (Parser, First_Token_Index);
         when Nl_Rule =>
            Result := Nl_Transform_Parse_0
              (Parser, First_Token_Index);
         when Main_Rule_Rule =>
            Result := Main_Rule_Transform_Parse_0
              (Parser, First_Token_Index);
         when Decorator_Rule =>
            Result := Decorator_Transform_Parse_0
              (Parser, First_Token_Index);
         when Decorators_Rule =>
            Result := Decorators_List_Parse_0
              (Parser, First_Token_Index);
         when Decorated_Rule =>
            Result := Decorated_Transform_Parse_0
              (Parser, First_Token_Index);
         when Func_Def_Rule =>
            Result := Func_Def_Transform_Parse_0
              (Parser, First_Token_Index);
         when Parameters_Rule =>
            Result := Parameters_Extract_Parse_0
              (Parser, First_Token_Index);
         when Varargslist_Rule =>
            Result := Varargslist_Transform_Parse_1
              (Parser, First_Token_Index);
         when Fpdef_Rule =>
            Result := Fpdef_Or_Parse_0
              (Parser, First_Token_Index);
         when Name_List_Rule =>
            Result := Name_List_Extract_Parse_0
              (Parser, First_Token_Index);
         when Stmt_Rule =>
            Result := Stmt_Or_Parse_0
              (Parser, First_Token_Index);
         when Simple_Stmt_Rule =>
            Result := Simple_Stmt_Extract_Parse_1
              (Parser, First_Token_Index);
         when Small_Stmt_Rule =>
            Result := Small_Stmt_Or_Parse_0
              (Parser, First_Token_Index);
         when Expr_Stmt_Rule =>
            Result := Expr_Stmt_Or_Parse_3
              (Parser, First_Token_Index);
         when Print_Stmt_Rule =>
            Result := Print_Stmt_Or_Parse_0
              (Parser, First_Token_Index);
         when Del_Stmt_Rule =>
            Result := Del_Stmt_Transform_Parse_0
              (Parser, First_Token_Index);
         when Pass_Stmt_Rule =>
            Result := Pass_Stmt_Transform_Parse_0
              (Parser, First_Token_Index);
         when Flow_Stmt_Rule =>
            Result := Flow_Stmt_Or_Parse_0
              (Parser, First_Token_Index);
         when Break_Stmt_Rule =>
            Result := Break_Stmt_Transform_Parse_0
              (Parser, First_Token_Index);
         when Continue_Stmt_Rule =>
            Result := Continue_Stmt_Transform_Parse_0
              (Parser, First_Token_Index);
         when Return_Stmt_Rule =>
            Result := Return_Stmt_Transform_Parse_0
              (Parser, First_Token_Index);
         when Yield_Stmt_Rule =>
            Result := Yield_Stmt_Defer_Parse_0
              (Parser, First_Token_Index);
         when Raise_Stmt_Rule =>
            Result := Raise_Stmt_Transform_Parse_0
              (Parser, First_Token_Index);
         when Import_Stmt_Rule =>
            Result := Import_Stmt_Or_Parse_0
              (Parser, First_Token_Index);
         when Import_Name_Rule =>
            Result := Import_Name_Transform_Parse_0
              (Parser, First_Token_Index);
         when Dot_Rule =>
            Result := Dot_Transform_Parse_0
              (Parser, First_Token_Index);
         when Import_From_Rule =>
            Result := Import_From_Transform_Parse_2
              (Parser, First_Token_Index);
         when As_Name_Rule =>
            Result := As_Name_Transform_Parse_0
              (Parser, First_Token_Index);
         when Dotted_As_Name_Rule =>
            Result := Dotted_As_Name_Transform_Parse_0
              (Parser, First_Token_Index);
         when Import_As_Names_Rule =>
            Result := Import_As_Names_Extract_Parse_0
              (Parser, First_Token_Index);
         when Dotted_As_Names_Rule =>
            Result := Dotted_As_Names_Extract_Parse_0
              (Parser, First_Token_Index);
         when Dotted_Name_Rule =>
            Result := Dotted_Name_Or_Parse_0
              (Parser, First_Token_Index);
         when Global_Stmt_Rule =>
            Result := Global_Stmt_Transform_Parse_0
              (Parser, First_Token_Index);
         when Exec_Stmt_Rule =>
            Result := Exec_Stmt_Transform_Parse_0
              (Parser, First_Token_Index);
         when Assert_Stmt_Rule =>
            Result := Assert_Stmt_Transform_Parse_0
              (Parser, First_Token_Index);
         when Compound_Stmt_Rule =>
            Result := Compound_Stmt_Or_Parse_0
              (Parser, First_Token_Index);
         when Else_Part_Rule =>
            Result := Else_Part_Transform_Parse_0
              (Parser, First_Token_Index);
         when If_Stmt_Rule =>
            Result := If_Stmt_Transform_Parse_1
              (Parser, First_Token_Index);
         when While_Stmt_Rule =>
            Result := While_Stmt_Transform_Parse_0
              (Parser, First_Token_Index);
         when For_Stmt_Rule =>
            Result := For_Stmt_Transform_Parse_0
              (Parser, First_Token_Index);
         when Try_Stmt_Rule =>
            Result := Try_Stmt_Transform_Parse_2
              (Parser, First_Token_Index);
         when With_Stmt_Rule =>
            Result := With_Stmt_Transform_Parse_0
              (Parser, First_Token_Index);
         when With_Item_Rule =>
            Result := With_Item_Transform_Parse_0
              (Parser, First_Token_Index);
         when Suite_Rule =>
            Result := Suite_Or_Parse_0
              (Parser, First_Token_Index);
         when Test_Rule =>
            Result := Test_Or_Parse_0
              (Parser, First_Token_Index);
         when Or_Test_Rule =>
            Result := Or_Test_Or_Parse_0
              (Parser, First_Token_Index);
         when And_Test_Rule =>
            Result := And_Test_Or_Parse_0
              (Parser, First_Token_Index);
         when Not_Test_Rule =>
            Result := Not_Test_Or_Parse_0
              (Parser, First_Token_Index);
         when Comparison_Rule =>
            Result := Comparison_Or_Parse_1
              (Parser, First_Token_Index);
         when Expr_Rule =>
            Result := Expr_Or_Parse_0
              (Parser, First_Token_Index);
         when Xor_Expr_Rule =>
            Result := Xor_Expr_Or_Parse_0
              (Parser, First_Token_Index);
         when And_Expr_Rule =>
            Result := And_Expr_Or_Parse_0
              (Parser, First_Token_Index);
         when Shift_Expr_Rule =>
            Result := Shift_Expr_Or_Parse_1
              (Parser, First_Token_Index);
         when Arith_Expr_Rule =>
            Result := Arith_Expr_Or_Parse_1
              (Parser, First_Token_Index);
         when Term_Rule =>
            Result := Term_Or_Parse_1
              (Parser, First_Token_Index);
         when Factor_Rule =>
            Result := Factor_Or_Parse_1
              (Parser, First_Token_Index);
         when Power_Rule =>
            Result := Power_Or_Parse_0
              (Parser, First_Token_Index);
         when Atom_Expr_Rule =>
            Result := Atom_Expr_Or_Parse_0
              (Parser, First_Token_Index);
         when Dict_Assoc_Rule =>
            Result := Dict_Assoc_Transform_Parse_0
              (Parser, First_Token_Index);
         when Yield_Expr_Rule =>
            Result := Yield_Expr_Transform_Parse_0
              (Parser, First_Token_Index);
         when Atom_Rule =>
            Result := Atom_Or_Parse_0
              (Parser, First_Token_Index);
         when Set_Lit_Rule =>
            Result := Set_Lit_Transform_Parse_0
              (Parser, First_Token_Index);
         when Lambdef_Rule =>
            Result := Lambdef_Transform_Parse_0
              (Parser, First_Token_Index);
         when Subscript_List_Rule =>
            Result := Subscript_List_Extract_Parse_0
              (Parser, First_Token_Index);
         when Subscript_Rule =>
            Result := Subscript_Or_Parse_0
              (Parser, First_Token_Index);
         when Expr_List_Rule =>
            Result := Expr_List_Extract_Parse_0
              (Parser, First_Token_Index);
         when Test_List_Rule =>
            Result := Test_List_Extract_Parse_0
              (Parser, First_Token_Index);
         when Empty_Test_List_Rule =>
            Result := Empty_Test_List_Extract_Parse_0
              (Parser, First_Token_Index);
         when Class_Def_Rule =>
            Result := Class_Def_Transform_Parse_0
              (Parser, First_Token_Index);
         when Arg_List_Rule =>
            Result := Arg_List_Extract_Parse_1
              (Parser, First_Token_Index);
         when List_Iter_Rule =>
            Result := List_Iter_Or_Parse_0
              (Parser, First_Token_Index);
         when List_For_Rule =>
            Result := List_For_Transform_Parse_0
              (Parser, First_Token_Index);
         when List_If_Rule =>
            Result := List_If_Transform_Parse_0
              (Parser, First_Token_Index);
         when Comp_Iter_Rule =>
            Result := Comp_Iter_Or_Parse_0
              (Parser, First_Token_Index);
         when Comp_For_Rule =>
            Result := Comp_For_Transform_Parse_0
              (Parser, First_Token_Index);
         when Comp_If_Rule =>
            Result := Comp_If_Transform_Parse_0
              (Parser, First_Token_Index);
      end case;
      Process_Parsing_Error (Parser, Check_Complete);
      Set_Parents (Result, null);
      return Parsed_Node (Result);
   exception
      when Exc : Property_Error =>
         Append
           (Parser.Diagnostics,
            No_Source_Location_Range,
            To_Text ("Error during parsing: "
                     & Ada.Exceptions.Exception_Message (Exc)));
         return Parsed_Node (No_Bare_Turkixir_Node);
   end Parse;

   


function Name_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Id
is
   use Bare_Id_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_13 :
            Token_Index
               := No_Token_Index;
      Token_Pos_18 :
            Token_Index
               := No_Token_Index;
      Token_Res_18 :
            Token_Index
               := No_Token_Index;
      Transform_Res_13 :
            Bare_Id
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Name_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_13 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_13;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_13;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_13 := Pos;



--  Start tok_code

Token_Res_18 := Row_Pos_13;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_18));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Id)
   then
       Token_Pos_18 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_13 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_13,
             Expected_Token_Id => Turkixir_T_T__Id,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_18 := Row_Pos_13 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_18 /= No_Token_Index then

   Row_Pos_13 := Token_Pos_18;

else
   Row_Pos_13 := No_Token_Index;
   goto Exit_Row_11_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_11_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_13 /= No_Token_Index then

   Transform_Res_13 := Allocate_Id (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_13,
      Kind => Turkixir_Id,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_13 = Pos
                            then No_Token_Index
                            else Row_Pos_13 - 1));




end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Name_Transform_Parse_0_Memo,
      Row_Pos_13 /= No_Token_Index,
      Transform_Res_13,
      Pos,
      Row_Pos_13);


   Parser.Current_Pos := Row_Pos_13;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_13;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Name_Transform_Parse_0;

   


function Name_List_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Id_List
is
   use Bare_Id_List_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_26 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_2 :
            Token_Index
               := No_Token_Index;
      Tmp_List_2 :
            Free_Parse_List;
      Defer_Pos_41 :
            Token_Index
               := No_Token_Index;
      Defer_Res_41 :
            Bare_Id
               := No_Bare_Turkixir_Node;
      Token_Pos_34 :
            Token_Index
               := No_Token_Index;
      Token_Res_34 :
            Token_Index
               := No_Token_Index;
      List_Pos_2 :
            Token_Index
               := No_Token_Index;
      List_Res_2 :
            Bare_Id_List
               := No_Bare_Turkixir_Node;
      Token_Pos_35 :
            Token_Index
               := No_Token_Index;
      Token_Res_35 :
            Token_Index
               := No_Token_Index;


   M : Memo_Entry := Get (Parser.Private_Part.Name_List_Extract_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res_2 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return List_Res_2;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return List_Res_2;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start row_code

Row_Pos_26 := Pos;



--  Start list_code

    List_Pos_2 := No_Token_Index;



Lst_Cpos_2 := Row_Pos_26;
Tmp_List_2 := Get_Parse_List (Parser);

loop
   
Defer_Res_41 :=
   Name_Transform_Parse_0 (Parser, Lst_Cpos_2);
Defer_Pos_41 := Parser.Current_Pos;


   exit when Defer_Pos_41 = No_Token_Index;

   List_Pos_2 := Defer_Pos_41;
   Lst_Cpos_2 := List_Pos_2;

   Tmp_List_2.Nodes.Append (Defer_Res_41);

      
--  Start tok_code

Token_Res_34 := Lst_Cpos_2;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_34));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_34 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos_2 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos_2,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_34 := Lst_Cpos_2 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos_34 /= No_Token_Index then
          Lst_Cpos_2 := Token_Pos_34;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_2.Nodes.Length;
begin
   List_Res_2 :=
      Allocate_Id_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_26;
      Token_End := (if Lst_Cpos_2 = Row_Pos_26
                    then Row_Pos_26
                    else Lst_Cpos_2 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_26, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_2,
      Kind              => Turkixir_Id_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_2,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_2.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_2.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_2);

--  End list_code



if List_Pos_2 /= No_Token_Index then

   Row_Pos_26 := List_Pos_2;

else
   Row_Pos_26 := No_Token_Index;
   goto Exit_Row_19_0;

end if;


--  Start opt_code




--  Start tok_code

Token_Res_35 := Row_Pos_26;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_35));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_35 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_26 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_26,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_35 := Row_Pos_26 + 1;
   end if;
end;

--  End tok_code


if Token_Pos_35 = No_Token_Index then

        Token_Res_35 := No_Token_Index;


    Token_Pos_35 := Row_Pos_26;


end if;

--  End opt_code



if Token_Pos_35 /= No_Token_Index then

   Row_Pos_26 := Token_Pos_35;

else
   Row_Pos_26 := No_Token_Index;
   goto Exit_Row_19_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_19_0>>
pragma Warnings (On, "referenced");

--  End row_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Name_List_Extract_Parse_0_Memo,
      Row_Pos_26 /= No_Token_Index,
      List_Res_2,
      Pos,
      Row_Pos_26);


   Parser.Current_Pos := Row_Pos_26;

   Exit_Call (Parser, Call_Depth);
   return List_Res_2;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Name_List_Extract_Parse_0;

   


function Fpdef_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node
is
   use Bare_Turkixir_Node_Memos;

   Call_Depth : aliased Natural;

      Defer_Pos_39 :
            Token_Index
               := No_Token_Index;
      Defer_Res_39 :
            Bare_Id
               := No_Bare_Turkixir_Node;
      Row_Pos_25 :
            Token_Index
               := No_Token_Index;
      Token_Pos_32 :
            Token_Index
               := No_Token_Index;
      Token_Res_32 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_40 :
            Token_Index
               := No_Token_Index;
      Defer_Res_40 :
            Bare_Id_List
               := No_Bare_Turkixir_Node;
      Token_Pos_33 :
            Token_Index
               := No_Token_Index;
      Token_Res_33 :
            Token_Index
               := No_Token_Index;
      Or_Pos_13 :
            Token_Index
               := No_Token_Index;
      Or_Res_13 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Fpdef_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_13 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_13;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_13;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_13 := No_Token_Index;
Or_Res_13 := No_Bare_Turkixir_Node;
    
Defer_Res_39 :=
   Name_Transform_Parse_0 (Parser, Pos);
Defer_Pos_39 := Parser.Current_Pos;

    if Defer_Pos_39 /= No_Token_Index then
        Or_Pos_13 := Defer_Pos_39;
        Or_Res_13 := Defer_Res_39;
        goto Exit_Or_13;
    end if;
    
--  Start row_code

Row_Pos_25 := Pos;



--  Start tok_code

Token_Res_32 := Row_Pos_25;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_32));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_L_Par)
   then
       Token_Pos_32 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_25 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_25,
             Expected_Token_Id => Turkixir_T_T_L_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_32 := Row_Pos_25 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_32 /= No_Token_Index then

   Row_Pos_25 := Token_Pos_32;

else
   Row_Pos_25 := No_Token_Index;
   goto Exit_Row_18_0;

end if;


Defer_Res_40 :=
   Name_List_Extract_Parse_0 (Parser, Row_Pos_25);
Defer_Pos_40 := Parser.Current_Pos;



if Defer_Pos_40 /= No_Token_Index then

   Row_Pos_25 := Defer_Pos_40;

else
   Row_Pos_25 := No_Token_Index;
   goto Exit_Row_18_0;

end if;


--  Start tok_code

Token_Res_33 := Row_Pos_25;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_33));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_R_Par)
   then
       Token_Pos_33 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_25 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_25,
             Expected_Token_Id => Turkixir_T_T_R_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_33 := Row_Pos_25 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_33 /= No_Token_Index then

   Row_Pos_25 := Token_Pos_33;

else
   Row_Pos_25 := No_Token_Index;
   goto Exit_Row_18_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_18_0>>
pragma Warnings (On, "referenced");

--  End row_code

    if Row_Pos_25 /= No_Token_Index then
        Or_Pos_13 := Row_Pos_25;
        Or_Res_13 := Defer_Res_40;
        goto Exit_Or_13;
    end if;
<<Exit_Or_13>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Fpdef_Or_Parse_0_Memo,
      Or_Pos_13 /= No_Token_Index,
      Or_Res_13,
      Pos,
      Or_Pos_13);


   Parser.Current_Pos := Or_Pos_13;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_13;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Fpdef_Or_Parse_0;

   


function Varargslist_Transform_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Params
is
   use Bare_Params_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_22 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_1 :
            Token_Index
               := No_Token_Index;
      Tmp_List_1 :
            Free_Parse_List;
      Row_Pos_23 :
            Token_Index
               := No_Token_Index;
      Token_Pos_28 :
            Token_Index
               := No_Token_Index;
      Token_Res_28 :
            Token_Index
               := No_Token_Index;
      Opt_Res_0 :
            Bare_Var_Args_Flag
               := No_Bare_Turkixir_Node;
      Token_Pos_29 :
            Token_Index
               := No_Token_Index;
      Token_Res_29 :
            Token_Index
               := No_Token_Index;
      Opt_Res_1 :
            Bare_Kw_Args_Flag
               := No_Bare_Turkixir_Node;
      Defer_Pos_37 :
            Token_Index
               := No_Token_Index;
      Defer_Res_37 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Row_Pos_24 :
            Token_Index
               := No_Token_Index;
      Token_Pos_30 :
            Token_Index
               := No_Token_Index;
      Token_Res_30 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_38 :
            Token_Index
               := No_Token_Index;
      Defer_Res_38 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_20 :
            Bare_Single_Param
               := No_Bare_Turkixir_Node;
      Token_Pos_31 :
            Token_Index
               := No_Token_Index;
      Token_Res_31 :
            Token_Index
               := No_Token_Index;
      List_Pos_1 :
            Token_Index
               := No_Token_Index;
      List_Res_1 :
            Bare_Single_Param_List
               := No_Bare_Turkixir_Node;
      Transform_Res_21 :
            Bare_Params
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Varargslist_Transform_Parse_1_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_21 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_21;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_21;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_22 := Pos;



--  Start list_code

    List_Pos_1 := Row_Pos_22;



Lst_Cpos_1 := Row_Pos_22;
Tmp_List_1 := Get_Parse_List (Parser);

loop
   
--  Start transform_code


--  Start row_code

Row_Pos_23 := Lst_Cpos_1;



--  Start opt_code




--  Start tok_code

Token_Res_28 := Row_Pos_23;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_28));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Multiply)
   then
       Token_Pos_28 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_23 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_23,
             Expected_Token_Id => Turkixir_T_T__Multiply,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_28 := Row_Pos_23 + 1;
   end if;
end;

--  End tok_code


if Token_Pos_28 = No_Token_Index then

         Opt_Res_0 := Allocate_Var_Args_Flag_Absent (Parser.Mem_Pool);
         Initialize
           (Self              => Opt_Res_0,
            Kind              => Turkixir_Var_Args_Flag_Absent,
            Unit              => Parser.Unit,
            Token_Start_Index => Row_Pos_23,
            Token_End_Index   => No_Token_Index);


    Token_Pos_28 := Row_Pos_23;

else

      Opt_Res_0 := Allocate_Var_Args_Flag_Present (Parser.Mem_Pool);
      Initialize
        (Self              => Opt_Res_0,
         Kind              => Turkixir_Var_Args_Flag_Present,
         Unit              => Parser.Unit,
         Token_Start_Index => Row_Pos_23,
         Token_End_Index   => Token_Pos_28 - 1);

end if;

--  End opt_code



if Token_Pos_28 /= No_Token_Index then

   Row_Pos_23 := Token_Pos_28;

else
   Row_Pos_23 := No_Token_Index;
   goto Exit_Row_17_0;

end if;


--  Start opt_code




--  Start tok_code

Token_Res_29 := Row_Pos_23;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_29));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Power)
   then
       Token_Pos_29 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_23 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_23,
             Expected_Token_Id => Turkixir_T_T__Power,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_29 := Row_Pos_23 + 1;
   end if;
end;

--  End tok_code


if Token_Pos_29 = No_Token_Index then

         Opt_Res_1 := Allocate_Kw_Args_Flag_Absent (Parser.Mem_Pool);
         Initialize
           (Self              => Opt_Res_1,
            Kind              => Turkixir_Kw_Args_Flag_Absent,
            Unit              => Parser.Unit,
            Token_Start_Index => Row_Pos_23,
            Token_End_Index   => No_Token_Index);


    Token_Pos_29 := Row_Pos_23;

else

      Opt_Res_1 := Allocate_Kw_Args_Flag_Present (Parser.Mem_Pool);
      Initialize
        (Self              => Opt_Res_1,
         Kind              => Turkixir_Kw_Args_Flag_Present,
         Unit              => Parser.Unit,
         Token_Start_Index => Row_Pos_23,
         Token_End_Index   => Token_Pos_29 - 1);

end if;

--  End opt_code



if Token_Pos_29 /= No_Token_Index then

   Row_Pos_23 := Token_Pos_29;

else
   Row_Pos_23 := No_Token_Index;
   goto Exit_Row_17_0;

end if;


Defer_Res_37 :=
   Fpdef_Or_Parse_0 (Parser, Row_Pos_23);
Defer_Pos_37 := Parser.Current_Pos;



if Defer_Pos_37 /= No_Token_Index then

   Row_Pos_23 := Defer_Pos_37;

else
   Row_Pos_23 := No_Token_Index;
   goto Exit_Row_17_0;

end if;


--  Start opt_code




--  Start row_code

Row_Pos_24 := Row_Pos_23;



--  Start tok_code

Token_Res_30 := Row_Pos_24;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_30));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Assign)
   then
       Token_Pos_30 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_24 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_24,
             Expected_Token_Id => Turkixir_T_T__Assign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_30 := Row_Pos_24 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_30 /= No_Token_Index then

   Row_Pos_24 := Token_Pos_30;

else
   Row_Pos_24 := No_Token_Index;
   goto Exit_Row_20_0;

end if;


Defer_Res_38 :=
   Test_Or_Parse_0 (Parser, Row_Pos_24);
Defer_Pos_38 := Parser.Current_Pos;



if Defer_Pos_38 /= No_Token_Index then

   Row_Pos_24 := Defer_Pos_38;

else
   Row_Pos_24 := No_Token_Index;
   goto Exit_Row_20_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_20_0>>
pragma Warnings (On, "referenced");

--  End row_code


if Row_Pos_24 = No_Token_Index then

        Defer_Res_38 := No_Bare_Turkixir_Node;


    Row_Pos_24 := Row_Pos_23;


end if;

--  End opt_code



if Row_Pos_24 /= No_Token_Index then

   Row_Pos_23 := Row_Pos_24;

else
   Row_Pos_23 := No_Token_Index;
   goto Exit_Row_17_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_17_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_23 /= No_Token_Index then

   Transform_Res_20 := Allocate_Single_Param (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_20,
      Kind => Turkixir_Single_Param,
      Unit => Parser.Unit,

      Token_Start_Index => Lst_Cpos_1,
      Token_End_Index   => (if Row_Pos_23 = Lst_Cpos_1
                            then No_Token_Index
                            else Row_Pos_23 - 1));

      Initialize_Fields_For_Single_Param
        (Self => Transform_Res_20, Single_Param_F_Is_Varargs => Opt_Res_0, Single_Param_F_Is_Kwargs => Opt_Res_1, Single_Param_F_Name => Defer_Res_37, Single_Param_F_Default_Value => Defer_Res_38);

         if Opt_Res_0 /= null and then Is_Incomplete (Opt_Res_0) then
            Transform_Res_20.Last_Attempted_Child := 0;
         elsif Opt_Res_0 /= null and then not Is_Ghost (Opt_Res_0) then
            Transform_Res_20.Last_Attempted_Child := -1;
         end if;
         if Opt_Res_1 /= null and then Is_Incomplete (Opt_Res_1) then
            Transform_Res_20.Last_Attempted_Child := 0;
         elsif Opt_Res_1 /= null and then not Is_Ghost (Opt_Res_1) then
            Transform_Res_20.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_37 /= null and then Is_Incomplete (Defer_Res_37) then
            Transform_Res_20.Last_Attempted_Child := 0;
         elsif Defer_Res_37 /= null and then not Is_Ghost (Defer_Res_37) then
            Transform_Res_20.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_38 /= null and then Is_Incomplete (Defer_Res_38) then
            Transform_Res_20.Last_Attempted_Child := 0;
         elsif Defer_Res_38 /= null and then not Is_Ghost (Defer_Res_38) then
            Transform_Res_20.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   exit when Row_Pos_23 = No_Token_Index;

   List_Pos_1 := Row_Pos_23;
   Lst_Cpos_1 := List_Pos_1;

   Tmp_List_1.Nodes.Append (Transform_Res_20);

      
--  Start tok_code

Token_Res_31 := Lst_Cpos_1;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_31));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_31 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos_1 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos_1,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_31 := Lst_Cpos_1 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos_31 /= No_Token_Index then
          Lst_Cpos_1 := Token_Pos_31;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_1.Nodes.Length;
begin
   List_Res_1 :=
      Allocate_Single_Param_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_22;
      Token_End := (if Lst_Cpos_1 = Row_Pos_22
                    then Row_Pos_22
                    else Lst_Cpos_1 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_22, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_1,
      Kind              => Turkixir_Single_Param_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_1,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_1.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_1.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_1);

--  End list_code



if List_Pos_1 /= No_Token_Index then

   Row_Pos_22 := List_Pos_1;

else
   Row_Pos_22 := No_Token_Index;
   goto Exit_Row_16_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_16_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_22 /= No_Token_Index then

   Transform_Res_21 := Allocate_Params (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_21,
      Kind => Turkixir_Params,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_22 = Pos
                            then No_Token_Index
                            else Row_Pos_22 - 1));

      Initialize_Fields_For_Params
        (Self => Transform_Res_21, Params_F_Single_Params => List_Res_1);

         if List_Res_1 /= null and then Is_Incomplete (List_Res_1) then
            Transform_Res_21.Last_Attempted_Child := 0;
         elsif List_Res_1 /= null and then not Is_Ghost (List_Res_1) then
            Transform_Res_21.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Varargslist_Transform_Parse_1_Memo,
      Row_Pos_22 /= No_Token_Index,
      Transform_Res_21,
      Pos,
      Row_Pos_22);


   Parser.Current_Pos := Row_Pos_22;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_21;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Varargslist_Transform_Parse_1;

   


function Lambdef_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Lambda_Def
is
   use Bare_Lambda_Def_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_21 :
            Token_Index
               := No_Token_Index;
      Token_Pos_26 :
            Token_Index
               := No_Token_Index;
      Token_Res_26 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_35 :
            Token_Index
               := No_Token_Index;
      Defer_Res_35 :
            Bare_Params
               := No_Bare_Turkixir_Node;
      Token_Pos_27 :
            Token_Index
               := No_Token_Index;
      Token_Res_27 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_36 :
            Token_Index
               := No_Token_Index;
      Defer_Res_36 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_19 :
            Bare_Lambda_Def
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Lambdef_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_19 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_19;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_19;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_21 := Pos;



--  Start tok_code

Token_Res_26 := Row_Pos_21;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_26));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Lambda)
   then
       Token_Pos_26 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_21 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_21,
             Expected_Token_Id => Turkixir_T_T__Lambda,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_26 := Row_Pos_21 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_26 /= No_Token_Index then

   Row_Pos_21 := Token_Pos_26;

else
   Row_Pos_21 := No_Token_Index;
   goto Exit_Row_15_0;

end if;


Defer_Res_35 :=
   Varargslist_Transform_Parse_1 (Parser, Row_Pos_21);
Defer_Pos_35 := Parser.Current_Pos;



if Defer_Pos_35 /= No_Token_Index then

   Row_Pos_21 := Defer_Pos_35;

else
   Row_Pos_21 := No_Token_Index;
   goto Exit_Row_15_0;

end if;


--  Start tok_code

Token_Res_27 := Row_Pos_21;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_27));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_27 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_21 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_21,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_27 := Row_Pos_21 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_27 /= No_Token_Index then

   Row_Pos_21 := Token_Pos_27;

else
   Row_Pos_21 := No_Token_Index;
   goto Exit_Row_15_0;

end if;


Defer_Res_36 :=
   Test_Or_Parse_0 (Parser, Row_Pos_21);
Defer_Pos_36 := Parser.Current_Pos;



if Defer_Pos_36 /= No_Token_Index then

   Row_Pos_21 := Defer_Pos_36;

else
   Row_Pos_21 := No_Token_Index;
   goto Exit_Row_15_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_15_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_21 /= No_Token_Index then

   Transform_Res_19 := Allocate_Lambda_Def (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_19,
      Kind => Turkixir_Lambda_Def,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_21 = Pos
                            then No_Token_Index
                            else Row_Pos_21 - 1));

      Initialize_Fields_For_Lambda_Def
        (Self => Transform_Res_19, Lambda_Def_F_Args => Defer_Res_35, Lambda_Def_F_Expr => Defer_Res_36);

         if Defer_Res_35 /= null and then Is_Incomplete (Defer_Res_35) then
            Transform_Res_19.Last_Attempted_Child := 0;
         elsif Defer_Res_35 /= null and then not Is_Ghost (Defer_Res_35) then
            Transform_Res_19.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_36 /= null and then Is_Incomplete (Defer_Res_36) then
            Transform_Res_19.Last_Attempted_Child := 0;
         elsif Defer_Res_36 /= null and then not Is_Ghost (Defer_Res_36) then
            Transform_Res_19.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Lambdef_Transform_Parse_0_Memo,
      Row_Pos_21 /= No_Token_Index,
      Transform_Res_19,
      Pos,
      Row_Pos_21);


   Parser.Current_Pos := Row_Pos_21;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_19;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Lambdef_Transform_Parse_0;

   


function Xor_Expr_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_43 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_56 :
            Token_Index
               := No_Token_Index;
      Defer_Res_56 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_53 :
            Token_Index
               := No_Token_Index;
      Token_Res_53 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_57 :
            Token_Index
               := No_Token_Index;
      Defer_Res_57 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_38 :
            Bare_Xor_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_58 :
            Token_Index
               := No_Token_Index;
      Defer_Res_58 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_20 :
            Token_Index
               := No_Token_Index;
      Or_Res_20 :
            Bare_Expr
               := No_Bare_Turkixir_Node;

      Mem_Pos : Token_Index := Pos;
      Mem_Res : Bare_Expr := No_Bare_Turkixir_Node;

   M : Memo_Entry := Get (Parser.Private_Part.Xor_Expr_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_20 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_20;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_20;
   end if;

       Set (Parser.Private_Part.Xor_Expr_Or_Parse_0_Memo, False, Or_Res_20, Pos, Mem_Pos);

       <<Try_Again>>



   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_20 := No_Token_Index;
Or_Res_20 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_43 := Pos;



Defer_Res_56 :=
   Xor_Expr_Or_Parse_0 (Parser, Row_Pos_43);
Defer_Pos_56 := Parser.Current_Pos;



if Defer_Pos_56 /= No_Token_Index then

   Row_Pos_43 := Defer_Pos_56;

else
   Row_Pos_43 := No_Token_Index;
   goto Exit_Row_38_0;

end if;


--  Start tok_code

Token_Res_53 := Row_Pos_43;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_53));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Xor)
   then
       Token_Pos_53 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_43 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_43,
             Expected_Token_Id => Turkixir_T_T__Xor,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_53 := Row_Pos_43 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_53 /= No_Token_Index then

   Row_Pos_43 := Token_Pos_53;

else
   Row_Pos_43 := No_Token_Index;
   goto Exit_Row_38_0;

end if;


Defer_Res_57 :=
   And_Expr_Or_Parse_0 (Parser, Row_Pos_43);
Defer_Pos_57 := Parser.Current_Pos;



if Defer_Pos_57 /= No_Token_Index then

   Row_Pos_43 := Defer_Pos_57;

else
   Row_Pos_43 := No_Token_Index;
   goto Exit_Row_38_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_38_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_43 /= No_Token_Index then

   Transform_Res_38 := Allocate_Xor_Expr (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_38,
      Kind => Turkixir_Xor_Expr,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_43 = Pos
                            then No_Token_Index
                            else Row_Pos_43 - 1));

      Initialize_Fields_For_Xor_Expr
        (Self => Transform_Res_38, Xor_Expr_F_Left => Defer_Res_56, Xor_Expr_F_Right => Defer_Res_57);

         if Defer_Res_56 /= null and then Is_Incomplete (Defer_Res_56) then
            Transform_Res_38.Last_Attempted_Child := 0;
         elsif Defer_Res_56 /= null and then not Is_Ghost (Defer_Res_56) then
            Transform_Res_38.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_57 /= null and then Is_Incomplete (Defer_Res_57) then
            Transform_Res_38.Last_Attempted_Child := 0;
         elsif Defer_Res_57 /= null and then not Is_Ghost (Defer_Res_57) then
            Transform_Res_38.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_43 /= No_Token_Index then
        Or_Pos_20 := Row_Pos_43;
        Or_Res_20 := Transform_Res_38;
        goto Exit_Or_20;
    end if;
    
Defer_Res_58 :=
   And_Expr_Or_Parse_0 (Parser, Pos);
Defer_Pos_58 := Parser.Current_Pos;

    if Defer_Pos_58 /= No_Token_Index then
        Or_Pos_20 := Defer_Pos_58;
        Or_Res_20 := Defer_Res_58;
        goto Exit_Or_20;
    end if;
<<Exit_Or_20>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

      if Or_Pos_20 > Mem_Pos then
         Mem_Pos := Or_Pos_20;
         Mem_Res := Or_Res_20;
         Set
           (Parser.Private_Part.Xor_Expr_Or_Parse_0_Memo,
            Or_Pos_20 /= No_Token_Index,
            Or_Res_20,
            Pos,
            Or_Pos_20);
         goto Try_Again;

      elsif Mem_Pos > Pos then
         Or_Res_20 := Mem_Res;
         Or_Pos_20 := Mem_Pos;
         goto No_Memo;
      end if;

   Set
     (Parser.Private_Part.Xor_Expr_Or_Parse_0_Memo,
      Or_Pos_20 /= No_Token_Index,
      Or_Res_20,
      Pos,
      Or_Pos_20);

       <<No_Memo>>

   Parser.Current_Pos := Or_Pos_20;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_20;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Xor_Expr_Or_Parse_0;

   


function Expr_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_42 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_53 :
            Token_Index
               := No_Token_Index;
      Defer_Res_53 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_52 :
            Token_Index
               := No_Token_Index;
      Token_Res_52 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_54 :
            Token_Index
               := No_Token_Index;
      Defer_Res_54 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_37 :
            Bare_Or_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_55 :
            Token_Index
               := No_Token_Index;
      Defer_Res_55 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_19 :
            Token_Index
               := No_Token_Index;
      Or_Res_19 :
            Bare_Expr
               := No_Bare_Turkixir_Node;

      Mem_Pos : Token_Index := Pos;
      Mem_Res : Bare_Expr := No_Bare_Turkixir_Node;

   M : Memo_Entry := Get (Parser.Private_Part.Expr_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_19 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_19;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_19;
   end if;

       Set (Parser.Private_Part.Expr_Or_Parse_0_Memo, False, Or_Res_19, Pos, Mem_Pos);

       <<Try_Again>>



   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_19 := No_Token_Index;
Or_Res_19 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_42 := Pos;



Defer_Res_53 :=
   Expr_Or_Parse_0 (Parser, Row_Pos_42);
Defer_Pos_53 := Parser.Current_Pos;



if Defer_Pos_53 /= No_Token_Index then

   Row_Pos_42 := Defer_Pos_53;

else
   Row_Pos_42 := No_Token_Index;
   goto Exit_Row_37_0;

end if;


--  Start tok_code

Token_Res_52 := Row_Pos_42;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_52));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Bin_Or)
   then
       Token_Pos_52 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_42 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_42,
             Expected_Token_Id => Turkixir_T_T__Bin_Or,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_52 := Row_Pos_42 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_52 /= No_Token_Index then

   Row_Pos_42 := Token_Pos_52;

else
   Row_Pos_42 := No_Token_Index;
   goto Exit_Row_37_0;

end if;


Defer_Res_54 :=
   Xor_Expr_Or_Parse_0 (Parser, Row_Pos_42);
Defer_Pos_54 := Parser.Current_Pos;



if Defer_Pos_54 /= No_Token_Index then

   Row_Pos_42 := Defer_Pos_54;

else
   Row_Pos_42 := No_Token_Index;
   goto Exit_Row_37_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_37_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_42 /= No_Token_Index then

   Transform_Res_37 := Allocate_Or_Expr (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_37,
      Kind => Turkixir_Or_Expr,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_42 = Pos
                            then No_Token_Index
                            else Row_Pos_42 - 1));

      Initialize_Fields_For_Or_Expr
        (Self => Transform_Res_37, Or_Expr_F_Left => Defer_Res_53, Or_Expr_F_Right => Defer_Res_54);

         if Defer_Res_53 /= null and then Is_Incomplete (Defer_Res_53) then
            Transform_Res_37.Last_Attempted_Child := 0;
         elsif Defer_Res_53 /= null and then not Is_Ghost (Defer_Res_53) then
            Transform_Res_37.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_54 /= null and then Is_Incomplete (Defer_Res_54) then
            Transform_Res_37.Last_Attempted_Child := 0;
         elsif Defer_Res_54 /= null and then not Is_Ghost (Defer_Res_54) then
            Transform_Res_37.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_42 /= No_Token_Index then
        Or_Pos_19 := Row_Pos_42;
        Or_Res_19 := Transform_Res_37;
        goto Exit_Or_19;
    end if;
    
Defer_Res_55 :=
   Xor_Expr_Or_Parse_0 (Parser, Pos);
Defer_Pos_55 := Parser.Current_Pos;

    if Defer_Pos_55 /= No_Token_Index then
        Or_Pos_19 := Defer_Pos_55;
        Or_Res_19 := Defer_Res_55;
        goto Exit_Or_19;
    end if;
<<Exit_Or_19>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

      if Or_Pos_19 > Mem_Pos then
         Mem_Pos := Or_Pos_19;
         Mem_Res := Or_Res_19;
         Set
           (Parser.Private_Part.Expr_Or_Parse_0_Memo,
            Or_Pos_19 /= No_Token_Index,
            Or_Res_19,
            Pos,
            Or_Pos_19);
         goto Try_Again;

      elsif Mem_Pos > Pos then
         Or_Res_19 := Mem_Res;
         Or_Pos_19 := Mem_Pos;
         goto No_Memo;
      end if;

   Set
     (Parser.Private_Part.Expr_Or_Parse_0_Memo,
      Or_Pos_19 /= No_Token_Index,
      Or_Res_19,
      Pos,
      Or_Pos_19);

       <<No_Memo>>

   Parser.Current_Pos := Or_Pos_19;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_19;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Expr_Or_Parse_0;

   


function Comparison_Or_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_30 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_50 :
            Token_Index
               := No_Token_Index;
      Defer_Res_50 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Row_Pos_31 :
            Token_Index
               := No_Token_Index;
      Token_Pos_39 :
            Token_Index
               := No_Token_Index;
      Token_Res_39 :
            Token_Index
               := No_Token_Index;
      Transform_Res_25 :
            Bare_Comp_Op_Kind_Lt
               := No_Bare_Turkixir_Node;
      Row_Pos_32 :
            Token_Index
               := No_Token_Index;
      Token_Pos_40 :
            Token_Index
               := No_Token_Index;
      Token_Res_40 :
            Token_Index
               := No_Token_Index;
      Transform_Res_26 :
            Bare_Comp_Op_Kind_Gt
               := No_Bare_Turkixir_Node;
      Row_Pos_33 :
            Token_Index
               := No_Token_Index;
      Token_Pos_41 :
            Token_Index
               := No_Token_Index;
      Token_Res_41 :
            Token_Index
               := No_Token_Index;
      Transform_Res_27 :
            Bare_Comp_Op_Kind_Eq
               := No_Bare_Turkixir_Node;
      Row_Pos_34 :
            Token_Index
               := No_Token_Index;
      Token_Pos_42 :
            Token_Index
               := No_Token_Index;
      Token_Res_42 :
            Token_Index
               := No_Token_Index;
      Transform_Res_28 :
            Bare_Comp_Op_Kind_Gte
               := No_Bare_Turkixir_Node;
      Row_Pos_35 :
            Token_Index
               := No_Token_Index;
      Token_Pos_43 :
            Token_Index
               := No_Token_Index;
      Token_Res_43 :
            Token_Index
               := No_Token_Index;
      Transform_Res_29 :
            Bare_Comp_Op_Kind_Lte
               := No_Bare_Turkixir_Node;
      Row_Pos_36 :
            Token_Index
               := No_Token_Index;
      Token_Pos_44 :
            Token_Index
               := No_Token_Index;
      Token_Res_44 :
            Token_Index
               := No_Token_Index;
      Transform_Res_30 :
            Bare_Comp_Op_Kind_Diamond
               := No_Bare_Turkixir_Node;
      Row_Pos_37 :
            Token_Index
               := No_Token_Index;
      Token_Pos_45 :
            Token_Index
               := No_Token_Index;
      Token_Res_45 :
            Token_Index
               := No_Token_Index;
      Transform_Res_31 :
            Bare_Comp_Op_Kind_Noteq
               := No_Bare_Turkixir_Node;
      Row_Pos_38 :
            Token_Index
               := No_Token_Index;
      Token_Pos_46 :
            Token_Index
               := No_Token_Index;
      Token_Res_46 :
            Token_Index
               := No_Token_Index;
      Transform_Res_32 :
            Bare_Comp_Op_Kind_In
               := No_Bare_Turkixir_Node;
      Row_Pos_39 :
            Token_Index
               := No_Token_Index;
      Token_Pos_47 :
            Token_Index
               := No_Token_Index;
      Token_Res_47 :
            Token_Index
               := No_Token_Index;
      Token_Pos_48 :
            Token_Index
               := No_Token_Index;
      Token_Res_48 :
            Token_Index
               := No_Token_Index;
      Transform_Res_33 :
            Bare_Comp_Op_Kind_Notin
               := No_Bare_Turkixir_Node;
      Row_Pos_40 :
            Token_Index
               := No_Token_Index;
      Token_Pos_49 :
            Token_Index
               := No_Token_Index;
      Token_Res_49 :
            Token_Index
               := No_Token_Index;
      Token_Pos_50 :
            Token_Index
               := No_Token_Index;
      Token_Res_50 :
            Token_Index
               := No_Token_Index;
      Transform_Res_34 :
            Bare_Comp_Op_Kind_Isnot
               := No_Bare_Turkixir_Node;
      Row_Pos_41 :
            Token_Index
               := No_Token_Index;
      Token_Pos_51 :
            Token_Index
               := No_Token_Index;
      Token_Res_51 :
            Token_Index
               := No_Token_Index;
      Transform_Res_35 :
            Bare_Comp_Op_Kind_Is
               := No_Bare_Turkixir_Node;
      Or_Pos_17 :
            Token_Index
               := No_Token_Index;
      Or_Res_17 :
            Bare_Comp_Op_Kind
               := No_Bare_Turkixir_Node;
      Defer_Pos_51 :
            Token_Index
               := No_Token_Index;
      Defer_Res_51 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_36 :
            Bare_Comp_Op
               := No_Bare_Turkixir_Node;
      Defer_Pos_52 :
            Token_Index
               := No_Token_Index;
      Defer_Res_52 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_18 :
            Token_Index
               := No_Token_Index;
      Or_Res_18 :
            Bare_Expr
               := No_Bare_Turkixir_Node;

      Mem_Pos : Token_Index := Pos;
      Mem_Res : Bare_Expr := No_Bare_Turkixir_Node;

   M : Memo_Entry := Get (Parser.Private_Part.Comparison_Or_Parse_1_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_18 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_18;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_18;
   end if;

       Set (Parser.Private_Part.Comparison_Or_Parse_1_Memo, False, Or_Res_18, Pos, Mem_Pos);

       <<Try_Again>>



   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_18 := No_Token_Index;
Or_Res_18 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_30 := Pos;



Defer_Res_50 :=
   Comparison_Or_Parse_1 (Parser, Row_Pos_30);
Defer_Pos_50 := Parser.Current_Pos;



if Defer_Pos_50 /= No_Token_Index then

   Row_Pos_30 := Defer_Pos_50;

else
   Row_Pos_30 := No_Token_Index;
   goto Exit_Row_25_0;

end if;


--  Start or_code

Or_Pos_17 := No_Token_Index;
Or_Res_17 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_31 := Row_Pos_30;



--  Start tok_code

Token_Res_39 := Row_Pos_31;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_39));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Lt)
   then
       Token_Pos_39 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_31 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_31,
             Expected_Token_Id => Turkixir_T_T__Lt,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_39 := Row_Pos_31 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_39 /= No_Token_Index then

   Row_Pos_31 := Token_Pos_39;

else
   Row_Pos_31 := No_Token_Index;
   goto Exit_Row_26_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_26_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_31 /= No_Token_Index then

   Transform_Res_25 := Allocate_Comp_Op_Kind_Lt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_25,
      Kind => Turkixir_Comp_Op_Kind_Lt,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_30,
      Token_End_Index   => (if Row_Pos_31 = Row_Pos_30
                            then No_Token_Index
                            else Row_Pos_31 - 1));




end if;

--  End transform_code

    if Row_Pos_31 /= No_Token_Index then
        Or_Pos_17 := Row_Pos_31;
        Or_Res_17 := Transform_Res_25;
        goto Exit_Or_18;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_32 := Row_Pos_30;



--  Start tok_code

Token_Res_40 := Row_Pos_32;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_40));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Gt)
   then
       Token_Pos_40 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_32 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_32,
             Expected_Token_Id => Turkixir_T_T__Gt,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_40 := Row_Pos_32 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_40 /= No_Token_Index then

   Row_Pos_32 := Token_Pos_40;

else
   Row_Pos_32 := No_Token_Index;
   goto Exit_Row_27_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_27_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_32 /= No_Token_Index then

   Transform_Res_26 := Allocate_Comp_Op_Kind_Gt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_26,
      Kind => Turkixir_Comp_Op_Kind_Gt,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_30,
      Token_End_Index   => (if Row_Pos_32 = Row_Pos_30
                            then No_Token_Index
                            else Row_Pos_32 - 1));




end if;

--  End transform_code

    if Row_Pos_32 /= No_Token_Index then
        Or_Pos_17 := Row_Pos_32;
        Or_Res_17 := Transform_Res_26;
        goto Exit_Or_18;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_33 := Row_Pos_30;



--  Start tok_code

Token_Res_41 := Row_Pos_33;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_41));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Equals)
   then
       Token_Pos_41 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_33 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_33,
             Expected_Token_Id => Turkixir_T_T__Equals,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_41 := Row_Pos_33 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_41 /= No_Token_Index then

   Row_Pos_33 := Token_Pos_41;

else
   Row_Pos_33 := No_Token_Index;
   goto Exit_Row_28_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_28_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_33 /= No_Token_Index then

   Transform_Res_27 := Allocate_Comp_Op_Kind_Eq (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_27,
      Kind => Turkixir_Comp_Op_Kind_Eq,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_30,
      Token_End_Index   => (if Row_Pos_33 = Row_Pos_30
                            then No_Token_Index
                            else Row_Pos_33 - 1));




end if;

--  End transform_code

    if Row_Pos_33 /= No_Token_Index then
        Or_Pos_17 := Row_Pos_33;
        Or_Res_17 := Transform_Res_27;
        goto Exit_Or_18;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_34 := Row_Pos_30;



--  Start tok_code

Token_Res_42 := Row_Pos_34;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_42));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Gte)
   then
       Token_Pos_42 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_34 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_34,
             Expected_Token_Id => Turkixir_T_T__Gte,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_42 := Row_Pos_34 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_42 /= No_Token_Index then

   Row_Pos_34 := Token_Pos_42;

else
   Row_Pos_34 := No_Token_Index;
   goto Exit_Row_29_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_29_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_34 /= No_Token_Index then

   Transform_Res_28 := Allocate_Comp_Op_Kind_Gte (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_28,
      Kind => Turkixir_Comp_Op_Kind_Gte,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_30,
      Token_End_Index   => (if Row_Pos_34 = Row_Pos_30
                            then No_Token_Index
                            else Row_Pos_34 - 1));




end if;

--  End transform_code

    if Row_Pos_34 /= No_Token_Index then
        Or_Pos_17 := Row_Pos_34;
        Or_Res_17 := Transform_Res_28;
        goto Exit_Or_18;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_35 := Row_Pos_30;



--  Start tok_code

Token_Res_43 := Row_Pos_35;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_43));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Lte)
   then
       Token_Pos_43 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_35 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_35,
             Expected_Token_Id => Turkixir_T_T__Lte,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_43 := Row_Pos_35 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_43 /= No_Token_Index then

   Row_Pos_35 := Token_Pos_43;

else
   Row_Pos_35 := No_Token_Index;
   goto Exit_Row_30_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_30_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_35 /= No_Token_Index then

   Transform_Res_29 := Allocate_Comp_Op_Kind_Lte (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_29,
      Kind => Turkixir_Comp_Op_Kind_Lte,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_30,
      Token_End_Index   => (if Row_Pos_35 = Row_Pos_30
                            then No_Token_Index
                            else Row_Pos_35 - 1));




end if;

--  End transform_code

    if Row_Pos_35 /= No_Token_Index then
        Or_Pos_17 := Row_Pos_35;
        Or_Res_17 := Transform_Res_29;
        goto Exit_Or_18;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_36 := Row_Pos_30;



--  Start tok_code

Token_Res_44 := Row_Pos_36;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_44));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Diamond)
   then
       Token_Pos_44 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_36 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_36,
             Expected_Token_Id => Turkixir_T_T__Diamond,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_44 := Row_Pos_36 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_44 /= No_Token_Index then

   Row_Pos_36 := Token_Pos_44;

else
   Row_Pos_36 := No_Token_Index;
   goto Exit_Row_31_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_31_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_36 /= No_Token_Index then

   Transform_Res_30 := Allocate_Comp_Op_Kind_Diamond (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_30,
      Kind => Turkixir_Comp_Op_Kind_Diamond,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_30,
      Token_End_Index   => (if Row_Pos_36 = Row_Pos_30
                            then No_Token_Index
                            else Row_Pos_36 - 1));




end if;

--  End transform_code

    if Row_Pos_36 /= No_Token_Index then
        Or_Pos_17 := Row_Pos_36;
        Or_Res_17 := Transform_Res_30;
        goto Exit_Or_18;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_37 := Row_Pos_30;



--  Start tok_code

Token_Res_45 := Row_Pos_37;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_45));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Notequal)
   then
       Token_Pos_45 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_37 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_37,
             Expected_Token_Id => Turkixir_T_T__Notequal,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_45 := Row_Pos_37 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_45 /= No_Token_Index then

   Row_Pos_37 := Token_Pos_45;

else
   Row_Pos_37 := No_Token_Index;
   goto Exit_Row_32_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_32_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_37 /= No_Token_Index then

   Transform_Res_31 := Allocate_Comp_Op_Kind_Noteq (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_31,
      Kind => Turkixir_Comp_Op_Kind_Noteq,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_30,
      Token_End_Index   => (if Row_Pos_37 = Row_Pos_30
                            then No_Token_Index
                            else Row_Pos_37 - 1));




end if;

--  End transform_code

    if Row_Pos_37 /= No_Token_Index then
        Or_Pos_17 := Row_Pos_37;
        Or_Res_17 := Transform_Res_31;
        goto Exit_Or_18;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_38 := Row_Pos_30;



--  Start tok_code

Token_Res_46 := Row_Pos_38;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_46));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__In)
   then
       Token_Pos_46 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_38 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_38,
             Expected_Token_Id => Turkixir_T_T__In,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_46 := Row_Pos_38 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_46 /= No_Token_Index then

   Row_Pos_38 := Token_Pos_46;

else
   Row_Pos_38 := No_Token_Index;
   goto Exit_Row_33_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_33_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_38 /= No_Token_Index then

   Transform_Res_32 := Allocate_Comp_Op_Kind_In (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_32,
      Kind => Turkixir_Comp_Op_Kind_In,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_30,
      Token_End_Index   => (if Row_Pos_38 = Row_Pos_30
                            then No_Token_Index
                            else Row_Pos_38 - 1));




end if;

--  End transform_code

    if Row_Pos_38 /= No_Token_Index then
        Or_Pos_17 := Row_Pos_38;
        Or_Res_17 := Transform_Res_32;
        goto Exit_Or_18;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_39 := Row_Pos_30;



--  Start tok_code

Token_Res_47 := Row_Pos_39;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_47));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Not)
   then
       Token_Pos_47 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_39 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_39,
             Expected_Token_Id => Turkixir_T_T__Not,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_47 := Row_Pos_39 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_47 /= No_Token_Index then

   Row_Pos_39 := Token_Pos_47;

else
   Row_Pos_39 := No_Token_Index;
   goto Exit_Row_34_0;

end if;


--  Start tok_code

Token_Res_48 := Row_Pos_39;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_48));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__In)
   then
       Token_Pos_48 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_39 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_39,
             Expected_Token_Id => Turkixir_T_T__In,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_48 := Row_Pos_39 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_48 /= No_Token_Index then

   Row_Pos_39 := Token_Pos_48;

else
   Row_Pos_39 := No_Token_Index;
   goto Exit_Row_34_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_34_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_39 /= No_Token_Index then

   Transform_Res_33 := Allocate_Comp_Op_Kind_Notin (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_33,
      Kind => Turkixir_Comp_Op_Kind_Notin,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_30,
      Token_End_Index   => (if Row_Pos_39 = Row_Pos_30
                            then No_Token_Index
                            else Row_Pos_39 - 1));




end if;

--  End transform_code

    if Row_Pos_39 /= No_Token_Index then
        Or_Pos_17 := Row_Pos_39;
        Or_Res_17 := Transform_Res_33;
        goto Exit_Or_18;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_40 := Row_Pos_30;



--  Start tok_code

Token_Res_49 := Row_Pos_40;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_49));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Is)
   then
       Token_Pos_49 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_40 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_40,
             Expected_Token_Id => Turkixir_T_T__Is,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_49 := Row_Pos_40 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_49 /= No_Token_Index then

   Row_Pos_40 := Token_Pos_49;

else
   Row_Pos_40 := No_Token_Index;
   goto Exit_Row_35_0;

end if;


--  Start tok_code

Token_Res_50 := Row_Pos_40;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_50));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Not)
   then
       Token_Pos_50 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_40 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_40,
             Expected_Token_Id => Turkixir_T_T__Not,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_50 := Row_Pos_40 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_50 /= No_Token_Index then

   Row_Pos_40 := Token_Pos_50;

else
   Row_Pos_40 := No_Token_Index;
   goto Exit_Row_35_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_35_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_40 /= No_Token_Index then

   Transform_Res_34 := Allocate_Comp_Op_Kind_Isnot (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_34,
      Kind => Turkixir_Comp_Op_Kind_Isnot,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_30,
      Token_End_Index   => (if Row_Pos_40 = Row_Pos_30
                            then No_Token_Index
                            else Row_Pos_40 - 1));




end if;

--  End transform_code

    if Row_Pos_40 /= No_Token_Index then
        Or_Pos_17 := Row_Pos_40;
        Or_Res_17 := Transform_Res_34;
        goto Exit_Or_18;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_41 := Row_Pos_30;



--  Start tok_code

Token_Res_51 := Row_Pos_41;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_51));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Is)
   then
       Token_Pos_51 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_41 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_41,
             Expected_Token_Id => Turkixir_T_T__Is,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_51 := Row_Pos_41 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_51 /= No_Token_Index then

   Row_Pos_41 := Token_Pos_51;

else
   Row_Pos_41 := No_Token_Index;
   goto Exit_Row_36_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_36_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_41 /= No_Token_Index then

   Transform_Res_35 := Allocate_Comp_Op_Kind_Is (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_35,
      Kind => Turkixir_Comp_Op_Kind_Is,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_30,
      Token_End_Index   => (if Row_Pos_41 = Row_Pos_30
                            then No_Token_Index
                            else Row_Pos_41 - 1));




end if;

--  End transform_code

    if Row_Pos_41 /= No_Token_Index then
        Or_Pos_17 := Row_Pos_41;
        Or_Res_17 := Transform_Res_35;
        goto Exit_Or_18;
    end if;
<<Exit_Or_18>>

--  End or_code



if Or_Pos_17 /= No_Token_Index then

   Row_Pos_30 := Or_Pos_17;

else
   Row_Pos_30 := No_Token_Index;
   goto Exit_Row_25_0;

end if;


Defer_Res_51 :=
   Expr_Or_Parse_0 (Parser, Row_Pos_30);
Defer_Pos_51 := Parser.Current_Pos;



if Defer_Pos_51 /= No_Token_Index then

   Row_Pos_30 := Defer_Pos_51;

else
   Row_Pos_30 := No_Token_Index;
   goto Exit_Row_25_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_25_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_30 /= No_Token_Index then

   Transform_Res_36 := Allocate_Comp_Op (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_36,
      Kind => Turkixir_Comp_Op,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_30 = Pos
                            then No_Token_Index
                            else Row_Pos_30 - 1));

      Initialize_Fields_For_Comp_Op
        (Self => Transform_Res_36, Comp_Op_F_Left => Defer_Res_50, Comp_Op_F_Op => Or_Res_17, Comp_Op_F_Right => Defer_Res_51);

         if Defer_Res_50 /= null and then Is_Incomplete (Defer_Res_50) then
            Transform_Res_36.Last_Attempted_Child := 0;
         elsif Defer_Res_50 /= null and then not Is_Ghost (Defer_Res_50) then
            Transform_Res_36.Last_Attempted_Child := -1;
         end if;
         if Or_Res_17 /= null and then Is_Incomplete (Or_Res_17) then
            Transform_Res_36.Last_Attempted_Child := 0;
         elsif Or_Res_17 /= null and then not Is_Ghost (Or_Res_17) then
            Transform_Res_36.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_51 /= null and then Is_Incomplete (Defer_Res_51) then
            Transform_Res_36.Last_Attempted_Child := 0;
         elsif Defer_Res_51 /= null and then not Is_Ghost (Defer_Res_51) then
            Transform_Res_36.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_30 /= No_Token_Index then
        Or_Pos_18 := Row_Pos_30;
        Or_Res_18 := Transform_Res_36;
        goto Exit_Or_17;
    end if;
    
Defer_Res_52 :=
   Expr_Or_Parse_0 (Parser, Pos);
Defer_Pos_52 := Parser.Current_Pos;

    if Defer_Pos_52 /= No_Token_Index then
        Or_Pos_18 := Defer_Pos_52;
        Or_Res_18 := Defer_Res_52;
        goto Exit_Or_17;
    end if;
<<Exit_Or_17>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

      if Or_Pos_18 > Mem_Pos then
         Mem_Pos := Or_Pos_18;
         Mem_Res := Or_Res_18;
         Set
           (Parser.Private_Part.Comparison_Or_Parse_1_Memo,
            Or_Pos_18 /= No_Token_Index,
            Or_Res_18,
            Pos,
            Or_Pos_18);
         goto Try_Again;

      elsif Mem_Pos > Pos then
         Or_Res_18 := Mem_Res;
         Or_Pos_18 := Mem_Pos;
         goto No_Memo;
      end if;

   Set
     (Parser.Private_Part.Comparison_Or_Parse_1_Memo,
      Or_Pos_18 /= No_Token_Index,
      Or_Res_18,
      Pos,
      Or_Pos_18);

       <<No_Memo>>

   Parser.Current_Pos := Or_Pos_18;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_18;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Comparison_Or_Parse_1;

   


function Not_Test_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_29 :
            Token_Index
               := No_Token_Index;
      Token_Pos_38 :
            Token_Index
               := No_Token_Index;
      Token_Res_38 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_48 :
            Token_Index
               := No_Token_Index;
      Defer_Res_48 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_24 :
            Bare_Not_Op
               := No_Bare_Turkixir_Node;
      Defer_Pos_49 :
            Token_Index
               := No_Token_Index;
      Defer_Res_49 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_16 :
            Token_Index
               := No_Token_Index;
      Or_Res_16 :
            Bare_Expr
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Not_Test_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_16 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_16;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_16;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_16 := No_Token_Index;
Or_Res_16 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_29 := Pos;



--  Start tok_code

Token_Res_38 := Row_Pos_29;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_38));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Not)
   then
       Token_Pos_38 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_29 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_29,
             Expected_Token_Id => Turkixir_T_T__Not,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_38 := Row_Pos_29 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_38 /= No_Token_Index then

   Row_Pos_29 := Token_Pos_38;

else
   Row_Pos_29 := No_Token_Index;
   goto Exit_Row_24_0;

end if;


Defer_Res_48 :=
   Not_Test_Or_Parse_0 (Parser, Row_Pos_29);
Defer_Pos_48 := Parser.Current_Pos;



if Defer_Pos_48 /= No_Token_Index then

   Row_Pos_29 := Defer_Pos_48;

else
   Row_Pos_29 := No_Token_Index;
   goto Exit_Row_24_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_24_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_29 /= No_Token_Index then

   Transform_Res_24 := Allocate_Not_Op (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_24,
      Kind => Turkixir_Not_Op,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_29 = Pos
                            then No_Token_Index
                            else Row_Pos_29 - 1));

      Initialize_Fields_For_Not_Op
        (Self => Transform_Res_24, Not_Op_F_Expr => Defer_Res_48);

         if Defer_Res_48 /= null and then Is_Incomplete (Defer_Res_48) then
            Transform_Res_24.Last_Attempted_Child := 0;
         elsif Defer_Res_48 /= null and then not Is_Ghost (Defer_Res_48) then
            Transform_Res_24.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_29 /= No_Token_Index then
        Or_Pos_16 := Row_Pos_29;
        Or_Res_16 := Transform_Res_24;
        goto Exit_Or_16;
    end if;
    
Defer_Res_49 :=
   Comparison_Or_Parse_1 (Parser, Pos);
Defer_Pos_49 := Parser.Current_Pos;

    if Defer_Pos_49 /= No_Token_Index then
        Or_Pos_16 := Defer_Pos_49;
        Or_Res_16 := Defer_Res_49;
        goto Exit_Or_16;
    end if;
<<Exit_Or_16>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Not_Test_Or_Parse_0_Memo,
      Or_Pos_16 /= No_Token_Index,
      Or_Res_16,
      Pos,
      Or_Pos_16);


   Parser.Current_Pos := Or_Pos_16;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_16;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Not_Test_Or_Parse_0;

   


function And_Test_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_28 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_45 :
            Token_Index
               := No_Token_Index;
      Defer_Res_45 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_37 :
            Token_Index
               := No_Token_Index;
      Token_Res_37 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_46 :
            Token_Index
               := No_Token_Index;
      Defer_Res_46 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_23 :
            Bare_And_Op
               := No_Bare_Turkixir_Node;
      Defer_Pos_47 :
            Token_Index
               := No_Token_Index;
      Defer_Res_47 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_15 :
            Token_Index
               := No_Token_Index;
      Or_Res_15 :
            Bare_Expr
               := No_Bare_Turkixir_Node;

      Mem_Pos : Token_Index := Pos;
      Mem_Res : Bare_Expr := No_Bare_Turkixir_Node;

   M : Memo_Entry := Get (Parser.Private_Part.And_Test_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_15 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_15;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_15;
   end if;

       Set (Parser.Private_Part.And_Test_Or_Parse_0_Memo, False, Or_Res_15, Pos, Mem_Pos);

       <<Try_Again>>



   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_15 := No_Token_Index;
Or_Res_15 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_28 := Pos;



Defer_Res_45 :=
   And_Test_Or_Parse_0 (Parser, Row_Pos_28);
Defer_Pos_45 := Parser.Current_Pos;



if Defer_Pos_45 /= No_Token_Index then

   Row_Pos_28 := Defer_Pos_45;

else
   Row_Pos_28 := No_Token_Index;
   goto Exit_Row_23_0;

end if;


--  Start tok_code

Token_Res_37 := Row_Pos_28;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_37));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__And)
   then
       Token_Pos_37 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_28 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_28,
             Expected_Token_Id => Turkixir_T_T__And,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_37 := Row_Pos_28 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_37 /= No_Token_Index then

   Row_Pos_28 := Token_Pos_37;

else
   Row_Pos_28 := No_Token_Index;
   goto Exit_Row_23_0;

end if;


Defer_Res_46 :=
   Not_Test_Or_Parse_0 (Parser, Row_Pos_28);
Defer_Pos_46 := Parser.Current_Pos;



if Defer_Pos_46 /= No_Token_Index then

   Row_Pos_28 := Defer_Pos_46;

else
   Row_Pos_28 := No_Token_Index;
   goto Exit_Row_23_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_23_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_28 /= No_Token_Index then

   Transform_Res_23 := Allocate_And_Op (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_23,
      Kind => Turkixir_And_Op,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_28 = Pos
                            then No_Token_Index
                            else Row_Pos_28 - 1));

      Initialize_Fields_For_And_Op
        (Self => Transform_Res_23, And_Op_F_Left => Defer_Res_45, And_Op_F_Right => Defer_Res_46);

         if Defer_Res_45 /= null and then Is_Incomplete (Defer_Res_45) then
            Transform_Res_23.Last_Attempted_Child := 0;
         elsif Defer_Res_45 /= null and then not Is_Ghost (Defer_Res_45) then
            Transform_Res_23.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_46 /= null and then Is_Incomplete (Defer_Res_46) then
            Transform_Res_23.Last_Attempted_Child := 0;
         elsif Defer_Res_46 /= null and then not Is_Ghost (Defer_Res_46) then
            Transform_Res_23.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_28 /= No_Token_Index then
        Or_Pos_15 := Row_Pos_28;
        Or_Res_15 := Transform_Res_23;
        goto Exit_Or_15;
    end if;
    
Defer_Res_47 :=
   Not_Test_Or_Parse_0 (Parser, Pos);
Defer_Pos_47 := Parser.Current_Pos;

    if Defer_Pos_47 /= No_Token_Index then
        Or_Pos_15 := Defer_Pos_47;
        Or_Res_15 := Defer_Res_47;
        goto Exit_Or_15;
    end if;
<<Exit_Or_15>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

      if Or_Pos_15 > Mem_Pos then
         Mem_Pos := Or_Pos_15;
         Mem_Res := Or_Res_15;
         Set
           (Parser.Private_Part.And_Test_Or_Parse_0_Memo,
            Or_Pos_15 /= No_Token_Index,
            Or_Res_15,
            Pos,
            Or_Pos_15);
         goto Try_Again;

      elsif Mem_Pos > Pos then
         Or_Res_15 := Mem_Res;
         Or_Pos_15 := Mem_Pos;
         goto No_Memo;
      end if;

   Set
     (Parser.Private_Part.And_Test_Or_Parse_0_Memo,
      Or_Pos_15 /= No_Token_Index,
      Or_Res_15,
      Pos,
      Or_Pos_15);

       <<No_Memo>>

   Parser.Current_Pos := Or_Pos_15;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_15;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end And_Test_Or_Parse_0;

   


function Or_Test_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_27 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_42 :
            Token_Index
               := No_Token_Index;
      Defer_Res_42 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_36 :
            Token_Index
               := No_Token_Index;
      Token_Res_36 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_43 :
            Token_Index
               := No_Token_Index;
      Defer_Res_43 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_22 :
            Bare_Or_Op
               := No_Bare_Turkixir_Node;
      Defer_Pos_44 :
            Token_Index
               := No_Token_Index;
      Defer_Res_44 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_14 :
            Token_Index
               := No_Token_Index;
      Or_Res_14 :
            Bare_Expr
               := No_Bare_Turkixir_Node;

      Mem_Pos : Token_Index := Pos;
      Mem_Res : Bare_Expr := No_Bare_Turkixir_Node;

   M : Memo_Entry := Get (Parser.Private_Part.Or_Test_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_14 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_14;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_14;
   end if;

       Set (Parser.Private_Part.Or_Test_Or_Parse_0_Memo, False, Or_Res_14, Pos, Mem_Pos);

       <<Try_Again>>



   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_14 := No_Token_Index;
Or_Res_14 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_27 := Pos;



Defer_Res_42 :=
   Or_Test_Or_Parse_0 (Parser, Row_Pos_27);
Defer_Pos_42 := Parser.Current_Pos;



if Defer_Pos_42 /= No_Token_Index then

   Row_Pos_27 := Defer_Pos_42;

else
   Row_Pos_27 := No_Token_Index;
   goto Exit_Row_22_0;

end if;


--  Start tok_code

Token_Res_36 := Row_Pos_27;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_36));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Or)
   then
       Token_Pos_36 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_27 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_27,
             Expected_Token_Id => Turkixir_T_T__Or,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_36 := Row_Pos_27 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_36 /= No_Token_Index then

   Row_Pos_27 := Token_Pos_36;

else
   Row_Pos_27 := No_Token_Index;
   goto Exit_Row_22_0;

end if;


Defer_Res_43 :=
   And_Test_Or_Parse_0 (Parser, Row_Pos_27);
Defer_Pos_43 := Parser.Current_Pos;



if Defer_Pos_43 /= No_Token_Index then

   Row_Pos_27 := Defer_Pos_43;

else
   Row_Pos_27 := No_Token_Index;
   goto Exit_Row_22_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_22_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_27 /= No_Token_Index then

   Transform_Res_22 := Allocate_Or_Op (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_22,
      Kind => Turkixir_Or_Op,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_27 = Pos
                            then No_Token_Index
                            else Row_Pos_27 - 1));

      Initialize_Fields_For_Or_Op
        (Self => Transform_Res_22, Or_Op_F_Left => Defer_Res_42, Or_Op_F_Right => Defer_Res_43);

         if Defer_Res_42 /= null and then Is_Incomplete (Defer_Res_42) then
            Transform_Res_22.Last_Attempted_Child := 0;
         elsif Defer_Res_42 /= null and then not Is_Ghost (Defer_Res_42) then
            Transform_Res_22.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_43 /= null and then Is_Incomplete (Defer_Res_43) then
            Transform_Res_22.Last_Attempted_Child := 0;
         elsif Defer_Res_43 /= null and then not Is_Ghost (Defer_Res_43) then
            Transform_Res_22.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_27 /= No_Token_Index then
        Or_Pos_14 := Row_Pos_27;
        Or_Res_14 := Transform_Res_22;
        goto Exit_Or_14;
    end if;
    
Defer_Res_44 :=
   And_Test_Or_Parse_0 (Parser, Pos);
Defer_Pos_44 := Parser.Current_Pos;

    if Defer_Pos_44 /= No_Token_Index then
        Or_Pos_14 := Defer_Pos_44;
        Or_Res_14 := Defer_Res_44;
        goto Exit_Or_14;
    end if;
<<Exit_Or_14>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

      if Or_Pos_14 > Mem_Pos then
         Mem_Pos := Or_Pos_14;
         Mem_Res := Or_Res_14;
         Set
           (Parser.Private_Part.Or_Test_Or_Parse_0_Memo,
            Or_Pos_14 /= No_Token_Index,
            Or_Res_14,
            Pos,
            Or_Pos_14);
         goto Try_Again;

      elsif Mem_Pos > Pos then
         Or_Res_14 := Mem_Res;
         Or_Pos_14 := Mem_Pos;
         goto No_Memo;
      end if;

   Set
     (Parser.Private_Part.Or_Test_Or_Parse_0_Memo,
      Or_Pos_14 /= No_Token_Index,
      Or_Res_14,
      Pos,
      Or_Pos_14);

       <<No_Memo>>

   Parser.Current_Pos := Or_Pos_14;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_14;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Or_Test_Or_Parse_0;

   


function Test_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Defer_Pos_30 :
            Token_Index
               := No_Token_Index;
      Defer_Res_30 :
            Bare_Lambda_Def
               := No_Bare_Turkixir_Node;
      Row_Pos_20 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_31 :
            Token_Index
               := No_Token_Index;
      Defer_Res_31 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_24 :
            Token_Index
               := No_Token_Index;
      Token_Res_24 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_32 :
            Token_Index
               := No_Token_Index;
      Defer_Res_32 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_25 :
            Token_Index
               := No_Token_Index;
      Token_Res_25 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_33 :
            Token_Index
               := No_Token_Index;
      Defer_Res_33 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_18 :
            Bare_If_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_34 :
            Token_Index
               := No_Token_Index;
      Defer_Res_34 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_12 :
            Token_Index
               := No_Token_Index;
      Or_Res_12 :
            Bare_Expr
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Test_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_12 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_12;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_12;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_12 := No_Token_Index;
Or_Res_12 := No_Bare_Turkixir_Node;
    
Defer_Res_30 :=
   Lambdef_Transform_Parse_0 (Parser, Pos);
Defer_Pos_30 := Parser.Current_Pos;

    if Defer_Pos_30 /= No_Token_Index then
        Or_Pos_12 := Defer_Pos_30;
        Or_Res_12 := Defer_Res_30;
        goto Exit_Or_12;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_20 := Pos;



Defer_Res_31 :=
   Or_Test_Or_Parse_0 (Parser, Row_Pos_20);
Defer_Pos_31 := Parser.Current_Pos;



if Defer_Pos_31 /= No_Token_Index then

   Row_Pos_20 := Defer_Pos_31;

else
   Row_Pos_20 := No_Token_Index;
   goto Exit_Row_21_0;

end if;


--  Start tok_code

Token_Res_24 := Row_Pos_20;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_24));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__If)
   then
       Token_Pos_24 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_20 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_20,
             Expected_Token_Id => Turkixir_T_T__If,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_24 := Row_Pos_20 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_24 /= No_Token_Index then

   Row_Pos_20 := Token_Pos_24;

else
   Row_Pos_20 := No_Token_Index;
   goto Exit_Row_21_0;

end if;


Defer_Res_32 :=
   Or_Test_Or_Parse_0 (Parser, Row_Pos_20);
Defer_Pos_32 := Parser.Current_Pos;



if Defer_Pos_32 /= No_Token_Index then

   Row_Pos_20 := Defer_Pos_32;

else
   Row_Pos_20 := No_Token_Index;
   goto Exit_Row_21_0;

end if;


--  Start tok_code

Token_Res_25 := Row_Pos_20;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_25));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Else)
   then
       Token_Pos_25 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_20 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_20,
             Expected_Token_Id => Turkixir_T_T__Else,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_25 := Row_Pos_20 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_25 /= No_Token_Index then

   Row_Pos_20 := Token_Pos_25;

else
   Row_Pos_20 := No_Token_Index;
   goto Exit_Row_21_0;

end if;


Defer_Res_33 :=
   Test_Or_Parse_0 (Parser, Row_Pos_20);
Defer_Pos_33 := Parser.Current_Pos;



if Defer_Pos_33 /= No_Token_Index then

   Row_Pos_20 := Defer_Pos_33;

else
   Row_Pos_20 := No_Token_Index;
   goto Exit_Row_21_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_21_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_20 /= No_Token_Index then

   Transform_Res_18 := Allocate_If_Expr (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_18,
      Kind => Turkixir_If_Expr,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_20 = Pos
                            then No_Token_Index
                            else Row_Pos_20 - 1));

      Initialize_Fields_For_If_Expr
        (Self => Transform_Res_18, If_Expr_F_Expr => Defer_Res_31, If_Expr_F_Cond => Defer_Res_32, If_Expr_F_Else_Expr => Defer_Res_33);

         if Defer_Res_31 /= null and then Is_Incomplete (Defer_Res_31) then
            Transform_Res_18.Last_Attempted_Child := 0;
         elsif Defer_Res_31 /= null and then not Is_Ghost (Defer_Res_31) then
            Transform_Res_18.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_32 /= null and then Is_Incomplete (Defer_Res_32) then
            Transform_Res_18.Last_Attempted_Child := 0;
         elsif Defer_Res_32 /= null and then not Is_Ghost (Defer_Res_32) then
            Transform_Res_18.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_33 /= null and then Is_Incomplete (Defer_Res_33) then
            Transform_Res_18.Last_Attempted_Child := 0;
         elsif Defer_Res_33 /= null and then not Is_Ghost (Defer_Res_33) then
            Transform_Res_18.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_20 /= No_Token_Index then
        Or_Pos_12 := Row_Pos_20;
        Or_Res_12 := Transform_Res_18;
        goto Exit_Or_12;
    end if;
    
Defer_Res_34 :=
   Or_Test_Or_Parse_0 (Parser, Pos);
Defer_Pos_34 := Parser.Current_Pos;

    if Defer_Pos_34 /= No_Token_Index then
        Or_Pos_12 := Defer_Pos_34;
        Or_Res_12 := Defer_Res_34;
        goto Exit_Or_12;
    end if;
<<Exit_Or_12>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Test_Or_Parse_0_Memo,
      Or_Pos_12 /= No_Token_Index,
      Or_Res_12,
      Pos,
      Or_Pos_12);


   Parser.Current_Pos := Or_Pos_12;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_12;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Test_Or_Parse_0;

   


function Expr_List_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr_List
is
   use Bare_Expr_List_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_45 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_3 :
            Token_Index
               := No_Token_Index;
      Tmp_List_3 :
            Free_Parse_List;
      Defer_Pos_62 :
            Token_Index
               := No_Token_Index;
      Defer_Res_62 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_56 :
            Token_Index
               := No_Token_Index;
      Token_Res_56 :
            Token_Index
               := No_Token_Index;
      List_Pos_3 :
            Token_Index
               := No_Token_Index;
      List_Res_3 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Token_Pos_57 :
            Token_Index
               := No_Token_Index;
      Token_Res_57 :
            Token_Index
               := No_Token_Index;


   M : Memo_Entry := Get (Parser.Private_Part.Expr_List_Extract_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res_3 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return List_Res_3;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return List_Res_3;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start row_code

Row_Pos_45 := Pos;



--  Start list_code

    List_Pos_3 := No_Token_Index;



Lst_Cpos_3 := Row_Pos_45;
Tmp_List_3 := Get_Parse_List (Parser);

loop
   
Defer_Res_62 :=
   Expr_Or_Parse_0 (Parser, Lst_Cpos_3);
Defer_Pos_62 := Parser.Current_Pos;


   exit when Defer_Pos_62 = No_Token_Index;

   List_Pos_3 := Defer_Pos_62;
   Lst_Cpos_3 := List_Pos_3;

   Tmp_List_3.Nodes.Append (Defer_Res_62);

      
--  Start tok_code

Token_Res_56 := Lst_Cpos_3;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_56));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_56 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos_3 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos_3,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_56 := Lst_Cpos_3 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos_56 /= No_Token_Index then
          Lst_Cpos_3 := Token_Pos_56;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_3.Nodes.Length;
begin
   List_Res_3 :=
      Allocate_Expr_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_45;
      Token_End := (if Lst_Cpos_3 = Row_Pos_45
                    then Row_Pos_45
                    else Lst_Cpos_3 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_45, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_3,
      Kind              => Turkixir_Expr_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_3,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_3.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_3.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_3);

--  End list_code



if List_Pos_3 /= No_Token_Index then

   Row_Pos_45 := List_Pos_3;

else
   Row_Pos_45 := No_Token_Index;
   goto Exit_Row_40_0;

end if;


--  Start opt_code




--  Start tok_code

Token_Res_57 := Row_Pos_45;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_57));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_57 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_45 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_45,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_57 := Row_Pos_45 + 1;
   end if;
end;

--  End tok_code


if Token_Pos_57 = No_Token_Index then

        Token_Res_57 := No_Token_Index;


    Token_Pos_57 := Row_Pos_45;


end if;

--  End opt_code



if Token_Pos_57 /= No_Token_Index then

   Row_Pos_45 := Token_Pos_57;

else
   Row_Pos_45 := No_Token_Index;
   goto Exit_Row_40_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_40_0>>
pragma Warnings (On, "referenced");

--  End row_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Expr_List_Extract_Parse_0_Memo,
      Row_Pos_45 /= No_Token_Index,
      List_Res_3,
      Pos,
      Row_Pos_45);


   Parser.Current_Pos := Row_Pos_45;

   Exit_Call (Parser, Call_Depth);
   return List_Res_3;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Expr_List_Extract_Parse_0;

   


function Comp_If_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Comp_If
is
   use Bare_Comp_If_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_46 :
            Token_Index
               := No_Token_Index;
      Token_Pos_58 :
            Token_Index
               := No_Token_Index;
      Token_Res_58 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_65 :
            Token_Index
               := No_Token_Index;
      Defer_Res_65 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_66 :
            Token_Index
               := No_Token_Index;
      Defer_Res_66 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Transform_Res_40 :
            Bare_Comp_If
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Comp_If_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_40 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_40;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_40;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_46 := Pos;



--  Start tok_code

Token_Res_58 := Row_Pos_46;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_58));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__If)
   then
       Token_Pos_58 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_46 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_46,
             Expected_Token_Id => Turkixir_T_T__If,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_58 := Row_Pos_46 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_58 /= No_Token_Index then

   Row_Pos_46 := Token_Pos_58;

else
   Row_Pos_46 := No_Token_Index;
   goto Exit_Row_41_0;

end if;


Defer_Res_65 :=
   Test_Or_Parse_0 (Parser, Row_Pos_46);
Defer_Pos_65 := Parser.Current_Pos;



if Defer_Pos_65 /= No_Token_Index then

   Row_Pos_46 := Defer_Pos_65;

else
   Row_Pos_46 := No_Token_Index;
   goto Exit_Row_41_0;

end if;


--  Start opt_code




Defer_Res_66 :=
   Comp_Iter_Or_Parse_0 (Parser, Row_Pos_46);
Defer_Pos_66 := Parser.Current_Pos;


if Defer_Pos_66 = No_Token_Index then

        Defer_Res_66 := No_Bare_Turkixir_Node;


    Defer_Pos_66 := Row_Pos_46;


end if;

--  End opt_code



if Defer_Pos_66 /= No_Token_Index then

   Row_Pos_46 := Defer_Pos_66;

else
   Row_Pos_46 := No_Token_Index;
   goto Exit_Row_41_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_41_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_46 /= No_Token_Index then

   Transform_Res_40 := Allocate_Comp_If (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_40,
      Kind => Turkixir_Comp_If,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_46 = Pos
                            then No_Token_Index
                            else Row_Pos_46 - 1));

      Initialize_Fields_For_Comp_If
        (Self => Transform_Res_40, Comp_If_F_Test => Defer_Res_65, Comp_If_F_Comp => Defer_Res_66);

         if Defer_Res_65 /= null and then Is_Incomplete (Defer_Res_65) then
            Transform_Res_40.Last_Attempted_Child := 0;
         elsif Defer_Res_65 /= null and then not Is_Ghost (Defer_Res_65) then
            Transform_Res_40.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_66 /= null and then Is_Incomplete (Defer_Res_66) then
            Transform_Res_40.Last_Attempted_Child := 0;
         elsif Defer_Res_66 /= null and then not Is_Ghost (Defer_Res_66) then
            Transform_Res_40.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Comp_If_Transform_Parse_0_Memo,
      Row_Pos_46 /= No_Token_Index,
      Transform_Res_40,
      Pos,
      Row_Pos_46);


   Parser.Current_Pos := Row_Pos_46;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_40;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Comp_If_Transform_Parse_0;

   


function Comp_Iter_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node
is
   use Bare_Turkixir_Node_Memos;

   Call_Depth : aliased Natural;

      Defer_Pos_63 :
            Token_Index
               := No_Token_Index;
      Defer_Res_63 :
            Bare_Comp_For
               := No_Bare_Turkixir_Node;
      Defer_Pos_64 :
            Token_Index
               := No_Token_Index;
      Defer_Res_64 :
            Bare_Comp_If
               := No_Bare_Turkixir_Node;
      Or_Pos_21 :
            Token_Index
               := No_Token_Index;
      Or_Res_21 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Comp_Iter_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_21 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_21;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_21;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_21 := No_Token_Index;
Or_Res_21 := No_Bare_Turkixir_Node;
    
Defer_Res_63 :=
   Comp_For_Transform_Parse_0 (Parser, Pos);
Defer_Pos_63 := Parser.Current_Pos;

    if Defer_Pos_63 /= No_Token_Index then
        Or_Pos_21 := Defer_Pos_63;
        Or_Res_21 := Defer_Res_63;
        goto Exit_Or_21;
    end if;
    
Defer_Res_64 :=
   Comp_If_Transform_Parse_0 (Parser, Pos);
Defer_Pos_64 := Parser.Current_Pos;

    if Defer_Pos_64 /= No_Token_Index then
        Or_Pos_21 := Defer_Pos_64;
        Or_Res_21 := Defer_Res_64;
        goto Exit_Or_21;
    end if;
<<Exit_Or_21>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Comp_Iter_Or_Parse_0_Memo,
      Or_Pos_21 /= No_Token_Index,
      Or_Res_21,
      Pos,
      Or_Pos_21);


   Parser.Current_Pos := Or_Pos_21;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_21;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Comp_Iter_Or_Parse_0;

   


function Comp_For_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Comp_For
is
   use Bare_Comp_For_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_44 :
            Token_Index
               := No_Token_Index;
      Token_Pos_54 :
            Token_Index
               := No_Token_Index;
      Token_Res_54 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_59 :
            Token_Index
               := No_Token_Index;
      Defer_Res_59 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Token_Pos_55 :
            Token_Index
               := No_Token_Index;
      Token_Res_55 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_60 :
            Token_Index
               := No_Token_Index;
      Defer_Res_60 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_61 :
            Token_Index
               := No_Token_Index;
      Defer_Res_61 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Transform_Res_39 :
            Bare_Comp_For
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Comp_For_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_39 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_39;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_39;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_44 := Pos;



--  Start tok_code

Token_Res_54 := Row_Pos_44;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_54));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__For)
   then
       Token_Pos_54 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_44 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_44,
             Expected_Token_Id => Turkixir_T_T__For,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_54 := Row_Pos_44 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_54 /= No_Token_Index then

   Row_Pos_44 := Token_Pos_54;

else
   Row_Pos_44 := No_Token_Index;
   goto Exit_Row_39_0;

end if;


Defer_Res_59 :=
   Expr_List_Extract_Parse_0 (Parser, Row_Pos_44);
Defer_Pos_59 := Parser.Current_Pos;



if Defer_Pos_59 /= No_Token_Index then

   Row_Pos_44 := Defer_Pos_59;

else
   Row_Pos_44 := No_Token_Index;
   goto Exit_Row_39_0;

end if;


--  Start tok_code

Token_Res_55 := Row_Pos_44;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_55));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__In)
   then
       Token_Pos_55 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_44 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_44,
             Expected_Token_Id => Turkixir_T_T__In,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_55 := Row_Pos_44 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_55 /= No_Token_Index then

   Row_Pos_44 := Token_Pos_55;

else
   Row_Pos_44 := No_Token_Index;
   goto Exit_Row_39_0;

end if;


Defer_Res_60 :=
   Or_Test_Or_Parse_0 (Parser, Row_Pos_44);
Defer_Pos_60 := Parser.Current_Pos;



if Defer_Pos_60 /= No_Token_Index then

   Row_Pos_44 := Defer_Pos_60;

else
   Row_Pos_44 := No_Token_Index;
   goto Exit_Row_39_0;

end if;


--  Start opt_code




Defer_Res_61 :=
   Comp_Iter_Or_Parse_0 (Parser, Row_Pos_44);
Defer_Pos_61 := Parser.Current_Pos;


if Defer_Pos_61 = No_Token_Index then

        Defer_Res_61 := No_Bare_Turkixir_Node;


    Defer_Pos_61 := Row_Pos_44;


end if;

--  End opt_code



if Defer_Pos_61 /= No_Token_Index then

   Row_Pos_44 := Defer_Pos_61;

else
   Row_Pos_44 := No_Token_Index;
   goto Exit_Row_39_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_39_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_44 /= No_Token_Index then

   Transform_Res_39 := Allocate_Comp_For (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_39,
      Kind => Turkixir_Comp_For,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_44 = Pos
                            then No_Token_Index
                            else Row_Pos_44 - 1));

      Initialize_Fields_For_Comp_For
        (Self => Transform_Res_39, Comp_For_F_Exprs => Defer_Res_59, Comp_For_F_Target => Defer_Res_60, Comp_For_F_Comp => Defer_Res_61);

         if Defer_Res_59 /= null and then Is_Incomplete (Defer_Res_59) then
            Transform_Res_39.Last_Attempted_Child := 0;
         elsif Defer_Res_59 /= null and then not Is_Ghost (Defer_Res_59) then
            Transform_Res_39.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_60 /= null and then Is_Incomplete (Defer_Res_60) then
            Transform_Res_39.Last_Attempted_Child := 0;
         elsif Defer_Res_60 /= null and then not Is_Ghost (Defer_Res_60) then
            Transform_Res_39.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_61 /= null and then Is_Incomplete (Defer_Res_61) then
            Transform_Res_39.Last_Attempted_Child := 0;
         elsif Defer_Res_61 /= null and then not Is_Ghost (Defer_Res_61) then
            Transform_Res_39.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Comp_For_Transform_Parse_0_Memo,
      Row_Pos_44 /= No_Token_Index,
      Transform_Res_39,
      Pos,
      Row_Pos_44);


   Parser.Current_Pos := Row_Pos_44;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_39;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Comp_For_Transform_Parse_0;

   


function Arg_List_Extract_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Arg_List
is
   use Bare_Arg_List_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_14 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_0 :
            Token_Index
               := No_Token_Index;
      Tmp_List_0 :
            Free_Parse_List;
      Row_Pos_15 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_24 :
            Token_Index
               := No_Token_Index;
      Defer_Res_24 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_25 :
            Token_Index
               := No_Token_Index;
      Defer_Res_25 :
            Bare_Comp_For
               := No_Bare_Turkixir_Node;
      Transform_Res_14 :
            Bare_Arg_Gen
               := No_Bare_Turkixir_Node;
      Row_Pos_16 :
            Token_Index
               := No_Token_Index;
      Row_Pos_17 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_26 :
            Token_Index
               := No_Token_Index;
      Defer_Res_26 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_19 :
            Token_Index
               := No_Token_Index;
      Token_Res_19 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_27 :
            Token_Index
               := No_Token_Index;
      Defer_Res_27 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_15 :
            Bare_Arg_Assoc
               := No_Bare_Turkixir_Node;
      Row_Pos_18 :
            Token_Index
               := No_Token_Index;
      Token_Pos_20 :
            Token_Index
               := No_Token_Index;
      Token_Res_20 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_28 :
            Token_Index
               := No_Token_Index;
      Defer_Res_28 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_16 :
            Bare_Var_Args
               := No_Bare_Turkixir_Node;
      Row_Pos_19 :
            Token_Index
               := No_Token_Index;
      Token_Pos_21 :
            Token_Index
               := No_Token_Index;
      Token_Res_21 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_29 :
            Token_Index
               := No_Token_Index;
      Defer_Res_29 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_17 :
            Bare_Kw_Args
               := No_Bare_Turkixir_Node;
      Or_Pos_11 :
            Token_Index
               := No_Token_Index;
      Or_Res_11 :
            Bare_Arg
               := No_Bare_Turkixir_Node;
      Token_Pos_22 :
            Token_Index
               := No_Token_Index;
      Token_Res_22 :
            Token_Index
               := No_Token_Index;
      List_Pos_0 :
            Token_Index
               := No_Token_Index;
      List_Res_0 :
            Bare_Arg_List
               := No_Bare_Turkixir_Node;
      Token_Pos_23 :
            Token_Index
               := No_Token_Index;
      Token_Res_23 :
            Token_Index
               := No_Token_Index;


   M : Memo_Entry := Get (Parser.Private_Part.Arg_List_Extract_Parse_1_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res_0 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return List_Res_0;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return List_Res_0;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start row_code

Row_Pos_14 := Pos;



--  Start list_code

    List_Pos_0 := Row_Pos_14;



Lst_Cpos_0 := Row_Pos_14;
Tmp_List_0 := Get_Parse_List (Parser);

loop
   
--  Start or_code

Or_Pos_11 := No_Token_Index;
Or_Res_11 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_15 := Lst_Cpos_0;



Defer_Res_24 :=
   Test_Or_Parse_0 (Parser, Row_Pos_15);
Defer_Pos_24 := Parser.Current_Pos;



if Defer_Pos_24 /= No_Token_Index then

   Row_Pos_15 := Defer_Pos_24;

else
   Row_Pos_15 := No_Token_Index;
   goto Exit_Row_14_0;

end if;


Defer_Res_25 :=
   Comp_For_Transform_Parse_0 (Parser, Row_Pos_15);
Defer_Pos_25 := Parser.Current_Pos;



if Defer_Pos_25 /= No_Token_Index then

   Row_Pos_15 := Defer_Pos_25;

else
   Row_Pos_15 := No_Token_Index;
   goto Exit_Row_14_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_14_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_15 /= No_Token_Index then

   Transform_Res_14 := Allocate_Arg_Gen (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_14,
      Kind => Turkixir_Arg_Gen,
      Unit => Parser.Unit,

      Token_Start_Index => Lst_Cpos_0,
      Token_End_Index   => (if Row_Pos_15 = Lst_Cpos_0
                            then No_Token_Index
                            else Row_Pos_15 - 1));

      Initialize_Fields_For_Arg_Gen
        (Self => Transform_Res_14, Arg_Gen_F_Expr => Defer_Res_24, Arg_Gen_F_Comprehension => Defer_Res_25);

         if Defer_Res_24 /= null and then Is_Incomplete (Defer_Res_24) then
            Transform_Res_14.Last_Attempted_Child := 0;
         elsif Defer_Res_24 /= null and then not Is_Ghost (Defer_Res_24) then
            Transform_Res_14.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_25 /= null and then Is_Incomplete (Defer_Res_25) then
            Transform_Res_14.Last_Attempted_Child := 0;
         elsif Defer_Res_25 /= null and then not Is_Ghost (Defer_Res_25) then
            Transform_Res_14.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_15 /= No_Token_Index then
        Or_Pos_11 := Row_Pos_15;
        Or_Res_11 := Transform_Res_14;
        goto Exit_Or_11;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_16 := Lst_Cpos_0;



--  Start opt_code




--  Start row_code

Row_Pos_17 := Row_Pos_16;



Defer_Res_26 :=
   Test_Or_Parse_0 (Parser, Row_Pos_17);
Defer_Pos_26 := Parser.Current_Pos;



if Defer_Pos_26 /= No_Token_Index then

   Row_Pos_17 := Defer_Pos_26;

else
   Row_Pos_17 := No_Token_Index;
   goto Exit_Row_43_0;

end if;


--  Start tok_code

Token_Res_19 := Row_Pos_17;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_19));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Assign)
   then
       Token_Pos_19 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_17 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_17,
             Expected_Token_Id => Turkixir_T_T__Assign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_19 := Row_Pos_17 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_19 /= No_Token_Index then

   Row_Pos_17 := Token_Pos_19;

else
   Row_Pos_17 := No_Token_Index;
   goto Exit_Row_43_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_43_0>>
pragma Warnings (On, "referenced");

--  End row_code


if Row_Pos_17 = No_Token_Index then

        Defer_Res_26 := No_Bare_Turkixir_Node;


    Row_Pos_17 := Row_Pos_16;


end if;

--  End opt_code



if Row_Pos_17 /= No_Token_Index then

   Row_Pos_16 := Row_Pos_17;

else
   Row_Pos_16 := No_Token_Index;
   goto Exit_Row_42_0;

end if;


Defer_Res_27 :=
   Test_Or_Parse_0 (Parser, Row_Pos_16);
Defer_Pos_27 := Parser.Current_Pos;



if Defer_Pos_27 /= No_Token_Index then

   Row_Pos_16 := Defer_Pos_27;

else
   Row_Pos_16 := No_Token_Index;
   goto Exit_Row_42_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_42_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_16 /= No_Token_Index then

   Transform_Res_15 := Allocate_Arg_Assoc (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_15,
      Kind => Turkixir_Arg_Assoc,
      Unit => Parser.Unit,

      Token_Start_Index => Lst_Cpos_0,
      Token_End_Index   => (if Row_Pos_16 = Lst_Cpos_0
                            then No_Token_Index
                            else Row_Pos_16 - 1));

      Initialize_Fields_For_Arg_Assoc
        (Self => Transform_Res_15, Arg_Assoc_F_Name => Defer_Res_26, Arg_Assoc_F_Expr => Defer_Res_27);

         if Defer_Res_26 /= null and then Is_Incomplete (Defer_Res_26) then
            Transform_Res_15.Last_Attempted_Child := 0;
         elsif Defer_Res_26 /= null and then not Is_Ghost (Defer_Res_26) then
            Transform_Res_15.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_27 /= null and then Is_Incomplete (Defer_Res_27) then
            Transform_Res_15.Last_Attempted_Child := 0;
         elsif Defer_Res_27 /= null and then not Is_Ghost (Defer_Res_27) then
            Transform_Res_15.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_16 /= No_Token_Index then
        Or_Pos_11 := Row_Pos_16;
        Or_Res_11 := Transform_Res_15;
        goto Exit_Or_11;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_18 := Lst_Cpos_0;



--  Start tok_code

Token_Res_20 := Row_Pos_18;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_20));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Multiply)
   then
       Token_Pos_20 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_18 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_18,
             Expected_Token_Id => Turkixir_T_T__Multiply,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_20 := Row_Pos_18 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_20 /= No_Token_Index then

   Row_Pos_18 := Token_Pos_20;

else
   Row_Pos_18 := No_Token_Index;
   goto Exit_Row_44_0;

end if;


Defer_Res_28 :=
   Test_Or_Parse_0 (Parser, Row_Pos_18);
Defer_Pos_28 := Parser.Current_Pos;



if Defer_Pos_28 /= No_Token_Index then

   Row_Pos_18 := Defer_Pos_28;

else
   Row_Pos_18 := No_Token_Index;
   goto Exit_Row_44_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_44_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_18 /= No_Token_Index then

   Transform_Res_16 := Allocate_Var_Args (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_16,
      Kind => Turkixir_Var_Args,
      Unit => Parser.Unit,

      Token_Start_Index => Lst_Cpos_0,
      Token_End_Index   => (if Row_Pos_18 = Lst_Cpos_0
                            then No_Token_Index
                            else Row_Pos_18 - 1));

      Initialize_Fields_For_Var_Args
        (Self => Transform_Res_16, Var_Args_F_Expr => Defer_Res_28);

         if Defer_Res_28 /= null and then Is_Incomplete (Defer_Res_28) then
            Transform_Res_16.Last_Attempted_Child := 0;
         elsif Defer_Res_28 /= null and then not Is_Ghost (Defer_Res_28) then
            Transform_Res_16.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_18 /= No_Token_Index then
        Or_Pos_11 := Row_Pos_18;
        Or_Res_11 := Transform_Res_16;
        goto Exit_Or_11;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_19 := Lst_Cpos_0;



--  Start tok_code

Token_Res_21 := Row_Pos_19;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_21));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Power)
   then
       Token_Pos_21 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_19 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_19,
             Expected_Token_Id => Turkixir_T_T__Power,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_21 := Row_Pos_19 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_21 /= No_Token_Index then

   Row_Pos_19 := Token_Pos_21;

else
   Row_Pos_19 := No_Token_Index;
   goto Exit_Row_45_0;

end if;


Defer_Res_29 :=
   Test_Or_Parse_0 (Parser, Row_Pos_19);
Defer_Pos_29 := Parser.Current_Pos;



if Defer_Pos_29 /= No_Token_Index then

   Row_Pos_19 := Defer_Pos_29;

else
   Row_Pos_19 := No_Token_Index;
   goto Exit_Row_45_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_45_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_19 /= No_Token_Index then

   Transform_Res_17 := Allocate_Kw_Args (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_17,
      Kind => Turkixir_Kw_Args,
      Unit => Parser.Unit,

      Token_Start_Index => Lst_Cpos_0,
      Token_End_Index   => (if Row_Pos_19 = Lst_Cpos_0
                            then No_Token_Index
                            else Row_Pos_19 - 1));

      Initialize_Fields_For_Kw_Args
        (Self => Transform_Res_17, Kw_Args_F_Expr => Defer_Res_29);

         if Defer_Res_29 /= null and then Is_Incomplete (Defer_Res_29) then
            Transform_Res_17.Last_Attempted_Child := 0;
         elsif Defer_Res_29 /= null and then not Is_Ghost (Defer_Res_29) then
            Transform_Res_17.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_19 /= No_Token_Index then
        Or_Pos_11 := Row_Pos_19;
        Or_Res_11 := Transform_Res_17;
        goto Exit_Or_11;
    end if;
<<Exit_Or_11>>

--  End or_code


   exit when Or_Pos_11 = No_Token_Index;

   List_Pos_0 := Or_Pos_11;
   Lst_Cpos_0 := List_Pos_0;

   Tmp_List_0.Nodes.Append (Or_Res_11);

      
--  Start tok_code

Token_Res_22 := Lst_Cpos_0;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_22));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_22 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos_0 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos_0,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_22 := Lst_Cpos_0 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos_22 /= No_Token_Index then
          Lst_Cpos_0 := Token_Pos_22;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_0.Nodes.Length;
begin
   List_Res_0 :=
      Allocate_Arg_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_14;
      Token_End := (if Lst_Cpos_0 = Row_Pos_14
                    then Row_Pos_14
                    else Lst_Cpos_0 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_14, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_0,
      Kind              => Turkixir_Arg_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_0,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_0.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_0.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_0);

--  End list_code



if List_Pos_0 /= No_Token_Index then

   Row_Pos_14 := List_Pos_0;

else
   Row_Pos_14 := No_Token_Index;
   goto Exit_Row_13_0;

end if;


--  Start opt_code




--  Start tok_code

Token_Res_23 := Row_Pos_14;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_23));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_23 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_14 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_14,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_23 := Row_Pos_14 + 1;
   end if;
end;

--  End tok_code


if Token_Pos_23 = No_Token_Index then

        Token_Res_23 := No_Token_Index;


    Token_Pos_23 := Row_Pos_14;


end if;

--  End opt_code



if Token_Pos_23 /= No_Token_Index then

   Row_Pos_14 := Token_Pos_23;

else
   Row_Pos_14 := No_Token_Index;
   goto Exit_Row_13_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_13_0>>
pragma Warnings (On, "referenced");

--  End row_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Arg_List_Extract_Parse_1_Memo,
      Row_Pos_14 /= No_Token_Index,
      List_Res_0,
      Pos,
      Row_Pos_14);


   Parser.Current_Pos := Row_Pos_14;

   Exit_Call (Parser, Call_Depth);
   return List_Res_0;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Arg_List_Extract_Parse_1;

   


function Subscript_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_48 :
            Token_Index
               := No_Token_Index;
      Token_Pos_61 :
            Token_Index
               := No_Token_Index;
      Token_Res_61 :
            Token_Index
               := No_Token_Index;
      Token_Pos_62 :
            Token_Index
               := No_Token_Index;
      Token_Res_62 :
            Token_Index
               := No_Token_Index;
      Token_Pos_63 :
            Token_Index
               := No_Token_Index;
      Token_Res_63 :
            Token_Index
               := No_Token_Index;
      Transform_Res_41 :
            Bare_Ellipsis_Expr
               := No_Bare_Turkixir_Node;
      Row_Pos_49 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_68 :
            Token_Index
               := No_Token_Index;
      Defer_Res_68 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_64 :
            Token_Index
               := No_Token_Index;
      Token_Res_64 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_69 :
            Token_Index
               := No_Token_Index;
      Defer_Res_69 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_65 :
            Token_Index
               := No_Token_Index;
      Token_Res_65 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_70 :
            Token_Index
               := No_Token_Index;
      Defer_Res_70 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_42 :
            Bare_Ext_Slice_Expr
               := No_Bare_Turkixir_Node;
      Row_Pos_50 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_71 :
            Token_Index
               := No_Token_Index;
      Defer_Res_71 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_66 :
            Token_Index
               := No_Token_Index;
      Token_Res_66 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_72 :
            Token_Index
               := No_Token_Index;
      Defer_Res_72 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_43 :
            Bare_Slice_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_73 :
            Token_Index
               := No_Token_Index;
      Defer_Res_73 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_22 :
            Token_Index
               := No_Token_Index;
      Or_Res_22 :
            Bare_Expr
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Subscript_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_22 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_22;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_22;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_22 := No_Token_Index;
Or_Res_22 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_48 := Pos;



--  Start tok_code

Token_Res_61 := Row_Pos_48;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_61));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Dot)
   then
       Token_Pos_61 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_48 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_48,
             Expected_Token_Id => Turkixir_T_T__Dot,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_61 := Row_Pos_48 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_61 /= No_Token_Index then

   Row_Pos_48 := Token_Pos_61;

else
   Row_Pos_48 := No_Token_Index;
   goto Exit_Row_48_0;

end if;


--  Start tok_code

Token_Res_62 := Row_Pos_48;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_62));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Dot)
   then
       Token_Pos_62 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_48 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_48,
             Expected_Token_Id => Turkixir_T_T__Dot,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_62 := Row_Pos_48 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_62 /= No_Token_Index then

   Row_Pos_48 := Token_Pos_62;

else
   Row_Pos_48 := No_Token_Index;
   goto Exit_Row_48_0;

end if;


--  Start tok_code

Token_Res_63 := Row_Pos_48;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_63));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Dot)
   then
       Token_Pos_63 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_48 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_48,
             Expected_Token_Id => Turkixir_T_T__Dot,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_63 := Row_Pos_48 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_63 /= No_Token_Index then

   Row_Pos_48 := Token_Pos_63;

else
   Row_Pos_48 := No_Token_Index;
   goto Exit_Row_48_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_48_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_48 /= No_Token_Index then

   Transform_Res_41 := Allocate_Ellipsis_Expr (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_41,
      Kind => Turkixir_Ellipsis_Expr,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_48 = Pos
                            then No_Token_Index
                            else Row_Pos_48 - 1));




end if;

--  End transform_code

    if Row_Pos_48 /= No_Token_Index then
        Or_Pos_22 := Row_Pos_48;
        Or_Res_22 := Transform_Res_41;
        goto Exit_Or_22;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_49 := Pos;



--  Start opt_code




Defer_Res_68 :=
   Test_Or_Parse_0 (Parser, Row_Pos_49);
Defer_Pos_68 := Parser.Current_Pos;


if Defer_Pos_68 = No_Token_Index then

        Defer_Res_68 := No_Bare_Turkixir_Node;


    Defer_Pos_68 := Row_Pos_49;


end if;

--  End opt_code



if Defer_Pos_68 /= No_Token_Index then

   Row_Pos_49 := Defer_Pos_68;

else
   Row_Pos_49 := No_Token_Index;
   goto Exit_Row_49_0;

end if;


--  Start tok_code

Token_Res_64 := Row_Pos_49;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_64));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_64 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_49 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_49,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_64 := Row_Pos_49 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_64 /= No_Token_Index then

   Row_Pos_49 := Token_Pos_64;

else
   Row_Pos_49 := No_Token_Index;
   goto Exit_Row_49_0;

end if;


--  Start opt_code




Defer_Res_69 :=
   Test_Or_Parse_0 (Parser, Row_Pos_49);
Defer_Pos_69 := Parser.Current_Pos;


if Defer_Pos_69 = No_Token_Index then

        Defer_Res_69 := No_Bare_Turkixir_Node;


    Defer_Pos_69 := Row_Pos_49;


end if;

--  End opt_code



if Defer_Pos_69 /= No_Token_Index then

   Row_Pos_49 := Defer_Pos_69;

else
   Row_Pos_49 := No_Token_Index;
   goto Exit_Row_49_0;

end if;


--  Start tok_code

Token_Res_65 := Row_Pos_49;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_65));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_65 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_49 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_49,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_65 := Row_Pos_49 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_65 /= No_Token_Index then

   Row_Pos_49 := Token_Pos_65;

else
   Row_Pos_49 := No_Token_Index;
   goto Exit_Row_49_0;

end if;


--  Start opt_code




Defer_Res_70 :=
   Test_Or_Parse_0 (Parser, Row_Pos_49);
Defer_Pos_70 := Parser.Current_Pos;


if Defer_Pos_70 = No_Token_Index then

        Defer_Res_70 := No_Bare_Turkixir_Node;


    Defer_Pos_70 := Row_Pos_49;


end if;

--  End opt_code



if Defer_Pos_70 /= No_Token_Index then

   Row_Pos_49 := Defer_Pos_70;

else
   Row_Pos_49 := No_Token_Index;
   goto Exit_Row_49_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_49_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_49 /= No_Token_Index then

   Transform_Res_42 := Allocate_Ext_Slice_Expr (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_42,
      Kind => Turkixir_Ext_Slice_Expr,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_49 = Pos
                            then No_Token_Index
                            else Row_Pos_49 - 1));

      Initialize_Fields_For_Ext_Slice_Expr
        (Self => Transform_Res_42, Slice_Expr_F_First => Defer_Res_68, Slice_Expr_F_Last => Defer_Res_69, Ext_Slice_Expr_F_Stride => Defer_Res_70);

         if Defer_Res_68 /= null and then Is_Incomplete (Defer_Res_68) then
            Transform_Res_42.Last_Attempted_Child := 0;
         elsif Defer_Res_68 /= null and then not Is_Ghost (Defer_Res_68) then
            Transform_Res_42.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_69 /= null and then Is_Incomplete (Defer_Res_69) then
            Transform_Res_42.Last_Attempted_Child := 0;
         elsif Defer_Res_69 /= null and then not Is_Ghost (Defer_Res_69) then
            Transform_Res_42.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_70 /= null and then Is_Incomplete (Defer_Res_70) then
            Transform_Res_42.Last_Attempted_Child := 0;
         elsif Defer_Res_70 /= null and then not Is_Ghost (Defer_Res_70) then
            Transform_Res_42.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_49 /= No_Token_Index then
        Or_Pos_22 := Row_Pos_49;
        Or_Res_22 := Transform_Res_42;
        goto Exit_Or_22;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_50 := Pos;



--  Start opt_code




Defer_Res_71 :=
   Test_Or_Parse_0 (Parser, Row_Pos_50);
Defer_Pos_71 := Parser.Current_Pos;


if Defer_Pos_71 = No_Token_Index then

        Defer_Res_71 := No_Bare_Turkixir_Node;


    Defer_Pos_71 := Row_Pos_50;


end if;

--  End opt_code



if Defer_Pos_71 /= No_Token_Index then

   Row_Pos_50 := Defer_Pos_71;

else
   Row_Pos_50 := No_Token_Index;
   goto Exit_Row_50_0;

end if;


--  Start tok_code

Token_Res_66 := Row_Pos_50;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_66));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_66 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_50 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_50,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_66 := Row_Pos_50 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_66 /= No_Token_Index then

   Row_Pos_50 := Token_Pos_66;

else
   Row_Pos_50 := No_Token_Index;
   goto Exit_Row_50_0;

end if;


--  Start opt_code




Defer_Res_72 :=
   Test_Or_Parse_0 (Parser, Row_Pos_50);
Defer_Pos_72 := Parser.Current_Pos;


if Defer_Pos_72 = No_Token_Index then

        Defer_Res_72 := No_Bare_Turkixir_Node;


    Defer_Pos_72 := Row_Pos_50;


end if;

--  End opt_code



if Defer_Pos_72 /= No_Token_Index then

   Row_Pos_50 := Defer_Pos_72;

else
   Row_Pos_50 := No_Token_Index;
   goto Exit_Row_50_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_50_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_50 /= No_Token_Index then

   Transform_Res_43 := Allocate_Slice_Expr (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_43,
      Kind => Turkixir_Slice_Expr,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_50 = Pos
                            then No_Token_Index
                            else Row_Pos_50 - 1));

      Initialize_Fields_For_Slice_Expr
        (Self => Transform_Res_43, Slice_Expr_F_First => Defer_Res_71, Slice_Expr_F_Last => Defer_Res_72);

         if Defer_Res_71 /= null and then Is_Incomplete (Defer_Res_71) then
            Transform_Res_43.Last_Attempted_Child := 0;
         elsif Defer_Res_71 /= null and then not Is_Ghost (Defer_Res_71) then
            Transform_Res_43.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_72 /= null and then Is_Incomplete (Defer_Res_72) then
            Transform_Res_43.Last_Attempted_Child := 0;
         elsif Defer_Res_72 /= null and then not Is_Ghost (Defer_Res_72) then
            Transform_Res_43.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_50 /= No_Token_Index then
        Or_Pos_22 := Row_Pos_50;
        Or_Res_22 := Transform_Res_43;
        goto Exit_Or_22;
    end if;
    
Defer_Res_73 :=
   Test_Or_Parse_0 (Parser, Pos);
Defer_Pos_73 := Parser.Current_Pos;

    if Defer_Pos_73 /= No_Token_Index then
        Or_Pos_22 := Defer_Pos_73;
        Or_Res_22 := Defer_Res_73;
        goto Exit_Or_22;
    end if;
<<Exit_Or_22>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Subscript_Or_Parse_0_Memo,
      Or_Pos_22 /= No_Token_Index,
      Or_Res_22,
      Pos,
      Or_Pos_22);


   Parser.Current_Pos := Or_Pos_22;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_22;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Subscript_Or_Parse_0;

   


function Subscript_List_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr_List
is
   use Bare_Expr_List_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_47 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_4 :
            Token_Index
               := No_Token_Index;
      Tmp_List_4 :
            Free_Parse_List;
      Defer_Pos_67 :
            Token_Index
               := No_Token_Index;
      Defer_Res_67 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_59 :
            Token_Index
               := No_Token_Index;
      Token_Res_59 :
            Token_Index
               := No_Token_Index;
      List_Pos_4 :
            Token_Index
               := No_Token_Index;
      List_Res_4 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Token_Pos_60 :
            Token_Index
               := No_Token_Index;
      Token_Res_60 :
            Token_Index
               := No_Token_Index;


   M : Memo_Entry := Get (Parser.Private_Part.Subscript_List_Extract_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res_4 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return List_Res_4;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return List_Res_4;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start row_code

Row_Pos_47 := Pos;



--  Start list_code

    List_Pos_4 := No_Token_Index;



Lst_Cpos_4 := Row_Pos_47;
Tmp_List_4 := Get_Parse_List (Parser);

loop
   
Defer_Res_67 :=
   Subscript_Or_Parse_0 (Parser, Lst_Cpos_4);
Defer_Pos_67 := Parser.Current_Pos;


   exit when Defer_Pos_67 = No_Token_Index;

   List_Pos_4 := Defer_Pos_67;
   Lst_Cpos_4 := List_Pos_4;

   Tmp_List_4.Nodes.Append (Defer_Res_67);

      
--  Start tok_code

Token_Res_59 := Lst_Cpos_4;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_59));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_59 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos_4 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos_4,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_59 := Lst_Cpos_4 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos_59 /= No_Token_Index then
          Lst_Cpos_4 := Token_Pos_59;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_4.Nodes.Length;
begin
   List_Res_4 :=
      Allocate_Expr_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_47;
      Token_End := (if Lst_Cpos_4 = Row_Pos_47
                    then Row_Pos_47
                    else Lst_Cpos_4 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_47, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_4,
      Kind              => Turkixir_Expr_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_4,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_4.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_4.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_4);

--  End list_code



if List_Pos_4 /= No_Token_Index then

   Row_Pos_47 := List_Pos_4;

else
   Row_Pos_47 := No_Token_Index;
   goto Exit_Row_47_0;

end if;


--  Start opt_code




--  Start tok_code

Token_Res_60 := Row_Pos_47;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_60));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_60 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_47 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_47,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_60 := Row_Pos_47 + 1;
   end if;
end;

--  End tok_code


if Token_Pos_60 = No_Token_Index then

        Token_Res_60 := No_Token_Index;


    Token_Pos_60 := Row_Pos_47;


end if;

--  End opt_code



if Token_Pos_60 /= No_Token_Index then

   Row_Pos_47 := Token_Pos_60;

else
   Row_Pos_47 := No_Token_Index;
   goto Exit_Row_47_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_47_0>>
pragma Warnings (On, "referenced");

--  End row_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Subscript_List_Extract_Parse_0_Memo,
      Row_Pos_47 /= No_Token_Index,
      List_Res_4,
      Pos,
      Row_Pos_47);


   Parser.Current_Pos := Row_Pos_47;

   Exit_Call (Parser, Call_Depth);
   return List_Res_4;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Subscript_List_Extract_Parse_0;

   


function Test_List_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr_List
is
   use Bare_Expr_List_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_62 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_6 :
            Token_Index
               := No_Token_Index;
      Tmp_List_6 :
            Free_Parse_List;
      Defer_Pos_93 :
            Token_Index
               := No_Token_Index;
      Defer_Res_93 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_88 :
            Token_Index
               := No_Token_Index;
      Token_Res_88 :
            Token_Index
               := No_Token_Index;
      List_Pos_6 :
            Token_Index
               := No_Token_Index;
      List_Res_6 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Token_Pos_89 :
            Token_Index
               := No_Token_Index;
      Token_Res_89 :
            Token_Index
               := No_Token_Index;


   M : Memo_Entry := Get (Parser.Private_Part.Test_List_Extract_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res_6 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return List_Res_6;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return List_Res_6;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start row_code

Row_Pos_62 := Pos;



--  Start list_code

    List_Pos_6 := No_Token_Index;



Lst_Cpos_6 := Row_Pos_62;
Tmp_List_6 := Get_Parse_List (Parser);

loop
   
Defer_Res_93 :=
   Test_Or_Parse_0 (Parser, Lst_Cpos_6);
Defer_Pos_93 := Parser.Current_Pos;


   exit when Defer_Pos_93 = No_Token_Index;

   List_Pos_6 := Defer_Pos_93;
   Lst_Cpos_6 := List_Pos_6;

   Tmp_List_6.Nodes.Append (Defer_Res_93);

      
--  Start tok_code

Token_Res_88 := Lst_Cpos_6;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_88));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_88 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos_6 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos_6,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_88 := Lst_Cpos_6 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos_88 /= No_Token_Index then
          Lst_Cpos_6 := Token_Pos_88;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_6.Nodes.Length;
begin
   List_Res_6 :=
      Allocate_Expr_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_62;
      Token_End := (if Lst_Cpos_6 = Row_Pos_62
                    then Row_Pos_62
                    else Lst_Cpos_6 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_62, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_6,
      Kind              => Turkixir_Expr_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_6,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_6.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_6.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_6);

--  End list_code



if List_Pos_6 /= No_Token_Index then

   Row_Pos_62 := List_Pos_6;

else
   Row_Pos_62 := No_Token_Index;
   goto Exit_Row_53_0;

end if;


--  Start opt_code




--  Start tok_code

Token_Res_89 := Row_Pos_62;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_89));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_89 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_62 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_62,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_89 := Row_Pos_62 + 1;
   end if;
end;

--  End tok_code


if Token_Pos_89 = No_Token_Index then

        Token_Res_89 := No_Token_Index;


    Token_Pos_89 := Row_Pos_62;


end if;

--  End opt_code



if Token_Pos_89 /= No_Token_Index then

   Row_Pos_62 := Token_Pos_89;

else
   Row_Pos_62 := No_Token_Index;
   goto Exit_Row_53_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_53_0>>
pragma Warnings (On, "referenced");

--  End row_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Test_List_Extract_Parse_0_Memo,
      Row_Pos_62 /= No_Token_Index,
      List_Res_6,
      Pos,
      Row_Pos_62);


   Parser.Current_Pos := Row_Pos_62;

   Exit_Call (Parser, Call_Depth);
   return List_Res_6;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Test_List_Extract_Parse_0;

   


function Yield_Expr_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Yield_Expr
is
   use Bare_Yield_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_61 :
            Token_Index
               := No_Token_Index;
      Token_Pos_87 :
            Token_Index
               := No_Token_Index;
      Token_Res_87 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_92 :
            Token_Index
               := No_Token_Index;
      Defer_Res_92 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Transform_Res_52 :
            Bare_Yield_Expr
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Yield_Expr_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_52 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_52;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_52;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_61 := Pos;



--  Start tok_code

Token_Res_87 := Row_Pos_61;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_87));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Yield)
   then
       Token_Pos_87 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_61 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_61,
             Expected_Token_Id => Turkixir_T_T__Yield,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_87 := Row_Pos_61 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_87 /= No_Token_Index then

   Row_Pos_61 := Token_Pos_87;

else
   Row_Pos_61 := No_Token_Index;
   goto Exit_Row_52_0;

end if;


--  Start opt_code




Defer_Res_92 :=
   Test_List_Extract_Parse_0 (Parser, Row_Pos_61);
Defer_Pos_92 := Parser.Current_Pos;


if Defer_Pos_92 = No_Token_Index then

        Defer_Res_92 :=
           Allocate_Expr_List (Parser.Mem_Pool);
         Initialize
           (Self              => Defer_Res_92,
            Kind              => Turkixir_Expr_List,
            Unit              => Parser.Unit,
            Token_Start_Index => Row_Pos_61 - 1,
            Token_End_Index   => No_Token_Index);
         Initialize_List
           (Self   => Defer_Res_92,
            Parser => Parser,
            Count  => 0);


    Defer_Pos_92 := Row_Pos_61;


end if;

--  End opt_code



if Defer_Pos_92 /= No_Token_Index then

   Row_Pos_61 := Defer_Pos_92;

else
   Row_Pos_61 := No_Token_Index;
   goto Exit_Row_52_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_52_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_61 /= No_Token_Index then

   Transform_Res_52 := Allocate_Yield_Expr (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_52,
      Kind => Turkixir_Yield_Expr,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_61 = Pos
                            then No_Token_Index
                            else Row_Pos_61 - 1));

      Initialize_Fields_For_Yield_Expr
        (Self => Transform_Res_52, Yield_Expr_F_Exprs => Defer_Res_92);

         if Defer_Res_92 /= null and then Is_Incomplete (Defer_Res_92) then
            Transform_Res_52.Last_Attempted_Child := 0;
         elsif Defer_Res_92 /= null and then not Is_Ghost (Defer_Res_92) then
            Transform_Res_52.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Yield_Expr_Transform_Parse_0_Memo,
      Row_Pos_61 /= No_Token_Index,
      Transform_Res_52,
      Pos,
      Row_Pos_61);


   Parser.Current_Pos := Row_Pos_61;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_52;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Yield_Expr_Transform_Parse_0;

   


function List_If_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Comp_If
is
   use Bare_Comp_If_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_64 :
            Token_Index
               := No_Token_Index;
      Token_Pos_92 :
            Token_Index
               := No_Token_Index;
      Token_Res_92 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_99 :
            Token_Index
               := No_Token_Index;
      Defer_Res_99 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_100 :
            Token_Index
               := No_Token_Index;
      Defer_Res_100 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Transform_Res_54 :
            Bare_Comp_If
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.List_If_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_54 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_54;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_54;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_64 := Pos;



--  Start tok_code

Token_Res_92 := Row_Pos_64;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_92));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__If)
   then
       Token_Pos_92 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_64 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_64,
             Expected_Token_Id => Turkixir_T_T__If,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_92 := Row_Pos_64 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_92 /= No_Token_Index then

   Row_Pos_64 := Token_Pos_92;

else
   Row_Pos_64 := No_Token_Index;
   goto Exit_Row_56_0;

end if;


Defer_Res_99 :=
   Test_Or_Parse_0 (Parser, Row_Pos_64);
Defer_Pos_99 := Parser.Current_Pos;



if Defer_Pos_99 /= No_Token_Index then

   Row_Pos_64 := Defer_Pos_99;

else
   Row_Pos_64 := No_Token_Index;
   goto Exit_Row_56_0;

end if;


--  Start opt_code




Defer_Res_100 :=
   List_Iter_Or_Parse_0 (Parser, Row_Pos_64);
Defer_Pos_100 := Parser.Current_Pos;


if Defer_Pos_100 = No_Token_Index then

        Defer_Res_100 := No_Bare_Turkixir_Node;


    Defer_Pos_100 := Row_Pos_64;


end if;

--  End opt_code



if Defer_Pos_100 /= No_Token_Index then

   Row_Pos_64 := Defer_Pos_100;

else
   Row_Pos_64 := No_Token_Index;
   goto Exit_Row_56_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_56_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_64 /= No_Token_Index then

   Transform_Res_54 := Allocate_Comp_If (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_54,
      Kind => Turkixir_Comp_If,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_64 = Pos
                            then No_Token_Index
                            else Row_Pos_64 - 1));

      Initialize_Fields_For_Comp_If
        (Self => Transform_Res_54, Comp_If_F_Test => Defer_Res_99, Comp_If_F_Comp => Defer_Res_100);

         if Defer_Res_99 /= null and then Is_Incomplete (Defer_Res_99) then
            Transform_Res_54.Last_Attempted_Child := 0;
         elsif Defer_Res_99 /= null and then not Is_Ghost (Defer_Res_99) then
            Transform_Res_54.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_100 /= null and then Is_Incomplete (Defer_Res_100) then
            Transform_Res_54.Last_Attempted_Child := 0;
         elsif Defer_Res_100 /= null and then not Is_Ghost (Defer_Res_100) then
            Transform_Res_54.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.List_If_Transform_Parse_0_Memo,
      Row_Pos_64 /= No_Token_Index,
      Transform_Res_54,
      Pos,
      Row_Pos_64);


   Parser.Current_Pos := Row_Pos_64;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_54;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end List_If_Transform_Parse_0;

   


function List_Iter_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node
is
   use Bare_Turkixir_Node_Memos;

   Call_Depth : aliased Natural;

      Defer_Pos_97 :
            Token_Index
               := No_Token_Index;
      Defer_Res_97 :
            Bare_Comp_ForL
               := No_Bare_Turkixir_Node;
      Defer_Pos_98 :
            Token_Index
               := No_Token_Index;
      Defer_Res_98 :
            Bare_Comp_If
               := No_Bare_Turkixir_Node;
      Or_Pos_24 :
            Token_Index
               := No_Token_Index;
      Or_Res_24 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.List_Iter_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_24 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_24;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_24;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_24 := No_Token_Index;
Or_Res_24 := No_Bare_Turkixir_Node;
    
Defer_Res_97 :=
   List_For_Transform_Parse_0 (Parser, Pos);
Defer_Pos_97 := Parser.Current_Pos;

    if Defer_Pos_97 /= No_Token_Index then
        Or_Pos_24 := Defer_Pos_97;
        Or_Res_24 := Defer_Res_97;
        goto Exit_Or_24;
    end if;
    
Defer_Res_98 :=
   List_If_Transform_Parse_0 (Parser, Pos);
Defer_Pos_98 := Parser.Current_Pos;

    if Defer_Pos_98 /= No_Token_Index then
        Or_Pos_24 := Defer_Pos_98;
        Or_Res_24 := Defer_Res_98;
        goto Exit_Or_24;
    end if;
<<Exit_Or_24>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.List_Iter_Or_Parse_0_Memo,
      Or_Pos_24 /= No_Token_Index,
      Or_Res_24,
      Pos,
      Or_Pos_24);


   Parser.Current_Pos := Or_Pos_24;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_24;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end List_Iter_Or_Parse_0;

   


function List_For_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Comp_ForL
is
   use Bare_Comp_ForL_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_63 :
            Token_Index
               := No_Token_Index;
      Token_Pos_90 :
            Token_Index
               := No_Token_Index;
      Token_Res_90 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_94 :
            Token_Index
               := No_Token_Index;
      Defer_Res_94 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Token_Pos_91 :
            Token_Index
               := No_Token_Index;
      Token_Res_91 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_95 :
            Token_Index
               := No_Token_Index;
      Defer_Res_95 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Defer_Pos_96 :
            Token_Index
               := No_Token_Index;
      Defer_Res_96 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Transform_Res_53 :
            Bare_Comp_ForL
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.List_For_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_53 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_53;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_53;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_63 := Pos;



--  Start tok_code

Token_Res_90 := Row_Pos_63;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_90));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__For)
   then
       Token_Pos_90 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_63 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_63,
             Expected_Token_Id => Turkixir_T_T__For,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_90 := Row_Pos_63 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_90 /= No_Token_Index then

   Row_Pos_63 := Token_Pos_90;

else
   Row_Pos_63 := No_Token_Index;
   goto Exit_Row_55_0;

end if;


Defer_Res_94 :=
   Expr_List_Extract_Parse_0 (Parser, Row_Pos_63);
Defer_Pos_94 := Parser.Current_Pos;



if Defer_Pos_94 /= No_Token_Index then

   Row_Pos_63 := Defer_Pos_94;

else
   Row_Pos_63 := No_Token_Index;
   goto Exit_Row_55_0;

end if;


--  Start tok_code

Token_Res_91 := Row_Pos_63;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_91));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__In)
   then
       Token_Pos_91 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_63 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_63,
             Expected_Token_Id => Turkixir_T_T__In,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_91 := Row_Pos_63 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_91 /= No_Token_Index then

   Row_Pos_63 := Token_Pos_91;

else
   Row_Pos_63 := No_Token_Index;
   goto Exit_Row_55_0;

end if;


Defer_Res_95 :=
   Test_List_Extract_Parse_0 (Parser, Row_Pos_63);
Defer_Pos_95 := Parser.Current_Pos;



if Defer_Pos_95 /= No_Token_Index then

   Row_Pos_63 := Defer_Pos_95;

else
   Row_Pos_63 := No_Token_Index;
   goto Exit_Row_55_0;

end if;


--  Start opt_code




Defer_Res_96 :=
   List_Iter_Or_Parse_0 (Parser, Row_Pos_63);
Defer_Pos_96 := Parser.Current_Pos;


if Defer_Pos_96 = No_Token_Index then

        Defer_Res_96 := No_Bare_Turkixir_Node;


    Defer_Pos_96 := Row_Pos_63;


end if;

--  End opt_code



if Defer_Pos_96 /= No_Token_Index then

   Row_Pos_63 := Defer_Pos_96;

else
   Row_Pos_63 := No_Token_Index;
   goto Exit_Row_55_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_55_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_63 /= No_Token_Index then

   Transform_Res_53 := Allocate_Comp_ForL (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_53,
      Kind => Turkixir_Comp_ForL,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_63 = Pos
                            then No_Token_Index
                            else Row_Pos_63 - 1));

      Initialize_Fields_For_Comp_ForL
        (Self => Transform_Res_53, Comp_ForL_F_Exprs => Defer_Res_94, Comp_ForL_F_Target => Defer_Res_95, Comp_ForL_F_Comp => Defer_Res_96);

         if Defer_Res_94 /= null and then Is_Incomplete (Defer_Res_94) then
            Transform_Res_53.Last_Attempted_Child := 0;
         elsif Defer_Res_94 /= null and then not Is_Ghost (Defer_Res_94) then
            Transform_Res_53.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_95 /= null and then Is_Incomplete (Defer_Res_95) then
            Transform_Res_53.Last_Attempted_Child := 0;
         elsif Defer_Res_95 /= null and then not Is_Ghost (Defer_Res_95) then
            Transform_Res_53.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_96 /= null and then Is_Incomplete (Defer_Res_96) then
            Transform_Res_53.Last_Attempted_Child := 0;
         elsif Defer_Res_96 /= null and then not Is_Ghost (Defer_Res_96) then
            Transform_Res_53.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.List_For_Transform_Parse_0_Memo,
      Row_Pos_63 /= No_Token_Index,
      Transform_Res_53,
      Pos,
      Row_Pos_63);


   Parser.Current_Pos := Row_Pos_63;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_53;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end List_For_Transform_Parse_0;

   


function Empty_Test_List_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr_List
is
   use Bare_Expr_List_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_65 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_7 :
            Token_Index
               := No_Token_Index;
      Tmp_List_7 :
            Free_Parse_List;
      Defer_Pos_101 :
            Token_Index
               := No_Token_Index;
      Defer_Res_101 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_93 :
            Token_Index
               := No_Token_Index;
      Token_Res_93 :
            Token_Index
               := No_Token_Index;
      List_Pos_7 :
            Token_Index
               := No_Token_Index;
      List_Res_7 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Token_Pos_94 :
            Token_Index
               := No_Token_Index;
      Token_Res_94 :
            Token_Index
               := No_Token_Index;


   M : Memo_Entry := Get (Parser.Private_Part.Empty_Test_List_Extract_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res_7 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return List_Res_7;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return List_Res_7;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start row_code

Row_Pos_65 := Pos;



--  Start list_code

    List_Pos_7 := Row_Pos_65;



Lst_Cpos_7 := Row_Pos_65;
Tmp_List_7 := Get_Parse_List (Parser);

loop
   
Defer_Res_101 :=
   Test_Or_Parse_0 (Parser, Lst_Cpos_7);
Defer_Pos_101 := Parser.Current_Pos;


   exit when Defer_Pos_101 = No_Token_Index;

   List_Pos_7 := Defer_Pos_101;
   Lst_Cpos_7 := List_Pos_7;

   Tmp_List_7.Nodes.Append (Defer_Res_101);

      
--  Start tok_code

Token_Res_93 := Lst_Cpos_7;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_93));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_93 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos_7 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos_7,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_93 := Lst_Cpos_7 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos_93 /= No_Token_Index then
          Lst_Cpos_7 := Token_Pos_93;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_7.Nodes.Length;
begin
   List_Res_7 :=
      Allocate_Expr_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_65;
      Token_End := (if Lst_Cpos_7 = Row_Pos_65
                    then Row_Pos_65
                    else Lst_Cpos_7 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_65, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_7,
      Kind              => Turkixir_Expr_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_7,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_7.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_7.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_7);

--  End list_code



if List_Pos_7 /= No_Token_Index then

   Row_Pos_65 := List_Pos_7;

else
   Row_Pos_65 := No_Token_Index;
   goto Exit_Row_60_0;

end if;


--  Start opt_code




--  Start tok_code

Token_Res_94 := Row_Pos_65;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_94));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_94 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_65 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_65,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_94 := Row_Pos_65 + 1;
   end if;
end;

--  End tok_code


if Token_Pos_94 = No_Token_Index then

        Token_Res_94 := No_Token_Index;


    Token_Pos_94 := Row_Pos_65;


end if;

--  End opt_code



if Token_Pos_94 /= No_Token_Index then

   Row_Pos_65 := Token_Pos_94;

else
   Row_Pos_65 := No_Token_Index;
   goto Exit_Row_60_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_60_0>>
pragma Warnings (On, "referenced");

--  End row_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Empty_Test_List_Extract_Parse_0_Memo,
      Row_Pos_65 /= No_Token_Index,
      List_Res_7,
      Pos,
      Row_Pos_65);


   Parser.Current_Pos := Row_Pos_65;

   Exit_Call (Parser, Call_Depth);
   return List_Res_7;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Empty_Test_List_Extract_Parse_0;

   


function Set_Lit_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Set_Lit
is
   use Bare_Set_Lit_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_66 :
            Token_Index
               := No_Token_Index;
      Token_Pos_95 :
            Token_Index
               := No_Token_Index;
      Token_Res_95 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_102 :
            Token_Index
               := No_Token_Index;
      Defer_Res_102 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Token_Pos_96 :
            Token_Index
               := No_Token_Index;
      Token_Res_96 :
            Token_Index
               := No_Token_Index;
      Transform_Res_55 :
            Bare_Set_Lit
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Set_Lit_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_55 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_55;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_55;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_66 := Pos;



--  Start tok_code

Token_Res_95 := Row_Pos_66;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_95));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_L_Curl)
   then
       Token_Pos_95 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_66 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_66,
             Expected_Token_Id => Turkixir_T_T_L_Curl,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_95 := Row_Pos_66 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_95 /= No_Token_Index then

   Row_Pos_66 := Token_Pos_95;

else
   Row_Pos_66 := No_Token_Index;
   goto Exit_Row_62_0;

end if;


Defer_Res_102 :=
   Empty_Test_List_Extract_Parse_0 (Parser, Row_Pos_66);
Defer_Pos_102 := Parser.Current_Pos;



if Defer_Pos_102 /= No_Token_Index then

   Row_Pos_66 := Defer_Pos_102;

else
   Row_Pos_66 := No_Token_Index;
   goto Exit_Row_62_0;

end if;


--  Start tok_code

Token_Res_96 := Row_Pos_66;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_96));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Rcurl)
   then
       Token_Pos_96 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_66 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_66,
             Expected_Token_Id => Turkixir_T_T__Rcurl,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_96 := Row_Pos_66 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_96 /= No_Token_Index then

   Row_Pos_66 := Token_Pos_96;

else
   Row_Pos_66 := No_Token_Index;
   goto Exit_Row_62_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_62_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_66 /= No_Token_Index then

   Transform_Res_55 := Allocate_Set_Lit (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_55,
      Kind => Turkixir_Set_Lit,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_66 = Pos
                            then No_Token_Index
                            else Row_Pos_66 - 1));

      Initialize_Fields_For_Set_Lit
        (Self => Transform_Res_55, Set_Lit_F_Exprs => Defer_Res_102);

         if Defer_Res_102 /= null and then Is_Incomplete (Defer_Res_102) then
            Transform_Res_55.Last_Attempted_Child := 0;
         elsif Defer_Res_102 /= null and then not Is_Ghost (Defer_Res_102) then
            Transform_Res_55.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Set_Lit_Transform_Parse_0_Memo,
      Row_Pos_66 /= No_Token_Index,
      Transform_Res_55,
      Pos,
      Row_Pos_66);


   Parser.Current_Pos := Row_Pos_66;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_55;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Set_Lit_Transform_Parse_0;

   


function Dict_Assoc_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Dict_Assoc
is
   use Bare_Dict_Assoc_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_67 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_103 :
            Token_Index
               := No_Token_Index;
      Defer_Res_103 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_97 :
            Token_Index
               := No_Token_Index;
      Token_Res_97 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_104 :
            Token_Index
               := No_Token_Index;
      Defer_Res_104 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_56 :
            Bare_Dict_Assoc
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Dict_Assoc_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_56 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_56;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_56;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_67 := Pos;



Defer_Res_103 :=
   Test_Or_Parse_0 (Parser, Row_Pos_67);
Defer_Pos_103 := Parser.Current_Pos;



if Defer_Pos_103 /= No_Token_Index then

   Row_Pos_67 := Defer_Pos_103;

else
   Row_Pos_67 := No_Token_Index;
   goto Exit_Row_64_0;

end if;


--  Start tok_code

Token_Res_97 := Row_Pos_67;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_97));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_97 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_67 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_67,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_97 := Row_Pos_67 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_97 /= No_Token_Index then

   Row_Pos_67 := Token_Pos_97;

else
   Row_Pos_67 := No_Token_Index;
   goto Exit_Row_64_0;

end if;


Defer_Res_104 :=
   Test_Or_Parse_0 (Parser, Row_Pos_67);
Defer_Pos_104 := Parser.Current_Pos;



if Defer_Pos_104 /= No_Token_Index then

   Row_Pos_67 := Defer_Pos_104;

else
   Row_Pos_67 := No_Token_Index;
   goto Exit_Row_64_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_64_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_67 /= No_Token_Index then

   Transform_Res_56 := Allocate_Dict_Assoc (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_56,
      Kind => Turkixir_Dict_Assoc,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_67 = Pos
                            then No_Token_Index
                            else Row_Pos_67 - 1));

      Initialize_Fields_For_Dict_Assoc
        (Self => Transform_Res_56, Dict_Assoc_F_Key => Defer_Res_103, Dict_Assoc_F_Value => Defer_Res_104);

         if Defer_Res_103 /= null and then Is_Incomplete (Defer_Res_103) then
            Transform_Res_56.Last_Attempted_Child := 0;
         elsif Defer_Res_103 /= null and then not Is_Ghost (Defer_Res_103) then
            Transform_Res_56.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_104 /= null and then Is_Incomplete (Defer_Res_104) then
            Transform_Res_56.Last_Attempted_Child := 0;
         elsif Defer_Res_104 /= null and then not Is_Ghost (Defer_Res_104) then
            Transform_Res_56.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Dict_Assoc_Transform_Parse_0_Memo,
      Row_Pos_67 /= No_Token_Index,
      Transform_Res_56,
      Pos,
      Row_Pos_67);


   Parser.Current_Pos := Row_Pos_67;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_56;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Dict_Assoc_Transform_Parse_0;

   


function Number_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Number_Lit
is
   use Bare_Number_Lit_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_68 :
            Token_Index
               := No_Token_Index;
      Token_Pos_98 :
            Token_Index
               := No_Token_Index;
      Token_Res_98 :
            Token_Index
               := No_Token_Index;
      Transform_Res_57 :
            Bare_Number_Lit
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Number_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_57 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_57;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_57;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_68 := Pos;



--  Start tok_code

Token_Res_98 := Row_Pos_68;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_98));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Number)
   then
       Token_Pos_98 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_68 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_68,
             Expected_Token_Id => Turkixir_T_T__Number,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_98 := Row_Pos_68 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_98 /= No_Token_Index then

   Row_Pos_68 := Token_Pos_98;

else
   Row_Pos_68 := No_Token_Index;
   goto Exit_Row_68_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_68_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_68 /= No_Token_Index then

   Transform_Res_57 := Allocate_Number_Lit (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_57,
      Kind => Turkixir_Number_Lit,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_68 = Pos
                            then No_Token_Index
                            else Row_Pos_68 - 1));




end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Number_Transform_Parse_0_Memo,
      Row_Pos_68 /= No_Token_Index,
      Transform_Res_57,
      Pos,
      Row_Pos_68);


   Parser.Current_Pos := Row_Pos_68;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_57;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Number_Transform_Parse_0;

   


function String_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_String_Lit
is
   use Bare_String_Lit_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_70 :
            Token_Index
               := No_Token_Index;
      Token_Pos_99 :
            Token_Index
               := No_Token_Index;
      Token_Res_99 :
            Token_Index
               := No_Token_Index;
      Transform_Res_59 :
            Bare_String_Lit
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.String_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_59 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_59;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_59;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_70 := Pos;



--  Start tok_code

Token_Res_99 := Row_Pos_70;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_99));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__String)
   then
       Token_Pos_99 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_70 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_70,
             Expected_Token_Id => Turkixir_T_T__String,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_99 := Row_Pos_70 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_99 /= No_Token_Index then

   Row_Pos_70 := Token_Pos_99;

else
   Row_Pos_70 := No_Token_Index;
   goto Exit_Row_70_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_70_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_70 /= No_Token_Index then

   Transform_Res_59 := Allocate_String_Lit (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_59,
      Kind => Turkixir_String_Lit,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_70 = Pos
                            then No_Token_Index
                            else Row_Pos_70 - 1));




end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.String_Transform_Parse_0_Memo,
      Row_Pos_70 /= No_Token_Index,
      Transform_Res_59,
      Pos,
      Row_Pos_70);


   Parser.Current_Pos := Row_Pos_70;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_59;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end String_Transform_Parse_0;

   


function Cat_String_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Concat_String_Lit
is
   use Bare_Concat_String_Lit_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_69 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_105 :
            Token_Index
               := No_Token_Index;
      Defer_Res_105 :
            Bare_String_Lit
               := No_Bare_Turkixir_Node;
      Lst_Cpos_8 :
            Token_Index
               := No_Token_Index;
      Tmp_List_8 :
            Free_Parse_List;
      Defer_Pos_106 :
            Token_Index
               := No_Token_Index;
      Defer_Res_106 :
            Bare_String_Lit
               := No_Bare_Turkixir_Node;
      List_Pos_8 :
            Token_Index
               := No_Token_Index;
      List_Res_8 :
            Bare_String_Lit_List
               := No_Bare_Turkixir_Node;
      Transform_Res_58 :
            Bare_Concat_String_Lit
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Cat_String_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_58 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_58;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_58;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_69 := Pos;



Defer_Res_105 :=
   String_Transform_Parse_0 (Parser, Row_Pos_69);
Defer_Pos_105 := Parser.Current_Pos;



if Defer_Pos_105 /= No_Token_Index then

   Row_Pos_69 := Defer_Pos_105;

else
   Row_Pos_69 := No_Token_Index;
   goto Exit_Row_69_0;

end if;


--  Start list_code

    List_Pos_8 := No_Token_Index;



Lst_Cpos_8 := Row_Pos_69;
Tmp_List_8 := Get_Parse_List (Parser);

loop
   
Defer_Res_106 :=
   String_Transform_Parse_0 (Parser, Lst_Cpos_8);
Defer_Pos_106 := Parser.Current_Pos;


   exit when Defer_Pos_106 = No_Token_Index;

   List_Pos_8 := Defer_Pos_106;
   Lst_Cpos_8 := List_Pos_8;

   Tmp_List_8.Nodes.Append (Defer_Res_106);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_8.Nodes.Length;
begin
   List_Res_8 :=
      Allocate_String_Lit_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_69;
      Token_End := (if Lst_Cpos_8 = Row_Pos_69
                    then Row_Pos_69
                    else Lst_Cpos_8 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_69, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_8,
      Kind              => Turkixir_String_Lit_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_8,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_8.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_8.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_8);

--  End list_code



if List_Pos_8 /= No_Token_Index then

   Row_Pos_69 := List_Pos_8;

else
   Row_Pos_69 := No_Token_Index;
   goto Exit_Row_69_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_69_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_69 /= No_Token_Index then

   Transform_Res_58 := Allocate_Concat_String_Lit (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_58,
      Kind => Turkixir_Concat_String_Lit,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_69 = Pos
                            then No_Token_Index
                            else Row_Pos_69 - 1));

      Initialize_Fields_For_Concat_String_Lit
        (Self => Transform_Res_58, Concat_String_Lit_F_First_Str => Defer_Res_105, Concat_String_Lit_F_Subsequent_Str => List_Res_8);

         if Defer_Res_105 /= null and then Is_Incomplete (Defer_Res_105) then
            Transform_Res_58.Last_Attempted_Child := 0;
         elsif Defer_Res_105 /= null and then not Is_Ghost (Defer_Res_105) then
            Transform_Res_58.Last_Attempted_Child := -1;
         end if;
         if List_Res_8 /= null and then Is_Incomplete (List_Res_8) then
            Transform_Res_58.Last_Attempted_Child := 0;
         elsif List_Res_8 /= null and then not Is_Ghost (List_Res_8) then
            Transform_Res_58.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Cat_String_Transform_Parse_0_Memo,
      Row_Pos_69 /= No_Token_Index,
      Transform_Res_58,
      Pos,
      Row_Pos_69);


   Parser.Current_Pos := Row_Pos_69;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_58;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Cat_String_Transform_Parse_0;

   


function Atom_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_51 :
            Token_Index
               := No_Token_Index;
      Token_Pos_67 :
            Token_Index
               := No_Token_Index;
      Token_Res_67 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_74 :
            Token_Index
               := No_Token_Index;
      Defer_Res_74 :
            Bare_Yield_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_68 :
            Token_Index
               := No_Token_Index;
      Token_Res_68 :
            Token_Index
               := No_Token_Index;
      Row_Pos_52 :
            Token_Index
               := No_Token_Index;
      Token_Pos_69 :
            Token_Index
               := No_Token_Index;
      Token_Res_69 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_75 :
            Token_Index
               := No_Token_Index;
      Defer_Res_75 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_76 :
            Token_Index
               := No_Token_Index;
      Defer_Res_76 :
            Bare_Comp_ForL
               := No_Bare_Turkixir_Node;
      Token_Pos_70 :
            Token_Index
               := No_Token_Index;
      Token_Res_70 :
            Token_Index
               := No_Token_Index;
      Transform_Res_44 :
            Bare_List_Gen
               := No_Bare_Turkixir_Node;
      Row_Pos_53 :
            Token_Index
               := No_Token_Index;
      Token_Pos_71 :
            Token_Index
               := No_Token_Index;
      Token_Res_71 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_77 :
            Token_Index
               := No_Token_Index;
      Defer_Res_77 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Token_Pos_72 :
            Token_Index
               := No_Token_Index;
      Token_Res_72 :
            Token_Index
               := No_Token_Index;
      Transform_Res_45 :
            Bare_Tuple_Lit
               := No_Bare_Turkixir_Node;
      Row_Pos_54 :
            Token_Index
               := No_Token_Index;
      Token_Pos_73 :
            Token_Index
               := No_Token_Index;
      Token_Res_73 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_78 :
            Token_Index
               := No_Token_Index;
      Defer_Res_78 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_79 :
            Token_Index
               := No_Token_Index;
      Defer_Res_79 :
            Bare_Comp_ForL
               := No_Bare_Turkixir_Node;
      Token_Pos_74 :
            Token_Index
               := No_Token_Index;
      Token_Res_74 :
            Token_Index
               := No_Token_Index;
      Transform_Res_46 :
            Bare_List_Comp
               := No_Bare_Turkixir_Node;
      Row_Pos_55 :
            Token_Index
               := No_Token_Index;
      Token_Pos_75 :
            Token_Index
               := No_Token_Index;
      Token_Res_75 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_80 :
            Token_Index
               := No_Token_Index;
      Defer_Res_80 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Token_Pos_76 :
            Token_Index
               := No_Token_Index;
      Token_Res_76 :
            Token_Index
               := No_Token_Index;
      Transform_Res_47 :
            Bare_List_Lit
               := No_Bare_Turkixir_Node;
      Row_Pos_56 :
            Token_Index
               := No_Token_Index;
      Token_Pos_77 :
            Token_Index
               := No_Token_Index;
      Token_Res_77 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_81 :
            Token_Index
               := No_Token_Index;
      Defer_Res_81 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_82 :
            Token_Index
               := No_Token_Index;
      Defer_Res_82 :
            Bare_Comp_For
               := No_Bare_Turkixir_Node;
      Token_Pos_78 :
            Token_Index
               := No_Token_Index;
      Token_Res_78 :
            Token_Index
               := No_Token_Index;
      Transform_Res_48 :
            Bare_Set_Comp
               := No_Bare_Turkixir_Node;
      Defer_Pos_83 :
            Token_Index
               := No_Token_Index;
      Defer_Res_83 :
            Bare_Set_Lit
               := No_Bare_Turkixir_Node;
      Row_Pos_57 :
            Token_Index
               := No_Token_Index;
      Token_Pos_79 :
            Token_Index
               := No_Token_Index;
      Token_Res_79 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_84 :
            Token_Index
               := No_Token_Index;
      Defer_Res_84 :
            Bare_Dict_Assoc
               := No_Bare_Turkixir_Node;
      Defer_Pos_85 :
            Token_Index
               := No_Token_Index;
      Defer_Res_85 :
            Bare_Comp_For
               := No_Bare_Turkixir_Node;
      Token_Pos_80 :
            Token_Index
               := No_Token_Index;
      Token_Res_80 :
            Token_Index
               := No_Token_Index;
      Transform_Res_49 :
            Bare_Dict_Comp
               := No_Bare_Turkixir_Node;
      Row_Pos_58 :
            Token_Index
               := No_Token_Index;
      Token_Pos_81 :
            Token_Index
               := No_Token_Index;
      Token_Res_81 :
            Token_Index
               := No_Token_Index;
      Row_Pos_59 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_5 :
            Token_Index
               := No_Token_Index;
      Tmp_List_5 :
            Free_Parse_List;
      Defer_Pos_86 :
            Token_Index
               := No_Token_Index;
      Defer_Res_86 :
            Bare_Dict_Assoc
               := No_Bare_Turkixir_Node;
      Token_Pos_82 :
            Token_Index
               := No_Token_Index;
      Token_Res_82 :
            Token_Index
               := No_Token_Index;
      List_Pos_5 :
            Token_Index
               := No_Token_Index;
      List_Res_5 :
            Bare_Dict_Assoc_List
               := No_Bare_Turkixir_Node;
      Token_Pos_83 :
            Token_Index
               := No_Token_Index;
      Token_Res_83 :
            Token_Index
               := No_Token_Index;
      Token_Pos_84 :
            Token_Index
               := No_Token_Index;
      Token_Res_84 :
            Token_Index
               := No_Token_Index;
      Transform_Res_50 :
            Bare_Dict_Lit
               := No_Bare_Turkixir_Node;
      Row_Pos_60 :
            Token_Index
               := No_Token_Index;
      Token_Pos_85 :
            Token_Index
               := No_Token_Index;
      Token_Res_85 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_87 :
            Token_Index
               := No_Token_Index;
      Defer_Res_87 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Token_Pos_86 :
            Token_Index
               := No_Token_Index;
      Token_Res_86 :
            Token_Index
               := No_Token_Index;
      Transform_Res_51 :
            Bare_Inline_Eval
               := No_Bare_Turkixir_Node;
      Defer_Pos_88 :
            Token_Index
               := No_Token_Index;
      Defer_Res_88 :
            Bare_Id
               := No_Bare_Turkixir_Node;
      Defer_Pos_89 :
            Token_Index
               := No_Token_Index;
      Defer_Res_89 :
            Bare_Number_Lit
               := No_Bare_Turkixir_Node;
      Defer_Pos_90 :
            Token_Index
               := No_Token_Index;
      Defer_Res_90 :
            Bare_Concat_String_Lit
               := No_Bare_Turkixir_Node;
      Defer_Pos_91 :
            Token_Index
               := No_Token_Index;
      Defer_Res_91 :
            Bare_String_Lit
               := No_Bare_Turkixir_Node;
      Or_Pos_23 :
            Token_Index
               := No_Token_Index;
      Or_Res_23 :
            Bare_Expr
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Atom_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_23 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_23;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_23;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_23 := No_Token_Index;
Or_Res_23 := No_Bare_Turkixir_Node;
    
--  Start row_code

Row_Pos_51 := Pos;



--  Start tok_code

Token_Res_67 := Row_Pos_51;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_67));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_L_Par)
   then
       Token_Pos_67 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_51 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_51,
             Expected_Token_Id => Turkixir_T_T_L_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_67 := Row_Pos_51 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_67 /= No_Token_Index then

   Row_Pos_51 := Token_Pos_67;

else
   Row_Pos_51 := No_Token_Index;
   goto Exit_Row_51_0;

end if;


Defer_Res_74 :=
   Yield_Expr_Transform_Parse_0 (Parser, Row_Pos_51);
Defer_Pos_74 := Parser.Current_Pos;



if Defer_Pos_74 /= No_Token_Index then

   Row_Pos_51 := Defer_Pos_74;

else
   Row_Pos_51 := No_Token_Index;
   goto Exit_Row_51_0;

end if;


--  Start tok_code

Token_Res_68 := Row_Pos_51;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_68));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_R_Par)
   then
       Token_Pos_68 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_51 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_51,
             Expected_Token_Id => Turkixir_T_T_R_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_68 := Row_Pos_51 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_68 /= No_Token_Index then

   Row_Pos_51 := Token_Pos_68;

else
   Row_Pos_51 := No_Token_Index;
   goto Exit_Row_51_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_51_0>>
pragma Warnings (On, "referenced");

--  End row_code

    if Row_Pos_51 /= No_Token_Index then
        Or_Pos_23 := Row_Pos_51;
        Or_Res_23 := Defer_Res_74;
        goto Exit_Or_23;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_52 := Pos;



--  Start tok_code

Token_Res_69 := Row_Pos_52;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_69));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_L_Par)
   then
       Token_Pos_69 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_52 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_52,
             Expected_Token_Id => Turkixir_T_T_L_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_69 := Row_Pos_52 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_69 /= No_Token_Index then

   Row_Pos_52 := Token_Pos_69;

else
   Row_Pos_52 := No_Token_Index;
   goto Exit_Row_54_0;

end if;


Defer_Res_75 :=
   Test_Or_Parse_0 (Parser, Row_Pos_52);
Defer_Pos_75 := Parser.Current_Pos;



if Defer_Pos_75 /= No_Token_Index then

   Row_Pos_52 := Defer_Pos_75;

else
   Row_Pos_52 := No_Token_Index;
   goto Exit_Row_54_0;

end if;


Defer_Res_76 :=
   List_For_Transform_Parse_0 (Parser, Row_Pos_52);
Defer_Pos_76 := Parser.Current_Pos;



if Defer_Pos_76 /= No_Token_Index then

   Row_Pos_52 := Defer_Pos_76;

else
   Row_Pos_52 := No_Token_Index;
   goto Exit_Row_54_0;

end if;


--  Start tok_code

Token_Res_70 := Row_Pos_52;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_70));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_R_Par)
   then
       Token_Pos_70 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_52 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_52,
             Expected_Token_Id => Turkixir_T_T_R_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_70 := Row_Pos_52 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_70 /= No_Token_Index then

   Row_Pos_52 := Token_Pos_70;

else
   Row_Pos_52 := No_Token_Index;
   goto Exit_Row_54_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_54_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_52 /= No_Token_Index then

   Transform_Res_44 := Allocate_List_Gen (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_44,
      Kind => Turkixir_List_Gen,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_52 = Pos
                            then No_Token_Index
                            else Row_Pos_52 - 1));

      Initialize_Fields_For_List_Gen
        (Self => Transform_Res_44, List_Gen_F_Expr => Defer_Res_75, List_Gen_F_Comprehension => Defer_Res_76);

         if Defer_Res_75 /= null and then Is_Incomplete (Defer_Res_75) then
            Transform_Res_44.Last_Attempted_Child := 0;
         elsif Defer_Res_75 /= null and then not Is_Ghost (Defer_Res_75) then
            Transform_Res_44.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_76 /= null and then Is_Incomplete (Defer_Res_76) then
            Transform_Res_44.Last_Attempted_Child := 0;
         elsif Defer_Res_76 /= null and then not Is_Ghost (Defer_Res_76) then
            Transform_Res_44.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_52 /= No_Token_Index then
        Or_Pos_23 := Row_Pos_52;
        Or_Res_23 := Transform_Res_44;
        goto Exit_Or_23;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_53 := Pos;



--  Start tok_code

Token_Res_71 := Row_Pos_53;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_71));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_L_Par)
   then
       Token_Pos_71 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_53 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_53,
             Expected_Token_Id => Turkixir_T_T_L_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_71 := Row_Pos_53 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_71 /= No_Token_Index then

   Row_Pos_53 := Token_Pos_71;

else
   Row_Pos_53 := No_Token_Index;
   goto Exit_Row_57_0;

end if;


--  Start opt_code




Defer_Res_77 :=
   Test_List_Extract_Parse_0 (Parser, Row_Pos_53);
Defer_Pos_77 := Parser.Current_Pos;


if Defer_Pos_77 = No_Token_Index then

        Defer_Res_77 :=
           Allocate_Expr_List (Parser.Mem_Pool);
         Initialize
           (Self              => Defer_Res_77,
            Kind              => Turkixir_Expr_List,
            Unit              => Parser.Unit,
            Token_Start_Index => Row_Pos_53 - 1,
            Token_End_Index   => No_Token_Index);
         Initialize_List
           (Self   => Defer_Res_77,
            Parser => Parser,
            Count  => 0);


    Defer_Pos_77 := Row_Pos_53;


end if;

--  End opt_code



if Defer_Pos_77 /= No_Token_Index then

   Row_Pos_53 := Defer_Pos_77;

else
   Row_Pos_53 := No_Token_Index;
   goto Exit_Row_57_0;

end if;


--  Start tok_code

Token_Res_72 := Row_Pos_53;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_72));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_R_Par)
   then
       Token_Pos_72 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_53 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_53,
             Expected_Token_Id => Turkixir_T_T_R_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_72 := Row_Pos_53 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_72 /= No_Token_Index then

   Row_Pos_53 := Token_Pos_72;

else
   Row_Pos_53 := No_Token_Index;
   goto Exit_Row_57_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_57_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_53 /= No_Token_Index then

   Transform_Res_45 := Allocate_Tuple_Lit (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_45,
      Kind => Turkixir_Tuple_Lit,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_53 = Pos
                            then No_Token_Index
                            else Row_Pos_53 - 1));

      Initialize_Fields_For_Tuple_Lit
        (Self => Transform_Res_45, Tuple_Lit_F_Exprs => Defer_Res_77);

         if Defer_Res_77 /= null and then Is_Incomplete (Defer_Res_77) then
            Transform_Res_45.Last_Attempted_Child := 0;
         elsif Defer_Res_77 /= null and then not Is_Ghost (Defer_Res_77) then
            Transform_Res_45.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_53 /= No_Token_Index then
        Or_Pos_23 := Row_Pos_53;
        Or_Res_23 := Transform_Res_45;
        goto Exit_Or_23;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_54 := Pos;



--  Start tok_code

Token_Res_73 := Row_Pos_54;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_73));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Lbrack)
   then
       Token_Pos_73 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_54 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_54,
             Expected_Token_Id => Turkixir_T_T__Lbrack,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_73 := Row_Pos_54 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_73 /= No_Token_Index then

   Row_Pos_54 := Token_Pos_73;

else
   Row_Pos_54 := No_Token_Index;
   goto Exit_Row_58_0;

end if;


Defer_Res_78 :=
   Test_Or_Parse_0 (Parser, Row_Pos_54);
Defer_Pos_78 := Parser.Current_Pos;



if Defer_Pos_78 /= No_Token_Index then

   Row_Pos_54 := Defer_Pos_78;

else
   Row_Pos_54 := No_Token_Index;
   goto Exit_Row_58_0;

end if;


Defer_Res_79 :=
   List_For_Transform_Parse_0 (Parser, Row_Pos_54);
Defer_Pos_79 := Parser.Current_Pos;



if Defer_Pos_79 /= No_Token_Index then

   Row_Pos_54 := Defer_Pos_79;

else
   Row_Pos_54 := No_Token_Index;
   goto Exit_Row_58_0;

end if;


--  Start tok_code

Token_Res_74 := Row_Pos_54;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_74));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Rbrack)
   then
       Token_Pos_74 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_54 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_54,
             Expected_Token_Id => Turkixir_T_T__Rbrack,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_74 := Row_Pos_54 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_74 /= No_Token_Index then

   Row_Pos_54 := Token_Pos_74;

else
   Row_Pos_54 := No_Token_Index;
   goto Exit_Row_58_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_58_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_54 /= No_Token_Index then

   Transform_Res_46 := Allocate_List_Comp (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_46,
      Kind => Turkixir_List_Comp,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_54 = Pos
                            then No_Token_Index
                            else Row_Pos_54 - 1));

      Initialize_Fields_For_List_Comp
        (Self => Transform_Res_46, List_Comp_F_Expr => Defer_Res_78, List_Comp_F_Comprehension => Defer_Res_79);

         if Defer_Res_78 /= null and then Is_Incomplete (Defer_Res_78) then
            Transform_Res_46.Last_Attempted_Child := 0;
         elsif Defer_Res_78 /= null and then not Is_Ghost (Defer_Res_78) then
            Transform_Res_46.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_79 /= null and then Is_Incomplete (Defer_Res_79) then
            Transform_Res_46.Last_Attempted_Child := 0;
         elsif Defer_Res_79 /= null and then not Is_Ghost (Defer_Res_79) then
            Transform_Res_46.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_54 /= No_Token_Index then
        Or_Pos_23 := Row_Pos_54;
        Or_Res_23 := Transform_Res_46;
        goto Exit_Or_23;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_55 := Pos;



--  Start tok_code

Token_Res_75 := Row_Pos_55;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_75));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Lbrack)
   then
       Token_Pos_75 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_55 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_55,
             Expected_Token_Id => Turkixir_T_T__Lbrack,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_75 := Row_Pos_55 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_75 /= No_Token_Index then

   Row_Pos_55 := Token_Pos_75;

else
   Row_Pos_55 := No_Token_Index;
   goto Exit_Row_59_0;

end if;


Defer_Res_80 :=
   Empty_Test_List_Extract_Parse_0 (Parser, Row_Pos_55);
Defer_Pos_80 := Parser.Current_Pos;



if Defer_Pos_80 /= No_Token_Index then

   Row_Pos_55 := Defer_Pos_80;

else
   Row_Pos_55 := No_Token_Index;
   goto Exit_Row_59_0;

end if;


--  Start tok_code

Token_Res_76 := Row_Pos_55;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_76));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Rbrack)
   then
       Token_Pos_76 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_55 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_55,
             Expected_Token_Id => Turkixir_T_T__Rbrack,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_76 := Row_Pos_55 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_76 /= No_Token_Index then

   Row_Pos_55 := Token_Pos_76;

else
   Row_Pos_55 := No_Token_Index;
   goto Exit_Row_59_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_59_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_55 /= No_Token_Index then

   Transform_Res_47 := Allocate_List_Lit (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_47,
      Kind => Turkixir_List_Lit,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_55 = Pos
                            then No_Token_Index
                            else Row_Pos_55 - 1));

      Initialize_Fields_For_List_Lit
        (Self => Transform_Res_47, List_Lit_F_Exprs => Defer_Res_80);

         if Defer_Res_80 /= null and then Is_Incomplete (Defer_Res_80) then
            Transform_Res_47.Last_Attempted_Child := 0;
         elsif Defer_Res_80 /= null and then not Is_Ghost (Defer_Res_80) then
            Transform_Res_47.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_55 /= No_Token_Index then
        Or_Pos_23 := Row_Pos_55;
        Or_Res_23 := Transform_Res_47;
        goto Exit_Or_23;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_56 := Pos;



--  Start tok_code

Token_Res_77 := Row_Pos_56;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_77));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_L_Curl)
   then
       Token_Pos_77 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_56 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_56,
             Expected_Token_Id => Turkixir_T_T_L_Curl,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_77 := Row_Pos_56 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_77 /= No_Token_Index then

   Row_Pos_56 := Token_Pos_77;

else
   Row_Pos_56 := No_Token_Index;
   goto Exit_Row_61_0;

end if;


Defer_Res_81 :=
   Test_Or_Parse_0 (Parser, Row_Pos_56);
Defer_Pos_81 := Parser.Current_Pos;



if Defer_Pos_81 /= No_Token_Index then

   Row_Pos_56 := Defer_Pos_81;

else
   Row_Pos_56 := No_Token_Index;
   goto Exit_Row_61_0;

end if;


Defer_Res_82 :=
   Comp_For_Transform_Parse_0 (Parser, Row_Pos_56);
Defer_Pos_82 := Parser.Current_Pos;



if Defer_Pos_82 /= No_Token_Index then

   Row_Pos_56 := Defer_Pos_82;

else
   Row_Pos_56 := No_Token_Index;
   goto Exit_Row_61_0;

end if;


--  Start tok_code

Token_Res_78 := Row_Pos_56;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_78));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Rcurl)
   then
       Token_Pos_78 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_56 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_56,
             Expected_Token_Id => Turkixir_T_T__Rcurl,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_78 := Row_Pos_56 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_78 /= No_Token_Index then

   Row_Pos_56 := Token_Pos_78;

else
   Row_Pos_56 := No_Token_Index;
   goto Exit_Row_61_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_61_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_56 /= No_Token_Index then

   Transform_Res_48 := Allocate_Set_Comp (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_48,
      Kind => Turkixir_Set_Comp,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_56 = Pos
                            then No_Token_Index
                            else Row_Pos_56 - 1));

      Initialize_Fields_For_Set_Comp
        (Self => Transform_Res_48, Set_Comp_F_Expr => Defer_Res_81, Set_Comp_F_Comprehension => Defer_Res_82);

         if Defer_Res_81 /= null and then Is_Incomplete (Defer_Res_81) then
            Transform_Res_48.Last_Attempted_Child := 0;
         elsif Defer_Res_81 /= null and then not Is_Ghost (Defer_Res_81) then
            Transform_Res_48.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_82 /= null and then Is_Incomplete (Defer_Res_82) then
            Transform_Res_48.Last_Attempted_Child := 0;
         elsif Defer_Res_82 /= null and then not Is_Ghost (Defer_Res_82) then
            Transform_Res_48.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_56 /= No_Token_Index then
        Or_Pos_23 := Row_Pos_56;
        Or_Res_23 := Transform_Res_48;
        goto Exit_Or_23;
    end if;
    
Defer_Res_83 :=
   Set_Lit_Transform_Parse_0 (Parser, Pos);
Defer_Pos_83 := Parser.Current_Pos;

    if Defer_Pos_83 /= No_Token_Index then
        Or_Pos_23 := Defer_Pos_83;
        Or_Res_23 := Defer_Res_83;
        goto Exit_Or_23;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_57 := Pos;



--  Start tok_code

Token_Res_79 := Row_Pos_57;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_79));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_L_Curl)
   then
       Token_Pos_79 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_57 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_57,
             Expected_Token_Id => Turkixir_T_T_L_Curl,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_79 := Row_Pos_57 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_79 /= No_Token_Index then

   Row_Pos_57 := Token_Pos_79;

else
   Row_Pos_57 := No_Token_Index;
   goto Exit_Row_63_0;

end if;


Defer_Res_84 :=
   Dict_Assoc_Transform_Parse_0 (Parser, Row_Pos_57);
Defer_Pos_84 := Parser.Current_Pos;



if Defer_Pos_84 /= No_Token_Index then

   Row_Pos_57 := Defer_Pos_84;

else
   Row_Pos_57 := No_Token_Index;
   goto Exit_Row_63_0;

end if;


Defer_Res_85 :=
   Comp_For_Transform_Parse_0 (Parser, Row_Pos_57);
Defer_Pos_85 := Parser.Current_Pos;



if Defer_Pos_85 /= No_Token_Index then

   Row_Pos_57 := Defer_Pos_85;

else
   Row_Pos_57 := No_Token_Index;
   goto Exit_Row_63_0;

end if;


--  Start tok_code

Token_Res_80 := Row_Pos_57;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_80));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Rcurl)
   then
       Token_Pos_80 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_57 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_57,
             Expected_Token_Id => Turkixir_T_T__Rcurl,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_80 := Row_Pos_57 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_80 /= No_Token_Index then

   Row_Pos_57 := Token_Pos_80;

else
   Row_Pos_57 := No_Token_Index;
   goto Exit_Row_63_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_63_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_57 /= No_Token_Index then

   Transform_Res_49 := Allocate_Dict_Comp (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_49,
      Kind => Turkixir_Dict_Comp,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_57 = Pos
                            then No_Token_Index
                            else Row_Pos_57 - 1));

      Initialize_Fields_For_Dict_Comp
        (Self => Transform_Res_49, Dict_Comp_F_Assoc => Defer_Res_84, Dict_Comp_F_Comprehension => Defer_Res_85);

         if Defer_Res_84 /= null and then Is_Incomplete (Defer_Res_84) then
            Transform_Res_49.Last_Attempted_Child := 0;
         elsif Defer_Res_84 /= null and then not Is_Ghost (Defer_Res_84) then
            Transform_Res_49.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_85 /= null and then Is_Incomplete (Defer_Res_85) then
            Transform_Res_49.Last_Attempted_Child := 0;
         elsif Defer_Res_85 /= null and then not Is_Ghost (Defer_Res_85) then
            Transform_Res_49.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_57 /= No_Token_Index then
        Or_Pos_23 := Row_Pos_57;
        Or_Res_23 := Transform_Res_49;
        goto Exit_Or_23;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_58 := Pos;



--  Start tok_code

Token_Res_81 := Row_Pos_58;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_81));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_L_Curl)
   then
       Token_Pos_81 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_58 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_58,
             Expected_Token_Id => Turkixir_T_T_L_Curl,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_81 := Row_Pos_58 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_81 /= No_Token_Index then

   Row_Pos_58 := Token_Pos_81;

else
   Row_Pos_58 := No_Token_Index;
   goto Exit_Row_65_0;

end if;


--  Start row_code

Row_Pos_59 := Row_Pos_58;



--  Start list_code

    List_Pos_5 := No_Token_Index;



Lst_Cpos_5 := Row_Pos_59;
Tmp_List_5 := Get_Parse_List (Parser);

loop
   
Defer_Res_86 :=
   Dict_Assoc_Transform_Parse_0 (Parser, Lst_Cpos_5);
Defer_Pos_86 := Parser.Current_Pos;


   exit when Defer_Pos_86 = No_Token_Index;

   List_Pos_5 := Defer_Pos_86;
   Lst_Cpos_5 := List_Pos_5;

   Tmp_List_5.Nodes.Append (Defer_Res_86);

      
--  Start tok_code

Token_Res_82 := Lst_Cpos_5;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_82));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_82 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos_5 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos_5,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_82 := Lst_Cpos_5 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos_82 /= No_Token_Index then
          Lst_Cpos_5 := Token_Pos_82;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_5.Nodes.Length;
begin
   List_Res_5 :=
      Allocate_Dict_Assoc_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_59;
      Token_End := (if Lst_Cpos_5 = Row_Pos_59
                    then Row_Pos_59
                    else Lst_Cpos_5 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_59, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_5,
      Kind              => Turkixir_Dict_Assoc_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_5,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_5.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_5.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_5);

--  End list_code



if List_Pos_5 /= No_Token_Index then

   Row_Pos_59 := List_Pos_5;

else
   Row_Pos_59 := No_Token_Index;
   goto Exit_Row_66_0;

end if;


--  Start opt_code




--  Start tok_code

Token_Res_83 := Row_Pos_59;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_83));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_83 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_59 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_59,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_83 := Row_Pos_59 + 1;
   end if;
end;

--  End tok_code


if Token_Pos_83 = No_Token_Index then

        Token_Res_83 := No_Token_Index;


    Token_Pos_83 := Row_Pos_59;


end if;

--  End opt_code



if Token_Pos_83 /= No_Token_Index then

   Row_Pos_59 := Token_Pos_83;

else
   Row_Pos_59 := No_Token_Index;
   goto Exit_Row_66_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_66_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_59 /= No_Token_Index then

   Row_Pos_58 := Row_Pos_59;

else
   Row_Pos_58 := No_Token_Index;
   goto Exit_Row_65_0;

end if;


--  Start tok_code

Token_Res_84 := Row_Pos_58;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_84));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Rcurl)
   then
       Token_Pos_84 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_58 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_58,
             Expected_Token_Id => Turkixir_T_T__Rcurl,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_84 := Row_Pos_58 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_84 /= No_Token_Index then

   Row_Pos_58 := Token_Pos_84;

else
   Row_Pos_58 := No_Token_Index;
   goto Exit_Row_65_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_65_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_58 /= No_Token_Index then

   Transform_Res_50 := Allocate_Dict_Lit (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_50,
      Kind => Turkixir_Dict_Lit,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_58 = Pos
                            then No_Token_Index
                            else Row_Pos_58 - 1));

      Initialize_Fields_For_Dict_Lit
        (Self => Transform_Res_50, Dict_Lit_F_Assocs => List_Res_5);

         if List_Res_5 /= null and then Is_Incomplete (List_Res_5) then
            Transform_Res_50.Last_Attempted_Child := 0;
         elsif List_Res_5 /= null and then not Is_Ghost (List_Res_5) then
            Transform_Res_50.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_58 /= No_Token_Index then
        Or_Pos_23 := Row_Pos_58;
        Or_Res_23 := Transform_Res_50;
        goto Exit_Or_23;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_60 := Pos;



--  Start tok_code

Token_Res_85 := Row_Pos_60;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_85));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Backtick)
   then
       Token_Pos_85 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_60 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_60,
             Expected_Token_Id => Turkixir_T_T__Backtick,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_85 := Row_Pos_60 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_85 /= No_Token_Index then

   Row_Pos_60 := Token_Pos_85;

else
   Row_Pos_60 := No_Token_Index;
   goto Exit_Row_67_0;

end if;


Defer_Res_87 :=
   Test_List_Extract_Parse_0 (Parser, Row_Pos_60);
Defer_Pos_87 := Parser.Current_Pos;



if Defer_Pos_87 /= No_Token_Index then

   Row_Pos_60 := Defer_Pos_87;

else
   Row_Pos_60 := No_Token_Index;
   goto Exit_Row_67_0;

end if;


--  Start tok_code

Token_Res_86 := Row_Pos_60;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_86));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Backtick)
   then
       Token_Pos_86 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_60 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_60,
             Expected_Token_Id => Turkixir_T_T__Backtick,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_86 := Row_Pos_60 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_86 /= No_Token_Index then

   Row_Pos_60 := Token_Pos_86;

else
   Row_Pos_60 := No_Token_Index;
   goto Exit_Row_67_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_67_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_60 /= No_Token_Index then

   Transform_Res_51 := Allocate_Inline_Eval (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_51,
      Kind => Turkixir_Inline_Eval,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_60 = Pos
                            then No_Token_Index
                            else Row_Pos_60 - 1));

      Initialize_Fields_For_Inline_Eval
        (Self => Transform_Res_51, Inline_Eval_F_Exprs => Defer_Res_87);

         if Defer_Res_87 /= null and then Is_Incomplete (Defer_Res_87) then
            Transform_Res_51.Last_Attempted_Child := 0;
         elsif Defer_Res_87 /= null and then not Is_Ghost (Defer_Res_87) then
            Transform_Res_51.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_60 /= No_Token_Index then
        Or_Pos_23 := Row_Pos_60;
        Or_Res_23 := Transform_Res_51;
        goto Exit_Or_23;
    end if;
    
Defer_Res_88 :=
   Name_Transform_Parse_0 (Parser, Pos);
Defer_Pos_88 := Parser.Current_Pos;

    if Defer_Pos_88 /= No_Token_Index then
        Or_Pos_23 := Defer_Pos_88;
        Or_Res_23 := Defer_Res_88;
        goto Exit_Or_23;
    end if;
    
Defer_Res_89 :=
   Number_Transform_Parse_0 (Parser, Pos);
Defer_Pos_89 := Parser.Current_Pos;

    if Defer_Pos_89 /= No_Token_Index then
        Or_Pos_23 := Defer_Pos_89;
        Or_Res_23 := Defer_Res_89;
        goto Exit_Or_23;
    end if;
    
Defer_Res_90 :=
   Cat_String_Transform_Parse_0 (Parser, Pos);
Defer_Pos_90 := Parser.Current_Pos;

    if Defer_Pos_90 /= No_Token_Index then
        Or_Pos_23 := Defer_Pos_90;
        Or_Res_23 := Defer_Res_90;
        goto Exit_Or_23;
    end if;
    
Defer_Res_91 :=
   String_Transform_Parse_0 (Parser, Pos);
Defer_Pos_91 := Parser.Current_Pos;

    if Defer_Pos_91 /= No_Token_Index then
        Or_Pos_23 := Defer_Pos_91;
        Or_Res_23 := Defer_Res_91;
        goto Exit_Or_23;
    end if;
<<Exit_Or_23>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Atom_Or_Parse_0_Memo,
      Or_Pos_23 /= No_Token_Index,
      Or_Res_23,
      Pos,
      Or_Pos_23);


   Parser.Current_Pos := Or_Pos_23;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_23;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Atom_Or_Parse_0;

   


function Atom_Expr_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_10 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_17 :
            Token_Index
               := No_Token_Index;
      Defer_Res_17 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_13 :
            Token_Index
               := No_Token_Index;
      Token_Res_13 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_18 :
            Token_Index
               := No_Token_Index;
      Defer_Res_18 :
            Bare_Id
               := No_Bare_Turkixir_Node;
      Transform_Res_10 :
            Bare_Dotted_Name
               := No_Bare_Turkixir_Node;
      Row_Pos_11 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_19 :
            Token_Index
               := No_Token_Index;
      Defer_Res_19 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_14 :
            Token_Index
               := No_Token_Index;
      Token_Res_14 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_20 :
            Token_Index
               := No_Token_Index;
      Defer_Res_20 :
            Bare_Arg_List
               := No_Bare_Turkixir_Node;
      Token_Pos_15 :
            Token_Index
               := No_Token_Index;
      Token_Res_15 :
            Token_Index
               := No_Token_Index;
      Transform_Res_11 :
            Bare_Call_Expr
               := No_Bare_Turkixir_Node;
      Row_Pos_12 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_21 :
            Token_Index
               := No_Token_Index;
      Defer_Res_21 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_16 :
            Token_Index
               := No_Token_Index;
      Token_Res_16 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_22 :
            Token_Index
               := No_Token_Index;
      Defer_Res_22 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Token_Pos_17 :
            Token_Index
               := No_Token_Index;
      Token_Res_17 :
            Token_Index
               := No_Token_Index;
      Transform_Res_12 :
            Bare_Subscript_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_23 :
            Token_Index
               := No_Token_Index;
      Defer_Res_23 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_10 :
            Token_Index
               := No_Token_Index;
      Or_Res_10 :
            Bare_Expr
               := No_Bare_Turkixir_Node;

      Mem_Pos : Token_Index := Pos;
      Mem_Res : Bare_Expr := No_Bare_Turkixir_Node;

   M : Memo_Entry := Get (Parser.Private_Part.Atom_Expr_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_10 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_10;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_10;
   end if;

       Set (Parser.Private_Part.Atom_Expr_Or_Parse_0_Memo, False, Or_Res_10, Pos, Mem_Pos);

       <<Try_Again>>



   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_10 := No_Token_Index;
Or_Res_10 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_10 := Pos;



Defer_Res_17 :=
   Atom_Expr_Or_Parse_0 (Parser, Row_Pos_10);
Defer_Pos_17 := Parser.Current_Pos;



if Defer_Pos_17 /= No_Token_Index then

   Row_Pos_10 := Defer_Pos_17;

else
   Row_Pos_10 := No_Token_Index;
   goto Exit_Row_10_0;

end if;


--  Start tok_code

Token_Res_13 := Row_Pos_10;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_13));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Dot)
   then
       Token_Pos_13 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_10 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_10,
             Expected_Token_Id => Turkixir_T_T__Dot,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_13 := Row_Pos_10 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_13 /= No_Token_Index then

   Row_Pos_10 := Token_Pos_13;

else
   Row_Pos_10 := No_Token_Index;
   goto Exit_Row_10_0;

end if;


Defer_Res_18 :=
   Name_Transform_Parse_0 (Parser, Row_Pos_10);
Defer_Pos_18 := Parser.Current_Pos;



if Defer_Pos_18 /= No_Token_Index then

   Row_Pos_10 := Defer_Pos_18;

else
   Row_Pos_10 := No_Token_Index;
   goto Exit_Row_10_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_10_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_10 /= No_Token_Index then

   Transform_Res_10 := Allocate_Dotted_Name (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_10,
      Kind => Turkixir_Dotted_Name,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_10 = Pos
                            then No_Token_Index
                            else Row_Pos_10 - 1));

      Initialize_Fields_For_Dotted_Name
        (Self => Transform_Res_10, Dotted_Name_F_Prefix => Defer_Res_17, Dotted_Name_F_Suffix => Defer_Res_18);

         if Defer_Res_17 /= null and then Is_Incomplete (Defer_Res_17) then
            Transform_Res_10.Last_Attempted_Child := 0;
         elsif Defer_Res_17 /= null and then not Is_Ghost (Defer_Res_17) then
            Transform_Res_10.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_18 /= null and then Is_Incomplete (Defer_Res_18) then
            Transform_Res_10.Last_Attempted_Child := 0;
         elsif Defer_Res_18 /= null and then not Is_Ghost (Defer_Res_18) then
            Transform_Res_10.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_10 /= No_Token_Index then
        Or_Pos_10 := Row_Pos_10;
        Or_Res_10 := Transform_Res_10;
        goto Exit_Or_10;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_11 := Pos;



Defer_Res_19 :=
   Atom_Expr_Or_Parse_0 (Parser, Row_Pos_11);
Defer_Pos_19 := Parser.Current_Pos;



if Defer_Pos_19 /= No_Token_Index then

   Row_Pos_11 := Defer_Pos_19;

else
   Row_Pos_11 := No_Token_Index;
   goto Exit_Row_12_0;

end if;


--  Start tok_code

Token_Res_14 := Row_Pos_11;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_14));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_L_Par)
   then
       Token_Pos_14 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_11 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_11,
             Expected_Token_Id => Turkixir_T_T_L_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_14 := Row_Pos_11 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_14 /= No_Token_Index then

   Row_Pos_11 := Token_Pos_14;

else
   Row_Pos_11 := No_Token_Index;
   goto Exit_Row_12_0;

end if;


Defer_Res_20 :=
   Arg_List_Extract_Parse_1 (Parser, Row_Pos_11);
Defer_Pos_20 := Parser.Current_Pos;



if Defer_Pos_20 /= No_Token_Index then

   Row_Pos_11 := Defer_Pos_20;

else
   Row_Pos_11 := No_Token_Index;
   goto Exit_Row_12_0;

end if;


--  Start tok_code

Token_Res_15 := Row_Pos_11;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_15));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_R_Par)
   then
       Token_Pos_15 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_11 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_11,
             Expected_Token_Id => Turkixir_T_T_R_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_15 := Row_Pos_11 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_15 /= No_Token_Index then

   Row_Pos_11 := Token_Pos_15;

else
   Row_Pos_11 := No_Token_Index;
   goto Exit_Row_12_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_12_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_11 /= No_Token_Index then

   Transform_Res_11 := Allocate_Call_Expr (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_11,
      Kind => Turkixir_Call_Expr,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_11 = Pos
                            then No_Token_Index
                            else Row_Pos_11 - 1));

      Initialize_Fields_For_Call_Expr
        (Self => Transform_Res_11, Call_Expr_F_Prefix => Defer_Res_19, Call_Expr_F_Suffix => Defer_Res_20);

         if Defer_Res_19 /= null and then Is_Incomplete (Defer_Res_19) then
            Transform_Res_11.Last_Attempted_Child := 0;
         elsif Defer_Res_19 /= null and then not Is_Ghost (Defer_Res_19) then
            Transform_Res_11.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_20 /= null and then Is_Incomplete (Defer_Res_20) then
            Transform_Res_11.Last_Attempted_Child := 0;
         elsif Defer_Res_20 /= null and then not Is_Ghost (Defer_Res_20) then
            Transform_Res_11.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_11 /= No_Token_Index then
        Or_Pos_10 := Row_Pos_11;
        Or_Res_10 := Transform_Res_11;
        goto Exit_Or_10;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_12 := Pos;



Defer_Res_21 :=
   Atom_Expr_Or_Parse_0 (Parser, Row_Pos_12);
Defer_Pos_21 := Parser.Current_Pos;



if Defer_Pos_21 /= No_Token_Index then

   Row_Pos_12 := Defer_Pos_21;

else
   Row_Pos_12 := No_Token_Index;
   goto Exit_Row_46_0;

end if;


--  Start tok_code

Token_Res_16 := Row_Pos_12;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_16));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Lbrack)
   then
       Token_Pos_16 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_12 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_12,
             Expected_Token_Id => Turkixir_T_T__Lbrack,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_16 := Row_Pos_12 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_16 /= No_Token_Index then

   Row_Pos_12 := Token_Pos_16;

else
   Row_Pos_12 := No_Token_Index;
   goto Exit_Row_46_0;

end if;


Defer_Res_22 :=
   Subscript_List_Extract_Parse_0 (Parser, Row_Pos_12);
Defer_Pos_22 := Parser.Current_Pos;



if Defer_Pos_22 /= No_Token_Index then

   Row_Pos_12 := Defer_Pos_22;

else
   Row_Pos_12 := No_Token_Index;
   goto Exit_Row_46_0;

end if;


--  Start tok_code

Token_Res_17 := Row_Pos_12;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_17));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Rbrack)
   then
       Token_Pos_17 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_12 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_12,
             Expected_Token_Id => Turkixir_T_T__Rbrack,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_17 := Row_Pos_12 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_17 /= No_Token_Index then

   Row_Pos_12 := Token_Pos_17;

else
   Row_Pos_12 := No_Token_Index;
   goto Exit_Row_46_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_46_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_12 /= No_Token_Index then

   Transform_Res_12 := Allocate_Subscript_Expr (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_12,
      Kind => Turkixir_Subscript_Expr,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_12 = Pos
                            then No_Token_Index
                            else Row_Pos_12 - 1));

      Initialize_Fields_For_Subscript_Expr
        (Self => Transform_Res_12, Subscript_Expr_F_Prefix => Defer_Res_21, Subscript_Expr_F_Suffix => Defer_Res_22);

         if Defer_Res_21 /= null and then Is_Incomplete (Defer_Res_21) then
            Transform_Res_12.Last_Attempted_Child := 0;
         elsif Defer_Res_21 /= null and then not Is_Ghost (Defer_Res_21) then
            Transform_Res_12.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_22 /= null and then Is_Incomplete (Defer_Res_22) then
            Transform_Res_12.Last_Attempted_Child := 0;
         elsif Defer_Res_22 /= null and then not Is_Ghost (Defer_Res_22) then
            Transform_Res_12.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_12 /= No_Token_Index then
        Or_Pos_10 := Row_Pos_12;
        Or_Res_10 := Transform_Res_12;
        goto Exit_Or_10;
    end if;
    
Defer_Res_23 :=
   Atom_Or_Parse_0 (Parser, Pos);
Defer_Pos_23 := Parser.Current_Pos;

    if Defer_Pos_23 /= No_Token_Index then
        Or_Pos_10 := Defer_Pos_23;
        Or_Res_10 := Defer_Res_23;
        goto Exit_Or_10;
    end if;
<<Exit_Or_10>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

      if Or_Pos_10 > Mem_Pos then
         Mem_Pos := Or_Pos_10;
         Mem_Res := Or_Res_10;
         Set
           (Parser.Private_Part.Atom_Expr_Or_Parse_0_Memo,
            Or_Pos_10 /= No_Token_Index,
            Or_Res_10,
            Pos,
            Or_Pos_10);
         goto Try_Again;

      elsif Mem_Pos > Pos then
         Or_Res_10 := Mem_Res;
         Or_Pos_10 := Mem_Pos;
         goto No_Memo;
      end if;

   Set
     (Parser.Private_Part.Atom_Expr_Or_Parse_0_Memo,
      Or_Pos_10 /= No_Token_Index,
      Or_Res_10,
      Pos,
      Or_Pos_10);

       <<No_Memo>>

   Parser.Current_Pos := Or_Pos_10;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_10;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Atom_Expr_Or_Parse_0;

   


function Power_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_9 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_14 :
            Token_Index
               := No_Token_Index;
      Defer_Res_14 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_12 :
            Token_Index
               := No_Token_Index;
      Token_Res_12 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_15 :
            Token_Index
               := No_Token_Index;
      Defer_Res_15 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_9 :
            Bare_Power
               := No_Bare_Turkixir_Node;
      Defer_Pos_16 :
            Token_Index
               := No_Token_Index;
      Defer_Res_16 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_9 :
            Token_Index
               := No_Token_Index;
      Or_Res_9 :
            Bare_Expr
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Power_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_9 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_9;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_9;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_9 := No_Token_Index;
Or_Res_9 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_9 := Pos;



Defer_Res_14 :=
   Atom_Expr_Or_Parse_0 (Parser, Row_Pos_9);
Defer_Pos_14 := Parser.Current_Pos;



if Defer_Pos_14 /= No_Token_Index then

   Row_Pos_9 := Defer_Pos_14;

else
   Row_Pos_9 := No_Token_Index;
   goto Exit_Row_9_0;

end if;


--  Start tok_code

Token_Res_12 := Row_Pos_9;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_12));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Power)
   then
       Token_Pos_12 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_9 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_9,
             Expected_Token_Id => Turkixir_T_T__Power,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_12 := Row_Pos_9 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_12 /= No_Token_Index then

   Row_Pos_9 := Token_Pos_12;

else
   Row_Pos_9 := No_Token_Index;
   goto Exit_Row_9_0;

end if;


Defer_Res_15 :=
   Factor_Or_Parse_1 (Parser, Row_Pos_9);
Defer_Pos_15 := Parser.Current_Pos;



if Defer_Pos_15 /= No_Token_Index then

   Row_Pos_9 := Defer_Pos_15;

else
   Row_Pos_9 := No_Token_Index;
   goto Exit_Row_9_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_9_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_9 /= No_Token_Index then

   Transform_Res_9 := Allocate_Power (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_9,
      Kind => Turkixir_Power,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_9 = Pos
                            then No_Token_Index
                            else Row_Pos_9 - 1));

      Initialize_Fields_For_Power
        (Self => Transform_Res_9, Power_F_Left => Defer_Res_14, Power_F_Right => Defer_Res_15);

         if Defer_Res_14 /= null and then Is_Incomplete (Defer_Res_14) then
            Transform_Res_9.Last_Attempted_Child := 0;
         elsif Defer_Res_14 /= null and then not Is_Ghost (Defer_Res_14) then
            Transform_Res_9.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_15 /= null and then Is_Incomplete (Defer_Res_15) then
            Transform_Res_9.Last_Attempted_Child := 0;
         elsif Defer_Res_15 /= null and then not Is_Ghost (Defer_Res_15) then
            Transform_Res_9.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_9 /= No_Token_Index then
        Or_Pos_9 := Row_Pos_9;
        Or_Res_9 := Transform_Res_9;
        goto Exit_Or_9;
    end if;
    
Defer_Res_16 :=
   Atom_Expr_Or_Parse_0 (Parser, Pos);
Defer_Pos_16 := Parser.Current_Pos;

    if Defer_Pos_16 /= No_Token_Index then
        Or_Pos_9 := Defer_Pos_16;
        Or_Res_9 := Defer_Res_16;
        goto Exit_Or_9;
    end if;
<<Exit_Or_9>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Power_Or_Parse_0_Memo,
      Or_Pos_9 /= No_Token_Index,
      Or_Res_9,
      Pos,
      Or_Pos_9);


   Parser.Current_Pos := Or_Pos_9;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_9;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Power_Or_Parse_0;

   


function Factor_Or_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_7 :
            Token_Index
               := No_Token_Index;
      Row_Pos_8 :
            Token_Index
               := No_Token_Index;
      Token_Pos_9 :
            Token_Index
               := No_Token_Index;
      Token_Res_9 :
            Token_Index
               := No_Token_Index;
      Token_Pos_10 :
            Token_Index
               := No_Token_Index;
      Token_Res_10 :
            Token_Index
               := No_Token_Index;
      Token_Pos_11 :
            Token_Index
               := No_Token_Index;
      Token_Res_11 :
            Token_Index
               := No_Token_Index;
      Or_Pos_7 :
            Token_Index
               := No_Token_Index;
      Or_Res_7 :
            Token_Index
               := No_Token_Index;
      Transform_Res_7 :
            Bare_Op
               := No_Bare_Turkixir_Node;
      Defer_Pos_12 :
            Token_Index
               := No_Token_Index;
      Defer_Res_12 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_8 :
            Bare_Factor
               := No_Bare_Turkixir_Node;
      Defer_Pos_13 :
            Token_Index
               := No_Token_Index;
      Defer_Res_13 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_8 :
            Token_Index
               := No_Token_Index;
      Or_Res_8 :
            Bare_Expr
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Factor_Or_Parse_1_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_8 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_8;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_8;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_8 := No_Token_Index;
Or_Res_8 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_7 := Pos;



--  Start transform_code


--  Start row_code

Row_Pos_8 := Row_Pos_7;



--  Start or_code

Or_Pos_7 := No_Token_Index;
Or_Res_7 := No_Token_Index;
    
--  Start tok_code

Token_Res_9 := Row_Pos_8;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_9));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Plus)
   then
       Token_Pos_9 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_8 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_8,
             Expected_Token_Id => Turkixir_T_T__Plus,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_9 := Row_Pos_8 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_9 /= No_Token_Index then
        Or_Pos_7 := Token_Pos_9;
        Or_Res_7 := Token_Res_9;
        goto Exit_Or_8;
    end if;
    
--  Start tok_code

Token_Res_10 := Row_Pos_8;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_10));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Minus)
   then
       Token_Pos_10 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_8 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_8,
             Expected_Token_Id => Turkixir_T_T__Minus,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_10 := Row_Pos_8 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_10 /= No_Token_Index then
        Or_Pos_7 := Token_Pos_10;
        Or_Res_7 := Token_Res_10;
        goto Exit_Or_8;
    end if;
    
--  Start tok_code

Token_Res_11 := Row_Pos_8;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_11));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Invert)
   then
       Token_Pos_11 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_8 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_8,
             Expected_Token_Id => Turkixir_T_T__Invert,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_11 := Row_Pos_8 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_11 /= No_Token_Index then
        Or_Pos_7 := Token_Pos_11;
        Or_Res_7 := Token_Res_11;
        goto Exit_Or_8;
    end if;
<<Exit_Or_8>>

--  End or_code



if Or_Pos_7 /= No_Token_Index then

   Row_Pos_8 := Or_Pos_7;

else
   Row_Pos_8 := No_Token_Index;
   goto Exit_Row_8_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_8_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_8 /= No_Token_Index then

   Transform_Res_7 := Allocate_Op (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_7,
      Kind => Turkixir_Op,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_7,
      Token_End_Index   => (if Row_Pos_8 = Row_Pos_7
                            then No_Token_Index
                            else Row_Pos_8 - 1));




end if;

--  End transform_code



if Row_Pos_8 /= No_Token_Index then

   Row_Pos_7 := Row_Pos_8;

else
   Row_Pos_7 := No_Token_Index;
   goto Exit_Row_7_0;

end if;


Defer_Res_12 :=
   Factor_Or_Parse_1 (Parser, Row_Pos_7);
Defer_Pos_12 := Parser.Current_Pos;



if Defer_Pos_12 /= No_Token_Index then

   Row_Pos_7 := Defer_Pos_12;

else
   Row_Pos_7 := No_Token_Index;
   goto Exit_Row_7_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_7_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_7 /= No_Token_Index then

   Transform_Res_8 := Allocate_Factor (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_8,
      Kind => Turkixir_Factor,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_7 = Pos
                            then No_Token_Index
                            else Row_Pos_7 - 1));

      Initialize_Fields_For_Factor
        (Self => Transform_Res_8, Factor_F_Op => Transform_Res_7, Factor_F_Expr => Defer_Res_12);

         if Transform_Res_7 /= null and then Is_Incomplete (Transform_Res_7) then
            Transform_Res_8.Last_Attempted_Child := 0;
         elsif Transform_Res_7 /= null and then not Is_Ghost (Transform_Res_7) then
            Transform_Res_8.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_12 /= null and then Is_Incomplete (Defer_Res_12) then
            Transform_Res_8.Last_Attempted_Child := 0;
         elsif Defer_Res_12 /= null and then not Is_Ghost (Defer_Res_12) then
            Transform_Res_8.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_7 /= No_Token_Index then
        Or_Pos_8 := Row_Pos_7;
        Or_Res_8 := Transform_Res_8;
        goto Exit_Or_7;
    end if;
    
Defer_Res_13 :=
   Power_Or_Parse_0 (Parser, Pos);
Defer_Pos_13 := Parser.Current_Pos;

    if Defer_Pos_13 /= No_Token_Index then
        Or_Pos_8 := Defer_Pos_13;
        Or_Res_8 := Defer_Res_13;
        goto Exit_Or_7;
    end if;
<<Exit_Or_7>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Factor_Or_Parse_1_Memo,
      Or_Pos_8 /= No_Token_Index,
      Or_Res_8,
      Pos,
      Or_Pos_8);


   Parser.Current_Pos := Or_Pos_8;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_8;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Factor_Or_Parse_1;

   


function Term_Or_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_5 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_9 :
            Token_Index
               := No_Token_Index;
      Defer_Res_9 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Row_Pos_6 :
            Token_Index
               := No_Token_Index;
      Token_Pos_5 :
            Token_Index
               := No_Token_Index;
      Token_Res_5 :
            Token_Index
               := No_Token_Index;
      Token_Pos_6 :
            Token_Index
               := No_Token_Index;
      Token_Res_6 :
            Token_Index
               := No_Token_Index;
      Token_Pos_7 :
            Token_Index
               := No_Token_Index;
      Token_Res_7 :
            Token_Index
               := No_Token_Index;
      Token_Pos_8 :
            Token_Index
               := No_Token_Index;
      Token_Res_8 :
            Token_Index
               := No_Token_Index;
      Or_Pos_5 :
            Token_Index
               := No_Token_Index;
      Or_Res_5 :
            Token_Index
               := No_Token_Index;
      Transform_Res_5 :
            Bare_Op
               := No_Bare_Turkixir_Node;
      Defer_Pos_10 :
            Token_Index
               := No_Token_Index;
      Defer_Res_10 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_6 :
            Bare_Term
               := No_Bare_Turkixir_Node;
      Defer_Pos_11 :
            Token_Index
               := No_Token_Index;
      Defer_Res_11 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_6 :
            Token_Index
               := No_Token_Index;
      Or_Res_6 :
            Bare_Expr
               := No_Bare_Turkixir_Node;

      Mem_Pos : Token_Index := Pos;
      Mem_Res : Bare_Expr := No_Bare_Turkixir_Node;

   M : Memo_Entry := Get (Parser.Private_Part.Term_Or_Parse_1_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_6 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_6;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_6;
   end if;

       Set (Parser.Private_Part.Term_Or_Parse_1_Memo, False, Or_Res_6, Pos, Mem_Pos);

       <<Try_Again>>



   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_6 := No_Token_Index;
Or_Res_6 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_5 := Pos;



Defer_Res_9 :=
   Term_Or_Parse_1 (Parser, Row_Pos_5);
Defer_Pos_9 := Parser.Current_Pos;



if Defer_Pos_9 /= No_Token_Index then

   Row_Pos_5 := Defer_Pos_9;

else
   Row_Pos_5 := No_Token_Index;
   goto Exit_Row_5_0;

end if;


--  Start transform_code


--  Start row_code

Row_Pos_6 := Row_Pos_5;



--  Start or_code

Or_Pos_5 := No_Token_Index;
Or_Res_5 := No_Token_Index;
    
--  Start tok_code

Token_Res_5 := Row_Pos_6;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_5));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Multiply)
   then
       Token_Pos_5 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_6 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_6,
             Expected_Token_Id => Turkixir_T_T__Multiply,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_5 := Row_Pos_6 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_5 /= No_Token_Index then
        Or_Pos_5 := Token_Pos_5;
        Or_Res_5 := Token_Res_5;
        goto Exit_Or_6;
    end if;
    
--  Start tok_code

Token_Res_6 := Row_Pos_6;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_6));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Divide)
   then
       Token_Pos_6 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_6 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_6,
             Expected_Token_Id => Turkixir_T_T__Divide,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_6 := Row_Pos_6 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_6 /= No_Token_Index then
        Or_Pos_5 := Token_Pos_6;
        Or_Res_5 := Token_Res_6;
        goto Exit_Or_6;
    end if;
    
--  Start tok_code

Token_Res_7 := Row_Pos_6;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_7));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Mod)
   then
       Token_Pos_7 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_6 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_6,
             Expected_Token_Id => Turkixir_T_T__Mod,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_7 := Row_Pos_6 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_7 /= No_Token_Index then
        Or_Pos_5 := Token_Pos_7;
        Or_Res_5 := Token_Res_7;
        goto Exit_Or_6;
    end if;
    
--  Start tok_code

Token_Res_8 := Row_Pos_6;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_8));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Floordiv)
   then
       Token_Pos_8 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_6 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_6,
             Expected_Token_Id => Turkixir_T_T__Floordiv,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_8 := Row_Pos_6 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_8 /= No_Token_Index then
        Or_Pos_5 := Token_Pos_8;
        Or_Res_5 := Token_Res_8;
        goto Exit_Or_6;
    end if;
<<Exit_Or_6>>

--  End or_code



if Or_Pos_5 /= No_Token_Index then

   Row_Pos_6 := Or_Pos_5;

else
   Row_Pos_6 := No_Token_Index;
   goto Exit_Row_6_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_6_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_6 /= No_Token_Index then

   Transform_Res_5 := Allocate_Op (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_5,
      Kind => Turkixir_Op,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_5,
      Token_End_Index   => (if Row_Pos_6 = Row_Pos_5
                            then No_Token_Index
                            else Row_Pos_6 - 1));




end if;

--  End transform_code



if Row_Pos_6 /= No_Token_Index then

   Row_Pos_5 := Row_Pos_6;

else
   Row_Pos_5 := No_Token_Index;
   goto Exit_Row_5_0;

end if;


Defer_Res_10 :=
   Factor_Or_Parse_1 (Parser, Row_Pos_5);
Defer_Pos_10 := Parser.Current_Pos;



if Defer_Pos_10 /= No_Token_Index then

   Row_Pos_5 := Defer_Pos_10;

else
   Row_Pos_5 := No_Token_Index;
   goto Exit_Row_5_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_5_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_5 /= No_Token_Index then

   Transform_Res_6 := Allocate_Term (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_6,
      Kind => Turkixir_Term,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_5 = Pos
                            then No_Token_Index
                            else Row_Pos_5 - 1));

      Initialize_Fields_For_Term
        (Self => Transform_Res_6, Bin_Op_F_Left => Defer_Res_9, Bin_Op_F_Op => Transform_Res_5, Bin_Op_F_Right => Defer_Res_10);

         if Defer_Res_9 /= null and then Is_Incomplete (Defer_Res_9) then
            Transform_Res_6.Last_Attempted_Child := 0;
         elsif Defer_Res_9 /= null and then not Is_Ghost (Defer_Res_9) then
            Transform_Res_6.Last_Attempted_Child := -1;
         end if;
         if Transform_Res_5 /= null and then Is_Incomplete (Transform_Res_5) then
            Transform_Res_6.Last_Attempted_Child := 0;
         elsif Transform_Res_5 /= null and then not Is_Ghost (Transform_Res_5) then
            Transform_Res_6.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_10 /= null and then Is_Incomplete (Defer_Res_10) then
            Transform_Res_6.Last_Attempted_Child := 0;
         elsif Defer_Res_10 /= null and then not Is_Ghost (Defer_Res_10) then
            Transform_Res_6.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_5 /= No_Token_Index then
        Or_Pos_6 := Row_Pos_5;
        Or_Res_6 := Transform_Res_6;
        goto Exit_Or_5;
    end if;
    
Defer_Res_11 :=
   Factor_Or_Parse_1 (Parser, Pos);
Defer_Pos_11 := Parser.Current_Pos;

    if Defer_Pos_11 /= No_Token_Index then
        Or_Pos_6 := Defer_Pos_11;
        Or_Res_6 := Defer_Res_11;
        goto Exit_Or_5;
    end if;
<<Exit_Or_5>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

      if Or_Pos_6 > Mem_Pos then
         Mem_Pos := Or_Pos_6;
         Mem_Res := Or_Res_6;
         Set
           (Parser.Private_Part.Term_Or_Parse_1_Memo,
            Or_Pos_6 /= No_Token_Index,
            Or_Res_6,
            Pos,
            Or_Pos_6);
         goto Try_Again;

      elsif Mem_Pos > Pos then
         Or_Res_6 := Mem_Res;
         Or_Pos_6 := Mem_Pos;
         goto No_Memo;
      end if;

   Set
     (Parser.Private_Part.Term_Or_Parse_1_Memo,
      Or_Pos_6 /= No_Token_Index,
      Or_Res_6,
      Pos,
      Or_Pos_6);

       <<No_Memo>>

   Parser.Current_Pos := Or_Pos_6;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_6;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Term_Or_Parse_1;

   


function Arith_Expr_Or_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_3 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_6 :
            Token_Index
               := No_Token_Index;
      Defer_Res_6 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Row_Pos_4 :
            Token_Index
               := No_Token_Index;
      Token_Pos_3 :
            Token_Index
               := No_Token_Index;
      Token_Res_3 :
            Token_Index
               := No_Token_Index;
      Token_Pos_4 :
            Token_Index
               := No_Token_Index;
      Token_Res_4 :
            Token_Index
               := No_Token_Index;
      Or_Pos_3 :
            Token_Index
               := No_Token_Index;
      Or_Res_3 :
            Token_Index
               := No_Token_Index;
      Transform_Res_3 :
            Bare_Op
               := No_Bare_Turkixir_Node;
      Defer_Pos_7 :
            Token_Index
               := No_Token_Index;
      Defer_Res_7 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_4 :
            Bare_Arith_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_8 :
            Token_Index
               := No_Token_Index;
      Defer_Res_8 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_4 :
            Token_Index
               := No_Token_Index;
      Or_Res_4 :
            Bare_Expr
               := No_Bare_Turkixir_Node;

      Mem_Pos : Token_Index := Pos;
      Mem_Res : Bare_Expr := No_Bare_Turkixir_Node;

   M : Memo_Entry := Get (Parser.Private_Part.Arith_Expr_Or_Parse_1_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_4 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_4;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_4;
   end if;

       Set (Parser.Private_Part.Arith_Expr_Or_Parse_1_Memo, False, Or_Res_4, Pos, Mem_Pos);

       <<Try_Again>>



   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_4 := No_Token_Index;
Or_Res_4 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_3 := Pos;



Defer_Res_6 :=
   Arith_Expr_Or_Parse_1 (Parser, Row_Pos_3);
Defer_Pos_6 := Parser.Current_Pos;



if Defer_Pos_6 /= No_Token_Index then

   Row_Pos_3 := Defer_Pos_6;

else
   Row_Pos_3 := No_Token_Index;
   goto Exit_Row_3_0;

end if;


--  Start transform_code


--  Start row_code

Row_Pos_4 := Row_Pos_3;



--  Start or_code

Or_Pos_3 := No_Token_Index;
Or_Res_3 := No_Token_Index;
    
--  Start tok_code

Token_Res_3 := Row_Pos_4;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_3));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Plus)
   then
       Token_Pos_3 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_4 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_4,
             Expected_Token_Id => Turkixir_T_T__Plus,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_3 := Row_Pos_4 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_3 /= No_Token_Index then
        Or_Pos_3 := Token_Pos_3;
        Or_Res_3 := Token_Res_3;
        goto Exit_Or_4;
    end if;
    
--  Start tok_code

Token_Res_4 := Row_Pos_4;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_4));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Minus)
   then
       Token_Pos_4 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_4 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_4,
             Expected_Token_Id => Turkixir_T_T__Minus,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_4 := Row_Pos_4 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_4 /= No_Token_Index then
        Or_Pos_3 := Token_Pos_4;
        Or_Res_3 := Token_Res_4;
        goto Exit_Or_4;
    end if;
<<Exit_Or_4>>

--  End or_code



if Or_Pos_3 /= No_Token_Index then

   Row_Pos_4 := Or_Pos_3;

else
   Row_Pos_4 := No_Token_Index;
   goto Exit_Row_4_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_4_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_4 /= No_Token_Index then

   Transform_Res_3 := Allocate_Op (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_3,
      Kind => Turkixir_Op,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_3,
      Token_End_Index   => (if Row_Pos_4 = Row_Pos_3
                            then No_Token_Index
                            else Row_Pos_4 - 1));




end if;

--  End transform_code



if Row_Pos_4 /= No_Token_Index then

   Row_Pos_3 := Row_Pos_4;

else
   Row_Pos_3 := No_Token_Index;
   goto Exit_Row_3_0;

end if;


Defer_Res_7 :=
   Term_Or_Parse_1 (Parser, Row_Pos_3);
Defer_Pos_7 := Parser.Current_Pos;



if Defer_Pos_7 /= No_Token_Index then

   Row_Pos_3 := Defer_Pos_7;

else
   Row_Pos_3 := No_Token_Index;
   goto Exit_Row_3_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_3_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_3 /= No_Token_Index then

   Transform_Res_4 := Allocate_Arith_Expr (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_4,
      Kind => Turkixir_Arith_Expr,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_3 = Pos
                            then No_Token_Index
                            else Row_Pos_3 - 1));

      Initialize_Fields_For_Arith_Expr
        (Self => Transform_Res_4, Bin_Op_F_Left => Defer_Res_6, Bin_Op_F_Op => Transform_Res_3, Bin_Op_F_Right => Defer_Res_7);

         if Defer_Res_6 /= null and then Is_Incomplete (Defer_Res_6) then
            Transform_Res_4.Last_Attempted_Child := 0;
         elsif Defer_Res_6 /= null and then not Is_Ghost (Defer_Res_6) then
            Transform_Res_4.Last_Attempted_Child := -1;
         end if;
         if Transform_Res_3 /= null and then Is_Incomplete (Transform_Res_3) then
            Transform_Res_4.Last_Attempted_Child := 0;
         elsif Transform_Res_3 /= null and then not Is_Ghost (Transform_Res_3) then
            Transform_Res_4.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_7 /= null and then Is_Incomplete (Defer_Res_7) then
            Transform_Res_4.Last_Attempted_Child := 0;
         elsif Defer_Res_7 /= null and then not Is_Ghost (Defer_Res_7) then
            Transform_Res_4.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_3 /= No_Token_Index then
        Or_Pos_4 := Row_Pos_3;
        Or_Res_4 := Transform_Res_4;
        goto Exit_Or_3;
    end if;
    
Defer_Res_8 :=
   Term_Or_Parse_1 (Parser, Pos);
Defer_Pos_8 := Parser.Current_Pos;

    if Defer_Pos_8 /= No_Token_Index then
        Or_Pos_4 := Defer_Pos_8;
        Or_Res_4 := Defer_Res_8;
        goto Exit_Or_3;
    end if;
<<Exit_Or_3>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

      if Or_Pos_4 > Mem_Pos then
         Mem_Pos := Or_Pos_4;
         Mem_Res := Or_Res_4;
         Set
           (Parser.Private_Part.Arith_Expr_Or_Parse_1_Memo,
            Or_Pos_4 /= No_Token_Index,
            Or_Res_4,
            Pos,
            Or_Pos_4);
         goto Try_Again;

      elsif Mem_Pos > Pos then
         Or_Res_4 := Mem_Res;
         Or_Pos_4 := Mem_Pos;
         goto No_Memo;
      end if;

   Set
     (Parser.Private_Part.Arith_Expr_Or_Parse_1_Memo,
      Or_Pos_4 /= No_Token_Index,
      Or_Res_4,
      Pos,
      Or_Pos_4);

       <<No_Memo>>

   Parser.Current_Pos := Or_Pos_4;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_4;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Arith_Expr_Or_Parse_1;

   


function Shift_Expr_Or_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_1 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_3 :
            Token_Index
               := No_Token_Index;
      Defer_Res_3 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Row_Pos_2 :
            Token_Index
               := No_Token_Index;
      Token_Pos_1 :
            Token_Index
               := No_Token_Index;
      Token_Res_1 :
            Token_Index
               := No_Token_Index;
      Token_Pos_2 :
            Token_Index
               := No_Token_Index;
      Token_Res_2 :
            Token_Index
               := No_Token_Index;
      Or_Pos_1 :
            Token_Index
               := No_Token_Index;
      Or_Res_1 :
            Token_Index
               := No_Token_Index;
      Transform_Res_1 :
            Bare_Op
               := No_Bare_Turkixir_Node;
      Defer_Pos_4 :
            Token_Index
               := No_Token_Index;
      Defer_Res_4 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_2 :
            Bare_Shift_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_5 :
            Token_Index
               := No_Token_Index;
      Defer_Res_5 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_2 :
            Token_Index
               := No_Token_Index;
      Or_Res_2 :
            Bare_Expr
               := No_Bare_Turkixir_Node;

      Mem_Pos : Token_Index := Pos;
      Mem_Res : Bare_Expr := No_Bare_Turkixir_Node;

   M : Memo_Entry := Get (Parser.Private_Part.Shift_Expr_Or_Parse_1_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_2 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_2;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_2;
   end if;

       Set (Parser.Private_Part.Shift_Expr_Or_Parse_1_Memo, False, Or_Res_2, Pos, Mem_Pos);

       <<Try_Again>>



   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_2 := No_Token_Index;
Or_Res_2 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_1 := Pos;



Defer_Res_3 :=
   Shift_Expr_Or_Parse_1 (Parser, Row_Pos_1);
Defer_Pos_3 := Parser.Current_Pos;



if Defer_Pos_3 /= No_Token_Index then

   Row_Pos_1 := Defer_Pos_3;

else
   Row_Pos_1 := No_Token_Index;
   goto Exit_Row_1_0;

end if;


--  Start transform_code


--  Start row_code

Row_Pos_2 := Row_Pos_1;



--  Start or_code

Or_Pos_1 := No_Token_Index;
Or_Res_1 := No_Token_Index;
    
--  Start tok_code

Token_Res_1 := Row_Pos_2;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_1));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Lsh)
   then
       Token_Pos_1 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_2 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_2,
             Expected_Token_Id => Turkixir_T_T__Lsh,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_1 := Row_Pos_2 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_1 /= No_Token_Index then
        Or_Pos_1 := Token_Pos_1;
        Or_Res_1 := Token_Res_1;
        goto Exit_Or_2;
    end if;
    
--  Start tok_code

Token_Res_2 := Row_Pos_2;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_2));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Rsh)
   then
       Token_Pos_2 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_2 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_2,
             Expected_Token_Id => Turkixir_T_T__Rsh,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_2 := Row_Pos_2 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_2 /= No_Token_Index then
        Or_Pos_1 := Token_Pos_2;
        Or_Res_1 := Token_Res_2;
        goto Exit_Or_2;
    end if;
<<Exit_Or_2>>

--  End or_code



if Or_Pos_1 /= No_Token_Index then

   Row_Pos_2 := Or_Pos_1;

else
   Row_Pos_2 := No_Token_Index;
   goto Exit_Row_2_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_2_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_2 /= No_Token_Index then

   Transform_Res_1 := Allocate_Op (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_1,
      Kind => Turkixir_Op,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_1,
      Token_End_Index   => (if Row_Pos_2 = Row_Pos_1
                            then No_Token_Index
                            else Row_Pos_2 - 1));




end if;

--  End transform_code



if Row_Pos_2 /= No_Token_Index then

   Row_Pos_1 := Row_Pos_2;

else
   Row_Pos_1 := No_Token_Index;
   goto Exit_Row_1_0;

end if;


Defer_Res_4 :=
   Arith_Expr_Or_Parse_1 (Parser, Row_Pos_1);
Defer_Pos_4 := Parser.Current_Pos;



if Defer_Pos_4 /= No_Token_Index then

   Row_Pos_1 := Defer_Pos_4;

else
   Row_Pos_1 := No_Token_Index;
   goto Exit_Row_1_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_1_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_1 /= No_Token_Index then

   Transform_Res_2 := Allocate_Shift_Expr (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_2,
      Kind => Turkixir_Shift_Expr,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_1 = Pos
                            then No_Token_Index
                            else Row_Pos_1 - 1));

      Initialize_Fields_For_Shift_Expr
        (Self => Transform_Res_2, Bin_Op_F_Left => Defer_Res_3, Bin_Op_F_Op => Transform_Res_1, Bin_Op_F_Right => Defer_Res_4);

         if Defer_Res_3 /= null and then Is_Incomplete (Defer_Res_3) then
            Transform_Res_2.Last_Attempted_Child := 0;
         elsif Defer_Res_3 /= null and then not Is_Ghost (Defer_Res_3) then
            Transform_Res_2.Last_Attempted_Child := -1;
         end if;
         if Transform_Res_1 /= null and then Is_Incomplete (Transform_Res_1) then
            Transform_Res_2.Last_Attempted_Child := 0;
         elsif Transform_Res_1 /= null and then not Is_Ghost (Transform_Res_1) then
            Transform_Res_2.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_4 /= null and then Is_Incomplete (Defer_Res_4) then
            Transform_Res_2.Last_Attempted_Child := 0;
         elsif Defer_Res_4 /= null and then not Is_Ghost (Defer_Res_4) then
            Transform_Res_2.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_1 /= No_Token_Index then
        Or_Pos_2 := Row_Pos_1;
        Or_Res_2 := Transform_Res_2;
        goto Exit_Or_1;
    end if;
    
Defer_Res_5 :=
   Arith_Expr_Or_Parse_1 (Parser, Pos);
Defer_Pos_5 := Parser.Current_Pos;

    if Defer_Pos_5 /= No_Token_Index then
        Or_Pos_2 := Defer_Pos_5;
        Or_Res_2 := Defer_Res_5;
        goto Exit_Or_1;
    end if;
<<Exit_Or_1>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

      if Or_Pos_2 > Mem_Pos then
         Mem_Pos := Or_Pos_2;
         Mem_Res := Or_Res_2;
         Set
           (Parser.Private_Part.Shift_Expr_Or_Parse_1_Memo,
            Or_Pos_2 /= No_Token_Index,
            Or_Res_2,
            Pos,
            Or_Pos_2);
         goto Try_Again;

      elsif Mem_Pos > Pos then
         Or_Res_2 := Mem_Res;
         Or_Pos_2 := Mem_Pos;
         goto No_Memo;
      end if;

   Set
     (Parser.Private_Part.Shift_Expr_Or_Parse_1_Memo,
      Or_Pos_2 /= No_Token_Index,
      Or_Res_2,
      Pos,
      Or_Pos_2);

       <<No_Memo>>

   Parser.Current_Pos := Or_Pos_2;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_2;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Shift_Expr_Or_Parse_1;

   


function And_Expr_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Expr
is
   use Bare_Expr_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_0 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_0 :
            Token_Index
               := No_Token_Index;
      Defer_Res_0 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_0 :
            Token_Index
               := No_Token_Index;
      Token_Res_0 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_1 :
            Token_Index
               := No_Token_Index;
      Defer_Res_1 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_0 :
            Bare_And_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_2 :
            Token_Index
               := No_Token_Index;
      Defer_Res_2 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_0 :
            Token_Index
               := No_Token_Index;
      Or_Res_0 :
            Bare_Expr
               := No_Bare_Turkixir_Node;

      Mem_Pos : Token_Index := Pos;
      Mem_Res : Bare_Expr := No_Bare_Turkixir_Node;

   M : Memo_Entry := Get (Parser.Private_Part.And_Expr_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_0 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_0;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_0;
   end if;

       Set (Parser.Private_Part.And_Expr_Or_Parse_0_Memo, False, Or_Res_0, Pos, Mem_Pos);

       <<Try_Again>>



   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_0 := No_Token_Index;
Or_Res_0 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_0 := Pos;



Defer_Res_0 :=
   And_Expr_Or_Parse_0 (Parser, Row_Pos_0);
Defer_Pos_0 := Parser.Current_Pos;



if Defer_Pos_0 /= No_Token_Index then

   Row_Pos_0 := Defer_Pos_0;

else
   Row_Pos_0 := No_Token_Index;
   goto Exit_Row_0_0;

end if;


--  Start tok_code

Token_Res_0 := Row_Pos_0;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_0));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Amp)
   then
       Token_Pos_0 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_0 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_0,
             Expected_Token_Id => Turkixir_T_T__Amp,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_0 := Row_Pos_0 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_0 /= No_Token_Index then

   Row_Pos_0 := Token_Pos_0;

else
   Row_Pos_0 := No_Token_Index;
   goto Exit_Row_0_0;

end if;


Defer_Res_1 :=
   Shift_Expr_Or_Parse_1 (Parser, Row_Pos_0);
Defer_Pos_1 := Parser.Current_Pos;



if Defer_Pos_1 /= No_Token_Index then

   Row_Pos_0 := Defer_Pos_1;

else
   Row_Pos_0 := No_Token_Index;
   goto Exit_Row_0_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_0_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_0 /= No_Token_Index then

   Transform_Res_0 := Allocate_And_Expr (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_0,
      Kind => Turkixir_And_Expr,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_0 = Pos
                            then No_Token_Index
                            else Row_Pos_0 - 1));

      Initialize_Fields_For_And_Expr
        (Self => Transform_Res_0, And_Expr_F_Left => Defer_Res_0, And_Expr_F_Right => Defer_Res_1);

         if Defer_Res_0 /= null and then Is_Incomplete (Defer_Res_0) then
            Transform_Res_0.Last_Attempted_Child := 0;
         elsif Defer_Res_0 /= null and then not Is_Ghost (Defer_Res_0) then
            Transform_Res_0.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_1 /= null and then Is_Incomplete (Defer_Res_1) then
            Transform_Res_0.Last_Attempted_Child := 0;
         elsif Defer_Res_1 /= null and then not Is_Ghost (Defer_Res_1) then
            Transform_Res_0.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_0 /= No_Token_Index then
        Or_Pos_0 := Row_Pos_0;
        Or_Res_0 := Transform_Res_0;
        goto Exit_Or_0;
    end if;
    
Defer_Res_2 :=
   Shift_Expr_Or_Parse_1 (Parser, Pos);
Defer_Pos_2 := Parser.Current_Pos;

    if Defer_Pos_2 /= No_Token_Index then
        Or_Pos_0 := Defer_Pos_2;
        Or_Res_0 := Defer_Res_2;
        goto Exit_Or_0;
    end if;
<<Exit_Or_0>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

      if Or_Pos_0 > Mem_Pos then
         Mem_Pos := Or_Pos_0;
         Mem_Res := Or_Res_0;
         Set
           (Parser.Private_Part.And_Expr_Or_Parse_0_Memo,
            Or_Pos_0 /= No_Token_Index,
            Or_Res_0,
            Pos,
            Or_Pos_0);
         goto Try_Again;

      elsif Mem_Pos > Pos then
         Or_Res_0 := Mem_Res;
         Or_Pos_0 := Mem_Pos;
         goto No_Memo;
      end if;

   Set
     (Parser.Private_Part.And_Expr_Or_Parse_0_Memo,
      Or_Pos_0 /= No_Token_Index,
      Or_Res_0,
      Pos,
      Or_Pos_0);

       <<No_Memo>>

   Parser.Current_Pos := Or_Pos_0;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_0;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end And_Expr_Or_Parse_0;

   


function As_Name_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_As_Name_Node
is
   use Bare_As_Name_Node_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_71 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_107 :
            Token_Index
               := No_Token_Index;
      Defer_Res_107 :
            Bare_Id
               := No_Bare_Turkixir_Node;
      Token_Pos_100 :
            Token_Index
               := No_Token_Index;
      Token_Res_100 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_108 :
            Token_Index
               := No_Token_Index;
      Defer_Res_108 :
            Bare_Id
               := No_Bare_Turkixir_Node;
      Transform_Res_60 :
            Bare_As_Name_Node
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.As_Name_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_60 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_60;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_60;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_71 := Pos;



Defer_Res_107 :=
   Name_Transform_Parse_0 (Parser, Row_Pos_71);
Defer_Pos_107 := Parser.Current_Pos;



if Defer_Pos_107 /= No_Token_Index then

   Row_Pos_71 := Defer_Pos_107;

else
   Row_Pos_71 := No_Token_Index;
   goto Exit_Row_71_0;

end if;


--  Start tok_code

Token_Res_100 := Row_Pos_71;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_100));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__As)
   then
       Token_Pos_100 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_71 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_71,
             Expected_Token_Id => Turkixir_T_T__As,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_100 := Row_Pos_71 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_100 /= No_Token_Index then

   Row_Pos_71 := Token_Pos_100;

else
   Row_Pos_71 := No_Token_Index;
   goto Exit_Row_71_0;

end if;


Defer_Res_108 :=
   Name_Transform_Parse_0 (Parser, Row_Pos_71);
Defer_Pos_108 := Parser.Current_Pos;



if Defer_Pos_108 /= No_Token_Index then

   Row_Pos_71 := Defer_Pos_108;

else
   Row_Pos_71 := No_Token_Index;
   goto Exit_Row_71_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_71_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_71 /= No_Token_Index then

   Transform_Res_60 := Allocate_As_Name_Node (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_60,
      Kind => Turkixir_As_Name_Node,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_71 = Pos
                            then No_Token_Index
                            else Row_Pos_71 - 1));

      Initialize_Fields_For_As_Name_Node
        (Self => Transform_Res_60, As_Name_Node_F_Imported => Defer_Res_107, As_Name_Node_F_As_Name => Defer_Res_108);

         if Defer_Res_107 /= null and then Is_Incomplete (Defer_Res_107) then
            Transform_Res_60.Last_Attempted_Child := 0;
         elsif Defer_Res_107 /= null and then not Is_Ghost (Defer_Res_107) then
            Transform_Res_60.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_108 /= null and then Is_Incomplete (Defer_Res_108) then
            Transform_Res_60.Last_Attempted_Child := 0;
         elsif Defer_Res_108 /= null and then not Is_Ghost (Defer_Res_108) then
            Transform_Res_60.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.As_Name_Transform_Parse_0_Memo,
      Row_Pos_71 /= No_Token_Index,
      Transform_Res_60,
      Pos,
      Row_Pos_71);


   Parser.Current_Pos := Row_Pos_71;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_60;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end As_Name_Transform_Parse_0;

   


function Assert_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Assert_Stmt
is
   use Bare_Assert_Stmt_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_72 :
            Token_Index
               := No_Token_Index;
      Token_Pos_101 :
            Token_Index
               := No_Token_Index;
      Token_Res_101 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_109 :
            Token_Index
               := No_Token_Index;
      Defer_Res_109 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Row_Pos_73 :
            Token_Index
               := No_Token_Index;
      Token_Pos_102 :
            Token_Index
               := No_Token_Index;
      Token_Res_102 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_110 :
            Token_Index
               := No_Token_Index;
      Defer_Res_110 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_61 :
            Bare_Assert_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Assert_Stmt_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_61 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_61;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_61;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_72 := Pos;



--  Start tok_code

Token_Res_101 := Row_Pos_72;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_101));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Assert)
   then
       Token_Pos_101 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_72 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_72,
             Expected_Token_Id => Turkixir_T_T__Assert,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_101 := Row_Pos_72 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_101 /= No_Token_Index then

   Row_Pos_72 := Token_Pos_101;

else
   Row_Pos_72 := No_Token_Index;
   goto Exit_Row_72_0;

end if;


Defer_Res_109 :=
   Test_Or_Parse_0 (Parser, Row_Pos_72);
Defer_Pos_109 := Parser.Current_Pos;



if Defer_Pos_109 /= No_Token_Index then

   Row_Pos_72 := Defer_Pos_109;

else
   Row_Pos_72 := No_Token_Index;
   goto Exit_Row_72_0;

end if;


--  Start opt_code




--  Start row_code

Row_Pos_73 := Row_Pos_72;



--  Start tok_code

Token_Res_102 := Row_Pos_73;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_102));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_102 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_73 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_73,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_102 := Row_Pos_73 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_102 /= No_Token_Index then

   Row_Pos_73 := Token_Pos_102;

else
   Row_Pos_73 := No_Token_Index;
   goto Exit_Row_73_0;

end if;


Defer_Res_110 :=
   Test_Or_Parse_0 (Parser, Row_Pos_73);
Defer_Pos_110 := Parser.Current_Pos;



if Defer_Pos_110 /= No_Token_Index then

   Row_Pos_73 := Defer_Pos_110;

else
   Row_Pos_73 := No_Token_Index;
   goto Exit_Row_73_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_73_0>>
pragma Warnings (On, "referenced");

--  End row_code


if Row_Pos_73 = No_Token_Index then

        Defer_Res_110 := No_Bare_Turkixir_Node;


    Row_Pos_73 := Row_Pos_72;


end if;

--  End opt_code



if Row_Pos_73 /= No_Token_Index then

   Row_Pos_72 := Row_Pos_73;

else
   Row_Pos_72 := No_Token_Index;
   goto Exit_Row_72_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_72_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_72 /= No_Token_Index then

   Transform_Res_61 := Allocate_Assert_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_61,
      Kind => Turkixir_Assert_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_72 = Pos
                            then No_Token_Index
                            else Row_Pos_72 - 1));

      Initialize_Fields_For_Assert_Stmt
        (Self => Transform_Res_61, Assert_Stmt_F_Test_Expr => Defer_Res_109, Assert_Stmt_F_Msg => Defer_Res_110);

         if Defer_Res_109 /= null and then Is_Incomplete (Defer_Res_109) then
            Transform_Res_61.Last_Attempted_Child := 0;
         elsif Defer_Res_109 /= null and then not Is_Ghost (Defer_Res_109) then
            Transform_Res_61.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_110 /= null and then Is_Incomplete (Defer_Res_110) then
            Transform_Res_61.Last_Attempted_Child := 0;
         elsif Defer_Res_110 /= null and then not Is_Ghost (Defer_Res_110) then
            Transform_Res_61.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Assert_Stmt_Transform_Parse_0_Memo,
      Row_Pos_72 /= No_Token_Index,
      Transform_Res_61,
      Pos,
      Row_Pos_72);


   Parser.Current_Pos := Row_Pos_72;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_61;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Assert_Stmt_Transform_Parse_0;

   


function Break_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Break_Stmt
is
   use Bare_Break_Stmt_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_74 :
            Token_Index
               := No_Token_Index;
      Token_Pos_103 :
            Token_Index
               := No_Token_Index;
      Token_Res_103 :
            Token_Index
               := No_Token_Index;
      Transform_Res_62 :
            Bare_Break_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Break_Stmt_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_62 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_62;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_62;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_74 := Pos;



--  Start tok_code

Token_Res_103 := Row_Pos_74;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_103));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Break)
   then
       Token_Pos_103 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_74 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_74,
             Expected_Token_Id => Turkixir_T_T__Break,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_103 := Row_Pos_74 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_103 /= No_Token_Index then

   Row_Pos_74 := Token_Pos_103;

else
   Row_Pos_74 := No_Token_Index;
   goto Exit_Row_74_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_74_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_74 /= No_Token_Index then

   Transform_Res_62 := Allocate_Break_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_62,
      Kind => Turkixir_Break_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_74 = Pos
                            then No_Token_Index
                            else Row_Pos_74 - 1));




end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Break_Stmt_Transform_Parse_0_Memo,
      Row_Pos_74 /= No_Token_Index,
      Transform_Res_62,
      Pos,
      Row_Pos_74);


   Parser.Current_Pos := Row_Pos_74;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_62;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Break_Stmt_Transform_Parse_0;

   


function Nl_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_NL
is
   use Bare_NL_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_79 :
            Token_Index
               := No_Token_Index;
      Token_Pos_110 :
            Token_Index
               := No_Token_Index;
      Token_Res_110 :
            Token_Index
               := No_Token_Index;
      Transform_Res_64 :
            Bare_NL
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Nl_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_64 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_64;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_64;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_79 := Pos;



--  Start tok_code

Token_Res_110 := Row_Pos_79;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_110));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_Newline)
   then
       Token_Pos_110 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_79 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_79,
             Expected_Token_Id => Turkixir_Newline,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_110 := Row_Pos_79 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_110 /= No_Token_Index then

   Row_Pos_79 := Token_Pos_110;

else
   Row_Pos_79 := No_Token_Index;
   goto Exit_Row_78_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_78_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_79 /= No_Token_Index then

   Transform_Res_64 := Allocate_NL (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_64,
      Kind => Turkixir_NL,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_79 = Pos
                            then No_Token_Index
                            else Row_Pos_79 - 1));




end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Nl_Transform_Parse_0_Memo,
      Row_Pos_79 /= No_Token_Index,
      Transform_Res_64,
      Pos,
      Row_Pos_79);


   Parser.Current_Pos := Row_Pos_79;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_64;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Nl_Transform_Parse_0;

   


function Expr_Stmt_Or_Parse_3
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node
is
   use Bare_Turkixir_Node_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_82 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_132 :
            Token_Index
               := No_Token_Index;
      Defer_Res_132 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Row_Pos_83 :
            Token_Index
               := No_Token_Index;
      Token_Pos_114 :
            Token_Index
               := No_Token_Index;
      Token_Res_114 :
            Token_Index
               := No_Token_Index;
      Token_Pos_115 :
            Token_Index
               := No_Token_Index;
      Token_Res_115 :
            Token_Index
               := No_Token_Index;
      Token_Pos_116 :
            Token_Index
               := No_Token_Index;
      Token_Res_116 :
            Token_Index
               := No_Token_Index;
      Token_Pos_117 :
            Token_Index
               := No_Token_Index;
      Token_Res_117 :
            Token_Index
               := No_Token_Index;
      Token_Pos_118 :
            Token_Index
               := No_Token_Index;
      Token_Res_118 :
            Token_Index
               := No_Token_Index;
      Token_Pos_119 :
            Token_Index
               := No_Token_Index;
      Token_Res_119 :
            Token_Index
               := No_Token_Index;
      Token_Pos_120 :
            Token_Index
               := No_Token_Index;
      Token_Res_120 :
            Token_Index
               := No_Token_Index;
      Token_Pos_121 :
            Token_Index
               := No_Token_Index;
      Token_Res_121 :
            Token_Index
               := No_Token_Index;
      Token_Pos_122 :
            Token_Index
               := No_Token_Index;
      Token_Res_122 :
            Token_Index
               := No_Token_Index;
      Token_Pos_123 :
            Token_Index
               := No_Token_Index;
      Token_Res_123 :
            Token_Index
               := No_Token_Index;
      Token_Pos_124 :
            Token_Index
               := No_Token_Index;
      Token_Res_124 :
            Token_Index
               := No_Token_Index;
      Token_Pos_125 :
            Token_Index
               := No_Token_Index;
      Token_Res_125 :
            Token_Index
               := No_Token_Index;
      Or_Pos_29 :
            Token_Index
               := No_Token_Index;
      Or_Res_29 :
            Token_Index
               := No_Token_Index;
      Transform_Res_65 :
            Bare_Op
               := No_Bare_Turkixir_Node;
      Defer_Pos_133 :
            Token_Index
               := No_Token_Index;
      Defer_Res_133 :
            Bare_Yield_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_134 :
            Token_Index
               := No_Token_Index;
      Defer_Res_134 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Or_Pos_30 :
            Token_Index
               := No_Token_Index;
      Or_Res_30 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Transform_Res_66 :
            Bare_Aug_Assign_Stmt
               := No_Bare_Turkixir_Node;
      Row_Pos_84 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_135 :
            Token_Index
               := No_Token_Index;
      Defer_Res_135 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Lst_Cpos_14 :
            Token_Index
               := No_Token_Index;
      Tmp_List_14 :
            Free_Parse_List;
      Row_Pos_85 :
            Token_Index
               := No_Token_Index;
      Token_Pos_126 :
            Token_Index
               := No_Token_Index;
      Token_Res_126 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_136 :
            Token_Index
               := No_Token_Index;
      Defer_Res_136 :
            Bare_Yield_Expr
               := No_Bare_Turkixir_Node;
      Defer_Pos_137 :
            Token_Index
               := No_Token_Index;
      Defer_Res_137 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Or_Pos_31 :
            Token_Index
               := No_Token_Index;
      Or_Res_31 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      List_Pos_14 :
            Token_Index
               := No_Token_Index;
      List_Res_14 :
            Bare_Turkixir_Node_List
               := No_Bare_Turkixir_Node;
      Transform_Res_67 :
            Bare_Assign_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_138 :
            Token_Index
               := No_Token_Index;
      Defer_Res_138 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Or_Pos_32 :
            Token_Index
               := No_Token_Index;
      Or_Res_32 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Expr_Stmt_Or_Parse_3_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_32 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_32;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_32;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_32 := No_Token_Index;
Or_Res_32 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_82 := Pos;



Defer_Res_132 :=
   Test_List_Extract_Parse_0 (Parser, Row_Pos_82);
Defer_Pos_132 := Parser.Current_Pos;



if Defer_Pos_132 /= No_Token_Index then

   Row_Pos_82 := Defer_Pos_132;

else
   Row_Pos_82 := No_Token_Index;
   goto Exit_Row_81_0;

end if;


--  Start transform_code


--  Start row_code

Row_Pos_83 := Row_Pos_82;



--  Start or_code

Or_Pos_29 := No_Token_Index;
Or_Res_29 := No_Token_Index;
    
--  Start tok_code

Token_Res_114 := Row_Pos_83;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_114));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Add_Asign)
   then
       Token_Pos_114 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_83 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_83,
             Expected_Token_Id => Turkixir_T_T__Add_Asign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_114 := Row_Pos_83 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_114 /= No_Token_Index then
        Or_Pos_29 := Token_Pos_114;
        Or_Res_29 := Token_Res_114;
        goto Exit_Or_30;
    end if;
    
--  Start tok_code

Token_Res_115 := Row_Pos_83;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_115));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Minus_Assign)
   then
       Token_Pos_115 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_83 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_83,
             Expected_Token_Id => Turkixir_T_T__Minus_Assign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_115 := Row_Pos_83 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_115 /= No_Token_Index then
        Or_Pos_29 := Token_Pos_115;
        Or_Res_29 := Token_Res_115;
        goto Exit_Or_30;
    end if;
    
--  Start tok_code

Token_Res_116 := Row_Pos_83;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_116));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Mult_Assign)
   then
       Token_Pos_116 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_83 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_83,
             Expected_Token_Id => Turkixir_T_T__Mult_Assign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_116 := Row_Pos_83 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_116 /= No_Token_Index then
        Or_Pos_29 := Token_Pos_116;
        Or_Res_29 := Token_Res_116;
        goto Exit_Or_30;
    end if;
    
--  Start tok_code

Token_Res_117 := Row_Pos_83;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_117));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Div_Assign)
   then
       Token_Pos_117 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_83 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_83,
             Expected_Token_Id => Turkixir_T_T__Div_Assign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_117 := Row_Pos_83 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_117 /= No_Token_Index then
        Or_Pos_29 := Token_Pos_117;
        Or_Res_29 := Token_Res_117;
        goto Exit_Or_30;
    end if;
    
--  Start tok_code

Token_Res_118 := Row_Pos_83;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_118));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Mod_Assign)
   then
       Token_Pos_118 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_83 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_83,
             Expected_Token_Id => Turkixir_T_T__Mod_Assign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_118 := Row_Pos_83 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_118 /= No_Token_Index then
        Or_Pos_29 := Token_Pos_118;
        Or_Res_29 := Token_Res_118;
        goto Exit_Or_30;
    end if;
    
--  Start tok_code

Token_Res_119 := Row_Pos_83;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_119));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__And_Assign)
   then
       Token_Pos_119 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_83 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_83,
             Expected_Token_Id => Turkixir_T_T__And_Assign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_119 := Row_Pos_83 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_119 /= No_Token_Index then
        Or_Pos_29 := Token_Pos_119;
        Or_Res_29 := Token_Res_119;
        goto Exit_Or_30;
    end if;
    
--  Start tok_code

Token_Res_120 := Row_Pos_83;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_120));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Or_Assign)
   then
       Token_Pos_120 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_83 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_83,
             Expected_Token_Id => Turkixir_T_T__Or_Assign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_120 := Row_Pos_83 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_120 /= No_Token_Index then
        Or_Pos_29 := Token_Pos_120;
        Or_Res_29 := Token_Res_120;
        goto Exit_Or_30;
    end if;
    
--  Start tok_code

Token_Res_121 := Row_Pos_83;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_121));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Xor_Assign)
   then
       Token_Pos_121 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_83 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_83,
             Expected_Token_Id => Turkixir_T_T__Xor_Assign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_121 := Row_Pos_83 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_121 /= No_Token_Index then
        Or_Pos_29 := Token_Pos_121;
        Or_Res_29 := Token_Res_121;
        goto Exit_Or_30;
    end if;
    
--  Start tok_code

Token_Res_122 := Row_Pos_83;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_122));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Lsh_Assign)
   then
       Token_Pos_122 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_83 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_83,
             Expected_Token_Id => Turkixir_T_T__Lsh_Assign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_122 := Row_Pos_83 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_122 /= No_Token_Index then
        Or_Pos_29 := Token_Pos_122;
        Or_Res_29 := Token_Res_122;
        goto Exit_Or_30;
    end if;
    
--  Start tok_code

Token_Res_123 := Row_Pos_83;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_123));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Rsh_Assign)
   then
       Token_Pos_123 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_83 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_83,
             Expected_Token_Id => Turkixir_T_T__Rsh_Assign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_123 := Row_Pos_83 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_123 /= No_Token_Index then
        Or_Pos_29 := Token_Pos_123;
        Or_Res_29 := Token_Res_123;
        goto Exit_Or_30;
    end if;
    
--  Start tok_code

Token_Res_124 := Row_Pos_83;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_124));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Power_Assign)
   then
       Token_Pos_124 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_83 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_83,
             Expected_Token_Id => Turkixir_T_T__Power_Assign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_124 := Row_Pos_83 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_124 /= No_Token_Index then
        Or_Pos_29 := Token_Pos_124;
        Or_Res_29 := Token_Res_124;
        goto Exit_Or_30;
    end if;
    
--  Start tok_code

Token_Res_125 := Row_Pos_83;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_125));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Floordiv_Assign)
   then
       Token_Pos_125 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_83 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_83,
             Expected_Token_Id => Turkixir_T_T__Floordiv_Assign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_125 := Row_Pos_83 + 1;
   end if;
end;

--  End tok_code

    if Token_Pos_125 /= No_Token_Index then
        Or_Pos_29 := Token_Pos_125;
        Or_Res_29 := Token_Res_125;
        goto Exit_Or_30;
    end if;
<<Exit_Or_30>>

--  End or_code



if Or_Pos_29 /= No_Token_Index then

   Row_Pos_83 := Or_Pos_29;

else
   Row_Pos_83 := No_Token_Index;
   goto Exit_Row_82_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_82_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_83 /= No_Token_Index then

   Transform_Res_65 := Allocate_Op (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_65,
      Kind => Turkixir_Op,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_82,
      Token_End_Index   => (if Row_Pos_83 = Row_Pos_82
                            then No_Token_Index
                            else Row_Pos_83 - 1));




end if;

--  End transform_code



if Row_Pos_83 /= No_Token_Index then

   Row_Pos_82 := Row_Pos_83;

else
   Row_Pos_82 := No_Token_Index;
   goto Exit_Row_81_0;

end if;


--  Start or_code

Or_Pos_30 := No_Token_Index;
Or_Res_30 := No_Bare_Turkixir_Node;
    
Defer_Res_133 :=
   Yield_Expr_Transform_Parse_0 (Parser, Row_Pos_82);
Defer_Pos_133 := Parser.Current_Pos;

    if Defer_Pos_133 /= No_Token_Index then
        Or_Pos_30 := Defer_Pos_133;
        Or_Res_30 := Defer_Res_133;
        goto Exit_Or_31;
    end if;
    
Defer_Res_134 :=
   Test_List_Extract_Parse_0 (Parser, Row_Pos_82);
Defer_Pos_134 := Parser.Current_Pos;

    if Defer_Pos_134 /= No_Token_Index then
        Or_Pos_30 := Defer_Pos_134;
        Or_Res_30 := Defer_Res_134;
        goto Exit_Or_31;
    end if;
<<Exit_Or_31>>

--  End or_code



if Or_Pos_30 /= No_Token_Index then

   Row_Pos_82 := Or_Pos_30;

else
   Row_Pos_82 := No_Token_Index;
   goto Exit_Row_81_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_81_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_82 /= No_Token_Index then

   Transform_Res_66 := Allocate_Aug_Assign_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_66,
      Kind => Turkixir_Aug_Assign_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_82 = Pos
                            then No_Token_Index
                            else Row_Pos_82 - 1));

      Initialize_Fields_For_Aug_Assign_Stmt
        (Self => Transform_Res_66, Aug_Assign_Stmt_F_L_Value => Defer_Res_132, Aug_Assign_Stmt_F_Op => Transform_Res_65, Aug_Assign_Stmt_F_R_Value => Or_Res_30);

         if Defer_Res_132 /= null and then Is_Incomplete (Defer_Res_132) then
            Transform_Res_66.Last_Attempted_Child := 0;
         elsif Defer_Res_132 /= null and then not Is_Ghost (Defer_Res_132) then
            Transform_Res_66.Last_Attempted_Child := -1;
         end if;
         if Transform_Res_65 /= null and then Is_Incomplete (Transform_Res_65) then
            Transform_Res_66.Last_Attempted_Child := 0;
         elsif Transform_Res_65 /= null and then not Is_Ghost (Transform_Res_65) then
            Transform_Res_66.Last_Attempted_Child := -1;
         end if;
         if Or_Res_30 /= null and then Is_Incomplete (Or_Res_30) then
            Transform_Res_66.Last_Attempted_Child := 0;
         elsif Or_Res_30 /= null and then not Is_Ghost (Or_Res_30) then
            Transform_Res_66.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_82 /= No_Token_Index then
        Or_Pos_32 := Row_Pos_82;
        Or_Res_32 := Transform_Res_66;
        goto Exit_Or_29;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_84 := Pos;



Defer_Res_135 :=
   Test_List_Extract_Parse_0 (Parser, Row_Pos_84);
Defer_Pos_135 := Parser.Current_Pos;



if Defer_Pos_135 /= No_Token_Index then

   Row_Pos_84 := Defer_Pos_135;

else
   Row_Pos_84 := No_Token_Index;
   goto Exit_Row_83_0;

end if;


--  Start list_code

    List_Pos_14 := No_Token_Index;



Lst_Cpos_14 := Row_Pos_84;
Tmp_List_14 := Get_Parse_List (Parser);

loop
   
--  Start row_code

Row_Pos_85 := Lst_Cpos_14;



--  Start tok_code

Token_Res_126 := Row_Pos_85;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_126));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Assign)
   then
       Token_Pos_126 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_85 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_85,
             Expected_Token_Id => Turkixir_T_T__Assign,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_126 := Row_Pos_85 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_126 /= No_Token_Index then

   Row_Pos_85 := Token_Pos_126;

else
   Row_Pos_85 := No_Token_Index;
   goto Exit_Row_84_0;

end if;


--  Start or_code

Or_Pos_31 := No_Token_Index;
Or_Res_31 := No_Bare_Turkixir_Node;
    
Defer_Res_136 :=
   Yield_Expr_Transform_Parse_0 (Parser, Row_Pos_85);
Defer_Pos_136 := Parser.Current_Pos;

    if Defer_Pos_136 /= No_Token_Index then
        Or_Pos_31 := Defer_Pos_136;
        Or_Res_31 := Defer_Res_136;
        goto Exit_Or_32;
    end if;
    
Defer_Res_137 :=
   Test_List_Extract_Parse_0 (Parser, Row_Pos_85);
Defer_Pos_137 := Parser.Current_Pos;

    if Defer_Pos_137 /= No_Token_Index then
        Or_Pos_31 := Defer_Pos_137;
        Or_Res_31 := Defer_Res_137;
        goto Exit_Or_32;
    end if;
<<Exit_Or_32>>

--  End or_code



if Or_Pos_31 /= No_Token_Index then

   Row_Pos_85 := Or_Pos_31;

else
   Row_Pos_85 := No_Token_Index;
   goto Exit_Row_84_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_84_0>>
pragma Warnings (On, "referenced");

--  End row_code


   exit when Row_Pos_85 = No_Token_Index;

   List_Pos_14 := Row_Pos_85;
   Lst_Cpos_14 := List_Pos_14;

   Tmp_List_14.Nodes.Append (Or_Res_31);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_14.Nodes.Length;
begin
   List_Res_14 :=
      Allocate_Turkixir_Node_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_84;
      Token_End := (if Lst_Cpos_14 = Row_Pos_84
                    then Row_Pos_84
                    else Lst_Cpos_14 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_84, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_14,
      Kind              => Turkixir_Turkixir_Node_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_14,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_14.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_14.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_14);

--  End list_code



if List_Pos_14 /= No_Token_Index then

   Row_Pos_84 := List_Pos_14;

else
   Row_Pos_84 := No_Token_Index;
   goto Exit_Row_83_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_83_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_84 /= No_Token_Index then

   Transform_Res_67 := Allocate_Assign_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_67,
      Kind => Turkixir_Assign_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_84 = Pos
                            then No_Token_Index
                            else Row_Pos_84 - 1));

      Initialize_Fields_For_Assign_Stmt
        (Self => Transform_Res_67, Assign_Stmt_F_L_Value => Defer_Res_135, Assign_Stmt_F_R_Values => List_Res_14);

         if Defer_Res_135 /= null and then Is_Incomplete (Defer_Res_135) then
            Transform_Res_67.Last_Attempted_Child := 0;
         elsif Defer_Res_135 /= null and then not Is_Ghost (Defer_Res_135) then
            Transform_Res_67.Last_Attempted_Child := -1;
         end if;
         if List_Res_14 /= null and then Is_Incomplete (List_Res_14) then
            Transform_Res_67.Last_Attempted_Child := 0;
         elsif List_Res_14 /= null and then not Is_Ghost (List_Res_14) then
            Transform_Res_67.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_84 /= No_Token_Index then
        Or_Pos_32 := Row_Pos_84;
        Or_Res_32 := Transform_Res_67;
        goto Exit_Or_29;
    end if;
    
Defer_Res_138 :=
   Test_List_Extract_Parse_0 (Parser, Pos);
Defer_Pos_138 := Parser.Current_Pos;

    if Defer_Pos_138 /= No_Token_Index then
        Or_Pos_32 := Defer_Pos_138;
        Or_Res_32 := Defer_Res_138;
        goto Exit_Or_29;
    end if;
<<Exit_Or_29>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Expr_Stmt_Or_Parse_3_Memo,
      Or_Pos_32 /= No_Token_Index,
      Or_Res_32,
      Pos,
      Or_Pos_32);


   Parser.Current_Pos := Or_Pos_32;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_32;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Expr_Stmt_Or_Parse_3;

   


function Print_Stmt_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Stmt
is
   use Bare_Stmt_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_86 :
            Token_Index
               := No_Token_Index;
      Token_Pos_127 :
            Token_Index
               := No_Token_Index;
      Token_Res_127 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_139 :
            Token_Index
               := No_Token_Index;
      Defer_Res_139 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Transform_Res_68 :
            Bare_Print_Stmt
               := No_Bare_Turkixir_Node;
      Row_Pos_87 :
            Token_Index
               := No_Token_Index;
      Token_Pos_128 :
            Token_Index
               := No_Token_Index;
      Token_Res_128 :
            Token_Index
               := No_Token_Index;
      Token_Pos_129 :
            Token_Index
               := No_Token_Index;
      Token_Res_129 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_140 :
            Token_Index
               := No_Token_Index;
      Defer_Res_140 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_130 :
            Token_Index
               := No_Token_Index;
      Token_Res_130 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_141 :
            Token_Index
               := No_Token_Index;
      Defer_Res_141 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Transform_Res_69 :
            Bare_Stream_Print_Stmt
               := No_Bare_Turkixir_Node;
      Or_Pos_33 :
            Token_Index
               := No_Token_Index;
      Or_Res_33 :
            Bare_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Print_Stmt_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_33 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_33;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_33;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_33 := No_Token_Index;
Or_Res_33 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_86 := Pos;



--  Start tok_code

Token_Res_127 := Row_Pos_86;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_127));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Print)
   then
       Token_Pos_127 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_86 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_86,
             Expected_Token_Id => Turkixir_T_T__Print,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_127 := Row_Pos_86 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_127 /= No_Token_Index then

   Row_Pos_86 := Token_Pos_127;

else
   Row_Pos_86 := No_Token_Index;
   goto Exit_Row_85_0;

end if;


Defer_Res_139 :=
   Test_List_Extract_Parse_0 (Parser, Row_Pos_86);
Defer_Pos_139 := Parser.Current_Pos;



if Defer_Pos_139 /= No_Token_Index then

   Row_Pos_86 := Defer_Pos_139;

else
   Row_Pos_86 := No_Token_Index;
   goto Exit_Row_85_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_85_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_86 /= No_Token_Index then

   Transform_Res_68 := Allocate_Print_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_68,
      Kind => Turkixir_Print_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_86 = Pos
                            then No_Token_Index
                            else Row_Pos_86 - 1));

      Initialize_Fields_For_Print_Stmt
        (Self => Transform_Res_68, Print_Stmt_F_Exprs => Defer_Res_139);

         if Defer_Res_139 /= null and then Is_Incomplete (Defer_Res_139) then
            Transform_Res_68.Last_Attempted_Child := 0;
         elsif Defer_Res_139 /= null and then not Is_Ghost (Defer_Res_139) then
            Transform_Res_68.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_86 /= No_Token_Index then
        Or_Pos_33 := Row_Pos_86;
        Or_Res_33 := Transform_Res_68;
        goto Exit_Or_33;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_87 := Pos;



--  Start tok_code

Token_Res_128 := Row_Pos_87;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_128));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Print)
   then
       Token_Pos_128 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_87 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_87,
             Expected_Token_Id => Turkixir_T_T__Print,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_128 := Row_Pos_87 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_128 /= No_Token_Index then

   Row_Pos_87 := Token_Pos_128;

else
   Row_Pos_87 := No_Token_Index;
   goto Exit_Row_86_0;

end if;


--  Start tok_code

Token_Res_129 := Row_Pos_87;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_129));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Rsh)
   then
       Token_Pos_129 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_87 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_87,
             Expected_Token_Id => Turkixir_T_T__Rsh,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_129 := Row_Pos_87 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_129 /= No_Token_Index then

   Row_Pos_87 := Token_Pos_129;

else
   Row_Pos_87 := No_Token_Index;
   goto Exit_Row_86_0;

end if;


Defer_Res_140 :=
   Test_Or_Parse_0 (Parser, Row_Pos_87);
Defer_Pos_140 := Parser.Current_Pos;



if Defer_Pos_140 /= No_Token_Index then

   Row_Pos_87 := Defer_Pos_140;

else
   Row_Pos_87 := No_Token_Index;
   goto Exit_Row_86_0;

end if;


--  Start tok_code

Token_Res_130 := Row_Pos_87;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_130));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_130 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_87 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_87,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_130 := Row_Pos_87 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_130 /= No_Token_Index then

   Row_Pos_87 := Token_Pos_130;

else
   Row_Pos_87 := No_Token_Index;
   goto Exit_Row_86_0;

end if;


Defer_Res_141 :=
   Test_List_Extract_Parse_0 (Parser, Row_Pos_87);
Defer_Pos_141 := Parser.Current_Pos;



if Defer_Pos_141 /= No_Token_Index then

   Row_Pos_87 := Defer_Pos_141;

else
   Row_Pos_87 := No_Token_Index;
   goto Exit_Row_86_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_86_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_87 /= No_Token_Index then

   Transform_Res_69 := Allocate_Stream_Print_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_69,
      Kind => Turkixir_Stream_Print_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_87 = Pos
                            then No_Token_Index
                            else Row_Pos_87 - 1));

      Initialize_Fields_For_Stream_Print_Stmt
        (Self => Transform_Res_69, Stream_Print_Stmt_F_Stream_Expr => Defer_Res_140, Stream_Print_Stmt_F_Exprs => Defer_Res_141);

         if Defer_Res_140 /= null and then Is_Incomplete (Defer_Res_140) then
            Transform_Res_69.Last_Attempted_Child := 0;
         elsif Defer_Res_140 /= null and then not Is_Ghost (Defer_Res_140) then
            Transform_Res_69.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_141 /= null and then Is_Incomplete (Defer_Res_141) then
            Transform_Res_69.Last_Attempted_Child := 0;
         elsif Defer_Res_141 /= null and then not Is_Ghost (Defer_Res_141) then
            Transform_Res_69.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_87 /= No_Token_Index then
        Or_Pos_33 := Row_Pos_87;
        Or_Res_33 := Transform_Res_69;
        goto Exit_Or_33;
    end if;
<<Exit_Or_33>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Print_Stmt_Or_Parse_0_Memo,
      Or_Pos_33 /= No_Token_Index,
      Or_Res_33,
      Pos,
      Or_Pos_33);


   Parser.Current_Pos := Or_Pos_33;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_33;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Print_Stmt_Or_Parse_0;

   


function Del_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Del_Stmt
is
   use Bare_Del_Stmt_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_88 :
            Token_Index
               := No_Token_Index;
      Token_Pos_131 :
            Token_Index
               := No_Token_Index;
      Token_Res_131 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_142 :
            Token_Index
               := No_Token_Index;
      Defer_Res_142 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Transform_Res_70 :
            Bare_Del_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Del_Stmt_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_70 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_70;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_70;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_88 := Pos;



--  Start tok_code

Token_Res_131 := Row_Pos_88;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_131));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Del)
   then
       Token_Pos_131 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_88 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_88,
             Expected_Token_Id => Turkixir_T_T__Del,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_131 := Row_Pos_88 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_131 /= No_Token_Index then

   Row_Pos_88 := Token_Pos_131;

else
   Row_Pos_88 := No_Token_Index;
   goto Exit_Row_87_0;

end if;


Defer_Res_142 :=
   Expr_List_Extract_Parse_0 (Parser, Row_Pos_88);
Defer_Pos_142 := Parser.Current_Pos;



if Defer_Pos_142 /= No_Token_Index then

   Row_Pos_88 := Defer_Pos_142;

else
   Row_Pos_88 := No_Token_Index;
   goto Exit_Row_87_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_87_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_88 /= No_Token_Index then

   Transform_Res_70 := Allocate_Del_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_70,
      Kind => Turkixir_Del_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_88 = Pos
                            then No_Token_Index
                            else Row_Pos_88 - 1));

      Initialize_Fields_For_Del_Stmt
        (Self => Transform_Res_70, Del_Stmt_F_Exprs => Defer_Res_142);

         if Defer_Res_142 /= null and then Is_Incomplete (Defer_Res_142) then
            Transform_Res_70.Last_Attempted_Child := 0;
         elsif Defer_Res_142 /= null and then not Is_Ghost (Defer_Res_142) then
            Transform_Res_70.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Del_Stmt_Transform_Parse_0_Memo,
      Row_Pos_88 /= No_Token_Index,
      Transform_Res_70,
      Pos,
      Row_Pos_88);


   Parser.Current_Pos := Row_Pos_88;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_70;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Del_Stmt_Transform_Parse_0;

   


function Pass_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Pass_Stmt
is
   use Bare_Pass_Stmt_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_89 :
            Token_Index
               := No_Token_Index;
      Token_Pos_132 :
            Token_Index
               := No_Token_Index;
      Token_Res_132 :
            Token_Index
               := No_Token_Index;
      Transform_Res_71 :
            Bare_Pass_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Pass_Stmt_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_71 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_71;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_71;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_89 := Pos;



--  Start tok_code

Token_Res_132 := Row_Pos_89;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_132));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Pass)
   then
       Token_Pos_132 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_89 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_89,
             Expected_Token_Id => Turkixir_T_T__Pass,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_132 := Row_Pos_89 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_132 /= No_Token_Index then

   Row_Pos_89 := Token_Pos_132;

else
   Row_Pos_89 := No_Token_Index;
   goto Exit_Row_88_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_88_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_89 /= No_Token_Index then

   Transform_Res_71 := Allocate_Pass_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_71,
      Kind => Turkixir_Pass_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_89 = Pos
                            then No_Token_Index
                            else Row_Pos_89 - 1));




end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Pass_Stmt_Transform_Parse_0_Memo,
      Row_Pos_89 /= No_Token_Index,
      Transform_Res_71,
      Pos,
      Row_Pos_89);


   Parser.Current_Pos := Row_Pos_89;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_71;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Pass_Stmt_Transform_Parse_0;

   


function Continue_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Continue_Stmt
is
   use Bare_Continue_Stmt_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_90 :
            Token_Index
               := No_Token_Index;
      Token_Pos_133 :
            Token_Index
               := No_Token_Index;
      Token_Res_133 :
            Token_Index
               := No_Token_Index;
      Transform_Res_72 :
            Bare_Continue_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Continue_Stmt_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_72 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_72;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_72;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_90 := Pos;



--  Start tok_code

Token_Res_133 := Row_Pos_90;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_133));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Continue)
   then
       Token_Pos_133 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_90 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_90,
             Expected_Token_Id => Turkixir_T_T__Continue,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_133 := Row_Pos_90 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_133 /= No_Token_Index then

   Row_Pos_90 := Token_Pos_133;

else
   Row_Pos_90 := No_Token_Index;
   goto Exit_Row_89_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_89_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_90 /= No_Token_Index then

   Transform_Res_72 := Allocate_Continue_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_72,
      Kind => Turkixir_Continue_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_90 = Pos
                            then No_Token_Index
                            else Row_Pos_90 - 1));




end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Continue_Stmt_Transform_Parse_0_Memo,
      Row_Pos_90 /= No_Token_Index,
      Transform_Res_72,
      Pos,
      Row_Pos_90);


   Parser.Current_Pos := Row_Pos_90;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_72;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Continue_Stmt_Transform_Parse_0;

   


function Return_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Return_Stmt
is
   use Bare_Return_Stmt_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_91 :
            Token_Index
               := No_Token_Index;
      Token_Pos_134 :
            Token_Index
               := No_Token_Index;
      Token_Res_134 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_148 :
            Token_Index
               := No_Token_Index;
      Defer_Res_148 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Transform_Res_73 :
            Bare_Return_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Return_Stmt_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_73 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_73;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_73;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_91 := Pos;



--  Start tok_code

Token_Res_134 := Row_Pos_91;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_134));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Return)
   then
       Token_Pos_134 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_91 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_91,
             Expected_Token_Id => Turkixir_T_T__Return,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_134 := Row_Pos_91 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_134 /= No_Token_Index then

   Row_Pos_91 := Token_Pos_134;

else
   Row_Pos_91 := No_Token_Index;
   goto Exit_Row_90_0;

end if;


--  Start opt_code




Defer_Res_148 :=
   Test_List_Extract_Parse_0 (Parser, Row_Pos_91);
Defer_Pos_148 := Parser.Current_Pos;


if Defer_Pos_148 = No_Token_Index then

        Defer_Res_148 :=
           Allocate_Expr_List (Parser.Mem_Pool);
         Initialize
           (Self              => Defer_Res_148,
            Kind              => Turkixir_Expr_List,
            Unit              => Parser.Unit,
            Token_Start_Index => Row_Pos_91 - 1,
            Token_End_Index   => No_Token_Index);
         Initialize_List
           (Self   => Defer_Res_148,
            Parser => Parser,
            Count  => 0);


    Defer_Pos_148 := Row_Pos_91;


end if;

--  End opt_code



if Defer_Pos_148 /= No_Token_Index then

   Row_Pos_91 := Defer_Pos_148;

else
   Row_Pos_91 := No_Token_Index;
   goto Exit_Row_90_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_90_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_91 /= No_Token_Index then

   Transform_Res_73 := Allocate_Return_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_73,
      Kind => Turkixir_Return_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_91 = Pos
                            then No_Token_Index
                            else Row_Pos_91 - 1));

      Initialize_Fields_For_Return_Stmt
        (Self => Transform_Res_73, Return_Stmt_F_Exprs => Defer_Res_148);

         if Defer_Res_148 /= null and then Is_Incomplete (Defer_Res_148) then
            Transform_Res_73.Last_Attempted_Child := 0;
         elsif Defer_Res_148 /= null and then not Is_Ghost (Defer_Res_148) then
            Transform_Res_73.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Return_Stmt_Transform_Parse_0_Memo,
      Row_Pos_91 /= No_Token_Index,
      Transform_Res_73,
      Pos,
      Row_Pos_91);


   Parser.Current_Pos := Row_Pos_91;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_73;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Return_Stmt_Transform_Parse_0;

   


function Raise_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Raise_Stmt
is
   use Bare_Raise_Stmt_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_92 :
            Token_Index
               := No_Token_Index;
      Token_Pos_135 :
            Token_Index
               := No_Token_Index;
      Token_Res_135 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_149 :
            Token_Index
               := No_Token_Index;
      Defer_Res_149 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Transform_Res_74 :
            Bare_Raise_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Raise_Stmt_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_74 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_74;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_74;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_92 := Pos;



--  Start tok_code

Token_Res_135 := Row_Pos_92;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_135));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Raise)
   then
       Token_Pos_135 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_92 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_92,
             Expected_Token_Id => Turkixir_T_T__Raise,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_135 := Row_Pos_92 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_135 /= No_Token_Index then

   Row_Pos_92 := Token_Pos_135;

else
   Row_Pos_92 := No_Token_Index;
   goto Exit_Row_91_0;

end if;


--  Start opt_code




Defer_Res_149 :=
   Test_List_Extract_Parse_0 (Parser, Row_Pos_92);
Defer_Pos_149 := Parser.Current_Pos;


if Defer_Pos_149 = No_Token_Index then

        Defer_Res_149 :=
           Allocate_Expr_List (Parser.Mem_Pool);
         Initialize
           (Self              => Defer_Res_149,
            Kind              => Turkixir_Expr_List,
            Unit              => Parser.Unit,
            Token_Start_Index => Row_Pos_92 - 1,
            Token_End_Index   => No_Token_Index);
         Initialize_List
           (Self   => Defer_Res_149,
            Parser => Parser,
            Count  => 0);


    Defer_Pos_149 := Row_Pos_92;


end if;

--  End opt_code



if Defer_Pos_149 /= No_Token_Index then

   Row_Pos_92 := Defer_Pos_149;

else
   Row_Pos_92 := No_Token_Index;
   goto Exit_Row_91_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_91_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_92 /= No_Token_Index then

   Transform_Res_74 := Allocate_Raise_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_74,
      Kind => Turkixir_Raise_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_92 = Pos
                            then No_Token_Index
                            else Row_Pos_92 - 1));

      Initialize_Fields_For_Raise_Stmt
        (Self => Transform_Res_74, Raise_Stmt_F_Exprs => Defer_Res_149);

         if Defer_Res_149 /= null and then Is_Incomplete (Defer_Res_149) then
            Transform_Res_74.Last_Attempted_Child := 0;
         elsif Defer_Res_149 /= null and then not Is_Ghost (Defer_Res_149) then
            Transform_Res_74.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Raise_Stmt_Transform_Parse_0_Memo,
      Row_Pos_92 /= No_Token_Index,
      Transform_Res_74,
      Pos,
      Row_Pos_92);


   Parser.Current_Pos := Row_Pos_92;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_74;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Raise_Stmt_Transform_Parse_0;

   


function Yield_Stmt_Defer_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Yield_Expr
is
   use Bare_Yield_Expr_Memos;

   Call_Depth : aliased Natural;

      Defer_Pos_150 :
            Token_Index
               := No_Token_Index;
      Defer_Res_150 :
            Bare_Yield_Expr
               := No_Bare_Turkixir_Node;

      Mem_Pos : Token_Index := Pos;
      Mem_Res : Bare_Yield_Expr := No_Bare_Turkixir_Node;

   M : Memo_Entry := Get (Parser.Private_Part.Yield_Stmt_Defer_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Defer_Res_150 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Defer_Res_150;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Defer_Res_150;
   end if;

       Set (Parser.Private_Part.Yield_Stmt_Defer_Parse_0_Memo, False, Defer_Res_150, Pos, Mem_Pos);

       <<Try_Again>>



   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
Defer_Res_150 :=
   Yield_Expr_Transform_Parse_0 (Parser, Pos);
Defer_Pos_150 := Parser.Current_Pos;


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

      if Defer_Pos_150 > Mem_Pos then
         Mem_Pos := Defer_Pos_150;
         Mem_Res := Defer_Res_150;
         Set
           (Parser.Private_Part.Yield_Stmt_Defer_Parse_0_Memo,
            Defer_Pos_150 /= No_Token_Index,
            Defer_Res_150,
            Pos,
            Defer_Pos_150);
         goto Try_Again;

      elsif Mem_Pos > Pos then
         Defer_Res_150 := Mem_Res;
         Defer_Pos_150 := Mem_Pos;
         goto No_Memo;
      end if;

   Set
     (Parser.Private_Part.Yield_Stmt_Defer_Parse_0_Memo,
      Defer_Pos_150 /= No_Token_Index,
      Defer_Res_150,
      Pos,
      Defer_Pos_150);

       <<No_Memo>>

   Parser.Current_Pos := Defer_Pos_150;

   Exit_Call (Parser, Call_Depth);
   return Defer_Res_150;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Yield_Stmt_Defer_Parse_0;

   


function Flow_Stmt_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node
is
   use Bare_Turkixir_Node_Memos;

   Call_Depth : aliased Natural;

      Defer_Pos_143 :
            Token_Index
               := No_Token_Index;
      Defer_Res_143 :
            Bare_Break_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_144 :
            Token_Index
               := No_Token_Index;
      Defer_Res_144 :
            Bare_Continue_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_145 :
            Token_Index
               := No_Token_Index;
      Defer_Res_145 :
            Bare_Return_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_146 :
            Token_Index
               := No_Token_Index;
      Defer_Res_146 :
            Bare_Raise_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_147 :
            Token_Index
               := No_Token_Index;
      Defer_Res_147 :
            Bare_Yield_Expr
               := No_Bare_Turkixir_Node;
      Or_Pos_34 :
            Token_Index
               := No_Token_Index;
      Or_Res_34 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Flow_Stmt_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_34 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_34;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_34;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_34 := No_Token_Index;
Or_Res_34 := No_Bare_Turkixir_Node;
    
Defer_Res_143 :=
   Break_Stmt_Transform_Parse_0 (Parser, Pos);
Defer_Pos_143 := Parser.Current_Pos;

    if Defer_Pos_143 /= No_Token_Index then
        Or_Pos_34 := Defer_Pos_143;
        Or_Res_34 := Defer_Res_143;
        goto Exit_Or_34;
    end if;
    
Defer_Res_144 :=
   Continue_Stmt_Transform_Parse_0 (Parser, Pos);
Defer_Pos_144 := Parser.Current_Pos;

    if Defer_Pos_144 /= No_Token_Index then
        Or_Pos_34 := Defer_Pos_144;
        Or_Res_34 := Defer_Res_144;
        goto Exit_Or_34;
    end if;
    
Defer_Res_145 :=
   Return_Stmt_Transform_Parse_0 (Parser, Pos);
Defer_Pos_145 := Parser.Current_Pos;

    if Defer_Pos_145 /= No_Token_Index then
        Or_Pos_34 := Defer_Pos_145;
        Or_Res_34 := Defer_Res_145;
        goto Exit_Or_34;
    end if;
    
Defer_Res_146 :=
   Raise_Stmt_Transform_Parse_0 (Parser, Pos);
Defer_Pos_146 := Parser.Current_Pos;

    if Defer_Pos_146 /= No_Token_Index then
        Or_Pos_34 := Defer_Pos_146;
        Or_Res_34 := Defer_Res_146;
        goto Exit_Or_34;
    end if;
    
Defer_Res_147 :=
   Yield_Stmt_Defer_Parse_0 (Parser, Pos);
Defer_Pos_147 := Parser.Current_Pos;

    if Defer_Pos_147 /= No_Token_Index then
        Or_Pos_34 := Defer_Pos_147;
        Or_Res_34 := Defer_Res_147;
        goto Exit_Or_34;
    end if;
<<Exit_Or_34>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Flow_Stmt_Or_Parse_0_Memo,
      Or_Pos_34 /= No_Token_Index,
      Or_Res_34,
      Pos,
      Or_Pos_34);


   Parser.Current_Pos := Or_Pos_34;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_34;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Flow_Stmt_Or_Parse_0;

   


function Dotted_Name_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Name
is
   use Bare_Name_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_96 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_158 :
            Token_Index
               := No_Token_Index;
      Defer_Res_158 :
            Bare_Name
               := No_Bare_Turkixir_Node;
      Token_Pos_140 :
            Token_Index
               := No_Token_Index;
      Token_Res_140 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_159 :
            Token_Index
               := No_Token_Index;
      Defer_Res_159 :
            Bare_Id
               := No_Bare_Turkixir_Node;
      Transform_Res_77 :
            Bare_Dotted_Name
               := No_Bare_Turkixir_Node;
      Defer_Pos_160 :
            Token_Index
               := No_Token_Index;
      Defer_Res_160 :
            Bare_Id
               := No_Bare_Turkixir_Node;
      Or_Pos_37 :
            Token_Index
               := No_Token_Index;
      Or_Res_37 :
            Bare_Name
               := No_Bare_Turkixir_Node;

      Mem_Pos : Token_Index := Pos;
      Mem_Res : Bare_Name := No_Bare_Turkixir_Node;

   M : Memo_Entry := Get (Parser.Private_Part.Dotted_Name_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_37 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_37;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_37;
   end if;

       Set (Parser.Private_Part.Dotted_Name_Or_Parse_0_Memo, False, Or_Res_37, Pos, Mem_Pos);

       <<Try_Again>>



   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_37 := No_Token_Index;
Or_Res_37 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_96 := Pos;



Defer_Res_158 :=
   Dotted_Name_Or_Parse_0 (Parser, Row_Pos_96);
Defer_Pos_158 := Parser.Current_Pos;



if Defer_Pos_158 /= No_Token_Index then

   Row_Pos_96 := Defer_Pos_158;

else
   Row_Pos_96 := No_Token_Index;
   goto Exit_Row_95_0;

end if;


--  Start tok_code

Token_Res_140 := Row_Pos_96;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_140));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Dot)
   then
       Token_Pos_140 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_96 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_96,
             Expected_Token_Id => Turkixir_T_T__Dot,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_140 := Row_Pos_96 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_140 /= No_Token_Index then

   Row_Pos_96 := Token_Pos_140;

else
   Row_Pos_96 := No_Token_Index;
   goto Exit_Row_95_0;

end if;


Defer_Res_159 :=
   Name_Transform_Parse_0 (Parser, Row_Pos_96);
Defer_Pos_159 := Parser.Current_Pos;



if Defer_Pos_159 /= No_Token_Index then

   Row_Pos_96 := Defer_Pos_159;

else
   Row_Pos_96 := No_Token_Index;
   goto Exit_Row_95_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_95_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_96 /= No_Token_Index then

   Transform_Res_77 := Allocate_Dotted_Name (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_77,
      Kind => Turkixir_Dotted_Name,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_96 = Pos
                            then No_Token_Index
                            else Row_Pos_96 - 1));

      Initialize_Fields_For_Dotted_Name
        (Self => Transform_Res_77, Dotted_Name_F_Prefix => Defer_Res_158, Dotted_Name_F_Suffix => Defer_Res_159);

         if Defer_Res_158 /= null and then Is_Incomplete (Defer_Res_158) then
            Transform_Res_77.Last_Attempted_Child := 0;
         elsif Defer_Res_158 /= null and then not Is_Ghost (Defer_Res_158) then
            Transform_Res_77.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_159 /= null and then Is_Incomplete (Defer_Res_159) then
            Transform_Res_77.Last_Attempted_Child := 0;
         elsif Defer_Res_159 /= null and then not Is_Ghost (Defer_Res_159) then
            Transform_Res_77.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_96 /= No_Token_Index then
        Or_Pos_37 := Row_Pos_96;
        Or_Res_37 := Transform_Res_77;
        goto Exit_Or_37;
    end if;
    
Defer_Res_160 :=
   Name_Transform_Parse_0 (Parser, Pos);
Defer_Pos_160 := Parser.Current_Pos;

    if Defer_Pos_160 /= No_Token_Index then
        Or_Pos_37 := Defer_Pos_160;
        Or_Res_37 := Defer_Res_160;
        goto Exit_Or_37;
    end if;
<<Exit_Or_37>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

      if Or_Pos_37 > Mem_Pos then
         Mem_Pos := Or_Pos_37;
         Mem_Res := Or_Res_37;
         Set
           (Parser.Private_Part.Dotted_Name_Or_Parse_0_Memo,
            Or_Pos_37 /= No_Token_Index,
            Or_Res_37,
            Pos,
            Or_Pos_37);
         goto Try_Again;

      elsif Mem_Pos > Pos then
         Or_Res_37 := Mem_Res;
         Or_Pos_37 := Mem_Pos;
         goto No_Memo;
      end if;

   Set
     (Parser.Private_Part.Dotted_Name_Or_Parse_0_Memo,
      Or_Pos_37 /= No_Token_Index,
      Or_Res_37,
      Pos,
      Or_Pos_37);

       <<No_Memo>>

   Parser.Current_Pos := Or_Pos_37;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_37;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Dotted_Name_Or_Parse_0;

   


function Dotted_As_Name_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_As_Name_Node
is
   use Bare_As_Name_Node_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_95 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_156 :
            Token_Index
               := No_Token_Index;
      Defer_Res_156 :
            Bare_Name
               := No_Bare_Turkixir_Node;
      Token_Pos_139 :
            Token_Index
               := No_Token_Index;
      Token_Res_139 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_157 :
            Token_Index
               := No_Token_Index;
      Defer_Res_157 :
            Bare_Id
               := No_Bare_Turkixir_Node;
      Transform_Res_76 :
            Bare_As_Name_Node
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Dotted_As_Name_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_76 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_76;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_76;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_95 := Pos;



Defer_Res_156 :=
   Dotted_Name_Or_Parse_0 (Parser, Row_Pos_95);
Defer_Pos_156 := Parser.Current_Pos;



if Defer_Pos_156 /= No_Token_Index then

   Row_Pos_95 := Defer_Pos_156;

else
   Row_Pos_95 := No_Token_Index;
   goto Exit_Row_94_0;

end if;


--  Start tok_code

Token_Res_139 := Row_Pos_95;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_139));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__As)
   then
       Token_Pos_139 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_95 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_95,
             Expected_Token_Id => Turkixir_T_T__As,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_139 := Row_Pos_95 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_139 /= No_Token_Index then

   Row_Pos_95 := Token_Pos_139;

else
   Row_Pos_95 := No_Token_Index;
   goto Exit_Row_94_0;

end if;


Defer_Res_157 :=
   Name_Transform_Parse_0 (Parser, Row_Pos_95);
Defer_Pos_157 := Parser.Current_Pos;



if Defer_Pos_157 /= No_Token_Index then

   Row_Pos_95 := Defer_Pos_157;

else
   Row_Pos_95 := No_Token_Index;
   goto Exit_Row_94_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_94_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_95 /= No_Token_Index then

   Transform_Res_76 := Allocate_As_Name_Node (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_76,
      Kind => Turkixir_As_Name_Node,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_95 = Pos
                            then No_Token_Index
                            else Row_Pos_95 - 1));

      Initialize_Fields_For_As_Name_Node
        (Self => Transform_Res_76, As_Name_Node_F_Imported => Defer_Res_156, As_Name_Node_F_As_Name => Defer_Res_157);

         if Defer_Res_156 /= null and then Is_Incomplete (Defer_Res_156) then
            Transform_Res_76.Last_Attempted_Child := 0;
         elsif Defer_Res_156 /= null and then not Is_Ghost (Defer_Res_156) then
            Transform_Res_76.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_157 /= null and then Is_Incomplete (Defer_Res_157) then
            Transform_Res_76.Last_Attempted_Child := 0;
         elsif Defer_Res_157 /= null and then not Is_Ghost (Defer_Res_157) then
            Transform_Res_76.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Dotted_As_Name_Transform_Parse_0_Memo,
      Row_Pos_95 /= No_Token_Index,
      Transform_Res_76,
      Pos,
      Row_Pos_95);


   Parser.Current_Pos := Row_Pos_95;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_76;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Dotted_As_Name_Transform_Parse_0;

   


function Dotted_As_Names_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node_List
is
   use Bare_Turkixir_Node_List_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_94 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_15 :
            Token_Index
               := No_Token_Index;
      Tmp_List_15 :
            Free_Parse_List;
      Defer_Pos_154 :
            Token_Index
               := No_Token_Index;
      Defer_Res_154 :
            Bare_As_Name_Node
               := No_Bare_Turkixir_Node;
      Defer_Pos_155 :
            Token_Index
               := No_Token_Index;
      Defer_Res_155 :
            Bare_Name
               := No_Bare_Turkixir_Node;
      Or_Pos_36 :
            Token_Index
               := No_Token_Index;
      Or_Res_36 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Token_Pos_137 :
            Token_Index
               := No_Token_Index;
      Token_Res_137 :
            Token_Index
               := No_Token_Index;
      List_Pos_15 :
            Token_Index
               := No_Token_Index;
      List_Res_15 :
            Bare_Turkixir_Node_List
               := No_Bare_Turkixir_Node;
      Token_Pos_138 :
            Token_Index
               := No_Token_Index;
      Token_Res_138 :
            Token_Index
               := No_Token_Index;


   M : Memo_Entry := Get (Parser.Private_Part.Dotted_As_Names_Extract_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res_15 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return List_Res_15;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return List_Res_15;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start row_code

Row_Pos_94 := Pos;



--  Start list_code

    List_Pos_15 := No_Token_Index;



Lst_Cpos_15 := Row_Pos_94;
Tmp_List_15 := Get_Parse_List (Parser);

loop
   
--  Start or_code

Or_Pos_36 := No_Token_Index;
Or_Res_36 := No_Bare_Turkixir_Node;
    
Defer_Res_154 :=
   Dotted_As_Name_Transform_Parse_0 (Parser, Lst_Cpos_15);
Defer_Pos_154 := Parser.Current_Pos;

    if Defer_Pos_154 /= No_Token_Index then
        Or_Pos_36 := Defer_Pos_154;
        Or_Res_36 := Defer_Res_154;
        goto Exit_Or_36;
    end if;
    
Defer_Res_155 :=
   Dotted_Name_Or_Parse_0 (Parser, Lst_Cpos_15);
Defer_Pos_155 := Parser.Current_Pos;

    if Defer_Pos_155 /= No_Token_Index then
        Or_Pos_36 := Defer_Pos_155;
        Or_Res_36 := Defer_Res_155;
        goto Exit_Or_36;
    end if;
<<Exit_Or_36>>

--  End or_code


   exit when Or_Pos_36 = No_Token_Index;

   List_Pos_15 := Or_Pos_36;
   Lst_Cpos_15 := List_Pos_15;

   Tmp_List_15.Nodes.Append (Or_Res_36);

      
--  Start tok_code

Token_Res_137 := Lst_Cpos_15;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_137));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_137 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos_15 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos_15,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_137 := Lst_Cpos_15 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos_137 /= No_Token_Index then
          Lst_Cpos_15 := Token_Pos_137;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_15.Nodes.Length;
begin
   List_Res_15 :=
      Allocate_Turkixir_Node_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_94;
      Token_End := (if Lst_Cpos_15 = Row_Pos_94
                    then Row_Pos_94
                    else Lst_Cpos_15 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_94, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_15,
      Kind              => Turkixir_Turkixir_Node_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_15,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_15.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_15.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_15);

--  End list_code



if List_Pos_15 /= No_Token_Index then

   Row_Pos_94 := List_Pos_15;

else
   Row_Pos_94 := No_Token_Index;
   goto Exit_Row_93_0;

end if;


--  Start opt_code




--  Start tok_code

Token_Res_138 := Row_Pos_94;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_138));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_138 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_94 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_94,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_138 := Row_Pos_94 + 1;
   end if;
end;

--  End tok_code


if Token_Pos_138 = No_Token_Index then

        Token_Res_138 := No_Token_Index;


    Token_Pos_138 := Row_Pos_94;


end if;

--  End opt_code



if Token_Pos_138 /= No_Token_Index then

   Row_Pos_94 := Token_Pos_138;

else
   Row_Pos_94 := No_Token_Index;
   goto Exit_Row_93_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_93_0>>
pragma Warnings (On, "referenced");

--  End row_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Dotted_As_Names_Extract_Parse_0_Memo,
      Row_Pos_94 /= No_Token_Index,
      List_Res_15,
      Pos,
      Row_Pos_94);


   Parser.Current_Pos := Row_Pos_94;

   Exit_Call (Parser, Call_Depth);
   return List_Res_15;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Dotted_As_Names_Extract_Parse_0;

   


function Import_Name_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Import_Name
is
   use Bare_Import_Name_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_93 :
            Token_Index
               := No_Token_Index;
      Token_Pos_136 :
            Token_Index
               := No_Token_Index;
      Token_Res_136 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_153 :
            Token_Index
               := No_Token_Index;
      Defer_Res_153 :
            Bare_Turkixir_Node_List
               := No_Bare_Turkixir_Node;
      Transform_Res_75 :
            Bare_Import_Name
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Import_Name_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_75 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_75;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_75;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_93 := Pos;



--  Start tok_code

Token_Res_136 := Row_Pos_93;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_136));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Import)
   then
       Token_Pos_136 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_93 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_93,
             Expected_Token_Id => Turkixir_T_T__Import,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_136 := Row_Pos_93 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_136 /= No_Token_Index then

   Row_Pos_93 := Token_Pos_136;

else
   Row_Pos_93 := No_Token_Index;
   goto Exit_Row_92_0;

end if;


Defer_Res_153 :=
   Dotted_As_Names_Extract_Parse_0 (Parser, Row_Pos_93);
Defer_Pos_153 := Parser.Current_Pos;



if Defer_Pos_153 /= No_Token_Index then

   Row_Pos_93 := Defer_Pos_153;

else
   Row_Pos_93 := No_Token_Index;
   goto Exit_Row_92_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_92_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_93 /= No_Token_Index then

   Transform_Res_75 := Allocate_Import_Name (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_75,
      Kind => Turkixir_Import_Name,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_93 = Pos
                            then No_Token_Index
                            else Row_Pos_93 - 1));

      Initialize_Fields_For_Import_Name
        (Self => Transform_Res_75, Import_Name_F_Imported_Names => Defer_Res_153);

         if Defer_Res_153 /= null and then Is_Incomplete (Defer_Res_153) then
            Transform_Res_75.Last_Attempted_Child := 0;
         elsif Defer_Res_153 /= null and then not Is_Ghost (Defer_Res_153) then
            Transform_Res_75.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Import_Name_Transform_Parse_0_Memo,
      Row_Pos_93 /= No_Token_Index,
      Transform_Res_75,
      Pos,
      Row_Pos_93);


   Parser.Current_Pos := Row_Pos_93;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_75;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Import_Name_Transform_Parse_0;

   


function Dot_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Dot
is
   use Bare_Dot_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_101 :
            Token_Index
               := No_Token_Index;
      Token_Pos_146 :
            Token_Index
               := No_Token_Index;
      Token_Res_146 :
            Token_Index
               := No_Token_Index;
      Transform_Res_81 :
            Bare_Dot
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Dot_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_81 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_81;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_81;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_101 := Pos;



--  Start tok_code

Token_Res_146 := Row_Pos_101;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_146));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Dot)
   then
       Token_Pos_146 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_101 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_101,
             Expected_Token_Id => Turkixir_T_T__Dot,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_146 := Row_Pos_101 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_146 /= No_Token_Index then

   Row_Pos_101 := Token_Pos_146;

else
   Row_Pos_101 := No_Token_Index;
   goto Exit_Row_98_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_98_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_101 /= No_Token_Index then

   Transform_Res_81 := Allocate_Dot (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_81,
      Kind => Turkixir_Dot,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_101 = Pos
                            then No_Token_Index
                            else Row_Pos_101 - 1));




end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Dot_Transform_Parse_0_Memo,
      Row_Pos_101 /= No_Token_Index,
      Transform_Res_81,
      Pos,
      Row_Pos_101);


   Parser.Current_Pos := Row_Pos_101;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_81;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Dot_Transform_Parse_0;

   


function Import_As_Names_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node_List
is
   use Bare_Turkixir_Node_List_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_102 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_17 :
            Token_Index
               := No_Token_Index;
      Tmp_List_17 :
            Free_Parse_List;
      Defer_Pos_166 :
            Token_Index
               := No_Token_Index;
      Defer_Res_166 :
            Bare_As_Name_Node
               := No_Bare_Turkixir_Node;
      Defer_Pos_167 :
            Token_Index
               := No_Token_Index;
      Defer_Res_167 :
            Bare_Id
               := No_Bare_Turkixir_Node;
      Or_Pos_40 :
            Token_Index
               := No_Token_Index;
      Or_Res_40 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Token_Pos_147 :
            Token_Index
               := No_Token_Index;
      Token_Res_147 :
            Token_Index
               := No_Token_Index;
      List_Pos_17 :
            Token_Index
               := No_Token_Index;
      List_Res_17 :
            Bare_Turkixir_Node_List
               := No_Bare_Turkixir_Node;
      Token_Pos_148 :
            Token_Index
               := No_Token_Index;
      Token_Res_148 :
            Token_Index
               := No_Token_Index;


   M : Memo_Entry := Get (Parser.Private_Part.Import_As_Names_Extract_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res_17 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return List_Res_17;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return List_Res_17;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start row_code

Row_Pos_102 := Pos;



--  Start list_code

    List_Pos_17 := No_Token_Index;



Lst_Cpos_17 := Row_Pos_102;
Tmp_List_17 := Get_Parse_List (Parser);

loop
   
--  Start or_code

Or_Pos_40 := No_Token_Index;
Or_Res_40 := No_Bare_Turkixir_Node;
    
Defer_Res_166 :=
   As_Name_Transform_Parse_0 (Parser, Lst_Cpos_17);
Defer_Pos_166 := Parser.Current_Pos;

    if Defer_Pos_166 /= No_Token_Index then
        Or_Pos_40 := Defer_Pos_166;
        Or_Res_40 := Defer_Res_166;
        goto Exit_Or_40;
    end if;
    
Defer_Res_167 :=
   Name_Transform_Parse_0 (Parser, Lst_Cpos_17);
Defer_Pos_167 := Parser.Current_Pos;

    if Defer_Pos_167 /= No_Token_Index then
        Or_Pos_40 := Defer_Pos_167;
        Or_Res_40 := Defer_Res_167;
        goto Exit_Or_40;
    end if;
<<Exit_Or_40>>

--  End or_code


   exit when Or_Pos_40 = No_Token_Index;

   List_Pos_17 := Or_Pos_40;
   Lst_Cpos_17 := List_Pos_17;

   Tmp_List_17.Nodes.Append (Or_Res_40);

      
--  Start tok_code

Token_Res_147 := Lst_Cpos_17;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_147));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_147 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos_17 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos_17,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_147 := Lst_Cpos_17 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos_147 /= No_Token_Index then
          Lst_Cpos_17 := Token_Pos_147;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_17.Nodes.Length;
begin
   List_Res_17 :=
      Allocate_Turkixir_Node_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_102;
      Token_End := (if Lst_Cpos_17 = Row_Pos_102
                    then Row_Pos_102
                    else Lst_Cpos_17 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_102, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_17,
      Kind              => Turkixir_Turkixir_Node_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_17,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_17.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_17.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_17);

--  End list_code



if List_Pos_17 /= No_Token_Index then

   Row_Pos_102 := List_Pos_17;

else
   Row_Pos_102 := No_Token_Index;
   goto Exit_Row_101_0;

end if;


--  Start opt_code




--  Start tok_code

Token_Res_148 := Row_Pos_102;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_148));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_148 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_102 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_102,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_148 := Row_Pos_102 + 1;
   end if;
end;

--  End tok_code


if Token_Pos_148 = No_Token_Index then

        Token_Res_148 := No_Token_Index;


    Token_Pos_148 := Row_Pos_102;


end if;

--  End opt_code



if Token_Pos_148 /= No_Token_Index then

   Row_Pos_102 := Token_Pos_148;

else
   Row_Pos_102 := No_Token_Index;
   goto Exit_Row_101_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_101_0>>
pragma Warnings (On, "referenced");

--  End row_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Import_As_Names_Extract_Parse_0_Memo,
      Row_Pos_102 /= No_Token_Index,
      List_Res_17,
      Pos,
      Row_Pos_102);


   Parser.Current_Pos := Row_Pos_102;

   Exit_Call (Parser, Call_Depth);
   return List_Res_17;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Import_As_Names_Extract_Parse_0;

   


function Import_From_Transform_Parse_2
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Import_From
is
   use Bare_Import_From_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_97 :
            Token_Index
               := No_Token_Index;
      Token_Pos_141 :
            Token_Index
               := No_Token_Index;
      Token_Res_141 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_161 :
            Token_Index
               := No_Token_Index;
      Defer_Res_161 :
            Bare_Name
               := No_Bare_Turkixir_Node;
      Row_Pos_98 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_16 :
            Token_Index
               := No_Token_Index;
      Tmp_List_16 :
            Free_Parse_List;
      Defer_Pos_162 :
            Token_Index
               := No_Token_Index;
      Defer_Res_162 :
            Bare_Dot
               := No_Bare_Turkixir_Node;
      List_Pos_16 :
            Token_Index
               := No_Token_Index;
      List_Res_16 :
            Bare_Dot_List
               := No_Bare_Turkixir_Node;
      Defer_Pos_163 :
            Token_Index
               := No_Token_Index;
      Defer_Res_163 :
            Bare_Name
               := No_Bare_Turkixir_Node;
      Transform_Res_78 :
            Bare_Rel_Name
               := No_Bare_Turkixir_Node;
      Or_Pos_38 :
            Token_Index
               := No_Token_Index;
      Or_Res_38 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Token_Pos_142 :
            Token_Index
               := No_Token_Index;
      Token_Res_142 :
            Token_Index
               := No_Token_Index;
      Row_Pos_99 :
            Token_Index
               := No_Token_Index;
      Token_Pos_143 :
            Token_Index
               := No_Token_Index;
      Token_Res_143 :
            Token_Index
               := No_Token_Index;
      Transform_Res_79 :
            Bare_Import_Star
               := No_Bare_Turkixir_Node;
      Row_Pos_100 :
            Token_Index
               := No_Token_Index;
      Token_Pos_144 :
            Token_Index
               := No_Token_Index;
      Token_Res_144 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_164 :
            Token_Index
               := No_Token_Index;
      Defer_Res_164 :
            Bare_Turkixir_Node_List
               := No_Bare_Turkixir_Node;
      Token_Pos_145 :
            Token_Index
               := No_Token_Index;
      Token_Res_145 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_165 :
            Token_Index
               := No_Token_Index;
      Defer_Res_165 :
            Bare_Turkixir_Node_List
               := No_Bare_Turkixir_Node;
      Or_Pos_39 :
            Token_Index
               := No_Token_Index;
      Or_Res_39 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Transform_Res_80 :
            Bare_Import_From
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Import_From_Transform_Parse_2_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_80 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_80;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_80;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_97 := Pos;



--  Start tok_code

Token_Res_141 := Row_Pos_97;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_141));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__From)
   then
       Token_Pos_141 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_97 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_97,
             Expected_Token_Id => Turkixir_T_T__From,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_141 := Row_Pos_97 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_141 /= No_Token_Index then

   Row_Pos_97 := Token_Pos_141;

else
   Row_Pos_97 := No_Token_Index;
   goto Exit_Row_96_0;

end if;


--  Start or_code

Or_Pos_38 := No_Token_Index;
Or_Res_38 := No_Bare_Turkixir_Node;
    
Defer_Res_161 :=
   Dotted_Name_Or_Parse_0 (Parser, Row_Pos_97);
Defer_Pos_161 := Parser.Current_Pos;

    if Defer_Pos_161 /= No_Token_Index then
        Or_Pos_38 := Defer_Pos_161;
        Or_Res_38 := Defer_Res_161;
        goto Exit_Or_38;
    end if;
    
--  Start transform_code


--  Start row_code

Row_Pos_98 := Row_Pos_97;



--  Start list_code

    List_Pos_16 := No_Token_Index;



Lst_Cpos_16 := Row_Pos_98;
Tmp_List_16 := Get_Parse_List (Parser);

loop
   
Defer_Res_162 :=
   Dot_Transform_Parse_0 (Parser, Lst_Cpos_16);
Defer_Pos_162 := Parser.Current_Pos;


   exit when Defer_Pos_162 = No_Token_Index;

   List_Pos_16 := Defer_Pos_162;
   Lst_Cpos_16 := List_Pos_16;

   Tmp_List_16.Nodes.Append (Defer_Res_162);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_16.Nodes.Length;
begin
   List_Res_16 :=
      Allocate_Dot_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_98;
      Token_End := (if Lst_Cpos_16 = Row_Pos_98
                    then Row_Pos_98
                    else Lst_Cpos_16 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_98, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_16,
      Kind              => Turkixir_Dot_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_16,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_16.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_16.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_16);

--  End list_code



if List_Pos_16 /= No_Token_Index then

   Row_Pos_98 := List_Pos_16;

else
   Row_Pos_98 := No_Token_Index;
   goto Exit_Row_97_0;

end if;


--  Start opt_code




Defer_Res_163 :=
   Dotted_Name_Or_Parse_0 (Parser, Row_Pos_98);
Defer_Pos_163 := Parser.Current_Pos;


if Defer_Pos_163 = No_Token_Index then

        Defer_Res_163 := No_Bare_Turkixir_Node;


    Defer_Pos_163 := Row_Pos_98;


end if;

--  End opt_code



if Defer_Pos_163 /= No_Token_Index then

   Row_Pos_98 := Defer_Pos_163;

else
   Row_Pos_98 := No_Token_Index;
   goto Exit_Row_97_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_97_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_98 /= No_Token_Index then

   Transform_Res_78 := Allocate_Rel_Name (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_78,
      Kind => Turkixir_Rel_Name,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_97,
      Token_End_Index   => (if Row_Pos_98 = Row_Pos_97
                            then No_Token_Index
                            else Row_Pos_98 - 1));

      Initialize_Fields_For_Rel_Name
        (Self => Transform_Res_78, Rel_Name_F_Dots => List_Res_16, Rel_Name_F_Name => Defer_Res_163);

         if List_Res_16 /= null and then Is_Incomplete (List_Res_16) then
            Transform_Res_78.Last_Attempted_Child := 0;
         elsif List_Res_16 /= null and then not Is_Ghost (List_Res_16) then
            Transform_Res_78.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_163 /= null and then Is_Incomplete (Defer_Res_163) then
            Transform_Res_78.Last_Attempted_Child := 0;
         elsif Defer_Res_163 /= null and then not Is_Ghost (Defer_Res_163) then
            Transform_Res_78.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code

    if Row_Pos_98 /= No_Token_Index then
        Or_Pos_38 := Row_Pos_98;
        Or_Res_38 := Transform_Res_78;
        goto Exit_Or_38;
    end if;
<<Exit_Or_38>>

--  End or_code



if Or_Pos_38 /= No_Token_Index then

   Row_Pos_97 := Or_Pos_38;

else
   Row_Pos_97 := No_Token_Index;
   goto Exit_Row_96_0;

end if;


--  Start tok_code

Token_Res_142 := Row_Pos_97;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_142));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Import)
   then
       Token_Pos_142 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_97 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_97,
             Expected_Token_Id => Turkixir_T_T__Import,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_142 := Row_Pos_97 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_142 /= No_Token_Index then

   Row_Pos_97 := Token_Pos_142;

else
   Row_Pos_97 := No_Token_Index;
   goto Exit_Row_96_0;

end if;


--  Start or_code

Or_Pos_39 := No_Token_Index;
Or_Res_39 := No_Bare_Turkixir_Node;
    
--  Start transform_code


--  Start row_code

Row_Pos_99 := Row_Pos_97;



--  Start tok_code

Token_Res_143 := Row_Pos_99;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_143));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Multiply)
   then
       Token_Pos_143 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_99 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_99,
             Expected_Token_Id => Turkixir_T_T__Multiply,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_143 := Row_Pos_99 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_143 /= No_Token_Index then

   Row_Pos_99 := Token_Pos_143;

else
   Row_Pos_99 := No_Token_Index;
   goto Exit_Row_99_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_99_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_99 /= No_Token_Index then

   Transform_Res_79 := Allocate_Import_Star (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_79,
      Kind => Turkixir_Import_Star,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_97,
      Token_End_Index   => (if Row_Pos_99 = Row_Pos_97
                            then No_Token_Index
                            else Row_Pos_99 - 1));




end if;

--  End transform_code

    if Row_Pos_99 /= No_Token_Index then
        Or_Pos_39 := Row_Pos_99;
        Or_Res_39 := Transform_Res_79;
        goto Exit_Or_39;
    end if;
    
--  Start row_code

Row_Pos_100 := Row_Pos_97;



--  Start tok_code

Token_Res_144 := Row_Pos_100;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_144));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_L_Par)
   then
       Token_Pos_144 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_100 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_100,
             Expected_Token_Id => Turkixir_T_T_L_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_144 := Row_Pos_100 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_144 /= No_Token_Index then

   Row_Pos_100 := Token_Pos_144;

else
   Row_Pos_100 := No_Token_Index;
   goto Exit_Row_100_0;

end if;


Defer_Res_164 :=
   Import_As_Names_Extract_Parse_0 (Parser, Row_Pos_100);
Defer_Pos_164 := Parser.Current_Pos;



if Defer_Pos_164 /= No_Token_Index then

   Row_Pos_100 := Defer_Pos_164;

else
   Row_Pos_100 := No_Token_Index;
   goto Exit_Row_100_0;

end if;


--  Start tok_code

Token_Res_145 := Row_Pos_100;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_145));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_R_Par)
   then
       Token_Pos_145 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_100 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_100,
             Expected_Token_Id => Turkixir_T_T_R_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_145 := Row_Pos_100 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_145 /= No_Token_Index then

   Row_Pos_100 := Token_Pos_145;

else
   Row_Pos_100 := No_Token_Index;
   goto Exit_Row_100_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_100_0>>
pragma Warnings (On, "referenced");

--  End row_code

    if Row_Pos_100 /= No_Token_Index then
        Or_Pos_39 := Row_Pos_100;
        Or_Res_39 := Defer_Res_164;
        goto Exit_Or_39;
    end if;
    
Defer_Res_165 :=
   Import_As_Names_Extract_Parse_0 (Parser, Row_Pos_97);
Defer_Pos_165 := Parser.Current_Pos;

    if Defer_Pos_165 /= No_Token_Index then
        Or_Pos_39 := Defer_Pos_165;
        Or_Res_39 := Defer_Res_165;
        goto Exit_Or_39;
    end if;
<<Exit_Or_39>>

--  End or_code



if Or_Pos_39 /= No_Token_Index then

   Row_Pos_97 := Or_Pos_39;

else
   Row_Pos_97 := No_Token_Index;
   goto Exit_Row_96_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_96_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_97 /= No_Token_Index then

   Transform_Res_80 := Allocate_Import_From (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_80,
      Kind => Turkixir_Import_From,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_97 = Pos
                            then No_Token_Index
                            else Row_Pos_97 - 1));

      Initialize_Fields_For_Import_From
        (Self => Transform_Res_80, Import_From_F_Rel_Name => Or_Res_38, Import_From_F_Imported => Or_Res_39);

         if Or_Res_38 /= null and then Is_Incomplete (Or_Res_38) then
            Transform_Res_80.Last_Attempted_Child := 0;
         elsif Or_Res_38 /= null and then not Is_Ghost (Or_Res_38) then
            Transform_Res_80.Last_Attempted_Child := -1;
         end if;
         if Or_Res_39 /= null and then Is_Incomplete (Or_Res_39) then
            Transform_Res_80.Last_Attempted_Child := 0;
         elsif Or_Res_39 /= null and then not Is_Ghost (Or_Res_39) then
            Transform_Res_80.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Import_From_Transform_Parse_2_Memo,
      Row_Pos_97 /= No_Token_Index,
      Transform_Res_80,
      Pos,
      Row_Pos_97);


   Parser.Current_Pos := Row_Pos_97;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_80;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Import_From_Transform_Parse_2;

   


function Import_Stmt_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Stmt
is
   use Bare_Stmt_Memos;

   Call_Depth : aliased Natural;

      Defer_Pos_151 :
            Token_Index
               := No_Token_Index;
      Defer_Res_151 :
            Bare_Import_Name
               := No_Bare_Turkixir_Node;
      Defer_Pos_152 :
            Token_Index
               := No_Token_Index;
      Defer_Res_152 :
            Bare_Import_From
               := No_Bare_Turkixir_Node;
      Or_Pos_35 :
            Token_Index
               := No_Token_Index;
      Or_Res_35 :
            Bare_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Import_Stmt_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_35 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_35;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_35;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_35 := No_Token_Index;
Or_Res_35 := No_Bare_Turkixir_Node;
    
Defer_Res_151 :=
   Import_Name_Transform_Parse_0 (Parser, Pos);
Defer_Pos_151 := Parser.Current_Pos;

    if Defer_Pos_151 /= No_Token_Index then
        Or_Pos_35 := Defer_Pos_151;
        Or_Res_35 := Defer_Res_151;
        goto Exit_Or_35;
    end if;
    
Defer_Res_152 :=
   Import_From_Transform_Parse_2 (Parser, Pos);
Defer_Pos_152 := Parser.Current_Pos;

    if Defer_Pos_152 /= No_Token_Index then
        Or_Pos_35 := Defer_Pos_152;
        Or_Res_35 := Defer_Res_152;
        goto Exit_Or_35;
    end if;
<<Exit_Or_35>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Import_Stmt_Or_Parse_0_Memo,
      Or_Pos_35 /= No_Token_Index,
      Or_Res_35,
      Pos,
      Or_Pos_35);


   Parser.Current_Pos := Or_Pos_35;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_35;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Import_Stmt_Or_Parse_0;

   


function Global_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Global_Stmt
is
   use Bare_Global_Stmt_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_103 :
            Token_Index
               := No_Token_Index;
      Token_Pos_149 :
            Token_Index
               := No_Token_Index;
      Token_Res_149 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_168 :
            Token_Index
               := No_Token_Index;
      Defer_Res_168 :
            Bare_Id_List
               := No_Bare_Turkixir_Node;
      Transform_Res_82 :
            Bare_Global_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Global_Stmt_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_82 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_82;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_82;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_103 := Pos;



--  Start tok_code

Token_Res_149 := Row_Pos_103;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_149));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Global)
   then
       Token_Pos_149 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_103 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_103,
             Expected_Token_Id => Turkixir_T_T__Global,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_149 := Row_Pos_103 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_149 /= No_Token_Index then

   Row_Pos_103 := Token_Pos_149;

else
   Row_Pos_103 := No_Token_Index;
   goto Exit_Row_102_0;

end if;


Defer_Res_168 :=
   Name_List_Extract_Parse_0 (Parser, Row_Pos_103);
Defer_Pos_168 := Parser.Current_Pos;



if Defer_Pos_168 /= No_Token_Index then

   Row_Pos_103 := Defer_Pos_168;

else
   Row_Pos_103 := No_Token_Index;
   goto Exit_Row_102_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_102_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_103 /= No_Token_Index then

   Transform_Res_82 := Allocate_Global_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_82,
      Kind => Turkixir_Global_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_103 = Pos
                            then No_Token_Index
                            else Row_Pos_103 - 1));

      Initialize_Fields_For_Global_Stmt
        (Self => Transform_Res_82, Global_Stmt_F_Names => Defer_Res_168);

         if Defer_Res_168 /= null and then Is_Incomplete (Defer_Res_168) then
            Transform_Res_82.Last_Attempted_Child := 0;
         elsif Defer_Res_168 /= null and then not Is_Ghost (Defer_Res_168) then
            Transform_Res_82.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Global_Stmt_Transform_Parse_0_Memo,
      Row_Pos_103 /= No_Token_Index,
      Transform_Res_82,
      Pos,
      Row_Pos_103);


   Parser.Current_Pos := Row_Pos_103;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_82;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Global_Stmt_Transform_Parse_0;

   


function Exec_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Exec_Stmt
is
   use Bare_Exec_Stmt_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_104 :
            Token_Index
               := No_Token_Index;
      Token_Pos_150 :
            Token_Index
               := No_Token_Index;
      Token_Res_150 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_169 :
            Token_Index
               := No_Token_Index;
      Defer_Res_169 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Row_Pos_105 :
            Token_Index
               := No_Token_Index;
      Token_Pos_151 :
            Token_Index
               := No_Token_Index;
      Token_Res_151 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_170 :
            Token_Index
               := No_Token_Index;
      Defer_Res_170 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Transform_Res_83 :
            Bare_Exec_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Exec_Stmt_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_83 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_83;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_83;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_104 := Pos;



--  Start tok_code

Token_Res_150 := Row_Pos_104;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_150));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Exec)
   then
       Token_Pos_150 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_104 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_104,
             Expected_Token_Id => Turkixir_T_T__Exec,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_150 := Row_Pos_104 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_150 /= No_Token_Index then

   Row_Pos_104 := Token_Pos_150;

else
   Row_Pos_104 := No_Token_Index;
   goto Exit_Row_103_0;

end if;


Defer_Res_169 :=
   Expr_Or_Parse_0 (Parser, Row_Pos_104);
Defer_Pos_169 := Parser.Current_Pos;



if Defer_Pos_169 /= No_Token_Index then

   Row_Pos_104 := Defer_Pos_169;

else
   Row_Pos_104 := No_Token_Index;
   goto Exit_Row_103_0;

end if;


--  Start opt_code




--  Start row_code

Row_Pos_105 := Row_Pos_104;



--  Start tok_code

Token_Res_151 := Row_Pos_105;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_151));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__In)
   then
       Token_Pos_151 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_105 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_105,
             Expected_Token_Id => Turkixir_T_T__In,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_151 := Row_Pos_105 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_151 /= No_Token_Index then

   Row_Pos_105 := Token_Pos_151;

else
   Row_Pos_105 := No_Token_Index;
   goto Exit_Row_104_0;

end if;


Defer_Res_170 :=
   Test_List_Extract_Parse_0 (Parser, Row_Pos_105);
Defer_Pos_170 := Parser.Current_Pos;



if Defer_Pos_170 /= No_Token_Index then

   Row_Pos_105 := Defer_Pos_170;

else
   Row_Pos_105 := No_Token_Index;
   goto Exit_Row_104_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_104_0>>
pragma Warnings (On, "referenced");

--  End row_code


if Row_Pos_105 = No_Token_Index then

        Defer_Res_170 :=
           Allocate_Expr_List (Parser.Mem_Pool);
         Initialize
           (Self              => Defer_Res_170,
            Kind              => Turkixir_Expr_List,
            Unit              => Parser.Unit,
            Token_Start_Index => Row_Pos_104 - 1,
            Token_End_Index   => No_Token_Index);
         Initialize_List
           (Self   => Defer_Res_170,
            Parser => Parser,
            Count  => 0);


    Row_Pos_105 := Row_Pos_104;


end if;

--  End opt_code



if Row_Pos_105 /= No_Token_Index then

   Row_Pos_104 := Row_Pos_105;

else
   Row_Pos_104 := No_Token_Index;
   goto Exit_Row_103_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_103_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_104 /= No_Token_Index then

   Transform_Res_83 := Allocate_Exec_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_83,
      Kind => Turkixir_Exec_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_104 = Pos
                            then No_Token_Index
                            else Row_Pos_104 - 1));

      Initialize_Fields_For_Exec_Stmt
        (Self => Transform_Res_83, Exec_Stmt_F_Expr => Defer_Res_169, Exec_Stmt_F_In_List => Defer_Res_170);

         if Defer_Res_169 /= null and then Is_Incomplete (Defer_Res_169) then
            Transform_Res_83.Last_Attempted_Child := 0;
         elsif Defer_Res_169 /= null and then not Is_Ghost (Defer_Res_169) then
            Transform_Res_83.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_170 /= null and then Is_Incomplete (Defer_Res_170) then
            Transform_Res_83.Last_Attempted_Child := 0;
         elsif Defer_Res_170 /= null and then not Is_Ghost (Defer_Res_170) then
            Transform_Res_83.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Exec_Stmt_Transform_Parse_0_Memo,
      Row_Pos_104 /= No_Token_Index,
      Transform_Res_83,
      Pos,
      Row_Pos_104);


   Parser.Current_Pos := Row_Pos_104;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_83;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Exec_Stmt_Transform_Parse_0;

   


function Small_Stmt_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node
is
   use Bare_Turkixir_Node_Memos;

   Call_Depth : aliased Natural;

      Defer_Pos_123 :
            Token_Index
               := No_Token_Index;
      Defer_Res_123 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Defer_Pos_124 :
            Token_Index
               := No_Token_Index;
      Defer_Res_124 :
            Bare_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_125 :
            Token_Index
               := No_Token_Index;
      Defer_Res_125 :
            Bare_Del_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_126 :
            Token_Index
               := No_Token_Index;
      Defer_Res_126 :
            Bare_Pass_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_127 :
            Token_Index
               := No_Token_Index;
      Defer_Res_127 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Defer_Pos_128 :
            Token_Index
               := No_Token_Index;
      Defer_Res_128 :
            Bare_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_129 :
            Token_Index
               := No_Token_Index;
      Defer_Res_129 :
            Bare_Global_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_130 :
            Token_Index
               := No_Token_Index;
      Defer_Res_130 :
            Bare_Exec_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_131 :
            Token_Index
               := No_Token_Index;
      Defer_Res_131 :
            Bare_Assert_Stmt
               := No_Bare_Turkixir_Node;
      Or_Pos_28 :
            Token_Index
               := No_Token_Index;
      Or_Res_28 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Small_Stmt_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_28 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_28;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_28;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_28 := No_Token_Index;
Or_Res_28 := No_Bare_Turkixir_Node;
    
Defer_Res_123 :=
   Expr_Stmt_Or_Parse_3 (Parser, Pos);
Defer_Pos_123 := Parser.Current_Pos;

    if Defer_Pos_123 /= No_Token_Index then
        Or_Pos_28 := Defer_Pos_123;
        Or_Res_28 := Defer_Res_123;
        goto Exit_Or_28;
    end if;
    
Defer_Res_124 :=
   Print_Stmt_Or_Parse_0 (Parser, Pos);
Defer_Pos_124 := Parser.Current_Pos;

    if Defer_Pos_124 /= No_Token_Index then
        Or_Pos_28 := Defer_Pos_124;
        Or_Res_28 := Defer_Res_124;
        goto Exit_Or_28;
    end if;
    
Defer_Res_125 :=
   Del_Stmt_Transform_Parse_0 (Parser, Pos);
Defer_Pos_125 := Parser.Current_Pos;

    if Defer_Pos_125 /= No_Token_Index then
        Or_Pos_28 := Defer_Pos_125;
        Or_Res_28 := Defer_Res_125;
        goto Exit_Or_28;
    end if;
    
Defer_Res_126 :=
   Pass_Stmt_Transform_Parse_0 (Parser, Pos);
Defer_Pos_126 := Parser.Current_Pos;

    if Defer_Pos_126 /= No_Token_Index then
        Or_Pos_28 := Defer_Pos_126;
        Or_Res_28 := Defer_Res_126;
        goto Exit_Or_28;
    end if;
    
Defer_Res_127 :=
   Flow_Stmt_Or_Parse_0 (Parser, Pos);
Defer_Pos_127 := Parser.Current_Pos;

    if Defer_Pos_127 /= No_Token_Index then
        Or_Pos_28 := Defer_Pos_127;
        Or_Res_28 := Defer_Res_127;
        goto Exit_Or_28;
    end if;
    
Defer_Res_128 :=
   Import_Stmt_Or_Parse_0 (Parser, Pos);
Defer_Pos_128 := Parser.Current_Pos;

    if Defer_Pos_128 /= No_Token_Index then
        Or_Pos_28 := Defer_Pos_128;
        Or_Res_28 := Defer_Res_128;
        goto Exit_Or_28;
    end if;
    
Defer_Res_129 :=
   Global_Stmt_Transform_Parse_0 (Parser, Pos);
Defer_Pos_129 := Parser.Current_Pos;

    if Defer_Pos_129 /= No_Token_Index then
        Or_Pos_28 := Defer_Pos_129;
        Or_Res_28 := Defer_Res_129;
        goto Exit_Or_28;
    end if;
    
Defer_Res_130 :=
   Exec_Stmt_Transform_Parse_0 (Parser, Pos);
Defer_Pos_130 := Parser.Current_Pos;

    if Defer_Pos_130 /= No_Token_Index then
        Or_Pos_28 := Defer_Pos_130;
        Or_Res_28 := Defer_Res_130;
        goto Exit_Or_28;
    end if;
    
Defer_Res_131 :=
   Assert_Stmt_Transform_Parse_0 (Parser, Pos);
Defer_Pos_131 := Parser.Current_Pos;

    if Defer_Pos_131 /= No_Token_Index then
        Or_Pos_28 := Defer_Pos_131;
        Or_Res_28 := Defer_Res_131;
        goto Exit_Or_28;
    end if;
<<Exit_Or_28>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Small_Stmt_Or_Parse_0_Memo,
      Or_Pos_28 /= No_Token_Index,
      Or_Res_28,
      Pos,
      Or_Pos_28);


   Parser.Current_Pos := Or_Pos_28;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_28;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Small_Stmt_Or_Parse_0;

   


function Simple_Stmt_Extract_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node
is
   use Bare_Turkixir_Node_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_80 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_121 :
            Token_Index
               := No_Token_Index;
      Defer_Res_121 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Row_Pos_81 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_13 :
            Token_Index
               := No_Token_Index;
      Tmp_List_13 :
            Free_Parse_List;
      Defer_Pos_122 :
            Token_Index
               := No_Token_Index;
      Defer_Res_122 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Token_Pos_111 :
            Token_Index
               := No_Token_Index;
      Token_Res_111 :
            Token_Index
               := No_Token_Index;
      List_Pos_13 :
            Token_Index
               := No_Token_Index;
      List_Res_13 :
            Bare_Turkixir_Node_List
               := No_Bare_Turkixir_Node;
      Token_Pos_112 :
            Token_Index
               := No_Token_Index;
      Token_Res_112 :
            Token_Index
               := No_Token_Index;
      Or_Pos_27 :
            Token_Index
               := No_Token_Index;
      Or_Res_27 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Token_Pos_113 :
            Token_Index
               := No_Token_Index;
      Token_Res_113 :
            Token_Index
               := No_Token_Index;


   M : Memo_Entry := Get (Parser.Private_Part.Simple_Stmt_Extract_Parse_1_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_27 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_27;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_27;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start row_code

Row_Pos_80 := Pos;



--  Start or_code

Or_Pos_27 := No_Token_Index;
Or_Res_27 := No_Bare_Turkixir_Node;
    
Defer_Res_121 :=
   Small_Stmt_Or_Parse_0 (Parser, Row_Pos_80);
Defer_Pos_121 := Parser.Current_Pos;

    if Defer_Pos_121 /= No_Token_Index then
        Or_Pos_27 := Defer_Pos_121;
        Or_Res_27 := Defer_Res_121;
        goto Exit_Or_27;
    end if;
    
--  Start row_code

Row_Pos_81 := Row_Pos_80;



--  Start list_code

    List_Pos_13 := No_Token_Index;



Lst_Cpos_13 := Row_Pos_81;
Tmp_List_13 := Get_Parse_List (Parser);

loop
   
Defer_Res_122 :=
   Small_Stmt_Or_Parse_0 (Parser, Lst_Cpos_13);
Defer_Pos_122 := Parser.Current_Pos;


   exit when Defer_Pos_122 = No_Token_Index;

   List_Pos_13 := Defer_Pos_122;
   Lst_Cpos_13 := List_Pos_13;

   Tmp_List_13.Nodes.Append (Defer_Res_122);

      
--  Start tok_code

Token_Res_111 := Lst_Cpos_13;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_111));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Semicolon)
   then
       Token_Pos_111 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos_13 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos_13,
             Expected_Token_Id => Turkixir_T_T__Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_111 := Lst_Cpos_13 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos_111 /= No_Token_Index then
          Lst_Cpos_13 := Token_Pos_111;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_13.Nodes.Length;
begin
   List_Res_13 :=
      Allocate_Turkixir_Node_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_81;
      Token_End := (if Lst_Cpos_13 = Row_Pos_81
                    then Row_Pos_81
                    else Lst_Cpos_13 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_81, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_13,
      Kind              => Turkixir_Turkixir_Node_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_13,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_13.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_13.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_13);

--  End list_code



if List_Pos_13 /= No_Token_Index then

   Row_Pos_81 := List_Pos_13;

else
   Row_Pos_81 := No_Token_Index;
   goto Exit_Row_105_0;

end if;


--  Start opt_code




--  Start tok_code

Token_Res_112 := Row_Pos_81;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_112));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Semicolon)
   then
       Token_Pos_112 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_81 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_81,
             Expected_Token_Id => Turkixir_T_T__Semicolon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_112 := Row_Pos_81 + 1;
   end if;
end;

--  End tok_code


if Token_Pos_112 = No_Token_Index then

        Token_Res_112 := No_Token_Index;


    Token_Pos_112 := Row_Pos_81;


end if;

--  End opt_code



if Token_Pos_112 /= No_Token_Index then

   Row_Pos_81 := Token_Pos_112;

else
   Row_Pos_81 := No_Token_Index;
   goto Exit_Row_105_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_105_0>>
pragma Warnings (On, "referenced");

--  End row_code

    if Row_Pos_81 /= No_Token_Index then
        Or_Pos_27 := Row_Pos_81;
        Or_Res_27 := List_Res_13;
        goto Exit_Or_27;
    end if;
<<Exit_Or_27>>

--  End or_code



if Or_Pos_27 /= No_Token_Index then

   Row_Pos_80 := Or_Pos_27;

else
   Row_Pos_80 := No_Token_Index;
   goto Exit_Row_80_0;

end if;


--  Start tok_code

Token_Res_113 := Row_Pos_80;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_113));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_Newline)
   then
       Token_Pos_113 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_80 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_80,
             Expected_Token_Id => Turkixir_Newline,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_113 := Row_Pos_80 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_113 /= No_Token_Index then

   Row_Pos_80 := Token_Pos_113;

else
   Row_Pos_80 := No_Token_Index;
   goto Exit_Row_80_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_80_0>>
pragma Warnings (On, "referenced");

--  End row_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Simple_Stmt_Extract_Parse_1_Memo,
      Row_Pos_80 /= No_Token_Index,
      Or_Res_27,
      Pos,
      Row_Pos_80);


   Parser.Current_Pos := Row_Pos_80;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_27;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Simple_Stmt_Extract_Parse_1;

   


function Else_Part_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Else_Part
is
   use Bare_Else_Part_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_109 :
            Token_Index
               := No_Token_Index;
      Token_Pos_156 :
            Token_Index
               := No_Token_Index;
      Token_Res_156 :
            Token_Index
               := No_Token_Index;
      Token_Pos_157 :
            Token_Index
               := No_Token_Index;
      Token_Res_157 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_184 :
            Token_Index
               := No_Token_Index;
      Defer_Res_184 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Transform_Res_86 :
            Bare_Else_Part
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Else_Part_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_86 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_86;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_86;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_109 := Pos;



--  Start tok_code

Token_Res_156 := Row_Pos_109;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_156));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Else)
   then
       Token_Pos_156 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_109 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_109,
             Expected_Token_Id => Turkixir_T_T__Else,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_156 := Row_Pos_109 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_156 /= No_Token_Index then

   Row_Pos_109 := Token_Pos_156;

else
   Row_Pos_109 := No_Token_Index;
   goto Exit_Row_109_0;

end if;


--  Start tok_code

Token_Res_157 := Row_Pos_109;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_157));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_157 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_109 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_109,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_157 := Row_Pos_109 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_157 /= No_Token_Index then

   Row_Pos_109 := Token_Pos_157;

else
   Row_Pos_109 := No_Token_Index;
   goto Exit_Row_109_0;

end if;


Defer_Res_184 :=
   Suite_Or_Parse_0 (Parser, Row_Pos_109);
Defer_Pos_184 := Parser.Current_Pos;



if Defer_Pos_184 /= No_Token_Index then

   Row_Pos_109 := Defer_Pos_184;

else
   Row_Pos_109 := No_Token_Index;
   goto Exit_Row_109_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_109_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_109 /= No_Token_Index then

   Transform_Res_86 := Allocate_Else_Part (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_86,
      Kind => Turkixir_Else_Part,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_109 = Pos
                            then No_Token_Index
                            else Row_Pos_109 - 1));

      Initialize_Fields_For_Else_Part
        (Self => Transform_Res_86, Else_Part_F_Statements => Defer_Res_184);

         if Defer_Res_184 /= null and then Is_Incomplete (Defer_Res_184) then
            Transform_Res_86.Last_Attempted_Child := 0;
         elsif Defer_Res_184 /= null and then not Is_Ghost (Defer_Res_184) then
            Transform_Res_86.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Else_Part_Transform_Parse_0_Memo,
      Row_Pos_109 /= No_Token_Index,
      Transform_Res_86,
      Pos,
      Row_Pos_109);


   Parser.Current_Pos := Row_Pos_109;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_86;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Else_Part_Transform_Parse_0;

   


function If_Stmt_Transform_Parse_1
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_If_Stmt
is
   use Bare_If_Stmt_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_106 :
            Token_Index
               := No_Token_Index;
      Token_Pos_152 :
            Token_Index
               := No_Token_Index;
      Token_Res_152 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_179 :
            Token_Index
               := No_Token_Index;
      Defer_Res_179 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_153 :
            Token_Index
               := No_Token_Index;
      Token_Res_153 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_180 :
            Token_Index
               := No_Token_Index;
      Defer_Res_180 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Lst_Cpos_18 :
            Token_Index
               := No_Token_Index;
      Tmp_List_18 :
            Free_Parse_List;
      Row_Pos_107 :
            Token_Index
               := No_Token_Index;
      Token_Pos_154 :
            Token_Index
               := No_Token_Index;
      Token_Res_154 :
            Token_Index
               := No_Token_Index;
      Row_Pos_108 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_181 :
            Token_Index
               := No_Token_Index;
      Defer_Res_181 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_155 :
            Token_Index
               := No_Token_Index;
      Token_Res_155 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_182 :
            Token_Index
               := No_Token_Index;
      Defer_Res_182 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Transform_Res_84 :
            Bare_Elif_Branch
               := No_Bare_Turkixir_Node;
      List_Pos_18 :
            Token_Index
               := No_Token_Index;
      List_Res_18 :
            Bare_Elif_Branch_List
               := No_Bare_Turkixir_Node;
      Defer_Pos_183 :
            Token_Index
               := No_Token_Index;
      Defer_Res_183 :
            Bare_Else_Part
               := No_Bare_Turkixir_Node;
      Transform_Res_85 :
            Bare_If_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.If_Stmt_Transform_Parse_1_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_85 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_85;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_85;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_106 := Pos;



--  Start tok_code

Token_Res_152 := Row_Pos_106;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_152));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__If)
   then
       Token_Pos_152 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_106 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_106,
             Expected_Token_Id => Turkixir_T_T__If,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_152 := Row_Pos_106 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_152 /= No_Token_Index then

   Row_Pos_106 := Token_Pos_152;

else
   Row_Pos_106 := No_Token_Index;
   goto Exit_Row_106_0;

end if;


Defer_Res_179 :=
   Test_Or_Parse_0 (Parser, Row_Pos_106);
Defer_Pos_179 := Parser.Current_Pos;



if Defer_Pos_179 /= No_Token_Index then

   Row_Pos_106 := Defer_Pos_179;

else
   Row_Pos_106 := No_Token_Index;
   goto Exit_Row_106_0;

end if;


--  Start tok_code

Token_Res_153 := Row_Pos_106;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_153));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_153 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_106 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_106,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_153 := Row_Pos_106 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_153 /= No_Token_Index then

   Row_Pos_106 := Token_Pos_153;

else
   Row_Pos_106 := No_Token_Index;
   goto Exit_Row_106_0;

end if;


Defer_Res_180 :=
   Suite_Or_Parse_0 (Parser, Row_Pos_106);
Defer_Pos_180 := Parser.Current_Pos;



if Defer_Pos_180 /= No_Token_Index then

   Row_Pos_106 := Defer_Pos_180;

else
   Row_Pos_106 := No_Token_Index;
   goto Exit_Row_106_0;

end if;


--  Start list_code

    List_Pos_18 := Row_Pos_106;



Lst_Cpos_18 := Row_Pos_106;
Tmp_List_18 := Get_Parse_List (Parser);

loop
   
--  Start row_code

Row_Pos_107 := Lst_Cpos_18;



--  Start tok_code

Token_Res_154 := Row_Pos_107;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_154));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Elif)
   then
       Token_Pos_154 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_107 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_107,
             Expected_Token_Id => Turkixir_T_T__Elif,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_154 := Row_Pos_107 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_154 /= No_Token_Index then

   Row_Pos_107 := Token_Pos_154;

else
   Row_Pos_107 := No_Token_Index;
   goto Exit_Row_107_0;

end if;


--  Start transform_code


--  Start row_code

Row_Pos_108 := Row_Pos_107;



Defer_Res_181 :=
   Test_Or_Parse_0 (Parser, Row_Pos_108);
Defer_Pos_181 := Parser.Current_Pos;



if Defer_Pos_181 /= No_Token_Index then

   Row_Pos_108 := Defer_Pos_181;

else
   Row_Pos_108 := No_Token_Index;
   goto Exit_Row_108_0;

end if;


--  Start tok_code

Token_Res_155 := Row_Pos_108;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_155));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_155 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_108 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_108,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_155 := Row_Pos_108 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_155 /= No_Token_Index then

   Row_Pos_108 := Token_Pos_155;

else
   Row_Pos_108 := No_Token_Index;
   goto Exit_Row_108_0;

end if;


Defer_Res_182 :=
   Suite_Or_Parse_0 (Parser, Row_Pos_108);
Defer_Pos_182 := Parser.Current_Pos;



if Defer_Pos_182 /= No_Token_Index then

   Row_Pos_108 := Defer_Pos_182;

else
   Row_Pos_108 := No_Token_Index;
   goto Exit_Row_108_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_108_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_108 /= No_Token_Index then

   Transform_Res_84 := Allocate_Elif_Branch (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_84,
      Kind => Turkixir_Elif_Branch,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_107,
      Token_End_Index   => (if Row_Pos_108 = Row_Pos_107
                            then No_Token_Index
                            else Row_Pos_108 - 1));

      Initialize_Fields_For_Elif_Branch
        (Self => Transform_Res_84, Elif_Branch_F_Cond_Test => Defer_Res_181, Elif_Branch_F_Statements => Defer_Res_182);

         if Defer_Res_181 /= null and then Is_Incomplete (Defer_Res_181) then
            Transform_Res_84.Last_Attempted_Child := 0;
         elsif Defer_Res_181 /= null and then not Is_Ghost (Defer_Res_181) then
            Transform_Res_84.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_182 /= null and then Is_Incomplete (Defer_Res_182) then
            Transform_Res_84.Last_Attempted_Child := 0;
         elsif Defer_Res_182 /= null and then not Is_Ghost (Defer_Res_182) then
            Transform_Res_84.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code



if Row_Pos_108 /= No_Token_Index then

   Row_Pos_107 := Row_Pos_108;

else
   Row_Pos_107 := No_Token_Index;
   goto Exit_Row_107_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_107_0>>
pragma Warnings (On, "referenced");

--  End row_code


   exit when Row_Pos_107 = No_Token_Index;

   List_Pos_18 := Row_Pos_107;
   Lst_Cpos_18 := List_Pos_18;

   Tmp_List_18.Nodes.Append (Transform_Res_84);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_18.Nodes.Length;
begin
   List_Res_18 :=
      Allocate_Elif_Branch_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_106;
      Token_End := (if Lst_Cpos_18 = Row_Pos_106
                    then Row_Pos_106
                    else Lst_Cpos_18 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_106, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_18,
      Kind              => Turkixir_Elif_Branch_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_18,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_18.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_18.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_18);

--  End list_code



if List_Pos_18 /= No_Token_Index then

   Row_Pos_106 := List_Pos_18;

else
   Row_Pos_106 := No_Token_Index;
   goto Exit_Row_106_0;

end if;


--  Start opt_code




Defer_Res_183 :=
   Else_Part_Transform_Parse_0 (Parser, Row_Pos_106);
Defer_Pos_183 := Parser.Current_Pos;


if Defer_Pos_183 = No_Token_Index then

        Defer_Res_183 := No_Bare_Turkixir_Node;


    Defer_Pos_183 := Row_Pos_106;


end if;

--  End opt_code



if Defer_Pos_183 /= No_Token_Index then

   Row_Pos_106 := Defer_Pos_183;

else
   Row_Pos_106 := No_Token_Index;
   goto Exit_Row_106_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_106_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_106 /= No_Token_Index then

   Transform_Res_85 := Allocate_If_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_85,
      Kind => Turkixir_If_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_106 = Pos
                            then No_Token_Index
                            else Row_Pos_106 - 1));

      Initialize_Fields_For_If_Stmt
        (Self => Transform_Res_85, If_Stmt_F_Cond_Test => Defer_Res_179, If_Stmt_F_Statements => Defer_Res_180, If_Stmt_F_Elif_Branchs => List_Res_18, If_Stmt_F_Else_Part => Defer_Res_183);

         if Defer_Res_179 /= null and then Is_Incomplete (Defer_Res_179) then
            Transform_Res_85.Last_Attempted_Child := 0;
         elsif Defer_Res_179 /= null and then not Is_Ghost (Defer_Res_179) then
            Transform_Res_85.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_180 /= null and then Is_Incomplete (Defer_Res_180) then
            Transform_Res_85.Last_Attempted_Child := 0;
         elsif Defer_Res_180 /= null and then not Is_Ghost (Defer_Res_180) then
            Transform_Res_85.Last_Attempted_Child := -1;
         end if;
         if List_Res_18 /= null and then Is_Incomplete (List_Res_18) then
            Transform_Res_85.Last_Attempted_Child := 0;
         elsif List_Res_18 /= null and then not Is_Ghost (List_Res_18) then
            Transform_Res_85.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_183 /= null and then Is_Incomplete (Defer_Res_183) then
            Transform_Res_85.Last_Attempted_Child := 0;
         elsif Defer_Res_183 /= null and then not Is_Ghost (Defer_Res_183) then
            Transform_Res_85.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.If_Stmt_Transform_Parse_1_Memo,
      Row_Pos_106 /= No_Token_Index,
      Transform_Res_85,
      Pos,
      Row_Pos_106);


   Parser.Current_Pos := Row_Pos_106;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_85;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end If_Stmt_Transform_Parse_1;

   


function While_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_While_Stmt
is
   use Bare_While_Stmt_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_110 :
            Token_Index
               := No_Token_Index;
      Token_Pos_158 :
            Token_Index
               := No_Token_Index;
      Token_Res_158 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_185 :
            Token_Index
               := No_Token_Index;
      Defer_Res_185 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Token_Pos_159 :
            Token_Index
               := No_Token_Index;
      Token_Res_159 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_186 :
            Token_Index
               := No_Token_Index;
      Defer_Res_186 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Defer_Pos_187 :
            Token_Index
               := No_Token_Index;
      Defer_Res_187 :
            Bare_Else_Part
               := No_Bare_Turkixir_Node;
      Transform_Res_87 :
            Bare_While_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.While_Stmt_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_87 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_87;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_87;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_110 := Pos;



--  Start tok_code

Token_Res_158 := Row_Pos_110;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_158));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__While)
   then
       Token_Pos_158 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_110 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_110,
             Expected_Token_Id => Turkixir_T_T__While,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_158 := Row_Pos_110 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_158 /= No_Token_Index then

   Row_Pos_110 := Token_Pos_158;

else
   Row_Pos_110 := No_Token_Index;
   goto Exit_Row_110_0;

end if;


Defer_Res_185 :=
   Test_Or_Parse_0 (Parser, Row_Pos_110);
Defer_Pos_185 := Parser.Current_Pos;



if Defer_Pos_185 /= No_Token_Index then

   Row_Pos_110 := Defer_Pos_185;

else
   Row_Pos_110 := No_Token_Index;
   goto Exit_Row_110_0;

end if;


--  Start tok_code

Token_Res_159 := Row_Pos_110;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_159));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_159 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_110 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_110,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_159 := Row_Pos_110 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_159 /= No_Token_Index then

   Row_Pos_110 := Token_Pos_159;

else
   Row_Pos_110 := No_Token_Index;
   goto Exit_Row_110_0;

end if;


Defer_Res_186 :=
   Suite_Or_Parse_0 (Parser, Row_Pos_110);
Defer_Pos_186 := Parser.Current_Pos;



if Defer_Pos_186 /= No_Token_Index then

   Row_Pos_110 := Defer_Pos_186;

else
   Row_Pos_110 := No_Token_Index;
   goto Exit_Row_110_0;

end if;


--  Start opt_code




Defer_Res_187 :=
   Else_Part_Transform_Parse_0 (Parser, Row_Pos_110);
Defer_Pos_187 := Parser.Current_Pos;


if Defer_Pos_187 = No_Token_Index then

        Defer_Res_187 := No_Bare_Turkixir_Node;


    Defer_Pos_187 := Row_Pos_110;


end if;

--  End opt_code



if Defer_Pos_187 /= No_Token_Index then

   Row_Pos_110 := Defer_Pos_187;

else
   Row_Pos_110 := No_Token_Index;
   goto Exit_Row_110_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_110_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_110 /= No_Token_Index then

   Transform_Res_87 := Allocate_While_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_87,
      Kind => Turkixir_While_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_110 = Pos
                            then No_Token_Index
                            else Row_Pos_110 - 1));

      Initialize_Fields_For_While_Stmt
        (Self => Transform_Res_87, While_Stmt_F_Cond_Test => Defer_Res_185, While_Stmt_F_Statements => Defer_Res_186, While_Stmt_F_Else_Part => Defer_Res_187);

         if Defer_Res_185 /= null and then Is_Incomplete (Defer_Res_185) then
            Transform_Res_87.Last_Attempted_Child := 0;
         elsif Defer_Res_185 /= null and then not Is_Ghost (Defer_Res_185) then
            Transform_Res_87.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_186 /= null and then Is_Incomplete (Defer_Res_186) then
            Transform_Res_87.Last_Attempted_Child := 0;
         elsif Defer_Res_186 /= null and then not Is_Ghost (Defer_Res_186) then
            Transform_Res_87.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_187 /= null and then Is_Incomplete (Defer_Res_187) then
            Transform_Res_87.Last_Attempted_Child := 0;
         elsif Defer_Res_187 /= null and then not Is_Ghost (Defer_Res_187) then
            Transform_Res_87.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.While_Stmt_Transform_Parse_0_Memo,
      Row_Pos_110 /= No_Token_Index,
      Transform_Res_87,
      Pos,
      Row_Pos_110);


   Parser.Current_Pos := Row_Pos_110;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_87;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end While_Stmt_Transform_Parse_0;

   


function For_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_For_Stmt
is
   use Bare_For_Stmt_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_111 :
            Token_Index
               := No_Token_Index;
      Token_Pos_160 :
            Token_Index
               := No_Token_Index;
      Token_Res_160 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_188 :
            Token_Index
               := No_Token_Index;
      Defer_Res_188 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Token_Pos_161 :
            Token_Index
               := No_Token_Index;
      Token_Res_161 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_189 :
            Token_Index
               := No_Token_Index;
      Defer_Res_189 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Token_Pos_162 :
            Token_Index
               := No_Token_Index;
      Token_Res_162 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_190 :
            Token_Index
               := No_Token_Index;
      Defer_Res_190 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Defer_Pos_191 :
            Token_Index
               := No_Token_Index;
      Defer_Res_191 :
            Bare_Else_Part
               := No_Bare_Turkixir_Node;
      Transform_Res_88 :
            Bare_For_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.For_Stmt_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_88 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_88;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_88;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_111 := Pos;



--  Start tok_code

Token_Res_160 := Row_Pos_111;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_160));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__For)
   then
       Token_Pos_160 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_111 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_111,
             Expected_Token_Id => Turkixir_T_T__For,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_160 := Row_Pos_111 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_160 /= No_Token_Index then

   Row_Pos_111 := Token_Pos_160;

else
   Row_Pos_111 := No_Token_Index;
   goto Exit_Row_111_0;

end if;


Defer_Res_188 :=
   Expr_List_Extract_Parse_0 (Parser, Row_Pos_111);
Defer_Pos_188 := Parser.Current_Pos;



if Defer_Pos_188 /= No_Token_Index then

   Row_Pos_111 := Defer_Pos_188;

else
   Row_Pos_111 := No_Token_Index;
   goto Exit_Row_111_0;

end if;


--  Start tok_code

Token_Res_161 := Row_Pos_111;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_161));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__In)
   then
       Token_Pos_161 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_111 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_111,
             Expected_Token_Id => Turkixir_T_T__In,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_161 := Row_Pos_111 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_161 /= No_Token_Index then

   Row_Pos_111 := Token_Pos_161;

else
   Row_Pos_111 := No_Token_Index;
   goto Exit_Row_111_0;

end if;


Defer_Res_189 :=
   Test_List_Extract_Parse_0 (Parser, Row_Pos_111);
Defer_Pos_189 := Parser.Current_Pos;



if Defer_Pos_189 /= No_Token_Index then

   Row_Pos_111 := Defer_Pos_189;

else
   Row_Pos_111 := No_Token_Index;
   goto Exit_Row_111_0;

end if;


--  Start tok_code

Token_Res_162 := Row_Pos_111;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_162));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_162 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_111 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_111,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_162 := Row_Pos_111 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_162 /= No_Token_Index then

   Row_Pos_111 := Token_Pos_162;

else
   Row_Pos_111 := No_Token_Index;
   goto Exit_Row_111_0;

end if;


Defer_Res_190 :=
   Suite_Or_Parse_0 (Parser, Row_Pos_111);
Defer_Pos_190 := Parser.Current_Pos;



if Defer_Pos_190 /= No_Token_Index then

   Row_Pos_111 := Defer_Pos_190;

else
   Row_Pos_111 := No_Token_Index;
   goto Exit_Row_111_0;

end if;


--  Start opt_code




Defer_Res_191 :=
   Else_Part_Transform_Parse_0 (Parser, Row_Pos_111);
Defer_Pos_191 := Parser.Current_Pos;


if Defer_Pos_191 = No_Token_Index then

        Defer_Res_191 := No_Bare_Turkixir_Node;


    Defer_Pos_191 := Row_Pos_111;


end if;

--  End opt_code



if Defer_Pos_191 /= No_Token_Index then

   Row_Pos_111 := Defer_Pos_191;

else
   Row_Pos_111 := No_Token_Index;
   goto Exit_Row_111_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_111_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_111 /= No_Token_Index then

   Transform_Res_88 := Allocate_For_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_88,
      Kind => Turkixir_For_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_111 = Pos
                            then No_Token_Index
                            else Row_Pos_111 - 1));

      Initialize_Fields_For_For_Stmt
        (Self => Transform_Res_88, For_Stmt_F_Bindings => Defer_Res_188, For_Stmt_F_Expr => Defer_Res_189, For_Stmt_F_Statements => Defer_Res_190, For_Stmt_F_Else_Part => Defer_Res_191);

         if Defer_Res_188 /= null and then Is_Incomplete (Defer_Res_188) then
            Transform_Res_88.Last_Attempted_Child := 0;
         elsif Defer_Res_188 /= null and then not Is_Ghost (Defer_Res_188) then
            Transform_Res_88.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_189 /= null and then Is_Incomplete (Defer_Res_189) then
            Transform_Res_88.Last_Attempted_Child := 0;
         elsif Defer_Res_189 /= null and then not Is_Ghost (Defer_Res_189) then
            Transform_Res_88.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_190 /= null and then Is_Incomplete (Defer_Res_190) then
            Transform_Res_88.Last_Attempted_Child := 0;
         elsif Defer_Res_190 /= null and then not Is_Ghost (Defer_Res_190) then
            Transform_Res_88.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_191 /= null and then Is_Incomplete (Defer_Res_191) then
            Transform_Res_88.Last_Attempted_Child := 0;
         elsif Defer_Res_191 /= null and then not Is_Ghost (Defer_Res_191) then
            Transform_Res_88.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.For_Stmt_Transform_Parse_0_Memo,
      Row_Pos_111 /= No_Token_Index,
      Transform_Res_88,
      Pos,
      Row_Pos_111);


   Parser.Current_Pos := Row_Pos_111;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_88;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end For_Stmt_Transform_Parse_0;

   


function Try_Stmt_Transform_Parse_2
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Try_Stmt
is
   use Bare_Try_Stmt_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_112 :
            Token_Index
               := No_Token_Index;
      Token_Pos_163 :
            Token_Index
               := No_Token_Index;
      Token_Res_163 :
            Token_Index
               := No_Token_Index;
      Token_Pos_164 :
            Token_Index
               := No_Token_Index;
      Token_Res_164 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_192 :
            Token_Index
               := No_Token_Index;
      Defer_Res_192 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Lst_Cpos_19 :
            Token_Index
               := No_Token_Index;
      Tmp_List_19 :
            Free_Parse_List;
      Row_Pos_113 :
            Token_Index
               := No_Token_Index;
      Token_Pos_165 :
            Token_Index
               := No_Token_Index;
      Token_Res_165 :
            Token_Index
               := No_Token_Index;
      Row_Pos_114 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_193 :
            Token_Index
               := No_Token_Index;
      Defer_Res_193 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Row_Pos_115 :
            Token_Index
               := No_Token_Index;
      Token_Pos_166 :
            Token_Index
               := No_Token_Index;
      Token_Res_166 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_194 :
            Token_Index
               := No_Token_Index;
      Defer_Res_194 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_89 :
            Bare_As_Name_Node
               := No_Bare_Turkixir_Node;
      Token_Pos_167 :
            Token_Index
               := No_Token_Index;
      Token_Res_167 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_195 :
            Token_Index
               := No_Token_Index;
      Defer_Res_195 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Transform_Res_90 :
            Bare_Except_Part
               := No_Bare_Turkixir_Node;
      List_Pos_19 :
            Token_Index
               := No_Token_Index;
      List_Res_19 :
            Bare_Except_Part_List
               := No_Bare_Turkixir_Node;
      Defer_Pos_196 :
            Token_Index
               := No_Token_Index;
      Defer_Res_196 :
            Bare_Else_Part
               := No_Bare_Turkixir_Node;
      Row_Pos_116 :
            Token_Index
               := No_Token_Index;
      Token_Pos_168 :
            Token_Index
               := No_Token_Index;
      Token_Res_168 :
            Token_Index
               := No_Token_Index;
      Token_Pos_169 :
            Token_Index
               := No_Token_Index;
      Token_Res_169 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_197 :
            Token_Index
               := No_Token_Index;
      Defer_Res_197 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Transform_Res_91 :
            Bare_Try_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Try_Stmt_Transform_Parse_2_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_91 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_91;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_91;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_112 := Pos;



--  Start tok_code

Token_Res_163 := Row_Pos_112;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_163));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Try)
   then
       Token_Pos_163 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_112 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_112,
             Expected_Token_Id => Turkixir_T_T__Try,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_163 := Row_Pos_112 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_163 /= No_Token_Index then

   Row_Pos_112 := Token_Pos_163;

else
   Row_Pos_112 := No_Token_Index;
   goto Exit_Row_112_0;

end if;


--  Start tok_code

Token_Res_164 := Row_Pos_112;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_164));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_164 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_112 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_112,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_164 := Row_Pos_112 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_164 /= No_Token_Index then

   Row_Pos_112 := Token_Pos_164;

else
   Row_Pos_112 := No_Token_Index;
   goto Exit_Row_112_0;

end if;


Defer_Res_192 :=
   Suite_Or_Parse_0 (Parser, Row_Pos_112);
Defer_Pos_192 := Parser.Current_Pos;



if Defer_Pos_192 /= No_Token_Index then

   Row_Pos_112 := Defer_Pos_192;

else
   Row_Pos_112 := No_Token_Index;
   goto Exit_Row_112_0;

end if;


--  Start list_code

    List_Pos_19 := Row_Pos_112;



Lst_Cpos_19 := Row_Pos_112;
Tmp_List_19 := Get_Parse_List (Parser);

loop
   
--  Start transform_code


--  Start row_code

Row_Pos_113 := Lst_Cpos_19;



--  Start tok_code

Token_Res_165 := Row_Pos_113;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_165));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Except)
   then
       Token_Pos_165 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_113 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_113,
             Expected_Token_Id => Turkixir_T_T__Except,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_165 := Row_Pos_113 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_165 /= No_Token_Index then

   Row_Pos_113 := Token_Pos_165;

else
   Row_Pos_113 := No_Token_Index;
   goto Exit_Row_113_0;

end if;


--  Start opt_code




--  Start transform_code


--  Start row_code

Row_Pos_114 := Row_Pos_113;



Defer_Res_193 :=
   Test_Or_Parse_0 (Parser, Row_Pos_114);
Defer_Pos_193 := Parser.Current_Pos;



if Defer_Pos_193 /= No_Token_Index then

   Row_Pos_114 := Defer_Pos_193;

else
   Row_Pos_114 := No_Token_Index;
   goto Exit_Row_114_0;

end if;


--  Start opt_code




--  Start row_code

Row_Pos_115 := Row_Pos_114;



--  Start tok_code

Token_Res_166 := Row_Pos_115;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_166));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__As)
   then
       Token_Pos_166 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_115 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_115,
             Expected_Token_Id => Turkixir_T_T__As,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_166 := Row_Pos_115 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_166 /= No_Token_Index then

   Row_Pos_115 := Token_Pos_166;

else
   Row_Pos_115 := No_Token_Index;
   goto Exit_Row_115_0;

end if;


Defer_Res_194 :=
   Test_Or_Parse_0 (Parser, Row_Pos_115);
Defer_Pos_194 := Parser.Current_Pos;



if Defer_Pos_194 /= No_Token_Index then

   Row_Pos_115 := Defer_Pos_194;

else
   Row_Pos_115 := No_Token_Index;
   goto Exit_Row_115_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_115_0>>
pragma Warnings (On, "referenced");

--  End row_code


if Row_Pos_115 = No_Token_Index then

        Defer_Res_194 := No_Bare_Turkixir_Node;


    Row_Pos_115 := Row_Pos_114;


end if;

--  End opt_code



if Row_Pos_115 /= No_Token_Index then

   Row_Pos_114 := Row_Pos_115;

else
   Row_Pos_114 := No_Token_Index;
   goto Exit_Row_114_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_114_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_114 /= No_Token_Index then

   Transform_Res_89 := Allocate_As_Name_Node (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_89,
      Kind => Turkixir_As_Name_Node,
      Unit => Parser.Unit,

      Token_Start_Index => Row_Pos_113,
      Token_End_Index   => (if Row_Pos_114 = Row_Pos_113
                            then No_Token_Index
                            else Row_Pos_114 - 1));

      Initialize_Fields_For_As_Name_Node
        (Self => Transform_Res_89, As_Name_Node_F_Imported => Defer_Res_193, As_Name_Node_F_As_Name => Defer_Res_194);

         if Defer_Res_193 /= null and then Is_Incomplete (Defer_Res_193) then
            Transform_Res_89.Last_Attempted_Child := 0;
         elsif Defer_Res_193 /= null and then not Is_Ghost (Defer_Res_193) then
            Transform_Res_89.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_194 /= null and then Is_Incomplete (Defer_Res_194) then
            Transform_Res_89.Last_Attempted_Child := 0;
         elsif Defer_Res_194 /= null and then not Is_Ghost (Defer_Res_194) then
            Transform_Res_89.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


if Row_Pos_114 = No_Token_Index then

        Transform_Res_89 := No_Bare_Turkixir_Node;


    Row_Pos_114 := Row_Pos_113;


end if;

--  End opt_code



if Row_Pos_114 /= No_Token_Index then

   Row_Pos_113 := Row_Pos_114;

else
   Row_Pos_113 := No_Token_Index;
   goto Exit_Row_113_0;

end if;


--  Start tok_code

Token_Res_167 := Row_Pos_113;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_167));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_167 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_113 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_113,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_167 := Row_Pos_113 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_167 /= No_Token_Index then

   Row_Pos_113 := Token_Pos_167;

else
   Row_Pos_113 := No_Token_Index;
   goto Exit_Row_113_0;

end if;


Defer_Res_195 :=
   Suite_Or_Parse_0 (Parser, Row_Pos_113);
Defer_Pos_195 := Parser.Current_Pos;



if Defer_Pos_195 /= No_Token_Index then

   Row_Pos_113 := Defer_Pos_195;

else
   Row_Pos_113 := No_Token_Index;
   goto Exit_Row_113_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_113_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_113 /= No_Token_Index then

   Transform_Res_90 := Allocate_Except_Part (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_90,
      Kind => Turkixir_Except_Part,
      Unit => Parser.Unit,

      Token_Start_Index => Lst_Cpos_19,
      Token_End_Index   => (if Row_Pos_113 = Lst_Cpos_19
                            then No_Token_Index
                            else Row_Pos_113 - 1));

      Initialize_Fields_For_Except_Part
        (Self => Transform_Res_90, Except_Part_F_As_Name => Transform_Res_89, Except_Part_F_Statements => Defer_Res_195);

         if Transform_Res_89 /= null and then Is_Incomplete (Transform_Res_89) then
            Transform_Res_90.Last_Attempted_Child := 0;
         elsif Transform_Res_89 /= null and then not Is_Ghost (Transform_Res_89) then
            Transform_Res_90.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_195 /= null and then Is_Incomplete (Defer_Res_195) then
            Transform_Res_90.Last_Attempted_Child := 0;
         elsif Defer_Res_195 /= null and then not Is_Ghost (Defer_Res_195) then
            Transform_Res_90.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   exit when Row_Pos_113 = No_Token_Index;

   List_Pos_19 := Row_Pos_113;
   Lst_Cpos_19 := List_Pos_19;

   Tmp_List_19.Nodes.Append (Transform_Res_90);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_19.Nodes.Length;
begin
   List_Res_19 :=
      Allocate_Except_Part_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_112;
      Token_End := (if Lst_Cpos_19 = Row_Pos_112
                    then Row_Pos_112
                    else Lst_Cpos_19 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_112, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_19,
      Kind              => Turkixir_Except_Part_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_19,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_19.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_19.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_19);

--  End list_code



if List_Pos_19 /= No_Token_Index then

   Row_Pos_112 := List_Pos_19;

else
   Row_Pos_112 := No_Token_Index;
   goto Exit_Row_112_0;

end if;


--  Start opt_code




Defer_Res_196 :=
   Else_Part_Transform_Parse_0 (Parser, Row_Pos_112);
Defer_Pos_196 := Parser.Current_Pos;


if Defer_Pos_196 = No_Token_Index then

        Defer_Res_196 := No_Bare_Turkixir_Node;


    Defer_Pos_196 := Row_Pos_112;


end if;

--  End opt_code



if Defer_Pos_196 /= No_Token_Index then

   Row_Pos_112 := Defer_Pos_196;

else
   Row_Pos_112 := No_Token_Index;
   goto Exit_Row_112_0;

end if;


--  Start opt_code




--  Start row_code

Row_Pos_116 := Row_Pos_112;



--  Start tok_code

Token_Res_168 := Row_Pos_116;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_168));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Finally)
   then
       Token_Pos_168 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_116 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_116,
             Expected_Token_Id => Turkixir_T_T__Finally,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_168 := Row_Pos_116 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_168 /= No_Token_Index then

   Row_Pos_116 := Token_Pos_168;

else
   Row_Pos_116 := No_Token_Index;
   goto Exit_Row_116_0;

end if;


--  Start tok_code

Token_Res_169 := Row_Pos_116;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_169));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_169 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_116 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_116,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_169 := Row_Pos_116 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_169 /= No_Token_Index then

   Row_Pos_116 := Token_Pos_169;

else
   Row_Pos_116 := No_Token_Index;
   goto Exit_Row_116_0;

end if;


Defer_Res_197 :=
   Suite_Or_Parse_0 (Parser, Row_Pos_116);
Defer_Pos_197 := Parser.Current_Pos;



if Defer_Pos_197 /= No_Token_Index then

   Row_Pos_116 := Defer_Pos_197;

else
   Row_Pos_116 := No_Token_Index;
   goto Exit_Row_116_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_116_0>>
pragma Warnings (On, "referenced");

--  End row_code


if Row_Pos_116 = No_Token_Index then

        Defer_Res_197 := No_Bare_Turkixir_Node;


    Row_Pos_116 := Row_Pos_112;


end if;

--  End opt_code



if Row_Pos_116 /= No_Token_Index then

   Row_Pos_112 := Row_Pos_116;

else
   Row_Pos_112 := No_Token_Index;
   goto Exit_Row_112_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_112_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_112 /= No_Token_Index then

   Transform_Res_91 := Allocate_Try_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_91,
      Kind => Turkixir_Try_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_112 = Pos
                            then No_Token_Index
                            else Row_Pos_112 - 1));

      Initialize_Fields_For_Try_Stmt
        (Self => Transform_Res_91, Try_Stmt_F_Statements => Defer_Res_192, Try_Stmt_F_Except_Parts => List_Res_19, Try_Stmt_F_Else_Part => Defer_Res_196, Try_Stmt_F_Finally_Part => Defer_Res_197);

         if Defer_Res_192 /= null and then Is_Incomplete (Defer_Res_192) then
            Transform_Res_91.Last_Attempted_Child := 0;
         elsif Defer_Res_192 /= null and then not Is_Ghost (Defer_Res_192) then
            Transform_Res_91.Last_Attempted_Child := -1;
         end if;
         if List_Res_19 /= null and then Is_Incomplete (List_Res_19) then
            Transform_Res_91.Last_Attempted_Child := 0;
         elsif List_Res_19 /= null and then not Is_Ghost (List_Res_19) then
            Transform_Res_91.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_196 /= null and then Is_Incomplete (Defer_Res_196) then
            Transform_Res_91.Last_Attempted_Child := 0;
         elsif Defer_Res_196 /= null and then not Is_Ghost (Defer_Res_196) then
            Transform_Res_91.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_197 /= null and then Is_Incomplete (Defer_Res_197) then
            Transform_Res_91.Last_Attempted_Child := 0;
         elsif Defer_Res_197 /= null and then not Is_Ghost (Defer_Res_197) then
            Transform_Res_91.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Try_Stmt_Transform_Parse_2_Memo,
      Row_Pos_112 /= No_Token_Index,
      Transform_Res_91,
      Pos,
      Row_Pos_112);


   Parser.Current_Pos := Row_Pos_112;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_91;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Try_Stmt_Transform_Parse_2;

   


function With_Item_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_As_Name_Node
is
   use Bare_As_Name_Node_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_118 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_200 :
            Token_Index
               := No_Token_Index;
      Defer_Res_200 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Row_Pos_119 :
            Token_Index
               := No_Token_Index;
      Token_Pos_173 :
            Token_Index
               := No_Token_Index;
      Token_Res_173 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_201 :
            Token_Index
               := No_Token_Index;
      Defer_Res_201 :
            Bare_Expr
               := No_Bare_Turkixir_Node;
      Transform_Res_93 :
            Bare_As_Name_Node
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.With_Item_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_93 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_93;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_93;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_118 := Pos;



Defer_Res_200 :=
   Test_Or_Parse_0 (Parser, Row_Pos_118);
Defer_Pos_200 := Parser.Current_Pos;



if Defer_Pos_200 /= No_Token_Index then

   Row_Pos_118 := Defer_Pos_200;

else
   Row_Pos_118 := No_Token_Index;
   goto Exit_Row_118_0;

end if;


--  Start opt_code




--  Start row_code

Row_Pos_119 := Row_Pos_118;



--  Start tok_code

Token_Res_173 := Row_Pos_119;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_173));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__As)
   then
       Token_Pos_173 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_119 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_119,
             Expected_Token_Id => Turkixir_T_T__As,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_173 := Row_Pos_119 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_173 /= No_Token_Index then

   Row_Pos_119 := Token_Pos_173;

else
   Row_Pos_119 := No_Token_Index;
   goto Exit_Row_119_0;

end if;


Defer_Res_201 :=
   Expr_Or_Parse_0 (Parser, Row_Pos_119);
Defer_Pos_201 := Parser.Current_Pos;



if Defer_Pos_201 /= No_Token_Index then

   Row_Pos_119 := Defer_Pos_201;

else
   Row_Pos_119 := No_Token_Index;
   goto Exit_Row_119_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_119_0>>
pragma Warnings (On, "referenced");

--  End row_code


if Row_Pos_119 = No_Token_Index then

        Defer_Res_201 := No_Bare_Turkixir_Node;


    Row_Pos_119 := Row_Pos_118;


end if;

--  End opt_code



if Row_Pos_119 /= No_Token_Index then

   Row_Pos_118 := Row_Pos_119;

else
   Row_Pos_118 := No_Token_Index;
   goto Exit_Row_118_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_118_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_118 /= No_Token_Index then

   Transform_Res_93 := Allocate_As_Name_Node (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_93,
      Kind => Turkixir_As_Name_Node,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_118 = Pos
                            then No_Token_Index
                            else Row_Pos_118 - 1));

      Initialize_Fields_For_As_Name_Node
        (Self => Transform_Res_93, As_Name_Node_F_Imported => Defer_Res_200, As_Name_Node_F_As_Name => Defer_Res_201);

         if Defer_Res_200 /= null and then Is_Incomplete (Defer_Res_200) then
            Transform_Res_93.Last_Attempted_Child := 0;
         elsif Defer_Res_200 /= null and then not Is_Ghost (Defer_Res_200) then
            Transform_Res_93.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_201 /= null and then Is_Incomplete (Defer_Res_201) then
            Transform_Res_93.Last_Attempted_Child := 0;
         elsif Defer_Res_201 /= null and then not Is_Ghost (Defer_Res_201) then
            Transform_Res_93.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.With_Item_Transform_Parse_0_Memo,
      Row_Pos_118 /= No_Token_Index,
      Transform_Res_93,
      Pos,
      Row_Pos_118);


   Parser.Current_Pos := Row_Pos_118;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_93;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end With_Item_Transform_Parse_0;

   


function With_Stmt_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_With_Stmt
is
   use Bare_With_Stmt_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_117 :
            Token_Index
               := No_Token_Index;
      Token_Pos_170 :
            Token_Index
               := No_Token_Index;
      Token_Res_170 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_20 :
            Token_Index
               := No_Token_Index;
      Tmp_List_20 :
            Free_Parse_List;
      Defer_Pos_198 :
            Token_Index
               := No_Token_Index;
      Defer_Res_198 :
            Bare_As_Name_Node
               := No_Bare_Turkixir_Node;
      Token_Pos_171 :
            Token_Index
               := No_Token_Index;
      Token_Res_171 :
            Token_Index
               := No_Token_Index;
      List_Pos_20 :
            Token_Index
               := No_Token_Index;
      List_Res_20 :
            Bare_As_Name_Node_List
               := No_Bare_Turkixir_Node;
      Token_Pos_172 :
            Token_Index
               := No_Token_Index;
      Token_Res_172 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_199 :
            Token_Index
               := No_Token_Index;
      Defer_Res_199 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Transform_Res_92 :
            Bare_With_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.With_Stmt_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_92 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_92;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_92;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_117 := Pos;



--  Start tok_code

Token_Res_170 := Row_Pos_117;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_170));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__With)
   then
       Token_Pos_170 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_117 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_117,
             Expected_Token_Id => Turkixir_T_T__With,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_170 := Row_Pos_117 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_170 /= No_Token_Index then

   Row_Pos_117 := Token_Pos_170;

else
   Row_Pos_117 := No_Token_Index;
   goto Exit_Row_117_0;

end if;


--  Start list_code

    List_Pos_20 := No_Token_Index;



Lst_Cpos_20 := Row_Pos_117;
Tmp_List_20 := Get_Parse_List (Parser);

loop
   
Defer_Res_198 :=
   With_Item_Transform_Parse_0 (Parser, Lst_Cpos_20);
Defer_Pos_198 := Parser.Current_Pos;


   exit when Defer_Pos_198 = No_Token_Index;

   List_Pos_20 := Defer_Pos_198;
   Lst_Cpos_20 := List_Pos_20;

   Tmp_List_20.Nodes.Append (Defer_Res_198);

      
--  Start tok_code

Token_Res_171 := Lst_Cpos_20;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_171));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Comma)
   then
       Token_Pos_171 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Lst_Cpos_20 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Lst_Cpos_20,
             Expected_Token_Id => Turkixir_T_T__Comma,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_171 := Lst_Cpos_20 + 1;
   end if;
end;

--  End tok_code

      if Token_Pos_171 /= No_Token_Index then
          Lst_Cpos_20 := Token_Pos_171;
      else
         exit;
      end if;

end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_20.Nodes.Length;
begin
   List_Res_20 :=
      Allocate_As_Name_Node_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_117;
      Token_End := (if Lst_Cpos_20 = Row_Pos_117
                    then Row_Pos_117
                    else Lst_Cpos_20 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_117, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_20,
      Kind              => Turkixir_As_Name_Node_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_20,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_20.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_20.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_20);

--  End list_code



if List_Pos_20 /= No_Token_Index then

   Row_Pos_117 := List_Pos_20;

else
   Row_Pos_117 := No_Token_Index;
   goto Exit_Row_117_0;

end if;


--  Start tok_code

Token_Res_172 := Row_Pos_117;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_172));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_172 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_117 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_117,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_172 := Row_Pos_117 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_172 /= No_Token_Index then

   Row_Pos_117 := Token_Pos_172;

else
   Row_Pos_117 := No_Token_Index;
   goto Exit_Row_117_0;

end if;


Defer_Res_199 :=
   Suite_Or_Parse_0 (Parser, Row_Pos_117);
Defer_Pos_199 := Parser.Current_Pos;



if Defer_Pos_199 /= No_Token_Index then

   Row_Pos_117 := Defer_Pos_199;

else
   Row_Pos_117 := No_Token_Index;
   goto Exit_Row_117_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_117_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_117 /= No_Token_Index then

   Transform_Res_92 := Allocate_With_Stmt (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_92,
      Kind => Turkixir_With_Stmt,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_117 = Pos
                            then No_Token_Index
                            else Row_Pos_117 - 1));

      Initialize_Fields_For_With_Stmt
        (Self => Transform_Res_92, With_Stmt_F_Bindings => List_Res_20, With_Stmt_F_Statements => Defer_Res_199);

         if List_Res_20 /= null and then Is_Incomplete (List_Res_20) then
            Transform_Res_92.Last_Attempted_Child := 0;
         elsif List_Res_20 /= null and then not Is_Ghost (List_Res_20) then
            Transform_Res_92.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_199 /= null and then Is_Incomplete (Defer_Res_199) then
            Transform_Res_92.Last_Attempted_Child := 0;
         elsif Defer_Res_199 /= null and then not Is_Ghost (Defer_Res_199) then
            Transform_Res_92.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.With_Stmt_Transform_Parse_0_Memo,
      Row_Pos_117 /= No_Token_Index,
      Transform_Res_92,
      Pos,
      Row_Pos_117);


   Parser.Current_Pos := Row_Pos_117;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_92;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end With_Stmt_Transform_Parse_0;

   


function Parameters_Extract_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Params
is
   use Bare_Params_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_121 :
            Token_Index
               := No_Token_Index;
      Token_Pos_176 :
            Token_Index
               := No_Token_Index;
      Token_Res_176 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_205 :
            Token_Index
               := No_Token_Index;
      Defer_Res_205 :
            Bare_Params
               := No_Bare_Turkixir_Node;
      Token_Pos_177 :
            Token_Index
               := No_Token_Index;
      Token_Res_177 :
            Token_Index
               := No_Token_Index;


   M : Memo_Entry := Get (Parser.Private_Part.Parameters_Extract_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Defer_Res_205 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Defer_Res_205;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Defer_Res_205;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start row_code

Row_Pos_121 := Pos;



--  Start tok_code

Token_Res_176 := Row_Pos_121;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_176));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_L_Par)
   then
       Token_Pos_176 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_121 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_121,
             Expected_Token_Id => Turkixir_T_T_L_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_176 := Row_Pos_121 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_176 /= No_Token_Index then

   Row_Pos_121 := Token_Pos_176;

else
   Row_Pos_121 := No_Token_Index;
   goto Exit_Row_121_0;

end if;


--  Start opt_code




Defer_Res_205 :=
   Varargslist_Transform_Parse_1 (Parser, Row_Pos_121);
Defer_Pos_205 := Parser.Current_Pos;


if Defer_Pos_205 = No_Token_Index then

        Defer_Res_205 := No_Bare_Turkixir_Node;


    Defer_Pos_205 := Row_Pos_121;


end if;

--  End opt_code



if Defer_Pos_205 /= No_Token_Index then

   Row_Pos_121 := Defer_Pos_205;

else
   Row_Pos_121 := No_Token_Index;
   goto Exit_Row_121_0;

end if;


--  Start tok_code

Token_Res_177 := Row_Pos_121;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_177));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_R_Par)
   then
       Token_Pos_177 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_121 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_121,
             Expected_Token_Id => Turkixir_T_T_R_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_177 := Row_Pos_121 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_177 /= No_Token_Index then

   Row_Pos_121 := Token_Pos_177;

else
   Row_Pos_121 := No_Token_Index;
   goto Exit_Row_121_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_121_0>>
pragma Warnings (On, "referenced");

--  End row_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Parameters_Extract_Parse_0_Memo,
      Row_Pos_121 /= No_Token_Index,
      Defer_Res_205,
      Pos,
      Row_Pos_121);


   Parser.Current_Pos := Row_Pos_121;

   Exit_Call (Parser, Call_Depth);
   return Defer_Res_205;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Parameters_Extract_Parse_0;

   


function Func_Def_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Func_Def
is
   use Bare_Func_Def_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_120 :
            Token_Index
               := No_Token_Index;
      Token_Pos_174 :
            Token_Index
               := No_Token_Index;
      Token_Res_174 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_202 :
            Token_Index
               := No_Token_Index;
      Defer_Res_202 :
            Bare_Id
               := No_Bare_Turkixir_Node;
      Defer_Pos_203 :
            Token_Index
               := No_Token_Index;
      Defer_Res_203 :
            Bare_Params
               := No_Bare_Turkixir_Node;
      Token_Pos_175 :
            Token_Index
               := No_Token_Index;
      Token_Res_175 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_204 :
            Token_Index
               := No_Token_Index;
      Defer_Res_204 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Transform_Res_94 :
            Bare_Func_Def
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Func_Def_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_94 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_94;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_94;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_120 := Pos;



--  Start tok_code

Token_Res_174 := Row_Pos_120;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_174));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Def)
   then
       Token_Pos_174 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_120 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_120,
             Expected_Token_Id => Turkixir_T_T__Def,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_174 := Row_Pos_120 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_174 /= No_Token_Index then

   Row_Pos_120 := Token_Pos_174;

else
   Row_Pos_120 := No_Token_Index;
   goto Exit_Row_120_0;

end if;


Defer_Res_202 :=
   Name_Transform_Parse_0 (Parser, Row_Pos_120);
Defer_Pos_202 := Parser.Current_Pos;



if Defer_Pos_202 /= No_Token_Index then

   Row_Pos_120 := Defer_Pos_202;

else
   Row_Pos_120 := No_Token_Index;
   goto Exit_Row_120_0;

end if;


Defer_Res_203 :=
   Parameters_Extract_Parse_0 (Parser, Row_Pos_120);
Defer_Pos_203 := Parser.Current_Pos;



if Defer_Pos_203 /= No_Token_Index then

   Row_Pos_120 := Defer_Pos_203;

else
   Row_Pos_120 := No_Token_Index;
   goto Exit_Row_120_0;

end if;


--  Start tok_code

Token_Res_175 := Row_Pos_120;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_175));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_175 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_120 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_120,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_175 := Row_Pos_120 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_175 /= No_Token_Index then

   Row_Pos_120 := Token_Pos_175;

else
   Row_Pos_120 := No_Token_Index;
   goto Exit_Row_120_0;

end if;


Defer_Res_204 :=
   Suite_Or_Parse_0 (Parser, Row_Pos_120);
Defer_Pos_204 := Parser.Current_Pos;



if Defer_Pos_204 /= No_Token_Index then

   Row_Pos_120 := Defer_Pos_204;

else
   Row_Pos_120 := No_Token_Index;
   goto Exit_Row_120_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_120_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_120 /= No_Token_Index then

   Transform_Res_94 := Allocate_Func_Def (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_94,
      Kind => Turkixir_Func_Def,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_120 = Pos
                            then No_Token_Index
                            else Row_Pos_120 - 1));

      Initialize_Fields_For_Func_Def
        (Self => Transform_Res_94, Func_Def_F_Name => Defer_Res_202, Func_Def_F_Parameters => Defer_Res_203, Func_Def_F_Body => Defer_Res_204);

         if Defer_Res_202 /= null and then Is_Incomplete (Defer_Res_202) then
            Transform_Res_94.Last_Attempted_Child := 0;
         elsif Defer_Res_202 /= null and then not Is_Ghost (Defer_Res_202) then
            Transform_Res_94.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_203 /= null and then Is_Incomplete (Defer_Res_203) then
            Transform_Res_94.Last_Attempted_Child := 0;
         elsif Defer_Res_203 /= null and then not Is_Ghost (Defer_Res_203) then
            Transform_Res_94.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_204 /= null and then Is_Incomplete (Defer_Res_204) then
            Transform_Res_94.Last_Attempted_Child := 0;
         elsif Defer_Res_204 /= null and then not Is_Ghost (Defer_Res_204) then
            Transform_Res_94.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Func_Def_Transform_Parse_0_Memo,
      Row_Pos_120 /= No_Token_Index,
      Transform_Res_94,
      Pos,
      Row_Pos_120);


   Parser.Current_Pos := Row_Pos_120;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_94;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Func_Def_Transform_Parse_0;

   


function Decorator_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Decorator
is
   use Bare_Decorator_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_123 :
            Token_Index
               := No_Token_Index;
      Token_Pos_178 :
            Token_Index
               := No_Token_Index;
      Token_Res_178 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_210 :
            Token_Index
               := No_Token_Index;
      Defer_Res_210 :
            Bare_Name
               := No_Bare_Turkixir_Node;
      Row_Pos_124 :
            Token_Index
               := No_Token_Index;
      Token_Pos_179 :
            Token_Index
               := No_Token_Index;
      Token_Res_179 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_211 :
            Token_Index
               := No_Token_Index;
      Defer_Res_211 :
            Bare_Arg_List
               := No_Bare_Turkixir_Node;
      Token_Pos_180 :
            Token_Index
               := No_Token_Index;
      Token_Res_180 :
            Token_Index
               := No_Token_Index;
      Token_Pos_181 :
            Token_Index
               := No_Token_Index;
      Token_Res_181 :
            Token_Index
               := No_Token_Index;
      Transform_Res_96 :
            Bare_Decorator
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Decorator_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_96 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_96;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_96;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_123 := Pos;



--  Start tok_code

Token_Res_178 := Row_Pos_123;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_178));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__At)
   then
       Token_Pos_178 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_123 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_123,
             Expected_Token_Id => Turkixir_T_T__At,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_178 := Row_Pos_123 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_178 /= No_Token_Index then

   Row_Pos_123 := Token_Pos_178;

else
   Row_Pos_123 := No_Token_Index;
   goto Exit_Row_123_0;

end if;


Defer_Res_210 :=
   Dotted_Name_Or_Parse_0 (Parser, Row_Pos_123);
Defer_Pos_210 := Parser.Current_Pos;



if Defer_Pos_210 /= No_Token_Index then

   Row_Pos_123 := Defer_Pos_210;

else
   Row_Pos_123 := No_Token_Index;
   goto Exit_Row_123_0;

end if;


--  Start opt_code




--  Start row_code

Row_Pos_124 := Row_Pos_123;



--  Start tok_code

Token_Res_179 := Row_Pos_124;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_179));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_L_Par)
   then
       Token_Pos_179 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_124 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_124,
             Expected_Token_Id => Turkixir_T_T_L_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_179 := Row_Pos_124 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_179 /= No_Token_Index then

   Row_Pos_124 := Token_Pos_179;

else
   Row_Pos_124 := No_Token_Index;
   goto Exit_Row_124_0;

end if;


Defer_Res_211 :=
   Arg_List_Extract_Parse_1 (Parser, Row_Pos_124);
Defer_Pos_211 := Parser.Current_Pos;



if Defer_Pos_211 /= No_Token_Index then

   Row_Pos_124 := Defer_Pos_211;

else
   Row_Pos_124 := No_Token_Index;
   goto Exit_Row_124_0;

end if;


--  Start tok_code

Token_Res_180 := Row_Pos_124;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_180));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_R_Par)
   then
       Token_Pos_180 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_124 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_124,
             Expected_Token_Id => Turkixir_T_T_R_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_180 := Row_Pos_124 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_180 /= No_Token_Index then

   Row_Pos_124 := Token_Pos_180;

else
   Row_Pos_124 := No_Token_Index;
   goto Exit_Row_124_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_124_0>>
pragma Warnings (On, "referenced");

--  End row_code


if Row_Pos_124 = No_Token_Index then

        Defer_Res_211 :=
           Allocate_Arg_List (Parser.Mem_Pool);
         Initialize
           (Self              => Defer_Res_211,
            Kind              => Turkixir_Arg_List,
            Unit              => Parser.Unit,
            Token_Start_Index => Row_Pos_123 - 1,
            Token_End_Index   => No_Token_Index);
         Initialize_List
           (Self   => Defer_Res_211,
            Parser => Parser,
            Count  => 0);


    Row_Pos_124 := Row_Pos_123;


end if;

--  End opt_code



if Row_Pos_124 /= No_Token_Index then

   Row_Pos_123 := Row_Pos_124;

else
   Row_Pos_123 := No_Token_Index;
   goto Exit_Row_123_0;

end if;


--  Start tok_code

Token_Res_181 := Row_Pos_123;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_181));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_Newline)
   then
       Token_Pos_181 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_123 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_123,
             Expected_Token_Id => Turkixir_Newline,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_181 := Row_Pos_123 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_181 /= No_Token_Index then

   Row_Pos_123 := Token_Pos_181;

else
   Row_Pos_123 := No_Token_Index;
   goto Exit_Row_123_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_123_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_123 /= No_Token_Index then

   Transform_Res_96 := Allocate_Decorator (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_96,
      Kind => Turkixir_Decorator,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_123 = Pos
                            then No_Token_Index
                            else Row_Pos_123 - 1));

      Initialize_Fields_For_Decorator
        (Self => Transform_Res_96, Decorator_F_Dec_Name => Defer_Res_210, Decorator_F_Arg_List => Defer_Res_211);

         if Defer_Res_210 /= null and then Is_Incomplete (Defer_Res_210) then
            Transform_Res_96.Last_Attempted_Child := 0;
         elsif Defer_Res_210 /= null and then not Is_Ghost (Defer_Res_210) then
            Transform_Res_96.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_211 /= null and then Is_Incomplete (Defer_Res_211) then
            Transform_Res_96.Last_Attempted_Child := 0;
         elsif Defer_Res_211 /= null and then not Is_Ghost (Defer_Res_211) then
            Transform_Res_96.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Decorator_Transform_Parse_0_Memo,
      Row_Pos_123 /= No_Token_Index,
      Transform_Res_96,
      Pos,
      Row_Pos_123);


   Parser.Current_Pos := Row_Pos_123;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_96;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Decorator_Transform_Parse_0;

   


function Decorators_List_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Decorator_List
is
   use Bare_Decorator_List_Memos;

   Call_Depth : aliased Natural;

      Lst_Cpos_21 :
            Token_Index
               := No_Token_Index;
      Tmp_List_21 :
            Free_Parse_List;
      Defer_Pos_209 :
            Token_Index
               := No_Token_Index;
      Defer_Res_209 :
            Bare_Decorator
               := No_Bare_Turkixir_Node;
      List_Pos_21 :
            Token_Index
               := No_Token_Index;
      List_Res_21 :
            Bare_Decorator_List
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Decorators_List_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      List_Res_21 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return List_Res_21;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return List_Res_21;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start list_code

    List_Pos_21 := No_Token_Index;



Lst_Cpos_21 := Pos;
Tmp_List_21 := Get_Parse_List (Parser);

loop
   
Defer_Res_209 :=
   Decorator_Transform_Parse_0 (Parser, Lst_Cpos_21);
Defer_Pos_209 := Parser.Current_Pos;


   exit when Defer_Pos_209 = No_Token_Index;

   List_Pos_21 := Defer_Pos_209;
   Lst_Cpos_21 := List_Pos_21;

   Tmp_List_21.Nodes.Append (Defer_Res_209);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_21.Nodes.Length;
begin
   List_Res_21 :=
      Allocate_Decorator_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Pos;
      Token_End := (if Lst_Cpos_21 = Pos
                    then Pos
                    else Lst_Cpos_21 - 1);

   else
      Token_Start := Token_Index'Max (Pos, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_21,
      Kind              => Turkixir_Decorator_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_21,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_21.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_21.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_21);

--  End list_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Decorators_List_Parse_0_Memo,
      List_Pos_21 /= No_Token_Index,
      List_Res_21,
      Pos,
      List_Pos_21);


   Parser.Current_Pos := List_Pos_21;

   Exit_Call (Parser, Call_Depth);
   return List_Res_21;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Decorators_List_Parse_0;

   


function Decorated_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Decorated
is
   use Bare_Decorated_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_122 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_206 :
            Token_Index
               := No_Token_Index;
      Defer_Res_206 :
            Bare_Decorator_List
               := No_Bare_Turkixir_Node;
      Defer_Pos_207 :
            Token_Index
               := No_Token_Index;
      Defer_Res_207 :
            Bare_Class_Def
               := No_Bare_Turkixir_Node;
      Defer_Pos_208 :
            Token_Index
               := No_Token_Index;
      Defer_Res_208 :
            Bare_Func_Def
               := No_Bare_Turkixir_Node;
      Or_Pos_42 :
            Token_Index
               := No_Token_Index;
      Or_Res_42 :
            Bare_Def_Stmt
               := No_Bare_Turkixir_Node;
      Transform_Res_95 :
            Bare_Decorated
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Decorated_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_95 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_95;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_95;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_122 := Pos;



Defer_Res_206 :=
   Decorators_List_Parse_0 (Parser, Row_Pos_122);
Defer_Pos_206 := Parser.Current_Pos;



if Defer_Pos_206 /= No_Token_Index then

   Row_Pos_122 := Defer_Pos_206;

else
   Row_Pos_122 := No_Token_Index;
   goto Exit_Row_122_0;

end if;


--  Start or_code

Or_Pos_42 := No_Token_Index;
Or_Res_42 := No_Bare_Turkixir_Node;
    
Defer_Res_207 :=
   Class_Def_Transform_Parse_0 (Parser, Row_Pos_122);
Defer_Pos_207 := Parser.Current_Pos;

    if Defer_Pos_207 /= No_Token_Index then
        Or_Pos_42 := Defer_Pos_207;
        Or_Res_42 := Defer_Res_207;
        goto Exit_Or_42;
    end if;
    
Defer_Res_208 :=
   Func_Def_Transform_Parse_0 (Parser, Row_Pos_122);
Defer_Pos_208 := Parser.Current_Pos;

    if Defer_Pos_208 /= No_Token_Index then
        Or_Pos_42 := Defer_Pos_208;
        Or_Res_42 := Defer_Res_208;
        goto Exit_Or_42;
    end if;
<<Exit_Or_42>>

--  End or_code



if Or_Pos_42 /= No_Token_Index then

   Row_Pos_122 := Or_Pos_42;

else
   Row_Pos_122 := No_Token_Index;
   goto Exit_Row_122_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_122_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_122 /= No_Token_Index then

   Transform_Res_95 := Allocate_Decorated (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_95,
      Kind => Turkixir_Decorated,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_122 = Pos
                            then No_Token_Index
                            else Row_Pos_122 - 1));

      Initialize_Fields_For_Decorated
        (Self => Transform_Res_95, Decorated_F_Decorators => Defer_Res_206, Decorated_F_Defn => Or_Res_42);

         if Defer_Res_206 /= null and then Is_Incomplete (Defer_Res_206) then
            Transform_Res_95.Last_Attempted_Child := 0;
         elsif Defer_Res_206 /= null and then not Is_Ghost (Defer_Res_206) then
            Transform_Res_95.Last_Attempted_Child := -1;
         end if;
         if Or_Res_42 /= null and then Is_Incomplete (Or_Res_42) then
            Transform_Res_95.Last_Attempted_Child := 0;
         elsif Or_Res_42 /= null and then not Is_Ghost (Or_Res_42) then
            Transform_Res_95.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Decorated_Transform_Parse_0_Memo,
      Row_Pos_122 /= No_Token_Index,
      Transform_Res_95,
      Pos,
      Row_Pos_122);


   Parser.Current_Pos := Row_Pos_122;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_95;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Decorated_Transform_Parse_0;

   


function Compound_Stmt_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Stmt
is
   use Bare_Stmt_Memos;

   Call_Depth : aliased Natural;

      Defer_Pos_171 :
            Token_Index
               := No_Token_Index;
      Defer_Res_171 :
            Bare_If_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_172 :
            Token_Index
               := No_Token_Index;
      Defer_Res_172 :
            Bare_While_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_173 :
            Token_Index
               := No_Token_Index;
      Defer_Res_173 :
            Bare_For_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_174 :
            Token_Index
               := No_Token_Index;
      Defer_Res_174 :
            Bare_Try_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_175 :
            Token_Index
               := No_Token_Index;
      Defer_Res_175 :
            Bare_With_Stmt
               := No_Bare_Turkixir_Node;
      Defer_Pos_176 :
            Token_Index
               := No_Token_Index;
      Defer_Res_176 :
            Bare_Func_Def
               := No_Bare_Turkixir_Node;
      Defer_Pos_177 :
            Token_Index
               := No_Token_Index;
      Defer_Res_177 :
            Bare_Class_Def
               := No_Bare_Turkixir_Node;
      Defer_Pos_178 :
            Token_Index
               := No_Token_Index;
      Defer_Res_178 :
            Bare_Decorated
               := No_Bare_Turkixir_Node;
      Or_Pos_41 :
            Token_Index
               := No_Token_Index;
      Or_Res_41 :
            Bare_Stmt
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Compound_Stmt_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_41 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_41;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_41;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_41 := No_Token_Index;
Or_Res_41 := No_Bare_Turkixir_Node;
    
Defer_Res_171 :=
   If_Stmt_Transform_Parse_1 (Parser, Pos);
Defer_Pos_171 := Parser.Current_Pos;

    if Defer_Pos_171 /= No_Token_Index then
        Or_Pos_41 := Defer_Pos_171;
        Or_Res_41 := Defer_Res_171;
        goto Exit_Or_41;
    end if;
    
Defer_Res_172 :=
   While_Stmt_Transform_Parse_0 (Parser, Pos);
Defer_Pos_172 := Parser.Current_Pos;

    if Defer_Pos_172 /= No_Token_Index then
        Or_Pos_41 := Defer_Pos_172;
        Or_Res_41 := Defer_Res_172;
        goto Exit_Or_41;
    end if;
    
Defer_Res_173 :=
   For_Stmt_Transform_Parse_0 (Parser, Pos);
Defer_Pos_173 := Parser.Current_Pos;

    if Defer_Pos_173 /= No_Token_Index then
        Or_Pos_41 := Defer_Pos_173;
        Or_Res_41 := Defer_Res_173;
        goto Exit_Or_41;
    end if;
    
Defer_Res_174 :=
   Try_Stmt_Transform_Parse_2 (Parser, Pos);
Defer_Pos_174 := Parser.Current_Pos;

    if Defer_Pos_174 /= No_Token_Index then
        Or_Pos_41 := Defer_Pos_174;
        Or_Res_41 := Defer_Res_174;
        goto Exit_Or_41;
    end if;
    
Defer_Res_175 :=
   With_Stmt_Transform_Parse_0 (Parser, Pos);
Defer_Pos_175 := Parser.Current_Pos;

    if Defer_Pos_175 /= No_Token_Index then
        Or_Pos_41 := Defer_Pos_175;
        Or_Res_41 := Defer_Res_175;
        goto Exit_Or_41;
    end if;
    
Defer_Res_176 :=
   Func_Def_Transform_Parse_0 (Parser, Pos);
Defer_Pos_176 := Parser.Current_Pos;

    if Defer_Pos_176 /= No_Token_Index then
        Or_Pos_41 := Defer_Pos_176;
        Or_Res_41 := Defer_Res_176;
        goto Exit_Or_41;
    end if;
    
Defer_Res_177 :=
   Class_Def_Transform_Parse_0 (Parser, Pos);
Defer_Pos_177 := Parser.Current_Pos;

    if Defer_Pos_177 /= No_Token_Index then
        Or_Pos_41 := Defer_Pos_177;
        Or_Res_41 := Defer_Res_177;
        goto Exit_Or_41;
    end if;
    
Defer_Res_178 :=
   Decorated_Transform_Parse_0 (Parser, Pos);
Defer_Pos_178 := Parser.Current_Pos;

    if Defer_Pos_178 /= No_Token_Index then
        Or_Pos_41 := Defer_Pos_178;
        Or_Res_41 := Defer_Res_178;
        goto Exit_Or_41;
    end if;
<<Exit_Or_41>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Compound_Stmt_Or_Parse_0_Memo,
      Or_Pos_41 /= No_Token_Index,
      Or_Res_41,
      Pos,
      Or_Pos_41);


   Parser.Current_Pos := Or_Pos_41;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_41;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Compound_Stmt_Or_Parse_0;

   


function Stmt_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node
is
   use Bare_Turkixir_Node_Memos;

   Call_Depth : aliased Natural;

      Defer_Pos_119 :
            Token_Index
               := No_Token_Index;
      Defer_Res_119 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Defer_Pos_120 :
            Token_Index
               := No_Token_Index;
      Defer_Res_120 :
            Bare_Stmt
               := No_Bare_Turkixir_Node;
      Or_Pos_26 :
            Token_Index
               := No_Token_Index;
      Or_Res_26 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Stmt_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_26 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_26;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_26;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_26 := No_Token_Index;
Or_Res_26 := No_Bare_Turkixir_Node;
    
Defer_Res_119 :=
   Simple_Stmt_Extract_Parse_1 (Parser, Pos);
Defer_Pos_119 := Parser.Current_Pos;

    if Defer_Pos_119 /= No_Token_Index then
        Or_Pos_26 := Defer_Pos_119;
        Or_Res_26 := Defer_Res_119;
        goto Exit_Or_26;
    end if;
    
Defer_Res_120 :=
   Compound_Stmt_Or_Parse_0 (Parser, Pos);
Defer_Pos_120 := Parser.Current_Pos;

    if Defer_Pos_120 /= No_Token_Index then
        Or_Pos_26 := Defer_Pos_120;
        Or_Res_26 := Defer_Res_120;
        goto Exit_Or_26;
    end if;
<<Exit_Or_26>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Stmt_Or_Parse_0_Memo,
      Or_Pos_26 /= No_Token_Index,
      Or_Res_26,
      Pos,
      Or_Pos_26);


   Parser.Current_Pos := Or_Pos_26;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_26;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Stmt_Or_Parse_0;

   


function Suite_Or_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Turkixir_Node
is
   use Bare_Turkixir_Node_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_77 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_9 :
            Token_Index
               := No_Token_Index;
      Tmp_List_9 :
            Free_Parse_List;
      Defer_Pos_114 :
            Token_Index
               := No_Token_Index;
      Defer_Res_114 :
            Bare_NL
               := No_Bare_Turkixir_Node;
      List_Pos_9 :
            Token_Index
               := No_Token_Index;
      List_Res_9 :
            Bare_NL_List
               := No_Bare_Turkixir_Node;
      Token_Pos_108 :
            Token_Index
               := No_Token_Index;
      Token_Res_108 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_10 :
            Token_Index
               := No_Token_Index;
      Tmp_List_10 :
            Free_Parse_List;
      Row_Pos_78 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_11 :
            Token_Index
               := No_Token_Index;
      Tmp_List_11 :
            Free_Parse_List;
      Defer_Pos_115 :
            Token_Index
               := No_Token_Index;
      Defer_Res_115 :
            Bare_NL
               := No_Bare_Turkixir_Node;
      List_Pos_10 :
            Token_Index
               := No_Token_Index;
      List_Res_10 :
            Bare_NL_List
               := No_Bare_Turkixir_Node;
      Defer_Pos_116 :
            Token_Index
               := No_Token_Index;
      Defer_Res_116 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Lst_Cpos_12 :
            Token_Index
               := No_Token_Index;
      Tmp_List_12 :
            Free_Parse_List;
      Defer_Pos_117 :
            Token_Index
               := No_Token_Index;
      Defer_Res_117 :
            Bare_NL
               := No_Bare_Turkixir_Node;
      List_Pos_11 :
            Token_Index
               := No_Token_Index;
      List_Res_11 :
            Bare_NL_List
               := No_Bare_Turkixir_Node;
      List_Pos_12 :
            Token_Index
               := No_Token_Index;
      List_Res_12 :
            Bare_Turkixir_Node_List
               := No_Bare_Turkixir_Node;
      Token_Pos_109 :
            Token_Index
               := No_Token_Index;
      Token_Res_109 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_118 :
            Token_Index
               := No_Token_Index;
      Defer_Res_118 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Or_Pos_25 :
            Token_Index
               := No_Token_Index;
      Or_Res_25 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Suite_Or_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Or_Res_25 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_25;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Or_Res_25;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start or_code

Or_Pos_25 := No_Token_Index;
Or_Res_25 := No_Bare_Turkixir_Node;
    
--  Start row_code

Row_Pos_77 := Pos;



--  Start list_code

    List_Pos_9 := Row_Pos_77;



Lst_Cpos_9 := Row_Pos_77;
Tmp_List_9 := Get_Parse_List (Parser);

loop
   
Defer_Res_114 :=
   Nl_Transform_Parse_0 (Parser, Lst_Cpos_9);
Defer_Pos_114 := Parser.Current_Pos;


   exit when Defer_Pos_114 = No_Token_Index;

   List_Pos_9 := Defer_Pos_114;
   Lst_Cpos_9 := List_Pos_9;

   Tmp_List_9.Nodes.Append (Defer_Res_114);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_9.Nodes.Length;
begin
   List_Res_9 :=
      Allocate_NL_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_77;
      Token_End := (if Lst_Cpos_9 = Row_Pos_77
                    then Row_Pos_77
                    else Lst_Cpos_9 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_77, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_9,
      Kind              => Turkixir_NL_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_9,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_9.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_9.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_9);

--  End list_code



if List_Pos_9 /= No_Token_Index then

   Row_Pos_77 := List_Pos_9;

else
   Row_Pos_77 := No_Token_Index;
   goto Exit_Row_77_0;

end if;


--  Start tok_code

Token_Res_108 := Row_Pos_77;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_108));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_Indent)
   then
       Token_Pos_108 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_77 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_77,
             Expected_Token_Id => Turkixir_Indent,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_108 := Row_Pos_77 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_108 /= No_Token_Index then

   Row_Pos_77 := Token_Pos_108;

else
   Row_Pos_77 := No_Token_Index;
   goto Exit_Row_77_0;

end if;


--  Start list_code

    List_Pos_12 := No_Token_Index;



Lst_Cpos_10 := Row_Pos_77;
Tmp_List_10 := Get_Parse_List (Parser);

loop
   
--  Start row_code

Row_Pos_78 := Lst_Cpos_10;



--  Start list_code

    List_Pos_10 := Row_Pos_78;



Lst_Cpos_11 := Row_Pos_78;
Tmp_List_11 := Get_Parse_List (Parser);

loop
   
Defer_Res_115 :=
   Nl_Transform_Parse_0 (Parser, Lst_Cpos_11);
Defer_Pos_115 := Parser.Current_Pos;


   exit when Defer_Pos_115 = No_Token_Index;

   List_Pos_10 := Defer_Pos_115;
   Lst_Cpos_11 := List_Pos_10;

   Tmp_List_11.Nodes.Append (Defer_Res_115);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_11.Nodes.Length;
begin
   List_Res_10 :=
      Allocate_NL_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_78;
      Token_End := (if Lst_Cpos_11 = Row_Pos_78
                    then Row_Pos_78
                    else Lst_Cpos_11 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_78, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_10,
      Kind              => Turkixir_NL_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_10,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_11.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_10.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_11);

--  End list_code



if List_Pos_10 /= No_Token_Index then

   Row_Pos_78 := List_Pos_10;

else
   Row_Pos_78 := No_Token_Index;
   goto Exit_Row_79_0;

end if;


Defer_Res_116 :=
   Stmt_Or_Parse_0 (Parser, Row_Pos_78);
Defer_Pos_116 := Parser.Current_Pos;



if Defer_Pos_116 /= No_Token_Index then

   Row_Pos_78 := Defer_Pos_116;

else
   Row_Pos_78 := No_Token_Index;
   goto Exit_Row_79_0;

end if;


--  Start list_code

    List_Pos_11 := Row_Pos_78;



Lst_Cpos_12 := Row_Pos_78;
Tmp_List_12 := Get_Parse_List (Parser);

loop
   
Defer_Res_117 :=
   Nl_Transform_Parse_0 (Parser, Lst_Cpos_12);
Defer_Pos_117 := Parser.Current_Pos;


   exit when Defer_Pos_117 = No_Token_Index;

   List_Pos_11 := Defer_Pos_117;
   Lst_Cpos_12 := List_Pos_11;

   Tmp_List_12.Nodes.Append (Defer_Res_117);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_12.Nodes.Length;
begin
   List_Res_11 :=
      Allocate_NL_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_78;
      Token_End := (if Lst_Cpos_12 = Row_Pos_78
                    then Row_Pos_78
                    else Lst_Cpos_12 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_78, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_11,
      Kind              => Turkixir_NL_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_11,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_12.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_11.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_12);

--  End list_code



if List_Pos_11 /= No_Token_Index then

   Row_Pos_78 := List_Pos_11;

else
   Row_Pos_78 := No_Token_Index;
   goto Exit_Row_79_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_79_0>>
pragma Warnings (On, "referenced");

--  End row_code


   exit when Row_Pos_78 = No_Token_Index;

   List_Pos_12 := Row_Pos_78;
   Lst_Cpos_10 := List_Pos_12;

   Tmp_List_10.Nodes.Append (Defer_Res_116);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_10.Nodes.Length;
begin
   List_Res_12 :=
      Allocate_Turkixir_Node_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_77;
      Token_End := (if Lst_Cpos_10 = Row_Pos_77
                    then Row_Pos_77
                    else Lst_Cpos_10 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_77, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_12,
      Kind              => Turkixir_Turkixir_Node_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_12,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_10.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_12.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_10);

--  End list_code



if List_Pos_12 /= No_Token_Index then

   Row_Pos_77 := List_Pos_12;

else
   Row_Pos_77 := No_Token_Index;
   goto Exit_Row_77_0;

end if;


--  Start tok_code

Token_Res_109 := Row_Pos_77;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_109));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_Dedent)
   then
       Token_Pos_109 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_77 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_77,
             Expected_Token_Id => Turkixir_Dedent,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_109 := Row_Pos_77 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_109 /= No_Token_Index then

   Row_Pos_77 := Token_Pos_109;

else
   Row_Pos_77 := No_Token_Index;
   goto Exit_Row_77_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_77_0>>
pragma Warnings (On, "referenced");

--  End row_code

    if Row_Pos_77 /= No_Token_Index then
        Or_Pos_25 := Row_Pos_77;
        Or_Res_25 := List_Res_12;
        goto Exit_Or_25;
    end if;
    
Defer_Res_118 :=
   Simple_Stmt_Extract_Parse_1 (Parser, Pos);
Defer_Pos_118 := Parser.Current_Pos;

    if Defer_Pos_118 /= No_Token_Index then
        Or_Pos_25 := Defer_Pos_118;
        Or_Res_25 := Defer_Res_118;
        goto Exit_Or_25;
    end if;
<<Exit_Or_25>>

--  End or_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Suite_Or_Parse_0_Memo,
      Or_Pos_25 /= No_Token_Index,
      Or_Res_25,
      Pos,
      Or_Pos_25);


   Parser.Current_Pos := Or_Pos_25;

   Exit_Call (Parser, Call_Depth);
   return Or_Res_25;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Suite_Or_Parse_0;

   


function Class_Def_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_Class_Def
is
   use Bare_Class_Def_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_75 :
            Token_Index
               := No_Token_Index;
      Token_Pos_104 :
            Token_Index
               := No_Token_Index;
      Token_Res_104 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_111 :
            Token_Index
               := No_Token_Index;
      Defer_Res_111 :
            Bare_Id
               := No_Bare_Turkixir_Node;
      Row_Pos_76 :
            Token_Index
               := No_Token_Index;
      Token_Pos_105 :
            Token_Index
               := No_Token_Index;
      Token_Res_105 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_112 :
            Token_Index
               := No_Token_Index;
      Defer_Res_112 :
            Bare_Expr_List
               := No_Bare_Turkixir_Node;
      Token_Pos_106 :
            Token_Index
               := No_Token_Index;
      Token_Res_106 :
            Token_Index
               := No_Token_Index;
      Token_Pos_107 :
            Token_Index
               := No_Token_Index;
      Token_Res_107 :
            Token_Index
               := No_Token_Index;
      Defer_Pos_113 :
            Token_Index
               := No_Token_Index;
      Defer_Res_113 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Transform_Res_63 :
            Bare_Class_Def
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Class_Def_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_63 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_63;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_63;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_75 := Pos;



--  Start tok_code

Token_Res_104 := Row_Pos_75;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_104));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Class)
   then
       Token_Pos_104 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_75 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_75,
             Expected_Token_Id => Turkixir_T_T__Class,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_104 := Row_Pos_75 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_104 /= No_Token_Index then

   Row_Pos_75 := Token_Pos_104;

else
   Row_Pos_75 := No_Token_Index;
   goto Exit_Row_75_0;

end if;


Defer_Res_111 :=
   Name_Transform_Parse_0 (Parser, Row_Pos_75);
Defer_Pos_111 := Parser.Current_Pos;



if Defer_Pos_111 /= No_Token_Index then

   Row_Pos_75 := Defer_Pos_111;

else
   Row_Pos_75 := No_Token_Index;
   goto Exit_Row_75_0;

end if;


--  Start opt_code




--  Start row_code

Row_Pos_76 := Row_Pos_75;



--  Start tok_code

Token_Res_105 := Row_Pos_76;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_105));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_L_Par)
   then
       Token_Pos_105 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_76 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_76,
             Expected_Token_Id => Turkixir_T_T_L_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_105 := Row_Pos_76 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_105 /= No_Token_Index then

   Row_Pos_76 := Token_Pos_105;

else
   Row_Pos_76 := No_Token_Index;
   goto Exit_Row_76_0;

end if;


--  Start opt_code




Defer_Res_112 :=
   Test_List_Extract_Parse_0 (Parser, Row_Pos_76);
Defer_Pos_112 := Parser.Current_Pos;


if Defer_Pos_112 = No_Token_Index then

        Defer_Res_112 :=
           Allocate_Expr_List (Parser.Mem_Pool);
         Initialize
           (Self              => Defer_Res_112,
            Kind              => Turkixir_Expr_List,
            Unit              => Parser.Unit,
            Token_Start_Index => Row_Pos_76 - 1,
            Token_End_Index   => No_Token_Index);
         Initialize_List
           (Self   => Defer_Res_112,
            Parser => Parser,
            Count  => 0);


    Defer_Pos_112 := Row_Pos_76;


end if;

--  End opt_code



if Defer_Pos_112 /= No_Token_Index then

   Row_Pos_76 := Defer_Pos_112;

else
   Row_Pos_76 := No_Token_Index;
   goto Exit_Row_76_0;

end if;


--  Start tok_code

Token_Res_106 := Row_Pos_76;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_106));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T_R_Par)
   then
       Token_Pos_106 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_76 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_76,
             Expected_Token_Id => Turkixir_T_T_R_Par,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_106 := Row_Pos_76 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_106 /= No_Token_Index then

   Row_Pos_76 := Token_Pos_106;

else
   Row_Pos_76 := No_Token_Index;
   goto Exit_Row_76_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_76_0>>
pragma Warnings (On, "referenced");

--  End row_code


if Row_Pos_76 = No_Token_Index then

        Defer_Res_112 :=
           Allocate_Expr_List (Parser.Mem_Pool);
         Initialize
           (Self              => Defer_Res_112,
            Kind              => Turkixir_Expr_List,
            Unit              => Parser.Unit,
            Token_Start_Index => Row_Pos_75 - 1,
            Token_End_Index   => No_Token_Index);
         Initialize_List
           (Self   => Defer_Res_112,
            Parser => Parser,
            Count  => 0);


    Row_Pos_76 := Row_Pos_75;


end if;

--  End opt_code



if Row_Pos_76 /= No_Token_Index then

   Row_Pos_75 := Row_Pos_76;

else
   Row_Pos_75 := No_Token_Index;
   goto Exit_Row_75_0;

end if;


--  Start tok_code

Token_Res_107 := Row_Pos_75;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_107));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_T_T__Colon)
   then
       Token_Pos_107 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_75 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_75,
             Expected_Token_Id => Turkixir_T_T__Colon,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_107 := Row_Pos_75 + 1;
   end if;
end;

--  End tok_code



if Token_Pos_107 /= No_Token_Index then

   Row_Pos_75 := Token_Pos_107;

else
   Row_Pos_75 := No_Token_Index;
   goto Exit_Row_75_0;

end if;


Defer_Res_113 :=
   Suite_Or_Parse_0 (Parser, Row_Pos_75);
Defer_Pos_113 := Parser.Current_Pos;



if Defer_Pos_113 /= No_Token_Index then

   Row_Pos_75 := Defer_Pos_113;

else
   Row_Pos_75 := No_Token_Index;
   goto Exit_Row_75_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_75_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_75 /= No_Token_Index then

   Transform_Res_63 := Allocate_Class_Def (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_63,
      Kind => Turkixir_Class_Def,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_75 = Pos
                            then No_Token_Index
                            else Row_Pos_75 - 1));

      Initialize_Fields_For_Class_Def
        (Self => Transform_Res_63, Class_Def_F_Name => Defer_Res_111, Class_Def_F_Bases => Defer_Res_112, Class_Def_F_Statements => Defer_Res_113);

         if Defer_Res_111 /= null and then Is_Incomplete (Defer_Res_111) then
            Transform_Res_63.Last_Attempted_Child := 0;
         elsif Defer_Res_111 /= null and then not Is_Ghost (Defer_Res_111) then
            Transform_Res_63.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_112 /= null and then Is_Incomplete (Defer_Res_112) then
            Transform_Res_63.Last_Attempted_Child := 0;
         elsif Defer_Res_112 /= null and then not Is_Ghost (Defer_Res_112) then
            Transform_Res_63.Last_Attempted_Child := -1;
         end if;
         if Defer_Res_113 /= null and then Is_Incomplete (Defer_Res_113) then
            Transform_Res_63.Last_Attempted_Child := 0;
         elsif Defer_Res_113 /= null and then not Is_Ghost (Defer_Res_113) then
            Transform_Res_63.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Class_Def_Transform_Parse_0_Memo,
      Row_Pos_75 /= No_Token_Index,
      Transform_Res_63,
      Pos,
      Row_Pos_75);


   Parser.Current_Pos := Row_Pos_75;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_63;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Class_Def_Transform_Parse_0;

   


function Main_Rule_Transform_Parse_0
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return Bare_File_Node
is
   use Bare_File_Node_Memos;

   Call_Depth : aliased Natural;

      Row_Pos_125 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_22 :
            Token_Index
               := No_Token_Index;
      Tmp_List_22 :
            Free_Parse_List;
      Row_Pos_126 :
            Token_Index
               := No_Token_Index;
      Lst_Cpos_23 :
            Token_Index
               := No_Token_Index;
      Tmp_List_23 :
            Free_Parse_List;
      Defer_Pos_212 :
            Token_Index
               := No_Token_Index;
      Defer_Res_212 :
            Bare_NL
               := No_Bare_Turkixir_Node;
      List_Pos_22 :
            Token_Index
               := No_Token_Index;
      List_Res_22 :
            Bare_NL_List
               := No_Bare_Turkixir_Node;
      Defer_Pos_213 :
            Token_Index
               := No_Token_Index;
      Defer_Res_213 :
            Bare_Turkixir_Node
               := No_Bare_Turkixir_Node;
      Lst_Cpos_24 :
            Token_Index
               := No_Token_Index;
      Tmp_List_24 :
            Free_Parse_List;
      Defer_Pos_214 :
            Token_Index
               := No_Token_Index;
      Defer_Res_214 :
            Bare_NL
               := No_Bare_Turkixir_Node;
      List_Pos_23 :
            Token_Index
               := No_Token_Index;
      List_Res_23 :
            Bare_NL_List
               := No_Bare_Turkixir_Node;
      List_Pos_24 :
            Token_Index
               := No_Token_Index;
      List_Res_24 :
            Bare_Turkixir_Node_List
               := No_Bare_Turkixir_Node;
      Token_Pos_182 :
            Token_Index
               := No_Token_Index;
      Token_Res_182 :
            Token_Index
               := No_Token_Index;
      Transform_Res_97 :
            Bare_File_Node
               := No_Bare_Turkixir_Node;


   M : Memo_Entry := Get (Parser.Private_Part.Main_Rule_Transform_Parse_0_Memo, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Transform_Res_97 := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_97;
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
      return Transform_Res_97;
   end if;


   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   
--  Start transform_code


--  Start row_code

Row_Pos_125 := Pos;



--  Start list_code

    List_Pos_24 := No_Token_Index;



Lst_Cpos_22 := Row_Pos_125;
Tmp_List_22 := Get_Parse_List (Parser);

loop
   
--  Start row_code

Row_Pos_126 := Lst_Cpos_22;



--  Start list_code

    List_Pos_22 := Row_Pos_126;



Lst_Cpos_23 := Row_Pos_126;
Tmp_List_23 := Get_Parse_List (Parser);

loop
   
Defer_Res_212 :=
   Nl_Transform_Parse_0 (Parser, Lst_Cpos_23);
Defer_Pos_212 := Parser.Current_Pos;


   exit when Defer_Pos_212 = No_Token_Index;

   List_Pos_22 := Defer_Pos_212;
   Lst_Cpos_23 := List_Pos_22;

   Tmp_List_23.Nodes.Append (Defer_Res_212);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_23.Nodes.Length;
begin
   List_Res_22 :=
      Allocate_NL_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_126;
      Token_End := (if Lst_Cpos_23 = Row_Pos_126
                    then Row_Pos_126
                    else Lst_Cpos_23 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_126, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_22,
      Kind              => Turkixir_NL_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_22,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_23.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_22.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_23);

--  End list_code



if List_Pos_22 /= No_Token_Index then

   Row_Pos_126 := List_Pos_22;

else
   Row_Pos_126 := No_Token_Index;
   goto Exit_Row_126_0;

end if;


Defer_Res_213 :=
   Stmt_Or_Parse_0 (Parser, Row_Pos_126);
Defer_Pos_213 := Parser.Current_Pos;



if Defer_Pos_213 /= No_Token_Index then

   Row_Pos_126 := Defer_Pos_213;

else
   Row_Pos_126 := No_Token_Index;
   goto Exit_Row_126_0;

end if;


--  Start list_code

    List_Pos_23 := Row_Pos_126;



Lst_Cpos_24 := Row_Pos_126;
Tmp_List_24 := Get_Parse_List (Parser);

loop
   
Defer_Res_214 :=
   Nl_Transform_Parse_0 (Parser, Lst_Cpos_24);
Defer_Pos_214 := Parser.Current_Pos;


   exit when Defer_Pos_214 = No_Token_Index;

   List_Pos_23 := Defer_Pos_214;
   Lst_Cpos_24 := List_Pos_23;

   Tmp_List_24.Nodes.Append (Defer_Res_214);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_24.Nodes.Length;
begin
   List_Res_23 :=
      Allocate_NL_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_126;
      Token_End := (if Lst_Cpos_24 = Row_Pos_126
                    then Row_Pos_126
                    else Lst_Cpos_24 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_126, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_23,
      Kind              => Turkixir_NL_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_23,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_24.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_23.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_24);

--  End list_code



if List_Pos_23 /= No_Token_Index then

   Row_Pos_126 := List_Pos_23;

else
   Row_Pos_126 := No_Token_Index;
   goto Exit_Row_126_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_126_0>>
pragma Warnings (On, "referenced");

--  End row_code


   exit when Row_Pos_126 = No_Token_Index;

   List_Pos_24 := Row_Pos_126;
   Lst_Cpos_22 := List_Pos_24;

   Tmp_List_22.Nodes.Append (Defer_Res_213);


end loop;

declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := Tmp_List_22.Nodes.Length;
begin
   List_Res_24 :=
      Allocate_Turkixir_Node_List (Parser.Mem_Pool);

   if Count > 0 then
      Token_Start := Row_Pos_125;
      Token_End := (if Lst_Cpos_22 = Row_Pos_125
                    then Row_Pos_125
                    else Lst_Cpos_22 - 1);

   else
      Token_Start := Token_Index'Max (Row_Pos_125, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => List_Res_24,
      Kind              => Turkixir_Turkixir_Node_List,
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => List_Res_24,
      Parser => Parser,
      Count  => Count);

   declare
      Vec : Bare_Turkixir_Node_Vectors.Vector renames
         Tmp_List_22.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         List_Res_24.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, Tmp_List_22);

--  End list_code



if List_Pos_24 /= No_Token_Index then

   Row_Pos_125 := List_Pos_24;

else
   Row_Pos_125 := No_Token_Index;
   goto Exit_Row_125_0;

end if;


--  Start tok_code

Token_Res_182 := Row_Pos_125;

declare
   T : constant Stored_Token_Data :=
      Token_Vectors.Get (Parser.TDH.Tokens, Natural (Token_Res_182));
begin
   if
      T.Kind /= From_Token_Kind (Turkixir_Termination)
   then
       Token_Pos_182 := No_Token_Index;

       if Parser.Last_Fail.Pos <= Row_Pos_125 then
          Parser.Last_Fail :=
            (Kind              => Token_Fail,
             Pos               => Row_Pos_125,
             Expected_Token_Id => Turkixir_Termination,
             Found_Token_Id    => To_Token_Kind (T.Kind));
       end if;
   else
          Token_Pos_182 := Row_Pos_125;
   end if;
end;

--  End tok_code



if Token_Pos_182 /= No_Token_Index then

   Row_Pos_125 := Token_Pos_182;

else
   Row_Pos_125 := No_Token_Index;
   goto Exit_Row_125_0;

end if;

pragma Warnings (Off, "referenced");
<<Exit_Row_125_0>>
pragma Warnings (On, "referenced");

--  End row_code



if Row_Pos_125 /= No_Token_Index then

   Transform_Res_97 := Allocate_File_Node (Parser.Mem_Pool);

   Initialize
     (Self => Transform_Res_97,
      Kind => Turkixir_File_Node,
      Unit => Parser.Unit,

      Token_Start_Index => Pos,
      Token_End_Index   => (if Row_Pos_125 = Pos
                            then No_Token_Index
                            else Row_Pos_125 - 1));

      Initialize_Fields_For_File_Node
        (Self => Transform_Res_97, File_Node_F_Statements => List_Res_24);

         if List_Res_24 /= null and then Is_Incomplete (List_Res_24) then
            Transform_Res_97.Last_Attempted_Child := 0;
         elsif List_Res_24 /= null and then not Is_Ghost (List_Res_24) then
            Transform_Res_97.Last_Attempted_Child := -1;
         end if;


end if;

--  End transform_code


   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------


   Set
     (Parser.Private_Part.Main_Rule_Transform_Parse_0_Memo,
      Row_Pos_125 /= No_Token_Index,
      Transform_Res_97,
      Pos,
      Row_Pos_125);


   Parser.Current_Pos := Row_Pos_125;

   Exit_Call (Parser, Call_Depth);
   return Transform_Res_97;

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end Main_Rule_Transform_Parse_0;


   -----------
   -- Reset --
   -----------

   procedure Reset (Parser : in out Parser_Type) is
      New_Parser : Parser_Type;
      --  We create this new parser instance to leverage creation of default
      --  values, so as to not repeat them.
   begin
      --  We just keep the private part, to not have to reallocate it
      New_Parser.Private_Part := Parser.Private_Part;

      --  And then reset everything else
      Parser := New_Parser;

      --  Reset the memo tables in the private part
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.And_Expr_Or_Parse_0_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.And_Test_Or_Parse_0_Memo);
         Bare_Arg_List_Memos.Clear
           (Parser.Private_Part.Arg_List_Extract_Parse_1_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.Arith_Expr_Or_Parse_1_Memo);
         Bare_As_Name_Node_Memos.Clear
           (Parser.Private_Part.As_Name_Transform_Parse_0_Memo);
         Bare_Assert_Stmt_Memos.Clear
           (Parser.Private_Part.Assert_Stmt_Transform_Parse_0_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.Atom_Expr_Or_Parse_0_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.Atom_Or_Parse_0_Memo);
         Bare_Break_Stmt_Memos.Clear
           (Parser.Private_Part.Break_Stmt_Transform_Parse_0_Memo);
         Bare_Concat_String_Lit_Memos.Clear
           (Parser.Private_Part.Cat_String_Transform_Parse_0_Memo);
         Bare_Class_Def_Memos.Clear
           (Parser.Private_Part.Class_Def_Transform_Parse_0_Memo);
         Bare_Comp_For_Memos.Clear
           (Parser.Private_Part.Comp_For_Transform_Parse_0_Memo);
         Bare_Comp_If_Memos.Clear
           (Parser.Private_Part.Comp_If_Transform_Parse_0_Memo);
         Bare_Turkixir_Node_Memos.Clear
           (Parser.Private_Part.Comp_Iter_Or_Parse_0_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.Comparison_Or_Parse_1_Memo);
         Bare_Stmt_Memos.Clear
           (Parser.Private_Part.Compound_Stmt_Or_Parse_0_Memo);
         Bare_Continue_Stmt_Memos.Clear
           (Parser.Private_Part.Continue_Stmt_Transform_Parse_0_Memo);
         Bare_Decorated_Memos.Clear
           (Parser.Private_Part.Decorated_Transform_Parse_0_Memo);
         Bare_Decorator_Memos.Clear
           (Parser.Private_Part.Decorator_Transform_Parse_0_Memo);
         Bare_Decorator_List_Memos.Clear
           (Parser.Private_Part.Decorators_List_Parse_0_Memo);
         Bare_Del_Stmt_Memos.Clear
           (Parser.Private_Part.Del_Stmt_Transform_Parse_0_Memo);
         Bare_Dict_Assoc_Memos.Clear
           (Parser.Private_Part.Dict_Assoc_Transform_Parse_0_Memo);
         Bare_Dot_Memos.Clear
           (Parser.Private_Part.Dot_Transform_Parse_0_Memo);
         Bare_As_Name_Node_Memos.Clear
           (Parser.Private_Part.Dotted_As_Name_Transform_Parse_0_Memo);
         Bare_Turkixir_Node_List_Memos.Clear
           (Parser.Private_Part.Dotted_As_Names_Extract_Parse_0_Memo);
         Bare_Name_Memos.Clear
           (Parser.Private_Part.Dotted_Name_Or_Parse_0_Memo);
         Bare_Else_Part_Memos.Clear
           (Parser.Private_Part.Else_Part_Transform_Parse_0_Memo);
         Bare_Expr_List_Memos.Clear
           (Parser.Private_Part.Empty_Test_List_Extract_Parse_0_Memo);
         Bare_Exec_Stmt_Memos.Clear
           (Parser.Private_Part.Exec_Stmt_Transform_Parse_0_Memo);
         Bare_Expr_List_Memos.Clear
           (Parser.Private_Part.Expr_List_Extract_Parse_0_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.Expr_Or_Parse_0_Memo);
         Bare_Turkixir_Node_Memos.Clear
           (Parser.Private_Part.Expr_Stmt_Or_Parse_3_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.Factor_Or_Parse_1_Memo);
         Bare_Turkixir_Node_Memos.Clear
           (Parser.Private_Part.Flow_Stmt_Or_Parse_0_Memo);
         Bare_For_Stmt_Memos.Clear
           (Parser.Private_Part.For_Stmt_Transform_Parse_0_Memo);
         Bare_Turkixir_Node_Memos.Clear
           (Parser.Private_Part.Fpdef_Or_Parse_0_Memo);
         Bare_Func_Def_Memos.Clear
           (Parser.Private_Part.Func_Def_Transform_Parse_0_Memo);
         Bare_Global_Stmt_Memos.Clear
           (Parser.Private_Part.Global_Stmt_Transform_Parse_0_Memo);
         Bare_If_Stmt_Memos.Clear
           (Parser.Private_Part.If_Stmt_Transform_Parse_1_Memo);
         Bare_Turkixir_Node_List_Memos.Clear
           (Parser.Private_Part.Import_As_Names_Extract_Parse_0_Memo);
         Bare_Import_From_Memos.Clear
           (Parser.Private_Part.Import_From_Transform_Parse_2_Memo);
         Bare_Import_Name_Memos.Clear
           (Parser.Private_Part.Import_Name_Transform_Parse_0_Memo);
         Bare_Stmt_Memos.Clear
           (Parser.Private_Part.Import_Stmt_Or_Parse_0_Memo);
         Bare_Lambda_Def_Memos.Clear
           (Parser.Private_Part.Lambdef_Transform_Parse_0_Memo);
         Bare_Comp_ForL_Memos.Clear
           (Parser.Private_Part.List_For_Transform_Parse_0_Memo);
         Bare_Comp_If_Memos.Clear
           (Parser.Private_Part.List_If_Transform_Parse_0_Memo);
         Bare_Turkixir_Node_Memos.Clear
           (Parser.Private_Part.List_Iter_Or_Parse_0_Memo);
         Bare_File_Node_Memos.Clear
           (Parser.Private_Part.Main_Rule_Transform_Parse_0_Memo);
         Bare_Id_List_Memos.Clear
           (Parser.Private_Part.Name_List_Extract_Parse_0_Memo);
         Bare_Id_Memos.Clear
           (Parser.Private_Part.Name_Transform_Parse_0_Memo);
         Bare_NL_Memos.Clear
           (Parser.Private_Part.Nl_Transform_Parse_0_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.Not_Test_Or_Parse_0_Memo);
         Bare_Number_Lit_Memos.Clear
           (Parser.Private_Part.Number_Transform_Parse_0_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.Or_Test_Or_Parse_0_Memo);
         Bare_Params_Memos.Clear
           (Parser.Private_Part.Parameters_Extract_Parse_0_Memo);
         Bare_Pass_Stmt_Memos.Clear
           (Parser.Private_Part.Pass_Stmt_Transform_Parse_0_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.Power_Or_Parse_0_Memo);
         Bare_Stmt_Memos.Clear
           (Parser.Private_Part.Print_Stmt_Or_Parse_0_Memo);
         Bare_Raise_Stmt_Memos.Clear
           (Parser.Private_Part.Raise_Stmt_Transform_Parse_0_Memo);
         Bare_Return_Stmt_Memos.Clear
           (Parser.Private_Part.Return_Stmt_Transform_Parse_0_Memo);
         Bare_Set_Lit_Memos.Clear
           (Parser.Private_Part.Set_Lit_Transform_Parse_0_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.Shift_Expr_Or_Parse_1_Memo);
         Bare_Turkixir_Node_Memos.Clear
           (Parser.Private_Part.Simple_Stmt_Extract_Parse_1_Memo);
         Bare_Turkixir_Node_Memos.Clear
           (Parser.Private_Part.Small_Stmt_Or_Parse_0_Memo);
         Bare_Turkixir_Node_Memos.Clear
           (Parser.Private_Part.Stmt_Or_Parse_0_Memo);
         Bare_String_Lit_Memos.Clear
           (Parser.Private_Part.String_Transform_Parse_0_Memo);
         Bare_Expr_List_Memos.Clear
           (Parser.Private_Part.Subscript_List_Extract_Parse_0_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.Subscript_Or_Parse_0_Memo);
         Bare_Turkixir_Node_Memos.Clear
           (Parser.Private_Part.Suite_Or_Parse_0_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.Term_Or_Parse_1_Memo);
         Bare_Expr_List_Memos.Clear
           (Parser.Private_Part.Test_List_Extract_Parse_0_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.Test_Or_Parse_0_Memo);
         Bare_Try_Stmt_Memos.Clear
           (Parser.Private_Part.Try_Stmt_Transform_Parse_2_Memo);
         Bare_Params_Memos.Clear
           (Parser.Private_Part.Varargslist_Transform_Parse_1_Memo);
         Bare_While_Stmt_Memos.Clear
           (Parser.Private_Part.While_Stmt_Transform_Parse_0_Memo);
         Bare_As_Name_Node_Memos.Clear
           (Parser.Private_Part.With_Item_Transform_Parse_0_Memo);
         Bare_With_Stmt_Memos.Clear
           (Parser.Private_Part.With_Stmt_Transform_Parse_0_Memo);
         Bare_Expr_Memos.Clear
           (Parser.Private_Part.Xor_Expr_Or_Parse_0_Memo);
         Bare_Yield_Expr_Memos.Clear
           (Parser.Private_Part.Yield_Expr_Transform_Parse_0_Memo);
         Bare_Yield_Expr_Memos.Clear
           (Parser.Private_Part.Yield_Stmt_Defer_Parse_0_Memo);
   end Reset;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Parser : in out Parser_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Parser_Private_Part_Type, Parser_Private_Part);
      procedure Free is new Ada.Unchecked_Deallocation
        (Free_Parse_List_Record, Free_Parse_List);

      Cur : Free_Parse_List renames Parser.Private_Part.Parse_Lists;
   begin
      while Cur /= null loop
         declare
            Next : constant Free_Parse_List := Cur.Next;
         begin
            Cur.Nodes.Destroy;
            Free (Cur);
            Cur := Next;
         end;
      end loop;
      Free (Parser.Private_Part);
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Parser : in out Parser_Type) is
   begin
      Parser.Private_Part := new Parser_Private_Part_Type'(others => <>);
   end Initialize;

   --------------------
   -- Get_Parse_List --
   --------------------

   function Get_Parse_List (Parser : Parser_Type) return Free_Parse_List is
      Lists  : Free_Parse_List renames Parser.Private_Part.Parse_Lists;
      Result : Free_Parse_List;
   begin
      if Lists = null then
         Result := new Free_Parse_List_Record;

      else
         Result := Lists;
         Lists := Lists.Next;
      end if;

      return Result;
   end Get_Parse_List;

   ------------------------
   -- Release_Parse_List --
   ------------------------

   procedure Release_Parse_List
     (Parser : Parser_Type; List : in out Free_Parse_List)
   is
      Lists  : Free_Parse_List renames Parser.Private_Part.Parse_Lists;
   begin
      List.Nodes.Clear;
      List.Next := Lists;
      Lists := List;
      List := null;
   end Release_Parse_List;

   ----------------
   -- Enter_Call --
   ----------------

   procedure Enter_Call (Parser : Parser_Type; Call_Depth : access Natural) is
   begin
      Enter_Call (Parser.Unit.Context, Call_Depth);
   end Enter_Call;

   ---------------
   -- Exit_Call --
   ---------------

   procedure Exit_Call (Parser : Parser_Type; Call_Depth : Natural) is
   begin
      Exit_Call (Parser.Unit.Context, Call_Depth);
   end Exit_Call;

end Libturkixirlang.Parsers;
