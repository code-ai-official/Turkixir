


package body Libturkixirlang.Introspection_Implementation is

   ----------------
   -- As_Boolean --
   ----------------

   function As_Boolean (Self : Internal_Value) return Boolean is
   begin
      return Self.Boolean_Value;
   end As_Boolean;

   ----------------
   -- As_Integer --
   ----------------

   function As_Integer (Self : Internal_Value) return Integer is
   begin
      return Self.Integer_Value;
   end As_Integer;

   ------------------
   -- As_Character --
   ------------------

   function As_Character (Self : Internal_Value) return Character_Type is
   begin
      return Self.Character_Value;
   end As_Character;

   ---------------
   -- As_String --
   ---------------

   function As_String (Self : Internal_Value) return String_Type is
   begin
      return Self.String_Value;
   end As_String;

   -------------
   -- As_Node --
   -------------

   function As_Node (Self : Internal_Value) return Internal_Entity is
   begin
      return Self.Node_Value;
   end As_Node;

      function As_Analysis_Unit_Kind
        (Self : Internal_Value) return Analysis_Unit_Kind is
      begin
         return Self.Analysis_Unit_Kind_Value;
      end As_Analysis_Unit_Kind;

      function As_Lookup_Kind
        (Self : Internal_Value) return Lookup_Kind is
      begin
         return Self.Lookup_Kind_Value;
      end As_Lookup_Kind;

      function As_Designated_Env_Kind
        (Self : Internal_Value) return Designated_Env_Kind is
      begin
         return Self.Designated_Env_Kind_Value;
      end As_Designated_Env_Kind;

      function As_Grammar_Rule
        (Self : Internal_Value) return Grammar_Rule is
      begin
         return Self.Grammar_Rule_Value;
      end As_Grammar_Rule;


   --  Now we can emit descriptor tables

   ----------------------
   -- Struct_Type_Desc --
   ----------------------

   function Struct_Type_Desc
     (Kind : Struct_Value_Kind) return Struct_Type_Descriptor_Access
   is
   begin
         pragma Unreferenced (Kind);
         return (raise Program_Error);
   end Struct_Type_Desc;

   -----------------------
   -- Struct_Field_Name --
   -----------------------

   function Struct_Field_Name (Field : Struct_Field_Reference) return Text_Type
   is
   begin
      pragma Warnings (Off, "value not in range of subtype");
      return To_Text (Struct_Field_Descriptors (Field).Name);
      pragma Warnings (On, "value not in range of subtype");
   end Struct_Field_Name;

   -----------------------
   -- Struct_Field_Type --
   -----------------------

   function Struct_Field_Type
     (Field : Struct_Field_Reference) return Type_Constraint is
   begin
      pragma Warnings (Off, "value not in range of subtype");
      return Struct_Field_Descriptors (Field).Field_Type;
      pragma Warnings (On, "value not in range of subtype");
   end Struct_Field_Type;

   -------------------
   -- Struct_Fields --
   -------------------

   pragma Warnings (Off, "referenced");
   function Struct_Fields
     (Kind : Struct_Value_Kind) return Struct_Field_Reference_Array
   is
      pragma Warnings (On, "referenced");
   begin
         return (raise Program_Error);
   end Struct_Fields;

   --------------
   -- DSL_Name --
   --------------

   function DSL_Name (Id : Node_Type_Id) return Text_Type is
   begin
      return To_Text (To_String (Node_Type_Descriptors (Id).DSL_Name));
   end DSL_Name;

   ---------------------
   -- Lookup_DSL_Name --
   ---------------------

   function Lookup_DSL_Name (Name : Text_Type) return Any_Node_Type_Id is
      use Node_Type_Id_Maps;

      Position : constant Cursor :=
         DSL_Name_To_Node_Type.Find (To_Unbounded_String (Image (Name)));
   begin
      if Has_Element (Position) then
         return Element (Position);
      else
         return None;
      end if;
   end Lookup_DSL_Name;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (Id : Node_Type_Id) return Boolean is
   begin
      return Node_Type_Descriptors (Id).Is_Abstract;
   end Is_Abstract;

   --------------
   -- Kind_For --
   --------------

   function Kind_For (Id : Node_Type_Id) return Turkixir_Node_Kind_Type is
      Desc : Node_Type_Descriptor renames Node_Type_Descriptors (Id).all;
   begin
      if Desc.Is_Abstract then
         raise Bad_Type_Error with "trying to get kind for abstract node";
      end if;
      return Desc.Kind;
   end Kind_For;

   --------------------
   -- First_Kind_For --
   --------------------

   function First_Kind_For (Id : Node_Type_Id) return Turkixir_Node_Kind_Type is

      --  Look for the leftmost leaf derivation of an abstract node. Langkit
      --  disallows abstract nodes with no concrete derivation, so each time we
      --  see an an abstract node, we know there are concrete derivations down
      --  the tree.
      --
      --  Note that we have to stop at the first concrete node we see because
      --  of the way we sort kinds: the kind of concrete root comes before the
      --  kinds of all its derivations.

      Cur : Node_Type_Id := Id;
   begin
      loop
         declare
            Desc : Node_Type_Descriptor renames
               Node_Type_Descriptors (Cur).all;
         begin
            exit when not Desc.Is_Abstract or else Desc.Derivations'Length = 0;
            Cur := Desc.Derivations (Desc.Derivations'First);
         end;
      end loop;
      return Kind_For (Cur);
   end First_Kind_For;

   -------------------
   -- Last_Kind_For --
   -------------------

   function Last_Kind_For (Id : Node_Type_Id) return Turkixir_Node_Kind_Type is

      --  Look for the rightmost leaf derivation. Langkit disallows abstract
      --  nodes with no concrete derivation, so we know that the result is
      --  concrete.

      Cur : Node_Type_Id := Id;
   begin
      loop
         declare
            Desc : Node_Type_Descriptor renames
               Node_Type_Descriptors (Cur).all;
         begin
            exit when Desc.Derivations'Length = 0;
            Cur := Desc.Derivations (Desc.Derivations'Last);
         end;
      end loop;
      return Kind_For (Cur);
   end Last_Kind_For;

   -----------------
   -- Id_For_Kind --
   -----------------

   function Id_For_Kind (Kind : Turkixir_Node_Kind_Type) return Node_Type_Id is
   begin
      return Kind_To_Id (Kind);
   end Id_For_Kind;

   ------------------
   -- Is_Root_Node --
   ------------------

   function Is_Root_Node (Id : Node_Type_Id) return Boolean is
   begin
      return Id = Common.Turkixir_Node_Type_Id;
   end Is_Root_Node;

   ---------------
   -- Base_Type --
   ---------------

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id is
   begin
      if Is_Root_Node (Id) then
         raise Bad_Type_Error with "trying to get base type of root node";
      end if;
      return Node_Type_Descriptors (Id).Base_Type;
   end Base_Type;

   -------------------
   -- Derived_Types --
   -------------------

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array is
   begin
      return Node_Type_Descriptors (Id).Derivations;
   end Derived_Types;

   ---------------------
   -- Is_Derived_From --
   ---------------------

   function Is_Derived_From (Id, Parent : Node_Type_Id) return Boolean is
      Cursor : Any_Node_Type_Id := Id;
   begin
      while Cursor /= None loop
         if Cursor = Parent then
            return True;
         end if;

         Cursor := Node_Type_Descriptors (Cursor).Base_Type;
      end loop;
      return False;
   end Is_Derived_From;

   -----------------
   -- Member_Name --
   -----------------

   function Member_Name (Member : Member_Reference) return Text_Type is
   begin
      case Member is
         when Struct_Field_Reference =>
            pragma Warnings (Off, "value not in range of type");
            return Struct_Field_Name (Member);
            pragma Warnings (On, "value not in range of type");

         when Syntax_Field_Reference =>
            pragma Warnings (Off, "value not in range of type");
            return Syntax_Field_Name (Member);
            pragma Warnings (On, "value not in range of type");

         when Property_Reference =>
            return Property_Name (Member);
      end case;
   end Member_Name;

   -----------------
   -- Member_Type --
   -----------------

   function Member_Type (Member : Member_Reference) return Type_Constraint is
   begin
      case Member is
         when Struct_Field_Reference =>
            pragma Warnings (Off, "value not in range of type");
            return Struct_Field_Type (Member);
            pragma Warnings (On, "value not in range of type");

         when Syntax_Field_Reference =>
            pragma Warnings (Off, "value not in range of type");
            return (Kind      => Node_Value,
                    Node_Type => Syntax_Field_Type (Member));
            pragma Warnings (On, "value not in range of type");

         when Property_Reference =>
            return Property_Return_Type (Member);
      end case;
   end Member_Type;

   --------------------------
   -- Lookup_Member_Struct --
   --------------------------

   function Lookup_Member_Struct
     (Kind : Struct_Value_Kind;
      Name : Text_Type) return Any_Member_Reference
   is
      pragma Warnings (Off, "value not in range of type");
      Desc : Struct_Type_Descriptor renames Struct_Type_Desc (Kind).all;
      pragma Warnings (On, "value not in range of type");
   begin
      for F of Desc.Fields loop
         if To_Text (F.Name) = Name then
            return F.Reference;
         end if;
      end loop;

      return None;
   end Lookup_Member_Struct;

   ------------------------
   -- Lookup_Member_Node --
   ------------------------

   function Lookup_Member_Node
     (Id   : Node_Type_Id;
      Name : Text_Type) return Any_Member_Reference
   is
      Cursor : Any_Node_Type_Id := Id;
   begin
      --  Go through the derivation chain for Id and look for any field or
      --  property whose name matches Name.

      while Cursor /= None loop
         declare
            Node_Desc : Node_Type_Descriptor renames
               Node_Type_Descriptors (Cursor).all;
         begin
            for F of Node_Desc.Fields loop
               pragma Warnings (Off, "value not in range of type");
               if Syntax_Field_Name (F.Field) = Name then
                  return F.Field;
               end if;
               pragma Warnings (On, "value not in range of type");
            end loop;

            for P of Node_Desc.Properties loop
               if Property_Name (P) = Name then
                  return P;
               end if;
            end loop;

            Cursor := Node_Desc.Base_Type;
         end;
      end loop;
      return None;
   end Lookup_Member_Node;

   -----------------------
   -- Syntax_Field_Name --
   -----------------------

   function Syntax_Field_Name (Field : Syntax_Field_Reference) return Text_Type
   is
   begin
      pragma Warnings (Off, "value not in range of subtype");
      return To_Text (Syntax_Field_Descriptors (Field).Name);
      pragma Warnings (On, "value not in range of subtype");
   end Syntax_Field_Name;

   -----------------------
   -- Syntax_Field_Type --
   -----------------------

   function Syntax_Field_Type
     (Field : Syntax_Field_Reference) return Node_Type_Id is
   begin
      pragma Warnings (Off, "value not in range of subtype");
      return Syntax_Field_Descriptors (Field).Field_Type;
      pragma Warnings (On, "value not in range of subtype");
   end Syntax_Field_Type;

   -----------------------
   -- Eval_Syntax_Field --
   -----------------------

   function Eval_Syntax_Field
     (Node  : Bare_Turkixir_Node;
      Field : Syntax_Field_Reference) return Bare_Turkixir_Node
   is
      Kind : constant Turkixir_Node_Kind_Type := Node.Kind;
   begin
      
      case Turkixir_Turkixir_Node (Kind) is
when Turkixir_Arg_Assoc_Range =>
declare
N_Bare_Arg_Assoc : constant Bare_Arg_Assoc := Node;
begin
case Field is
when Arg_Assoc_F_Name => return Arg_Assoc_F_Name (N_Bare_Arg_Assoc);
when Arg_Assoc_F_Expr => return Arg_Assoc_F_Expr (N_Bare_Arg_Assoc);
when others => null;
end case;
end;
when Turkixir_Arg_Gen_Range =>
declare
N_Bare_Arg_Gen : constant Bare_Arg_Gen := Node;
begin
case Field is
when Arg_Gen_F_Expr => return Arg_Gen_F_Expr (N_Bare_Arg_Gen);
when Arg_Gen_F_Comprehension => return Arg_Gen_F_Comprehension (N_Bare_Arg_Gen);
when others => null;
end case;
end;
when Turkixir_Kw_Args_Range =>
declare
N_Bare_Kw_Args : constant Bare_Kw_Args := Node;
begin
case Field is
when Kw_Args_F_Expr => return Kw_Args_F_Expr (N_Bare_Kw_Args);
when others => null;
end case;
end;
when Turkixir_Var_Args_Range =>
declare
N_Bare_Var_Args : constant Bare_Var_Args := Node;
begin
case Field is
when Var_Args_F_Expr => return Var_Args_F_Expr (N_Bare_Var_Args);
when others => null;
end case;
end;
when Turkixir_As_Name_Node_Range =>
declare
N_Bare_As_Name_Node : constant Bare_As_Name_Node := Node;
begin
case Field is
when As_Name_Node_F_Imported => return As_Name_Node_F_Imported (N_Bare_As_Name_Node);
when As_Name_Node_F_As_Name => return As_Name_Node_F_As_Name (N_Bare_As_Name_Node);
when others => null;
end case;
end;
when Turkixir_Comp_If_Range =>
declare
N_Bare_Comp_If : constant Bare_Comp_If := Node;
begin
case Field is
when Comp_If_F_Test => return Comp_If_F_Test (N_Bare_Comp_If);
when Comp_If_F_Comp => return Comp_If_F_Comp (N_Bare_Comp_If);
when others => null;
end case;
end;
when Turkixir_Comp_For_Range =>
declare
N_Bare_Comp_For : constant Bare_Comp_For := Node;
begin
case Field is
when Comp_For_F_Exprs => return Comp_For_F_Exprs (N_Bare_Comp_For);
when Comp_For_F_Target => return Comp_For_F_Target (N_Bare_Comp_For);
when Comp_For_F_Comp => return Comp_For_F_Comp (N_Bare_Comp_For);
when others => null;
end case;
end;
when Turkixir_Comp_ForL_Range =>
declare
N_Bare_Comp_ForL : constant Bare_Comp_ForL := Node;
begin
case Field is
when Comp_ForL_F_Exprs => return Comp_ForL_F_Exprs (N_Bare_Comp_ForL);
when Comp_ForL_F_Target => return Comp_ForL_F_Target (N_Bare_Comp_ForL);
when Comp_ForL_F_Comp => return Comp_ForL_F_Comp (N_Bare_Comp_ForL);
when others => null;
end case;
end;
when Turkixir_Decorator_Range =>
declare
N_Bare_Decorator : constant Bare_Decorator := Node;
begin
case Field is
when Decorator_F_Dec_Name => return Decorator_F_Dec_Name (N_Bare_Decorator);
when Decorator_F_Arg_List => return Decorator_F_Arg_List (N_Bare_Decorator);
when others => null;
end case;
end;
when Turkixir_Dict_Assoc_Range =>
declare
N_Bare_Dict_Assoc : constant Bare_Dict_Assoc := Node;
begin
case Field is
when Dict_Assoc_F_Key => return Dict_Assoc_F_Key (N_Bare_Dict_Assoc);
when Dict_Assoc_F_Value => return Dict_Assoc_F_Value (N_Bare_Dict_Assoc);
when others => null;
end case;
end;
when Turkixir_Else_Part_Range =>
declare
N_Bare_Else_Part : constant Bare_Else_Part := Node;
begin
case Field is
when Else_Part_F_Statements => return Else_Part_F_Statements (N_Bare_Else_Part);
when others => null;
end case;
end;
when Turkixir_Except_Part_Range =>
declare
N_Bare_Except_Part : constant Bare_Except_Part := Node;
begin
case Field is
when Except_Part_F_As_Name => return Except_Part_F_As_Name (N_Bare_Except_Part);
when Except_Part_F_Statements => return Except_Part_F_Statements (N_Bare_Except_Part);
when others => null;
end case;
end;
when Turkixir_And_Expr_Range =>
declare
N_Bare_And_Expr : constant Bare_And_Expr := Node;
begin
case Field is
when And_Expr_F_Left => return And_Expr_F_Left (N_Bare_And_Expr);
when And_Expr_F_Right => return And_Expr_F_Right (N_Bare_And_Expr);
when others => null;
end case;
end;
when Turkixir_And_Op_Range =>
declare
N_Bare_And_Op : constant Bare_And_Op := Node;
begin
case Field is
when And_Op_F_Left => return And_Op_F_Left (N_Bare_And_Op);
when And_Op_F_Right => return And_Op_F_Right (N_Bare_And_Op);
when others => null;
end case;
end;
when Turkixir_Bin_Op =>
declare
N_Bare_Bin_Op : constant Bare_Bin_Op := Node;
begin
case Field is
when Bin_Op_F_Left => return Bin_Op_F_Left (N_Bare_Bin_Op);
when Bin_Op_F_Op => return Bin_Op_F_Op (N_Bare_Bin_Op);
when Bin_Op_F_Right => return Bin_Op_F_Right (N_Bare_Bin_Op);
when others => null;
end case;
end;
when Turkixir_Call_Expr_Range =>
declare
N_Bare_Call_Expr : constant Bare_Call_Expr := Node;
begin
case Field is
when Call_Expr_F_Prefix => return Call_Expr_F_Prefix (N_Bare_Call_Expr);
when Call_Expr_F_Suffix => return Call_Expr_F_Suffix (N_Bare_Call_Expr);
when others => null;
end case;
end;
when Turkixir_Comp_Op_Range =>
declare
N_Bare_Comp_Op : constant Bare_Comp_Op := Node;
begin
case Field is
when Comp_Op_F_Left => return Comp_Op_F_Left (N_Bare_Comp_Op);
when Comp_Op_F_Op => return Comp_Op_F_Op (N_Bare_Comp_Op);
when Comp_Op_F_Right => return Comp_Op_F_Right (N_Bare_Comp_Op);
when others => null;
end case;
end;
when Turkixir_Concat_String_Lit_Range =>
declare
N_Bare_Concat_String_Lit : constant Bare_Concat_String_Lit := Node;
begin
case Field is
when Concat_String_Lit_F_First_Str => return Concat_String_Lit_F_First_Str (N_Bare_Concat_String_Lit);
when Concat_String_Lit_F_Subsequent_Str => return Concat_String_Lit_F_Subsequent_Str (N_Bare_Concat_String_Lit);
when others => null;
end case;
end;
when Turkixir_Dict_Comp_Range =>
declare
N_Bare_Dict_Comp : constant Bare_Dict_Comp := Node;
begin
case Field is
when Dict_Comp_F_Assoc => return Dict_Comp_F_Assoc (N_Bare_Dict_Comp);
when Dict_Comp_F_Comprehension => return Dict_Comp_F_Comprehension (N_Bare_Dict_Comp);
when others => null;
end case;
end;
when Turkixir_Dict_Lit_Range =>
declare
N_Bare_Dict_Lit : constant Bare_Dict_Lit := Node;
begin
case Field is
when Dict_Lit_F_Assocs => return Dict_Lit_F_Assocs (N_Bare_Dict_Lit);
when others => null;
end case;
end;
when Turkixir_Factor_Range =>
declare
N_Bare_Factor : constant Bare_Factor := Node;
begin
case Field is
when Factor_F_Op => return Factor_F_Op (N_Bare_Factor);
when Factor_F_Expr => return Factor_F_Expr (N_Bare_Factor);
when others => null;
end case;
end;
when Turkixir_If_Expr_Range =>
declare
N_Bare_If_Expr : constant Bare_If_Expr := Node;
begin
case Field is
when If_Expr_F_Expr => return If_Expr_F_Expr (N_Bare_If_Expr);
when If_Expr_F_Cond => return If_Expr_F_Cond (N_Bare_If_Expr);
when If_Expr_F_Else_Expr => return If_Expr_F_Else_Expr (N_Bare_If_Expr);
when others => null;
end case;
end;
when Turkixir_Inline_Eval_Range =>
declare
N_Bare_Inline_Eval : constant Bare_Inline_Eval := Node;
begin
case Field is
when Inline_Eval_F_Exprs => return Inline_Eval_F_Exprs (N_Bare_Inline_Eval);
when others => null;
end case;
end;
when Turkixir_Lambda_Def_Range =>
declare
N_Bare_Lambda_Def : constant Bare_Lambda_Def := Node;
begin
case Field is
when Lambda_Def_F_Args => return Lambda_Def_F_Args (N_Bare_Lambda_Def);
when Lambda_Def_F_Expr => return Lambda_Def_F_Expr (N_Bare_Lambda_Def);
when others => null;
end case;
end;
when Turkixir_List_Comp_Range =>
declare
N_Bare_List_Comp : constant Bare_List_Comp := Node;
begin
case Field is
when List_Comp_F_Expr => return List_Comp_F_Expr (N_Bare_List_Comp);
when List_Comp_F_Comprehension => return List_Comp_F_Comprehension (N_Bare_List_Comp);
when others => null;
end case;
end;
when Turkixir_List_Gen_Range =>
declare
N_Bare_List_Gen : constant Bare_List_Gen := Node;
begin
case Field is
when List_Gen_F_Expr => return List_Gen_F_Expr (N_Bare_List_Gen);
when List_Gen_F_Comprehension => return List_Gen_F_Comprehension (N_Bare_List_Gen);
when others => null;
end case;
end;
when Turkixir_List_Lit_Range =>
declare
N_Bare_List_Lit : constant Bare_List_Lit := Node;
begin
case Field is
when List_Lit_F_Exprs => return List_Lit_F_Exprs (N_Bare_List_Lit);
when others => null;
end case;
end;
when Turkixir_Dotted_Name_Range =>
declare
N_Bare_Dotted_Name : constant Bare_Dotted_Name := Node;
begin
case Field is
when Dotted_Name_F_Prefix => return Dotted_Name_F_Prefix (N_Bare_Dotted_Name);
when Dotted_Name_F_Suffix => return Dotted_Name_F_Suffix (N_Bare_Dotted_Name);
when others => null;
end case;
end;
when Turkixir_Not_Op_Range =>
declare
N_Bare_Not_Op : constant Bare_Not_Op := Node;
begin
case Field is
when Not_Op_F_Expr => return Not_Op_F_Expr (N_Bare_Not_Op);
when others => null;
end case;
end;
when Turkixir_Or_Expr_Range =>
declare
N_Bare_Or_Expr : constant Bare_Or_Expr := Node;
begin
case Field is
when Or_Expr_F_Left => return Or_Expr_F_Left (N_Bare_Or_Expr);
when Or_Expr_F_Right => return Or_Expr_F_Right (N_Bare_Or_Expr);
when others => null;
end case;
end;
when Turkixir_Or_Op_Range =>
declare
N_Bare_Or_Op : constant Bare_Or_Op := Node;
begin
case Field is
when Or_Op_F_Left => return Or_Op_F_Left (N_Bare_Or_Op);
when Or_Op_F_Right => return Or_Op_F_Right (N_Bare_Or_Op);
when others => null;
end case;
end;
when Turkixir_Power_Range =>
declare
N_Bare_Power : constant Bare_Power := Node;
begin
case Field is
when Power_F_Left => return Power_F_Left (N_Bare_Power);
when Power_F_Right => return Power_F_Right (N_Bare_Power);
when others => null;
end case;
end;
when Turkixir_Set_Comp_Range =>
declare
N_Bare_Set_Comp : constant Bare_Set_Comp := Node;
begin
case Field is
when Set_Comp_F_Expr => return Set_Comp_F_Expr (N_Bare_Set_Comp);
when Set_Comp_F_Comprehension => return Set_Comp_F_Comprehension (N_Bare_Set_Comp);
when others => null;
end case;
end;
when Turkixir_Set_Lit_Range =>
declare
N_Bare_Set_Lit : constant Bare_Set_Lit := Node;
begin
case Field is
when Set_Lit_F_Exprs => return Set_Lit_F_Exprs (N_Bare_Set_Lit);
when others => null;
end case;
end;
when Turkixir_Slice_Expr_Range =>
declare
N_Bare_Slice_Expr : constant Bare_Slice_Expr := Node;
begin
case Field is
when Slice_Expr_F_First => return Slice_Expr_F_First (N_Bare_Slice_Expr);
when Slice_Expr_F_Last => return Slice_Expr_F_Last (N_Bare_Slice_Expr);
when others => null;
end case;
case Turkixir_Slice_Expr_Range (Kind) is
when Turkixir_Ext_Slice_Expr_Range =>
declare
N_Bare_Ext_Slice_Expr : constant Bare_Ext_Slice_Expr := N_Bare_Slice_Expr;
begin
case Field is
when Ext_Slice_Expr_F_Stride => return Ext_Slice_Expr_F_Stride (N_Bare_Ext_Slice_Expr);
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
case Field is
when Subscript_Expr_F_Prefix => return Subscript_Expr_F_Prefix (N_Bare_Subscript_Expr);
when Subscript_Expr_F_Suffix => return Subscript_Expr_F_Suffix (N_Bare_Subscript_Expr);
when others => null;
end case;
end;
when Turkixir_Tuple_Lit_Range =>
declare
N_Bare_Tuple_Lit : constant Bare_Tuple_Lit := Node;
begin
case Field is
when Tuple_Lit_F_Exprs => return Tuple_Lit_F_Exprs (N_Bare_Tuple_Lit);
when others => null;
end case;
end;
when Turkixir_Xor_Expr_Range =>
declare
N_Bare_Xor_Expr : constant Bare_Xor_Expr := Node;
begin
case Field is
when Xor_Expr_F_Left => return Xor_Expr_F_Left (N_Bare_Xor_Expr);
when Xor_Expr_F_Right => return Xor_Expr_F_Right (N_Bare_Xor_Expr);
when others => null;
end case;
end;
when Turkixir_Yield_Expr_Range =>
declare
N_Bare_Yield_Expr : constant Bare_Yield_Expr := Node;
begin
case Field is
when Yield_Expr_F_Exprs => return Yield_Expr_F_Exprs (N_Bare_Yield_Expr);
when others => null;
end case;
end;
when Turkixir_File_Node_Range =>
declare
N_Bare_File_Node : constant Bare_File_Node := Node;
begin
case Field is
when File_Node_F_Statements => return File_Node_F_Statements (N_Bare_File_Node);
when others => null;
end case;
end;
when Turkixir_Params_Range =>
declare
N_Bare_Params : constant Bare_Params := Node;
begin
case Field is
when Params_F_Single_Params => return Params_F_Single_Params (N_Bare_Params);
when others => null;
end case;
end;
when Turkixir_Rel_Name_Range =>
declare
N_Bare_Rel_Name : constant Bare_Rel_Name := Node;
begin
case Field is
when Rel_Name_F_Dots => return Rel_Name_F_Dots (N_Bare_Rel_Name);
when Rel_Name_F_Name => return Rel_Name_F_Name (N_Bare_Rel_Name);
when others => null;
end case;
end;
when Turkixir_Single_Param_Range =>
declare
N_Bare_Single_Param : constant Bare_Single_Param := Node;
begin
case Field is
when Single_Param_F_Is_Varargs => return Single_Param_F_Is_Varargs (N_Bare_Single_Param);
when Single_Param_F_Is_Kwargs => return Single_Param_F_Is_Kwargs (N_Bare_Single_Param);
when Single_Param_F_Name => return Single_Param_F_Name (N_Bare_Single_Param);
when Single_Param_F_Default_Value => return Single_Param_F_Default_Value (N_Bare_Single_Param);
when others => null;
end case;
end;
when Turkixir_Assert_Stmt_Range =>
declare
N_Bare_Assert_Stmt : constant Bare_Assert_Stmt := Node;
begin
case Field is
when Assert_Stmt_F_Test_Expr => return Assert_Stmt_F_Test_Expr (N_Bare_Assert_Stmt);
when Assert_Stmt_F_Msg => return Assert_Stmt_F_Msg (N_Bare_Assert_Stmt);
when others => null;
end case;
end;
when Turkixir_Assign_Stmt_Range =>
declare
N_Bare_Assign_Stmt : constant Bare_Assign_Stmt := Node;
begin
case Field is
when Assign_Stmt_F_L_Value => return Assign_Stmt_F_L_Value (N_Bare_Assign_Stmt);
when Assign_Stmt_F_R_Values => return Assign_Stmt_F_R_Values (N_Bare_Assign_Stmt);
when others => null;
end case;
end;
when Turkixir_Aug_Assign_Stmt_Range =>
declare
N_Bare_Aug_Assign_Stmt : constant Bare_Aug_Assign_Stmt := Node;
begin
case Field is
when Aug_Assign_Stmt_F_L_Value => return Aug_Assign_Stmt_F_L_Value (N_Bare_Aug_Assign_Stmt);
when Aug_Assign_Stmt_F_Op => return Aug_Assign_Stmt_F_Op (N_Bare_Aug_Assign_Stmt);
when Aug_Assign_Stmt_F_R_Value => return Aug_Assign_Stmt_F_R_Value (N_Bare_Aug_Assign_Stmt);
when others => null;
end case;
end;
when Turkixir_Decorated_Range =>
declare
N_Bare_Decorated : constant Bare_Decorated := Node;
begin
case Field is
when Decorated_F_Decorators => return Decorated_F_Decorators (N_Bare_Decorated);
when Decorated_F_Defn => return Decorated_F_Defn (N_Bare_Decorated);
when others => null;
end case;
end;
when Turkixir_Class_Def_Range =>
declare
N_Bare_Class_Def : constant Bare_Class_Def := Node;
begin
case Field is
when Class_Def_F_Name => return Class_Def_F_Name (N_Bare_Class_Def);
when Class_Def_F_Bases => return Class_Def_F_Bases (N_Bare_Class_Def);
when Class_Def_F_Statements => return Class_Def_F_Statements (N_Bare_Class_Def);
when others => null;
end case;
end;
when Turkixir_Func_Def_Range =>
declare
N_Bare_Func_Def : constant Bare_Func_Def := Node;
begin
case Field is
when Func_Def_F_Name => return Func_Def_F_Name (N_Bare_Func_Def);
when Func_Def_F_Parameters => return Func_Def_F_Parameters (N_Bare_Func_Def);
when Func_Def_F_Body => return Func_Def_F_Body (N_Bare_Func_Def);
when others => null;
end case;
end;
when Turkixir_Del_Stmt_Range =>
declare
N_Bare_Del_Stmt : constant Bare_Del_Stmt := Node;
begin
case Field is
when Del_Stmt_F_Exprs => return Del_Stmt_F_Exprs (N_Bare_Del_Stmt);
when others => null;
end case;
end;
when Turkixir_Elif_Branch_Range =>
declare
N_Bare_Elif_Branch : constant Bare_Elif_Branch := Node;
begin
case Field is
when Elif_Branch_F_Cond_Test => return Elif_Branch_F_Cond_Test (N_Bare_Elif_Branch);
when Elif_Branch_F_Statements => return Elif_Branch_F_Statements (N_Bare_Elif_Branch);
when others => null;
end case;
end;
when Turkixir_Exec_Stmt_Range =>
declare
N_Bare_Exec_Stmt : constant Bare_Exec_Stmt := Node;
begin
case Field is
when Exec_Stmt_F_Expr => return Exec_Stmt_F_Expr (N_Bare_Exec_Stmt);
when Exec_Stmt_F_In_List => return Exec_Stmt_F_In_List (N_Bare_Exec_Stmt);
when others => null;
end case;
end;
when Turkixir_For_Stmt_Range =>
declare
N_Bare_For_Stmt : constant Bare_For_Stmt := Node;
begin
case Field is
when For_Stmt_F_Bindings => return For_Stmt_F_Bindings (N_Bare_For_Stmt);
when For_Stmt_F_Expr => return For_Stmt_F_Expr (N_Bare_For_Stmt);
when For_Stmt_F_Statements => return For_Stmt_F_Statements (N_Bare_For_Stmt);
when For_Stmt_F_Else_Part => return For_Stmt_F_Else_Part (N_Bare_For_Stmt);
when others => null;
end case;
end;
when Turkixir_Global_Stmt_Range =>
declare
N_Bare_Global_Stmt : constant Bare_Global_Stmt := Node;
begin
case Field is
when Global_Stmt_F_Names => return Global_Stmt_F_Names (N_Bare_Global_Stmt);
when others => null;
end case;
end;
when Turkixir_If_Stmt_Range =>
declare
N_Bare_If_Stmt : constant Bare_If_Stmt := Node;
begin
case Field is
when If_Stmt_F_Cond_Test => return If_Stmt_F_Cond_Test (N_Bare_If_Stmt);
when If_Stmt_F_Statements => return If_Stmt_F_Statements (N_Bare_If_Stmt);
when If_Stmt_F_Elif_Branchs => return If_Stmt_F_Elif_Branchs (N_Bare_If_Stmt);
when If_Stmt_F_Else_Part => return If_Stmt_F_Else_Part (N_Bare_If_Stmt);
when others => null;
end case;
end;
when Turkixir_Import_From_Range =>
declare
N_Bare_Import_From : constant Bare_Import_From := Node;
begin
case Field is
when Import_From_F_Rel_Name => return Import_From_F_Rel_Name (N_Bare_Import_From);
when Import_From_F_Imported => return Import_From_F_Imported (N_Bare_Import_From);
when others => null;
end case;
end;
when Turkixir_Import_Name_Range =>
declare
N_Bare_Import_Name : constant Bare_Import_Name := Node;
begin
case Field is
when Import_Name_F_Imported_Names => return Import_Name_F_Imported_Names (N_Bare_Import_Name);
when others => null;
end case;
end;
when Turkixir_Print_Stmt_Range =>
declare
N_Bare_Print_Stmt : constant Bare_Print_Stmt := Node;
begin
case Field is
when Print_Stmt_F_Exprs => return Print_Stmt_F_Exprs (N_Bare_Print_Stmt);
when others => null;
end case;
end;
when Turkixir_Raise_Stmt_Range =>
declare
N_Bare_Raise_Stmt : constant Bare_Raise_Stmt := Node;
begin
case Field is
when Raise_Stmt_F_Exprs => return Raise_Stmt_F_Exprs (N_Bare_Raise_Stmt);
when others => null;
end case;
end;
when Turkixir_Return_Stmt_Range =>
declare
N_Bare_Return_Stmt : constant Bare_Return_Stmt := Node;
begin
case Field is
when Return_Stmt_F_Exprs => return Return_Stmt_F_Exprs (N_Bare_Return_Stmt);
when others => null;
end case;
end;
when Turkixir_Stream_Print_Stmt_Range =>
declare
N_Bare_Stream_Print_Stmt : constant Bare_Stream_Print_Stmt := Node;
begin
case Field is
when Stream_Print_Stmt_F_Stream_Expr => return Stream_Print_Stmt_F_Stream_Expr (N_Bare_Stream_Print_Stmt);
when Stream_Print_Stmt_F_Exprs => return Stream_Print_Stmt_F_Exprs (N_Bare_Stream_Print_Stmt);
when others => null;
end case;
end;
when Turkixir_Try_Stmt_Range =>
declare
N_Bare_Try_Stmt : constant Bare_Try_Stmt := Node;
begin
case Field is
when Try_Stmt_F_Statements => return Try_Stmt_F_Statements (N_Bare_Try_Stmt);
when Try_Stmt_F_Except_Parts => return Try_Stmt_F_Except_Parts (N_Bare_Try_Stmt);
when Try_Stmt_F_Else_Part => return Try_Stmt_F_Else_Part (N_Bare_Try_Stmt);
when Try_Stmt_F_Finally_Part => return Try_Stmt_F_Finally_Part (N_Bare_Try_Stmt);
when others => null;
end case;
end;
when Turkixir_While_Stmt_Range =>
declare
N_Bare_While_Stmt : constant Bare_While_Stmt := Node;
begin
case Field is
when While_Stmt_F_Cond_Test => return While_Stmt_F_Cond_Test (N_Bare_While_Stmt);
when While_Stmt_F_Statements => return While_Stmt_F_Statements (N_Bare_While_Stmt);
when While_Stmt_F_Else_Part => return While_Stmt_F_Else_Part (N_Bare_While_Stmt);
when others => null;
end case;
end;
when Turkixir_With_Stmt_Range =>
declare
N_Bare_With_Stmt : constant Bare_With_Stmt := Node;
begin
case Field is
when With_Stmt_F_Bindings => return With_Stmt_F_Bindings (N_Bare_With_Stmt);
when With_Stmt_F_Statements => return With_Stmt_F_Statements (N_Bare_With_Stmt);
when others => null;
end case;
end;
when others => null;
end case;

      return (raise Bad_Type_Error with "no such field on this node");
   end Eval_Syntax_Field;

   -----------
   -- Index --
   -----------

   function Index
     (Kind : Turkixir_Node_Kind_Type; Field : Syntax_Field_Reference) return Positive is
   begin
         
         case Kind is
               when Turkixir_Arg_Assoc =>
               return (case Field is
                       when Arg_Assoc_F_Name => 1,
                       when Arg_Assoc_F_Expr => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Arg_Gen =>
               return (case Field is
                       when Arg_Gen_F_Expr => 1,
                       when Arg_Gen_F_Comprehension => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Kw_Args =>
               return (case Field is
                       when Kw_Args_F_Expr => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_Var_Args =>
               return (case Field is
                       when Var_Args_F_Expr => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_As_Name_Node =>
               return (case Field is
                       when As_Name_Node_F_Imported => 1,
                       when As_Name_Node_F_As_Name => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Comp_If =>
               return (case Field is
                       when Comp_If_F_Test => 1,
                       when Comp_If_F_Comp => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Comp_Op_Kind_Diamond =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Comp_Op_Kind_Eq =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Comp_Op_Kind_Gt =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Comp_Op_Kind_Gte =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Comp_Op_Kind_In =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Comp_Op_Kind_Is =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Comp_Op_Kind_Isnot =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Comp_Op_Kind_Lt =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Comp_Op_Kind_Lte =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Comp_Op_Kind_Noteq =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Comp_Op_Kind_Notin =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Comp_For =>
               return (case Field is
                       when Comp_For_F_Exprs => 1,
                       when Comp_For_F_Target => 2,
                       when Comp_For_F_Comp => 3,
                       when others => raise Bad_Type_Error);
               when Turkixir_Comp_ForL =>
               return (case Field is
                       when Comp_ForL_F_Exprs => 1,
                       when Comp_ForL_F_Target => 2,
                       when Comp_ForL_F_Comp => 3,
                       when others => raise Bad_Type_Error);
               when Turkixir_Decorator =>
               return (case Field is
                       when Decorator_F_Dec_Name => 1,
                       when Decorator_F_Arg_List => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Dict_Assoc =>
               return (case Field is
                       when Dict_Assoc_F_Key => 1,
                       when Dict_Assoc_F_Value => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Else_Part =>
               return (case Field is
                       when Else_Part_F_Statements => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_Except_Part =>
               return (case Field is
                       when Except_Part_F_As_Name => 1,
                       when Except_Part_F_Statements => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_And_Expr =>
               return (case Field is
                       when And_Expr_F_Left => 1,
                       when And_Expr_F_Right => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_And_Op =>
               return (case Field is
                       when And_Op_F_Left => 1,
                       when And_Op_F_Right => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Arith_Expr =>
               return (case Field is
                       when Bin_Op_F_Left => 1,
                       when Bin_Op_F_Op => 2,
                       when Bin_Op_F_Right => 3,
                       when others => raise Bad_Type_Error);
               when Turkixir_Shift_Expr =>
               return (case Field is
                       when Bin_Op_F_Left => 1,
                       when Bin_Op_F_Op => 2,
                       when Bin_Op_F_Right => 3,
                       when others => raise Bad_Type_Error);
               when Turkixir_Term =>
               return (case Field is
                       when Bin_Op_F_Left => 1,
                       when Bin_Op_F_Op => 2,
                       when Bin_Op_F_Right => 3,
                       when others => raise Bad_Type_Error);
               when Turkixir_Call_Expr =>
               return (case Field is
                       when Call_Expr_F_Prefix => 1,
                       when Call_Expr_F_Suffix => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Comp_Op =>
               return (case Field is
                       when Comp_Op_F_Left => 1,
                       when Comp_Op_F_Op => 2,
                       when Comp_Op_F_Right => 3,
                       when others => raise Bad_Type_Error);
               when Turkixir_Concat_String_Lit =>
               return (case Field is
                       when Concat_String_Lit_F_First_Str => 1,
                       when Concat_String_Lit_F_Subsequent_Str => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Dict_Comp =>
               return (case Field is
                       when Dict_Comp_F_Assoc => 1,
                       when Dict_Comp_F_Comprehension => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Dict_Lit =>
               return (case Field is
                       when Dict_Lit_F_Assocs => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_Dot =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Ellipsis_Expr =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Factor =>
               return (case Field is
                       when Factor_F_Op => 1,
                       when Factor_F_Expr => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_If_Expr =>
               return (case Field is
                       when If_Expr_F_Expr => 1,
                       when If_Expr_F_Cond => 2,
                       when If_Expr_F_Else_Expr => 3,
                       when others => raise Bad_Type_Error);
               when Turkixir_Inline_Eval =>
               return (case Field is
                       when Inline_Eval_F_Exprs => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_Lambda_Def =>
               return (case Field is
                       when Lambda_Def_F_Args => 1,
                       when Lambda_Def_F_Expr => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_List_Comp =>
               return (case Field is
                       when List_Comp_F_Expr => 1,
                       when List_Comp_F_Comprehension => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_List_Gen =>
               return (case Field is
                       when List_Gen_F_Expr => 1,
                       when List_Gen_F_Comprehension => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_List_Lit =>
               return (case Field is
                       when List_Lit_F_Exprs => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_Dotted_Name =>
               return (case Field is
                       when Dotted_Name_F_Prefix => 1,
                       when Dotted_Name_F_Suffix => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Id =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Not_Op =>
               return (case Field is
                       when Not_Op_F_Expr => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_Number_Lit =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Or_Expr =>
               return (case Field is
                       when Or_Expr_F_Left => 1,
                       when Or_Expr_F_Right => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Or_Op =>
               return (case Field is
                       when Or_Op_F_Left => 1,
                       when Or_Op_F_Right => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Power =>
               return (case Field is
                       when Power_F_Left => 1,
                       when Power_F_Right => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Set_Comp =>
               return (case Field is
                       when Set_Comp_F_Expr => 1,
                       when Set_Comp_F_Comprehension => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Set_Lit =>
               return (case Field is
                       when Set_Lit_F_Exprs => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_Slice_Expr =>
               return (case Field is
                       when Slice_Expr_F_First => 1,
                       when Slice_Expr_F_Last => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Ext_Slice_Expr =>
               return (case Field is
                       when Slice_Expr_F_First => 1,
                       when Slice_Expr_F_Last => 2,
                       when Ext_Slice_Expr_F_Stride => 3,
                       when others => raise Bad_Type_Error);
               when Turkixir_String_Lit =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Subscript_Expr =>
               return (case Field is
                       when Subscript_Expr_F_Prefix => 1,
                       when Subscript_Expr_F_Suffix => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Tuple_Lit =>
               return (case Field is
                       when Tuple_Lit_F_Exprs => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_Xor_Expr =>
               return (case Field is
                       when Xor_Expr_F_Left => 1,
                       when Xor_Expr_F_Right => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Yield_Expr =>
               return (case Field is
                       when Yield_Expr_F_Exprs => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_File_Node =>
               return (case Field is
                       when File_Node_F_Statements => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_Import_Star =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Kw_Args_Flag_Absent =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Kw_Args_Flag_Present =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_NL =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Op =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Params =>
               return (case Field is
                       when Params_F_Single_Params => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_Rel_Name =>
               return (case Field is
                       when Rel_Name_F_Dots => 1,
                       when Rel_Name_F_Name => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Single_Param =>
               return (case Field is
                       when Single_Param_F_Is_Varargs => 1,
                       when Single_Param_F_Is_Kwargs => 2,
                       when Single_Param_F_Name => 3,
                       when Single_Param_F_Default_Value => 4,
                       when others => raise Bad_Type_Error);
               when Turkixir_Assert_Stmt =>
               return (case Field is
                       when Assert_Stmt_F_Test_Expr => 1,
                       when Assert_Stmt_F_Msg => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Assign_Stmt =>
               return (case Field is
                       when Assign_Stmt_F_L_Value => 1,
                       when Assign_Stmt_F_R_Values => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Aug_Assign_Stmt =>
               return (case Field is
                       when Aug_Assign_Stmt_F_L_Value => 1,
                       when Aug_Assign_Stmt_F_Op => 2,
                       when Aug_Assign_Stmt_F_R_Value => 3,
                       when others => raise Bad_Type_Error);
               when Turkixir_Break_Stmt =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Continue_Stmt =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Decorated =>
               return (case Field is
                       when Decorated_F_Decorators => 1,
                       when Decorated_F_Defn => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Class_Def =>
               return (case Field is
                       when Class_Def_F_Name => 1,
                       when Class_Def_F_Bases => 2,
                       when Class_Def_F_Statements => 3,
                       when others => raise Bad_Type_Error);
               when Turkixir_Func_Def =>
               return (case Field is
                       when Func_Def_F_Name => 1,
                       when Func_Def_F_Parameters => 2,
                       when Func_Def_F_Body => 3,
                       when others => raise Bad_Type_Error);
               when Turkixir_Del_Stmt =>
               return (case Field is
                       when Del_Stmt_F_Exprs => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_Elif_Branch =>
               return (case Field is
                       when Elif_Branch_F_Cond_Test => 1,
                       when Elif_Branch_F_Statements => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Exec_Stmt =>
               return (case Field is
                       when Exec_Stmt_F_Expr => 1,
                       when Exec_Stmt_F_In_List => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_For_Stmt =>
               return (case Field is
                       when For_Stmt_F_Bindings => 1,
                       when For_Stmt_F_Expr => 2,
                       when For_Stmt_F_Statements => 3,
                       when For_Stmt_F_Else_Part => 4,
                       when others => raise Bad_Type_Error);
               when Turkixir_Global_Stmt =>
               return (case Field is
                       when Global_Stmt_F_Names => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_If_Stmt =>
               return (case Field is
                       when If_Stmt_F_Cond_Test => 1,
                       when If_Stmt_F_Statements => 2,
                       when If_Stmt_F_Elif_Branchs => 3,
                       when If_Stmt_F_Else_Part => 4,
                       when others => raise Bad_Type_Error);
               when Turkixir_Import_From =>
               return (case Field is
                       when Import_From_F_Rel_Name => 1,
                       when Import_From_F_Imported => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Import_Name =>
               return (case Field is
                       when Import_Name_F_Imported_Names => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_Pass_Stmt =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Print_Stmt =>
               return (case Field is
                       when Print_Stmt_F_Exprs => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_Raise_Stmt =>
               return (case Field is
                       when Raise_Stmt_F_Exprs => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_Return_Stmt =>
               return (case Field is
                       when Return_Stmt_F_Exprs => 1,
                       when others => raise Bad_Type_Error);
               when Turkixir_Stream_Print_Stmt =>
               return (case Field is
                       when Stream_Print_Stmt_F_Stream_Expr => 1,
                       when Stream_Print_Stmt_F_Exprs => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Try_Stmt =>
               return (case Field is
                       when Try_Stmt_F_Statements => 1,
                       when Try_Stmt_F_Except_Parts => 2,
                       when Try_Stmt_F_Else_Part => 3,
                       when Try_Stmt_F_Finally_Part => 4,
                       when others => raise Bad_Type_Error);
               when Turkixir_While_Stmt =>
               return (case Field is
                       when While_Stmt_F_Cond_Test => 1,
                       when While_Stmt_F_Statements => 2,
                       when While_Stmt_F_Else_Part => 3,
                       when others => raise Bad_Type_Error);
               when Turkixir_With_Stmt =>
               return (case Field is
                       when With_Stmt_F_Bindings => 1,
                       when With_Stmt_F_Statements => 2,
                       when others => raise Bad_Type_Error);
               when Turkixir_Arg_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_As_Name_Node_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Decorator_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Dict_Assoc_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Dot_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Elif_Branch_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Except_Part_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Expr_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Id_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_NL_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Single_Param_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_String_Lit_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Turkixir_Node_List =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Var_Args_Flag_Absent =>
               return (case Field is
                       when others => raise Bad_Type_Error);
               when Turkixir_Var_Args_Flag_Present =>
               return (case Field is
                       when others => raise Bad_Type_Error);
         end case;

   end Index;

   ---------------------------------------
   -- Syntax_Field_Reference_From_Index --
   ---------------------------------------

   function Syntax_Field_Reference_From_Index
     (Kind : Turkixir_Node_Kind_Type; Index : Positive) return Syntax_Field_Reference is
   begin
      
      case Turkixir_Turkixir_Node (Kind) is
when Turkixir_Arg_Assoc_Range =>
case Index is
when 1 => return Arg_Assoc_F_Name;
when 2 => return Arg_Assoc_F_Expr;
when others => null;
end case;
when Turkixir_Arg_Gen_Range =>
case Index is
when 1 => return Arg_Gen_F_Expr;
when 2 => return Arg_Gen_F_Comprehension;
when others => null;
end case;
when Turkixir_Kw_Args_Range =>
case Index is
when 1 => return Kw_Args_F_Expr;
when others => null;
end case;
when Turkixir_Var_Args_Range =>
case Index is
when 1 => return Var_Args_F_Expr;
when others => null;
end case;
when Turkixir_As_Name_Node_Range =>
case Index is
when 1 => return As_Name_Node_F_Imported;
when 2 => return As_Name_Node_F_As_Name;
when others => null;
end case;
when Turkixir_Comp_If_Range =>
case Index is
when 1 => return Comp_If_F_Test;
when 2 => return Comp_If_F_Comp;
when others => null;
end case;
when Turkixir_Comp_For_Range =>
case Index is
when 1 => return Comp_For_F_Exprs;
when 2 => return Comp_For_F_Target;
when 3 => return Comp_For_F_Comp;
when others => null;
end case;
when Turkixir_Comp_ForL_Range =>
case Index is
when 1 => return Comp_ForL_F_Exprs;
when 2 => return Comp_ForL_F_Target;
when 3 => return Comp_ForL_F_Comp;
when others => null;
end case;
when Turkixir_Decorator_Range =>
case Index is
when 1 => return Decorator_F_Dec_Name;
when 2 => return Decorator_F_Arg_List;
when others => null;
end case;
when Turkixir_Dict_Assoc_Range =>
case Index is
when 1 => return Dict_Assoc_F_Key;
when 2 => return Dict_Assoc_F_Value;
when others => null;
end case;
when Turkixir_Else_Part_Range =>
case Index is
when 1 => return Else_Part_F_Statements;
when others => null;
end case;
when Turkixir_Except_Part_Range =>
case Index is
when 1 => return Except_Part_F_As_Name;
when 2 => return Except_Part_F_Statements;
when others => null;
end case;
when Turkixir_And_Expr_Range =>
case Index is
when 1 => return And_Expr_F_Left;
when 2 => return And_Expr_F_Right;
when others => null;
end case;
when Turkixir_And_Op_Range =>
case Index is
when 1 => return And_Op_F_Left;
when 2 => return And_Op_F_Right;
when others => null;
end case;
when Turkixir_Bin_Op =>
case Index is
when 1 => return Bin_Op_F_Left;
when 2 => return Bin_Op_F_Op;
when 3 => return Bin_Op_F_Right;
when others => null;
end case;
when Turkixir_Call_Expr_Range =>
case Index is
when 1 => return Call_Expr_F_Prefix;
when 2 => return Call_Expr_F_Suffix;
when others => null;
end case;
when Turkixir_Comp_Op_Range =>
case Index is
when 1 => return Comp_Op_F_Left;
when 2 => return Comp_Op_F_Op;
when 3 => return Comp_Op_F_Right;
when others => null;
end case;
when Turkixir_Concat_String_Lit_Range =>
case Index is
when 1 => return Concat_String_Lit_F_First_Str;
when 2 => return Concat_String_Lit_F_Subsequent_Str;
when others => null;
end case;
when Turkixir_Dict_Comp_Range =>
case Index is
when 1 => return Dict_Comp_F_Assoc;
when 2 => return Dict_Comp_F_Comprehension;
when others => null;
end case;
when Turkixir_Dict_Lit_Range =>
case Index is
when 1 => return Dict_Lit_F_Assocs;
when others => null;
end case;
when Turkixir_Factor_Range =>
case Index is
when 1 => return Factor_F_Op;
when 2 => return Factor_F_Expr;
when others => null;
end case;
when Turkixir_If_Expr_Range =>
case Index is
when 1 => return If_Expr_F_Expr;
when 2 => return If_Expr_F_Cond;
when 3 => return If_Expr_F_Else_Expr;
when others => null;
end case;
when Turkixir_Inline_Eval_Range =>
case Index is
when 1 => return Inline_Eval_F_Exprs;
when others => null;
end case;
when Turkixir_Lambda_Def_Range =>
case Index is
when 1 => return Lambda_Def_F_Args;
when 2 => return Lambda_Def_F_Expr;
when others => null;
end case;
when Turkixir_List_Comp_Range =>
case Index is
when 1 => return List_Comp_F_Expr;
when 2 => return List_Comp_F_Comprehension;
when others => null;
end case;
when Turkixir_List_Gen_Range =>
case Index is
when 1 => return List_Gen_F_Expr;
when 2 => return List_Gen_F_Comprehension;
when others => null;
end case;
when Turkixir_List_Lit_Range =>
case Index is
when 1 => return List_Lit_F_Exprs;
when others => null;
end case;
when Turkixir_Dotted_Name_Range =>
case Index is
when 1 => return Dotted_Name_F_Prefix;
when 2 => return Dotted_Name_F_Suffix;
when others => null;
end case;
when Turkixir_Not_Op_Range =>
case Index is
when 1 => return Not_Op_F_Expr;
when others => null;
end case;
when Turkixir_Or_Expr_Range =>
case Index is
when 1 => return Or_Expr_F_Left;
when 2 => return Or_Expr_F_Right;
when others => null;
end case;
when Turkixir_Or_Op_Range =>
case Index is
when 1 => return Or_Op_F_Left;
when 2 => return Or_Op_F_Right;
when others => null;
end case;
when Turkixir_Power_Range =>
case Index is
when 1 => return Power_F_Left;
when 2 => return Power_F_Right;
when others => null;
end case;
when Turkixir_Set_Comp_Range =>
case Index is
when 1 => return Set_Comp_F_Expr;
when 2 => return Set_Comp_F_Comprehension;
when others => null;
end case;
when Turkixir_Set_Lit_Range =>
case Index is
when 1 => return Set_Lit_F_Exprs;
when others => null;
end case;
when Turkixir_Slice_Expr_Range =>
case Index is
when 1 => return Slice_Expr_F_First;
when 2 => return Slice_Expr_F_Last;
when others => null;
end case;
case Turkixir_Slice_Expr_Range (Kind) is
when Turkixir_Ext_Slice_Expr_Range =>
case Index is
when 3 => return Ext_Slice_Expr_F_Stride;
when others => null;
end case;
when others => null;
end case;
when Turkixir_Subscript_Expr_Range =>
case Index is
when 1 => return Subscript_Expr_F_Prefix;
when 2 => return Subscript_Expr_F_Suffix;
when others => null;
end case;
when Turkixir_Tuple_Lit_Range =>
case Index is
when 1 => return Tuple_Lit_F_Exprs;
when others => null;
end case;
when Turkixir_Xor_Expr_Range =>
case Index is
when 1 => return Xor_Expr_F_Left;
when 2 => return Xor_Expr_F_Right;
when others => null;
end case;
when Turkixir_Yield_Expr_Range =>
case Index is
when 1 => return Yield_Expr_F_Exprs;
when others => null;
end case;
when Turkixir_File_Node_Range =>
case Index is
when 1 => return File_Node_F_Statements;
when others => null;
end case;
when Turkixir_Params_Range =>
case Index is
when 1 => return Params_F_Single_Params;
when others => null;
end case;
when Turkixir_Rel_Name_Range =>
case Index is
when 1 => return Rel_Name_F_Dots;
when 2 => return Rel_Name_F_Name;
when others => null;
end case;
when Turkixir_Single_Param_Range =>
case Index is
when 1 => return Single_Param_F_Is_Varargs;
when 2 => return Single_Param_F_Is_Kwargs;
when 3 => return Single_Param_F_Name;
when 4 => return Single_Param_F_Default_Value;
when others => null;
end case;
when Turkixir_Assert_Stmt_Range =>
case Index is
when 1 => return Assert_Stmt_F_Test_Expr;
when 2 => return Assert_Stmt_F_Msg;
when others => null;
end case;
when Turkixir_Assign_Stmt_Range =>
case Index is
when 1 => return Assign_Stmt_F_L_Value;
when 2 => return Assign_Stmt_F_R_Values;
when others => null;
end case;
when Turkixir_Aug_Assign_Stmt_Range =>
case Index is
when 1 => return Aug_Assign_Stmt_F_L_Value;
when 2 => return Aug_Assign_Stmt_F_Op;
when 3 => return Aug_Assign_Stmt_F_R_Value;
when others => null;
end case;
when Turkixir_Decorated_Range =>
case Index is
when 1 => return Decorated_F_Decorators;
when 2 => return Decorated_F_Defn;
when others => null;
end case;
when Turkixir_Class_Def_Range =>
case Index is
when 1 => return Class_Def_F_Name;
when 2 => return Class_Def_F_Bases;
when 3 => return Class_Def_F_Statements;
when others => null;
end case;
when Turkixir_Func_Def_Range =>
case Index is
when 1 => return Func_Def_F_Name;
when 2 => return Func_Def_F_Parameters;
when 3 => return Func_Def_F_Body;
when others => null;
end case;
when Turkixir_Del_Stmt_Range =>
case Index is
when 1 => return Del_Stmt_F_Exprs;
when others => null;
end case;
when Turkixir_Elif_Branch_Range =>
case Index is
when 1 => return Elif_Branch_F_Cond_Test;
when 2 => return Elif_Branch_F_Statements;
when others => null;
end case;
when Turkixir_Exec_Stmt_Range =>
case Index is
when 1 => return Exec_Stmt_F_Expr;
when 2 => return Exec_Stmt_F_In_List;
when others => null;
end case;
when Turkixir_For_Stmt_Range =>
case Index is
when 1 => return For_Stmt_F_Bindings;
when 2 => return For_Stmt_F_Expr;
when 3 => return For_Stmt_F_Statements;
when 4 => return For_Stmt_F_Else_Part;
when others => null;
end case;
when Turkixir_Global_Stmt_Range =>
case Index is
when 1 => return Global_Stmt_F_Names;
when others => null;
end case;
when Turkixir_If_Stmt_Range =>
case Index is
when 1 => return If_Stmt_F_Cond_Test;
when 2 => return If_Stmt_F_Statements;
when 3 => return If_Stmt_F_Elif_Branchs;
when 4 => return If_Stmt_F_Else_Part;
when others => null;
end case;
when Turkixir_Import_From_Range =>
case Index is
when 1 => return Import_From_F_Rel_Name;
when 2 => return Import_From_F_Imported;
when others => null;
end case;
when Turkixir_Import_Name_Range =>
case Index is
when 1 => return Import_Name_F_Imported_Names;
when others => null;
end case;
when Turkixir_Print_Stmt_Range =>
case Index is
when 1 => return Print_Stmt_F_Exprs;
when others => null;
end case;
when Turkixir_Raise_Stmt_Range =>
case Index is
when 1 => return Raise_Stmt_F_Exprs;
when others => null;
end case;
when Turkixir_Return_Stmt_Range =>
case Index is
when 1 => return Return_Stmt_F_Exprs;
when others => null;
end case;
when Turkixir_Stream_Print_Stmt_Range =>
case Index is
when 1 => return Stream_Print_Stmt_F_Stream_Expr;
when 2 => return Stream_Print_Stmt_F_Exprs;
when others => null;
end case;
when Turkixir_Try_Stmt_Range =>
case Index is
when 1 => return Try_Stmt_F_Statements;
when 2 => return Try_Stmt_F_Except_Parts;
when 3 => return Try_Stmt_F_Else_Part;
when 4 => return Try_Stmt_F_Finally_Part;
when others => null;
end case;
when Turkixir_While_Stmt_Range =>
case Index is
when 1 => return While_Stmt_F_Cond_Test;
when 2 => return While_Stmt_F_Statements;
when 3 => return While_Stmt_F_Else_Part;
when others => null;
end case;
when Turkixir_With_Stmt_Range =>
case Index is
when 1 => return With_Stmt_F_Bindings;
when 2 => return With_Stmt_F_Statements;
when others => null;
end case;
when Turkixir_Turkixir_Node_Base_List =>
raise Bad_Type_Error with "List AST nodes have no field";
when others => null;
end case;

      pragma Warnings (Off, "value not in range of type");
      return (raise Bad_Type_Error with "Index is out of bounds");
      pragma Warnings (On, "value not in range of type");
   end Syntax_Field_Reference_From_Index;

   -------------------
   -- Syntax_Fields --
   -------------------

   function Syntax_Fields
     (Kind : Turkixir_Node_Kind_Type) return Syntax_Field_Reference_Array is
   begin
         return Syntax_Fields (Id_For_Kind (Kind), Concrete_Only => True);
   end Syntax_Fields;

   -------------------
   -- Syntax_Fields --
   -------------------

   function Syntax_Fields
     (Id            : Node_Type_Id;
      Concrete_Only : Boolean) return Syntax_Field_Reference_Array
   is
      Cursor : Any_Node_Type_Id := Id;

      Added_Fields : array (Syntax_Field_Reference) of Boolean :=
        (others => False);
      --  Set of field references that were added to Result

      Result : Syntax_Field_Reference_Array (1 .. Added_Fields'Length);
      --  Temporary to hold the result. We return Result (1 .. Last).

      Last : Natural := 0;
      --  Index of the last element in Result to return
   begin

         --  Go through the derivation chain for Id and collect fields. Do
         --  it in reverse order as we process base types last.
         while Cursor /= None loop
            declare
               Node_Desc : Node_Type_Descriptor renames
                  Node_Type_Descriptors (Cursor).all;
            begin
               for Field_Index in reverse Node_Desc.Fields'Range loop
                  declare
                     Field_Desc : Node_Field_Descriptor renames
                        Node_Desc.Fields (Field_Index).all;
                     Field      : Syntax_Field_Reference renames
                        Field_Desc.Field;
                  begin
                     --  Abstract fields share the same Syntax_Field_Reference
                     --  value with the corresponding concrete fields, so
                     --  collect fields only once. We process fields in reverse
                     --  order, so we know that concrete ones will be processed
                     --  before the abstract fields they override.
                     if not (Concrete_Only
                             and then Field_Desc.Is_Abstract_Or_Null)
                        and then not Added_Fields (Field)
                     then
                        Added_Fields (Field) := True;
                        Last := Last + 1;
                        Result (Last) := Field;
                     end if;
                  end;
               end loop;
               Cursor := Node_Desc.Base_Type;
            end;
         end loop;

         --  At this point, Result contains elements in the opposite order as
         --  expected, so reverse it.

         for I in 1 .. Last / 2 loop
            declare
               Other_I : constant Positive := Last - I + 1;
               Swap    : constant Syntax_Field_Reference := Result (I);
            begin
               Result (I) := Result (Other_I);
               Result (Other_I) := Swap;
            end;
         end loop;

         return Result (1 .. Last);

   end Syntax_Fields;

   -------------------
   -- Syntax_Fields --
   -------------------

   function Syntax_Fields
     (Id : Node_Type_Id) return Syntax_Field_Reference_Array is
   begin
      return Syntax_Fields (Id, Concrete_Only => False);
   end Syntax_Fields;


   -------------------
   -- Property_Name --
   -------------------

   function Property_Name (Property : Property_Reference) return Text_Type is
   begin
      return To_Text (Property_Descriptors (Property).Name);
   end Property_Name;

   --------------------------
   -- Property_Return_Type --
   --------------------------

   function Property_Return_Type
     (Property : Property_Reference) return Type_Constraint is
   begin
      return Property_Descriptors (Property).Return_Type;
   end Property_Return_Type;

   ---------------------------
   -- Check_Argument_Number --
   ---------------------------

   procedure Check_Argument_Number
     (Desc : Property_Descriptor; Argument_Number : Positive) is
   begin
      if Argument_Number not in Desc.Argument_Names'Range then
         raise Property_Error with "out-of-bounds argument number";
      end if;
   end Check_Argument_Number;

   -----------------------------
   -- Property_Argument_Types --
   -----------------------------

   function Property_Argument_Types
     (Property : Property_Reference) return Type_Constraint_Array is
   begin
      return Property_Descriptors (Property).Argument_Types;
   end Property_Argument_Types;

   ----------------------------
   -- Property_Argument_Name --
   ----------------------------

   function Property_Argument_Name
     (Property        : Property_Reference;
      Argument_Number : Positive) return Text_Type
   is
      Desc : Property_Descriptor renames Property_Descriptors (Property).all;
   begin
      Check_Argument_Number (Desc, Argument_Number);
      return To_Text
        (Property_Descriptors (Property).Argument_Names (Argument_Number).all);
   end Property_Argument_Name;

   -------------------------------------
   -- Property_Argument_Default_Value --
   -------------------------------------

   function Property_Argument_Default_Value
     (Property        : Property_Reference;
      Argument_Number : Positive) return Internal_Value
   is
      Desc : Property_Descriptor renames Property_Descriptors (Property).all;
   begin
      Check_Argument_Number (Desc, Argument_Number);
      return Desc.Argument_Default_Values (Argument_Number);
   end Property_Argument_Default_Value;

   ----------------
   -- Properties --
   ----------------

   function Properties (Kind : Turkixir_Node_Kind_Type) return Property_Reference_Array
   is
   begin
      return Properties (Id_For_Kind (Kind));
   end Properties;

   ----------------
   -- Properties --
   ----------------

   function Properties (Id : Node_Type_Id) return Property_Reference_Array is
      Cursor : Any_Node_Type_Id := Id;

      Result : Property_Reference_Array (1 .. Property_Descriptors'Length);
      --  Temporary to hold the result. We return Result (1 .. Last).

      Last : Natural := 0;
      --  Index of the last element in Result to return
   begin
      --  Go through the derivation chain for Id and collect properties. Do
      --  it in reverse order as we process base types last.

      while Cursor /= None loop
         declare
            Node_Desc : Node_Type_Descriptor renames
               Node_Type_Descriptors (Cursor).all;
         begin
            for Prop_Desc of reverse Node_Desc.Properties loop
               Last := Last + 1;
               Result (Last) := Prop_Desc;
            end loop;
            Cursor := Node_Desc.Base_Type;
         end;
      end loop;

      --  At this point, Result contains elements in the opposite order as
      --  expected, so reverse it.

      for I in 1 .. Last / 2 loop
         declare
            Other_I : constant Positive := Last - I + 1;
            Swap    : constant Property_Reference := Result (I);
         begin
            Result (I) := Result (Other_I);
            Result (Other_I) := Swap;
         end;
      end loop;

      return Result (1 .. Last);
   end Properties;


   ---------------------
   -- Token_Node_Kind --
   ---------------------

   function Token_Node_Kind (Kind : Turkixir_Node_Kind_Type) return Token_Kind is
      
   begin
         pragma Unreferenced (Kind);
         return (raise Program_Error);
   end Token_Node_Kind;

begin
   for D in Node_Type_Descriptors'Range loop
      DSL_Name_To_Node_Type.Insert (Node_Type_Descriptors (D).DSL_Name, D);
   end loop;
end Libturkixirlang.Introspection_Implementation;
