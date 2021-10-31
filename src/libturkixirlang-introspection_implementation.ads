
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with Libturkixirlang.Implementation; use Libturkixirlang.Implementation;
with Libturkixirlang.Common;         use Libturkixirlang.Common;

private package Libturkixirlang.Introspection_Implementation is

   use Support.Text;

   ------------------------
   -- Polymorphic values --
   ------------------------

   --  TODO: for now, support only value types that are required to represent
   --  default values for property arguments.

   subtype Internal_Value_Kind is Any_Value_Kind
      with Static_Predicate => Internal_Value_Kind in
         None | Boolean_Value | Integer_Value | Character_Value | String_Value
       | Analysis_Unit_Kind_Value
       | Lookup_Kind_Value
       | Designated_Env_Kind_Value
       | Grammar_Rule_Value
       | Node_Value;

   type Internal_Value (Kind : Internal_Value_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Boolean_Value =>
            Boolean_Value : Boolean;

         when Integer_Value =>
            Integer_Value : Integer;

         when Character_Value =>
            Character_Value : Character_Type;

         when String_Value =>
            String_Value : String_Type;

         when Analysis_Unit_Kind_Value =>
            Analysis_Unit_Kind_Value : Analysis_Unit_Kind;
         when Lookup_Kind_Value =>
            Lookup_Kind_Value : Lookup_Kind;
         when Designated_Env_Kind_Value =>
            Designated_Env_Kind_Value : Designated_Env_Kind;
         when Grammar_Rule_Value =>
            Grammar_Rule_Value : Grammar_Rule;

         when Node_Value =>
            Node_Value : Internal_Entity;
      end case;
   end record;

   No_Internal_Value : constant Internal_Value := (Kind => None);

   type Internal_Value_Array is array (Positive range <>) of Internal_Value;

   function As_Boolean (Self : Internal_Value) return Boolean;
   function Create_Boolean (Value : Boolean) return Internal_Value is
     ((Kind => Boolean_Value, Boolean_Value => Value));

   function As_Integer (Self : Internal_Value) return Integer;
   function Create_Integer (Value : Integer) return Internal_Value is
     ((Kind => Integer_Value, Integer_Value => Value));

   function As_Character (Self : Internal_Value) return Character_Type;
   function Create_Character (Value : Character_Type) return Internal_Value is
     ((Kind => Character_Value, Character_Value => Value));

   function As_String (Self : Internal_Value) return String_Type;
   function Create_String (Value : String_Type) return Internal_Value is
     ((Kind => String_Value, String_Value => Value));

   function As_Node (Self : Internal_Value) return Internal_Entity;
   function Create_Node (Value : Internal_Entity) return Internal_Value is
     ((Kind => Node_Value, Node_Value => Value));

      function As_Analysis_Unit_Kind
        (Self : Internal_Value) return Analysis_Unit_Kind;
      function Create_Analysis_Unit_Kind
        (Value : Analysis_Unit_Kind) return Internal_Value
      is ((Kind => Analysis_Unit_Kind_Value,
           Analysis_Unit_Kind_Value => Value));
      function As_Lookup_Kind
        (Self : Internal_Value) return Lookup_Kind;
      function Create_Lookup_Kind
        (Value : Lookup_Kind) return Internal_Value
      is ((Kind => Lookup_Kind_Value,
           Lookup_Kind_Value => Value));
      function As_Designated_Env_Kind
        (Self : Internal_Value) return Designated_Env_Kind;
      function Create_Designated_Env_Kind
        (Value : Designated_Env_Kind) return Internal_Value
      is ((Kind => Designated_Env_Kind_Value,
           Designated_Env_Kind_Value => Value));
      function As_Grammar_Rule
        (Self : Internal_Value) return Grammar_Rule;
      function Create_Grammar_Rule
        (Value : Grammar_Rule) return Internal_Value
      is ((Kind => Grammar_Rule_Value,
           Grammar_Rule_Value => Value));

   -----------------------
   -- Descriptor tables --
   -----------------------

   type String_Access is access constant String;
   type String_Array is array (Positive range <>) of String_Access;

   ------------------------------
   -- Struct field descriptors --
   ------------------------------

   type Struct_Field_Descriptor (Name_Length : Natural) is record
      Reference : Struct_Field_Reference;
      --  Enum value that designates this field

      Field_Type : Type_Constraint;
      --  Type for this field

      Name : String (1 .. Name_Length);
      --  Lower-case name for this field
   end record;
   --  General description of a struct field

   type Struct_Field_Descriptor_Access is
      access constant Struct_Field_Descriptor;
   type Struct_Field_Descriptor_Array is
      array (Positive range <>) of Struct_Field_Descriptor_Access;

   -----------------------------
   -- Struct type descriptors --
   -----------------------------

   type Struct_Type_Descriptor (Fields_Count : Natural) is record
      Fields : Struct_Field_Descriptor_Array (1 .. Fields_Count);
   end record;

   type Struct_Type_Descriptor_Access is
      access constant Struct_Type_Descriptor;

   ------------------------------
   -- Syntax field descriptors --
   ------------------------------

   type Syntax_Field_Descriptor (Name_Length : Natural) is record
      Field_Type : Node_Type_Id;
      Name       : String (1 .. Name_Length);
   end record;
   --  General description of a field (independent of field implementations)

   type Syntax_Field_Descriptor_Access is
      access constant Syntax_Field_Descriptor;

   --  Descriptors for syntax fields

      
      Desc_For_Arg_Assoc_F_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_name"
         );
      
      Desc_For_Arg_Assoc_F_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_expr"
         );
      
      Desc_For_Arg_Gen_F_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_expr"
         );
      
      Desc_For_Arg_Gen_F_Comprehension : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 15,
            Field_Type  => Common.Comp_For_Type_Id,
            Name        => "f_comprehension"
         );
      
      Desc_For_Kw_Args_F_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_expr"
         );
      
      Desc_For_Var_Args_F_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_expr"
         );
      
      Desc_For_As_Name_Node_F_Imported : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_imported"
         );
      
      Desc_For_As_Name_Node_F_As_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 9,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_as_name"
         );
      
      Desc_For_Comp_If_F_Test : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_test"
         );
      
      Desc_For_Comp_If_F_Comp : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_comp"
         );
      
      Desc_For_Comp_For_F_Exprs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_exprs"
         );
      
      Desc_For_Comp_For_F_Target : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 8,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_target"
         );
      
      Desc_For_Comp_For_F_Comp : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_comp"
         );
      
      Desc_For_Comp_ForL_F_Exprs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_exprs"
         );
      
      Desc_For_Comp_ForL_F_Target : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 8,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_target"
         );
      
      Desc_For_Comp_ForL_F_Comp : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_comp"
         );
      
      Desc_For_Decorator_F_Dec_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Name_Type_Id,
            Name        => "f_dec_name"
         );
      
      Desc_For_Decorator_F_Arg_List : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Arg_List_Type_Id,
            Name        => "f_arg_list"
         );
      
      Desc_For_Dict_Assoc_F_Key : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 5,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_key"
         );
      
      Desc_For_Dict_Assoc_F_Value : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_value"
         );
      
      Desc_For_Else_Part_F_Statements : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_statements"
         );
      
      Desc_For_Except_Part_F_As_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 9,
            Field_Type  => Common.As_Name_Node_Type_Id,
            Name        => "f_as_name"
         );
      
      Desc_For_Except_Part_F_Statements : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_statements"
         );
      
      Desc_For_And_Expr_F_Left : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_left"
         );
      
      Desc_For_And_Expr_F_Right : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_right"
         );
      
      Desc_For_And_Op_F_Left : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_left"
         );
      
      Desc_For_And_Op_F_Right : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_right"
         );
      
      Desc_For_Bin_Op_F_Left : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_left"
         );
      
      Desc_For_Bin_Op_F_Op : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 4,
            Field_Type  => Common.Op_Type_Id,
            Name        => "f_op"
         );
      
      Desc_For_Bin_Op_F_Right : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_right"
         );
      
      Desc_For_Call_Expr_F_Prefix : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 8,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_prefix"
         );
      
      Desc_For_Call_Expr_F_Suffix : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 8,
            Field_Type  => Common.Arg_List_Type_Id,
            Name        => "f_suffix"
         );
      
      Desc_For_Comp_Op_F_Left : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_left"
         );
      
      Desc_For_Comp_Op_F_Op : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 4,
            Field_Type  => Common.Comp_Op_Kind_Type_Id,
            Name        => "f_op"
         );
      
      Desc_For_Comp_Op_F_Right : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_right"
         );
      
      Desc_For_Concat_String_Lit_F_First_Str : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.String_Lit_Type_Id,
            Name        => "f_first_str"
         );
      
      Desc_For_Concat_String_Lit_F_Subsequent_Str : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 16,
            Field_Type  => Common.String_Lit_List_Type_Id,
            Name        => "f_subsequent_str"
         );
      
      Desc_For_Dict_Comp_F_Assoc : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Dict_Assoc_Type_Id,
            Name        => "f_assoc"
         );
      
      Desc_For_Dict_Comp_F_Comprehension : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 15,
            Field_Type  => Common.Comp_For_Type_Id,
            Name        => "f_comprehension"
         );
      
      Desc_For_Dict_Lit_F_Assocs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 8,
            Field_Type  => Common.Dict_Assoc_List_Type_Id,
            Name        => "f_assocs"
         );
      
      Desc_For_Factor_F_Op : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 4,
            Field_Type  => Common.Op_Type_Id,
            Name        => "f_op"
         );
      
      Desc_For_Factor_F_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_expr"
         );
      
      Desc_For_If_Expr_F_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_expr"
         );
      
      Desc_For_If_Expr_F_Cond : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_cond"
         );
      
      Desc_For_If_Expr_F_Else_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_else_expr"
         );
      
      Desc_For_Inline_Eval_F_Exprs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_exprs"
         );
      
      Desc_For_Lambda_Def_F_Args : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Params_Type_Id,
            Name        => "f_args"
         );
      
      Desc_For_Lambda_Def_F_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_expr"
         );
      
      Desc_For_List_Comp_F_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_expr"
         );
      
      Desc_For_List_Comp_F_Comprehension : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 15,
            Field_Type  => Common.Comp_ForL_Type_Id,
            Name        => "f_comprehension"
         );
      
      Desc_For_List_Gen_F_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_expr"
         );
      
      Desc_For_List_Gen_F_Comprehension : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 15,
            Field_Type  => Common.Comp_ForL_Type_Id,
            Name        => "f_comprehension"
         );
      
      Desc_For_List_Lit_F_Exprs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_exprs"
         );
      
      Desc_For_Dotted_Name_F_Prefix : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 8,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_prefix"
         );
      
      Desc_For_Dotted_Name_F_Suffix : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 8,
            Field_Type  => Common.Id_Type_Id,
            Name        => "f_suffix"
         );
      
      Desc_For_Not_Op_F_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_expr"
         );
      
      Desc_For_Or_Expr_F_Left : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_left"
         );
      
      Desc_For_Or_Expr_F_Right : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_right"
         );
      
      Desc_For_Or_Op_F_Left : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_left"
         );
      
      Desc_For_Or_Op_F_Right : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_right"
         );
      
      Desc_For_Power_F_Left : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_left"
         );
      
      Desc_For_Power_F_Right : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_right"
         );
      
      Desc_For_Set_Comp_F_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_expr"
         );
      
      Desc_For_Set_Comp_F_Comprehension : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 15,
            Field_Type  => Common.Comp_For_Type_Id,
            Name        => "f_comprehension"
         );
      
      Desc_For_Set_Lit_F_Exprs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_exprs"
         );
      
      Desc_For_Slice_Expr_F_First : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_first"
         );
      
      Desc_For_Slice_Expr_F_Last : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_last"
         );
      
      Desc_For_Ext_Slice_Expr_F_Stride : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 8,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_stride"
         );
      
      Desc_For_Subscript_Expr_F_Prefix : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 8,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_prefix"
         );
      
      Desc_For_Subscript_Expr_F_Suffix : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 8,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_suffix"
         );
      
      Desc_For_Tuple_Lit_F_Exprs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_exprs"
         );
      
      Desc_For_Xor_Expr_F_Left : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_left"
         );
      
      Desc_For_Xor_Expr_F_Right : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_right"
         );
      
      Desc_For_Yield_Expr_F_Exprs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_exprs"
         );
      
      Desc_For_File_Node_F_Statements : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Turkixir_Node_List_Type_Id,
            Name        => "f_statements"
         );
      
      Desc_For_Params_F_Single_Params : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 15,
            Field_Type  => Common.Single_Param_List_Type_Id,
            Name        => "f_single_params"
         );
      
      Desc_For_Rel_Name_F_Dots : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Dot_List_Type_Id,
            Name        => "f_dots"
         );
      
      Desc_For_Rel_Name_F_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Name_Type_Id,
            Name        => "f_name"
         );
      
      Desc_For_Single_Param_F_Is_Varargs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Var_Args_Flag_Type_Id,
            Name        => "f_is_varargs"
         );
      
      Desc_For_Single_Param_F_Is_Kwargs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Kw_Args_Flag_Type_Id,
            Name        => "f_is_kwargs"
         );
      
      Desc_For_Single_Param_F_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_name"
         );
      
      Desc_For_Single_Param_F_Default_Value : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 15,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_default_value"
         );
      
      Desc_For_Assert_Stmt_F_Test_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_test_expr"
         );
      
      Desc_For_Assert_Stmt_F_Msg : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 5,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_msg"
         );
      
      Desc_For_Assign_Stmt_F_L_Value : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 9,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_l_value"
         );
      
      Desc_For_Assign_Stmt_F_R_Values : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Turkixir_Node_List_Type_Id,
            Name        => "f_r_values"
         );
      
      Desc_For_Aug_Assign_Stmt_F_L_Value : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 9,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_l_value"
         );
      
      Desc_For_Aug_Assign_Stmt_F_Op : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 4,
            Field_Type  => Common.Op_Type_Id,
            Name        => "f_op"
         );
      
      Desc_For_Aug_Assign_Stmt_F_R_Value : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 9,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_r_value"
         );
      
      Desc_For_Decorated_F_Decorators : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Decorator_List_Type_Id,
            Name        => "f_decorators"
         );
      
      Desc_For_Decorated_F_Defn : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Def_Stmt_Type_Id,
            Name        => "f_defn"
         );
      
      Desc_For_Class_Def_F_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Id_Type_Id,
            Name        => "f_name"
         );
      
      Desc_For_Class_Def_F_Bases : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_bases"
         );
      
      Desc_For_Class_Def_F_Statements : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_statements"
         );
      
      Desc_For_Func_Def_F_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Id_Type_Id,
            Name        => "f_name"
         );
      
      Desc_For_Func_Def_F_Parameters : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Params_Type_Id,
            Name        => "f_parameters"
         );
      
      Desc_For_Func_Def_F_Body : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_body"
         );
      
      Desc_For_Del_Stmt_F_Exprs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_exprs"
         );
      
      Desc_For_Elif_Branch_F_Cond_Test : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_cond_test"
         );
      
      Desc_For_Elif_Branch_F_Statements : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_statements"
         );
      
      Desc_For_Exec_Stmt_F_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_expr"
         );
      
      Desc_For_Exec_Stmt_F_In_List : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 9,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_in_list"
         );
      
      Desc_For_For_Stmt_F_Bindings : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_bindings"
         );
      
      Desc_For_For_Stmt_F_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 6,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_expr"
         );
      
      Desc_For_For_Stmt_F_Statements : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_statements"
         );
      
      Desc_For_For_Stmt_F_Else_Part : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Else_Part_Type_Id,
            Name        => "f_else_part"
         );
      
      Desc_For_Global_Stmt_F_Names : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Id_List_Type_Id,
            Name        => "f_names"
         );
      
      Desc_For_If_Stmt_F_Cond_Test : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_cond_test"
         );
      
      Desc_For_If_Stmt_F_Statements : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_statements"
         );
      
      Desc_For_If_Stmt_F_Elif_Branchs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 14,
            Field_Type  => Common.Elif_Branch_List_Type_Id,
            Name        => "f_elif_branchs"
         );
      
      Desc_For_If_Stmt_F_Else_Part : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Else_Part_Type_Id,
            Name        => "f_else_part"
         );
      
      Desc_For_Import_From_F_Rel_Name : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_rel_name"
         );
      
      Desc_For_Import_From_F_Imported : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_imported"
         );
      
      Desc_For_Import_Name_F_Imported_Names : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 16,
            Field_Type  => Common.Turkixir_Node_List_Type_Id,
            Name        => "f_imported_names"
         );
      
      Desc_For_Print_Stmt_F_Exprs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_exprs"
         );
      
      Desc_For_Raise_Stmt_F_Exprs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_exprs"
         );
      
      Desc_For_Return_Stmt_F_Exprs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_exprs"
         );
      
      Desc_For_Stream_Print_Stmt_F_Stream_Expr : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 13,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_stream_expr"
         );
      
      Desc_For_Stream_Print_Stmt_F_Exprs : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 7,
            Field_Type  => Common.Expr_List_Type_Id,
            Name        => "f_exprs"
         );
      
      Desc_For_Try_Stmt_F_Statements : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_statements"
         );
      
      Desc_For_Try_Stmt_F_Except_Parts : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 14,
            Field_Type  => Common.Except_Part_List_Type_Id,
            Name        => "f_except_parts"
         );
      
      Desc_For_Try_Stmt_F_Else_Part : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Else_Part_Type_Id,
            Name        => "f_else_part"
         );
      
      Desc_For_Try_Stmt_F_Finally_Part : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 14,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_finally_part"
         );
      
      Desc_For_While_Stmt_F_Cond_Test : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Expr_Type_Id,
            Name        => "f_cond_test"
         );
      
      Desc_For_While_Stmt_F_Statements : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_statements"
         );
      
      Desc_For_While_Stmt_F_Else_Part : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 11,
            Field_Type  => Common.Else_Part_Type_Id,
            Name        => "f_else_part"
         );
      
      Desc_For_With_Stmt_F_Bindings : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 10,
            Field_Type  => Common.As_Name_Node_List_Type_Id,
            Name        => "f_bindings"
         );
      
      Desc_For_With_Stmt_F_Statements : aliased constant
         Syntax_Field_Descriptor := (
            Name_Length => 12,
            Field_Type  => Common.Turkixir_Node_Type_Id,
            Name        => "f_statements"
         );

   Syntax_Field_Descriptors : constant
      array (Syntax_Field_Reference) of Syntax_Field_Descriptor_Access := (
         Arg_Assoc_F_Name => Desc_For_Arg_Assoc_F_Name'Access, Arg_Assoc_F_Expr => Desc_For_Arg_Assoc_F_Expr'Access, Arg_Gen_F_Expr => Desc_For_Arg_Gen_F_Expr'Access, Arg_Gen_F_Comprehension => Desc_For_Arg_Gen_F_Comprehension'Access, Kw_Args_F_Expr => Desc_For_Kw_Args_F_Expr'Access, Var_Args_F_Expr => Desc_For_Var_Args_F_Expr'Access, As_Name_Node_F_Imported => Desc_For_As_Name_Node_F_Imported'Access, As_Name_Node_F_As_Name => Desc_For_As_Name_Node_F_As_Name'Access, Comp_If_F_Test => Desc_For_Comp_If_F_Test'Access, Comp_If_F_Comp => Desc_For_Comp_If_F_Comp'Access, Comp_For_F_Exprs => Desc_For_Comp_For_F_Exprs'Access, Comp_For_F_Target => Desc_For_Comp_For_F_Target'Access, Comp_For_F_Comp => Desc_For_Comp_For_F_Comp'Access, Comp_ForL_F_Exprs => Desc_For_Comp_ForL_F_Exprs'Access, Comp_ForL_F_Target => Desc_For_Comp_ForL_F_Target'Access, Comp_ForL_F_Comp => Desc_For_Comp_ForL_F_Comp'Access, Decorator_F_Dec_Name => Desc_For_Decorator_F_Dec_Name'Access, Decorator_F_Arg_List => Desc_For_Decorator_F_Arg_List'Access, Dict_Assoc_F_Key => Desc_For_Dict_Assoc_F_Key'Access, Dict_Assoc_F_Value => Desc_For_Dict_Assoc_F_Value'Access, Else_Part_F_Statements => Desc_For_Else_Part_F_Statements'Access, Except_Part_F_As_Name => Desc_For_Except_Part_F_As_Name'Access, Except_Part_F_Statements => Desc_For_Except_Part_F_Statements'Access, And_Expr_F_Left => Desc_For_And_Expr_F_Left'Access, And_Expr_F_Right => Desc_For_And_Expr_F_Right'Access, And_Op_F_Left => Desc_For_And_Op_F_Left'Access, And_Op_F_Right => Desc_For_And_Op_F_Right'Access, Bin_Op_F_Left => Desc_For_Bin_Op_F_Left'Access, Bin_Op_F_Op => Desc_For_Bin_Op_F_Op'Access, Bin_Op_F_Right => Desc_For_Bin_Op_F_Right'Access, Call_Expr_F_Prefix => Desc_For_Call_Expr_F_Prefix'Access, Call_Expr_F_Suffix => Desc_For_Call_Expr_F_Suffix'Access, Comp_Op_F_Left => Desc_For_Comp_Op_F_Left'Access, Comp_Op_F_Op => Desc_For_Comp_Op_F_Op'Access, Comp_Op_F_Right => Desc_For_Comp_Op_F_Right'Access, Concat_String_Lit_F_First_Str => Desc_For_Concat_String_Lit_F_First_Str'Access, Concat_String_Lit_F_Subsequent_Str => Desc_For_Concat_String_Lit_F_Subsequent_Str'Access, Dict_Comp_F_Assoc => Desc_For_Dict_Comp_F_Assoc'Access, Dict_Comp_F_Comprehension => Desc_For_Dict_Comp_F_Comprehension'Access, Dict_Lit_F_Assocs => Desc_For_Dict_Lit_F_Assocs'Access, Factor_F_Op => Desc_For_Factor_F_Op'Access, Factor_F_Expr => Desc_For_Factor_F_Expr'Access, If_Expr_F_Expr => Desc_For_If_Expr_F_Expr'Access, If_Expr_F_Cond => Desc_For_If_Expr_F_Cond'Access, If_Expr_F_Else_Expr => Desc_For_If_Expr_F_Else_Expr'Access, Inline_Eval_F_Exprs => Desc_For_Inline_Eval_F_Exprs'Access, Lambda_Def_F_Args => Desc_For_Lambda_Def_F_Args'Access, Lambda_Def_F_Expr => Desc_For_Lambda_Def_F_Expr'Access, List_Comp_F_Expr => Desc_For_List_Comp_F_Expr'Access, List_Comp_F_Comprehension => Desc_For_List_Comp_F_Comprehension'Access, List_Gen_F_Expr => Desc_For_List_Gen_F_Expr'Access, List_Gen_F_Comprehension => Desc_For_List_Gen_F_Comprehension'Access, List_Lit_F_Exprs => Desc_For_List_Lit_F_Exprs'Access, Dotted_Name_F_Prefix => Desc_For_Dotted_Name_F_Prefix'Access, Dotted_Name_F_Suffix => Desc_For_Dotted_Name_F_Suffix'Access, Not_Op_F_Expr => Desc_For_Not_Op_F_Expr'Access, Or_Expr_F_Left => Desc_For_Or_Expr_F_Left'Access, Or_Expr_F_Right => Desc_For_Or_Expr_F_Right'Access, Or_Op_F_Left => Desc_For_Or_Op_F_Left'Access, Or_Op_F_Right => Desc_For_Or_Op_F_Right'Access, Power_F_Left => Desc_For_Power_F_Left'Access, Power_F_Right => Desc_For_Power_F_Right'Access, Set_Comp_F_Expr => Desc_For_Set_Comp_F_Expr'Access, Set_Comp_F_Comprehension => Desc_For_Set_Comp_F_Comprehension'Access, Set_Lit_F_Exprs => Desc_For_Set_Lit_F_Exprs'Access, Slice_Expr_F_First => Desc_For_Slice_Expr_F_First'Access, Slice_Expr_F_Last => Desc_For_Slice_Expr_F_Last'Access, Ext_Slice_Expr_F_Stride => Desc_For_Ext_Slice_Expr_F_Stride'Access, Subscript_Expr_F_Prefix => Desc_For_Subscript_Expr_F_Prefix'Access, Subscript_Expr_F_Suffix => Desc_For_Subscript_Expr_F_Suffix'Access, Tuple_Lit_F_Exprs => Desc_For_Tuple_Lit_F_Exprs'Access, Xor_Expr_F_Left => Desc_For_Xor_Expr_F_Left'Access, Xor_Expr_F_Right => Desc_For_Xor_Expr_F_Right'Access, Yield_Expr_F_Exprs => Desc_For_Yield_Expr_F_Exprs'Access, File_Node_F_Statements => Desc_For_File_Node_F_Statements'Access, Params_F_Single_Params => Desc_For_Params_F_Single_Params'Access, Rel_Name_F_Dots => Desc_For_Rel_Name_F_Dots'Access, Rel_Name_F_Name => Desc_For_Rel_Name_F_Name'Access, Single_Param_F_Is_Varargs => Desc_For_Single_Param_F_Is_Varargs'Access, Single_Param_F_Is_Kwargs => Desc_For_Single_Param_F_Is_Kwargs'Access, Single_Param_F_Name => Desc_For_Single_Param_F_Name'Access, Single_Param_F_Default_Value => Desc_For_Single_Param_F_Default_Value'Access, Assert_Stmt_F_Test_Expr => Desc_For_Assert_Stmt_F_Test_Expr'Access, Assert_Stmt_F_Msg => Desc_For_Assert_Stmt_F_Msg'Access, Assign_Stmt_F_L_Value => Desc_For_Assign_Stmt_F_L_Value'Access, Assign_Stmt_F_R_Values => Desc_For_Assign_Stmt_F_R_Values'Access, Aug_Assign_Stmt_F_L_Value => Desc_For_Aug_Assign_Stmt_F_L_Value'Access, Aug_Assign_Stmt_F_Op => Desc_For_Aug_Assign_Stmt_F_Op'Access, Aug_Assign_Stmt_F_R_Value => Desc_For_Aug_Assign_Stmt_F_R_Value'Access, Decorated_F_Decorators => Desc_For_Decorated_F_Decorators'Access, Decorated_F_Defn => Desc_For_Decorated_F_Defn'Access, Class_Def_F_Name => Desc_For_Class_Def_F_Name'Access, Class_Def_F_Bases => Desc_For_Class_Def_F_Bases'Access, Class_Def_F_Statements => Desc_For_Class_Def_F_Statements'Access, Func_Def_F_Name => Desc_For_Func_Def_F_Name'Access, Func_Def_F_Parameters => Desc_For_Func_Def_F_Parameters'Access, Func_Def_F_Body => Desc_For_Func_Def_F_Body'Access, Del_Stmt_F_Exprs => Desc_For_Del_Stmt_F_Exprs'Access, Elif_Branch_F_Cond_Test => Desc_For_Elif_Branch_F_Cond_Test'Access, Elif_Branch_F_Statements => Desc_For_Elif_Branch_F_Statements'Access, Exec_Stmt_F_Expr => Desc_For_Exec_Stmt_F_Expr'Access, Exec_Stmt_F_In_List => Desc_For_Exec_Stmt_F_In_List'Access, For_Stmt_F_Bindings => Desc_For_For_Stmt_F_Bindings'Access, For_Stmt_F_Expr => Desc_For_For_Stmt_F_Expr'Access, For_Stmt_F_Statements => Desc_For_For_Stmt_F_Statements'Access, For_Stmt_F_Else_Part => Desc_For_For_Stmt_F_Else_Part'Access, Global_Stmt_F_Names => Desc_For_Global_Stmt_F_Names'Access, If_Stmt_F_Cond_Test => Desc_For_If_Stmt_F_Cond_Test'Access, If_Stmt_F_Statements => Desc_For_If_Stmt_F_Statements'Access, If_Stmt_F_Elif_Branchs => Desc_For_If_Stmt_F_Elif_Branchs'Access, If_Stmt_F_Else_Part => Desc_For_If_Stmt_F_Else_Part'Access, Import_From_F_Rel_Name => Desc_For_Import_From_F_Rel_Name'Access, Import_From_F_Imported => Desc_For_Import_From_F_Imported'Access, Import_Name_F_Imported_Names => Desc_For_Import_Name_F_Imported_Names'Access, Print_Stmt_F_Exprs => Desc_For_Print_Stmt_F_Exprs'Access, Raise_Stmt_F_Exprs => Desc_For_Raise_Stmt_F_Exprs'Access, Return_Stmt_F_Exprs => Desc_For_Return_Stmt_F_Exprs'Access, Stream_Print_Stmt_F_Stream_Expr => Desc_For_Stream_Print_Stmt_F_Stream_Expr'Access, Stream_Print_Stmt_F_Exprs => Desc_For_Stream_Print_Stmt_F_Exprs'Access, Try_Stmt_F_Statements => Desc_For_Try_Stmt_F_Statements'Access, Try_Stmt_F_Except_Parts => Desc_For_Try_Stmt_F_Except_Parts'Access, Try_Stmt_F_Else_Part => Desc_For_Try_Stmt_F_Else_Part'Access, Try_Stmt_F_Finally_Part => Desc_For_Try_Stmt_F_Finally_Part'Access, While_Stmt_F_Cond_Test => Desc_For_While_Stmt_F_Cond_Test'Access, While_Stmt_F_Statements => Desc_For_While_Stmt_F_Statements'Access, While_Stmt_F_Else_Part => Desc_For_While_Stmt_F_Else_Part'Access, With_Stmt_F_Bindings => Desc_For_With_Stmt_F_Bindings'Access, With_Stmt_F_Statements => Desc_For_With_Stmt_F_Statements'Access
   );

   --------------------------
   -- Property descriptors --
   --------------------------

   type Property_Descriptor (
      Name_Length : Natural;
      --  Length of the proprety name

      Arity : Natural
      --  Number of arguments this property takes (exclude the ``Self``
      --  argument).
   )
   is record
      Name : String (1 .. Name_Length);
      --  Lower-case name for this property

      Return_Type : Type_Constraint;
      --  Return type for this property

      Argument_Types : Type_Constraint_Array (1 .. Arity);
      --  Types of the arguments that this property takes

      Argument_Names : String_Array (1 .. Arity);
      --  Lower-case names for arguments that this property takes

      Argument_Default_Values : Internal_Value_Array (1 .. Arity);
      --  Default values (if any, otherwise ``No_Internal_Value``) for
      --  arguments that this property takes.
   end record;

   type Property_Descriptor_Access is access constant Property_Descriptor;

   --  Descriptors for properties

   
   Name_For_with_self : aliased constant String := "with_self";

      
      Desc_For_Turkixir_Node_Parent : aliased constant
         Property_Descriptor := (
            Name_Length => 6,
            Arity       => 0,

            Name => "parent",

            Return_Type    => (Kind => Node_Value, Node_Type => Common.Turkixir_Node_Type_Id),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Turkixir_Node_Parents : aliased constant
         Property_Descriptor := (
            Name_Length => 7,
            Arity       => 1,

            Name => "parents",

            Return_Type    => (Kind => Turkixir_Node_Array_Value),
            Argument_Types => (
                  1 => (Kind => Boolean_Value)
            ),
            Argument_Names => (
                  1 => Name_For_with_self'Access
            ),
            Argument_Default_Values => (
                  1 => Create_Boolean (True)
            )
         );
      
      Desc_For_Turkixir_Node_Children : aliased constant
         Property_Descriptor := (
            Name_Length => 8,
            Arity       => 0,

            Name => "children",

            Return_Type    => (Kind => Turkixir_Node_Array_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Turkixir_Node_Token_Start : aliased constant
         Property_Descriptor := (
            Name_Length => 11,
            Arity       => 0,

            Name => "token_start",

            Return_Type    => (Kind => Token_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Turkixir_Node_Token_End : aliased constant
         Property_Descriptor := (
            Name_Length => 9,
            Arity       => 0,

            Name => "token_end",

            Return_Type    => (Kind => Token_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Turkixir_Node_Child_Index : aliased constant
         Property_Descriptor := (
            Name_Length => 11,
            Arity       => 0,

            Name => "child_index",

            Return_Type    => (Kind => Integer_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Turkixir_Node_Previous_Sibling : aliased constant
         Property_Descriptor := (
            Name_Length => 16,
            Arity       => 0,

            Name => "previous_sibling",

            Return_Type    => (Kind => Node_Value, Node_Type => Common.Turkixir_Node_Type_Id),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Turkixir_Node_Next_Sibling : aliased constant
         Property_Descriptor := (
            Name_Length => 12,
            Arity       => 0,

            Name => "next_sibling",

            Return_Type    => (Kind => Node_Value, Node_Type => Common.Turkixir_Node_Type_Id),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Turkixir_Node_Unit : aliased constant
         Property_Descriptor := (
            Name_Length => 4,
            Arity       => 0,

            Name => "unit",

            Return_Type    => (Kind => Analysis_Unit_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Turkixir_Node_Is_Ghost : aliased constant
         Property_Descriptor := (
            Name_Length => 8,
            Arity       => 0,

            Name => "is_ghost",

            Return_Type    => (Kind => Boolean_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Turkixir_Node_Full_Sloc_Image : aliased constant
         Property_Descriptor := (
            Name_Length => 15,
            Arity       => 0,

            Name => "full_sloc_image",

            Return_Type    => (Kind => String_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Kw_Args_Flag_P_As_Bool : aliased constant
         Property_Descriptor := (
            Name_Length => 9,
            Arity       => 0,

            Name => "p_as_bool",

            Return_Type    => (Kind => Boolean_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );
      
      Desc_For_Var_Args_Flag_P_As_Bool : aliased constant
         Property_Descriptor := (
            Name_Length => 9,
            Arity       => 0,

            Name => "p_as_bool",

            Return_Type    => (Kind => Boolean_Value),
            Argument_Types => (
                  1 .. 0 => <>
            ),
            Argument_Names => (
                  1 .. 0 => <>
            ),
            Argument_Default_Values => (
                  1 .. 0 => <>
            )
         );

      Property_Descriptors : constant
         array (Property_Reference) of Property_Descriptor_Access := (
            Desc_For_Turkixir_Node_Parent'Access, Desc_For_Turkixir_Node_Parents'Access, Desc_For_Turkixir_Node_Children'Access, Desc_For_Turkixir_Node_Token_Start'Access, Desc_For_Turkixir_Node_Token_End'Access, Desc_For_Turkixir_Node_Child_Index'Access, Desc_For_Turkixir_Node_Previous_Sibling'Access, Desc_For_Turkixir_Node_Next_Sibling'Access, Desc_For_Turkixir_Node_Unit'Access, Desc_For_Turkixir_Node_Is_Ghost'Access, Desc_For_Turkixir_Node_Full_Sloc_Image'Access, Desc_For_Kw_Args_Flag_P_As_Bool'Access, Desc_For_Var_Args_Flag_P_As_Bool'Access
      );

   ---------------------------
   -- Node type descriptors --
   ---------------------------

   type Node_Field_Descriptor (Is_Abstract_Or_Null : Boolean) is record
      Field : Syntax_Field_Reference;
      --  Reference to the field this describes

      --  Only non-null concrete fields are assigned an index

      case Is_Abstract_Or_Null is
         when False =>
            Index : Positive;
            --  Index for this field

         when True =>
            null;
      end case;
   end record;
   --  Description of a field as implemented by a specific node

   type Node_Field_Descriptor_Access is access constant Node_Field_Descriptor;
   type Node_Field_Descriptor_Array is
      array (Positive range <>) of Node_Field_Descriptor_Access;

   type Node_Type_Descriptor
     (Is_Abstract       : Boolean;
      Derivations_Count : Natural;
      Fields_Count      : Natural;
      Properties_Count  : Natural)
   is record
      Base_Type : Any_Node_Type_Id;
      --  Reference to the node type from which this derives

      Derivations : Node_Type_Id_Array (1 .. Derivations_Count);
      --  List of references for all node types that derives from this

      DSL_Name : Unbounded_String;
      --  Name for this type in the Langkit DSL

      Inherited_Fields : Natural;
      --  Number of syntax field inherited from the base type

      Fields : Node_Field_Descriptor_Array (1 .. Fields_Count);
      --  For regular node types, list of syntax fields that are specific to
      --  this derivation (i.e. excluding fields from the base type).

      Properties : Property_Reference_Array (1 .. Properties_Count);
      --  List of properties that this node provides that are specific to this
      --  derivation (i.e. excluding fields from the base type).

      --  Only concrete nodes are assigned a node kind

      case Is_Abstract is
         when False =>
            Kind : Turkixir_Node_Kind_Type;
            --  Kind corresponding this this node type

         when True =>
            null;
      end case;
   end record;

   type Node_Type_Descriptor_Access is access constant Node_Type_Descriptor;

   --  Descriptors for struct types and their fields


   Struct_Field_Descriptors : constant
      array (Struct_Field_Reference) of Struct_Field_Descriptor_Access := (
         Struct_Field_Reference => <>
   );


   --  Descriptors for node types and their syntax fields

   


   Desc_For_Turkixir_Node : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 21,
      Fields_Count      => 0,
      Properties_Count  => 11,

      Base_Type   => None,
      Derivations =>
         (1 => Common.Arg_Type_Id, 2 => Common.As_Name_Node_Type_Id, 3 => Common.Comp_If_Type_Id, 4 => Common.Comp_Op_Kind_Type_Id, 5 => Common.Comprehension_Type_Id, 6 => Common.Decorator_Type_Id, 7 => Common.Dict_Assoc_Type_Id, 8 => Common.Else_Part_Type_Id, 9 => Common.Except_Part_Type_Id, 10 => Common.Expr_Type_Id, 11 => Common.File_Node_Type_Id, 12 => Common.Import_Star_Type_Id, 13 => Common.Kw_Args_Flag_Type_Id, 14 => Common.NL_Type_Id, 15 => Common.Op_Type_Id, 16 => Common.Params_Type_Id, 17 => Common.Rel_Name_Type_Id, 18 => Common.Single_Param_Type_Id, 19 => Common.Stmt_Type_Id, 20 => Common.Turkixir_Node_Base_List_Type_Id, 21 => Common.Var_Args_Flag_Type_Id),

      DSL_Name => To_Unbounded_String ("TurkixirNode"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 => Turkixir_Node_Parent, 2 => Turkixir_Node_Parents, 3 => Turkixir_Node_Children, 4 => Turkixir_Node_Token_Start, 5 => Turkixir_Node_Token_End, 6 => Turkixir_Node_Child_Index, 7 => Turkixir_Node_Previous_Sibling, 8 => Turkixir_Node_Next_Sibling, 9 => Turkixir_Node_Unit, 10 => Turkixir_Node_Is_Ghost, 11 => Turkixir_Node_Full_Sloc_Image
      )

   );
   


   Desc_For_Arg : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 4,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 => Common.Arg_Assoc_Type_Id, 2 => Common.Arg_Gen_Type_Id, 3 => Common.Kw_Args_Type_Id, 4 => Common.Var_Args_Type_Id),

      DSL_Name => To_Unbounded_String ("Arg"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   

   Arg_Assoc_F_Name_For_Arg_Assoc : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Arg_Assoc_F_Name

         , Index => 1
   );
   Arg_Assoc_F_Expr_For_Arg_Assoc : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Arg_Assoc_F_Expr

         , Index => 2
   );

   Desc_For_Arg_Assoc : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Arg_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ArgAssoc"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Arg_Assoc_F_Name_For_Arg_Assoc'Access, 2 => Arg_Assoc_F_Expr_For_Arg_Assoc'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Arg_Assoc
   );
   

   Arg_Gen_F_Expr_For_Arg_Gen : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Arg_Gen_F_Expr

         , Index => 1
   );
   Arg_Gen_F_Comprehension_For_Arg_Gen : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Arg_Gen_F_Comprehension

         , Index => 2
   );

   Desc_For_Arg_Gen : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Arg_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ArgGen"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Arg_Gen_F_Expr_For_Arg_Gen'Access, 2 => Arg_Gen_F_Comprehension_For_Arg_Gen'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Arg_Gen
   );
   

   Kw_Args_F_Expr_For_Kw_Args : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Kw_Args_F_Expr

         , Index => 1
   );

   Desc_For_Kw_Args : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Arg_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("KwArgs"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Kw_Args_F_Expr_For_Kw_Args'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Kw_Args
   );
   

   Var_Args_F_Expr_For_Var_Args : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Var_Args_F_Expr

         , Index => 1
   );

   Desc_For_Var_Args : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Arg_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("VarArgs"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Var_Args_F_Expr_For_Var_Args'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Var_Args
   );
   

   As_Name_Node_F_Imported_For_As_Name_Node : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => As_Name_Node_F_Imported

         , Index => 1
   );
   As_Name_Node_F_As_Name_For_As_Name_Node : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => As_Name_Node_F_As_Name

         , Index => 2
   );

   Desc_For_As_Name_Node : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AsNameNode"),

      Inherited_Fields => 0,
      Fields           => (
            1 => As_Name_Node_F_Imported_For_As_Name_Node'Access, 2 => As_Name_Node_F_As_Name_For_As_Name_Node'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_As_Name_Node
   );
   

   Comp_If_F_Test_For_Comp_If : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Comp_If_F_Test

         , Index => 1
   );
   Comp_If_F_Comp_For_Comp_If : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Comp_If_F_Comp

         , Index => 2
   );

   Desc_For_Comp_If : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompIf"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Comp_If_F_Test_For_Comp_If'Access, 2 => Comp_If_F_Comp_For_Comp_If'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Comp_If
   );
   


   Desc_For_Comp_Op_Kind : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 11,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 => Common.Comp_Op_Kind_Diamond_Type_Id, 2 => Common.Comp_Op_Kind_Eq_Type_Id, 3 => Common.Comp_Op_Kind_Gt_Type_Id, 4 => Common.Comp_Op_Kind_Gte_Type_Id, 5 => Common.Comp_Op_Kind_In_Type_Id, 6 => Common.Comp_Op_Kind_Is_Type_Id, 7 => Common.Comp_Op_Kind_Isnot_Type_Id, 8 => Common.Comp_Op_Kind_Lt_Type_Id, 9 => Common.Comp_Op_Kind_Lte_Type_Id, 10 => Common.Comp_Op_Kind_Noteq_Type_Id, 11 => Common.Comp_Op_Kind_Notin_Type_Id),

      DSL_Name => To_Unbounded_String ("CompOpKind"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   


   Desc_For_Comp_Op_Kind_Diamond : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Comp_Op_Kind_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompOpKind.Diamond"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Comp_Op_Kind_Diamond
   );
   


   Desc_For_Comp_Op_Kind_Eq : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Comp_Op_Kind_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompOpKind.Eq"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Comp_Op_Kind_Eq
   );
   


   Desc_For_Comp_Op_Kind_Gt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Comp_Op_Kind_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompOpKind.Gt"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Comp_Op_Kind_Gt
   );
   


   Desc_For_Comp_Op_Kind_Gte : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Comp_Op_Kind_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompOpKind.Gte"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Comp_Op_Kind_Gte
   );
   


   Desc_For_Comp_Op_Kind_In : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Comp_Op_Kind_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompOpKind.In"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Comp_Op_Kind_In
   );
   


   Desc_For_Comp_Op_Kind_Is : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Comp_Op_Kind_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompOpKind.Is"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Comp_Op_Kind_Is
   );
   


   Desc_For_Comp_Op_Kind_Isnot : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Comp_Op_Kind_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompOpKind.Isnot"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Comp_Op_Kind_Isnot
   );
   


   Desc_For_Comp_Op_Kind_Lt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Comp_Op_Kind_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompOpKind.Lt"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Comp_Op_Kind_Lt
   );
   


   Desc_For_Comp_Op_Kind_Lte : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Comp_Op_Kind_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompOpKind.Lte"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Comp_Op_Kind_Lte
   );
   


   Desc_For_Comp_Op_Kind_Noteq : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Comp_Op_Kind_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompOpKind.Noteq"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Comp_Op_Kind_Noteq
   );
   


   Desc_For_Comp_Op_Kind_Notin : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Comp_Op_Kind_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompOpKind.Notin"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Comp_Op_Kind_Notin
   );
   


   Desc_For_Comprehension : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 2,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 => Common.Comp_For_Type_Id, 2 => Common.Comp_ForL_Type_Id),

      DSL_Name => To_Unbounded_String ("Comprehension"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   

   Comp_For_F_Exprs_For_Comp_For : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Comp_For_F_Exprs

         , Index => 1
   );
   Comp_For_F_Target_For_Comp_For : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Comp_For_F_Target

         , Index => 2
   );
   Comp_For_F_Comp_For_Comp_For : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Comp_For_F_Comp

         , Index => 3
   );

   Desc_For_Comp_For : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 3,
      Properties_Count  => 0,

      Base_Type   => Common.Comprehension_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompFor"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Comp_For_F_Exprs_For_Comp_For'Access, 2 => Comp_For_F_Target_For_Comp_For'Access, 3 => Comp_For_F_Comp_For_Comp_For'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Comp_For
   );
   

   Comp_ForL_F_Exprs_For_Comp_ForL : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Comp_ForL_F_Exprs

         , Index => 1
   );
   Comp_ForL_F_Target_For_Comp_ForL : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Comp_ForL_F_Target

         , Index => 2
   );
   Comp_ForL_F_Comp_For_Comp_ForL : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Comp_ForL_F_Comp

         , Index => 3
   );

   Desc_For_Comp_ForL : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 3,
      Properties_Count  => 0,

      Base_Type   => Common.Comprehension_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompForL"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Comp_ForL_F_Exprs_For_Comp_ForL'Access, 2 => Comp_ForL_F_Target_For_Comp_ForL'Access, 3 => Comp_ForL_F_Comp_For_Comp_ForL'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Comp_ForL
   );
   

   Decorator_F_Dec_Name_For_Decorator : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Decorator_F_Dec_Name

         , Index => 1
   );
   Decorator_F_Arg_List_For_Decorator : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Decorator_F_Arg_List

         , Index => 2
   );

   Desc_For_Decorator : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Decorator"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Decorator_F_Dec_Name_For_Decorator'Access, 2 => Decorator_F_Arg_List_For_Decorator'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Decorator
   );
   

   Dict_Assoc_F_Key_For_Dict_Assoc : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Dict_Assoc_F_Key

         , Index => 1
   );
   Dict_Assoc_F_Value_For_Dict_Assoc : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Dict_Assoc_F_Value

         , Index => 2
   );

   Desc_For_Dict_Assoc : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DictAssoc"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Dict_Assoc_F_Key_For_Dict_Assoc'Access, 2 => Dict_Assoc_F_Value_For_Dict_Assoc'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Dict_Assoc
   );
   

   Else_Part_F_Statements_For_Else_Part : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Else_Part_F_Statements

         , Index => 1
   );

   Desc_For_Else_Part : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ElsePart"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Else_Part_F_Statements_For_Else_Part'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Else_Part
   );
   

   Except_Part_F_As_Name_For_Except_Part : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Except_Part_F_As_Name

         , Index => 1
   );
   Except_Part_F_Statements_For_Except_Part : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Except_Part_F_Statements

         , Index => 2
   );

   Desc_For_Except_Part : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ExceptPart"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Except_Part_F_As_Name_For_Except_Part'Access, 2 => Except_Part_F_Statements_For_Except_Part'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Except_Part
   );
   


   Desc_For_Expr : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 31,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 => Common.And_Expr_Type_Id, 2 => Common.And_Op_Type_Id, 3 => Common.Bin_Op_Type_Id, 4 => Common.Call_Expr_Type_Id, 5 => Common.Comp_Op_Type_Id, 6 => Common.Concat_String_Lit_Type_Id, 7 => Common.Dict_Comp_Type_Id, 8 => Common.Dict_Lit_Type_Id, 9 => Common.Dot_Type_Id, 10 => Common.Ellipsis_Expr_Type_Id, 11 => Common.Factor_Type_Id, 12 => Common.If_Expr_Type_Id, 13 => Common.Inline_Eval_Type_Id, 14 => Common.Lambda_Def_Type_Id, 15 => Common.List_Comp_Type_Id, 16 => Common.List_Gen_Type_Id, 17 => Common.List_Lit_Type_Id, 18 => Common.Name_Type_Id, 19 => Common.Not_Op_Type_Id, 20 => Common.Number_Lit_Type_Id, 21 => Common.Or_Expr_Type_Id, 22 => Common.Or_Op_Type_Id, 23 => Common.Power_Type_Id, 24 => Common.Set_Comp_Type_Id, 25 => Common.Set_Lit_Type_Id, 26 => Common.Slice_Expr_Type_Id, 27 => Common.String_Lit_Type_Id, 28 => Common.Subscript_Expr_Type_Id, 29 => Common.Tuple_Lit_Type_Id, 30 => Common.Xor_Expr_Type_Id, 31 => Common.Yield_Expr_Type_Id),

      DSL_Name => To_Unbounded_String ("Expr"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   

   And_Expr_F_Left_For_And_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => And_Expr_F_Left

         , Index => 1
   );
   And_Expr_F_Right_For_And_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => And_Expr_F_Right

         , Index => 2
   );

   Desc_For_And_Expr : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AndExpr"),

      Inherited_Fields => 0,
      Fields           => (
            1 => And_Expr_F_Left_For_And_Expr'Access, 2 => And_Expr_F_Right_For_And_Expr'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_And_Expr
   );
   

   And_Op_F_Left_For_And_Op : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => And_Op_F_Left

         , Index => 1
   );
   And_Op_F_Right_For_And_Op : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => And_Op_F_Right

         , Index => 2
   );

   Desc_For_And_Op : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AndOp"),

      Inherited_Fields => 0,
      Fields           => (
            1 => And_Op_F_Left_For_And_Op'Access, 2 => And_Op_F_Right_For_And_Op'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_And_Op
   );
   

   Bin_Op_F_Left_For_Bin_Op : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Bin_Op_F_Left

         , Index => 1
   );
   Bin_Op_F_Op_For_Bin_Op : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Bin_Op_F_Op

         , Index => 2
   );
   Bin_Op_F_Right_For_Bin_Op : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Bin_Op_F_Right

         , Index => 3
   );

   Desc_For_Bin_Op : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 3,
      Fields_Count      => 3,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 => Common.Arith_Expr_Type_Id, 2 => Common.Shift_Expr_Type_Id, 3 => Common.Term_Type_Id),

      DSL_Name => To_Unbounded_String ("BinOp"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Bin_Op_F_Left_For_Bin_Op'Access, 2 => Bin_Op_F_Op_For_Bin_Op'Access, 3 => Bin_Op_F_Right_For_Bin_Op'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   


   Desc_For_Arith_Expr : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Bin_Op_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ArithExpr"),

      Inherited_Fields => 3,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Arith_Expr
   );
   


   Desc_For_Shift_Expr : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Bin_Op_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ShiftExpr"),

      Inherited_Fields => 3,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Shift_Expr
   );
   


   Desc_For_Term : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Bin_Op_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Term"),

      Inherited_Fields => 3,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Term
   );
   

   Call_Expr_F_Prefix_For_Call_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Call_Expr_F_Prefix

         , Index => 1
   );
   Call_Expr_F_Suffix_For_Call_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Call_Expr_F_Suffix

         , Index => 2
   );

   Desc_For_Call_Expr : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CallExpr"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Call_Expr_F_Prefix_For_Call_Expr'Access, 2 => Call_Expr_F_Suffix_For_Call_Expr'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Call_Expr
   );
   

   Comp_Op_F_Left_For_Comp_Op : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Comp_Op_F_Left

         , Index => 1
   );
   Comp_Op_F_Op_For_Comp_Op : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Comp_Op_F_Op

         , Index => 2
   );
   Comp_Op_F_Right_For_Comp_Op : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Comp_Op_F_Right

         , Index => 3
   );

   Desc_For_Comp_Op : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 3,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("CompOp"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Comp_Op_F_Left_For_Comp_Op'Access, 2 => Comp_Op_F_Op_For_Comp_Op'Access, 3 => Comp_Op_F_Right_For_Comp_Op'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Comp_Op
   );
   

   Concat_String_Lit_F_First_Str_For_Concat_String_Lit : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Concat_String_Lit_F_First_Str

         , Index => 1
   );
   Concat_String_Lit_F_Subsequent_Str_For_Concat_String_Lit : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Concat_String_Lit_F_Subsequent_Str

         , Index => 2
   );

   Desc_For_Concat_String_Lit : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ConcatStringLit"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Concat_String_Lit_F_First_Str_For_Concat_String_Lit'Access, 2 => Concat_String_Lit_F_Subsequent_Str_For_Concat_String_Lit'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Concat_String_Lit
   );
   

   Dict_Comp_F_Assoc_For_Dict_Comp : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Dict_Comp_F_Assoc

         , Index => 1
   );
   Dict_Comp_F_Comprehension_For_Dict_Comp : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Dict_Comp_F_Comprehension

         , Index => 2
   );

   Desc_For_Dict_Comp : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DictComp"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Dict_Comp_F_Assoc_For_Dict_Comp'Access, 2 => Dict_Comp_F_Comprehension_For_Dict_Comp'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Dict_Comp
   );
   

   Dict_Lit_F_Assocs_For_Dict_Lit : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Dict_Lit_F_Assocs

         , Index => 1
   );

   Desc_For_Dict_Lit : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DictLit"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Dict_Lit_F_Assocs_For_Dict_Lit'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Dict_Lit
   );
   


   Desc_For_Dot : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Dot"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Dot
   );
   


   Desc_For_Ellipsis_Expr : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("EllipsisExpr"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Ellipsis_Expr
   );
   

   Factor_F_Op_For_Factor : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Factor_F_Op

         , Index => 1
   );
   Factor_F_Expr_For_Factor : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Factor_F_Expr

         , Index => 2
   );

   Desc_For_Factor : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Factor"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Factor_F_Op_For_Factor'Access, 2 => Factor_F_Expr_For_Factor'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Factor
   );
   

   If_Expr_F_Expr_For_If_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => If_Expr_F_Expr

         , Index => 1
   );
   If_Expr_F_Cond_For_If_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => If_Expr_F_Cond

         , Index => 2
   );
   If_Expr_F_Else_Expr_For_If_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => If_Expr_F_Else_Expr

         , Index => 3
   );

   Desc_For_If_Expr : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 3,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("IfExpr"),

      Inherited_Fields => 0,
      Fields           => (
            1 => If_Expr_F_Expr_For_If_Expr'Access, 2 => If_Expr_F_Cond_For_If_Expr'Access, 3 => If_Expr_F_Else_Expr_For_If_Expr'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_If_Expr
   );
   

   Inline_Eval_F_Exprs_For_Inline_Eval : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Inline_Eval_F_Exprs

         , Index => 1
   );

   Desc_For_Inline_Eval : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("InlineEval"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Inline_Eval_F_Exprs_For_Inline_Eval'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Inline_Eval
   );
   

   Lambda_Def_F_Args_For_Lambda_Def : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Lambda_Def_F_Args

         , Index => 1
   );
   Lambda_Def_F_Expr_For_Lambda_Def : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Lambda_Def_F_Expr

         , Index => 2
   );

   Desc_For_Lambda_Def : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("LambdaDef"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Lambda_Def_F_Args_For_Lambda_Def'Access, 2 => Lambda_Def_F_Expr_For_Lambda_Def'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Lambda_Def
   );
   

   List_Comp_F_Expr_For_List_Comp : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => List_Comp_F_Expr

         , Index => 1
   );
   List_Comp_F_Comprehension_For_List_Comp : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => List_Comp_F_Comprehension

         , Index => 2
   );

   Desc_For_List_Comp : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ListComp"),

      Inherited_Fields => 0,
      Fields           => (
            1 => List_Comp_F_Expr_For_List_Comp'Access, 2 => List_Comp_F_Comprehension_For_List_Comp'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_List_Comp
   );
   

   List_Gen_F_Expr_For_List_Gen : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => List_Gen_F_Expr

         , Index => 1
   );
   List_Gen_F_Comprehension_For_List_Gen : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => List_Gen_F_Comprehension

         , Index => 2
   );

   Desc_For_List_Gen : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ListGen"),

      Inherited_Fields => 0,
      Fields           => (
            1 => List_Gen_F_Expr_For_List_Gen'Access, 2 => List_Gen_F_Comprehension_For_List_Gen'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_List_Gen
   );
   

   List_Lit_F_Exprs_For_List_Lit : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => List_Lit_F_Exprs

         , Index => 1
   );

   Desc_For_List_Lit : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ListLit"),

      Inherited_Fields => 0,
      Fields           => (
            1 => List_Lit_F_Exprs_For_List_Lit'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_List_Lit
   );
   


   Desc_For_Name : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 2,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 => Common.Dotted_Name_Type_Id, 2 => Common.Id_Type_Id),

      DSL_Name => To_Unbounded_String ("Name"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   

   Dotted_Name_F_Prefix_For_Dotted_Name : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Dotted_Name_F_Prefix

         , Index => 1
   );
   Dotted_Name_F_Suffix_For_Dotted_Name : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Dotted_Name_F_Suffix

         , Index => 2
   );

   Desc_For_Dotted_Name : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Name_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DottedName"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Dotted_Name_F_Prefix_For_Dotted_Name'Access, 2 => Dotted_Name_F_Suffix_For_Dotted_Name'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Dotted_Name
   );
   


   Desc_For_Id : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Name_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Id"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Id
   );
   

   Not_Op_F_Expr_For_Not_Op : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Not_Op_F_Expr

         , Index => 1
   );

   Desc_For_Not_Op : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("NotOp"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Not_Op_F_Expr_For_Not_Op'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Not_Op
   );
   


   Desc_For_Number_Lit : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("NumberLit"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Number_Lit
   );
   

   Or_Expr_F_Left_For_Or_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Or_Expr_F_Left

         , Index => 1
   );
   Or_Expr_F_Right_For_Or_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Or_Expr_F_Right

         , Index => 2
   );

   Desc_For_Or_Expr : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("OrExpr"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Or_Expr_F_Left_For_Or_Expr'Access, 2 => Or_Expr_F_Right_For_Or_Expr'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Or_Expr
   );
   

   Or_Op_F_Left_For_Or_Op : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Or_Op_F_Left

         , Index => 1
   );
   Or_Op_F_Right_For_Or_Op : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Or_Op_F_Right

         , Index => 2
   );

   Desc_For_Or_Op : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("OrOp"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Or_Op_F_Left_For_Or_Op'Access, 2 => Or_Op_F_Right_For_Or_Op'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Or_Op
   );
   

   Power_F_Left_For_Power : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Power_F_Left

         , Index => 1
   );
   Power_F_Right_For_Power : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Power_F_Right

         , Index => 2
   );

   Desc_For_Power : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Power"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Power_F_Left_For_Power'Access, 2 => Power_F_Right_For_Power'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Power
   );
   

   Set_Comp_F_Expr_For_Set_Comp : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Set_Comp_F_Expr

         , Index => 1
   );
   Set_Comp_F_Comprehension_For_Set_Comp : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Set_Comp_F_Comprehension

         , Index => 2
   );

   Desc_For_Set_Comp : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SetComp"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Set_Comp_F_Expr_For_Set_Comp'Access, 2 => Set_Comp_F_Comprehension_For_Set_Comp'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Set_Comp
   );
   

   Set_Lit_F_Exprs_For_Set_Lit : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Set_Lit_F_Exprs

         , Index => 1
   );

   Desc_For_Set_Lit : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SetLit"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Set_Lit_F_Exprs_For_Set_Lit'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Set_Lit
   );
   

   Slice_Expr_F_First_For_Slice_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Slice_Expr_F_First

         , Index => 1
   );
   Slice_Expr_F_Last_For_Slice_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Slice_Expr_F_Last

         , Index => 2
   );

   Desc_For_Slice_Expr : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 1,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 => Common.Ext_Slice_Expr_Type_Id),

      DSL_Name => To_Unbounded_String ("SliceExpr"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Slice_Expr_F_First_For_Slice_Expr'Access, 2 => Slice_Expr_F_Last_For_Slice_Expr'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Slice_Expr
   );
   

   Ext_Slice_Expr_F_Stride_For_Ext_Slice_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Ext_Slice_Expr_F_Stride

         , Index => 3
   );

   Desc_For_Ext_Slice_Expr : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Slice_Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ExtSliceExpr"),

      Inherited_Fields => 2,
      Fields           => (
            1 => Ext_Slice_Expr_F_Stride_For_Ext_Slice_Expr'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Ext_Slice_Expr
   );
   


   Desc_For_String_Lit : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("StringLit"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_String_Lit
   );
   

   Subscript_Expr_F_Prefix_For_Subscript_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Subscript_Expr_F_Prefix

         , Index => 1
   );
   Subscript_Expr_F_Suffix_For_Subscript_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Subscript_Expr_F_Suffix

         , Index => 2
   );

   Desc_For_Subscript_Expr : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SubscriptExpr"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Subscript_Expr_F_Prefix_For_Subscript_Expr'Access, 2 => Subscript_Expr_F_Suffix_For_Subscript_Expr'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Subscript_Expr
   );
   

   Tuple_Lit_F_Exprs_For_Tuple_Lit : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Tuple_Lit_F_Exprs

         , Index => 1
   );

   Desc_For_Tuple_Lit : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("TupleLit"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Tuple_Lit_F_Exprs_For_Tuple_Lit'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Tuple_Lit
   );
   

   Xor_Expr_F_Left_For_Xor_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Xor_Expr_F_Left

         , Index => 1
   );
   Xor_Expr_F_Right_For_Xor_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Xor_Expr_F_Right

         , Index => 2
   );

   Desc_For_Xor_Expr : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("XorExpr"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Xor_Expr_F_Left_For_Xor_Expr'Access, 2 => Xor_Expr_F_Right_For_Xor_Expr'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Xor_Expr
   );
   

   Yield_Expr_F_Exprs_For_Yield_Expr : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Yield_Expr_F_Exprs

         , Index => 1
   );

   Desc_For_Yield_Expr : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Expr_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("YieldExpr"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Yield_Expr_F_Exprs_For_Yield_Expr'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Yield_Expr
   );
   

   File_Node_F_Statements_For_File_Node : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => File_Node_F_Statements

         , Index => 1
   );

   Desc_For_File_Node : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("FileNode"),

      Inherited_Fields => 0,
      Fields           => (
            1 => File_Node_F_Statements_For_File_Node'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_File_Node
   );
   


   Desc_For_Import_Star : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ImportStar"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Import_Star
   );
   


   Desc_For_Kw_Args_Flag : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 2,
      Fields_Count      => 0,
      Properties_Count  => 1,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 => Common.Kw_Args_Flag_Absent_Type_Id, 2 => Common.Kw_Args_Flag_Present_Type_Id),

      DSL_Name => To_Unbounded_String ("KwArgsFlag"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 => Kw_Args_Flag_P_As_Bool
      )

   );
   


   Desc_For_Kw_Args_Flag_Absent : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Kw_Args_Flag_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("KwArgsFlag.Absent"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Kw_Args_Flag_Absent
   );
   


   Desc_For_Kw_Args_Flag_Present : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Kw_Args_Flag_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("KwArgsFlag.Present"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Kw_Args_Flag_Present
   );
   


   Desc_For_NL : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("NL"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_NL
   );
   


   Desc_For_Op : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Op"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Op
   );
   

   Params_F_Single_Params_For_Params : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Params_F_Single_Params

         , Index => 1
   );

   Desc_For_Params : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Params"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Params_F_Single_Params_For_Params'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Params
   );
   

   Rel_Name_F_Dots_For_Rel_Name : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Rel_Name_F_Dots

         , Index => 1
   );
   Rel_Name_F_Name_For_Rel_Name : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Rel_Name_F_Name

         , Index => 2
   );

   Desc_For_Rel_Name : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("RelName"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Rel_Name_F_Dots_For_Rel_Name'Access, 2 => Rel_Name_F_Name_For_Rel_Name'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Rel_Name
   );
   

   Single_Param_F_Is_Varargs_For_Single_Param : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Single_Param_F_Is_Varargs

         , Index => 1
   );
   Single_Param_F_Is_Kwargs_For_Single_Param : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Single_Param_F_Is_Kwargs

         , Index => 2
   );
   Single_Param_F_Name_For_Single_Param : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Single_Param_F_Name

         , Index => 3
   );
   Single_Param_F_Default_Value_For_Single_Param : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Single_Param_F_Default_Value

         , Index => 4
   );

   Desc_For_Single_Param : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 4,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SingleParam"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Single_Param_F_Is_Varargs_For_Single_Param'Access, 2 => Single_Param_F_Is_Kwargs_For_Single_Param'Access, 3 => Single_Param_F_Name_For_Single_Param'Access, 4 => Single_Param_F_Default_Value_For_Single_Param'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Single_Param
   );
   


   Desc_For_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 23,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 => Common.Assert_Stmt_Type_Id, 2 => Common.Assign_Stmt_Type_Id, 3 => Common.Aug_Assign_Stmt_Type_Id, 4 => Common.Break_Stmt_Type_Id, 5 => Common.Continue_Stmt_Type_Id, 6 => Common.Decorated_Type_Id, 7 => Common.Def_Stmt_Type_Id, 8 => Common.Del_Stmt_Type_Id, 9 => Common.Elif_Branch_Type_Id, 10 => Common.Exec_Stmt_Type_Id, 11 => Common.For_Stmt_Type_Id, 12 => Common.Global_Stmt_Type_Id, 13 => Common.If_Stmt_Type_Id, 14 => Common.Import_From_Type_Id, 15 => Common.Import_Name_Type_Id, 16 => Common.Pass_Stmt_Type_Id, 17 => Common.Print_Stmt_Type_Id, 18 => Common.Raise_Stmt_Type_Id, 19 => Common.Return_Stmt_Type_Id, 20 => Common.Stream_Print_Stmt_Type_Id, 21 => Common.Try_Stmt_Type_Id, 22 => Common.While_Stmt_Type_Id, 23 => Common.With_Stmt_Type_Id),

      DSL_Name => To_Unbounded_String ("Stmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   

   Assert_Stmt_F_Test_Expr_For_Assert_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Assert_Stmt_F_Test_Expr

         , Index => 1
   );
   Assert_Stmt_F_Msg_For_Assert_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Assert_Stmt_F_Msg

         , Index => 2
   );

   Desc_For_Assert_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AssertStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Assert_Stmt_F_Test_Expr_For_Assert_Stmt'Access, 2 => Assert_Stmt_F_Msg_For_Assert_Stmt'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Assert_Stmt
   );
   

   Assign_Stmt_F_L_Value_For_Assign_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Assign_Stmt_F_L_Value

         , Index => 1
   );
   Assign_Stmt_F_R_Values_For_Assign_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Assign_Stmt_F_R_Values

         , Index => 2
   );

   Desc_For_Assign_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AssignStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Assign_Stmt_F_L_Value_For_Assign_Stmt'Access, 2 => Assign_Stmt_F_R_Values_For_Assign_Stmt'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Assign_Stmt
   );
   

   Aug_Assign_Stmt_F_L_Value_For_Aug_Assign_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Aug_Assign_Stmt_F_L_Value

         , Index => 1
   );
   Aug_Assign_Stmt_F_Op_For_Aug_Assign_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Aug_Assign_Stmt_F_Op

         , Index => 2
   );
   Aug_Assign_Stmt_F_R_Value_For_Aug_Assign_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Aug_Assign_Stmt_F_R_Value

         , Index => 3
   );

   Desc_For_Aug_Assign_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 3,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AugAssignStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Aug_Assign_Stmt_F_L_Value_For_Aug_Assign_Stmt'Access, 2 => Aug_Assign_Stmt_F_Op_For_Aug_Assign_Stmt'Access, 3 => Aug_Assign_Stmt_F_R_Value_For_Aug_Assign_Stmt'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Aug_Assign_Stmt
   );
   


   Desc_For_Break_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("BreakStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Break_Stmt
   );
   


   Desc_For_Continue_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ContinueStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Continue_Stmt
   );
   

   Decorated_F_Decorators_For_Decorated : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Decorated_F_Decorators

         , Index => 1
   );
   Decorated_F_Defn_For_Decorated : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Decorated_F_Defn

         , Index => 2
   );

   Desc_For_Decorated : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Decorated"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Decorated_F_Decorators_For_Decorated'Access, 2 => Decorated_F_Defn_For_Decorated'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Decorated
   );
   


   Desc_For_Def_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 2,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 => Common.Class_Def_Type_Id, 2 => Common.Func_Def_Type_Id),

      DSL_Name => To_Unbounded_String ("DefStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   

   Class_Def_F_Name_For_Class_Def : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Class_Def_F_Name

         , Index => 1
   );
   Class_Def_F_Bases_For_Class_Def : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Class_Def_F_Bases

         , Index => 2
   );
   Class_Def_F_Statements_For_Class_Def : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Class_Def_F_Statements

         , Index => 3
   );

   Desc_For_Class_Def : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 3,
      Properties_Count  => 0,

      Base_Type   => Common.Def_Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ClassDef"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Class_Def_F_Name_For_Class_Def'Access, 2 => Class_Def_F_Bases_For_Class_Def'Access, 3 => Class_Def_F_Statements_For_Class_Def'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Class_Def
   );
   

   Func_Def_F_Name_For_Func_Def : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Func_Def_F_Name

         , Index => 1
   );
   Func_Def_F_Parameters_For_Func_Def : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Func_Def_F_Parameters

         , Index => 2
   );
   Func_Def_F_Body_For_Func_Def : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Func_Def_F_Body

         , Index => 3
   );

   Desc_For_Func_Def : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 3,
      Properties_Count  => 0,

      Base_Type   => Common.Def_Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("FuncDef"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Func_Def_F_Name_For_Func_Def'Access, 2 => Func_Def_F_Parameters_For_Func_Def'Access, 3 => Func_Def_F_Body_For_Func_Def'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Func_Def
   );
   

   Del_Stmt_F_Exprs_For_Del_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Del_Stmt_F_Exprs

         , Index => 1
   );

   Desc_For_Del_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DelStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Del_Stmt_F_Exprs_For_Del_Stmt'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Del_Stmt
   );
   

   Elif_Branch_F_Cond_Test_For_Elif_Branch : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Elif_Branch_F_Cond_Test

         , Index => 1
   );
   Elif_Branch_F_Statements_For_Elif_Branch : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Elif_Branch_F_Statements

         , Index => 2
   );

   Desc_For_Elif_Branch : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ElifBranch"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Elif_Branch_F_Cond_Test_For_Elif_Branch'Access, 2 => Elif_Branch_F_Statements_For_Elif_Branch'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Elif_Branch
   );
   

   Exec_Stmt_F_Expr_For_Exec_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Exec_Stmt_F_Expr

         , Index => 1
   );
   Exec_Stmt_F_In_List_For_Exec_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Exec_Stmt_F_In_List

         , Index => 2
   );

   Desc_For_Exec_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ExecStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Exec_Stmt_F_Expr_For_Exec_Stmt'Access, 2 => Exec_Stmt_F_In_List_For_Exec_Stmt'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Exec_Stmt
   );
   

   For_Stmt_F_Bindings_For_For_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => For_Stmt_F_Bindings

         , Index => 1
   );
   For_Stmt_F_Expr_For_For_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => For_Stmt_F_Expr

         , Index => 2
   );
   For_Stmt_F_Statements_For_For_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => For_Stmt_F_Statements

         , Index => 3
   );
   For_Stmt_F_Else_Part_For_For_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => For_Stmt_F_Else_Part

         , Index => 4
   );

   Desc_For_For_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 4,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ForStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => For_Stmt_F_Bindings_For_For_Stmt'Access, 2 => For_Stmt_F_Expr_For_For_Stmt'Access, 3 => For_Stmt_F_Statements_For_For_Stmt'Access, 4 => For_Stmt_F_Else_Part_For_For_Stmt'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_For_Stmt
   );
   

   Global_Stmt_F_Names_For_Global_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Global_Stmt_F_Names

         , Index => 1
   );

   Desc_For_Global_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("GlobalStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Global_Stmt_F_Names_For_Global_Stmt'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Global_Stmt
   );
   

   If_Stmt_F_Cond_Test_For_If_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => If_Stmt_F_Cond_Test

         , Index => 1
   );
   If_Stmt_F_Statements_For_If_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => If_Stmt_F_Statements

         , Index => 2
   );
   If_Stmt_F_Elif_Branchs_For_If_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => If_Stmt_F_Elif_Branchs

         , Index => 3
   );
   If_Stmt_F_Else_Part_For_If_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => If_Stmt_F_Else_Part

         , Index => 4
   );

   Desc_For_If_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 4,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("IfStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => If_Stmt_F_Cond_Test_For_If_Stmt'Access, 2 => If_Stmt_F_Statements_For_If_Stmt'Access, 3 => If_Stmt_F_Elif_Branchs_For_If_Stmt'Access, 4 => If_Stmt_F_Else_Part_For_If_Stmt'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_If_Stmt
   );
   

   Import_From_F_Rel_Name_For_Import_From : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Import_From_F_Rel_Name

         , Index => 1
   );
   Import_From_F_Imported_For_Import_From : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Import_From_F_Imported

         , Index => 2
   );

   Desc_For_Import_From : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ImportFrom"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Import_From_F_Rel_Name_For_Import_From'Access, 2 => Import_From_F_Imported_For_Import_From'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Import_From
   );
   

   Import_Name_F_Imported_Names_For_Import_Name : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Import_Name_F_Imported_Names

         , Index => 1
   );

   Desc_For_Import_Name : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ImportName"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Import_Name_F_Imported_Names_For_Import_Name'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Import_Name
   );
   


   Desc_For_Pass_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("PassStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Pass_Stmt
   );
   

   Print_Stmt_F_Exprs_For_Print_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Print_Stmt_F_Exprs

         , Index => 1
   );

   Desc_For_Print_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("PrintStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Print_Stmt_F_Exprs_For_Print_Stmt'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Print_Stmt
   );
   

   Raise_Stmt_F_Exprs_For_Raise_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Raise_Stmt_F_Exprs

         , Index => 1
   );

   Desc_For_Raise_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("RaiseStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Raise_Stmt_F_Exprs_For_Raise_Stmt'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Raise_Stmt
   );
   

   Return_Stmt_F_Exprs_For_Return_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Return_Stmt_F_Exprs

         , Index => 1
   );

   Desc_For_Return_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 1,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ReturnStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Return_Stmt_F_Exprs_For_Return_Stmt'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Return_Stmt
   );
   

   Stream_Print_Stmt_F_Stream_Expr_For_Stream_Print_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Stream_Print_Stmt_F_Stream_Expr

         , Index => 1
   );
   Stream_Print_Stmt_F_Exprs_For_Stream_Print_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Stream_Print_Stmt_F_Exprs

         , Index => 2
   );

   Desc_For_Stream_Print_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("StreamPrintStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Stream_Print_Stmt_F_Stream_Expr_For_Stream_Print_Stmt'Access, 2 => Stream_Print_Stmt_F_Exprs_For_Stream_Print_Stmt'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Stream_Print_Stmt
   );
   

   Try_Stmt_F_Statements_For_Try_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Try_Stmt_F_Statements

         , Index => 1
   );
   Try_Stmt_F_Except_Parts_For_Try_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Try_Stmt_F_Except_Parts

         , Index => 2
   );
   Try_Stmt_F_Else_Part_For_Try_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Try_Stmt_F_Else_Part

         , Index => 3
   );
   Try_Stmt_F_Finally_Part_For_Try_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => Try_Stmt_F_Finally_Part

         , Index => 4
   );

   Desc_For_Try_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 4,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("TryStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => Try_Stmt_F_Statements_For_Try_Stmt'Access, 2 => Try_Stmt_F_Except_Parts_For_Try_Stmt'Access, 3 => Try_Stmt_F_Else_Part_For_Try_Stmt'Access, 4 => Try_Stmt_F_Finally_Part_For_Try_Stmt'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Try_Stmt
   );
   

   While_Stmt_F_Cond_Test_For_While_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => While_Stmt_F_Cond_Test

         , Index => 1
   );
   While_Stmt_F_Statements_For_While_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => While_Stmt_F_Statements

         , Index => 2
   );
   While_Stmt_F_Else_Part_For_While_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => While_Stmt_F_Else_Part

         , Index => 3
   );

   Desc_For_While_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 3,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("WhileStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => While_Stmt_F_Cond_Test_For_While_Stmt'Access, 2 => While_Stmt_F_Statements_For_While_Stmt'Access, 3 => While_Stmt_F_Else_Part_For_While_Stmt'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_While_Stmt
   );
   

   With_Stmt_F_Bindings_For_With_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => With_Stmt_F_Bindings

         , Index => 1
   );
   With_Stmt_F_Statements_For_With_Stmt : aliased constant Node_Field_Descriptor
   := (
      Is_Abstract_Or_Null => False,
      Field               => With_Stmt_F_Statements

         , Index => 2
   );

   Desc_For_With_Stmt : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 2,
      Properties_Count  => 0,

      Base_Type   => Common.Stmt_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("WithStmt"),

      Inherited_Fields => 0,
      Fields           => (
            1 => With_Stmt_F_Bindings_For_With_Stmt'Access, 2 => With_Stmt_F_Statements_For_With_Stmt'Access
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_With_Stmt
   );
   


   Desc_For_Turkixir_Node_Base_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 13,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 => Common.Arg_List_Type_Id, 2 => Common.As_Name_Node_List_Type_Id, 3 => Common.Decorator_List_Type_Id, 4 => Common.Dict_Assoc_List_Type_Id, 5 => Common.Dot_List_Type_Id, 6 => Common.Elif_Branch_List_Type_Id, 7 => Common.Except_Part_List_Type_Id, 8 => Common.Expr_List_Type_Id, 9 => Common.Id_List_Type_Id, 10 => Common.NL_List_Type_Id, 11 => Common.Single_Param_List_Type_Id, 12 => Common.String_Lit_List_Type_Id, 13 => Common.Turkixir_Node_List_Type_Id),

      DSL_Name => To_Unbounded_String ("TurkixirNodeBaseList"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

   );
   


   Desc_For_Arg_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Arg.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Arg_List
   );
   


   Desc_For_As_Name_Node_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("AsNameNode.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_As_Name_Node_List
   );
   


   Desc_For_Decorator_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Decorator.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Decorator_List
   );
   


   Desc_For_Dict_Assoc_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("DictAssoc.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Dict_Assoc_List
   );
   


   Desc_For_Dot_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Dot.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Dot_List
   );
   


   Desc_For_Elif_Branch_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ElifBranch.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Elif_Branch_List
   );
   


   Desc_For_Except_Part_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("ExceptPart.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Except_Part_List
   );
   


   Desc_For_Expr_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Expr.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Expr_List
   );
   


   Desc_For_Id_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("Id.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Id_List
   );
   


   Desc_For_NL_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("NL.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_NL_List
   );
   


   Desc_For_Single_Param_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("SingleParam.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Single_Param_List
   );
   


   Desc_For_String_Lit_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("StringLit.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_String_Lit_List
   );
   


   Desc_For_Turkixir_Node_List : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Turkixir_Node_Base_List_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("TurkixirNode.list"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Turkixir_Node_List
   );
   


   Desc_For_Var_Args_Flag : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => True,
      Derivations_Count => 2,
      Fields_Count      => 0,
      Properties_Count  => 1,

      Base_Type   => Common.Turkixir_Node_Type_Id,
      Derivations =>
         (1 => Common.Var_Args_Flag_Absent_Type_Id, 2 => Common.Var_Args_Flag_Present_Type_Id),

      DSL_Name => To_Unbounded_String ("VarArgsFlag"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 => Var_Args_Flag_P_As_Bool
      )

   );
   


   Desc_For_Var_Args_Flag_Absent : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Var_Args_Flag_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("VarArgsFlag.Absent"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Var_Args_Flag_Absent
   );
   


   Desc_For_Var_Args_Flag_Present : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => False,
      Derivations_Count => 0,
      Fields_Count      => 0,
      Properties_Count  => 0,

      Base_Type   => Common.Var_Args_Flag_Type_Id,
      Derivations =>
         (1 .. 0 => <>),

      DSL_Name => To_Unbounded_String ("VarArgsFlag.Present"),

      Inherited_Fields => 0,
      Fields           => (
            1 .. 0 => <>
      ),

      Properties => (
            1 .. 0 => <>
      )

      , Kind => Turkixir_Var_Args_Flag_Present
   );

   Node_Type_Descriptors : constant
      array (Node_Type_Id) of Node_Type_Descriptor_Access
   := (Desc_For_Turkixir_Node'Access, Desc_For_Arg'Access, Desc_For_Arg_Assoc'Access, Desc_For_Arg_Gen'Access, Desc_For_Kw_Args'Access, Desc_For_Var_Args'Access, Desc_For_As_Name_Node'Access, Desc_For_Comp_If'Access, Desc_For_Comp_Op_Kind'Access, Desc_For_Comp_Op_Kind_Diamond'Access, Desc_For_Comp_Op_Kind_Eq'Access, Desc_For_Comp_Op_Kind_Gt'Access, Desc_For_Comp_Op_Kind_Gte'Access, Desc_For_Comp_Op_Kind_In'Access, Desc_For_Comp_Op_Kind_Is'Access, Desc_For_Comp_Op_Kind_Isnot'Access, Desc_For_Comp_Op_Kind_Lt'Access, Desc_For_Comp_Op_Kind_Lte'Access, Desc_For_Comp_Op_Kind_Noteq'Access, Desc_For_Comp_Op_Kind_Notin'Access, Desc_For_Comprehension'Access, Desc_For_Comp_For'Access, Desc_For_Comp_ForL'Access, Desc_For_Decorator'Access, Desc_For_Dict_Assoc'Access, Desc_For_Else_Part'Access, Desc_For_Except_Part'Access, Desc_For_Expr'Access, Desc_For_And_Expr'Access, Desc_For_And_Op'Access, Desc_For_Bin_Op'Access, Desc_For_Arith_Expr'Access, Desc_For_Shift_Expr'Access, Desc_For_Term'Access, Desc_For_Call_Expr'Access, Desc_For_Comp_Op'Access, Desc_For_Concat_String_Lit'Access, Desc_For_Dict_Comp'Access, Desc_For_Dict_Lit'Access, Desc_For_Dot'Access, Desc_For_Ellipsis_Expr'Access, Desc_For_Factor'Access, Desc_For_If_Expr'Access, Desc_For_Inline_Eval'Access, Desc_For_Lambda_Def'Access, Desc_For_List_Comp'Access, Desc_For_List_Gen'Access, Desc_For_List_Lit'Access, Desc_For_Name'Access, Desc_For_Dotted_Name'Access, Desc_For_Id'Access, Desc_For_Not_Op'Access, Desc_For_Number_Lit'Access, Desc_For_Or_Expr'Access, Desc_For_Or_Op'Access, Desc_For_Power'Access, Desc_For_Set_Comp'Access, Desc_For_Set_Lit'Access, Desc_For_Slice_Expr'Access, Desc_For_Ext_Slice_Expr'Access, Desc_For_String_Lit'Access, Desc_For_Subscript_Expr'Access, Desc_For_Tuple_Lit'Access, Desc_For_Xor_Expr'Access, Desc_For_Yield_Expr'Access, Desc_For_File_Node'Access, Desc_For_Import_Star'Access, Desc_For_Kw_Args_Flag'Access, Desc_For_Kw_Args_Flag_Absent'Access, Desc_For_Kw_Args_Flag_Present'Access, Desc_For_NL'Access, Desc_For_Op'Access, Desc_For_Params'Access, Desc_For_Rel_Name'Access, Desc_For_Single_Param'Access, Desc_For_Stmt'Access, Desc_For_Assert_Stmt'Access, Desc_For_Assign_Stmt'Access, Desc_For_Aug_Assign_Stmt'Access, Desc_For_Break_Stmt'Access, Desc_For_Continue_Stmt'Access, Desc_For_Decorated'Access, Desc_For_Def_Stmt'Access, Desc_For_Class_Def'Access, Desc_For_Func_Def'Access, Desc_For_Del_Stmt'Access, Desc_For_Elif_Branch'Access, Desc_For_Exec_Stmt'Access, Desc_For_For_Stmt'Access, Desc_For_Global_Stmt'Access, Desc_For_If_Stmt'Access, Desc_For_Import_From'Access, Desc_For_Import_Name'Access, Desc_For_Pass_Stmt'Access, Desc_For_Print_Stmt'Access, Desc_For_Raise_Stmt'Access, Desc_For_Return_Stmt'Access, Desc_For_Stream_Print_Stmt'Access, Desc_For_Try_Stmt'Access, Desc_For_While_Stmt'Access, Desc_For_With_Stmt'Access, Desc_For_Turkixir_Node_Base_List'Access, Desc_For_Arg_List'Access, Desc_For_As_Name_Node_List'Access, Desc_For_Decorator_List'Access, Desc_For_Dict_Assoc_List'Access, Desc_For_Dot_List'Access, Desc_For_Elif_Branch_List'Access, Desc_For_Except_Part_List'Access, Desc_For_Expr_List'Access, Desc_For_Id_List'Access, Desc_For_NL_List'Access, Desc_For_Single_Param_List'Access, Desc_For_String_Lit_List'Access, Desc_For_Turkixir_Node_List'Access, Desc_For_Var_Args_Flag'Access, Desc_For_Var_Args_Flag_Absent'Access, Desc_For_Var_Args_Flag_Present'Access);

   ----------------------
   -- Various mappings --
   ----------------------

   package Node_Type_Id_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Node_Type_Id,
      Equivalent_Keys => "=",
      Hash            => Hash);

   DSL_Name_To_Node_Type : Node_Type_Id_Maps.Map;
   --  Lookup table for DSL names to node type references. Created at
   --  elaboration time and never updated after.

   Kind_To_Id : constant array (Turkixir_Node_Kind_Type) of Node_Type_Id := (
      Turkixir_Arg_Assoc => Common.Arg_Assoc_Type_Id, Turkixir_Arg_Gen => Common.Arg_Gen_Type_Id, Turkixir_Kw_Args => Common.Kw_Args_Type_Id, Turkixir_Var_Args => Common.Var_Args_Type_Id, Turkixir_As_Name_Node => Common.As_Name_Node_Type_Id, Turkixir_Comp_If => Common.Comp_If_Type_Id, Turkixir_Comp_Op_Kind_Diamond => Common.Comp_Op_Kind_Diamond_Type_Id, Turkixir_Comp_Op_Kind_Eq => Common.Comp_Op_Kind_Eq_Type_Id, Turkixir_Comp_Op_Kind_Gt => Common.Comp_Op_Kind_Gt_Type_Id, Turkixir_Comp_Op_Kind_Gte => Common.Comp_Op_Kind_Gte_Type_Id, Turkixir_Comp_Op_Kind_In => Common.Comp_Op_Kind_In_Type_Id, Turkixir_Comp_Op_Kind_Is => Common.Comp_Op_Kind_Is_Type_Id, Turkixir_Comp_Op_Kind_Isnot => Common.Comp_Op_Kind_Isnot_Type_Id, Turkixir_Comp_Op_Kind_Lt => Common.Comp_Op_Kind_Lt_Type_Id, Turkixir_Comp_Op_Kind_Lte => Common.Comp_Op_Kind_Lte_Type_Id, Turkixir_Comp_Op_Kind_Noteq => Common.Comp_Op_Kind_Noteq_Type_Id, Turkixir_Comp_Op_Kind_Notin => Common.Comp_Op_Kind_Notin_Type_Id, Turkixir_Comp_For => Common.Comp_For_Type_Id, Turkixir_Comp_ForL => Common.Comp_ForL_Type_Id, Turkixir_Decorator => Common.Decorator_Type_Id, Turkixir_Dict_Assoc => Common.Dict_Assoc_Type_Id, Turkixir_Else_Part => Common.Else_Part_Type_Id, Turkixir_Except_Part => Common.Except_Part_Type_Id, Turkixir_And_Expr => Common.And_Expr_Type_Id, Turkixir_And_Op => Common.And_Op_Type_Id, Turkixir_Arith_Expr => Common.Arith_Expr_Type_Id, Turkixir_Shift_Expr => Common.Shift_Expr_Type_Id, Turkixir_Term => Common.Term_Type_Id, Turkixir_Call_Expr => Common.Call_Expr_Type_Id, Turkixir_Comp_Op => Common.Comp_Op_Type_Id, Turkixir_Concat_String_Lit => Common.Concat_String_Lit_Type_Id, Turkixir_Dict_Comp => Common.Dict_Comp_Type_Id, Turkixir_Dict_Lit => Common.Dict_Lit_Type_Id, Turkixir_Dot => Common.Dot_Type_Id, Turkixir_Ellipsis_Expr => Common.Ellipsis_Expr_Type_Id, Turkixir_Factor => Common.Factor_Type_Id, Turkixir_If_Expr => Common.If_Expr_Type_Id, Turkixir_Inline_Eval => Common.Inline_Eval_Type_Id, Turkixir_Lambda_Def => Common.Lambda_Def_Type_Id, Turkixir_List_Comp => Common.List_Comp_Type_Id, Turkixir_List_Gen => Common.List_Gen_Type_Id, Turkixir_List_Lit => Common.List_Lit_Type_Id, Turkixir_Dotted_Name => Common.Dotted_Name_Type_Id, Turkixir_Id => Common.Id_Type_Id, Turkixir_Not_Op => Common.Not_Op_Type_Id, Turkixir_Number_Lit => Common.Number_Lit_Type_Id, Turkixir_Or_Expr => Common.Or_Expr_Type_Id, Turkixir_Or_Op => Common.Or_Op_Type_Id, Turkixir_Power => Common.Power_Type_Id, Turkixir_Set_Comp => Common.Set_Comp_Type_Id, Turkixir_Set_Lit => Common.Set_Lit_Type_Id, Turkixir_Slice_Expr => Common.Slice_Expr_Type_Id, Turkixir_Ext_Slice_Expr => Common.Ext_Slice_Expr_Type_Id, Turkixir_String_Lit => Common.String_Lit_Type_Id, Turkixir_Subscript_Expr => Common.Subscript_Expr_Type_Id, Turkixir_Tuple_Lit => Common.Tuple_Lit_Type_Id, Turkixir_Xor_Expr => Common.Xor_Expr_Type_Id, Turkixir_Yield_Expr => Common.Yield_Expr_Type_Id, Turkixir_File_Node => Common.File_Node_Type_Id, Turkixir_Import_Star => Common.Import_Star_Type_Id, Turkixir_Kw_Args_Flag_Absent => Common.Kw_Args_Flag_Absent_Type_Id, Turkixir_Kw_Args_Flag_Present => Common.Kw_Args_Flag_Present_Type_Id, Turkixir_NL => Common.NL_Type_Id, Turkixir_Op => Common.Op_Type_Id, Turkixir_Params => Common.Params_Type_Id, Turkixir_Rel_Name => Common.Rel_Name_Type_Id, Turkixir_Single_Param => Common.Single_Param_Type_Id, Turkixir_Assert_Stmt => Common.Assert_Stmt_Type_Id, Turkixir_Assign_Stmt => Common.Assign_Stmt_Type_Id, Turkixir_Aug_Assign_Stmt => Common.Aug_Assign_Stmt_Type_Id, Turkixir_Break_Stmt => Common.Break_Stmt_Type_Id, Turkixir_Continue_Stmt => Common.Continue_Stmt_Type_Id, Turkixir_Decorated => Common.Decorated_Type_Id, Turkixir_Class_Def => Common.Class_Def_Type_Id, Turkixir_Func_Def => Common.Func_Def_Type_Id, Turkixir_Del_Stmt => Common.Del_Stmt_Type_Id, Turkixir_Elif_Branch => Common.Elif_Branch_Type_Id, Turkixir_Exec_Stmt => Common.Exec_Stmt_Type_Id, Turkixir_For_Stmt => Common.For_Stmt_Type_Id, Turkixir_Global_Stmt => Common.Global_Stmt_Type_Id, Turkixir_If_Stmt => Common.If_Stmt_Type_Id, Turkixir_Import_From => Common.Import_From_Type_Id, Turkixir_Import_Name => Common.Import_Name_Type_Id, Turkixir_Pass_Stmt => Common.Pass_Stmt_Type_Id, Turkixir_Print_Stmt => Common.Print_Stmt_Type_Id, Turkixir_Raise_Stmt => Common.Raise_Stmt_Type_Id, Turkixir_Return_Stmt => Common.Return_Stmt_Type_Id, Turkixir_Stream_Print_Stmt => Common.Stream_Print_Stmt_Type_Id, Turkixir_Try_Stmt => Common.Try_Stmt_Type_Id, Turkixir_While_Stmt => Common.While_Stmt_Type_Id, Turkixir_With_Stmt => Common.With_Stmt_Type_Id, Turkixir_Arg_List => Common.Arg_List_Type_Id, Turkixir_As_Name_Node_List => Common.As_Name_Node_List_Type_Id, Turkixir_Decorator_List => Common.Decorator_List_Type_Id, Turkixir_Dict_Assoc_List => Common.Dict_Assoc_List_Type_Id, Turkixir_Dot_List => Common.Dot_List_Type_Id, Turkixir_Elif_Branch_List => Common.Elif_Branch_List_Type_Id, Turkixir_Except_Part_List => Common.Except_Part_List_Type_Id, Turkixir_Expr_List => Common.Expr_List_Type_Id, Turkixir_Id_List => Common.Id_List_Type_Id, Turkixir_NL_List => Common.NL_List_Type_Id, Turkixir_Single_Param_List => Common.Single_Param_List_Type_Id, Turkixir_String_Lit_List => Common.String_Lit_List_Type_Id, Turkixir_Turkixir_Node_List => Common.Turkixir_Node_List_Type_Id, Turkixir_Var_Args_Flag_Absent => Common.Var_Args_Flag_Absent_Type_Id, Turkixir_Var_Args_Flag_Present => Common.Var_Args_Flag_Present_Type_Id
   );

   ------------------
   -- Struct types --
   ------------------

   function Struct_Type_Desc
     (Kind : Struct_Value_Kind) return Struct_Type_Descriptor_Access;
   --  Return the type descriptor corresponding to the given struct type

   function Struct_Field_Name
     (Field : Struct_Field_Reference) return Text_Type;
   --  Helper for Member_Name: take care of structs

   function Struct_Field_Type
     (Field : Struct_Field_Reference) return Type_Constraint;
   --  Helper for Member_Type: take care of structs

   function Struct_Fields
     (Kind : Struct_Value_Kind) return Struct_Field_Reference_Array;
   --  Implementation for Introspection.Struct_Fields

   ----------------
   -- Node types --
   ----------------

   function DSL_Name (Id : Node_Type_Id) return Text_Type;
   --  Implementation for Introspection.DSL_Name

   function Lookup_DSL_Name (Name : Text_Type) return Any_Node_Type_Id;
   --  Implementation for Introspection.Lookup_DSL_Name

   function Is_Abstract (Id : Node_Type_Id) return Boolean;
   --  Implementation for Introspection.Is_Abstract

   function Is_Concrete (Id : Node_Type_Id) return Boolean
   is (not Is_Abstract (Id));

   function Kind_For (Id : Node_Type_Id) return Turkixir_Node_Kind_Type;
   --  Implementation for Introspection.Kind_For

   function First_Kind_For (Id : Node_Type_Id) return Turkixir_Node_Kind_Type;
   --  Implementation for Introspection.First_Kind_For

   function Last_Kind_For (Id : Node_Type_Id) return Turkixir_Node_Kind_Type;
   --  Implementation for Introspection.Last_Kind_For

   function Id_For_Kind (Kind : Turkixir_Node_Kind_Type) return Node_Type_Id;
   --  Implementation for Introspection.Id_For_Kind

   function Is_Root_Node (Id : Node_Type_Id) return Boolean;
   --  Implementation for Introspection.Is_Root_NOde

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id;
   --  Implementation for Introspection.Base_Type

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array;
   --  Implementation for Introspection.Derived_Types

   function Is_Derived_From (Id, Parent : Node_Type_Id) return Boolean;
   --  Implementation for Introspection.Is_Derived_From

   ------------
   -- Member --
   ------------

   function Member_Name (Member : Member_Reference) return Text_Type;
   --  Implementation for Introspection.Member_Name

   function Member_Type (Member : Member_Reference) return Type_Constraint;
   --  Implementation for Introspection.Member_Type

   function Lookup_Member_Struct
     (Kind : Struct_Value_Kind;
      Name : Text_Type) return Any_Member_Reference;
   --  Helper for Introspection.Lookup_Member: take care of struct types

   function Lookup_Member_Node
     (Id   : Node_Type_Id;
      Name : Text_Type) return Any_Member_Reference;
   --  Helper for Introspection.Lookup_Member: take care of nodes

   -------------------
   -- Syntax fields --
   -------------------

   function Syntax_Field_Name
     (Field : Syntax_Field_Reference) return Text_Type;
   --  Helper for Member_Name: take care of syntax fields

   function Syntax_Field_Type
     (Field : Syntax_Field_Reference) return Node_Type_Id;
   --  Helper for Member_Type: take care of syntax fields

   function Eval_Syntax_Field
     (Node  : Bare_Turkixir_Node;
      Field : Syntax_Field_Reference) return Bare_Turkixir_Node;
   --  Implementation for Introspection.Eval_Field

   function Index
     (Kind : Turkixir_Node_Kind_Type; Field : Syntax_Field_Reference) return Positive;
   --  Implementation for Introspection.Index

   function Syntax_Field_Reference_From_Index
     (Kind : Turkixir_Node_Kind_Type; Index : Positive) return Syntax_Field_Reference;
   --  Implementation for Introspection.Syntax_Field_Reference_From_Index

   function Syntax_Fields
     (Id            : Node_Type_Id;
      Concrete_Only : Boolean) return Syntax_Field_Reference_Array;
   --  Return the list of fields associated to ``Id``. If ``Concrete_Only`` is
   --  true, collect only non-null and concrete fields. Otherwise, collect all
   --  fields.

   function Syntax_Fields
     (Kind : Turkixir_Node_Kind_Type) return Syntax_Field_Reference_Array;
   --  Implementation for Introspection.Fields

   function Syntax_Fields
     (Id : Node_Type_Id) return Syntax_Field_Reference_Array;
   --  Implementation for Introspection.Fields

   ----------------
   -- Properties --
   ----------------

   function Property_Name (Property : Property_Reference) return Text_Type;
   --  Helper for Member_Name: take care of properties

   function Property_Return_Type
     (Property : Property_Reference) return Type_Constraint;
   --  Helper for Member_Type: take care of properties

   function Property_Argument_Types
     (Property : Property_Reference) return Type_Constraint_Array;
   --  Implementation for Introspection.Property_Argument_Types

   function Property_Argument_Name
     (Property        : Property_Reference;
      Argument_Number : Positive) return Text_Type;
   --  Implementation for Introspection.Property_Argument_Name

   function Property_Argument_Default_Value
     (Property        : Property_Reference;
      Argument_Number : Positive) return Internal_Value;
   --  Implementation for Introspection.Property_Argument_Default_Value

   function Properties (Kind : Turkixir_Node_Kind_Type) return Property_Reference_Array;
   --  Implementation for Introspection.Properties

   function Properties (Id : Node_Type_Id) return Property_Reference_Array;
   --  Implementation for Introspection.Properties

   procedure Check_Argument_Number
     (Desc : Property_Descriptor; Argument_Number : Positive);
   --  Raise a ``Property_Error`` if ``Argument_Number`` is not valid for the
   --  property that ``Desc`` describes. Do nothing otherwise.


   ------------
   -- Tokens --
   ------------

   function Token_Node_Kind (Kind : Turkixir_Node_Kind_Type) return Token_Kind;
   --  Implementation for Introspection.Token_Node_Kind

end Libturkixirlang.Introspection_Implementation;
