
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System;

with Langkit_Support.File_Readers;      use Langkit_Support.File_Readers;
with Langkit_Support.Internal;          use Langkit_Support.Internal;
with Langkit_Support.Internal.Analysis; use Langkit_Support.Internal.Analysis;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Text;              use Langkit_Support.Text;
with Langkit_Support.Types;             use Langkit_Support.Types;

with Libturkixirlang.Common;
with Libturkixirlang.Implementation;
with Libturkixirlang.Generic_Introspection;
use Libturkixirlang.Generic_Introspection;
with Libturkixirlang.Private_Converters; use Libturkixirlang.Private_Converters;
with Libturkixirlang.Public_Converters;  use Libturkixirlang.Public_Converters;

package body Libturkixirlang.Generic_API is

   pragma Warnings (Off, "possible aliasing problem for type");
   function "+" is new Ada.Unchecked_Conversion
     (Internal_Context, Implementation.Internal_Context);
   function "+" is new Ada.Unchecked_Conversion
     (Implementation.Internal_Context, Internal_Context);
   pragma Warnings (On, "possible aliasing problem for type");

   function "+" is new Ada.Unchecked_Conversion
     (Internal_Unit, Implementation.Internal_Unit);
   function "+" is new Ada.Unchecked_Conversion
     (Implementation.Internal_Unit, Internal_Unit);

   function "+" is new Ada.Unchecked_Conversion
     (Internal_Node, Implementation.Bare_Turkixir_Node);
   function "+" is new Ada.Unchecked_Conversion
     (Implementation.Bare_Turkixir_Node, Internal_Node);

   function "+"
     (Entity : Internal_Entity) return Implementation.Internal_Entity;

   function "+" (Rule : Grammar_Rule_Index) return Common.Grammar_Rule
   is (Common.Grammar_Rule'Val (Rule - 1));
   --  Grammar rules start at 1 in the generic API: rebase the value before
   --  converting it to the native type.

   function "+" (Token : Common.Token_Reference) return Internal_Token
   is ((Get_Token_TDH (Token), Get_Token_Index (Token)));

   --  Descriptors for grammar rules

   
      
      Rule_Name_1 : aliased constant Text_Type :=
        "Name";
      
      Rule_Name_2 : aliased constant Text_Type :=
        "Number";
      
      Rule_Name_3 : aliased constant Text_Type :=
        "String";
      
      Rule_Name_4 : aliased constant Text_Type :=
        "Cat_String";
      
      Rule_Name_5 : aliased constant Text_Type :=
        "Nl";
      
      Rule_Name_6 : aliased constant Text_Type :=
        "Main_Rule";
      
      Rule_Name_7 : aliased constant Text_Type :=
        "Decorator";
      
      Rule_Name_8 : aliased constant Text_Type :=
        "Decorators";
      
      Rule_Name_9 : aliased constant Text_Type :=
        "Decorated";
      
      Rule_Name_10 : aliased constant Text_Type :=
        "Func_Def";
      
      Rule_Name_11 : aliased constant Text_Type :=
        "Parameters";
      
      Rule_Name_12 : aliased constant Text_Type :=
        "Varargslist";
      
      Rule_Name_13 : aliased constant Text_Type :=
        "Fpdef";
      
      Rule_Name_14 : aliased constant Text_Type :=
        "Name_List";
      
      Rule_Name_15 : aliased constant Text_Type :=
        "Stmt";
      
      Rule_Name_16 : aliased constant Text_Type :=
        "Simple_Stmt";
      
      Rule_Name_17 : aliased constant Text_Type :=
        "Small_Stmt";
      
      Rule_Name_18 : aliased constant Text_Type :=
        "Expr_Stmt";
      
      Rule_Name_19 : aliased constant Text_Type :=
        "Print_Stmt";
      
      Rule_Name_20 : aliased constant Text_Type :=
        "Del_Stmt";
      
      Rule_Name_21 : aliased constant Text_Type :=
        "Pass_Stmt";
      
      Rule_Name_22 : aliased constant Text_Type :=
        "Flow_Stmt";
      
      Rule_Name_23 : aliased constant Text_Type :=
        "Break_Stmt";
      
      Rule_Name_24 : aliased constant Text_Type :=
        "Continue_Stmt";
      
      Rule_Name_25 : aliased constant Text_Type :=
        "Return_Stmt";
      
      Rule_Name_26 : aliased constant Text_Type :=
        "Yield_Stmt";
      
      Rule_Name_27 : aliased constant Text_Type :=
        "Raise_Stmt";
      
      Rule_Name_28 : aliased constant Text_Type :=
        "Import_Stmt";
      
      Rule_Name_29 : aliased constant Text_Type :=
        "Import_Name";
      
      Rule_Name_30 : aliased constant Text_Type :=
        "Dot";
      
      Rule_Name_31 : aliased constant Text_Type :=
        "Import_From";
      
      Rule_Name_32 : aliased constant Text_Type :=
        "As_Name";
      
      Rule_Name_33 : aliased constant Text_Type :=
        "Dotted_As_Name";
      
      Rule_Name_34 : aliased constant Text_Type :=
        "Import_As_Names";
      
      Rule_Name_35 : aliased constant Text_Type :=
        "Dotted_As_Names";
      
      Rule_Name_36 : aliased constant Text_Type :=
        "Dotted_Name";
      
      Rule_Name_37 : aliased constant Text_Type :=
        "Global_Stmt";
      
      Rule_Name_38 : aliased constant Text_Type :=
        "Exec_Stmt";
      
      Rule_Name_39 : aliased constant Text_Type :=
        "Assert_Stmt";
      
      Rule_Name_40 : aliased constant Text_Type :=
        "Compound_Stmt";
      
      Rule_Name_41 : aliased constant Text_Type :=
        "Else_Part";
      
      Rule_Name_42 : aliased constant Text_Type :=
        "If_Stmt";
      
      Rule_Name_43 : aliased constant Text_Type :=
        "While_Stmt";
      
      Rule_Name_44 : aliased constant Text_Type :=
        "For_Stmt";
      
      Rule_Name_45 : aliased constant Text_Type :=
        "Try_Stmt";
      
      Rule_Name_46 : aliased constant Text_Type :=
        "With_Stmt";
      
      Rule_Name_47 : aliased constant Text_Type :=
        "With_Item";
      
      Rule_Name_48 : aliased constant Text_Type :=
        "Suite";
      
      Rule_Name_49 : aliased constant Text_Type :=
        "Test";
      
      Rule_Name_50 : aliased constant Text_Type :=
        "Or_Test";
      
      Rule_Name_51 : aliased constant Text_Type :=
        "And_Test";
      
      Rule_Name_52 : aliased constant Text_Type :=
        "Not_Test";
      
      Rule_Name_53 : aliased constant Text_Type :=
        "Comparison";
      
      Rule_Name_54 : aliased constant Text_Type :=
        "Expr";
      
      Rule_Name_55 : aliased constant Text_Type :=
        "Xor_Expr";
      
      Rule_Name_56 : aliased constant Text_Type :=
        "And_Expr";
      
      Rule_Name_57 : aliased constant Text_Type :=
        "Shift_Expr";
      
      Rule_Name_58 : aliased constant Text_Type :=
        "Arith_Expr";
      
      Rule_Name_59 : aliased constant Text_Type :=
        "Term";
      
      Rule_Name_60 : aliased constant Text_Type :=
        "Factor";
      
      Rule_Name_61 : aliased constant Text_Type :=
        "Power";
      
      Rule_Name_62 : aliased constant Text_Type :=
        "Atom_Expr";
      
      Rule_Name_63 : aliased constant Text_Type :=
        "Dict_Assoc";
      
      Rule_Name_64 : aliased constant Text_Type :=
        "Yield_Expr";
      
      Rule_Name_65 : aliased constant Text_Type :=
        "Atom";
      
      Rule_Name_66 : aliased constant Text_Type :=
        "Set_Lit";
      
      Rule_Name_67 : aliased constant Text_Type :=
        "Lambdef";
      
      Rule_Name_68 : aliased constant Text_Type :=
        "Subscript_List";
      
      Rule_Name_69 : aliased constant Text_Type :=
        "Subscript";
      
      Rule_Name_70 : aliased constant Text_Type :=
        "Expr_List";
      
      Rule_Name_71 : aliased constant Text_Type :=
        "Test_List";
      
      Rule_Name_72 : aliased constant Text_Type :=
        "Empty_Test_List";
      
      Rule_Name_73 : aliased constant Text_Type :=
        "Class_Def";
      
      Rule_Name_74 : aliased constant Text_Type :=
        "Arg_List";
      
      Rule_Name_75 : aliased constant Text_Type :=
        "List_Iter";
      
      Rule_Name_76 : aliased constant Text_Type :=
        "List_For";
      
      Rule_Name_77 : aliased constant Text_Type :=
        "List_If";
      
      Rule_Name_78 : aliased constant Text_Type :=
        "Comp_Iter";
      
      Rule_Name_79 : aliased constant Text_Type :=
        "Comp_For";
      
      Rule_Name_80 : aliased constant Text_Type :=
        "Comp_If";
   
   Grammar_Rule_Names : aliased constant Grammar_Rule_Name_Array :=
     (1 => Rule_Name_1'Access, 2 => Rule_Name_2'Access, 3 => Rule_Name_3'Access, 4 => Rule_Name_4'Access, 5 => Rule_Name_5'Access, 6 => Rule_Name_6'Access, 7 => Rule_Name_7'Access, 8 => Rule_Name_8'Access, 9 => Rule_Name_9'Access, 10 => Rule_Name_10'Access, 11 => Rule_Name_11'Access, 12 => Rule_Name_12'Access, 13 => Rule_Name_13'Access, 14 => Rule_Name_14'Access, 15 => Rule_Name_15'Access, 16 => Rule_Name_16'Access, 17 => Rule_Name_17'Access, 18 => Rule_Name_18'Access, 19 => Rule_Name_19'Access, 20 => Rule_Name_20'Access, 21 => Rule_Name_21'Access, 22 => Rule_Name_22'Access, 23 => Rule_Name_23'Access, 24 => Rule_Name_24'Access, 25 => Rule_Name_25'Access, 26 => Rule_Name_26'Access, 27 => Rule_Name_27'Access, 28 => Rule_Name_28'Access, 29 => Rule_Name_29'Access, 30 => Rule_Name_30'Access, 31 => Rule_Name_31'Access, 32 => Rule_Name_32'Access, 33 => Rule_Name_33'Access, 34 => Rule_Name_34'Access, 35 => Rule_Name_35'Access, 36 => Rule_Name_36'Access, 37 => Rule_Name_37'Access, 38 => Rule_Name_38'Access, 39 => Rule_Name_39'Access, 40 => Rule_Name_40'Access, 41 => Rule_Name_41'Access, 42 => Rule_Name_42'Access, 43 => Rule_Name_43'Access, 44 => Rule_Name_44'Access, 45 => Rule_Name_45'Access, 46 => Rule_Name_46'Access, 47 => Rule_Name_47'Access, 48 => Rule_Name_48'Access, 49 => Rule_Name_49'Access, 50 => Rule_Name_50'Access, 51 => Rule_Name_51'Access, 52 => Rule_Name_52'Access, 53 => Rule_Name_53'Access, 54 => Rule_Name_54'Access, 55 => Rule_Name_55'Access, 56 => Rule_Name_56'Access, 57 => Rule_Name_57'Access, 58 => Rule_Name_58'Access, 59 => Rule_Name_59'Access, 60 => Rule_Name_60'Access, 61 => Rule_Name_61'Access, 62 => Rule_Name_62'Access, 63 => Rule_Name_63'Access, 64 => Rule_Name_64'Access, 65 => Rule_Name_65'Access, 66 => Rule_Name_66'Access, 67 => Rule_Name_67'Access, 68 => Rule_Name_68'Access, 69 => Rule_Name_69'Access, 70 => Rule_Name_70'Access, 71 => Rule_Name_71'Access, 72 => Rule_Name_72'Access, 73 => Rule_Name_73'Access, 74 => Rule_Name_74'Access, 75 => Rule_Name_75'Access, 76 => Rule_Name_76'Access, 77 => Rule_Name_77'Access, 78 => Rule_Name_78'Access, 79 => Rule_Name_79'Access, 80 => Rule_Name_80'Access);

   --  Descriptors for toen kinds

   
      
      Token_Kind_Name_1 : aliased constant Text_Type :=
        "Termination";
      
      Token_Kind_Name_2 : aliased constant Text_Type :=
        "Lexing_Failure";
      
      Token_Kind_Name_3 : aliased constant Text_Type :=
        "T_T__Rsh_Assign";
      
      Token_Kind_Name_4 : aliased constant Text_Type :=
        "T_T__Is";
      
      Token_Kind_Name_5 : aliased constant Text_Type :=
        "T_T__Equals";
      
      Token_Kind_Name_6 : aliased constant Text_Type :=
        "T_T__Def";
      
      Token_Kind_Name_7 : aliased constant Text_Type :=
        "T_T__Lte";
      
      Token_Kind_Name_8 : aliased constant Text_Type :=
        "T_T__Raise";
      
      Token_Kind_Name_9 : aliased constant Text_Type :=
        "T_T__Mod";
      
      Token_Kind_Name_10 : aliased constant Text_Type :=
        "T_T__Yield";
      
      Token_Kind_Name_11 : aliased constant Text_Type :=
        "T_T__Xor_Assign";
      
      Token_Kind_Name_12 : aliased constant Text_Type :=
        "T_T__As";
      
      Token_Kind_Name_13 : aliased constant Text_Type :=
        "T_T__Lambda";
      
      Token_Kind_Name_14 : aliased constant Text_Type :=
        "T_T__Backtick";
      
      Token_Kind_Name_15 : aliased constant Text_Type :=
        "T_T__Try";
      
      Token_Kind_Name_16 : aliased constant Text_Type :=
        "T_T__Divide";
      
      Token_Kind_Name_17 : aliased constant Text_Type :=
        "T_T__Invert";
      
      Token_Kind_Name_18 : aliased constant Text_Type :=
        "T_T__Return";
      
      Token_Kind_Name_19 : aliased constant Text_Type :=
        "T_T__Assert";
      
      Token_Kind_Name_20 : aliased constant Text_Type :=
        "T_T__Xor";
      
      Token_Kind_Name_21 : aliased constant Text_Type :=
        "T_T__Break";
      
      Token_Kind_Name_22 : aliased constant Text_Type :=
        "T_T__Rbrack";
      
      Token_Kind_Name_23 : aliased constant Text_Type :=
        "T_T__Power_Assign";
      
      Token_Kind_Name_24 : aliased constant Text_Type :=
        "T_T__Import";
      
      Token_Kind_Name_25 : aliased constant Text_Type :=
        "T_T__Exec";
      
      Token_Kind_Name_26 : aliased constant Text_Type :=
        "T_T__Comma";
      
      Token_Kind_Name_27 : aliased constant Text_Type :=
        "T_T_L_Par";
      
      Token_Kind_Name_28 : aliased constant Text_Type :=
        "T_T__Dot";
      
      Token_Kind_Name_29 : aliased constant Text_Type :=
        "T_T__Gte";
      
      Token_Kind_Name_30 : aliased constant Text_Type :=
        "T_T__Floordiv_Assign";
      
      Token_Kind_Name_31 : aliased constant Text_Type :=
        "T_T__Multiply";
      
      Token_Kind_Name_32 : aliased constant Text_Type :=
        "T_T__Div_Assign";
      
      Token_Kind_Name_33 : aliased constant Text_Type :=
        "T_T__At";
      
      Token_Kind_Name_34 : aliased constant Text_Type :=
        "T_T__Assign";
      
      Token_Kind_Name_35 : aliased constant Text_Type :=
        "T_T__Floordiv";
      
      Token_Kind_Name_36 : aliased constant Text_Type :=
        "T_T__Notequal";
      
      Token_Kind_Name_37 : aliased constant Text_Type :=
        "T_T__Mult_Assign";
      
      Token_Kind_Name_38 : aliased constant Text_Type :=
        "T_T__Mod_Assign";
      
      Token_Kind_Name_39 : aliased constant Text_Type :=
        "T_T__Gt";
      
      Token_Kind_Name_40 : aliased constant Text_Type :=
        "T_T__Power";
      
      Token_Kind_Name_41 : aliased constant Text_Type :=
        "T_T__Amp";
      
      Token_Kind_Name_42 : aliased constant Text_Type :=
        "T_T__Not";
      
      Token_Kind_Name_43 : aliased constant Text_Type :=
        "T_T__Colon";
      
      Token_Kind_Name_44 : aliased constant Text_Type :=
        "T_T__Diamond";
      
      Token_Kind_Name_45 : aliased constant Text_Type :=
        "T_T__In";
      
      Token_Kind_Name_46 : aliased constant Text_Type :=
        "T_T_L_Curl";
      
      Token_Kind_Name_47 : aliased constant Text_Type :=
        "T_T__Class";
      
      Token_Kind_Name_48 : aliased constant Text_Type :=
        "T_T__Or_Assign";
      
      Token_Kind_Name_49 : aliased constant Text_Type :=
        "T_T__Elif";
      
      Token_Kind_Name_50 : aliased constant Text_Type :=
        "T_T__And";
      
      Token_Kind_Name_51 : aliased constant Text_Type :=
        "T_T__Semicolon";
      
      Token_Kind_Name_52 : aliased constant Text_Type :=
        "T_T__Add_Asign";
      
      Token_Kind_Name_53 : aliased constant Text_Type :=
        "T_T__Print";
      
      Token_Kind_Name_54 : aliased constant Text_Type :=
        "T_T__Lsh";
      
      Token_Kind_Name_55 : aliased constant Text_Type :=
        "T_T__Continue";
      
      Token_Kind_Name_56 : aliased constant Text_Type :=
        "T_T__While";
      
      Token_Kind_Name_57 : aliased constant Text_Type :=
        "T_T__Except";
      
      Token_Kind_Name_58 : aliased constant Text_Type :=
        "T_T__If";
      
      Token_Kind_Name_59 : aliased constant Text_Type :=
        "T_T__Else";
      
      Token_Kind_Name_60 : aliased constant Text_Type :=
        "T_T__Del";
      
      Token_Kind_Name_61 : aliased constant Text_Type :=
        "T_T__Minus_Assign";
      
      Token_Kind_Name_62 : aliased constant Text_Type :=
        "T_T__Or";
      
      Token_Kind_Name_63 : aliased constant Text_Type :=
        "T_T__Minus";
      
      Token_Kind_Name_64 : aliased constant Text_Type :=
        "T_T__Lbrack";
      
      Token_Kind_Name_65 : aliased constant Text_Type :=
        "T_T__And_Assign";
      
      Token_Kind_Name_66 : aliased constant Text_Type :=
        "T_T_R_Par";
      
      Token_Kind_Name_67 : aliased constant Text_Type :=
        "T_T__Global";
      
      Token_Kind_Name_68 : aliased constant Text_Type :=
        "T_T__For";
      
      Token_Kind_Name_69 : aliased constant Text_Type :=
        "T_T__From";
      
      Token_Kind_Name_70 : aliased constant Text_Type :=
        "T_T__Rsh";
      
      Token_Kind_Name_71 : aliased constant Text_Type :=
        "T_T__Finally";
      
      Token_Kind_Name_72 : aliased constant Text_Type :=
        "T_T__Pass";
      
      Token_Kind_Name_73 : aliased constant Text_Type :=
        "T_T__Lsh_Assign";
      
      Token_Kind_Name_74 : aliased constant Text_Type :=
        "T_T__Bin_Or";
      
      Token_Kind_Name_75 : aliased constant Text_Type :=
        "T_T__Rcurl";
      
      Token_Kind_Name_76 : aliased constant Text_Type :=
        "T_T__With";
      
      Token_Kind_Name_77 : aliased constant Text_Type :=
        "T_T__Plus";
      
      Token_Kind_Name_78 : aliased constant Text_Type :=
        "T_T__Lt";
      
      Token_Kind_Name_79 : aliased constant Text_Type :=
        "T_T__Number";
      
      Token_Kind_Name_80 : aliased constant Text_Type :=
        "T_T__String";
      
      Token_Kind_Name_81 : aliased constant Text_Type :=
        "T_T__Comment";
      
      Token_Kind_Name_82 : aliased constant Text_Type :=
        "T_T__Id";
      
      Token_Kind_Name_83 : aliased constant Text_Type :=
        "Indent";
      
      Token_Kind_Name_84 : aliased constant Text_Type :=
        "Dedent";
      
      Token_Kind_Name_85 : aliased constant Text_Type :=
        "Newline";
   Token_Kind_Names : aliased constant Token_Kind_Name_Array :=
     (1 => Token_Kind_Name_1'Access, 2 => Token_Kind_Name_2'Access, 3 => Token_Kind_Name_3'Access, 4 => Token_Kind_Name_4'Access, 5 => Token_Kind_Name_5'Access, 6 => Token_Kind_Name_6'Access, 7 => Token_Kind_Name_7'Access, 8 => Token_Kind_Name_8'Access, 9 => Token_Kind_Name_9'Access, 10 => Token_Kind_Name_10'Access, 11 => Token_Kind_Name_11'Access, 12 => Token_Kind_Name_12'Access, 13 => Token_Kind_Name_13'Access, 14 => Token_Kind_Name_14'Access, 15 => Token_Kind_Name_15'Access, 16 => Token_Kind_Name_16'Access, 17 => Token_Kind_Name_17'Access, 18 => Token_Kind_Name_18'Access, 19 => Token_Kind_Name_19'Access, 20 => Token_Kind_Name_20'Access, 21 => Token_Kind_Name_21'Access, 22 => Token_Kind_Name_22'Access, 23 => Token_Kind_Name_23'Access, 24 => Token_Kind_Name_24'Access, 25 => Token_Kind_Name_25'Access, 26 => Token_Kind_Name_26'Access, 27 => Token_Kind_Name_27'Access, 28 => Token_Kind_Name_28'Access, 29 => Token_Kind_Name_29'Access, 30 => Token_Kind_Name_30'Access, 31 => Token_Kind_Name_31'Access, 32 => Token_Kind_Name_32'Access, 33 => Token_Kind_Name_33'Access, 34 => Token_Kind_Name_34'Access, 35 => Token_Kind_Name_35'Access, 36 => Token_Kind_Name_36'Access, 37 => Token_Kind_Name_37'Access, 38 => Token_Kind_Name_38'Access, 39 => Token_Kind_Name_39'Access, 40 => Token_Kind_Name_40'Access, 41 => Token_Kind_Name_41'Access, 42 => Token_Kind_Name_42'Access, 43 => Token_Kind_Name_43'Access, 44 => Token_Kind_Name_44'Access, 45 => Token_Kind_Name_45'Access, 46 => Token_Kind_Name_46'Access, 47 => Token_Kind_Name_47'Access, 48 => Token_Kind_Name_48'Access, 49 => Token_Kind_Name_49'Access, 50 => Token_Kind_Name_50'Access, 51 => Token_Kind_Name_51'Access, 52 => Token_Kind_Name_52'Access, 53 => Token_Kind_Name_53'Access, 54 => Token_Kind_Name_54'Access, 55 => Token_Kind_Name_55'Access, 56 => Token_Kind_Name_56'Access, 57 => Token_Kind_Name_57'Access, 58 => Token_Kind_Name_58'Access, 59 => Token_Kind_Name_59'Access, 60 => Token_Kind_Name_60'Access, 61 => Token_Kind_Name_61'Access, 62 => Token_Kind_Name_62'Access, 63 => Token_Kind_Name_63'Access, 64 => Token_Kind_Name_64'Access, 65 => Token_Kind_Name_65'Access, 66 => Token_Kind_Name_66'Access, 67 => Token_Kind_Name_67'Access, 68 => Token_Kind_Name_68'Access, 69 => Token_Kind_Name_69'Access, 70 => Token_Kind_Name_70'Access, 71 => Token_Kind_Name_71'Access, 72 => Token_Kind_Name_72'Access, 73 => Token_Kind_Name_73'Access, 74 => Token_Kind_Name_74'Access, 75 => Token_Kind_Name_75'Access, 76 => Token_Kind_Name_76'Access, 77 => Token_Kind_Name_77'Access, 78 => Token_Kind_Name_78'Access, 79 => Token_Kind_Name_79'Access, 80 => Token_Kind_Name_80'Access, 81 => Token_Kind_Name_81'Access, 82 => Token_Kind_Name_82'Access, 83 => Token_Kind_Name_83'Access, 84 => Token_Kind_Name_84'Access, 85 => Token_Kind_Name_85'Access);

   --  Implementations for generic operations on analysis types

   function Create_Context
     (Charset     : String;
      File_Reader : File_Reader_Reference;
      With_Trivia : Boolean;
      Tab_Stop    : Natural) return Internal_Context;

   procedure Context_Inc_Ref (Context : Internal_Context);
   procedure Context_Dec_Ref (Context : in out Internal_Context);
   function Context_Version (Context : Internal_Context) return Version_Number;
   function Context_Has_Unit
     (Context : Internal_Context; Unit_Filename : String) return Boolean;
   function Context_Get_From_File
     (Context           : Internal_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Rule              : Grammar_Rule_Index) return Internal_Unit;

   function Unit_Version (Unit : Internal_Unit) return Version_Number;
   function Unit_Filename (Unit : Internal_Unit) return String;
   function Unit_Root (Unit : Internal_Unit) return Internal_Node;
   function Unit_First_Token (Unit : Internal_Unit) return Internal_Token;
   function Unit_Last_Token (Unit : Internal_Unit) return Internal_Token;
   function Unit_Get_Line
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type;

   type Internal_Node_Metadata_Type is record
      Ref_Count : Natural;
      Internal  : Implementation.Internal_Metadata;
   end record;
   type Internal_Node_Metadata_Access is
      access all Internal_Node_Metadata_Type;

   function "+" is new Ada.Unchecked_Conversion
     (Internal_Node_Metadata, Internal_Node_Metadata_Access);

   procedure Node_Metadata_Inc_Ref (Metadata : Internal_Node_Metadata);
   procedure Node_Metadata_Dec_Ref (Metadata : in out Internal_Node_Metadata);

   function Node_Parent (Node : Internal_Node) return Internal_Node;
   function Node_Children_Count (Node : Internal_Node) return Natural;
   procedure Node_Get_Child
     (Node            : Internal_Node;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Internal_Node);
   function Node_Fetch_Sibling
     (Node : Internal_Node; Offset : Integer) return Internal_Node;
   function Node_Token_Start (Node : Internal_Node) return Internal_Token;
   function Node_Token_End (Node : Internal_Node) return Internal_Token;

   function Entity_Image (Entity : Internal_Entity) return String;

   ---------
   -- "+" --
   ---------

   function "+"
     (Entity : Internal_Entity) return Implementation.Internal_Entity
   is
      MD : constant Internal_Node_Metadata_Access := +Entity.Metadata;
   begin
      return (Node => +Entity.Node,
              Info => (MD           => (if MD = null
                                        then Implementation.No_Metadata
                                        else MD.Internal),
                       Rebindings   => Entity.Rebindings,
                       From_Rebound => Entity.From_Rebound));
   end "+";

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Charset     : String;
      File_Reader : File_Reader_Reference;
      With_Trivia : Boolean;
      Tab_Stop    : Natural) return Internal_Context
   is
      FR : Implementation.Internal_File_Reader_Access :=
         Wrap_Public_File_Reader (File_Reader);

      Actual_Tab_Stop : constant Positive :=
        (if Tab_Stop = 0
         then 8
         else Tab_Stop);

      Result : constant Implementation.Internal_Context :=
        Implementation.Create_Context
          (Charset        => Charset,
           File_Reader    => FR,
           Event_Handler  => null,
           Unit_Provider  => null,
           With_Trivia    => With_Trivia,
           Tab_Stop       => Actual_Tab_Stop,
           Max_Call_Depth => 1000);
   begin
      return +Result;
   end Create_Context;

   ---------------------
   -- Context_Inc_Ref --
   ---------------------

   procedure Context_Inc_Ref (Context : Internal_Context) is
   begin
      Implementation.Inc_Ref (+Context);
   end Context_Inc_Ref;

   ---------------------
   -- Context_Dec_Ref --
   ---------------------

   procedure Context_Dec_Ref (Context : in out Internal_Context) is
      Ctx : Implementation.Internal_Context := +Context;
   begin
      Implementation.Dec_Ref (Ctx);
      Context := +Ctx;
   end Context_Dec_Ref;

   ---------------------
   -- Context_Version --
   ---------------------

   function Context_Version (Context : Internal_Context) return Version_Number
   is
      Ctx : constant Implementation.Internal_Context := +Context;
   begin
      return Ctx.Serial_Number;
   end Context_Version;

   ----------------------
   -- Context_Has_Unit --
   ----------------------

   function Context_Has_Unit
     (Context : Internal_Context; Unit_Filename : String) return Boolean is
   begin
      return Implementation.Has_Unit (+Context, Unit_Filename);
   end Context_Has_Unit;

   ---------------------------
   -- Context_Get_From_File --
   ---------------------------

   function Context_Get_From_File
     (Context           : Internal_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Rule              : Grammar_Rule_Index) return Internal_Unit
   is
      Ctx : constant Implementation.Internal_Context := +Context;
   begin
      return +Implementation.Get_From_File
        (Ctx, Filename, Charset, Reparse, +Rule);
   end Context_Get_From_File;

   ------------------
   -- Unit_Version --
   ------------------

   function Unit_Version (Unit : Internal_Unit) return Version_Number is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return U.Unit_Version;
   end Unit_Version;

   -------------------
   -- Unit_Filename --
   -------------------

   function Unit_Filename (Unit : Internal_Unit) return String is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return Implementation.Get_Filename (U);
   end Unit_Filename;

   ---------------
   -- Unit_Root --
   ---------------

   function Unit_Root (Unit : Internal_Unit) return Internal_Node is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +U.AST_Root;
   end Unit_Root;

   ----------------------
   -- Unit_First_Token --
   ----------------------

   function Unit_First_Token (Unit : Internal_Unit) return Internal_Token is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +Implementation.First_Token (U);
   end Unit_First_Token;

   ---------------------
   -- Unit_Last_Token --
   ---------------------

   function Unit_Last_Token (Unit : Internal_Unit) return Internal_Token is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +Implementation.Last_Token (U);
   end Unit_Last_Token;

   -------------------
   -- Unit_Get_Line --
   -------------------

   function Unit_Get_Line
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type
   is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return Implementation.Get_Line (U, Line_Number);
   end Unit_Get_Line;

   ---------------------------
   -- Node_Metadata_Inc_Ref --
   ---------------------------

   procedure Node_Metadata_Inc_Ref (Metadata : Internal_Node_Metadata) is
      MD : constant Internal_Node_Metadata_Access := +Metadata;
   begin
      MD.Ref_Count := MD.Ref_Count + 1;
   end Node_Metadata_Inc_Ref;

   ---------------------------
   -- Node_Metadata_Dec_Ref --
   ---------------------------

   procedure Node_Metadata_Dec_Ref (Metadata : in out Internal_Node_Metadata)
   is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Internal_Node_Metadata_Type, Internal_Node_Metadata_Access);
      MD : Internal_Node_Metadata_Access := +Metadata;
   begin
      MD.Ref_Count := MD.Ref_Count - 1;
      if MD.Ref_Count = 0 then
         Destroy (MD);
      end if;
      Metadata := No_Internal_Node_Metadata;
   end Node_Metadata_Dec_Ref;

   -----------------
   -- Node_Parent --
   -----------------

   function Node_Parent (Node : Internal_Node) return Internal_Node is
      N : constant Implementation.Bare_Turkixir_Node := +Node;
   begin
      return +N.Parent;
   end Node_Parent;

   -------------------------
   -- Node_Children_Count --
   -------------------------

   function Node_Children_Count (Node : Internal_Node) return Natural is
      N : constant Implementation.Bare_Turkixir_Node := +Node;
   begin
      return Implementation.Children_Count (N);
   end Node_Children_Count;

   --------------------
   -- Node_Get_Child --
   --------------------

   procedure Node_Get_Child
     (Node            : Internal_Node;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Internal_Node)
   is
      R : Implementation.Bare_Turkixir_Node;
   begin
      Implementation.Get_Child (+Node, Index, Index_In_Bounds, R);
      Result := +R;
   end Node_Get_Child;

   ------------------------
   -- Node_Fetch_Sibling --
   ------------------------

   function Node_Fetch_Sibling
     (Node : Internal_Node; Offset : Integer) return Internal_Node is
   begin
      return +Implementation.Fetch_Sibling (+Node, Offset);
   end Node_Fetch_Sibling;

   ----------------------
   -- Node_Token_Start --
   ----------------------

   function Node_Token_Start (Node : Internal_Node) return Internal_Token is
   begin
      return +Implementation.Token_Start (+Node);
   end Node_Token_Start;

   --------------------
   -- Node_Token_End --
   --------------------

   function Node_Token_End (Node : Internal_Node) return Internal_Token is
   begin
      return +Implementation.Token_End (+Node);
   end Node_Token_End;

   ------------------
   -- Entity_Image --
   ------------------

   function Entity_Image (Entity : Internal_Entity) return String is
   begin
      return Implementation.Image (+Entity);
   end Entity_Image;

   --  Language descriptor table for Libturkixirlang.
   --
   --  We define it here and export its address to avoid making the
   --  $.Generic_API spec (which is public) depend on the
   --  $.Generic_Introspection one (which is private), which allows not
   --  exporting the many symbols from the latter when building a shared
   --  library (Windows has a small limit for the number of exported symbols).

   Language_Name : aliased constant Text_Type :=
     "Turkixir";

   Desc : aliased constant Language_Descriptor :=
     (Language_Name => Language_Name'Access,

      Default_Grammar_Rule => 6,
      Grammar_Rule_Names   => Grammar_Rule_Names'Access,

      Token_Kind_Names => Token_Kind_Names'Access,

      Types          => Generic_Introspection.Types'Access,
      Enum_Types     => Generic_Introspection.Enum_Types'Access,
      Array_Types    => Generic_Introspection.Array_Types'Access,
      Struct_Types   => Generic_Introspection.Struct_Types'Access,
      First_Node     => Generic_Introspection.First_Node,
      Struct_Members => Generic_Introspection.Struct_Members'Access,
      First_Property => Generic_Introspection.First_Property,

      Create_Context        => Create_Context'Access,
      Context_Inc_Ref       => Context_Inc_Ref'Access,
      Context_Dec_Ref       => Context_Dec_Ref'Access,
      Context_Version       => Context_Version'Access,
      Context_Has_Unit      => Context_Has_Unit'Access,
      Context_Get_From_File => Context_Get_From_File'Access,

      Unit_Version     => Unit_Version'Access,
      Unit_Filename    => Unit_Filename'Access,
      Unit_Root        => Unit_Root'Access,
      Unit_First_Token => Unit_First_Token'Access,
      Unit_Last_Token  => Unit_Last_Token'Access,
      Unit_Get_Line    => Unit_Get_Line'Access,

      Node_Metadata_Inc_Ref => Node_Metadata_Inc_Ref'Access,
      Node_Metadata_Dec_Ref => Node_Metadata_Dec_Ref'Access,

      Node_Parent         => Node_Parent'Access,
      Node_Children_Count => Node_Children_Count'Access,
      Node_Get_Child      => Node_Get_Child'Access,
      Node_Fetch_Sibling  => Node_Fetch_Sibling'Access,
      Node_Token_Start    => Node_Token_Start'Access,
      Node_Token_End      => Node_Token_End'Access,

      Entity_Image => Entity_Image'Access);

   Desc_Address : constant System.Address := Desc'Address
     with Export, External_Name => "Libturkixirlang__language_id";

   procedure Dummy is null;

end Libturkixirlang.Generic_API;
