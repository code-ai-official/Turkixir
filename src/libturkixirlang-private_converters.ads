
with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with Libturkixirlang.Common;         use Libturkixirlang.Common;
with Libturkixirlang.Implementation; use Libturkixirlang.Implementation;

--  Internal package: provide implementation helpers to switch between public
--  types and implementation ones.

private package Libturkixirlang.Private_Converters is

   type Token_Reference_Wrapper is access function
     (Context : Internal_Context;
      TDH     : Token_Data_Handler_Access;
      Index   : Token_Or_Trivia_Index) return Token_Reference;
   Wrap_Token_Reference : Token_Reference_Wrapper;

   type Token_Context_Getter is access function
     (Token : Token_Reference) return Internal_Context;
   Get_Token_Context : Token_Context_Getter;

   type Token_TDH_Getter is access function
     (Token : Token_Reference) return Token_Data_Handler_Access;
   Get_Token_TDH : Token_TDH_Getter;

   type Token_Index_Getter is access function
     (Token : Token_Reference) return Token_Or_Trivia_Index;
   Get_Token_Index : Token_Index_Getter;

   type Token_Text_Extractor is access procedure
     (Token         : Token_Data_Type;
      Source_Buffer : out Text_Cst_Access;
      First         : out Positive;
      Last          : out Natural);
   Extract_Token_Text : Token_Text_Extractor;

end Libturkixirlang.Private_Converters;
