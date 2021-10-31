
with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;

package Libturkixirlang.Generic_API is

   

   Turkixir_Lang_Id : constant Language_Id
     with Import, External_Name => "Libturkixirlang__language_id";
   --  Unique identifier for Libturkixirlang

private

   procedure Dummy;
   --  Dummy procedure so that this spec is allowed to have a body. See the
   --  body's hack to provide the Language_Id constant.

end Libturkixirlang.Generic_API;
