
with Langkit_Support;
with Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;
with Langkit_Support.Text;

package Libturkixirlang is

   Version    : constant String := "undefined";
   Build_Date : constant String := "undefined";

   --  Libturkixirlang's main entry point is the Libturkixirlang.Analysis
   --  package.

   --  Convenience renaming for support package that Langkit provides

   package Support renames Langkit_Support;
   package Diagnostics renames Support.Diagnostics;
   package Slocs renames Support.Slocs;
   package Text renames Support.Text;

end;
