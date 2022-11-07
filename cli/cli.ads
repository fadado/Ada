-- cli.ads

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

package CLI is 

   procedure Print_Help;

   procedure Set_Argument (
      arg_short   : in Unbounded_String;
      arg_long    : in Unbounded_String;
      description : in Unbounded_String;
      hasValue    : in Boolean
   );

   procedure Set_Description (
      description: in Unbounded_String
   );

   procedure Set_Usage (
      usage: in Unbounded_String
   );

   function  Parse_Arguments
      return Boolean;

   function  Get_Flag (
      arg_flag:  in  Unbounded_String;
      arg_value: out Unbounded_String
   ) return Boolean;

   function  Exists (
      arg_flag: in Unbounded_String
   ) return Boolean;

   function  Get_Text_Argument (
      index: in  Integer;
      value: out Unbounded_String
   ) return Boolean;

   function  Flag_Count
      return Integer;

   function  Command_Name
      return Unbounded_String;

end CLI;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
