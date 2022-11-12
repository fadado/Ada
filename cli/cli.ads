-- cli.ads

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

package CLI is 
   procedure Print_Help;

   procedure Set_Argument (
      short_name  : in Unbounded_String;
      long_name   : in Unbounded_String;
      description : in Unbounded_String;
      valued      : in Boolean
   );

   procedure Set_Description (
      description : in Unbounded_String
   ) with Inline;

   procedure Set_Usage (
      usage : in Unbounded_String
   ) with Inline;

   procedure  Parse_Arguments;

   Short_Flag_Not_Found  : exception;
   Long_Flag_Not_Found   : exception;
   Flag_Missing_Argument : exception;

   function  Get_Flag (
      name  : in  Unbounded_String;
      value : out Unbounded_String
   )  return Boolean;

   function  Exists (
      name : in Unbounded_String
   )  return Boolean;

   function  Get_Word (
      index : in  Integer;
      value : out Unbounded_String
   )  return Boolean;

   function  Flag_Count
      return Integer with Inline;

   function  Command_Name
      return String with Inline;
end CLI;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
