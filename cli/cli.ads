-- cli.ads

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

package CLI is 
   procedure Print_Help;

   procedure Set_Argument (
      short_name  : in STRING;
      long_name   : in STRING;
      description : in STRING;
      valued      : in BOOLEAN
   );

   procedure Set_Description (
      description : in STRING
   ) with Inline;

   procedure Set_Usage (
      usage : in STRING
   ) with Inline;

   procedure  Parse_Arguments;

   Short_Flag_Not_Found  : exception;
   Long_Flag_Not_Found   : exception;
   Flag_Missing_Argument : exception;

   function  Get_Flag (
      name  : in  STRING
   )  return STRING;

   function  Exists (
      name : in STRING
   )  return BOOLEAN;

   function  Get_Word (
      index : in  INTEGER
   )  return STRING;

   function  Flag_Count
      return INTEGER with Inline;

   function  Command_Name
      return STRING with Inline;
end CLI;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
