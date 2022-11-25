-- cli.ads

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

package CLI is 
   subtype VSTRING is Ada.Strings.Unbounded.Unbounded_String;

   procedure Print_Help;

   procedure Set_Argument (
      short_name  : in VSTRING;
      long_name   : in VSTRING;
      description : in VSTRING;
      valued      : in BOOLEAN
   );

   procedure Set_Description (
      description : in VSTRING
   ) with Inline;

   procedure Set_Usage (
      usage : in VSTRING
   ) with Inline;

   procedure  Parse_Arguments;

   Short_Flag_Not_Found  : exception;
   Long_Flag_Not_Found   : exception;
   Flag_Missing_Argument : exception;

   function  Get_Flag (
      name  : in  VSTRING;
      value : out VSTRING
   )  return BOOLEAN;

   function  Exists (
      name : in VSTRING
   )  return BOOLEAN;

   function  Get_Word (
      index : in  INTEGER;
      value : out VSTRING
   )  return BOOLEAN;

   function  Flag_Count
      return INTEGER with Inline;

   function  Command_Name
      return STRING with Inline;
end CLI;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
