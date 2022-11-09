-- cli.adb

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;

use Ada.Text_IO;
use Ada.Containers;
use Ada.Strings.Unbounded.Text_IO;

package body CLI is
------------------------------------------------------------------------

   function "+" (s: String) return Unbounded_String 
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   type Argument is
      record
         arg_short   : aliased Unbounded_String;
         arg_long    : aliased Unbounded_String;
         description : aliased Unbounded_String;
         value       : aliased Unbounded_String;
         hasValue    : aliased Boolean := False;
         parsed      : aliased Boolean := False;
      end record;

   type Argument_Access is access all Argument;

   package Argument_Vector is
      new Vectors(Natural, Argument);

   package Unbounded_String_Vector is
      new Vectors(Natural, Unbounded_String);

   --package Names_Map is new Indefinite_Ordered_Maps(Unbounded_String, Argument_Access);
   package Names_Map is
      new Indefinite_Ordered_Maps(Unbounded_String, Natural);

   self_command     : Unbounded_String;
   self_args        : Argument_Vector.Vector;
   self_argNames    : Names_Map.map;
   self_textArgumen : Unbounded_String_Vector.Vector;

   self_parsed      : Boolean;
   self_flagCount   : Integer := 0;

   self_usage       : Unbounded_String;
   self_description : Unbounded_String;

------------------------------------------------------------------------

   function Command_Name return Unbounded_String is
   begin
      return self_command;
   end Command_Name;

   function Flag_Count return Integer is
   begin
      return self_flagCount;
   end Flag_Count;

------------------------------------------------------------------------

   procedure Set_Argument (
      arg_short   : in Unbounded_String;
      arg_long    : in Unbounded_String;
      description : in Unbounded_String;
      hasValue    : in Boolean
   ) is
      arg: aliased Argument :=
         Argument'(
            arg_short   => arg_short,
            arg_long    => arg_long,
            description => description,
            hasValue    => hasValue,
            value       => +"",
            parsed      => False);
   begin
      self_args.append(arg);
      
      -- Set up links.
      if length(arg_short) > 0 then
         self_argNames.include(arg_short, self_args.Last_Index);
      end if;

      if length(arg_long) > 0 then
         self_argNames.include(arg_long, self_args.Last_Index);
      end if;
   end Set_Argument;

   procedure Set_Description(description: in Unbounded_String) is
   begin
      self_description := description;
   end Set_Description;

   procedure Set_Usage(usage: in Unbounded_String) is
   begin
      self_usage := usage;
   end Set_Usage;

------------------------------------------------------------------------

   function Parse_Arguments
      return Boolean
   is
      flag_it     : Names_Map.Cursor;
      expectValue : Boolean := False;
      arg         : Unbounded_String;
      short_arg   : Unbounded_String;
   begin
      -- 
      self_command := +Ada.Command_Line.command_name;

      for arg_i in 1 .. Ada.Command_Line.argument_count
      loop
         arg := +Ada.Command_Line.Argument(arg_i);
         -- Each flag will start with a '-' character. Multiple flags can be joined together in
         -- the same string if they're the short form flag type (one character per flag).
         if expectValue then
            -- Copy value.
            self_args.Reference(Names_Map.Element(flag_it)).value := arg;    
            expectValue := False;
         elsif Ada.Strings.Unbounded.Slice(arg, 1, 1) = "-" then
            -- Parse flag.
            -- First check for the long form.
            if Ada.Strings.Unbounded.Slice(arg, 1, 2) = "--" then
               -- Long form of the flag.
               -- First delete the preceding dashes.
               arg := Ada.Strings.Unbounded.Delete(arg, 1, 2);
               if not self_argNames.contains(arg) then
                  -- Flag wasn't found. Abort.
                  Ada.Strings.Unbounded.Text_IO.put_line("Long flag " & arg & " wasn't found");
                  return False;
               end if;

               -- Mark as found.
               flag_it := self_argNames.find(arg);
               self_args(Names_Map.Element(flag_it)).parsed := True;
               self_flagCount := self_flagCount + 1;

               if self_args(Names_Map.Element(flag_it)).hasValue = True then
                  expectValue := True;
               end if;
            else
               -- Parse short form flag. Parse all of them sequentially. Only the last one
               -- is allowed to have an additional value following it.
               -- First delete the preceding dash.
               arg := Ada.Strings.Unbounded.Delete(arg, 1, 1);
               for i in 1 .. Ada.Strings.Unbounded.Length(arg) loop
                  Ada.Strings.Unbounded.Append(short_arg, Ada.Strings.Unbounded.Element(arg, i));
                  if not Names_Map.Contains(self_argNames, short_arg) then
                     -- Flag wasn't found. Abort.
                     put_line("Short flag " & short_arg & " wasn't found.");
                     return False;
                  end if;

                  flag_it := self_argNames.find(short_arg);

                  -- Mark as found.
                  self_args(Names_Map.Element(flag_it)).parsed := True;
                  self_flagCount := self_flagCount + 1;

                  if not self_args(Names_Map.Element(flag_it)).hasValue then
                     if i /= (Ada.Strings.Unbounded.Length(arg)) then
                        -- Flag isn't at end, thus cannot have value.
                        put_line("Flag " & short_arg & " needs to be followed by a value string.");
                        return False;
                     else
                        expectValue := True;
                     end if;
                  end if;

                  Ada.Strings.Unbounded.Delete(short_arg, 1, 1);
               end loop;
            end if;  
         else
            -- Add to text argument vector.
            self_textArgumen.append(arg);
         end if;
      end loop;

      self_parsed := True;

      return True;
   end Parse_Arguments;

------------------------------------------------------------------------

   function  Get_Flag (
      arg_flag    :  in Unbounded_String;
      arg_value   : out Unbounded_String
   ) return Boolean
   is
      flag_it: Names_Map.Cursor;
      use Names_Map;
   begin
      if not self_parsed then
         return False;
      end if;

      flag_it := self_argNames.find(arg_flag);
      if flag_it = Names_Map.No_Element then
         return False;
      elsif not self_args(Names_Map.Element(flag_it)).parsed then
         return False;
      end if;

      if self_args(Names_Map.Element(flag_it)).hasValue then
         arg_value := self_args(Names_Map.Element(flag_it)).value;
      end if;

      return True;
   end Get_Flag;

   function Exists (
      arg_flag : in Unbounded_String
   ) return Boolean is
      flag_it : Names_Map.Cursor;
      use Names_Map;
   begin
      if not self_parsed then
         return False;
      end if;

      flag_it := self_argNames.find(arg_flag);
      if flag_it = Names_Map.No_Element then
         return False;
      elsif not self_args(Names_Map.Element(flag_it)).parsed then
         return False;
      end if;

      return True;
   end Exists;

   function  Get_Text_Argument (
      index : in  Integer;
      value : out Unbounded_String
   ) return Boolean is
   begin
      if index < Integer(Unbounded_String_Vector.length(self_textArgumen)) then
         value := self_textArgumen(index);
         return True;
      else
         return False;
      end if;
   end Get_Text_Argument;

------------------------------------------------------------------------

   procedure Print_Help is
      count    : Integer := 1;
      spaceCnt : Integer;
   begin
      new_line;
      put_line(self_description);
      put_line("Usage:");
      put_line(self_usage);
      new_line;
      put_line("Options:");

      -- Determine whitespace needed between arg_long and description.
      for flag in self_args.Iterate loop
         if Integer(Ada.Strings.Unbounded.length(self_args(flag).arg_long)) > count then
            count := Integer(Ada.Strings.Unbounded.length(self_args(flag).arg_long));
         end if;
      end loop;

      count := count + 3; -- Number of actual spaces between the longest arg_long and description.

      -- Print out the options.
      for opt in self_args.Iterate loop
         --spaceStr := Unbound_String(count - Ada.Strings.Unbounded.length(self_args(opt).arg_long)
         spaceCnt := (count - Integer(Ada.Strings.Unbounded.length(self_args(opt).arg_long)));
         if Ada.Strings.Unbounded.length(self_args(opt).arg_short) < 1 then
            Ada.Strings.Unbounded.Text_IO.put_line("    " & self_args(opt).arg_short 
            & "--" & self_args(opt).arg_long 
            & spaceCnt * " " & self_args(opt).description);
         else
            Ada.Strings.Unbounded.Text_IO.put_line("-" & self_args(opt).arg_short 
            & ", --" & self_args(opt).arg_long 
            & spaceCnt * " " & self_args(opt).description);
         end if;
      end loop;
   end Print_Help;

end CLI;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
