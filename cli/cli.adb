-- cli.adb

with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;

use Ada.Containers;

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

   self_command     : String renames Ada.Command_Line.command_name;
   self_arguments   : Argument_Vector.Vector;
   self_argNames    : Names_Map.map;
   self_words       : Unbounded_String_Vector.Vector;

   self_parsed      : Boolean;
   self_flagCount   : Integer := 0;

   self_usage       : Unbounded_String;
   self_description : Unbounded_String;

------------------------------------------------------------------------

   procedure Print_Help is separate;

   function Command_Name return String is
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
      arg: aliased Argument := (
         arg_short   => arg_short,
         arg_long    => arg_long,
         description => description,
         hasValue    => hasValue,
         value       => +"",
         parsed      => False);
   begin
      self_arguments.append(arg);
      
      -- Set up links.
      if arg_short /= "" then
         self_argNames.include(arg_short, self_arguments.Last_Index);
      end if;

      if arg_long /= "" then
         self_argNames.include(arg_long, self_arguments.Last_Index);
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
      for arg_i in 1 .. Ada.Command_Line.argument_count
      loop
         arg := +Ada.Command_Line.Argument(arg_i);
         -- Each flag will start with a '-' character. Multiple flags can be joined together in
         -- the same string if they're the short form flag type (one character per flag).
         if expectValue then
            -- Copy value.
            self_arguments.Reference(Names_Map.Element(flag_it)).value := arg;    
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
                  raise Long_Flag_Not_Found;
               end if;

               -- Mark as found.
               flag_it := self_argNames.find(arg);
               self_arguments(Names_Map.Element(flag_it)).parsed := True;
               self_flagCount := self_flagCount + 1;

               if self_arguments(Names_Map.Element(flag_it)).hasValue = True then
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
                     raise Long_Flag_Not_Found;
                  end if;

                  flag_it := self_argNames.find(short_arg);

                  -- Mark as found.
                  self_arguments(Names_Map.Element(flag_it)).parsed := True;
                  self_flagCount := self_flagCount + 1;

                  if not self_arguments(Names_Map.Element(flag_it)).hasValue then
                     if i /= (Ada.Strings.Unbounded.Length(arg)) then
                        -- Flag isn't at end, thus cannot have value. Abort.
                        raise Flag_Missing_Argument;
                     else
                        expectValue := True;
                     end if;
                  end if;

                  Ada.Strings.Unbounded.Delete(short_arg, 1, 1);
               end loop;
            end if;  
         else
            -- Add to text argument vector.
            self_words.append(arg);
         end if;
      end loop;

      self_parsed := True;

      return True;
   end Parse_Arguments;

------------------------------------------------------------------------

   function Get_Flag (
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
      elsif not self_arguments(Names_Map.Element(flag_it)).parsed then
         return False;
      end if;

      if self_arguments(Names_Map.Element(flag_it)).hasValue then
         arg_value := self_arguments(Names_Map.Element(flag_it)).value;
      end if;

      return True;
   end Get_Flag;

------------------------------------------------------------------------

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
      end if;
      if not self_arguments(Names_Map.Element(flag_it)).parsed then
         return False;
      end if;
      return True;
   end Exists;

------------------------------------------------------------------------

   function Get_Word(index: in Integer; value: out Unbounded_String)
   return Boolean is
      function length(vector: Unbounded_String_Vector.Vector)
         return Ada.Containers.Count_Type
         renames Unbounded_String_Vector.length;
   begin
      if index < Integer(length(self_words)) then
         value := self_words(index);
         return True;
      end if;
      return False;
   end Get_Word;

------------------------------------------------------------------------

end CLI;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
