-- cli.adb

with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;

package body CLI is
------------------------------------------------------------------------

   function "+" (s: String) return Unbounded_String 
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   type T_Argument is
      record
         short_name  : aliased Unbounded_String;
         long_name   : aliased Unbounded_String;
         description : aliased Unbounded_String;
         value       : aliased Unbounded_String;
         valued      : aliased Boolean := False;
         parsed      : aliased Boolean := False;
      end record;

   package Arguments is new Ada.Containers.Vectors(Natural, T_Argument);
   package UStrings  is new Ada.Containers.Vectors(Natural, Unbounded_String);
   package UString2Natural is
      new Ada.Containers.Indefinite_Ordered_Maps(Unbounded_String, Natural);

   self_command     : String renames Ada.Command_Line.command_name;
   self_arguments   : Arguments.Vector;
   self_flags       : UString2Natural.Map;
   self_words       : UStrings.Vector;

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
      short_name  : in Unbounded_String;
      long_name   : in Unbounded_String;
      description : in Unbounded_String;
      valued      : in Boolean
   ) is
      arg: aliased T_Argument := (
         short_name  => short_name,
         long_name   => long_name,
         description => description,
         valued      => valued,
         value       => +"",
         parsed      => False);
   begin
      self_arguments.Append(arg);
      
      -- Set up links.
      if short_name /= "" then
         self_flags.Include(short_name, self_arguments.Last_Index);
      end if;

      if long_name /= "" then
         self_flags.Include(long_name, self_arguments.Last_Index);
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

   procedure Parse_Arguments
   is
      flag_it     : UString2Natural.Cursor;
      expectValue : Boolean := False;
      arg         : Unbounded_String;
      short_arg   : Unbounded_String;
      --
      package US renames Ada.Strings.Unbounded;
   begin
      -- 
      for arg_i in 1 .. Ada.Command_Line.argument_count
      loop
         -- Each flag will start with a '-' character. Multiple flags can be
         -- joined together in the same string if they're the short form flag
         -- type (one character per flag).

         arg := +Ada.Command_Line.Argument(arg_i);

         if expectValue then
            -- Copy value.
            self_arguments.Reference(UString2Natural.Element(flag_it)).value := arg;    
            expectValue := False;

         elsif US.Slice(arg, 1, 1) /= "-" then
            -- Add to text argument vector.
            self_words.append(arg);

         else
            -- Parse flag.
            -- First check for the long form.
            if US.Slice(arg, 1, 2) = "--" then
               -- Long form of the flag.
               -- First delete the preceding dashes.
               arg := US.Delete(arg, 1, 2);
               if not self_flags.Contains(arg) then
                  -- Flag wasn't found. Abort.
                  raise Long_Flag_Not_Found;
               end if;

               -- Mark as found.
               flag_it := self_flags.Find(arg);
               self_arguments(UString2Natural.Element(flag_it)).parsed := True;
               self_flagCount := @ + 1;

               if self_arguments(UString2Natural.Element(flag_it)).valued then
                  expectValue := True;
               end if;
            else
               -- Parse short form flag. Parse all of them sequentially. Only
               -- the last one is allowed to have an additional value following
               -- it.  First delete the preceding dash.
               arg := US.Delete(arg, 1, 1);
               for i in 1 .. US.Length(arg) loop
                  US.Append(short_arg, US.Element(arg, i));
                  if not self_flags.Contains(short_arg) then
                     -- Flag wasn't found. Abort.
                     raise Long_Flag_Not_Found;
                  end if;

                  flag_it := self_flags.Find(short_arg);

                  -- Mark as found.
                  self_arguments(UString2Natural.Element(flag_it)).parsed := True;
                  self_flagCount := @ + 1;

                  if not self_arguments(UString2Natural.Element(flag_it)).valued then
                     if i /= (US.Length(arg)) then
                        -- Flag isn't at end, thus cannot have value. Abort.
                        raise Flag_Missing_Argument;
                     else
                        expectValue := True;
                     end if;
                  end if;

                  US.Delete(short_arg, 1, 1);
               end loop;
            end if;  
         end if;
      end loop;

      self_parsed := True;
   end Parse_Arguments;

------------------------------------------------------------------------

   function Get_Flag (
      name  :  in Unbounded_String;
      value : out Unbounded_String
   ) return Boolean
   is
      flag_it : UString2Natural.Cursor;
   begin
      if not self_parsed then
         return False;
      end if;

      flag_it := self_flags.Find(name);
      if UString2Natural."="(flag_it, UString2Natural.No_Element) then
         return False;
      elsif not self_arguments(UString2Natural.Element(flag_it)).parsed then
         return False;
      end if;

      if self_arguments(UString2Natural.Element(flag_it)).valued then
         value := self_arguments(UString2Natural.Element(flag_it)).value;
      end if;

      return True;
   end Get_Flag;

------------------------------------------------------------------------

   function Exists (
      name : in Unbounded_String
   ) return Boolean is
      flag_it : UString2Natural.Cursor;
   begin
      if not self_parsed then
         return False;
      end if;

      flag_it := self_flags.Find(name);
      if UString2Natural."="(flag_it, UString2Natural.No_Element) then
         return False;
      end if;
      if not self_arguments(UString2Natural.Element(flag_it)).parsed then
         return False;
      end if;
      return True;
   end Exists;

------------------------------------------------------------------------

   function Get_Word(index: in Integer; value: out Unbounded_String)
   return Boolean is
      function length(vector: UStrings.Vector)
         return Ada.Containers.Count_Type
         renames UStrings.Length;
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
