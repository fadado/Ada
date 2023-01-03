-- cli.adb

with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;

package body CLI is
   type HITCH is access STRING;

   function "="(s, t: HITCH) return BOOLEAN with Inline is
   begin
      return s.all = t.all;
   end;

   subtype USTRING is Ada.Strings.Unbounded.Unbounded_String;
------------------------------------------------------------------------

   function "+" (s: STRING) return USTRING 
      renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (s: USTRING) return STRING 
      renames Ada.Strings.Unbounded.To_String;

   type ARGUMENT is
      record
         short_name  : aliased USTRING;
         long_name   : aliased USTRING;
         description : aliased USTRING;
         value       : aliased USTRING;
         valued      : aliased BOOLEAN := FALSE;
         parsed      : aliased BOOLEAN := FALSE;
      end record;

   package Arguments is 
      new Ada.Containers.Vectors(NATURAL, ARGUMENT);
   package UStrings is
      new Ada.Containers.Vectors(NATURAL, USTRING);
   package UString2Natural is
      new Ada.Containers.Indefinite_Ordered_Maps(USTRING, NATURAL);

   self_command     : STRING renames Ada.Command_Line.command_name;
   self_arguments   : Arguments.VECTOR;
   self_flags       : UString2Natural.MAP;
   self_words       : UStrings.VECTOR;

   self_parsed      : BOOLEAN;
   self_flagCount   : INTEGER := 0;

   self_usage       : USTRING;
   self_description : USTRING;

------------------------------------------------------------------------

   procedure Print_Help is separate;

   function Command_Name return STRING is
   begin
      return self_command;
   end Command_Name;

   function Flag_Count return INTEGER is
   begin
      return self_flagCount;
   end Flag_Count;

------------------------------------------------------------------------

   procedure Set_Argument (
      short_name  : in STRING;
      long_name   : in STRING;
      description : in STRING;
      valued      : in BOOLEAN
   ) is
      arg: aliased ARGUMENT := (
         short_name  => +short_name,
         long_name   => +long_name,
         description => +description,
         valued      => valued,
         value       => +"",
         parsed      => FALSE);
   begin
      self_arguments.Append(arg);
      
      -- Set up links.
      if short_name /= "" then
         self_flags.Include(+short_name, self_arguments.Last_Index);
      end if;

      if long_name /= "" then
         self_flags.Include(+long_name, self_arguments.Last_Index);
      end if;
   end Set_Argument;

   procedure Set_Description(description: in STRING) is
   begin
      self_description := +description;
   end Set_Description;

   procedure Set_Usage(usage: in STRING) is
   begin
      self_usage := +usage;
   end Set_Usage;

------------------------------------------------------------------------

   procedure Parse_Arguments
   is
      flag_it     : UString2Natural.CURSOR;
      expectValue : BOOLEAN := FALSE;
      arg         : USTRING;
      short_arg   : USTRING;
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
            expectValue := FALSE;

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
               self_arguments(UString2Natural.Element(flag_it)).parsed := TRUE;
               self_flagCount := @ + 1;

               if self_arguments(UString2Natural.Element(flag_it)).valued then
                  expectValue := TRUE;
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
                  self_arguments(UString2Natural.Element(flag_it)).parsed := TRUE;
                  self_flagCount := @ + 1;

                  if not self_arguments(UString2Natural.Element(flag_it)).valued then
                     if i /= (US.Length(arg)) then
                        -- Flag isn't at end, thus cannot have value. Abort.
                        raise Flag_Missing_Argument;
                     else
                        expectValue := TRUE;
                     end if;
                  end if;

                  US.Delete(short_arg, 1, 1);
               end loop;
            end if;  
         end if;
      end loop;

      self_parsed := TRUE;
   end Parse_Arguments;

------------------------------------------------------------------------

   function Get_Flag (
      name  :  in STRING
   ) return STRING
   is
      use type UString2Natural.CURSOR;
      flag_it : UString2Natural.CURSOR;
   begin
      if not self_parsed then
         return "";
      end if;

      flag_it := self_flags.Find(+name);
      if flag_it = UString2Natural.No_Element then
         return "";
      elsif not self_arguments(UString2Natural.Element(flag_it)).parsed then
         return "";
      end if;

      if self_arguments(UString2Natural.Element(flag_it)).valued then
         return -self_arguments(UString2Natural.Element(flag_it)).value;
      end if;

      return "";
   end Get_Flag;

------------------------------------------------------------------------

   function Exists (name : in STRING)
   return BOOLEAN is
      use type UString2Natural.CURSOR;
      flag_it : UString2Natural.CURSOR;
   begin
      if not self_parsed then
         return FALSE;
      end if;

      flag_it := self_flags.Find(+name);
      if flag_it = UString2Natural.No_Element then
         return FALSE;
      end if;
      if not self_arguments(UString2Natural.Element(flag_it)).parsed then
         return FALSE;
      end if;
      return TRUE;
   end Exists;

------------------------------------------------------------------------

   function Get_Word(index: in INTEGER)
   return STRING is
      function length(vector: UStrings.VECTOR)
         return Ada.Containers.COUNT_TYPE
         renames UStrings.Length;
   begin
      if index < INTEGER(length(self_words)) then
         return -self_words(index);
      else
         return "";
      end if;
   end Get_Word;

------------------------------------------------------------------------

end CLI;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
