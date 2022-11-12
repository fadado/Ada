-- cli-print_help.adb

with Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;

separate (CLI)
procedure Print_Help is
   -- local objects
   count  : Integer := 1;
   spaces : Integer;

   -- aliased subprograms
   procedure print(str: Ada.Strings.Unbounded.Unbounded_String)
      renames Ada.Strings.Unbounded.Text_IO.put_line;
   procedure print(str: String)
      renames Ada.Text_IO.put_line;
   procedure print with Inline is
   begin
      Ada.Text_IO.new_line;
   end;
   function length(str: Unbounded_String) return Integer with Inline is
   begin
      return Integer(Ada.Strings.Unbounded.Length(str));
   end;
begin
   print;
   print(self_description);
   print("Usage:");
   print(self_usage);
   print;
   print("Options:");

   -- Determine whitespace needed between long_name and description.
   for flag in self_arguments.Iterate loop
      declare
         long_name: Unbounded_String renames self_arguments(flag).long_name;
      begin
         if length(long_name) > count then
            count := length(long_name);
         end if;
      end;
   end loop;

   -- Number of actual spaces between the longest long_name and description.
   count := count + 3;

   -- Print out the options.
   for opt in self_arguments.Iterate loop
      declare
         long_name: Unbounded_String renames self_arguments(opt).long_name;
         short_name: Unbounded_String renames self_arguments(opt).short_name;
         description: Unbounded_String renames self_arguments(opt).description;
      begin
         spaces := (count - length(long_name));
         if length(short_name) < 1 then
            print("    " & short_name & "--" & long_name & spaces*" " & description);
         else
            print("-" & short_name & ", --" & long_name & spaces*" " & description);
         end if;
      end;
   end loop;
end Print_Help;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
