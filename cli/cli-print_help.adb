-- cli-print_help.adb

with Ada.Text_IO,
     Ada.Strings.Unbounded.Text_IO;

separate (CLI) procedure Print_Help is
   -- aliased names
   package STR renames Ada.Strings.Unbounded;
   procedure print(s:STR.Unbounded_String) renames STR.Text_IO.put_line;
   procedure print(s:String) renames Ada.Text_IO.put_line;
   -- local objects
   count  : Integer := 1;
   spaces : Integer;
   -- bodies (after objects!)
   procedure new_line with Inline is begin Ada.Text_IO.new_line; end;
begin
   new_line;
   print(self_description);
   print("Usage:");
   print(self_usage);
   new_line;
   print("Options:");

   -- Determine whitespace needed between arg_long and description.
   for flag in self_arguments.Iterate loop
      if Integer(STR.length(self_arguments(flag).arg_long)) > count then
         count := Integer(STR.length(self_arguments(flag).arg_long));
      end if;
   end loop;

   -- Number of actual spaces between the longest arg_long and description.
   count := count + 3;

   -- Print out the options.
   for opt in self_arguments.Iterate loop
      spaces := (count - Integer(STR.length(self_arguments(opt).arg_long)));
      if STR.length(self_arguments(opt).arg_short) < 1 then
         print("    " & self_arguments(opt).arg_short 
               & "--" & self_arguments(opt).arg_long 
               & spaces*" " & self_arguments(opt).description);
      else
         print("-" & self_arguments(opt).arg_short 
               & ", --" & self_arguments(opt).arg_long 
               & spaces*" " & self_arguments(opt).description);
      end if;
   end loop;
end Print_Help;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
