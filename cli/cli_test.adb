-- cli_test.ads

with CLI;

with Ada.Text_IO,
     Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Text_IO;
use  Ada.Text_IO,
     Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Text_IO;

procedure CLI_TEST is

   function "+" (s: String) return Unbounded_String 
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   kittens  : Unbounded_String;
   number   : Unbounded_String;
   textarg  : Unbounded_String;

   isOption : constant Boolean := True;
   isFlag   : constant Boolean := False;

begin

   CLI.Set_Argument(+"h", +"help",    +"Get help.", isFlag);
   CLI.Set_Argument(+"k", +"kittens", +"K is for <kittens>. Everyone needs kittens in their life.", isOption);
   CLI.Set_Argument(+"n", +"number",  +"Gimme a <number>. Any number.", isOption);
   CLI.Set_Argument(+"a", +"apple",   +"Just an apple.", isFlag);
   CLI.Set_Argument(+"b", +"bear",    +"Look, it's a bear.", isFlag);
   CLI.Set_Argument(+"",  +"snake",   +"Snakes only come in long form, there are no short snakes.", isFlag);

   CLI.Set_Description(+"CLI command line argument parsing testing app. For demonstration purposes and testing.");

   CLI.Set_Usage(+"CLI_TEST <options>");

   if not CLI.Parse_Arguments then
      put_line("Couldn't parse arguments...");
      return;
   end if;

   put_line("Command name: " & CLI.Command_Name);

   put_line("Number of flags found: " & CLI.Flag_Count'Image);

   if CLI.Exists(+"help") then
      CLI.Print_Help;
   else
      put_line("No help requested...");
   end if;

   -- Read out Kittens and Number.
   if CLI.Get_Flag(+"kittens", kittens) then
      put_line("Got kittens: " & kittens);
   end if;

   if CLI.Get_Flag(+"number", number) then
      put_line("Got number: " & number);
   end if;

   if CLI.Get_Word(0, textarg) then
      put_line("Got text argument: " & textarg);
   end if;

end CLI_TEST;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
