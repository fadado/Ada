------------------------------------------------------------------------------
--  Reading stdin as an iterator
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Exceptions;
with Ada.Text_IO;

with Control; use Control;
with Control.Generators;
with Control.Collectors;
with Control.Junctions;

with Gotcha;

procedure test_io
is
   subtype BUFFER_TYPE is STRING(1..1024);

   package Line_Generator is new Generators (
      Output_Type  => NATURAL,
      Context_Type => BUFFER_TYPE
   );

   package Line_Collector is new Collectors (
      Input_Type   => NATURAL,
      Context_Type => BUFFER_TYPE
   );

   procedure Join is new Junctions.Junction (
      IO_Type           => NATURAL,
      Generator_Package => Line_Generator,
      Generator_Context => BUFFER_TYPE,
      Collector_Package => Line_Collector,
      Collector_Context => BUFFER_TYPE
   );

   use Line_Generator;
   use Line_Collector;

   procedure input_lines
     (generator : in out GENERATOR_INTERFACE'Class;
      bufptr    : access BUFFER_TYPE)
   is
      use Ada.Text_IO;

      buffer : BUFFER_TYPE renames bufptr.all;
      n : NATURAL;
   begin
      while not End_Of_File loop
         Get_Line(buffer, n);
         generator.Yield(n);
      end loop;
   end input_lines;

   procedure output_lines
     (collector : in out COLLECTOR_INTERFACE'Class;
      bufptr    : access BUFFER_TYPE)
   is
      use Ada.Text_IO;

      buffer : BUFFER_TYPE renames bufptr.all;
      n : NATURAL;
   begin
      loop
         n := collector.Yield;
         Put_Line(buffer(1..n));
      end loop;
   end output_lines;

begin
   Gotcha.Set_Handlers;

   declare
      use Ada.Text_IO;
   begin
      declare
         buffer : aliased BUFFER_TYPE;

         stdin  : GENERATOR_TYPE (input_lines'Access,  buffer'Unchecked_Access);
         stdout : COLLECTOR_TYPE (output_lines'Access, buffer'Unchecked_Access);
      begin
         Join(stdin, stdout);
      end;
   end;

exception
   when X : others =>
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_io;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
