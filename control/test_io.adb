------------------------------------------------------------------------------
--  Reading stdin as an iterator
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Exceptions;
with Ada.Text_IO;

with Control.Generators; use Control;
with Gotcha;

procedure test_io
is
   subtype BUFFER_TYPE is STRING(1..1024);
   type BUFFER_ACCESS is access all BUFFER_TYPE;

   package IO_types is new Generators (
      Output_Type  => NATURAL,
      Context_Type => BUFFER_TYPE
   );

   use IO_types;

   procedure read_input
     (generator : in out GENERATOR_INTERFACE'Class;
      bufptr    : in CONTEXT_ACCESS)
   is
      use Ada.Text_IO;

      buffer : BUFFER_TYPE renames bufptr.all;
      n : NATURAL;
   begin
      while not End_Of_File loop
         Get_Line(buffer, n);
         generator.Yield(n);
      end loop;
 --begin
 --   loop
 --      Get_Line(buffer, n);
 --      generator.Yield(n);
 --   end loop;
 --exception
 --   when End_Error => null;
   end read_input;

begin
   Gotcha.Set_Handlers;

   declare
      use Ada.Text_IO;
   begin
      declare
         buffer : aliased BUFFER_TYPE;
         stdin  : GENERATOR_TYPE (read_input'Access, buffer'Unchecked_Access);
      begin
         for n of stdin loop
            Put_Line(buffer(1..n));
         end loop;
      end;
   end;

exception
   when X : others =>
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_io;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
