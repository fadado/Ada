------------------------------------------------------------------------------
--  Reading stdin as an iterator
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Exceptions;
with Ada.Text_IO;

with Control.Generators; use Control;
with Control.Collectors;

with Gotcha;

procedure test_io
is
   subtype BUFFER_TYPE is STRING(1..1024);

   package IO_G is new Generators (
      Output_Type  => NATURAL,
      Context_Type => BUFFER_TYPE
   );

   package IO_C is new Collectors (
      Input_Type   => NATURAL,
      Context_Type => BUFFER_TYPE
   );

   use IO_G;
   use IO_C;

   procedure input_lines
     (generator : in out GENERATOR_INTERFACE'Class;
      bufptr    : in IO_G.CONTEXT_ACCESS)
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
      bufptr    : in IO_C.CONTEXT_ACCESS)
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
       --generic
       --   with package G_Pack is new Generators (<>);
       --   with package C_Pack is new Collectors (<>);
       --procedure Junction
       --  (generator : G_Pack.GENERATOR_TYPE;
       --   collector : C_Pack.COLLECTOR_TYPE);

       --procedure Junction
       --  (generator : G_Pack.GENERATOR_TYPE;
       --   collector : C_Pack.COLLECTOR_TYPE)
       --is
       --begin
       --   for x of generator loop
       --      collector.Resume(x);
       --   end loop;
       --end Junction;

------------------------------------------------------------------------
         buffer : aliased BUFFER_TYPE;

         stdin  : GENERATOR_TYPE (input_lines'Access,  buffer'Unchecked_Access);
         stdout : COLLECTOR_TYPE (output_lines'Access, buffer'Unchecked_Access);

       --procedure each(x: NATURAL) is
       --begin
       --   stdout.Resume(x);
       --end each;
      begin
       --stdin.For_Each(each'Access);

         for n of stdin loop stdout.Resume(n); end loop;
         stdout.Close;
      end;
   end;

exception
   when X : others =>
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_io;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
