------------------------------------------------------------------------------
--  Reading stdin as an iterator
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Exceptions;
with Ada.Text_IO;

with Control; use Control;
with Control.Generators;
with Control.Collectors;
with Control.Functors;
with Control.Junctions;

with Gotcha;

procedure test_io
is
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   use Junctions;

   subtype BUFFER_TYPE is STRING(1..1024);

   package Natural_Joint is new Joint_Signature (
      Element_Type   => NATURAL,
      Source_Context => BUFFER_TYPE,
      Target_Context => BUFFER_TYPE
   );

   package Line_Generator is new Generators (
      Element_Type  => NATURAL,
      Context_Type => BUFFER_TYPE
   );

   package Line_Collector is new Collectors (
      Element_Type => NATURAL,
      Context_Type => BUFFER_TYPE
   );

   use Line_Generator;
   use Line_Collector;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   procedure source_lines
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
   end source_lines;

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
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   Gotcha.Set_Handlers;

   declare
      use Ada.Text_IO;
   begin
      declare
         procedure Copy is new Joint (
            Natural_Joint,
            Line_Generator,
            Line_Collector
         );

         buffer : aliased BUFFER_TYPE;

         input  : GENERATOR_TYPE (source_lines'Access, buffer'Access);
         output : COLLECTOR_TYPE (output_lines'Access, buffer'Access);

      begin
         -- test simple joint
         Copy(input, output);

         -- test closure wrapper
         declare
            N : NATURAL := 0;

            function counter return NATURAL
            is
            begin
               N := N + 1;
               return N;
            end counter;

            package Natural_Flux is new Closure_Wrapper (NATURAL);

            count : Natural_Flux.ITERABLE_TYPE (counter'Access); 

         begin
            for e of count loop
               Put(e'Image);
               exit when e = 20;
            end loop;
            New_Line;
         end;
      end;
   end;

exception
   when X : others =>
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_io;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
