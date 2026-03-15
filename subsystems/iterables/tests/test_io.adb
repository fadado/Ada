------------------------------------------------------------------------------
--  Reading stdin as an iterator
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

--with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Iterables;

--with Gotcha;

procedure test_io
is
   -- test Closure_Wrapper
   N : NATURAL := 0;
   function counter return NATURAL
   is
   begin
      N := N + 1;
      return N;
   end counter;

   package Natural_Flux is new Iterables.Closure_Wrapper (NATURAL);
   count : Natural_Flux.ITERABLE_TYPE (counter'Access); 

begin
   for element of count loop
      Put(element'Image);
      exit when element = 20;
   end loop;
   New_Line;

--exception
   --when X : others =>
      --Gotcha.Report_Exception(X, "Handled exception at top level");

end test_io;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
