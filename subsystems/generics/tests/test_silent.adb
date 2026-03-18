pragma Assertion_Policy(Check);

with Ada.Assertions;
with Ada.Exceptions;
with Ada.Text_IO;

with Silent_Generics;
with Silent_Tuples;

procedure Test_Silent
is
   use Ada;
   use Assertions;
   use Exceptions;
begin
   Silent_Generics;
   Silent_Tuples;
exception
   when A : Assertion_Error =>
      Text_IO.Put_Line("ASSERTED:" & Exception_Message(A));
end Test_Silent;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
