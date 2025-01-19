-- tests.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Text_IO;
with Music;

procedure Tests is
   use Ada.Text_IO;
   use Music;
begin
   declare
      x, y : PITCH;
      i, j : PITCH_INTERVAL;
      u : UNORDERED_INTERVAL;
   begin
      x := 60; y := 67;
      i := Interval(x, y);
      j := Interval(y, x);
      u := abs Interval(y, x);

      pragma Assert(i = 7);
      pragma Assert(j = -7);
      pragma Assert(u = 7);

      pragma Assert(Transposition(x, i) = y);
      pragma Assert(Transposition(y, j) = x);
   end;
end Tests;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
