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
   declare
      x, y : PITCH_CLASS;
      i, j : DIRECTED_INTERVAL;
      u : INTERVAL_CLASS;
   begin
      x := 0; y := 7;
      i := Interval(x, y);
      j := Interval(y, x);
      u := abs j;

      pragma Assert(i = 7);
      pragma Assert(j = 5);
      pragma Assert(u = 5);

      pragma Assert(Transposition(x, i) = y);
      pragma Assert(Transposition(y, j) = x);

      pragma Assert(Inversion(0, 0) = 0);
      pragma Assert(Inversion(0, 5) = 5);
      pragma Assert(Inversion(1, 5) = 4);
      pragma Assert(Inversion(5, 2) = 9);
   end;
end Tests;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
