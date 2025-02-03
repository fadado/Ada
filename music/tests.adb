-- tests.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Text_IO;
with Music;

procedure Tests is
   use Ada.Text_IO;
   use Music;
begin
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   declare
      x, y : PITCH;
      i, j : PITCH_INTERVAL;
      u : UNORDERED_INTERVAL;
   begin
      x := 60; y := 67;
      i := Distance(x, y);
      j := Distance(y, x);
      u := abs Distance(y, x);

      pragma Assert(i = 7);
      pragma Assert(j = -7);
      pragma Assert(u = 7);

      pragma Assert(Transposition(i, x) = y);
      pragma Assert(Transposition(j, y) = x);
   end;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   declare
      x, y : PITCH_CLASS;
      i, j : PC_INTERVAL;
      u : INTERVAL_CLASS;
   begin
      x := 0; y := 7;
      i := Distance(x, y);
      j := Distance(y, x);
      u := abs j;

      pragma Assert(i = 7);
      pragma Assert(j = 5);
      pragma Assert(u = 5);

      pragma Assert(Transposition(i, x) = y);
      pragma Assert(Transposition(j, y) = x);
      pragma Assert(x + i = y);
      pragma Assert(y + j = x);

      x := 0; i := 0; pragma Assert(Inversion(i, x) = 0);
      x := 0; i := 5; pragma Assert(Inversion(i, x) = 5);
      x := 1; i := 5; pragma Assert(Inversion(i, x) = 4);
      x := 5; i := 2; pragma Assert(Inversion(i, x) = 9);
      x := 2; i := 5; pragma Assert(x - i = 9);

      i := -PC_INTERVAL'(1);  pragma Assert(i = 11);
      i := -PC_INTERVAL'(11); pragma Assert(i = 1);

      for x in PITCH_CLASS loop
         for y in PITCH_CLASS loop
            pragma Assert(Distance(x,y) = -Distance(y,x));

            for z in PITCH_CLASS loop
               pragma Assert(Distance(x,y)+Distance(y,z) = Distance(x,z));
            end loop;
         end loop;
      end loop;

      for i in PC_INTERVAL loop
         pragma Assert(i + 0  = i);
         pragma Assert(i + (-i) = 0);

         for j in PC_INTERVAL loop
            for k in PC_INTERVAL loop
               pragma Assert((i+j)+k = i+(j+k));
            end loop;
         end loop;

         for x in PITCH_CLASS loop
            pragma Assert(Distance(x,Transposition(i,x)) = i);
            pragma Assert(Inversion(i,x) = Transposition(i,-x));
            pragma Assert(Inversion(i,x) = i-x);
         end loop;
      end loop;
   end;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

end Tests;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
