-- demo.adb

with Ada.Text_IO; use Ada.Text_IO;

with High_Order;
use  High_Order;

procedure demo is
   Error : exception;
begin
   declare
      procedure Swap is
         new G_Swap (INTEGER);
      i, j : INTEGER;
   begin
      i := 1;
      j := 2;
      Swap(i, j);
      if i /= 2 or j /= 1 then raise Error; end if;
   end;
   
   declare
      function square(n: INTEGER) return INTEGER is (n*n);
      function half(n: INTEGER)   return INTEGER is (n/2);
      function square_and_half is
         new G_Compose (INTEGER, half, square);
   begin
      if square_and_half(5) /= 12 then raise Error; end if;
   end;
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
