-- demo.adb

with Ada.Text_IO; use Ada.Text_IO;

with High_Order;
use  High_Order;

procedure demo is
   Error : exception;

   procedure test_swap is
      procedure Swap is new G_Swap (INTEGER);
      i, j : INTEGER;
   begin
      i := 1;
      j := 2;
      Swap(i, j);
      if i /= 2 or j /= 1 then raise Error; end if;
   end;

   procedure test_compose is
      function square(n: INTEGER) return INTEGER is (n*n);
      function half(n: INTEGER)   return INTEGER is (n/2);
      function s_h is new G_Compose (INTEGER, half, square);
   begin
      if s_h(5) /= 12 then raise Error; end if;
   end;
begin
   test_swap;
   test_compose;

   declare
      package E is new Equality_Concept (INTEGER);
      package O is new Ordering_Concept (E);
   begin
      null;
   end;
end demo;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
