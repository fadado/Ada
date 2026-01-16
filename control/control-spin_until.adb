------------------------------------------------------------------------------
--  Control . Spin_Until implementation
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Real_Time;

use Ada.Real_Time;

procedure Control . Spin_Until
  (done : not null access function return BOOLEAN)
is
   msec : constant := 100;
   stop : constant TIME := Clock + Milliseconds(msec);
begin
   if not done.all then
      loop
         if Clock > stop then
            raise Control_Error with "spinning loop timed out";
         end if;
         Ada.Dispatching.Yield;
         exit when done.all;
      end loop;
   end if;
end Control . Spin_Until;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
