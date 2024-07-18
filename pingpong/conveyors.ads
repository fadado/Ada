-- conveyors.ads

with Signals;

package Conveyors is

   type CONVEYOR is tagged limited private;

   procedure Call(self: in out CONVEYOR);
   procedure Suspend(self: in out CONVEYOR);
   procedure Resume(self, other: in out CONVEYOR; W: BOOLEAN := True);
   procedure Yield(self: in out CONVEYOR);

   Conveyor_Error : exception;

private
   use Signals;

   type CONVEYOR is tagged limited
      record
         here: aliased SIGNAL; -- defaults to false
         back: access  SIGNAL; -- defaults to null
      end record;

end Conveyors;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
