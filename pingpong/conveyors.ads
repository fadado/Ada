-- conveyors.ads

with Signals;

package Conveyors is

   type CONVEYOR is tagged limited private;

   procedure Suspend(self: in out CONVEYOR) with Inline;

   procedure Resume(self: in out CONVEYOR) with Inline;
   procedure Resume(self: in out CONVEYOR; target: access CONVEYOR) with Inline;
   procedure Resume(self: in out CONVEYOR; target: in out CONVEYOR);

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
