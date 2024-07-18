-- conveyors.adb

package body Conveyors is

   procedure Suspend(self: in out CONVEYOR) is
   begin
      Wait(self.here);
   end Suspend;

   procedure Call(self: in out CONVEYOR) is
   begin
      Notify(self.here);
   end Call;

   procedure Resume(self, other: in out CONVEYOR; W: BOOLEAN := True) is
   begin
      other.back := (
         if self.back = null
         then self.here'Unchecked_Access
         else self.back
      );

      Notify(other.here);
      if W then
         Wait(self.here);
      end if;
   end Resume;

   procedure Yield(self: in out CONVEYOR) is
   begin
      if self.back = null then
         raise Conveyor_Error;
      end if;

      Notify(self.back.all);
      Wait(self.here);
   end Yield;

end Conveyors;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
