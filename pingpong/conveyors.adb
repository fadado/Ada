-- conveyors.adb

with Ada.Dispatching;

package body Conveyors is

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   procedure Suspend(self: in out CONVEYOR) is
   begin
      Wait(self.here);
   end Suspend;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   procedure Resume(self: in out CONVEYOR) is
   begin
      Notify(self.here);
   end Resume;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   procedure Resume(self: in out CONVEYOR; target: in out CONVEYOR) is
   begin
      target.back := (
         if self.back = null
         then self.here'Unchecked_Access
         else self.back
      );

      Notify(target.here);
      Wait(self.here);
   end Resume;

   procedure Resume(self: in out CONVEYOR; target: access CONVEYOR) is
   begin
      Resume(self, target.all); -- inlined in spec
   end Resume;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   procedure Yield(self: in out CONVEYOR) is
   begin
      if self.back = null then raise Conveyor_Error; end if;

      Notify(self.back.all);
      Wait(self.here);
   end Yield;

end Conveyors;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
