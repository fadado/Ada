-- conveyors.adb

with Ada.Dispatching;

package body Conveyors is

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   procedure Run(self: in out CONVEYOR) is
   begin
      Notify(self.here);
   end Run;

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
   procedure Call(self: in out CONVEYOR) is
   begin
      Notify(self.here);
      while Busy(self.here) loop Ada.Dispatching.Yield; end loop;
      Wait(self.here);
   end Call;

   procedure Finish(self: in out CONVEYOR) is
   begin
      Notify(self.here);
   end Finish;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   procedure Yield(self: in out CONVEYOR) is
   begin
      --if self.back = null then raise Conveyor_Error; end if;

      Notify(self.back.all);
      Wait(self.here);
   end Yield;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   procedure Resume(self: in out CONVEYOR; other: in out CONVEYOR) is
   begin
      other.back := (
         if self.back = null
         then self.here'Unchecked_Access
         else self.back
      );

      Notify(other.here);
      Wait(self.here);
   end Resume;

   procedure Resume(self: in out CONVEYOR; other: access CONVEYOR) is
   begin
      Resume(self, other.all); -- inlined in spec
   end Resume;

end Conveyors;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
