-- conveyors.adb

with Ada.Dispatching;

package body Conveyors is

   procedure Reset(self: in out CONVEYOR) is
   begin
      Clear(self.here);
      self.id := Null_Task_Id;
      self.back := null;
   end Reset;

   procedure Suspend(self: in out CONVEYOR) is
   begin
      if self.id = Null_Task_Id then
         -- initialize ID
         self.id := Current_Task;
      end if;

      if self.id /= Current_Task then
         -- only can suspend current task
         raise Conveyor_Error;
      end if;

      Wait(self.here);
   end Suspend;

   procedure Resume(target: in out CONVEYOR) is
   begin
      while target.id = Null_Task_Id loop
         -- the resumee must have to be suspended once at least
         Ada.Dispatching.Yield; -- TODO: check time!
      end loop;

      if target.id = Current_Task then
         -- cannot resume current task
         raise Conveyor_Error;
      end if;

      Notify(target.here);
   end Resume;

   procedure Resume(self: in out CONVEYOR; target: in out CONVEYOR) is
   begin
      if self.id = Null_Task_Id then
         -- initialize ID
         self.id := Current_Task;
      end if;

      if self.id /= Current_Task then
         -- only can resume from current task
         raise Conveyor_Error;
      end if;

      while target.id = Null_Task_Id loop
         -- the resumee must have to be suspended once at least
         Ada.Dispatching.Yield; -- TODO: check time!
      end loop;

      if self.id = target.id then
         -- cannot resume to itself
         raise Conveyor_Error;
      end if;

      target.back := (
         if self.back = null
         then self.here'Unchecked_Access
         else self.back
      );

      Notify(target.here);
      Wait(self.here);
   end Resume;

   procedure Yield(self: in out CONVEYOR; Await: BOOLEAN := TRUE) is
   begin
      if self.id /= Current_Task then
         -- cannot yield to the current task
         raise Conveyor_Error;
      end if;

      if self.back = null then
         -- cannot yield to null
         raise Conveyor_Error;
      end if;

      Notify(self.back.all);
      if Await then
         Wait(self.here);
      end if;
   end Yield;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   procedure Resume(self: in out CONVEYOR; target: access CONVEYOR) is
   begin
      Resume(self, target.all); -- inlined at spec
   end Resume;

end Conveyors;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
