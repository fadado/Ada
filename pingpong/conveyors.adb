-- conveyors.adb

with Ada.Dispatching;

package body Conveyors is

   ---------------------------------------------------------------------
   -- (Re)initialize a CONVEYOR to default values
   ---------------------------------------------------------------------
   procedure Reset(self: in out CONVEYOR) is
   begin
      Clear(self.here);
      self.id := Null_Task_Id;
      self.back := null;
   end Reset;

   ---------------------------------------------------------------------
   -- Put current task to await until a SIGNAL is received
   ---------------------------------------------------------------------
   procedure Suspend(self: in out CONVEYOR) is
   begin
      if self.id = Null_Task_Id then
         self.id := Current_Task;
      end if;

      Wait(self.here);
   end Suspend;

   ---------------------------------------------------------------------
   -- Notify a task, different to the current task, to resume
   ---------------------------------------------------------------------
   procedure Resume(self: in out CONVEYOR) is
   begin
      if self.id = Current_Task then
         raise Conveyor_Error;
      end if;

      Notify(self.here);
   end Resume;

   ---------------------------------------------------------------------
   -- Notify the target task to resume, and wait for the baton
   ---------------------------------------------------------------------
   procedure Resume(self: in out CONVEYOR; target: in out CONVEYOR) is
   begin
      if self.id = Null_Task_Id then
         self.id := Current_Task;
      end if;

      while target.id = Null_Task_Id loop
         Ada.Dispatching.Yield;
         -- TODO: check time!
      end loop;

      if self.id = target.id then
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

   procedure Resume(self: in out CONVEYOR; target: access CONVEYOR) is
   begin
      Resume(self, target.all); -- inlined at spec
   end Resume;

   ---------------------------------------------------------------------
   -- Suspend the current task after resuming the first resumer
   ---------------------------------------------------------------------
   procedure Yield(self: in out CONVEYOR) is
   begin
      if self.id /= Current_Task then
         raise Conveyor_Error;
      end if;

      if self.back = null then
         raise Conveyor_Error;
      end if;

      Notify(self.back.all);
      Wait(self.here);
   end Yield;

   ---------------------------------------------------------------------
   -- Resume the first resumer, and continue
   ---------------------------------------------------------------------
   procedure YieldX(self: in out CONVEYOR) is
   begin
      if self.id /= Current_Task then
         raise Conveyor_Error;
      end if;

      if self.back = null then
         raise Conveyor_Error;
      end if;

      Notify(self.back.all);
   end YieldX;

end Conveyors;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
