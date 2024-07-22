-- conveyors.adb

with Ada.Dispatching;

package body Conveyors is

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   procedure Reset(self: in out CONVEYOR) is
   begin
      Clear(self.here);
      self.id := Null_Task_Id;
      self.back := null;
   end Reset;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   procedure Suspend(self: in out CONVEYOR) is
   begin
      if self.id = Null_Task_Id then
         self.id := Current_Task;
      end if;

      Wait(self.here);
   end Suspend;

   ---------------------------------------------------------------------
   --
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
   --
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
   --
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

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   procedure Continue(self: in out CONVEYOR) is
   begin
      if self.id = Current_Task then
         raise Conveyor_Error;
      end if;

      Notify(self.here);
   end Continue;

end Conveyors;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
