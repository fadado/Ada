-- conveyors.adb

with Ada.Dispatching;

package body Conveyors is

   procedure Reset(C: in out CONVEYOR) is
   begin
      Clear(C.flag);
      C.id := Null_Task_Id;
      C.back := null;
   end Reset;

   procedure Suspend(here: in out CONVEYOR) is
   begin
      if here.id = Null_Task_Id then
         -- initialize ID
         here.id := Current_Task;
      end if;

      if here.id /= Current_Task then
         raise Conveyor_Error with "only can suspend current task";
      end if;

      Wait(here.flag);
   end Suspend;

   procedure Resume(there: in out CONVEYOR) is
   begin
      while there.id = Null_Task_Id loop
         -- there must have to be suspended once at least
         Ada.Dispatching.Yield; -- TODO: check time!
      end loop;

      if there.id = Current_Task then
         raise Conveyor_Error with "cannot resume current task";
      end if;

      Notify(there.flag);
   end Resume;

   procedure Resume(here: in out CONVEYOR; there: in out CONVEYOR) is
   begin
      if here.id = Null_Task_Id then
         -- initialize ID
         here.id := Current_Task;
      end if;

      if here.id /= Current_Task then
         raise Conveyor_Error with "only can resume from current task";
      end if;

      while there.id = Null_Task_Id loop
         -- the resumee must have to be suspended once at least
         Ada.Dispatching.Yield; -- TODO: check time!
      end loop;

      if here.id = there.id then
         raise Conveyor_Error with "cannot resume to itself";
      end if;

      there.back := (
         if here.back = null
         then here.flag'Unchecked_Access
         else here.back
      );

      Notify(there.flag);
      Wait(here.flag);
   end Resume;

   procedure Yield(here: in out CONVEYOR; await: BOOLEAN := TRUE) is
   begin
      if here.id /= Current_Task then
         raise Conveyor_Error with "cannot yield to the current task";
      end if;

      if here.back = null then
         raise Conveyor_Error with "cannot yield to null";
      end if;

      Notify(here.back.all);
      if await then
         Wait(here.flag);
      end if;
   end Yield;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   procedure Resume(here: in out CONVEYOR; there: access CONVEYOR) is
   begin
      Resume(here, there.all); -- inlined at spec
   end Resume;

end Conveyors;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
