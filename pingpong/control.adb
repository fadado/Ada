-- control.adb

with Ada.Dispatching;

package body Control is

   procedure Reset(C: in out CONTROLLER) is
   begin
      Clear(C.flag);
      C.id := Null_Task_Id;
      C.back := null;
   end Reset;

   procedure Suspend(here: in out CONTROLLER) is
   begin
      if here.id = Null_Task_Id then
         here.id := Current_Task; -- initialize ID
      end if;

      if here.id /= Current_Task then
         raise Control_Error with "only can suspend current task";
      end if;

      Wait(here.flag);
   end Suspend;

   procedure Resume(there: in out CONTROLLER) is
   begin
      while there.id = Null_Task_Id loop -- await initialization
         Ada.Dispatching.Yield; -- TODO: check time!
      end loop;

      if there.id = Current_Task then
         raise Control_Error with "cannot resume current task";
      end if;

      Notify(there.flag);
   end Resume;

   procedure Resume(here: in out CONTROLLER; there: in out CONTROLLER) is
   begin
      if here.id = Null_Task_Id then
         here.id := Current_Task; -- initialize ID
      end if;

      if here.id /= Current_Task then
         raise Control_Error with "only can resume from current task";
      end if;

      while there.id = Null_Task_Id loop -- await initialization
         Ada.Dispatching.Yield; -- TODO: check time!
      end loop;

      if here.id = there.id then
         raise Control_Error with "cannot resume to itself";
      end if;

      there.back := (
         if here.back = null
         then here.flag'Unchecked_Access
         else here.back
      );

      Notify(there.flag);
      Wait(here.flag);
   end Resume;

   procedure Yield(here: in out CONTROLLER; await: BOOLEAN := TRUE) is
   begin
      if here.id /= Current_Task then
         raise Control_Error with "cannot yield to the current task";
      end if;

      if here.back = null then
         raise Control_Error with "cannot yield to null";
      end if;

      Notify(here.back.all);
      if await then
         Wait(here.flag);
      end if;
   end Yield;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   procedure Resume(here: in out CONTROLLER; there: access CONTROLLER) is
   begin
      Resume(here, there.all); -- inlined at spec
   end Resume;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
