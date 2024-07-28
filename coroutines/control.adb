-- control.adb

with Ada.Dispatching;
with Ada.Real_Time;

package body Control is

   procedure initialize(c: in out CONTROLLER) with Inline is
   begin
      if c.id = Null_Task_Id then
         c.id := Current_Task;
      end if;

      if c.id /= Current_Task then
         raise Control_Error with "only can initialize current task";
      end if;
   end initialize;

   procedure await_initialized(c: in out CONTROLLER) with Inline is
      use Ada.Real_Time;
      stop : TIME := Clock + Milliseconds(100);
   begin
      while c.id = Null_Task_Id loop
         if Clock > stop then
            raise Control_Error with "loop timed out";
         end if;

         Ada.Dispatching.Yield;
      end loop;
   end await_initialized;

   ---------------------------------------------------------------------

   procedure Reset(c: in out CONTROLLER) is
   begin
      Clear(c.flag);
      c.id := Null_Task_Id;
      c.back := null;
   end Reset;

   procedure Suspend(here: in out CONTROLLER) is
   begin
      initialize(here);

      Wait(here.flag);
   end Suspend;

   procedure Resume(here: in out CONTROLLER; there: in out CONTROLLER) is
   begin
      initialize(here);

      await_initialized(there);

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

   procedure Go(there: in out CONTROLLER) is
   begin
      await_initialized(there);

      if there.id = Current_Task then
         raise Control_Error with "cannot resume current task";
      end if;

      Notify(there.flag);
   end Go;

   ---------------------------------------------------------------------

   procedure Resume(here: in out CONTROLLER; there: access CONTROLLER) is
   begin
      Resume(here, there.all); -- inlined at spec
   end Resume;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
