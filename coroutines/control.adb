-- control.adb

pragma Assertion_Policy(Ignore); -- Check / Ignore

with Ada.Dispatching;
with Ada.Real_Time;

package body Control is

   procedure set_ident(c: in out CONTROLLER) with Inline is
   begin
      if c.id = Null_Task_Id then
         c.id := Current_Task;
      elsif c.id = Current_Task then
         null;
      else
         raise Control_Error with "only can initialize current task";
      end if;
   end set_ident;

   procedure await_ident(c: in CONTROLLER) with Inline is
      use Ada.Real_Time;
      stop : TIME := Clock + Milliseconds(100);
   begin
      while c.id = Null_Task_Id loop
         if Clock > stop then
            raise Control_Error with "loop timed out";
         end if;

         Ada.Dispatching.Yield;
      end loop;
   end await_ident;

   procedure check_initialized(c: in CONTROLLER) with Inline is
   begin
      if c.id /= Current_Task then
         raise Control_Error with "must be called from the current task";
      end if;
      if c.back = null then
         raise Control_Error with "cannot go back to null";
      end if;
   end check_initialized;

   ---------------------------------------------------------------------

   procedure Suspend(here: in out CONTROLLER) is
   begin
      set_ident(here);

      Wait(here.flag);
   end Suspend;

   procedure Resume(here: in out CONTROLLER; there: in out CONTROLLER) is
   begin
      set_ident(here);
      await_ident(there);

      pragma Assert(here.id /= there.id, "cannot resume to itself");

      there.back := (
         if here.back = null
         then here.flag'Unchecked_Access
         else here.back
      );

      Notify(there.flag);
      Wait(here.flag);
   end Resume;

   procedure Go(there: in out CONTROLLER) is
   begin
      await_ident(there);

      pragma Assert(there.id /= Current_Task, "cannot resume current task");

      Notify(there.flag);
   end Go;

   procedure Yield(here: in out CONTROLLER) is
   begin
      check_initialized(here);

      Notify(here.back.all);
      Wait(here.flag);
   end Yield;

   procedure Finish(here: in out CONTROLLER) is
   begin
      check_initialized(here);

      Notify(here.back.all);

      here.id   := Null_Task_Id;
      here.back := null;
   end Finish;

   ---------------------------------------------------------------------

   procedure Resume(here: in out CONTROLLER; there: access CONTROLLER) is
   begin
      Resume(here, there.all); -- inlined at spec
   end Resume;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
