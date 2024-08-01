-- control.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Real_Time;

package body Control is

   procedure set_ident(co: in out CONTROLLER) with Inline is
   begin
      if co.id = Current_Task then
         null;
      elsif co.id = Null_Task_Id then
         co.id := Current_Task;
      else
         raise Control_Error with "only can initialize current task";
      end if;
   end set_ident;

   procedure await_ident(co: in CONTROLLER) with Inline is
      use Ada.Real_Time;
      stop : TIME := Clock + Milliseconds(100);
   begin
      if co.id = Null_Task_Id then
         loop
            if Clock > stop then
               raise Control_Error with "loop timed out";
            end if;

            Ada.Dispatching.Yield;

            exit when co.id /= Null_Task_Id;
         end loop;
      end if;
   end await_ident;

   ---------------------------------------------------------------------

   procedure Co_Begin(self: in out CONTROLLER) is
   begin
      set_ident(self);

      Wait(self.here);
   end Co_Begin;

   procedure Co_End(self: in out CONTROLLER) is
   begin
      pragma Assert(self.back /= null, "cannot go back to null");

      Notify(self.back.all);

      -- initialize controller to default values
      Clear(self.here);
      self.id   := Null_Task_Id;
      self.back := null;
   end Co_End;

   ---------------------------------------------------------------------

   procedure Resume(self: in out CONTROLLER; co: in out CONTROLLER) is
   begin
      set_ident(self);
      await_ident(co);

      pragma Assert(self.id /= co.id, "cannot resume to itself");

      co.back := (
         if self.back = null
         then self.here'Unchecked_Access
         else self.back
      );

      Notify(co.here);
      Wait(self.here);
   end Resume;

   procedure Go(self: in out CONTROLLER) is
   begin
      await_ident(self);

      pragma Assert(self.id /= Current_Task, "cannot resume current task");

      Notify(self.here);
   end Go;

   procedure Yield(self: in out CONTROLLER) is
   begin
      pragma Assert(self.back /= null, "cannot go back to null");

      Notify(self.back.all);
      Wait(self.here);
   end Yield;

   ---------------------------------------------------------------------

   procedure Resume(self: in out CONTROLLER; co: access CONTROLLER) is
   begin
      Resume(self, co.all); -- inlined at spec
   end Resume;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
