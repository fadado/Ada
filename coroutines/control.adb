-- control.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Real_Time;

package body Control is

   type CO_STATE is (RESETED, PAIRED, LINKED);

   function state(co: in CONTROLLER) return CO_STATE with Inline is
   begin
      if co.id = Null_Task_Id then
         return RESETED;
      end if;

      if co.back = NULL then
         return PAIRED;
      end if;

      -- co.back /= NULL
      return LINKED;
   end state;

   ---------------------------------------------------------------------

   procedure await_pairing(co: in CONTROLLER) with Inline is
      use Ada.Real_Time;
      stop : TIME := Clock + Milliseconds(100);
   begin
      if state(co) = RESETED then
         loop
            if Clock > stop then
               raise Program_Error with "loop timed out";
            end if;

            Ada.Dispatching.Yield;

            exit when state(co) /= RESETED;
         end loop;
      end if;

      pragma Assert(state(co) /= RESETED);
   end await_pairing;

   ---------------------------------------------------------------------

   procedure Co_Begin(self: in out CONTROLLER) is
   begin
      pragma Assert(state(self) = RESETED);

      self.id := Current_Task;
      pragma Assert(state(self) = PAIRED);

      Wait(self.here);
   end Co_Begin;

   procedure Co_End(self: in out CONTROLLER) is
   begin
      pragma Assert(state(self) /= RESETED);

      Notify(self.back.all);

      -- initialize controller to default values
      Clear(self.here);
      self.id   := Null_Task_Id;
      self.back := NULL;

      pragma Assert(state(self) = RESETED);
   end Co_End;

   ---------------------------------------------------------------------

   procedure Resume(self: in out CONTROLLER; co: in out CONTROLLER) is
   begin
      if state(self) = RESETED then
         self.id := Current_Task;
      end if;
      pragma Assert(state(self) in PAIRED..LINKED); -- fragile...

      await_pairing(co);

      pragma Assert(self.id /= co.id);

      co.back := (
         if self.back = NULL
         then self.here'Unchecked_Access
         else self.back
      );
      pragma Assert(state(co) = LINKED);

      Notify(co.here);
      Wait(self.here);
   end Resume;

   procedure Go(self: in out CONTROLLER) is
   begin
      await_pairing(self);

      pragma Assert(self.id /= Current_Task);

      Notify(self.here);
   end Go;

   procedure Yield(self: in out CONTROLLER) is
   begin
      pragma Assert(state(self) = LINKED);

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
