-- control.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Real_Time;

package body Control is

   -- initialize controller to default values
   procedure reset(co: in out CONTROLLER) with Inline is
   begin
      Clear(co.here);
      co.id := Null_Task_Id;
      co.master := NULL;
   end reset;

   ---------------------------------------------------------------------

   type CO_STATE is (RESETED, ATTACHED, LINKED);

   function state(co: in CONTROLLER) return CO_STATE with Inline is
   begin
      if co.id = Null_Task_Id then
         return RESETED;
      end if;

      if co.master = NULL then
         return ATTACHED;
      end if;

      -- co.master /= NULL
      return LINKED;
   end state;

   ---------------------------------------------------------------------

   procedure await_attach(co: in CONTROLLER) with Inline is
      use Ada.Real_Time;
      stop : TIME := Clock + Milliseconds(100);
   begin
      if state(co) = RESETED then
         loop
            if Clock > stop then
               raise Program_Error with "loop timed out";
            end if;

            Ada.Dispatching.Yield;

            exit when state(co) = ATTACHED;
         end loop;
      end if;

      pragma Assert(state(co) /= RESETED);
   end await_attach;

   ---------------------------------------------------------------------
   -- CONTROLLER methods
   ---------------------------------------------------------------------

   procedure Attach(self: in out CONTROLLER) is
   begin
      pragma Assert(state(self) = RESETED);

      self.id := Current_Task;
      pragma Assert(state(self) = ATTACHED);

      Wait(self.here);
   end Attach;

   ---------------------------------------------------------------------

   procedure Resume(self: in out CONTROLLER; target: in out CONTROLLER) is
   begin
      if state(self) = RESETED then
         -- self is the master controller
         self.id := Current_Task;
         pragma Assert(state(self) = ATTACHED);
      end if;

      pragma Assert(state(self) /= RESETED);

      await_attach(target);

      pragma Assert(self.id /= target.id);

      if self.master = NULL then
         target.master := self'Unchecked_Access;
      else
         -- TODO: preserve NULL in master itself?
         target.master := self.master;
      end if;

      pragma Assert(state(target) = LINKED);

      Notify(target.here);
      Wait(self.here);
   end Resume;

   ---------------------------------------------------------------------

   procedure Yield(self: in out CONTROLLER) is
      master : CONTROLLER renames self.master.all;
   begin
      pragma Assert(state(self) = LINKED);

      Notify(master.here);
      Wait(self.here);
   end Yield;

   ---------------------------------------------------------------------

   procedure Detach(self: in out CONTROLLER) is
      master : CONTROLLER renames self.master.all;
   begin
      pragma Assert(state(self) /= RESETED);

      Notify(master.here);

      reset(self);

      pragma Assert(state(self) = RESETED);
   end Detach;

   ---------------------------------------------------------------------

   procedure Detach(self: in out CONTROLLER; target: in out CONTROLLER) is
   begin
      pragma Assert(state(self) = LINKED);

      await_attach(target);

      pragma Assert(self.id /= target.id);

      Notify(target.here);

      reset(self);

      pragma Assert(state(self) = RESETED);
   end Detach;

   ---------------------------------------------------------------------

   procedure Resume(self: in out CONTROLLER; target: access CONTROLLER) is
   begin
      Resume(self, target.all); -- inlined at spec
   end Resume;

   procedure Detach(self: in out CONTROLLER; target: access CONTROLLER) is
   begin
      Detach(self, target.all); -- inlined at spec
   end Detach;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
