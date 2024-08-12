-- control.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Real_Time;

package body Control is

   type CO_STATE is (RESETED, SEMI_ATTACHED, ATTACHED);

   function State(co: in CONTROLLER) return CO_STATE with Inline is
   begin
      if co.id = Null_Task_Id then
         --pragma Assert(co.master = NULL);
         --pragma Assert(Is_Clean(co.flag));
         return RESETED;
      elsif co.master = NULL then
         return SEMI_ATTACHED;
      else
         return ATTACHED;
      end if;
   end State;

   ---------------------------------------------------------------------

   -- initialize controller to default values
   procedure Reset(co: in out CONTROLLER) with Inline is
   begin
      Clear(co.flag);
      co.id := Null_Task_Id;
      co.master := NULL;

      pragma Assert(State(co) = RESETED);
   end Reset;

   ---------------------------------------------------------------------

   procedure Await_Attach(co: in CONTROLLER) with Inline is
      use Ada.Real_Time;
      stop : TIME := Clock + Milliseconds(100);
   begin
      if State(co) = RESETED then
         loop
            if Clock > stop then
               raise Program_Error with "loop timed out";
            end if;

            Ada.Dispatching.Yield;

            exit when State(co) = SEMI_ATTACHED;
         end loop;
      end if;

      pragma Assert(State(co) /= RESETED);
   end Await_Attach;

   ---------------------------------------------------------------------
   -- CONTROLLER methods
   ---------------------------------------------------------------------

   procedure Attach(self: in out CONTROLLER) is
   begin
      pragma Assert(State(self) = RESETED);

      self.id := Current_Task;
      pragma Assert(State(self) = SEMI_ATTACHED);

      Wait(self.flag);
      pragma Assert(State(self) = ATTACHED);
   end Attach;

   ---------------------------------------------------------------------

   procedure Detach(self: in out CONTROLLER) is
      master : CONTROLLER renames self.master.all;
   begin
      Detach(self, master); -- inlined at spec
   end Detach;

   ---------------------------------------------------------------------

   procedure Detach(self: in out CONTROLLER; target: in out CONTROLLER) is
   begin
      pragma Assert(State(self)   = ATTACHED);
      pragma Assert(State(target) = ATTACHED);

      Notify(target.flag);
      Reset(self);

      pragma Assert(State(self) = RESETED);
   end Detach;

   ---------------------------------------------------------------------

   procedure Resume(self: in out CONTROLLER; target: in out CONTROLLER) is
   begin
      if State(self) = RESETED then
         -- self is the master controller
         self.id := Current_Task;

         -- circular link
         self.master := self'Unchecked_Access;
      end if;
      pragma Assert(State(self) = ATTACHED);

      Await_Attach(target);
      pragma Assert(self.id /= target.id);

      if target.master = NULL then
         target.master := self.master;
      end if;
      pragma Assert(State(target) = ATTACHED);

      Notify(target.flag);
      Wait(self.flag);
   end Resume;

   ---------------------------------------------------------------------

   procedure Yield(self: in out CONTROLLER) is
      master : CONTROLLER renames self.master.all;
   begin
      pragma Assert(State(self) = ATTACHED);

      Notify(master.flag);
      Wait(self.flag);
   end Yield;

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
