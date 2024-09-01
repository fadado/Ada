-- control.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Real_Time;

package body Control is

   ---------------------------------------------------------------------
   -- Local subprograms
   ---------------------------------------------------------------------

   type CO_STATE is (RESETED, SEMI_ATTACHED, ATTACHED);

   function State(co: in CONTROLLER) return CO_STATE with Inline is
   begin
      if co.id = Null_Task_Id then
         --pragma Assert(co.invoker = NULL);
         --pragma Assert(Is_Clean(co.flag));
         --pragma Assert(co.mode = UNDEFINED);
         return RESETED;
      elsif co.invoker = NULL then
         return SEMI_ATTACHED;
      else
         return ATTACHED;
      end if;
   end State;

   -- Initialize controller to default values.
   procedure Reset(co: in out CONTROLLER) with Inline is
   begin
      Clear(co.flag);
      co.id := Null_Task_Id;
      co.invoker := NULL;
      co.mode := UNDEFINED;

      pragma Assert(State(co) = RESETED);
   end Reset;

   -- Ensure controller is attached or semi-attached.
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

      pragma Assert(co.id /= Current_Task);
      pragma Assert(State(co) /= RESETED);
   end Await_Attach;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   procedure Attach(self: in out CONTROLLER) is
   begin
      pragma Assert(State(self) = RESETED);

      self.id := Current_Task;
      pragma Assert(State(self) = SEMI_ATTACHED);

      Wait(self.flag);
      pragma Assert(State(self) = ATTACHED);
   end Attach;

   procedure Detach(self: in out CONTROLLER) is
      target : CONTROLLER renames self.invoker.all;
   begin
      pragma Assert(State(self)   = ATTACHED);
      pragma Assert(State(target) = ATTACHED);

      pragma Assert(self.id    = Current_Task);
      pragma Assert(target.id /= Current_Task);

      pragma Assert(self.mode  = target.mode);

      Notify(target.flag);
      Reset(self);
   end Detach;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   procedure Resume(self: in out CONTROLLER; target: in out CONTROLLER) is
   begin
      if State(self) = RESETED then
         -- self is the main controller
         self.id := Current_Task;

         -- circular link
         self.invoker := self'Unchecked_Access;

         --
         self.mode := ASYMMETRIC;
      end if;
      pragma Assert(State(self) = ATTACHED);
      pragma Assert(self.id = Current_Task);
      pragma Assert(self.mode = ASYMMETRIC);

      Await_Attach(target);
      pragma Assert(target.id /= Current_Task);

      target.invoker := self'Unchecked_Access;
      pragma Assert(State(target) = ATTACHED);

      target.mode := ASYMMETRIC;
      pragma Assert(self.mode = target.mode);

      Notify(target.flag);
      Wait(self.flag);
   end Resume;

   procedure Yield(self: in out CONTROLLER) is
      invoker : CONTROLLER renames self.invoker.all;
   begin
      pragma Assert(State(self)    = ATTACHED);
      pragma Assert(State(invoker) = ATTACHED);

      pragma Assert(self.id        = Current_Task);
      pragma Assert(invoker.id    /= Current_Task);

      pragma Assert(self.mode      = ASYMMETRIC);
      pragma Assert(invoker.mode   = ASYMMETRIC);

      Notify(invoker.flag);
      Wait(self.flag);
   end Yield;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   procedure Transfer(self: in out CONTROLLER; target: in out CONTROLLER) is
   begin
      if State(self) = RESETED then
         -- self is the main controller
         self.id := Current_Task;

         -- circular link
         self.invoker := self'Unchecked_Access;

         --
         self.mode := SYMMETRIC;
      end if;
      pragma Assert(State(self) = ATTACHED);
      pragma Assert(self.id     = Current_Task);

      Await_Attach(target);
      pragma Assert(target.id /= Current_Task);

      target.invoker := self.invoker;
      pragma Assert(State(target) = ATTACHED);

      target.mode := SYMMETRIC;
      pragma Assert(self.mode = target.mode);

      Notify(target.flag);
      Wait(self.flag);
   end Transfer;

   procedure Detach(self: in out CONTROLLER; target: in out CONTROLLER) is
   begin
      pragma Assert(State(self)   = ATTACHED);
      pragma Assert(State(target) = ATTACHED);

      pragma Assert(self.id     = Current_Task);
      pragma Assert(target.id  /= Current_Task);

      pragma Assert(self.mode   = SYMMETRIC);
      pragma Assert(target.mode = SYMMETRIC);

      Notify(target.flag);
      Reset(self);
   end Detach;

   ---------------------------------------------------------------------
   -- syntactic sugar

   procedure Resume(self: in out CONTROLLER; target: access CONTROLLER) is
   begin
      Resume(self, target.all); -- inlined at spec
   end Resume;

   procedure Detach(self: in out CONTROLLER; target: access CONTROLLER) is
   begin
      Detach(self, target.all); -- inlined at spec
   end Detach;

   procedure Transfer(self: in out CONTROLLER; target: access CONTROLLER) is
   begin
      Transfer(self, target.all); -- inlined at spec
   end Transfer;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
