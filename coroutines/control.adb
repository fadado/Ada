-- control.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Real_Time;

package body Control is

   ---------------------------------------------------------------------
   -- Local subprograms
   ---------------------------------------------------------------------

   type CO_STATE is (RESETED, SEMI_ATTACHED, ATTACHED);

   function state(co: in BASE_CONTROLLER'Class) return CO_STATE with Inline is
   begin
      if co.id = Null_Task_Id then
         --pragma Assert(co.invoker = NULL);
         --pragma Assert(Is_Clean(co.flag));
         return RESETED;
      elsif co.invoker = NULL then
         return SEMI_ATTACHED;
      else
         return ATTACHED;
      end if;
   end state;

   -- Initialize controller to default values.
   procedure reset(co: in out BASE_CONTROLLER'Class) with Inline is
   begin
      Clear(co.flag);
      co.id := Null_Task_Id;
      co.invoker := NULL;

      pragma Assert(state(co) = RESETED);
   end reset;

   ---------------------------------------------------------------------
   -- Base controller
   ---------------------------------------------------------------------

   procedure Attach(self: in out BASE_CONTROLLER) is
   begin
      pragma Assert(state(self) = RESETED);

      self.id := Current_Task;
      pragma Assert(state(self) = SEMI_ATTACHED);

      Wait(self.flag);
      pragma Assert(state(self) = ATTACHED);
   end Attach;

   procedure Detach(self: in out BASE_CONTROLLER) is
      target : BASE_CONTROLLER renames BASE_CONTROLLER(self.invoker.all);
   begin
      pragma Assert(state(self)   = ATTACHED);
      pragma Assert(state(target) = ATTACHED);

      pragma Assert(self.id    = Current_Task);
      pragma Assert(target.id /= Current_Task);

      Notify(target.flag);
      reset(self);
   end Detach;

   procedure Resume(self: in out BASE_CONTROLLER; target: in out BASE_CONTROLLER) is

      procedure await_attach(co: in BASE_CONTROLLER) with Inline is
         use Ada.Real_Time;
         stop : TIME := Clock + Milliseconds(100);
      begin
         -- Ensure controller is attached or semi-attached.
         if state(co) = RESETED then
            loop
               if Clock > stop then
                  raise Program_Error with "loop timed out";
               end if;

               Ada.Dispatching.Yield;

               exit when state(co) = SEMI_ATTACHED;
            end loop;
         end if;

         pragma Assert(co.id /= Current_Task);
         pragma Assert(state(co) /= RESETED);
      end await_attach;

      function is_asymmetric(co: in BASE_CONTROLLER) return BOOLEAN with Inline is
      begin
         return BASE_CONTROLLER'Class(co) in ASYMMETRIC_CONTROLLER'Class;
      end is_asymmetric;

      function is_symmetric(co: in BASE_CONTROLLER) return BOOLEAN with Inline is
      begin
         return BASE_CONTROLLER'Class(co) in SYMMETRIC_CONTROLLER'Class;
      end is_symmetric;

   begin
      pragma Assert(is_asymmetric(self) = is_asymmetric(target));

      if state(self) = RESETED then
         -- initialize the main controller
         self.id := Current_Task;
         pragma Assert(state(self) = SEMI_ATTACHED);

         -- circular link
         self.invoker := self'Unchecked_Access;
      end if;

      pragma Assert(state(self) = ATTACHED);
      pragma Assert(self.id = Current_Task);

      await_attach(target);
      pragma Assert(target.id /= Current_Task);

      if is_asymmetric(self) then
         target.invoker := self'Unchecked_Access;
      else
         target.invoker := self.invoker;
      end if;
      pragma Assert(state(target) = ATTACHED);

      Notify(target.flag);
      Wait(self.flag);
   end Resume;

   ---------------------------------------------------------------------
   -- Asymmetric controller
   ---------------------------------------------------------------------

   procedure Yield(self: in out ASYMMETRIC_CONTROLLER) is
      target : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self.invoker.all);
   begin
      pragma Assert(state(self)   = ATTACHED);
      pragma Assert(state(target) = ATTACHED);

      pragma Assert(self.id    = Current_Task);
      pragma Assert(target.id /= Current_Task);

      Notify(target.flag);
      Wait(self.flag);
   end Yield;

   ---------------------------------------------------------------------
   -- Symmetric controller
   ---------------------------------------------------------------------

   procedure Detach(self: in out SYMMETRIC_CONTROLLER; target: in out SYMMETRIC_CONTROLLER) is
   begin
      pragma Assert(state(self)   = ATTACHED);
      pragma Assert(state(target) = ATTACHED);

      pragma Assert(self.id    = Current_Task);
      pragma Assert(target.id /= Current_Task);

      Notify(target.flag);
      reset(self);
   end Detach;

   ---------------------------------------------------------------------
   -- Syntactic sugar
   ---------------------------------------------------------------------

   procedure Resume(self: in out ASYMMETRIC_CONTROLLER; target: access ASYMMETRIC_CONTROLLER) is
   begin
      Resume(self, target.all); -- inlined at spec
   end Resume;

   procedure Resume(self: in out SYMMETRIC_CONTROLLER; target: access SYMMETRIC_CONTROLLER) is
   begin
      Resume(self, target.all); -- inlined at spec
   end Resume;

   procedure Detach(self: in out SYMMETRIC_CONTROLLER; target: access SYMMETRIC_CONTROLLER) is
   begin
      Detach(self, target.all); -- inlined at spec
   end Detach;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
