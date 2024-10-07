-- control.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Task_Identification;
with Ada.Unchecked_Deallocation;
with Signals;

use Ada.Task_Identification;

package body Control is
   ---------------------------------------------------------------------
   -- Base controller
   ---------------------------------------------------------------------

   procedure Attach(self: in out BASE_CONTROLLER) is
   begin
      self.id := Current_Task;
      Signals.Wait(self.flag);
   end Attach;

   procedure Detach(self: in out BASE_CONTROLLER) is
      -- back: `invoker` for assymetric or `head` for symmetric
      back : BASE_CONTROLLER renames BASE_CONTROLLER(self.link.all);
   begin
      self.id := Null_Task_Id;
      self.link := NULL;
      Signals.Notify(back.flag);
   end Detach;

   procedure Resume(self, target: in out BASE_CONTROLLER)
   is
      use Ada.Exceptions;

      procedure free is new Ada.Unchecked_Deallocation(
         EXCEPTION_OCCURRENCE,
         EXCEPTION_OCCURRENCE_ACCESS
      );

      procedure await_target_attach with Inline is
         use Ada.Real_Time;

         stop : TIME := Clock + Milliseconds(100);
      begin
         -- Is `target` never resumed?
         if target.id = Null_Task_Id then
            -- spin lock until `target.Attach` is called
            loop
               if Clock > stop then
                  raise Program_Error with "loop timed out";
               end if;
               Ada.Dispatching.Yield;
               exit when target.id /= Null_Task_Id;
            end loop;
         end if;
      end await_target_attach;

   begin
      pragma Assert(self.id = Current_Task);

      -- target must be active
      await_target_attach;
      pragma Assert(target.id /= Current_Task);

      -- transfers control
      Signals.Notify(target.flag);
      Signals.Wait(self.flag);

      -- check if target raised an exception!
      if self.migrant /= NULL then
         declare
            id : EXCEPTION_ID := Exception_Identity(self.migrant.all);
            ms : STRING := Exception_Message(self.migrant.all);
         begin
            --self.migrant := target.migrant;
            --free(target.migrant);
            Raise_Exception(id, ms);
         end;
      end if;
   end Resume;

   procedure Cancel(self: in out BASE_CONTROLLER; X: Ada.Exceptions.EXCEPTION_OCCURRENCE)
   -- warning: cannot call Current_Task here!
   is
      use Ada.Exceptions;

      -- back: `invoker` for assymetric or `head` for symmetric
      back : BASE_CONTROLLER renames BASE_CONTROLLER(self.link.all);
   begin
      pragma Assert(self.id /= Null_Task_Id);

      pragma Assert(self.link /= NULL);
      self.id := Null_Task_Id;
      Signals.Notify(back.flag);

      -- migrate exception
      self.link.migrant := Save_Occurrence(X);
   end Cancel;

   ---------------------------------------------------------------------
   -- Asymmetric controller
   ---------------------------------------------------------------------

   procedure Resume(self, target: in out ASYMMETRIC_CONTROLLER) is
      super : BASE_CONTROLLER renames BASE_CONTROLLER(self);
   begin
      -- is `self` an uninitialized controller?
      if self.id = Null_Task_Id then
         pragma Assert(self.link = NULL);
         self.id := Current_Task;
         self.link := self'Unchecked_Access; -- circular link
      end if;

      -- stack like linking
      target.link := self'Unchecked_Access;

      -- delegate to primary method
      super.Resume(BASE_CONTROLLER(target));
   end Resume;

   procedure Yield(self: in out ASYMMETRIC_CONTROLLER) is
      invoker : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self.link.all);
   begin
      Signals.Notify(invoker.flag);
      Signals.Wait(self.flag);
   end Yield;

   ---------------------------------------------------------------------
   -- Symmetric controller
   ---------------------------------------------------------------------

   procedure Detach(self, target: in out SYMMETRIC_CONTROLLER) is
   begin
      self.id := Null_Task_Id;
      self.link := NULL;
      Signals.Notify(target.flag);
   end Detach;

   procedure Resume(self, target: in out SYMMETRIC_CONTROLLER) is
      super : BASE_CONTROLLER renames BASE_CONTROLLER(self);
   begin
      if self.id = Null_Task_Id then
         pragma Assert(self.link = NULL);
         self.id := Current_Task;
         self.link := self'Unchecked_Access;
      end if;

      -- only can detach to the first controller
      target.link := self.link;

      -- delegate to primary method
      super.Resume(BASE_CONTROLLER(target));
   end Resume;

   ---------------------------------------------------------------------
   -- Syntactic sugar
   ---------------------------------------------------------------------

   procedure Resume(self: in out ASYMMETRIC_CONTROLLER;
                    target: access ASYMMETRIC_CONTROLLER) is
   begin
      Resume(self, target.all);
   end Resume;

   procedure Resume(self: in out SYMMETRIC_CONTROLLER;
                    target: access SYMMETRIC_CONTROLLER) is
   begin
      Resume(self, target.all);
   end Resume;

   procedure Detach(self: in out SYMMETRIC_CONTROLLER;
                    target: access SYMMETRIC_CONTROLLER) is
   begin
      Detach(self, target.all);
   end Detach;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
