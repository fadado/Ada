-- control.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Dispatching;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Task_Identification;
with Ada.Unchecked_Deallocation;
with Signals;

use Ada.Task_Identification;
use Signals;

package body Control is
   ---------------------------------------------------------------------
   -- Base controller
   ---------------------------------------------------------------------

   procedure Attach(self: in out BASE_CONTROLLER) is
   begin
      self.id := Current_Task;
      Wait(self.flag);
   end Attach;

   procedure Detach(self: in out BASE_CONTROLLER) is
      -- back: `invoker` for asymetric or `head` for symmetric
      back : BASE_CONTROLLER renames BASE_CONTROLLER(self.link.all);
   begin
      self.id := Null_Task_Id;
      self.link := NULL;
      Notify(back.flag);
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

      procedure migrate_exception with Inline is
         id : EXCEPTION_ID := Exception_Identity(self.migrant.all);
         ms : STRING := Exception_Message(self.migrant.all);
      begin
         free(self.migrant);
         self.migrant := NULL;
         Raise_Exception(id, ms);
      end migrate_exception;

   begin
      pragma Assert(self.id = Current_Task);

      -- target must be active
      await_target_attach;
      pragma Assert(target.id /= Current_Task);

      -- transfers control
      Notify(target.flag);
      Wait(self.flag);

      -- check if target raised an exception!
      if self.migrant /= NULL then
         migrate_exception;
      end if;
   end Resume;

   procedure Cancel(self: in out BASE_CONTROLLER; X: Ada.Exceptions.EXCEPTION_OCCURRENCE)
   -- warning: cannot call `Current_Task` here!
   is
      use Ada.Exceptions;

      function is_head(co: in out BASE_CONTROLLER'Class) return BOOLEAN with Inline is
         type PTR is not null access all BASE_CONTROLLER'Class;
      begin
         return PTR'(co.link) = PTR'(co'Unchecked_Access);
      end is_head;

      -- back: `invoker` for asymetric or `head` for symmetric
      back : BASE_CONTROLLER renames BASE_CONTROLLER(self.link.all);
   begin
      pragma Assert(self.id /= Null_Task_Id);
      pragma Assert(self.link /= NULL);
      if is_head(self) then
         raise Program_Error with "nowhere to migrate";
      end if;

      self.id := Null_Task_Id;
      self.link := NULL;

      -- migrate exception
      back.migrant := Save_Occurrence(X);
      Notify(back.flag);
   end Cancel;

   function Attached(self: in out BASE_CONTROLLER) return BOOLEAN is
   begin
      return self.id /= Null_Task_Id;
   end Attached;

   function Detached(self: in out BASE_CONTROLLER) return BOOLEAN is
   begin
      return self.id = Null_Task_Id;
   end Detached;

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
      Notify(invoker.flag);
      Wait(self.flag);
   end Yield;

   procedure Resume(target: in out ASYMMETRIC_CONTROLLER) is
      head : ASYMMETRIC_CONTROLLER;
   begin
      head.Resume(target);
   end;

   ---------------------------------------------------------------------
   -- Symmetric controller
   ---------------------------------------------------------------------

   procedure Jump(self, target: in out SYMMETRIC_CONTROLLER) is
   begin
      self.id := Null_Task_Id;
      self.link := NULL;
      Notify(target.flag);
   end Jump;

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

   procedure Resume(target: in out SYMMETRIC_CONTROLLER) is
      head : SYMMETRIC_CONTROLLER;
   begin
      head.Resume(target);
   end;

   ---------------------------------------------------------------------
   -- Syntactic sugar
   ---------------------------------------------------------------------

   procedure Resume(self: in out ASYMMETRIC_CONTROLLER; target: access ASYMMETRIC_CONTROLLER) is
   begin
      Resume(self, target.all);
   end Resume;

   procedure Resume(self: in out SYMMETRIC_CONTROLLER; target: access SYMMETRIC_CONTROLLER) is
   begin
      Resume(self, target.all);
   end Resume;

   procedure Jump(self: in out SYMMETRIC_CONTROLLER; target: access SYMMETRIC_CONTROLLER) is
   begin
      Jump(self, target.all);
   end Jump;

end Control;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
