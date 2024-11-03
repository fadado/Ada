------------------------------------------------------------------------------
--  Hello world examples using controllers
------------------------------------------------------------------------------

pragma Assertion_Policy(Check); -- Check / Ignore

pragma Restrictions (
   No_Abort_Statements,
   No_Task_Allocators,
   No_Protected_Type_Allocators,
   No_Requeue_Statements,
   No_Local_Protected_Objects,
   No_Select_Statements
);

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Control; use Control;

with Gotcha;

procedure test_hello_1 is

begin
   Gotcha.Set_Handlers;

   ---------------------------------------------------------------------------
   --  Test 1 - Simple hello world
   ---------------------------------------------------------------------------
   Test_1:
   declare
      pragma Warnings (Off, "unreachable code");

      task type HELLO_RUN (self: not null access ASYMMETRIC_CONTROLLER);
      task body HELLO_RUN is
      begin
         self.Attach;
         Put_Line("Test 1-Hello, world!");
         self.Suspend; -- closed here
         self.Detach;
      exception
         when Exit_Controller => null;
         when X: others => self.Detach(X); raise;
      end HELLO_RUN;

      main          : ASYMMETRIC_CONTROLLER;
      hello_control : aliased ASYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_RUN (hello_control'Unchecked_Access);
   begin
      main.Transfer(hello_control);
      hello_control.Request_To_Exit;
   end Test_1;

   ---------------------------------------------------------------------------
   --  Test 2 - Asymmetric hello world
   ---------------------------------------------------------------------------
   Test_2:
   declare
      task type HELLO_RUN (self: not null access ASYMMETRIC_CONTROLLER);
      task body HELLO_RUN is
      begin
         self.Attach;
         Put("Test 2-"); self.Suspend;
         Put("Hello");   self.Suspend;
         Put(", world"); self.Suspend;
         Put_Line("!");
         self.Detach;
      exception
         when X: others => self.Detach(X); raise;
      end HELLO_RUN;

      main : ASYMMETRIC_CONTROLLER;
      hello_control : aliased ASYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_RUN (hello_control'Unchecked_Access);
   begin
      loop
         main.Transfer(hello_control);
         exit when hello_control.Status = DEAD;
         -- do something here...
      end loop;
   end Test_2;

   ---------------------------------------------------------------------------
   --  Test 3 - Symmetric hello world
   ---------------------------------------------------------------------------
   Test_3:
   declare
      task type HELLO_RUN (self, other: not null access SYMMETRIC_CONTROLLER);
      task body HELLO_RUN is
         invoker : SYMMETRIC_CONTROLLER renames other.all;
      begin
         self.Attach;
         Put("Test 3-"); self.Transfer(invoker);
         Put("Hello");   self.Transfer(invoker);
         Put(", world"); self.Transfer(invoker);
         Put_Line("!");
         self.Detach;
      exception
         when X: others => self.Detach(X); raise;
      end HELLO_RUN;

      main : aliased SYMMETRIC_CONTROLLER;
      hello_control : aliased SYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_RUN (hello_control'Unchecked_Access, main'Unchecked_Access);
   begin
      loop
         main.Transfer(hello_control);
         exit when hello_control.Status = DEAD;
         -- do something here...
      end loop;
   end Test_3;

   ---------------------------------------------------------------------------
   --  Test 4 - "Multiple" inheritance
   ---------------------------------------------------------------------------
   Test_4:
   declare
      task type HELLO_RUN (self: not null access ASYMMETRIC_CONTROLLER'Class);
      task body HELLO_RUN is
      begin
         self.Attach;
         Put_Line("Test 4-Hello, world!");
         self.Detach;
      exception
         when X: others => self.Detach(X); raise;
      end HELLO_RUN;

      type HELLO_COROUTINE is limited new ASYMMETRIC_CONTROLLER with
         record
            run : HELLO_RUN (HELLO_COROUTINE'Unchecked_Access);
         end record;

      main  : ASYMMETRIC_CONTROLLER;
      hello : HELLO_COROUTINE;
   begin
      main.Transfer(ASYMMETRIC_CONTROLLER(hello));
   end Test_4;

   ---------------------------------------------------------------------------
   --  Test 5 - "Multiple" inheritance
   ---------------------------------------------------------------------------
   Test_5:
   declare
      type HELLO_COROUTINE is tagged;

      task type HELLO_RUN (self: not null access HELLO_COROUTINE);

      type HELLO_COROUTINE is limited new ASYMMETRIC_CONTROLLER with
         record
            run : HELLO_RUN (HELLO_COROUTINE'Unchecked_Access);
         end record;

      task body HELLO_RUN is
      begin
         self.Attach;
         Put_Line("Test 5-Hello, world!");
         self.Detach;
      exception
         when X: others => self.Detach(X); raise;
      end HELLO_RUN;

      main  : ASYMMETRIC_CONTROLLER;
      hello : HELLO_COROUTINE;
   begin
      main.Transfer(ASYMMETRIC_CONTROLLER(hello));
   end Test_5;

   ---------------------------------------------------------------------------
   --  Test 6 - "Multiple" inheritance
   ---------------------------------------------------------------------------
   Test_6:
   declare
      package Hello_Package is
         type HELLO_COROUTINE is limited new ASYMMETRIC_CONTROLLER with private;

         overriding
         procedure Detach(self: in out HELLO_COROUTINE;
                          X: Ada.Exceptions.EXCEPTION_OCCURRENCE);

         task type HELLO_RUN (self: not null access ASYMMETRIC_CONTROLLER'Class);

      private
         type HELLO_COROUTINE is limited new ASYMMETRIC_CONTROLLER with
            record
               run : HELLO_RUN (HELLO_COROUTINE'Unchecked_Access);
            end record;
      end Hello_Package;

      package body Hello_Package is
         overriding
         procedure Detach(self: in out HELLO_COROUTINE;
                          X: Ada.Exceptions.EXCEPTION_OCCURRENCE)
         is
            super : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self);
         begin
            super.Detach(X);
         end Detach;

         task body HELLO_RUN is
         begin
            self.Attach;
            Put_Line("Test 6-Hello, world!");
            self.Detach;
         exception
            when X: others => self.Detach(X); raise;
         end HELLO_RUN;
      end Hello_Package;

      main  : ASYMMETRIC_CONTROLLER;
      hello : Hello_Package.HELLO_COROUTINE;
   begin
      main.Transfer(ASYMMETRIC_CONTROLLER(hello));
   end Test_6;

exception
   when X : others =>
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_hello_1;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
