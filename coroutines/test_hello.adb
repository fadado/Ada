------------------------------------------------------------------------------
--  Hello world examples using Controllers
------------------------------------------------------------------------------

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Control.Routines;
use Control;

with Gotcha;

procedure test_hello is

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
         when Exit_Controller => self.Die;
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
   --  Test 2 - "Multiple" inheritance
   ---------------------------------------------------------------------------
   Test_2:
   declare
      task type HELLO_RUN (self: not null access ASYMMETRIC_CONTROLLER'Class);
      task body HELLO_RUN is
      begin
         self.Attach;
         Put_Line("Test 2-Hello, world!");
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
   end Test_2;

   ---------------------------------------------------------------------------
   --  Test 3 - "Multiple" inheritance
   ---------------------------------------------------------------------------
   Test_3:
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
         Put_Line("Test 3-Hello, world!");
         self.Detach;
      exception
         when X: others => self.Detach(X); raise;
      end HELLO_RUN;

      main  : ASYMMETRIC_CONTROLLER;
      hello : HELLO_COROUTINE;
   begin
      main.Transfer(ASYMMETRIC_CONTROLLER(hello));
   end Test_3;

   ---------------------------------------------------------------------------
   --  Test 4 - "Multiple" inheritance
   ---------------------------------------------------------------------------
   Test_4:
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
            Put_Line("Test 4-Hello, world!");
            self.Detach;
         exception
            when X: others => self.Detach(X); raise;
         end HELLO_RUN;
      end Hello_Package;

      main  : ASYMMETRIC_CONTROLLER;
      hello : Hello_Package.HELLO_COROUTINE;
   begin
      main.Transfer(ASYMMETRIC_CONTROLLER(hello));
   end Test_4;

   ---------------------------------------------------------------------------
   --  Test 5 - Routines example
   ---------------------------------------------------------------------------

   Test_5:
   declare
      pragma Warnings (Off, "unreachable code");

      type VOID is null record;

      package R is new Routines (Context_Type => VOID);
      use R;

      procedure hello_world(self: not null ROUTINE_ACCESS) is
      begin
         Put("Test 5-"); self.Yield;
         Put("Hello");   self.Yield;
         Put(", world"); self.Yield;
         Put_Line("!");
      end;

      hello : ROUTINE_TYPE (hello_world'Access, NULL);

   begin
      loop hello.Resume; end loop;
   exception
      when Stop_Iterator => null;
   end Test_5;

exception
   when X : others =>
      Gotcha.Report_Exception(X, "Handled exception at top level");

end test_hello;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
