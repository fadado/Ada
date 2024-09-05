-- test_hello.adb

pragma Restrictions (
   No_Select_Statements,
   No_Task_Allocators,
   No_Protected_Type_Allocators,
   No_Requeue_Statements,
   No_Local_Protected_Objects,
   No_Abort_Statements
);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Control; use Control;

procedure test_hello is

   procedure report_exception(X: EXCEPTION_OCCURRENCE; S: STRING) is
      msg : STRING := Exception_Message(X);
   begin
      Put_Line(Standard_Error, S);
      Put_Line(Standard_Error, Exception_Name(X));
      Put_Line(Standard_Error, Exception_Information(X));
      if msg /= "" then
         Put_Line(Standard_Error, msg);
      end if;
   end report_exception;

begin
   ---------------------------------------------------------------------
   -- 1
   ---------------------------------------------------------------------
   declare
      task type HELLO_TASK(self: ASYMMETRIC_COROUTINE);

      task body HELLO_TASK is
      begin
         self.Attach;

         Put_Line("1-Hello, world!");

         self.Detach;
      exception
         when X: others =>
            report_exception(X, "Oops at HELLO_TASK! Use ^C to kill me!");
      end HELLO_TASK;

      main : ASYMMETRIC_CONTROLLER;
      hello_control : aliased ASYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_TASK (hello_control'Unchecked_Access);
   begin
      main.Resume(hello_control);
   exception
      when X: others =>
         report_exception(X, "Oops at MAIN_TASK! Use ^C to kill me!");
   end;

   ---------------------------------------------------------------------
   -- 2
   ---------------------------------------------------------------------
   declare
      task type HELLO_TASK(self: ASYMMETRIC_COROUTINE);

      package Common is
         char : CHARACTER;
         more : BOOLEAN := TRUE;
      end Common;

      task body HELLO_TASK is
         msg : STRING := "2-Hello, world!";
         len : POSITIVE := msg'Length;
      begin
         self.Attach;

         for i in 1..len loop
            Common.char := msg(i);
            if i = len then
               Common.more := FALSE;
            else
               self.Yield;
            end if;
         end loop;

         self.Detach;
      exception
         when X: others =>
            report_exception(X, "Oops at HELLO_TASK! Use ^C to kill me!");
      end HELLO_TASK;

      hello_control : aliased ASYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_TASK (hello_control'Unchecked_Access);

      main : ASYMMETRIC_CONTROLLER;

      function getchar(c: in out CHARACTER) return BOOLEAN with Inline is
      begin
         if Common.more then
            main.Resume(hello_control);
            c := Common.Char;
            return TRUE;
         else 
            return FALSE;
         end if;
      end getchar;

      char : CHARACTER := ' ';

   begin
      while getchar(char) loop
         Put(char);
      end loop;
      New_Line;

   exception
      when X: others =>
         report_exception(X, "Oops at MAIN_TASK! Use ^C to kill me!");
   end;

   ---------------------------------------------------------------------
   -- 3
   ---------------------------------------------------------------------
   declare
      task type HELLO_TASK(self: ASYMMETRIC_COROUTINE);

      task body HELLO_TASK is
      begin
         self.Attach;

         Put("3-");      self.Yield;
         Put("Hello");   self.Yield;
         Put(", world"); self.Yield;
         Put_Line("!");

         self.Detach;
      exception
         when X: others =>
            report_exception(X, "Oops at HELLO_TASK! Use ^C to kill me!");
      end HELLO_TASK;

      main : ASYMMETRIC_CONTROLLER;
      hello_control : aliased ASYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_TASK (hello_control'Unchecked_Access);
   begin
      main.Resume(hello_control);
      main.Resume(hello_control);
      main.Resume(hello_control);
      main.Resume(hello_control);
   exception
      when X: others =>
         report_exception(X, "Oops at MAIN_TASK! Use ^C to kill me!");
   end;

   ---------------------------------------------------------------------
   -- 4
   ---------------------------------------------------------------------
   declare
      task type HELLO_TASK(self, invoker: SYMMETRIC_COROUTINE);

      task body HELLO_TASK is
      begin
         self.Attach;

         Put("4-");      self.Resume(invoker);
         Put("Hello");   self.Resume(invoker);
         Put(", world"); self.Resume(invoker);
         Put_Line("!");

         self.Detach;
      exception
         when X: others =>
            report_exception(X, "Oops at HELLO_TASK! Use ^C to kill me!");
      end HELLO_TASK;

      main : aliased SYMMETRIC_CONTROLLER;
      hello_control : aliased SYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_TASK (hello_control'Unchecked_Access, main'Unchecked_Access);
   begin
      main.Resume(hello_control);
      main.Resume(hello_control);
      main.Resume(hello_control);
      main.Resume(hello_control);
   exception
      when X: others =>
         report_exception(X, "Oops at MAIN_TASK! Use ^C to kill me!");
   end;

   ---------------------------------------------------------------------
   -- 5
   ---------------------------------------------------------------------
   declare
      task type HELLO_TASK(self: ASYMMETRIC_COROUTINE);

      task body HELLO_TASK is
      begin
         self.Attach;

         Put_Line("5-Hello, world!");

         self.Detach;
      exception
         when X: others =>
            report_exception(X, "Oops at HELLO_TASK! Use ^C to kill me!");
      end HELLO_TASK;

      type HELLO_COROUTINE is new ASYMMETRIC_CONTROLLER with
         record
            run : HELLO_TASK(HELLO_COROUTINE'Unchecked_Access);
         end record;

      main  : ASYMMETRIC_CONTROLLER;
      hello : HELLO_COROUTINE;
   begin
      main.Resume(ASYMMETRIC_CONTROLLER(hello));
   exception
      when X: others =>
         report_exception(X, "Oops at MAIN_TASK! Use ^C to kill me!");
   end;

end test_hello;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
