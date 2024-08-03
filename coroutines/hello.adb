-- hello.adb

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

procedure hello is

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
      task type HELLO_TASK(Hello: access CONTROLLER);

      task body HELLO_TASK is
      begin
         Hello.Co_Begin;

         Put_Line("1-Hello, world!");

         Hello.Co_End;
      exception
         when X: others =>
            report_exception(X, "Oops at HELLO_TASK!");
      end HELLO_TASK;

      master : CONTROLLER;
      hello_control : aliased CONTROLLER;
      hello_thread  : HELLO_TASK (hello_control'Access);
   begin
      master.Resume(hello_control);
   end;

   ---------------------------------------------------------------------
   -- 2 TO BE ELIMINATED!
   ---------------------------------------------------------------------
 --declare
 --   task type HELLO_TASK(Hello: access CONTROLLER);

 --   task body HELLO_TASK is
 --   begin
 --      Hello.Co_Begin;

 --      Put_Line("2-Hello, world!");
 --   exception
 --      when X: others =>
 --         report_exception(X, "Oops at HELLO_TASK!");
 --   end HELLO_TASK;

 --   hello_control : aliased CONTROLLER;
 --   hello_thread  : HELLO_TASK (hello_control'Access);
 --begin
 --   hello_control.Go;
 --exception
 --   when X: others =>
 --      report_exception(X, "Oops at HELLO_TASK!");
 --end;

   ---------------------------------------------------------------------
   -- 3
   ---------------------------------------------------------------------
   declare
      task type HELLO_TASK(Hello: access CONTROLLER);

      task body HELLO_TASK is
      begin
         Hello.Co_Begin;

         Put("3-");      Hello.Yield;
         Put("Hello");   Hello.Yield;
         Put(", world"); Hello.Yield;
         Put_Line("!");

         Hello.Co_End;
      exception
         when X: others =>
            report_exception(X, "Oops at HELLO_TASK!");
      end HELLO_TASK;

      master : CONTROLLER;
      hello_control : aliased CONTROLLER;
      hello_thread  : HELLO_TASK (hello_control'Access);
   begin
      master.Resume(hello_control);
      master.Resume(hello_control);
      master.Resume(hello_control);
      master.Resume(hello_control);
   end;

   ---------------------------------------------------------------------
   -- 4
   ---------------------------------------------------------------------
   declare
      task type HELLO_TASK(Hello, Main: access CONTROLLER);

      task body HELLO_TASK is
      begin
         Hello.Co_Begin;

         Put("4-");      Hello.Resume(Main);
         Put("Hello");   Hello.Resume(Main);
         Put(", world"); Hello.Resume(Main);
         Put_Line("!");

         --Main.Go;
         Hello.Co_End;
      exception
         when X: others =>
            report_exception(X, "Oops at HELLO_TASK!");
      end HELLO_TASK;

      master : aliased CONTROLLER;
      hello_control : aliased CONTROLLER;
      hello_thread  : HELLO_TASK (hello_control'Access, master'Access);
   begin
      master.Resume(hello_control);
      master.Resume(hello_control);
      master.Resume(hello_control);
      master.Resume(hello_control);
   end;

end hello;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
