-- test_pingpong.adb

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

procedure test_pingpong is

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

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type PING_TASK(Ping, Pong: access CONTROLLER);
   task type PONG_TASK(Pong, Ping: access CONTROLLER);

   task body PING_TASK is
   begin
      Ping.Attach;

      for i in 1..10 loop
         Put("PING!  ");
         if i < 10 then
            Ping.Transfer(Pong);
         else
            Ping.Detach(Pong); -- transfer without suspension
         end if;
      end loop;

   exception
      when X: others =>
         report_exception(X, "Oops at PING_TASK!");
   end PING_TASK;

   task body PONG_TASK is
   begin
      Pong.Attach;

      for i in 1..10 loop
         Put_Line("PONG!");
         if i < 10 then
            Pong.Transfer(Ping);
         else
            Pong.Detach;
         end if;
      end loop;

   exception
      when X: others =>
         report_exception(X, "Oops at PONG_TASK!");
   end PONG_TASK;

begin
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   declare
      master : CONTROLLER;
      ping_control : aliased CONTROLLER;
      pong_control : aliased CONTROLLER;
      ping_thread : PING_TASK (ping_control'Access, pong_control'Access);
      pong_thread : PONG_TASK (pong_control'Access, ping_control'Access);
   begin
      Put_Line("The players are ready...");
      New_Line;

      master.Transfer(ping_control);

      New_Line;
      Put_Line("Game Over");
   end;

end test_pingpong;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
