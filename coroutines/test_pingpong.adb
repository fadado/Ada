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

   task type PING_RUN(Ping, Pong: SYMMETRIC_COROUTINE);
   task type PONG_RUN(Pong, Ping: SYMMETRIC_COROUTINE);

   task body PING_RUN is
   begin
      Ping.Attach;

      for i in 1..10 loop
         Put("PING!  ");
         if i < 10 then
            Ping.Resume(Pong);
         else
            Ping.Detach(Pong); -- transfer without suspension
         end if;
      end loop;

   exception
      when X: others =>
         report_exception(X, "Oops at PING_RUN!");
   end PING_RUN;

   task body PONG_RUN is
   begin
      Pong.Attach;

      for i in 1..10 loop
         Put_Line("PONG!");
         if i < 10 then
            Pong.Resume(Ping);
         else
            Pong.Detach;
         end if;
      end loop;

   exception
      when X: others =>
         report_exception(X, "Oops at PONG_RUN!");
   end PONG_RUN;

begin
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   declare
      main : SYMMETRIC_CONTROLLER;
      ping_control : aliased SYMMETRIC_CONTROLLER;
      pong_control : aliased SYMMETRIC_CONTROLLER;
      ping_runner : PING_RUN (ping_control'Unchecked_Access,
                              pong_control'Unchecked_Access);
      pong_runner : PONG_RUN (pong_control'Unchecked_Access,
                              ping_control'Unchecked_Access);
   begin
      Put_Line("The players are ready...");
      New_Line;

      main.Resume(ping_control);

      New_Line;
      Put_Line("Game Over");
   exception
      when X: others =>
         report_exception(X, "Oops at MAIN TASK! Use ^C to kill me!");
   end;

end test_pingpong;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
