-- test_pingpong.adb

pragma Restrictions (
-- No_Abort_Statements,
   No_Task_Allocators,
   No_Protected_Type_Allocators,
   No_Requeue_Statements,
   No_Local_Protected_Objects,
   No_Select_Statements
);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Control; use Control;
with Gotcha;

procedure test_pingpong is

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type PING_RUN(Ping, Pong: not null access SYMMETRIC_CONTROLLER);
   task type PONG_RUN(Pong, Ping: not null access SYMMETRIC_CONTROLLER);

   task body PING_RUN is
   begin
      Ping.Attach;

      for i in 1..10 loop
         Put("PING!  ");
         --raise Program_Error;
         if i < 10 then
            Ping.Resume(Pong);
         else
            Ping.Jump(Pong); -- transfer without suspension
         end if;
      end loop;

   exception -- TODO!
      when X: others => Ping.Cancel(X); raise;
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

   exception -- TODO!
      when X: others => Pong.Cancel(X); raise;
   end PONG_RUN;

begin
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   declare
      head : SYMMETRIC_CONTROLLER;
      ping_control : aliased SYMMETRIC_CONTROLLER;
      pong_control : aliased SYMMETRIC_CONTROLLER;
      ping_runner  : PING_RUN (ping_control'Unchecked_Access,
                               pong_control'Unchecked_Access);
      pong_runner  : PONG_RUN (pong_control'Unchecked_Access,
                               ping_control'Unchecked_Access);
   begin
      Put_Line("The players are ready...");
      New_Line;

      head.Resume(ping_control);

      New_Line;
      Put_Line("Game Over");
   exception
      when X: others =>
         --TODO!
         abort ping_runner, pong_runner;
         Gotcha.Report_Exception(X, "Oops at HEAD TASK!!");
   end;

end test_pingpong;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
