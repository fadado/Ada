------------------------------------------------------------------------------
--  Symmetric ping/pong
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Control; use Control;

with Gotcha;

procedure test_ctrl_pingpong
is
   task type Ping_Run(This, That: not null CONTROLLER_ACCESS);
   task type Pong_Run(This, That: not null CONTROLLER_ACCESS);

   --------------
   -- Ping_Run --
   --------------

   task body Ping_Run is
      Ping : CONTROLLER_TYPE renames This.all;
      Pong : CONTROLLER_TYPE renames That.all;
   begin
      Ping.Commence;

      for i in 1..10 loop
         Put("PING!  ");
         if i < 10 then
            Ping.Transfer(Pong);
         end if;
      end loop;
      Ping.Transfer(Pong, FALSE);

      Ping.Quit;
   exception
      when Exit_Controller => null;
      when X: others => Ping.Quit(X); raise;
   end Ping_Run;

   --------------
   -- Pong_Run --
   --------------

   task body Pong_Run is
      Ping : CONTROLLER_TYPE renames This.all;
      Pong : CONTROLLER_TYPE renames That.all;
   begin
      Pong.Commence;

      for i in 1..10 loop
         Put_Line("PONG!");
         if i < 10 then
            Pong.Transfer(Ping);
         end if;
      end loop;

      Pong.Quit;
   exception
      when Exit_Controller => null;
      when X: others => Pong.Quit(X); raise;
   end Pong_Run;

begin
   -- MAIN

   Gotcha.Set_Handlers;

   declare
      dispatcher : DISPATCHER_TYPE;

      ping_control : aliased CONTROLLER_TYPE;
      pong_control : aliased CONTROLLER_TYPE;
      ping_runner  : Ping_Run (ping_control'Unchecked_Access,
                               pong_control'Unchecked_Access);
      pong_runner  : Pong_Run (ping_control'Unchecked_Access,
                               pong_control'Unchecked_Access);
   begin
      Put_Line("The players are ready...");

      begin
         dispatcher.Resume(ping_control);
      exception
         when Stop_Iteration => null;
      end;

      --TODO: bug: wait end of players
      while not ping_runner'Terminated loop null; end loop;
      while not pong_runner'Terminated loop null; end loop;

      Put_Line("Game Over");
      New_Line;
   exception
      when X: others =>
         Gotcha.Report_Exception(X, "Oops at MASTER TASK!!");
   end;

end test_ctrl_pingpong;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
