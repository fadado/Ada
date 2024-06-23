-- sieve.adb

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Bounded_Synchronized_Queues;
with Ada.Exceptions;
with GNAT.OS_Lib;

procedure sieve is
   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   LIMIT : constant := 999;

   subtype NUMBER is INTEGER range 1 .. INTEGER'Last;

   Count : NATURAL := 0;
 --pragma Atomic (Count);

   procedure Print(N: NUMBER) with Inline
   is
      use Ada.Text_IO;
      use Ada.Integer_Text_IO;
   begin
      Count := Count + 1;
      Put(N, Width => 7);
      if (Count rem 10) = 0 then New_Line; end if;
   end;

   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   package SQI is
      new Ada.Containers.Synchronized_Queue_Interfaces (
         Element_Type => NUMBER
      );
   package BSQ is
      new Ada.Containers.Bounded_Synchronized_Queues (
         Queue_Interfaces => SQI,
         Default_Capacity => 7
      );

   subtype QUEUE is BSQ.Queue;
   type access_QUEUE is access all QUEUE;

   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   task type Prime_Filter(Input_Channel: access_QUEUE);
   type access_FILTER is access Prime_Filter;

   function New_Filter(Channel: access_QUEUE)
     return access_FILTER
     with Inline is
   begin
      return F : access_FILTER do
         F := new Prime_Filter(Channel);
      end return;
   end New_Filter;

   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   task type Odds_Generator(Output_Channel: access_QUEUE);
   task body Odds_Generator is
      candidate : NUMBER := 3; -- start at the first odd prime
   begin
      -- print first prime
      Print(2);

      -- first filter
      run_filter:
         declare
            F : access_FILTER := New_Filter(Output_Channel);
         begin null; end run_filter;

      -- send odd numbers to filter
      while candidate <= LIMIT loop
         Output_Channel.Enqueue(candidate);
         candidate := candidate + 2;
      end loop;

      -- send 1 as a token to end and waits until exit
      Output_Channel.Enqueue(1);
      loop delay 1.1; end loop;
   end Odds_Generator;

   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   task body Prime_Filter is
      Output_Channel : aliased QUEUE;
      prime, candidate : NUMBER;
   begin
      -- receive a prime or 1
      Input_Channel.Dequeue(prime);

      -- exit?
      if prime = 1 then
         -- this is the last filter in the chain
         Ada.Text_IO.New_Line;
         GNAT.OS_Lib.OS_Exit(0); -- exit the program!
      end if;

      --
      Print(prime);

      -- search next primes
      run_filter:
         declare
            F : access_FILTER := New_Filter(Output_Channel'Unchecked_Access);
         begin null; end run_filter;
      --
      loop
         Input_Channel.Dequeue(candidate);
         if (candidate rem prime) /= 0 then
            Output_Channel.Enqueue(candidate);
         end if;
      end loop;
   end Prime_Filter;

------------------------------------------------------------------------
--
------------------------------------------------------------------------
begin
   run_generator:
      declare
         use Ada.Text_IO;
         use Ada.Exceptions;

         input : access_QUEUE := new QUEUE;
         --output : access_QUEUE;

         -- first task in the chain
         O : Odds_Generator(input);
      begin
         null;
      exception
         when X : others =>
            Put_Line(Standard_Error, Exception_Name(X));
            Put_Line(Standard_Error, Exception_Message(X));
            Put_Line(Standard_Error, Exception_Information(X));
      end run_generator;
end sieve;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
