-- sieve.adb

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Bounded_Synchronized_Queues;

use Ada;

procedure sieve is
   ------------------------------------------------------------
   -- Set of numbers to search for primes
   ------------------------------------------------------------

   subtype NUMBER is POSITIVE;

   Close_Filter : constant NUMBER := 1;
   -- We use 1 as a token to signal task's ending

   ------------------------------------------------------------
   -- Channels between tasks
   ------------------------------------------------------------

   Queue_Size : constant := 2;
   -- A greater size does not improve performance

   package SQI is
      new Containers.Synchronized_Queue_Interfaces (
         Element_Type => NUMBER
      );
   package BSQ is
      new Containers.Bounded_Synchronized_Queues (
         Queue_Interfaces => SQI,
         Default_Capacity => Queue_Size
      );

   subtype SYNCRONIZED_QUEUE is BSQ.Queue;
   type QUEUE is access SYNCRONIZED_QUEUE;

   ------------------------------------------------------------
   -- Task to generate odd numbers starting at 3
   ------------------------------------------------------------

   task type ODD_NUMBERS_GENERATOR (
      Limit        : NUMBER;
      Output_Queue : QUEUE
   );
   type GENERATOR is access ODD_NUMBERS_GENERATOR;

   task body ODD_NUMBERS_GENERATOR is
      candidate : NUMBER := 3;
   begin
      while candidate <= Limit loop
         Output_Queue.Enqueue(candidate);
         candidate := candidate + 2;
      end loop;
      Output_Queue.Enqueue(Close_Filter);
   end ODD_NUMBERS_GENERATOR;

   ------------------------------------------------------------
   -- Task to reject (or pass) prime cadidates
   ------------------------------------------------------------

   task type FILTER_PRIME_MULTIPLES (
      Input_Queue  : QUEUE;
      Output_Queue : QUEUE;
      Prime        : NUMBER
   );
   type FILTER is access FILTER_PRIME_MULTIPLES;

   task body FILTER_PRIME_MULTIPLES is
      function Is_Multiple(n : NUMBER) return BOOLEAN
         is (n rem Prime = 0) with Inline;
      candidate : NUMBER;
   begin
      loop
         Input_Queue.Dequeue(candidate);
         exit when candidate = Close_Filter;
         if Is_Multiple(candidate) then
            null;
         else
            Output_Queue.Enqueue(candidate);
         end if;
      end loop;
      Output_Queue.Enqueue(Close_Filter);
   end FILTER_PRIME_MULTIPLES;

   ------------------------------------------------------------
   -- Build a chain of filters
   ------------------------------------------------------------

   procedure Print with Inline;
   procedure Print(N: NUMBER) with Inline;

   procedure Build_Sieve(Limit: NUMBER) is
      prime         : NUMBER := 2; -- the only even prime
      input, output : QUEUE;
   begin
      Print(prime);

      input := new SYNCRONIZED_QUEUE;
      declare
         G : GENERATOR;
      begin
         G := new ODD_NUMBERS_GENERATOR (Limit, input);
      end;

      loop
         input.Dequeue(prime);
         exit when prime = Close_Filter;
         Print(prime);

         output := new SYNCRONIZED_QUEUE;
         declare
            F : FILTER;
         begin
            F := new FILTER_PRIME_MULTIPLES (input, output, prime);
         end;
         input := output;
      end loop;
      Print;
   end Build_Sieve;

   ------------------------------------------------------------
   -- Output utilities
   ------------------------------------------------------------

   Count : NATURAL := 0;

   procedure Print is
      use Text_IO;
   begin
      New_Line;
   end;

   procedure Print(N: NUMBER) is
      use Text_IO;
      use Integer_Text_IO;
      Field_Size : constant := 7;
      Columns    : constant := 10;
   begin
      Count := Count + 1;
      Put(N, Width => Field_Size);
      if (Count rem Columns) = 0 then
         New_Line;
      end if;
   end;

------------------------------------------------------------------------
-- Manage command line and start the sieve
------------------------------------------------------------------------

begin
   declare
      use Command_Line;
      procedure Usage is
         use Text_IO;
      begin
         Put_Line(Standard_Error, "Usage: ./sieve [LIMIT]");
         Put_Line(Standard_Error, "LIMIT must by a number greater than 1");
      end;
      limit : NUMBER;
   begin
      Set_Exit_Status(Failure);
      if Argument_Count = 0 then
         limit := 541; -- first 100 primes
      else
         begin
            limit := NUMBER'Value(Argument(1));
            if limit = 1 then raise Constraint_Error; end if;
         exception
            when Constraint_Error => Usage; return;
         end;
      end if;
      Build_Sieve(Limit => limit);
      Set_Exit_Status(Success);
   end;
end sieve;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
