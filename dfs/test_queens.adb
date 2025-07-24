-- Solves the
--    https://en.wikipedia.org/wiki/Eight_queens_puzzle

with Ada.Text_IO;

with Backtracker;

procedure test_queens is

   type ROW_INDEX    is range 1..8;
   type COLUMN_INDEX is new ROW_INDEX;
   type CHESS_BOARD  is array(ROW_INDEX) of COLUMN_INDEX;

   type LESS_DIAGONAL_ID is range ROW_INDEX'First - ROW_INDEX'Last
                               .. ROW_INDEX'Last  - ROW_INDEX'First;
   type PLUS_DIAGONAL_ID is range ROW_INDEX'First + ROW_INDEX'First
                               .. ROW_INDEX'Last  + ROW_INDEX'Last;

   Used_Column   : array (COLUMN_INDEX) of BOOLEAN := (others => FALSE);
   Less_Diagonal : array (ROW_INDEX) of LESS_DIAGONAL_ID;
   Plus_Diagonal : array (ROW_INDEX) of PLUS_DIAGONAL_ID;

   function Rejected
     (board : CHESS_BOARD;
      row   : ROW_INDEX;
      col   : COLUMN_INDEX) return BOOLEAN
   is
   begin
      if Used_Column(col) then
         return TRUE;
      end if;
      declare
         less_id : constant LESS_DIAGONAL_ID
            := LESS_DIAGONAL_ID(INTEGER(row) - INTEGER(col));
      begin
         if (for some r in ROW_INDEX'First .. ROW_INDEX'Pred(row)
               => less_id = Less_Diagonal(r))
         then
            return TRUE;
         end if;
      end;
      declare
         plus_id : constant PLUS_DIAGONAL_ID
            := PLUS_DIAGONAL_ID(INTEGER(row) + INTEGER(col));
      begin
         if (for some r in ROW_INDEX'First .. ROW_INDEX'Pred(row)
               => plus_id = Plus_Diagonal(r))
         then
            return TRUE;
         end if;
      end;
      return FALSE;
   end;

   procedure Enter
     (board : CHESS_BOARD;
      row   : ROW_INDEX;
      col   : COLUMN_INDEX)
   is
   begin
      Used_Column(col) := TRUE;
      Less_Diagonal(row) := LESS_DIAGONAL_ID(INTEGER(row) - INTEGER(col));
      Plus_Diagonal(row) := PLUS_DIAGONAL_ID(INTEGER(row) + INTEGER(col));
   end;

   procedure Leave
     (board : CHESS_BOARD;
      row   : ROW_INDEX;
      col   : COLUMN_INDEX)
   is
   begin
      Used_Column(col) := FALSE;
   end;

   procedure Output(board: CHESS_BOARD)
   is
      use Ada.Text_IO;
   begin
      for column of board loop
         Put(column'Image);
      end loop;
      New_Line;
   end Output;

begin
   declare
      package Queens_8 is
         new Backtracker (
           NODE_VALUE      => COLUMN_INDEX,
           VECTOR_INDEX    => ROW_INDEX,
           VECTOR_SOLUTION => CHESS_BOARD,
           Found           => Output,
           Rejected        => Rejected,
           Enter           => Enter,
           Leave           => Leave
         );
   begin
      Queens_8.Traverse;
   end;

end test_queens;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
