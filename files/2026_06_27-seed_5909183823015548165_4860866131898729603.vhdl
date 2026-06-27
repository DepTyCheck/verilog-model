-- Seed: 5909183823015548165,4860866131898729603

entity rcc is
  port (iwyvdspzds : buffer time_vector(1 to 3));
end rcc;

architecture hhusrkk of rcc is
  
begin
  -- Single-driven assignments
  iwyvdspzds <= (3 fs, 2#0_0_1_1_0# ms, 4 min);
end hhusrkk;

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (hcyslroab : inout time; yckpvrmp : inout std_logic_vector(0 to 2));
end c;

architecture w of c is
  signal t : time_vector(1 to 3);
  signal v : time_vector(1 to 3);
  signal hogdpcyya : time_vector(1 to 3);
begin
  msed : entity work.rcc
    port map (iwyvdspzds => hogdpcyya);
  d : entity work.rcc
    port map (iwyvdspzds => v);
  goz : entity work.rcc
    port map (iwyvdspzds => t);
  
  -- Single-driven assignments
  hcyslroab <= 16#10# us;
  
  -- Multi-driven assignments
  yckpvrmp <= ('X', 'W', '-');
  yckpvrmp <= "0HX";
  yckpvrmp <= "UUU";
  yckpvrmp <= "X11";
end w;

entity bdgiavh is
  port (nibvvcek : buffer character);
end bdgiavh;

architecture bfbt of bdgiavh is
  signal mflsxzvlq : time_vector(1 to 3);
begin
  nptl : entity work.rcc
    port map (iwyvdspzds => mflsxzvlq);
  
  -- Single-driven assignments
  nibvvcek <= 'p';
end bfbt;



-- Seed after: 8881938402082135404,4860866131898729603
