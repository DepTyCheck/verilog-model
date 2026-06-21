-- Seed: 14549990361722685420,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity odrxqnm is
  port (ytjytdqbo : out std_logic_vector(2 downto 2); nyvjfoklg : buffer real; nie : inout std_logic_vector(0 to 1));
end odrxqnm;

architecture s of odrxqnm is
  
begin
  -- Multi-driven assignments
  nie <= ('L', 'X');
  ytjytdqbo <= "-";
  nie <= "XX";
  nie <= "00";
end s;

entity dfktlmthg is
  port (codvnzloh : linkage integer; dp : out time);
end dfktlmthg;

library ieee;
use ieee.std_logic_1164.all;

architecture ocrni of dfktlmthg is
  signal jhbjkppgcs : std_logic_vector(0 to 1);
  signal ekxrd : real;
  signal yhs : std_logic_vector(2 downto 2);
begin
  j : entity work.odrxqnm
    port map (ytjytdqbo => yhs, nyvjfoklg => ekxrd, nie => jhbjkppgcs);
  
  -- Single-driven assignments
  dp <= 8#3_4_0_3_2.5# ns;
  
  -- Multi-driven assignments
  yhs <= "0";
  yhs <= (others => 'X');
  yhs <= "H";
end ocrni;



-- Seed after: 9313062138078072592,3687118713772291287
