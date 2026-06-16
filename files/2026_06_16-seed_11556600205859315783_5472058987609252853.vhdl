-- Seed: 11556600205859315783,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity tgb is
  port (nskf : in std_logic);
end tgb;

architecture qv of tgb is
  
begin
  
end qv;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (qlmwxubys : inout std_logic; wyomnxitm : out time);
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture gujgcqk of x is
  signal hst : std_logic;
begin
  duzff : entity work.tgb
    port map (nskf => qlmwxubys);
  lvgy : entity work.tgb
    port map (nskf => hst);
  
  -- Single-driven assignments
  wyomnxitm <= 3_0_3 ps;
  
  -- Multi-driven assignments
  qlmwxubys <= 'W';
  qlmwxubys <= '1';
  qlmwxubys <= 'W';
  hst <= 'L';
end gujgcqk;



-- Seed after: 6154501890929235995,5472058987609252853
