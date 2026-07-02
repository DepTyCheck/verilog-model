-- Seed: 3687480680168215491,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity yhh is
  port (vdzqcv : in std_logic);
end yhh;

architecture lioxvgw of yhh is
  
begin
  
end lioxvgw;

entity evtfgwzlji is
  port (vze : out bit);
end evtfgwzlji;

library ieee;
use ieee.std_logic_1164.all;

architecture juv of evtfgwzlji is
  signal bfnjhqe : std_logic;
  signal srwpjvei : std_logic;
begin
  ug : entity work.yhh
    port map (vdzqcv => srwpjvei);
  jmqimkldqa : entity work.yhh
    port map (vdzqcv => srwpjvei);
  fbjzudjbz : entity work.yhh
    port map (vdzqcv => bfnjhqe);
  
  -- Single-driven assignments
  vze <= '0';
  
  -- Multi-driven assignments
  srwpjvei <= '-';
  srwpjvei <= 'X';
end juv;



-- Seed after: 17793306910554509041,13694093582652240945
