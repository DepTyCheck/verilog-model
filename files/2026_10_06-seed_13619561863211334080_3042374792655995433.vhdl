-- Seed: 13619561863211334080,3042374792655995433

entity aldehftu is
  port (xmj : buffer bit_vector(3 to 4); ouvpdkjiu : buffer time_vector(1 downto 3); kzkqematbq : buffer integer);
end aldehftu;

architecture afcqhwdzsh of aldehftu is
  
begin
  -- Single-driven assignments
  ouvpdkjiu <= (others => 0 ns);
  xmj <= ('1', '1');
  kzkqematbq <= kzkqematbq;
end afcqhwdzsh;

library ieee;
use ieee.std_logic_1164.all;

entity wzk is
  port (i : inout std_logic_vector(2 to 4));
end wzk;

architecture qcjwi of wzk is
  signal ploguimr : integer;
  signal ga : time_vector(1 downto 3);
  signal yr : bit_vector(3 to 4);
begin
  cuasycqw : entity work.aldehftu
    port map (xmj => yr, ouvpdkjiu => ga, kzkqematbq => ploguimr);
  
  -- Multi-driven assignments
  i <= i;
  i <= "XXL";
  i <= i;
end qcjwi;

library ieee;
use ieee.std_logic_1164.all;

entity zfsh is
  port (pm : buffer std_logic_vector(3 downto 4); tvcrux : inout bit_vector(0 to 3));
end zfsh;

architecture fra of zfsh is
  
begin
  -- Single-driven assignments
  tvcrux <= tvcrux;
  
  -- Multi-driven assignments
  pm <= pm;
  pm <= pm;
  pm <= "";
  pm <= pm;
end fra;



-- Seed after: 8492945589216612650,3042374792655995433
