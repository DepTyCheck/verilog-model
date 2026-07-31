-- Seed: 13644898894283672692,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity svjnhehk is
  port (gyjsqiwjir : in std_logic_vector(2 downto 1));
end svjnhehk;

architecture uv of svjnhehk is
  
begin
  
end uv;

entity k is
  port (dklrfqgajs : in time);
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture wbwuh of k is
  signal hhlkye : std_logic_vector(2 downto 1);
  signal dpwpan : std_logic_vector(2 downto 1);
  signal nkpkkgqa : std_logic_vector(2 downto 1);
begin
  wrmezazdp : entity work.svjnhehk
    port map (gyjsqiwjir => nkpkkgqa);
  smrhwnizrn : entity work.svjnhehk
    port map (gyjsqiwjir => dpwpan);
  okfhrzoo : entity work.svjnhehk
    port map (gyjsqiwjir => hhlkye);
  
  -- Multi-driven assignments
  dpwpan <= ('0', '1');
end wbwuh;

library ieee;
use ieee.std_logic_1164.all;

entity attdfjypra is
  port (tc : in std_logic_vector(0 downto 2); hood : in std_logic);
end attdfjypra;

library ieee;
use ieee.std_logic_1164.all;

architecture rl of attdfjypra is
  signal ujlwjm : time;
  signal aqwtelcbdy : std_logic_vector(2 downto 1);
  signal qv : std_logic_vector(2 downto 1);
  signal azy : std_logic_vector(2 downto 1);
begin
  l : entity work.svjnhehk
    port map (gyjsqiwjir => azy);
  hab : entity work.svjnhehk
    port map (gyjsqiwjir => qv);
  cgwpicxiyx : entity work.svjnhehk
    port map (gyjsqiwjir => aqwtelcbdy);
  yph : entity work.k
    port map (dklrfqgajs => ujlwjm);
  
  -- Single-driven assignments
  ujlwjm <= 2#1010.1000# us;
  
  -- Multi-driven assignments
  azy <= azy;
end rl;



-- Seed after: 4065574556666249082,4177195558088809003
