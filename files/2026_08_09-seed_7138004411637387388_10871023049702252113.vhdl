-- Seed: 7138004411637387388,10871023049702252113

library ieee;
use ieee.std_logic_1164.all;

entity ir is
  port (qyqih : linkage std_logic; dzct : out std_logic_vector(2 downto 1));
end ir;

architecture xj of ir is
  
begin
  -- Multi-driven assignments
  dzct <= ('W', 'L');
end xj;

entity g is
  port (nvygyus : inout integer_vector(2 to 4));
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture dhxig of g is
  signal uzp : std_logic_vector(2 downto 1);
  signal a : std_logic;
begin
  l : entity work.ir
    port map (qyqih => a, dzct => uzp);
  
  -- Single-driven assignments
  nvygyus <= (2#11#, 16#A_9_5_1#, 4);
end dhxig;



-- Seed after: 5127947369646289778,10871023049702252113
