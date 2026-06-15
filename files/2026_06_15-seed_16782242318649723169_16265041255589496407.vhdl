-- Seed: 16782242318649723169,16265041255589496407

library ieee;
use ieee.std_logic_1164.all;

entity spe is
  port (ezimdn : buffer std_logic_vector(0 downto 4));
end spe;



architecture lcu of spe is
  
begin
  
end lcu;

library ieee;
use ieee.std_logic_1164.all;

entity npfmsir is
  port (pyjdehbg : inout boolean; s : out std_logic_vector(4 to 3); hawb : out time);
end npfmsir;

library ieee;
use ieee.std_logic_1164.all;

architecture ucncvg of npfmsir is
  signal zjsh : std_logic_vector(0 downto 4);
begin
  hv : entity work.spe
    port map (ezimdn => s);
  c : entity work.spe
    port map (ezimdn => s);
  iz : entity work.spe
    port map (ezimdn => zjsh);
end ucncvg;



-- Seed after: 3062826570898748768,16265041255589496407
