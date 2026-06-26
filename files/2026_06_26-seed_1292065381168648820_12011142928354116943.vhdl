-- Seed: 1292065381168648820,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity dvmpchj is
  port (aildnru : inout std_logic_vector(2 downto 0));
end dvmpchj;

architecture mxaefh of dvmpchj is
  
begin
  -- Multi-driven assignments
  aildnru <= "X0L";
  aildnru <= "L-1";
end mxaefh;

library ieee;
use ieee.std_logic_1164.all;

entity hrjnkth is
  port (ywfxooe : buffer real; ik : out std_logic);
end hrjnkth;

library ieee;
use ieee.std_logic_1164.all;

architecture bqamfy of hrjnkth is
  signal xheytzjm : std_logic_vector(2 downto 0);
begin
  klkt : entity work.dvmpchj
    port map (aildnru => xheytzjm);
  whygdho : entity work.dvmpchj
    port map (aildnru => xheytzjm);
  
  -- Multi-driven assignments
  xheytzjm <= "WLW";
end bqamfy;



-- Seed after: 11930378998340942665,12011142928354116943
