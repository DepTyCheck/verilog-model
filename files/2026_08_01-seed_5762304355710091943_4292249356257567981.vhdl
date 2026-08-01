-- Seed: 5762304355710091943,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity sxpua is
  port (qa : buffer std_logic_vector(3 downto 2); nbmdnikue : out integer);
end sxpua;

architecture ara of sxpua is
  
begin
  -- Single-driven assignments
  nbmdnikue <= 8#10#;
  
  -- Multi-driven assignments
  qa <= qa;
  qa <= qa;
  qa <= ('L', 'W');
  qa <= ('0', 'X');
end ara;

entity kinnevzh is
  port (xjtwuxmnit : linkage real; pew : inout time);
end kinnevzh;

architecture w of kinnevzh is
  
begin
  -- Single-driven assignments
  pew <= 16#E.5_A_D_E_0# ps;
end w;

entity qiwi is
  port (hjbzfh : out integer; wafvur : in time);
end qiwi;

library ieee;
use ieee.std_logic_1164.all;

architecture qitcpw of qiwi is
  signal odjycsmgpi : integer;
  signal lgs : std_logic_vector(3 downto 2);
begin
  qojgbhe : entity work.sxpua
    port map (qa => lgs, nbmdnikue => odjycsmgpi);
  so : entity work.sxpua
    port map (qa => lgs, nbmdnikue => hjbzfh);
  
  -- Multi-driven assignments
  lgs <= ('0', 'H');
end qitcpw;



-- Seed after: 3644022388948424083,4292249356257567981
