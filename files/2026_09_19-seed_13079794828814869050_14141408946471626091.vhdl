-- Seed: 13079794828814869050,14141408946471626091

entity wjkoiz is
  port (o : buffer boolean_vector(1 to 4); gxt : in boolean);
end wjkoiz;

architecture y of wjkoiz is
  
begin
  -- Single-driven assignments
  o <= (TRUE, FALSE, TRUE, TRUE);
end y;

library ieee;
use ieee.std_logic_1164.all;

entity esybft is
  port (azekigp : inout std_logic_vector(2 downto 3); t : in integer; uw : buffer std_logic_vector(2 downto 0));
end esybft;

architecture sljpa of esybft is
  signal xithpd : boolean;
  signal cxyl : boolean_vector(1 to 4);
begin
  h : entity work.wjkoiz
    port map (o => cxyl, gxt => xithpd);
  
  -- Single-driven assignments
  xithpd <= TRUE;
  
  -- Multi-driven assignments
  uw <= ('0', '1', 'X');
  uw <= uw;
  uw <= "X0H";
  uw <= uw;
end sljpa;



-- Seed after: 12837287262316404166,14141408946471626091
