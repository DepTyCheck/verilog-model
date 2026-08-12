-- Seed: 13858307570935684184,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (vbtlhao : in bit; nsilyov : in std_logic_vector(3 downto 4));
end c;

architecture bzjmtrokrf of c is
  
begin
  
end bzjmtrokrf;

entity yqxdol is
  port (aqsx : linkage severity_level);
end yqxdol;

library ieee;
use ieee.std_logic_1164.all;

architecture tl of yqxdol is
  signal glanyea : std_logic_vector(3 downto 4);
  signal dygeb : bit;
begin
  nbc : entity work.c
    port map (vbtlhao => dygeb, nsilyov => glanyea);
  
  -- Single-driven assignments
  dygeb <= '0';
  
  -- Multi-driven assignments
  glanyea <= glanyea;
  glanyea <= (others => '0');
  glanyea <= "";
end tl;



-- Seed after: 467670574651061298,8412319452373742525
