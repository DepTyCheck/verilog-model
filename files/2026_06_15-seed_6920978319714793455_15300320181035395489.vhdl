-- Seed: 6920978319714793455,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity dgteblcocx is
  port (zul : buffer std_logic_vector(4 downto 1));
end dgteblcocx;

architecture nhgppwpmy of dgteblcocx is
  
begin
  -- Multi-driven assignments
  zul <= "-HUX";
  zul <= ('X', 'W', 'U', 'Z');
  zul <= ('0', 'L', 'X', 'W');
end nhgppwpmy;

library ieee;
use ieee.std_logic_1164.all;

entity qai is
  port (extrx : buffer string(4 downto 4); gxjzdsuk : buffer std_logic; afjltji : buffer std_logic_vector(4 downto 4));
end qai;

library ieee;
use ieee.std_logic_1164.all;

architecture iaotnvunjh of qai is
  signal y : std_logic_vector(4 downto 1);
  signal jvkxr : std_logic_vector(4 downto 1);
  signal efyen : std_logic_vector(4 downto 1);
begin
  fmlutf : entity work.dgteblcocx
    port map (zul => efyen);
  vx : entity work.dgteblcocx
    port map (zul => jvkxr);
  n : entity work.dgteblcocx
    port map (zul => y);
  
  -- Single-driven assignments
  extrx <= "b";
  
  -- Multi-driven assignments
  gxjzdsuk <= 'W';
  afjltji <= "1";
  afjltji <= (others => 'L');
end iaotnvunjh;



-- Seed after: 4210118168309026503,15300320181035395489
