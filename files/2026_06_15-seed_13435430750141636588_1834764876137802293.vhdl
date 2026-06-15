-- Seed: 13435430750141636588,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity xveitefxfi is
  port (nfsaryxnq : in std_logic_vector(3 downto 1); hmtjzfils : linkage std_logic_vector(3 downto 0));
end xveitefxfi;

architecture o of xveitefxfi is
  
begin
  
end o;

library ieee;
use ieee.std_logic_1164.all;

entity qidz is
  port (urcqbjo : inout std_logic; dk : buffer severity_level);
end qidz;

library ieee;
use ieee.std_logic_1164.all;

architecture cipuxx of qidz is
  signal cnxyrdc : std_logic_vector(3 downto 0);
  signal chjauagsf : std_logic_vector(3 downto 1);
begin
  igpd : entity work.xveitefxfi
    port map (nfsaryxnq => chjauagsf, hmtjzfils => cnxyrdc);
  
  -- Single-driven assignments
  dk <= FAILURE;
  
  -- Multi-driven assignments
  chjauagsf <= "ZZZ";
  urcqbjo <= 'Z';
end cipuxx;



-- Seed after: 18140961565868176094,1834764876137802293
