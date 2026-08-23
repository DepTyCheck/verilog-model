-- Seed: 4204084551515266637,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity dv is
  port (qzpnstolo : inout std_logic_vector(1 downto 2));
end dv;

architecture ls of dv is
  
begin
  -- Multi-driven assignments
  qzpnstolo <= (others => '0');
  qzpnstolo <= (others => '0');
  qzpnstolo <= qzpnstolo;
end ls;

entity mshgkjg is
  port (kx : buffer severity_level);
end mshgkjg;

library ieee;
use ieee.std_logic_1164.all;

architecture mrn of mshgkjg is
  signal akiobl : std_logic_vector(1 downto 2);
begin
  cwmmlmu : entity work.dv
    port map (qzpnstolo => akiobl);
  kjgxduqjo : entity work.dv
    port map (qzpnstolo => akiobl);
  
  -- Single-driven assignments
  kx <= kx;
  
  -- Multi-driven assignments
  akiobl <= akiobl;
  akiobl <= (others => '0');
end mrn;

library ieee;
use ieee.std_logic_1164.all;

entity wrtingfw is
  port (qzbncv : inout time; lz : out std_logic_vector(2 to 3); fc : linkage std_logic_vector(3 to 2); hmxxzc : linkage std_logic);
end wrtingfw;

architecture jcskl of wrtingfw is
  
begin
  -- Multi-driven assignments
  lz <= lz;
  lz <= ('U', 'W');
  lz <= lz;
  lz <= "WH";
end jcskl;



-- Seed after: 15812365437228591730,4245627776430562977
