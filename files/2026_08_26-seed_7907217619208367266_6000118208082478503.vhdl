-- Seed: 7907217619208367266,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (tmmvjtnqzb : inout std_logic; um : in integer; w : out std_logic_vector(1 downto 1));
end v;

architecture ovuploo of v is
  
begin
  -- Multi-driven assignments
  w <= (others => '-');
  w <= "L";
  tmmvjtnqzb <= 'X';
end ovuploo;

library ieee;
use ieee.std_logic_1164.all;

entity mefhkzn is
  port (xlzsz : inout std_logic_vector(0 to 2); sfghap : buffer integer; quyxkmrgl : in time; lsydoojvfx : buffer bit_vector(3 to 2));
end mefhkzn;

library ieee;
use ieee.std_logic_1164.all;

architecture vms of mefhkzn is
  signal hdkhc : std_logic_vector(1 downto 1);
  signal mnzayeyh : integer;
  signal iz : std_logic;
begin
  ztrhmewcuo : entity work.v
    port map (tmmvjtnqzb => iz, um => mnzayeyh, w => hdkhc);
  zr : entity work.v
    port map (tmmvjtnqzb => iz, um => sfghap, w => hdkhc);
  
  -- Single-driven assignments
  lsydoojvfx <= (others => '0');
  mnzayeyh <= 10;
  sfghap <= sfghap;
end vms;



-- Seed after: 9660700050830694826,6000118208082478503
