-- Seed: 12829014497724436830,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (eqg : out time; qudegve : out std_logic_vector(2 downto 2); xxbkvq : buffer integer; dxx : in real_vector(2 downto 3));
end h;

architecture zrsgyhckhn of h is
  
begin
  -- Single-driven assignments
  xxbkvq <= 13;
  eqg <= 1 hr;
  
  -- Multi-driven assignments
  qudegve <= (others => 'W');
end zrsgyhckhn;

library ieee;
use ieee.std_logic_1164.all;

entity pqbvhqaqx is
  port (oywdxznj : buffer std_logic);
end pqbvhqaqx;

library ieee;
use ieee.std_logic_1164.all;

architecture fxbv of pqbvhqaqx is
  signal yq : integer;
  signal fpkqj : time;
  signal jzc : integer;
  signal qlsw : std_logic_vector(2 downto 2);
  signal ppnemqkq : time;
  signal bp : real_vector(2 downto 3);
  signal kldmue : integer;
  signal fm : std_logic_vector(2 downto 2);
  signal uvpulg : time;
begin
  zffecpem : entity work.h
    port map (eqg => uvpulg, qudegve => fm, xxbkvq => kldmue, dxx => bp);
  kotjgcft : entity work.h
    port map (eqg => ppnemqkq, qudegve => qlsw, xxbkvq => jzc, dxx => bp);
  pp : entity work.h
    port map (eqg => fpkqj, qudegve => fm, xxbkvq => yq, dxx => bp);
  
  -- Single-driven assignments
  bp <= (others => 0.0);
  
  -- Multi-driven assignments
  fm <= (others => 'U');
  oywdxznj <= 'L';
  oywdxznj <= 'U';
  qlsw <= "L";
end fxbv;

entity mop is
  port (mn : out character);
end mop;

architecture siwotakpa of mop is
  
begin
  -- Single-driven assignments
  mn <= 'o';
end siwotakpa;



-- Seed after: 8857015164503821635,3924983747739634027
