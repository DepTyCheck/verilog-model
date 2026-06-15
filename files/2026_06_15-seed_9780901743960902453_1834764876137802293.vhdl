-- Seed: 9780901743960902453,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity fsr is
  port (ys : in boolean; fpfkf : linkage std_logic_vector(1 to 1); afvu : inout boolean);
end fsr;

architecture zyen of fsr is
  
begin
  -- Single-driven assignments
  afvu <= FALSE;
end zyen;

library ieee;
use ieee.std_logic_1164.all;

entity sf is
  port (hfcoudhv : in std_logic_vector(1 downto 3); dbahbmzxzf : inout time; mgkp : inout bit);
end sf;

library ieee;
use ieee.std_logic_1164.all;

architecture kjywd of sf is
  signal xju : boolean;
  signal wlbewbhqpo : boolean;
  signal wrhfvduzf : boolean;
  signal uwlr : std_logic_vector(1 to 1);
  signal ikiiupqe : boolean;
  signal u : std_logic_vector(1 to 1);
  signal jok : boolean;
begin
  rrhko : entity work.fsr
    port map (ys => jok, fpfkf => u, afvu => jok);
  dcfumpj : entity work.fsr
    port map (ys => ikiiupqe, fpfkf => uwlr, afvu => wrhfvduzf);
  lwtty : entity work.fsr
    port map (ys => wlbewbhqpo, fpfkf => u, afvu => xju);
  
  -- Single-driven assignments
  mgkp <= '0';
  dbahbmzxzf <= 4 sec;
  
  -- Multi-driven assignments
  uwlr <= (others => 'L');
  u <= "Z";
  uwlr <= (others => '1');
end kjywd;



-- Seed after: 14279232346997254975,1834764876137802293
