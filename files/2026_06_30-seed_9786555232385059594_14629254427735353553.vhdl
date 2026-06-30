-- Seed: 9786555232385059594,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity vsx is
  port (lxyvp : inout real; wejojgq : in std_logic_vector(3 downto 2); wpzqzi : in boolean);
end vsx;

architecture fatxkf of vsx is
  
begin
  -- Single-driven assignments
  lxyvp <= 0_4_1_1.0_3_2_1;
end fatxkf;

library ieee;
use ieee.std_logic_1164.all;

entity lfo is
  port (towvuzex : in time; pkkomks : out std_logic);
end lfo;

library ieee;
use ieee.std_logic_1164.all;

architecture jdmpgc of lfo is
  signal wo : real;
  signal bz : boolean;
  signal uxwlatyzp : std_logic_vector(3 downto 2);
  signal ggcfbyqkw : real;
begin
  ufkbalzaj : entity work.vsx
    port map (lxyvp => ggcfbyqkw, wejojgq => uxwlatyzp, wpzqzi => bz);
  wrpizgnnp : entity work.vsx
    port map (lxyvp => wo, wejojgq => uxwlatyzp, wpzqzi => bz);
  
  -- Single-driven assignments
  bz <= FALSE;
  
  -- Multi-driven assignments
  pkkomks <= 'Z';
end jdmpgc;



-- Seed after: 9461960429631928242,14629254427735353553
