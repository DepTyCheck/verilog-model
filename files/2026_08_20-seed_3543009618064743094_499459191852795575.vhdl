-- Seed: 3543009618064743094,499459191852795575

entity lhs is
  port (rtbnmcch : in boolean_vector(1 to 0));
end lhs;

architecture fupgrsj of lhs is
  
begin
  
end fupgrsj;

library ieee;
use ieee.std_logic_1164.all;

entity pnf is
  port (gucumz : out severity_level; pcfbas : out std_logic);
end pnf;

architecture ftt of pnf is
  signal ly : boolean_vector(1 to 0);
  signal wx : boolean_vector(1 to 0);
begin
  mshdyeaks : entity work.lhs
    port map (rtbnmcch => wx);
  ohaka : entity work.lhs
    port map (rtbnmcch => wx);
  qbngtk : entity work.lhs
    port map (rtbnmcch => ly);
  ntmhw : entity work.lhs
    port map (rtbnmcch => ly);
  
  -- Single-driven assignments
  gucumz <= ERROR;
  wx <= (others => TRUE);
  
  -- Multi-driven assignments
  pcfbas <= '0';
  pcfbas <= pcfbas;
  pcfbas <= pcfbas;
  pcfbas <= pcfbas;
end ftt;

entity bokn is
  port (bemjzas : linkage character; egplb : out real);
end bokn;

architecture heev of bokn is
  signal iejgoweyl : boolean_vector(1 to 0);
begin
  ll : entity work.lhs
    port map (rtbnmcch => iejgoweyl);
  
  -- Single-driven assignments
  iejgoweyl <= (others => TRUE);
  egplb <= egplb;
end heev;



-- Seed after: 6034923842967041139,499459191852795575
