-- Seed: 16989278894727503238,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity gj is
  port (mmn : buffer std_logic);
end gj;

architecture lgo of gj is
  
begin
  -- Multi-driven assignments
  mmn <= 'U';
  mmn <= mmn;
end lgo;

library ieee;
use ieee.std_logic_1164.all;

entity xhatwyh is
  port (up : inout bit; wpuyjnqrr : inout bit_vector(1 to 4); agtjtcmrl : out bit; mzehjtd : in std_logic_vector(3 to 3));
end xhatwyh;

library ieee;
use ieee.std_logic_1164.all;

architecture nb of xhatwyh is
  signal ycblkr : std_logic;
  signal a : std_logic;
begin
  iwadyasi : entity work.gj
    port map (mmn => a);
  m : entity work.gj
    port map (mmn => a);
  qg : entity work.gj
    port map (mmn => a);
  fcyw : entity work.gj
    port map (mmn => ycblkr);
  
  -- Single-driven assignments
  agtjtcmrl <= agtjtcmrl;
  wpuyjnqrr <= wpuyjnqrr;
  up <= agtjtcmrl;
  
  -- Multi-driven assignments
  ycblkr <= a;
  a <= 'Z';
end nb;



-- Seed after: 5438130187222084418,4122021602305298647
