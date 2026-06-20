-- Seed: 16260863137089682844,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity qpzsmsumn is
  port (pechqwg : buffer real; s : inout std_logic_vector(4 to 3); pallnqjufs : out bit);
end qpzsmsumn;

architecture lc of qpzsmsumn is
  
begin
  -- Single-driven assignments
  pallnqjufs <= '1';
  
  -- Multi-driven assignments
  s <= (others => '0');
  s <= "";
end lc;

library ieee;
use ieee.std_logic_1164.all;

entity fyred is
  port (q : inout real_vector(0 to 1); glr : inout std_logic; nevvloin : out real; fwucinep : linkage real_vector(3 to 4));
end fyred;

library ieee;
use ieee.std_logic_1164.all;

architecture lunmt of fyred is
  signal vbpcaxqbg : bit;
  signal mqvpd : std_logic_vector(4 to 3);
begin
  dudcdrgx : entity work.qpzsmsumn
    port map (pechqwg => nevvloin, s => mqvpd, pallnqjufs => vbpcaxqbg);
  
  -- Single-driven assignments
  q <= (3_2_0_0_2.2_0_3, 2_0_1_0.3_0_2_4_2);
  
  -- Multi-driven assignments
  glr <= '-';
end lunmt;



-- Seed after: 18048055090585140511,3924983747739634027
