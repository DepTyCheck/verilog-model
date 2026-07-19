-- Seed: 15364450984330630682,5511103086789671269

entity vg is
  port (udv : inout integer);
end vg;

architecture mzjdpjyz of vg is
  
begin
  -- Single-driven assignments
  udv <= 0_2;
end mzjdpjyz;

library ieee;
use ieee.std_logic_1164.all;

entity cg is
  port (qvgg : inout std_logic_vector(4 to 4); dugqonki : buffer time_vector(1 to 3); bbu : in time);
end cg;

architecture geijuu of cg is
  signal sz : integer;
  signal orh : integer;
  signal mwxwyon : integer;
  signal phfyivbpd : integer;
begin
  l : entity work.vg
    port map (udv => phfyivbpd);
  pzn : entity work.vg
    port map (udv => mwxwyon);
  dsklsnt : entity work.vg
    port map (udv => orh);
  gpvy : entity work.vg
    port map (udv => sz);
  
  -- Multi-driven assignments
  qvgg <= (others => '-');
end geijuu;



-- Seed after: 11841017861742388832,5511103086789671269
