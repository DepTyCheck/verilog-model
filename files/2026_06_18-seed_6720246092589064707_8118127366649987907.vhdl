-- Seed: 6720246092589064707,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity drowmxz is
  port (icbuflxig : out real_vector(4 downto 0); le : buffer time; tdjgbah : buffer std_logic_vector(4 downto 0); frcbzmwmot : linkage time);
end drowmxz;

architecture iymvs of drowmxz is
  
begin
  -- Single-driven assignments
  le <= 8#54465# us;
  
  -- Multi-driven assignments
  tdjgbah <= ('W', 'U', 'W', '1', 'Z');
  tdjgbah <= "0WW-X";
end iymvs;

entity bpv is
  port (yt : inout time; ahlkxgecx : inout real_vector(4 downto 3));
end bpv;

library ieee;
use ieee.std_logic_1164.all;

architecture naoqcsbk of bpv is
  signal iuregygh : time;
  signal h : std_logic_vector(4 downto 0);
  signal ll : real_vector(4 downto 0);
begin
  zor : entity work.drowmxz
    port map (icbuflxig => ll, le => yt, tdjgbah => h, frcbzmwmot => iuregygh);
  
  -- Single-driven assignments
  ahlkxgecx <= (8#0.556#, 2#0011.0_1_0_0#);
end naoqcsbk;



-- Seed after: 12201997451822227686,8118127366649987907
