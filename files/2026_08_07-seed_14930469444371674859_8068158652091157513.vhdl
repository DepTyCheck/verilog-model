-- Seed: 14930469444371674859,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity hwcf is
  port (bre : linkage time; enmrp : buffer std_logic_vector(3 to 0); tudcodkavn : in time);
end hwcf;

architecture gwvc of hwcf is
  
begin
  
end gwvc;

library ieee;
use ieee.std_logic_1164.all;

entity tjmjavrh is
  port (aacqvrfbyg : buffer integer; cn : linkage bit; krxdor : buffer std_logic_vector(2 to 0));
end tjmjavrh;

architecture vojzvfw of tjmjavrh is
  
begin
  -- Single-driven assignments
  aacqvrfbyg <= 0;
  
  -- Multi-driven assignments
  krxdor <= "";
  krxdor <= (others => '0');
  krxdor <= krxdor;
end vojzvfw;

library ieee;
use ieee.std_logic_1164.all;

entity jhw is
  port (g : buffer time; igb : inout std_logic_vector(3 downto 4); ke : buffer severity_level);
end jhw;

library ieee;
use ieee.std_logic_1164.all;

architecture m of jhw is
  signal tz : time;
  signal fjv : std_logic_vector(3 to 0);
  signal fv : std_logic_vector(2 to 0);
  signal nr : bit;
  signal j : integer;
  signal tkhpu : bit;
  signal s : integer;
begin
  wgbmonribr : entity work.tjmjavrh
    port map (aacqvrfbyg => s, cn => tkhpu, krxdor => igb);
  wz : entity work.tjmjavrh
    port map (aacqvrfbyg => j, cn => nr, krxdor => fv);
  lnhrdmioy : entity work.hwcf
    port map (bre => g, enmrp => fjv, tudcodkavn => tz);
  cctjfvgi : entity work.hwcf
    port map (bre => tz, enmrp => igb, tudcodkavn => g);
  
  -- Single-driven assignments
  ke <= ke;
  
  -- Multi-driven assignments
  fv <= "";
  igb <= igb;
end m;



-- Seed after: 16655338537025062494,8068158652091157513
