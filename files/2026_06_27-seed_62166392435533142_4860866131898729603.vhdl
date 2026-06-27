-- Seed: 62166392435533142,4860866131898729603

entity jtxqg is
  port (vzkpykh : linkage real; ouralay : inout integer; mbvww : inout time; qqtgwglm : inout integer);
end jtxqg;

architecture gydi of jtxqg is
  
begin
  -- Single-driven assignments
  ouralay <= 1_2_2;
  mbvww <= 16#B_4_5_A_2# ns;
  qqtgwglm <= 2#00#;
end gydi;

library ieee;
use ieee.std_logic_1164.all;

entity tdzluvh is
  port (fqceyx : buffer time; csxohwr : linkage std_logic; p : in std_logic);
end tdzluvh;

architecture ldtlkrux of tdzluvh is
  
begin
  
end ldtlkrux;

library ieee;
use ieee.std_logic_1164.all;

entity ewalbk is
  port (ikimf : in bit_vector(2 downto 3); rxvrpphez : in std_logic; wlzi : out integer);
end ewalbk;

library ieee;
use ieee.std_logic_1164.all;

architecture afszdgbf of ewalbk is
  signal q : std_logic;
  signal kpvlxd : time;
  signal pvolz : integer;
  signal xwvrje : time;
  signal yurahs : real;
begin
  xjwrofcsg : entity work.jtxqg
    port map (vzkpykh => yurahs, ouralay => wlzi, mbvww => xwvrje, qqtgwglm => pvolz);
  dds : entity work.tdzluvh
    port map (fqceyx => kpvlxd, csxohwr => q, p => q);
  
  -- Multi-driven assignments
  q <= 'U';
  q <= 'H';
  q <= 'W';
end afszdgbf;



-- Seed after: 13233210761452221482,4860866131898729603
