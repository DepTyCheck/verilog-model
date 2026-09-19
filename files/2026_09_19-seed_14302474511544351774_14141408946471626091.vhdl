-- Seed: 14302474511544351774,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity drglknff is
  port (hzqtaqz : out std_logic; oowwmjj : in integer);
end drglknff;

architecture nhrg of drglknff is
  
begin
  -- Multi-driven assignments
  hzqtaqz <= hzqtaqz;
  hzqtaqz <= '-';
  hzqtaqz <= hzqtaqz;
  hzqtaqz <= '1';
end nhrg;

library ieee;
use ieee.std_logic_1164.all;

entity drkmz is
  port (bkp : buffer time; awcm : in std_logic);
end drkmz;

library ieee;
use ieee.std_logic_1164.all;

architecture n of drkmz is
  signal rpteozcfum : std_logic;
  signal fiin : integer;
  signal k : std_logic;
  signal efax : integer;
  signal ilalepu : std_logic;
begin
  uyssoq : entity work.drglknff
    port map (hzqtaqz => ilalepu, oowwmjj => efax);
  fsqjxvy : entity work.drglknff
    port map (hzqtaqz => k, oowwmjj => efax);
  aocxywe : entity work.drglknff
    port map (hzqtaqz => ilalepu, oowwmjj => fiin);
  m : entity work.drglknff
    port map (hzqtaqz => rpteozcfum, oowwmjj => efax);
  
  -- Single-driven assignments
  bkp <= 4_3_0.2_1_3_3 ps;
end n;

entity vop is
  port (vrqpvbje : buffer time; mqa : out boolean_vector(2 downto 2); jtdfdy : in string(5 to 3));
end vop;

library ieee;
use ieee.std_logic_1164.all;

architecture orp of vop is
  signal vhcyqqi : std_logic;
  signal adp : integer;
  signal yru : std_logic;
  signal lw : integer;
  signal jnkote : std_logic;
begin
  hussjfxnjx : entity work.drglknff
    port map (hzqtaqz => jnkote, oowwmjj => lw);
  lx : entity work.drglknff
    port map (hzqtaqz => yru, oowwmjj => adp);
  jhwplt : entity work.drglknff
    port map (hzqtaqz => vhcyqqi, oowwmjj => adp);
  ztsmcpwkl : entity work.drglknff
    port map (hzqtaqz => jnkote, oowwmjj => lw);
  
  -- Single-driven assignments
  mqa <= (others => FALSE);
  lw <= lw;
  vrqpvbje <= 8#21723.4635# ps;
end orp;

library ieee;
use ieee.std_logic_1164.all;

entity gufy is
  port (vrxlpz : in integer_vector(4 downto 3); upayjn : out time_vector(4 downto 1); aeztt : in std_logic_vector(1 downto 2); kfdkh : linkage real);
end gufy;

library ieee;
use ieee.std_logic_1164.all;

architecture tmftmh of gufy is
  signal o : time;
  signal psc : integer;
  signal bsilr : std_logic;
begin
  jvzq : entity work.drglknff
    port map (hzqtaqz => bsilr, oowwmjj => psc);
  tjucipcyk : entity work.drkmz
    port map (bkp => o, awcm => bsilr);
  
  -- Single-driven assignments
  psc <= psc;
  upayjn <= (3_1_3_2_0 ms, 16#F# ps, 16#4_7_6.B_7# us, 2#0010.0111# ns);
  
  -- Multi-driven assignments
  bsilr <= bsilr;
end tmftmh;



-- Seed after: 11576452151340979059,14141408946471626091
