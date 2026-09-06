-- Seed: 3286682117041906020,14094562573555574003

entity joah is
  port (slyalg : inout time; jcrulnvq : in integer; di : out string(3 downto 5));
end joah;

architecture yli of joah is
  
begin
  -- Single-driven assignments
  slyalg <= 0 hr;
  di <= di;
end yli;

library ieee;
use ieee.std_logic_1164.all;

entity yixlcu is
  port (zmlresbyju : buffer std_logic; lxio : buffer bit_vector(2 downto 3));
end yixlcu;

architecture qgikxfxqe of yixlcu is
  signal x : string(3 downto 5);
  signal nve : integer;
  signal qcwaevy : time;
  signal l : string(3 downto 5);
  signal xxkummb : integer;
  signal xcuf : time;
begin
  gkekzcwu : entity work.joah
    port map (slyalg => xcuf, jcrulnvq => xxkummb, di => l);
  ylqkxgl : entity work.joah
    port map (slyalg => qcwaevy, jcrulnvq => nve, di => x);
  
  -- Multi-driven assignments
  zmlresbyju <= 'U';
end qgikxfxqe;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (zcfdrwi : linkage std_logic; qsbvlkgrp : out integer);
end s;

architecture bldjwrzp of s is
  signal jcqwrexkc : string(3 downto 5);
  signal lf : integer;
  signal rjrx : time;
  signal xicbbx : string(3 downto 5);
  signal iits : integer;
  signal jsaiamnz : time;
begin
  hvblwpwcda : entity work.joah
    port map (slyalg => jsaiamnz, jcrulnvq => iits, di => xicbbx);
  namong : entity work.joah
    port map (slyalg => rjrx, jcrulnvq => lf, di => jcqwrexkc);
  
  -- Single-driven assignments
  qsbvlkgrp <= 2_1;
end bldjwrzp;

entity pzgguzruft is
  port (fvuiesnssw : linkage real);
end pzgguzruft;

library ieee;
use ieee.std_logic_1164.all;

architecture t of pzgguzruft is
  signal lozjggpaq : bit_vector(2 downto 3);
  signal vqwm : string(3 downto 5);
  signal quczziotzl : integer;
  signal kmgtfpwbkv : time;
  signal tjbbhfvau : bit_vector(2 downto 3);
  signal vgcirkbb : std_logic;
begin
  wyly : entity work.yixlcu
    port map (zmlresbyju => vgcirkbb, lxio => tjbbhfvau);
  ofaegwln : entity work.joah
    port map (slyalg => kmgtfpwbkv, jcrulnvq => quczziotzl, di => vqwm);
  qgydigfw : entity work.yixlcu
    port map (zmlresbyju => vgcirkbb, lxio => lozjggpaq);
  
  -- Single-driven assignments
  quczziotzl <= 2#0_1_0#;
  
  -- Multi-driven assignments
  vgcirkbb <= vgcirkbb;
  vgcirkbb <= '1';
end t;



-- Seed after: 14512095216737959280,14094562573555574003
