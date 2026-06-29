-- Seed: 7112211087097502020,17047277710231705797

entity tzkp is
  port (sxc : inout real);
end tzkp;

architecture mbzwbr of tzkp is
  
begin
  -- Single-driven assignments
  sxc <= 2#01110.10#;
end mbzwbr;

library ieee;
use ieee.std_logic_1164.all;

entity arjdasp is
  port (xer : out std_logic_vector(4 to 3); ogqqufwvnq : in std_logic; rayn : inout integer);
end arjdasp;

architecture eglaqh of arjdasp is
  signal qbjbaa : real;
begin
  ro : entity work.tzkp
    port map (sxc => qbjbaa);
  
  -- Multi-driven assignments
  xer <= "";
  xer <= (others => '0');
  xer <= "";
  xer <= "";
end eglaqh;

entity fne is
  port (hnh : inout severity_level; sxwdgblceu : in integer; fgfi : inout string(5 to 3));
end fne;

library ieee;
use ieee.std_logic_1164.all;

architecture xxshjv of fne is
  signal xyibc : real;
  signal akblxpl : integer;
  signal puaarh : std_logic;
  signal yseoz : std_logic_vector(4 to 3);
begin
  okeybt : entity work.arjdasp
    port map (xer => yseoz, ogqqufwvnq => puaarh, rayn => akblxpl);
  rstpxyquf : entity work.tzkp
    port map (sxc => xyibc);
  
  -- Single-driven assignments
  hnh <= FAILURE;
  fgfi <= (others => ' ');
  
  -- Multi-driven assignments
  puaarh <= 'H';
  yseoz <= (others => '0');
end xxshjv;

entity pv is
  port (tgrwqu : inout character);
end pv;

architecture tkdwlmj of pv is
  signal d : real;
  signal uvg : string(5 to 3);
  signal cbadnsik : integer;
  signal zuqr : severity_level;
  signal f : real;
  signal cgt : real;
begin
  kyz : entity work.tzkp
    port map (sxc => cgt);
  pcvs : entity work.tzkp
    port map (sxc => f);
  cwmgxel : entity work.fne
    port map (hnh => zuqr, sxwdgblceu => cbadnsik, fgfi => uvg);
  xvpidzpm : entity work.tzkp
    port map (sxc => d);
  
  -- Single-driven assignments
  tgrwqu <= 'w';
  cbadnsik <= 212;
end tkdwlmj;



-- Seed after: 4791925245978321292,17047277710231705797
