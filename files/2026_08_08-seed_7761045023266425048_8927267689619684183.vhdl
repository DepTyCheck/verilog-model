-- Seed: 7761045023266425048,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity dcvufxmxg is
  port (ckgbhydnu : buffer time; ewygvcszi : out std_logic; rmmkfonnwq : inout time_vector(3 downto 2));
end dcvufxmxg;

architecture myo of dcvufxmxg is
  
begin
  -- Single-driven assignments
  rmmkfonnwq <= rmmkfonnwq;
  ckgbhydnu <= ckgbhydnu;
  
  -- Multi-driven assignments
  ewygvcszi <= ewygvcszi;
  ewygvcszi <= '0';
  ewygvcszi <= ewygvcszi;
end myo;

entity icdmchw is
  port (aqpnbdx : in time; w : inout boolean);
end icdmchw;

library ieee;
use ieee.std_logic_1164.all;

architecture yndpfxpkam of icdmchw is
  signal mtgix : time_vector(3 downto 2);
  signal xtscwh : time;
  signal ljiqrlpv : time_vector(3 downto 2);
  signal udixhbiaf : time;
  signal diqulvu : time_vector(3 downto 2);
  signal djqii : std_logic;
  signal ds : time;
  signal isnxpnazm : time_vector(3 downto 2);
  signal pl : std_logic;
  signal arb : time;
begin
  ze : entity work.dcvufxmxg
    port map (ckgbhydnu => arb, ewygvcszi => pl, rmmkfonnwq => isnxpnazm);
  lz : entity work.dcvufxmxg
    port map (ckgbhydnu => ds, ewygvcszi => djqii, rmmkfonnwq => diqulvu);
  huhacm : entity work.dcvufxmxg
    port map (ckgbhydnu => udixhbiaf, ewygvcszi => pl, rmmkfonnwq => ljiqrlpv);
  cibsktgcu : entity work.dcvufxmxg
    port map (ckgbhydnu => xtscwh, ewygvcszi => pl, rmmkfonnwq => mtgix);
  
  -- Single-driven assignments
  w <= w;
  
  -- Multi-driven assignments
  djqii <= 'Z';
  pl <= 'W';
end yndpfxpkam;



-- Seed after: 301039058723148297,8927267689619684183
