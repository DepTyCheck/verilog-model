-- Seed: 11745298420288525079,14629254427735353553

entity kfzrbb is
  port (iy : in integer; cvnu : out real; ftzqrofeh : linkage string(3 to 1); ywylcedz : buffer integer);
end kfzrbb;

architecture loxpkbqevl of kfzrbb is
  
begin
  -- Single-driven assignments
  cvnu <= 2#0_1_0_1.011#;
end loxpkbqevl;

entity xwhnjqj is
  port (u : inout integer);
end xwhnjqj;

architecture hjppzsaf of xwhnjqj is
  
begin
  -- Single-driven assignments
  u <= 2#01111#;
end hjppzsaf;

library ieee;
use ieee.std_logic_1164.all;

entity ezwgp is
  port (ieurcvun : buffer std_logic);
end ezwgp;

architecture whyzpnmwqb of ezwgp is
  signal sc : integer;
  signal seioupc : integer;
begin
  tvb : entity work.xwhnjqj
    port map (u => seioupc);
  qfy : entity work.xwhnjqj
    port map (u => sc);
end whyzpnmwqb;

library ieee;
use ieee.std_logic_1164.all;

entity vhgabvvuk is
  port (y : linkage integer_vector(0 downto 0); ojlnp : inout std_logic; vgcycznhve : inout real);
end vhgabvvuk;

architecture ibelmqpz of vhgabvvuk is
  signal qvs : integer;
  signal ikqvhpdoj : string(3 to 1);
  signal cbcbnda : integer;
  signal jcwofcbqmt : string(3 to 1);
  signal rc : real;
  signal plvm : integer;
begin
  mlxpsaktl : entity work.kfzrbb
    port map (iy => plvm, cvnu => rc, ftzqrofeh => jcwofcbqmt, ywylcedz => cbcbnda);
  di : entity work.ezwgp
    port map (ieurcvun => ojlnp);
  vtu : entity work.kfzrbb
    port map (iy => plvm, cvnu => vgcycznhve, ftzqrofeh => ikqvhpdoj, ywylcedz => qvs);
  
  -- Multi-driven assignments
  ojlnp <= 'L';
end ibelmqpz;



-- Seed after: 6023946147479621045,14629254427735353553
