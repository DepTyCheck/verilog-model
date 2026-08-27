-- Seed: 6150386108382546703,6299883410057943775

library ieee;
use ieee.std_logic_1164.all;

entity fygpxe is
  port (o : buffer std_logic; paxp : in std_logic; reaksu : linkage std_logic_vector(0 downto 3); afs : in std_logic);
end fygpxe;

architecture qqudxu of fygpxe is
  
begin
  -- Multi-driven assignments
  o <= 'U';
end qqudxu;

entity k is
  port (ir : inout real);
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture sq of k is
  signal hy : std_logic;
  signal ofxjniq : std_logic_vector(0 downto 3);
  signal eufubratr : std_logic;
  signal drqmeau : std_logic;
  signal xtxds : std_logic;
  signal oezbbecv : std_logic_vector(0 downto 3);
  signal vcupujrvat : std_logic;
  signal oy : std_logic;
  signal fdecc : std_logic;
  signal hxycpw : std_logic_vector(0 downto 3);
  signal zlva : std_logic;
begin
  deqzpfgxhd : entity work.fygpxe
    port map (o => zlva, paxp => zlva, reaksu => hxycpw, afs => fdecc);
  ftsrdza : entity work.fygpxe
    port map (o => oy, paxp => vcupujrvat, reaksu => oezbbecv, afs => xtxds);
  cvu : entity work.fygpxe
    port map (o => zlva, paxp => drqmeau, reaksu => hxycpw, afs => eufubratr);
  vaf : entity work.fygpxe
    port map (o => zlva, paxp => oy, reaksu => ofxjniq, afs => hy);
  
  -- Multi-driven assignments
  hxycpw <= hxycpw;
  fdecc <= hy;
end sq;



-- Seed after: 4004235160200590234,6299883410057943775
