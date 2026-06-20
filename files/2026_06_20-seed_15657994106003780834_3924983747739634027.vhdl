-- Seed: 15657994106003780834,3924983747739634027

entity te is
  port (cgpch : buffer time);
end te;

architecture my of te is
  
begin
  -- Single-driven assignments
  cgpch <= 044.4233 fs;
end my;

entity j is
  port (jxdgz : out real; k : buffer character; maoqsm : linkage integer; mmp : inout bit);
end j;

architecture bklclk of j is
  signal hafpwh : time;
  signal qekda : time;
  signal tbzsa : time;
begin
  uua : entity work.te
    port map (cgpch => tbzsa);
  mcse : entity work.te
    port map (cgpch => qekda);
  oueeb : entity work.te
    port map (cgpch => hafpwh);
  
  -- Single-driven assignments
  k <= 'z';
end bklclk;

library ieee;
use ieee.std_logic_1164.all;

entity rroeegtcpa is
  port (jg : inout time; zvy : in std_logic; f : buffer integer_vector(2 downto 4));
end rroeegtcpa;

architecture jlzwhn of rroeegtcpa is
  signal gdiz : bit;
  signal wwbug : integer;
  signal fbu : character;
  signal ijosy : real;
  signal kjp : time;
begin
  ccurr : entity work.te
    port map (cgpch => jg);
  jwpb : entity work.te
    port map (cgpch => kjp);
  kkaopzy : entity work.j
    port map (jxdgz => ijosy, k => fbu, maoqsm => wwbug, mmp => gdiz);
  
  -- Single-driven assignments
  f <= (others => 0);
end jlzwhn;



-- Seed after: 9519916829489855653,3924983747739634027
