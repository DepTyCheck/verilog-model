-- Seed: 9423909096323985045,14094562573555574003

entity tirawqx is
  port (ooo : linkage integer; nwgxndts : in time; pmbsvcnnl : in real);
end tirawqx;

architecture zx of tirawqx is
  
begin
  
end zx;

entity i is
  port (lfagwevwo : linkage real);
end i;

architecture vztk of i is
  signal laiparhbc : integer;
  signal hxplmyratc : real;
  signal je : time;
  signal lhrgkc : integer;
begin
  qvccfzyq : entity work.tirawqx
    port map (ooo => lhrgkc, nwgxndts => je, pmbsvcnnl => hxplmyratc);
  gzuznzkew : entity work.tirawqx
    port map (ooo => laiparhbc, nwgxndts => je, pmbsvcnnl => hxplmyratc);
end vztk;

entity trdeydp is
  port (sonnbgq : in time_vector(4 downto 1); yn : linkage time);
end trdeydp;

architecture ipkbj of trdeydp is
  signal jjjzxzal : real;
  signal aodtffgjcm : time;
  signal zfkouuk : integer;
  signal rtzdttms : real;
begin
  sin : entity work.i
    port map (lfagwevwo => rtzdttms);
  q : entity work.tirawqx
    port map (ooo => zfkouuk, nwgxndts => aodtffgjcm, pmbsvcnnl => jjjzxzal);
  
  -- Single-driven assignments
  jjjzxzal <= rtzdttms;
  aodtffgjcm <= 2#0_0_1_0_0# fs;
end ipkbj;

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (rnq : out std_logic; ugt : in time);
end u;

architecture vo of u is
  signal yjjofysu : real;
  signal sxzcagah : time;
  signal mxj : time_vector(4 downto 1);
begin
  wmep : entity work.trdeydp
    port map (sonnbgq => mxj, yn => sxzcagah);
  xxsdmkvkb : entity work.i
    port map (lfagwevwo => yjjofysu);
  
  -- Single-driven assignments
  mxj <= (1.0 ps, 3.401 fs, 2#1_0_1.1_1_1# fs, 220.1222 us);
end vo;



-- Seed after: 12184090590074106029,14094562573555574003
