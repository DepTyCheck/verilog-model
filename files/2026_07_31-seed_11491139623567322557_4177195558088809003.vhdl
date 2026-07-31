-- Seed: 11491139623567322557,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity cpwwd is
  port (unjywqyxg : inout std_logic; pxkcs : linkage integer; qelgzbum : linkage time);
end cpwwd;

architecture knx of cpwwd is
  
begin
  -- Multi-driven assignments
  unjywqyxg <= '1';
  unjywqyxg <= unjywqyxg;
  unjywqyxg <= '-';
end knx;

entity jreinbvgq is
  port (mujtl : in time; drmloexcb : buffer real);
end jreinbvgq;

architecture i of jreinbvgq is
  
begin
  -- Single-driven assignments
  drmloexcb <= 1_4.340;
end i;

library ieee;
use ieee.std_logic_1164.all;

entity dvlr is
  port (vqplofj : out std_logic);
end dvlr;

architecture arbux of dvlr is
  signal nrz : time;
  signal zfv : integer;
  signal bmc : time;
  signal blgrdkvd : integer;
begin
  mwok : entity work.cpwwd
    port map (unjywqyxg => vqplofj, pxkcs => blgrdkvd, qelgzbum => bmc);
  ejr : entity work.cpwwd
    port map (unjywqyxg => vqplofj, pxkcs => zfv, qelgzbum => nrz);
  
  -- Multi-driven assignments
  vqplofj <= vqplofj;
  vqplofj <= 'H';
end arbux;

library ieee;
use ieee.std_logic_1164.all;

entity duacn is
  port (omxaiwcoks : linkage bit; bsmpz : inout std_logic_vector(2 to 3); pgnxiunq : linkage std_logic_vector(2 to 3); nf : buffer integer);
end duacn;

library ieee;
use ieee.std_logic_1164.all;

architecture rlvutsim of duacn is
  signal c : real;
  signal qix : real;
  signal yj : time;
  signal pygmd : std_logic;
begin
  itngzknb : entity work.dvlr
    port map (vqplofj => pygmd);
  htmmx : entity work.jreinbvgq
    port map (mujtl => yj, drmloexcb => qix);
  mj : entity work.jreinbvgq
    port map (mujtl => yj, drmloexcb => c);
  
  -- Single-driven assignments
  nf <= nf;
  yj <= 232.1 ps;
  
  -- Multi-driven assignments
  pygmd <= pygmd;
  bsmpz <= bsmpz;
  bsmpz <= bsmpz;
end rlvutsim;



-- Seed after: 7306438611729018630,4177195558088809003
