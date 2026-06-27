-- Seed: 15728519154964654492,4860866131898729603

entity nlyvvmtst is
  port (fcwimfoom : inout real; pswfephe : linkage real; aejvsywlvi : out time; cbgeyczsh : buffer real);
end nlyvvmtst;

architecture hk of nlyvvmtst is
  
begin
  -- Single-driven assignments
  fcwimfoom <= 1.0413;
  aejvsywlvi <= 8#7_4_5_1_5# us;
  cbgeyczsh <= 8#6_7.6_3#;
end hk;

library ieee;
use ieee.std_logic_1164.all;

entity smfy is
  port (qahm : out std_logic; veect : in integer; eusmcczzs : buffer real);
end smfy;

architecture lj of smfy is
  signal rbrh : time;
  signal ndsjpjaej : real;
  signal mqt : real;
  signal bufbkadp : real;
  signal sb : time;
  signal blo : real;
  signal gvhwld : real;
  signal q : real;
  signal yioqwrn : time;
  signal wivvtfcwce : real;
  signal orhblaixa : real;
begin
  apnqx : entity work.nlyvvmtst
    port map (fcwimfoom => orhblaixa, pswfephe => wivvtfcwce, aejvsywlvi => yioqwrn, cbgeyczsh => q);
  vmpdelip : entity work.nlyvvmtst
    port map (fcwimfoom => gvhwld, pswfephe => blo, aejvsywlvi => sb, cbgeyczsh => bufbkadp);
  fdbjaoosi : entity work.nlyvvmtst
    port map (fcwimfoom => mqt, pswfephe => ndsjpjaej, aejvsywlvi => rbrh, cbgeyczsh => eusmcczzs);
  
  -- Multi-driven assignments
  qahm <= 'X';
  qahm <= 'Z';
end lj;

entity dayvr is
  port (awypvkrcv : linkage integer);
end dayvr;

architecture foakgn of dayvr is
  
begin
  
end foakgn;



-- Seed after: 13425129699637622288,4860866131898729603
