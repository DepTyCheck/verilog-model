-- Seed: 13291958024227185857,8891552411914730853

entity wqoyyecpwp is
  port (kc : inout real; ddeciadj : inout real);
end wqoyyecpwp;

architecture mwjxziw of wqoyyecpwp is
  
begin
  -- Single-driven assignments
  kc <= 2#0_1_1_1_1.1_1_1_1#;
  ddeciadj <= kc;
end mwjxziw;

library ieee;
use ieee.std_logic_1164.all;

entity bo is
  port (sarzds : buffer std_logic);
end bo;

architecture zumhpuoxhl of bo is
  signal fjtqwowcba : real;
  signal bizxjx : real;
  signal tmekybiz : real;
  signal epbj : real;
  signal cbi : real;
  signal a : real;
  signal dh : real;
  signal kuuxo : real;
begin
  us : entity work.wqoyyecpwp
    port map (kc => kuuxo, ddeciadj => dh);
  xhwej : entity work.wqoyyecpwp
    port map (kc => a, ddeciadj => cbi);
  szw : entity work.wqoyyecpwp
    port map (kc => epbj, ddeciadj => tmekybiz);
  jgsgwaxfb : entity work.wqoyyecpwp
    port map (kc => bizxjx, ddeciadj => fjtqwowcba);
  
  -- Multi-driven assignments
  sarzds <= sarzds;
  sarzds <= sarzds;
end zumhpuoxhl;

library ieee;
use ieee.std_logic_1164.all;

entity xekeuox is
  port (hqnopkcmpb : in time; sgd : inout integer; aplr : buffer std_logic_vector(0 to 2); p : buffer time);
end xekeuox;

architecture mlmqfd of xekeuox is
  signal wkv : real;
  signal z : real;
  signal zkamafr : real;
  signal ja : real;
  signal dtyvq : real;
  signal y : real;
  signal hlgacu : real;
  signal cuqg : real;
begin
  e : entity work.wqoyyecpwp
    port map (kc => cuqg, ddeciadj => hlgacu);
  vrkpttz : entity work.wqoyyecpwp
    port map (kc => y, ddeciadj => dtyvq);
  ghj : entity work.wqoyyecpwp
    port map (kc => ja, ddeciadj => zkamafr);
  ygomqmowgm : entity work.wqoyyecpwp
    port map (kc => z, ddeciadj => wkv);
  
  -- Single-driven assignments
  sgd <= 4;
  p <= 2#01110.1# fs;
end mlmqfd;



-- Seed after: 11354579464628370829,8891552411914730853
