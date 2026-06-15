-- Seed: 24101878676112802,1834764876137802293

entity hbvba is
  port (ofngp : buffer real; vfinc : linkage time);
end hbvba;

architecture ruloasp of hbvba is
  
begin
  -- Single-driven assignments
  ofngp <= 16#8.C_D_B_7#;
end ruloasp;

library ieee;
use ieee.std_logic_1164.all;

entity jgn is
  port (rlqityzfp : in severity_level; yjksfwob : out std_logic);
end jgn;

architecture p of jgn is
  signal qpjikur : time;
  signal lzwoa : real;
  signal zhw : time;
  signal hqsppypttk : real;
  signal zjk : time;
  signal qabrhp : real;
  signal fllsbkp : time;
  signal ljckrwow : real;
begin
  inorqunuma : entity work.hbvba
    port map (ofngp => ljckrwow, vfinc => fllsbkp);
  qzx : entity work.hbvba
    port map (ofngp => qabrhp, vfinc => zjk);
  w : entity work.hbvba
    port map (ofngp => hqsppypttk, vfinc => zhw);
  krsqat : entity work.hbvba
    port map (ofngp => lzwoa, vfinc => qpjikur);
  
  -- Multi-driven assignments
  yjksfwob <= 'L';
  yjksfwob <= 'X';
  yjksfwob <= 'Z';
end p;



-- Seed after: 6357197357850267617,1834764876137802293
