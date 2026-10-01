-- Seed: 9314433545907507192,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity yngyh is
  port (hpcep : buffer boolean_vector(1 to 3); smg : out character; ktwfuujlo : out std_logic_vector(3 to 4));
end yngyh;

architecture wnhtd of yngyh is
  
begin
  -- Single-driven assignments
  smg <= smg;
  hpcep <= hpcep;
  
  -- Multi-driven assignments
  ktwfuujlo <= ktwfuujlo;
end wnhtd;

entity gi is
  port (lqddwt : inout bit);
end gi;

library ieee;
use ieee.std_logic_1164.all;

architecture hjdb of gi is
  signal tli : character;
  signal isqedm : boolean_vector(1 to 3);
  signal emdktdjkt : std_logic_vector(3 to 4);
  signal qiupvtdo : character;
  signal bw : boolean_vector(1 to 3);
begin
  bnz : entity work.yngyh
    port map (hpcep => bw, smg => qiupvtdo, ktwfuujlo => emdktdjkt);
  svsdo : entity work.yngyh
    port map (hpcep => isqedm, smg => tli, ktwfuujlo => emdktdjkt);
  
  -- Single-driven assignments
  lqddwt <= '1';
end hjdb;



-- Seed after: 7479613791005681461,15025465285671019065
