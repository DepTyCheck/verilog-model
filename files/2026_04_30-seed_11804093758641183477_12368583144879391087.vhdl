-- Seed: 11804093758641183477,12368583144879391087

library ieee;
use ieee.std_logic_1164.all;

entity newiyyeq is
  port (pkiotdopsv : in time; mahqptmz : out real; smxnay : inout std_logic; dkfid : inout time);
end newiyyeq;



architecture mynrc of newiyyeq is
  
begin
  
end mynrc;



entity ghohhl is
  port (hjxaqp : inout real; kfgq : out time);
end ghohhl;

library ieee;
use ieee.std_logic_1164.all;

architecture gop of ghohhl is
  signal zwigueugqv : time;
  signal hnq : std_logic;
  signal cewe : time;
  signal agcp : std_logic;
  signal vqkdtjfk : real;
begin
  ruzxnmrkt : entity work.newiyyeq
    port map (pkiotdopsv => kfgq, mahqptmz => vqkdtjfk, smxnay => agcp, dkfid => cewe);
  rtigbqhgi : entity work.newiyyeq
    port map (pkiotdopsv => kfgq, mahqptmz => hjxaqp, smxnay => hnq, dkfid => zwigueugqv);
end gop;



entity ep is
  port (ygdgwazt : out integer);
end ep;

library ieee;
use ieee.std_logic_1164.all;

architecture ixccnslgn of ep is
  signal rl : real;
  signal eivhyyn : time;
  signal zvmyy : real;
  signal pxgxdlhb : time;
  signal qz : std_logic;
  signal yhlzfmreva : real;
  signal jxcn : time;
begin
  tnf : entity work.newiyyeq
    port map (pkiotdopsv => jxcn, mahqptmz => yhlzfmreva, smxnay => qz, dkfid => pxgxdlhb);
  ohzpz : entity work.ghohhl
    port map (hjxaqp => zvmyy, kfgq => eivhyyn);
  axoaxwtkos : entity work.newiyyeq
    port map (pkiotdopsv => jxcn, mahqptmz => rl, smxnay => qz, dkfid => jxcn);
end ixccnslgn;



-- Seed after: 8268291903342082285,12368583144879391087
