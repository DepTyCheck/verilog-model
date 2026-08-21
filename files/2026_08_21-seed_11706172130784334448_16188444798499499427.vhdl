-- Seed: 11706172130784334448,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port (ehrjztmf : out std_logic; kbugh : buffer std_logic);
end q;

architecture mrq of q is
  
begin
  -- Multi-driven assignments
  kbugh <= 'U';
  ehrjztmf <= kbugh;
  kbugh <= kbugh;
  kbugh <= kbugh;
end mrq;

entity l is
  port (dqjxcgu : out real; bvurqpk : inout bit; twe : buffer time);
end l;

library ieee;
use ieee.std_logic_1164.all;

architecture qvh of l is
  signal abasavrlcf : std_logic;
  signal ytpsseluzi : std_logic;
  signal nslk : std_logic;
  signal mirxctr : std_logic;
  signal sb : std_logic;
begin
  pnyhfua : entity work.q
    port map (ehrjztmf => sb, kbugh => mirxctr);
  iuqzekdbh : entity work.q
    port map (ehrjztmf => nslk, kbugh => sb);
  chl : entity work.q
    port map (ehrjztmf => mirxctr, kbugh => ytpsseluzi);
  yjj : entity work.q
    port map (ehrjztmf => abasavrlcf, kbugh => nslk);
  
  -- Single-driven assignments
  dqjxcgu <= 13131.11;
  bvurqpk <= bvurqpk;
  twe <= 2#11100# ns;
  
  -- Multi-driven assignments
  abasavrlcf <= 'W';
  sb <= sb;
  sb <= 'L';
  mirxctr <= mirxctr;
end qvh;



-- Seed after: 15963667791033591177,16188444798499499427
