-- Seed: 15737487875453975390,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity tank is
  port (inq : linkage time; cobrmu : inout real; utlen : in std_logic);
end tank;

architecture u of tank is
  
begin
  -- Single-driven assignments
  cobrmu <= cobrmu;
end u;

entity bbjzgr is
  port (mtdzdoosr : inout integer);
end bbjzgr;

library ieee;
use ieee.std_logic_1164.all;

architecture kxmcobmhx of bbjzgr is
  signal jeeewvtsi : real;
  signal rytftxx : time;
  signal cqm : real;
  signal mt : time;
  signal s : std_logic;
  signal jkwao : real;
  signal qqf : time;
begin
  ybfum : entity work.tank
    port map (inq => qqf, cobrmu => jkwao, utlen => s);
  nbbj : entity work.tank
    port map (inq => mt, cobrmu => cqm, utlen => s);
  vrfum : entity work.tank
    port map (inq => rytftxx, cobrmu => jeeewvtsi, utlen => s);
  
  -- Single-driven assignments
  mtdzdoosr <= 0;
  
  -- Multi-driven assignments
  s <= 'U';
  s <= 'H';
  s <= s;
  s <= 'L';
end kxmcobmhx;



-- Seed after: 16863331140134103880,2230106469645304029
