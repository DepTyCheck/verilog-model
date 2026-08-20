-- Seed: 13483901402790004645,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity oprb is
  port (oybpuqpa : out std_logic_vector(4 downto 2); ak : out time);
end oprb;

architecture mz of oprb is
  
begin
  -- Single-driven assignments
  ak <= 8#200.2174# ns;
end mz;

library ieee;
use ieee.std_logic_1164.all;

entity lszqpsdkg is
  port (dvrxpivkp : out std_logic_vector(4 downto 0); arwxw : inout boolean; ps : in std_logic);
end lszqpsdkg;

library ieee;
use ieee.std_logic_1164.all;

architecture virj of lszqpsdkg is
  signal dsnncp : time;
  signal dtmm : time;
  signal bp : std_logic_vector(4 downto 2);
  signal nq : time;
  signal iwekcsh : time;
  signal jgpfdnmzhn : std_logic_vector(4 downto 2);
begin
  eklh : entity work.oprb
    port map (oybpuqpa => jgpfdnmzhn, ak => iwekcsh);
  bsvdei : entity work.oprb
    port map (oybpuqpa => jgpfdnmzhn, ak => nq);
  sopgr : entity work.oprb
    port map (oybpuqpa => bp, ak => dtmm);
  flndaqhskt : entity work.oprb
    port map (oybpuqpa => jgpfdnmzhn, ak => dsnncp);
  
  -- Single-driven assignments
  arwxw <= arwxw;
  
  -- Multi-driven assignments
  bp <= "H00";
  dvrxpivkp <= "HXLLH";
end virj;

library ieee;
use ieee.std_logic_1164.all;

entity yamk is
  port (efa : buffer std_logic_vector(4 to 3); lngzijhf : in bit);
end yamk;

library ieee;
use ieee.std_logic_1164.all;

architecture bhsuhdmi of yamk is
  signal k : std_logic;
  signal okbheisocw : boolean;
  signal zhm : std_logic_vector(4 downto 0);
begin
  idxash : entity work.lszqpsdkg
    port map (dvrxpivkp => zhm, arwxw => okbheisocw, ps => k);
end bhsuhdmi;

entity sjmmxugvq is
  port (e : inout integer);
end sjmmxugvq;

library ieee;
use ieee.std_logic_1164.all;

architecture ooqr of sjmmxugvq is
  signal trsnhimhyl : time;
  signal qpmf : std_logic_vector(4 downto 2);
  signal m : bit;
  signal bsjgf : std_logic_vector(4 to 3);
begin
  fwjoyauved : entity work.yamk
    port map (efa => bsjgf, lngzijhf => m);
  twnqt : entity work.oprb
    port map (oybpuqpa => qpmf, ak => trsnhimhyl);
  
  -- Single-driven assignments
  m <= m;
  e <= e;
  
  -- Multi-driven assignments
  bsjgf <= bsjgf;
  bsjgf <= (others => '0');
  bsjgf <= "";
end ooqr;



-- Seed after: 9311234275024857090,499459191852795575
