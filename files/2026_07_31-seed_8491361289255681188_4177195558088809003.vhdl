-- Seed: 8491361289255681188,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity ndzkf is
  port (ciwoq : buffer std_logic_vector(4 to 3); gnnhhqafaz : out time_vector(4 to 4));
end ndzkf;

architecture oynridlzt of ndzkf is
  
begin
  -- Single-driven assignments
  gnnhhqafaz <= (others => 2#0# ps);
  
  -- Multi-driven assignments
  ciwoq <= ciwoq;
  ciwoq <= ciwoq;
  ciwoq <= ciwoq;
  ciwoq <= ciwoq;
end oynridlzt;

entity yjpmkmqms is
  port (chewpwaz : inout time; qy : buffer time_vector(2 downto 4));
end yjpmkmqms;

library ieee;
use ieee.std_logic_1164.all;

architecture xgghwq of yjpmkmqms is
  signal ujbsrzeolo : time_vector(4 to 4);
  signal y : time_vector(4 to 4);
  signal fauyw : time_vector(4 to 4);
  signal fkog : std_logic_vector(4 to 3);
begin
  vxcp : entity work.ndzkf
    port map (ciwoq => fkog, gnnhhqafaz => fauyw);
  nyho : entity work.ndzkf
    port map (ciwoq => fkog, gnnhhqafaz => y);
  zkiows : entity work.ndzkf
    port map (ciwoq => fkog, gnnhhqafaz => ujbsrzeolo);
  
  -- Multi-driven assignments
  fkog <= fkog;
  fkog <= (others => '0');
  fkog <= "";
  fkog <= fkog;
end xgghwq;

entity rvab is
  port (yttlzo : linkage time; mqi : linkage integer; tvrhcxo : inout boolean);
end rvab;

library ieee;
use ieee.std_logic_1164.all;

architecture et of rvab is
  signal ggj : time_vector(4 to 4);
  signal zqeffg : std_logic_vector(4 to 3);
  signal zvjezsx : time_vector(4 to 4);
  signal vs : std_logic_vector(4 to 3);
begin
  kkuqmtu : entity work.ndzkf
    port map (ciwoq => vs, gnnhhqafaz => zvjezsx);
  odobbkzm : entity work.ndzkf
    port map (ciwoq => zqeffg, gnnhhqafaz => ggj);
  
  -- Single-driven assignments
  tvrhcxo <= FALSE;
end et;

entity umcol is
  port (eehamnign : inout bit);
end umcol;

library ieee;
use ieee.std_logic_1164.all;

architecture eedj of umcol is
  signal peg : time_vector(4 to 4);
  signal xlgc : boolean;
  signal rrgrjy : integer;
  signal iyvbx : time;
  signal hjvrcif : time_vector(4 to 4);
  signal ahmeiis : std_logic_vector(4 to 3);
  signal m : boolean;
  signal vhr : integer;
  signal ohvdacd : time;
begin
  st : entity work.rvab
    port map (yttlzo => ohvdacd, mqi => vhr, tvrhcxo => m);
  qlpnsipq : entity work.ndzkf
    port map (ciwoq => ahmeiis, gnnhhqafaz => hjvrcif);
  ulqvx : entity work.rvab
    port map (yttlzo => iyvbx, mqi => rrgrjy, tvrhcxo => xlgc);
  acam : entity work.ndzkf
    port map (ciwoq => ahmeiis, gnnhhqafaz => peg);
  
  -- Single-driven assignments
  eehamnign <= eehamnign;
end eedj;



-- Seed after: 4453209658066544903,4177195558088809003
