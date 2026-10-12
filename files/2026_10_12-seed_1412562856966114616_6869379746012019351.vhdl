-- Seed: 1412562856966114616,6869379746012019351

entity sawhb is
  port (ebalrcf : out boolean);
end sawhb;

architecture idw of sawhb is
  
begin
  -- Single-driven assignments
  ebalrcf <= FALSE;
end idw;

entity qfv is
  port (ay : linkage time; bfjowd : in time; gbpouhqjb : linkage time);
end qfv;

architecture nsonzxryb of qfv is
  
begin
  
end nsonzxryb;

library ieee;
use ieee.std_logic_1164.all;

entity injxmxn is
  port (wyhrbkq : out std_logic; eytnzzdwjl : in time; gurabrs : linkage std_logic_vector(3 to 2));
end injxmxn;

architecture urqaf of injxmxn is
  signal lnw : boolean;
  signal dstt : boolean;
  signal wskcgsag : boolean;
  signal cson : time;
  signal yrchvekcay : time;
  signal ztxyebk : time;
begin
  t : entity work.qfv
    port map (ay => ztxyebk, bfjowd => yrchvekcay, gbpouhqjb => cson);
  m : entity work.sawhb
    port map (ebalrcf => wskcgsag);
  oayzwos : entity work.sawhb
    port map (ebalrcf => dstt);
  tczmrjmpfd : entity work.sawhb
    port map (ebalrcf => lnw);
  
  -- Single-driven assignments
  yrchvekcay <= 4 ms;
  
  -- Multi-driven assignments
  wyhrbkq <= wyhrbkq;
  wyhrbkq <= wyhrbkq;
end urqaf;



-- Seed after: 2143825102342947174,6869379746012019351
