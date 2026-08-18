-- Seed: 16781633898189010908,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity plif is
  port (uxryg : inout std_logic_vector(2 downto 0); yzplztlhr : linkage bit; hdwj : out time);
end plif;

architecture u of plif is
  
begin
  -- Single-driven assignments
  hdwj <= hdwj;
  
  -- Multi-driven assignments
  uxryg <= uxryg;
end u;

entity exj is
  port (ucupa : linkage boolean_vector(0 downto 3); otrpy : linkage time);
end exj;

library ieee;
use ieee.std_logic_1164.all;

architecture zhwuiuo of exj is
  signal xgq : time;
  signal squfkepo : bit;
  signal rygepbfvo : std_logic_vector(2 downto 0);
begin
  l : entity work.plif
    port map (uxryg => rygepbfvo, yzplztlhr => squfkepo, hdwj => xgq);
end zhwuiuo;

entity ncggpyi is
  port (fcz : inout boolean);
end ncggpyi;

architecture v of ncggpyi is
  signal ldbig : time;
  signal rdrjcwpfs : boolean_vector(0 downto 3);
begin
  ddtxnbenpn : entity work.exj
    port map (ucupa => rdrjcwpfs, otrpy => ldbig);
  
  -- Single-driven assignments
  fcz <= TRUE;
end v;

entity fayuka is
  port (y : linkage integer);
end fayuka;

library ieee;
use ieee.std_logic_1164.all;

architecture nmq of fayuka is
  signal wqwc : time;
  signal lpkhw : bit;
  signal nsze : std_logic_vector(2 downto 0);
begin
  pnlwtyc : entity work.plif
    port map (uxryg => nsze, yzplztlhr => lpkhw, hdwj => wqwc);
  
  -- Multi-driven assignments
  nsze <= "WL0";
  nsze <= nsze;
  nsze <= "W0H";
end nmq;



-- Seed after: 1189068661036081834,5983430343285687595
