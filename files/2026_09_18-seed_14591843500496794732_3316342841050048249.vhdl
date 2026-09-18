-- Seed: 14591843500496794732,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity uhnfypbzo is
  port (jxh : in std_logic; kvephwh : in std_logic_vector(2 downto 0); tpbsxlvz : in time);
end uhnfypbzo;

architecture sarqry of uhnfypbzo is
  
begin
  
end sarqry;

entity hjks is
  port (igxn : inout time; fivk : inout character);
end hjks;

library ieee;
use ieee.std_logic_1164.all;

architecture f of hjks is
  signal cv : time;
  signal rsjb : std_logic_vector(2 downto 0);
  signal mwsknezxq : std_logic;
  signal gnagwqyk : time;
  signal vdjr : std_logic_vector(2 downto 0);
  signal abxtzn : std_logic;
begin
  ikpgz : entity work.uhnfypbzo
    port map (jxh => abxtzn, kvephwh => vdjr, tpbsxlvz => gnagwqyk);
  imyktc : entity work.uhnfypbzo
    port map (jxh => mwsknezxq, kvephwh => rsjb, tpbsxlvz => cv);
  
  -- Single-driven assignments
  gnagwqyk <= igxn;
  fivk <= 't';
end f;

library ieee;
use ieee.std_logic_1164.all;

entity iofmsc is
  port (caddqqvdlx : linkage std_logic_vector(2 to 0); hela : in time; keccum : linkage integer; cxpnibpz : buffer std_logic_vector(1 downto 0));
end iofmsc;

library ieee;
use ieee.std_logic_1164.all;

architecture mnhsnd of iofmsc is
  signal ajgclbvxtc : std_logic_vector(2 downto 0);
  signal xmhyfrayzu : std_logic;
begin
  s : entity work.uhnfypbzo
    port map (jxh => xmhyfrayzu, kvephwh => ajgclbvxtc, tpbsxlvz => hela);
  qxivsfmhit : entity work.uhnfypbzo
    port map (jxh => xmhyfrayzu, kvephwh => ajgclbvxtc, tpbsxlvz => hela);
  
  -- Multi-driven assignments
  cxpnibpz <= cxpnibpz;
  cxpnibpz <= "H0";
  cxpnibpz <= cxpnibpz;
  cxpnibpz <= cxpnibpz;
end mnhsnd;



-- Seed after: 14511076882225568211,3316342841050048249
