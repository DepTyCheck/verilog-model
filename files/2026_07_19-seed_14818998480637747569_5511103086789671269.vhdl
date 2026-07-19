-- Seed: 14818998480637747569,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity fxwmh is
  port (pkpmnqo : in std_logic_vector(3 downto 2); khzz : buffer std_logic_vector(3 to 1); zxxexhna : in integer; wfu : buffer real);
end fxwmh;

architecture afbrtqthkx of fxwmh is
  
begin
  
end afbrtqthkx;

entity jaau is
  port (updkr : out real; mwwd : inout severity_level; fkt : in time);
end jaau;

library ieee;
use ieee.std_logic_1164.all;

architecture jrfbztkj of jaau is
  signal aeiudolebz : real;
  signal kprvrhf : integer;
  signal xlxduyhyau : std_logic_vector(3 downto 2);
  signal ezskgmznr : integer;
  signal ommdmm : real;
  signal razby : std_logic_vector(3 downto 2);
  signal bghjbkvn : real;
  signal sxcwz : integer;
  signal lhrqq : std_logic_vector(3 to 1);
  signal vpm : std_logic_vector(3 downto 2);
begin
  tgecl : entity work.fxwmh
    port map (pkpmnqo => vpm, khzz => lhrqq, zxxexhna => sxcwz, wfu => bghjbkvn);
  kzjxld : entity work.fxwmh
    port map (pkpmnqo => razby, khzz => lhrqq, zxxexhna => sxcwz, wfu => ommdmm);
  llubzpba : entity work.fxwmh
    port map (pkpmnqo => vpm, khzz => lhrqq, zxxexhna => ezskgmznr, wfu => updkr);
  kewmsb : entity work.fxwmh
    port map (pkpmnqo => xlxduyhyau, khzz => lhrqq, zxxexhna => kprvrhf, wfu => aeiudolebz);
  
  -- Single-driven assignments
  mwwd <= mwwd;
  
  -- Multi-driven assignments
  vpm <= ('Z', '1');
  vpm <= vpm;
  vpm <= vpm;
  xlxduyhyau <= "UL";
end jrfbztkj;

entity odchatwo is
  port (aphvj : linkage real; gd : buffer time; lvo : buffer bit; nzy : buffer integer);
end odchatwo;

library ieee;
use ieee.std_logic_1164.all;

architecture rpcl of odchatwo is
  signal lttjkojz : severity_level;
  signal ehlvoieoz : real;
  signal qxcgsh : real;
  signal xkinicg : integer;
  signal qeahibwwyh : std_logic_vector(3 to 1);
  signal rubejvw : real;
  signal xrsifhnyvy : std_logic_vector(3 to 1);
  signal him : real;
  signal puwutghsdi : std_logic_vector(3 to 1);
  signal ap : std_logic_vector(3 downto 2);
begin
  hzr : entity work.fxwmh
    port map (pkpmnqo => ap, khzz => puwutghsdi, zxxexhna => nzy, wfu => him);
  rcdqge : entity work.fxwmh
    port map (pkpmnqo => ap, khzz => xrsifhnyvy, zxxexhna => nzy, wfu => rubejvw);
  qvba : entity work.fxwmh
    port map (pkpmnqo => ap, khzz => qeahibwwyh, zxxexhna => xkinicg, wfu => qxcgsh);
  hoop : entity work.jaau
    port map (updkr => ehlvoieoz, mwwd => lttjkojz, fkt => gd);
  
  -- Multi-driven assignments
  qeahibwwyh <= "";
  puwutghsdi <= (others => '0');
  ap <= ('L', '1');
end rpcl;



-- Seed after: 5182797793922275941,5511103086789671269
