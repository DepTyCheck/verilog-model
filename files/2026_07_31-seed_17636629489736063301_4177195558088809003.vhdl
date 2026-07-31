-- Seed: 17636629489736063301,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity sqewx is
  port (wlzoatfsj : inout std_logic; uibyqb : linkage std_logic_vector(3 downto 1); poyetfwaz : linkage real);
end sqewx;

architecture vfguz of sqewx is
  
begin
  
end vfguz;

entity qsbiirjc is
  port (jhkv : in time);
end qsbiirjc;

library ieee;
use ieee.std_logic_1164.all;

architecture yw of qsbiirjc is
  signal wcp : real;
  signal vddj : std_logic_vector(3 downto 1);
  signal ezvzpwrkq : real;
  signal qqkp : real;
  signal vba : std_logic_vector(3 downto 1);
  signal dgnpihy : real;
  signal hx : std_logic_vector(3 downto 1);
  signal hv : std_logic;
begin
  s : entity work.sqewx
    port map (wlzoatfsj => hv, uibyqb => hx, poyetfwaz => dgnpihy);
  gbasbulur : entity work.sqewx
    port map (wlzoatfsj => hv, uibyqb => vba, poyetfwaz => qqkp);
  arcrxo : entity work.sqewx
    port map (wlzoatfsj => hv, uibyqb => hx, poyetfwaz => ezvzpwrkq);
  gdmvjsqh : entity work.sqewx
    port map (wlzoatfsj => hv, uibyqb => vddj, poyetfwaz => wcp);
  
  -- Multi-driven assignments
  hv <= hv;
  vba <= "Z-0";
end yw;

library ieee;
use ieee.std_logic_1164.all;

entity yqb is
  port (gmixoqke : linkage character; sgzxer : linkage integer; fvsszp : out std_logic_vector(1 to 4); lzdxrdehdx : buffer severity_level);
end yqb;

library ieee;
use ieee.std_logic_1164.all;

architecture v of yqb is
  signal zzrr : real;
  signal nenqcyllr : std_logic;
  signal qeqn : real;
  signal erscia : std_logic_vector(3 downto 1);
  signal b : std_logic;
begin
  qtz : entity work.sqewx
    port map (wlzoatfsj => b, uibyqb => erscia, poyetfwaz => qeqn);
  eqeajmqtbw : entity work.sqewx
    port map (wlzoatfsj => nenqcyllr, uibyqb => erscia, poyetfwaz => zzrr);
  
  -- Single-driven assignments
  lzdxrdehdx <= lzdxrdehdx;
  
  -- Multi-driven assignments
  nenqcyllr <= 'L';
  fvsszp <= ('H', '0', 'X', 'L');
  fvsszp <= "0Z1W";
end v;



-- Seed after: 12171752958385559429,4177195558088809003
