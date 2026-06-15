-- Seed: 822601636223656979,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity gnqjag is
  port (k : buffer time; abkyju : in std_logic_vector(3 to 3));
end gnqjag;

architecture t of gnqjag is
  
begin
  -- Single-driven assignments
  k <= 2.2_4_2_0 us;
end t;

entity xoubebbvx is
  port (mkbgkwtja : inout boolean_vector(2 downto 2); upfdgc : in time; svyvul : out boolean);
end xoubebbvx;

library ieee;
use ieee.std_logic_1164.all;

architecture owsyqtzpga of xoubebbvx is
  signal wspjjbl : time;
  signal injqjktx : std_logic_vector(3 to 3);
  signal lb : time;
begin
  ywpp : entity work.gnqjag
    port map (k => lb, abkyju => injqjktx);
  oipcrsqn : entity work.gnqjag
    port map (k => wspjjbl, abkyju => injqjktx);
  
  -- Multi-driven assignments
  injqjktx <= (others => 'H');
  injqjktx <= (others => '0');
  injqjktx <= (others => '1');
end owsyqtzpga;

library ieee;
use ieee.std_logic_1164.all;

entity xgmos is
  port (xxqyzi : out boolean; tymynhrxzn : linkage std_logic);
end xgmos;

architecture zquifotlu of xgmos is
  
begin
  -- Single-driven assignments
  xxqyzi <= FALSE;
end zquifotlu;

library ieee;
use ieee.std_logic_1164.all;

entity hfu is
  port (beliwn : buffer real; sbbzbv : in integer; fxrwk : in std_logic_vector(0 downto 1));
end hfu;

library ieee;
use ieee.std_logic_1164.all;

architecture v of hfu is
  signal lhcfgmds : time;
  signal aolsri : boolean;
  signal umrcakzoab : time;
  signal bnzwxd : boolean_vector(2 downto 2);
  signal odtd : std_logic_vector(3 to 3);
  signal evuucftc : time;
begin
  qzuo : entity work.gnqjag
    port map (k => evuucftc, abkyju => odtd);
  rozys : entity work.xoubebbvx
    port map (mkbgkwtja => bnzwxd, upfdgc => umrcakzoab, svyvul => aolsri);
  jwanfxet : entity work.gnqjag
    port map (k => lhcfgmds, abkyju => odtd);
  
  -- Single-driven assignments
  umrcakzoab <= 8#4# ns;
  
  -- Multi-driven assignments
  odtd <= (others => 'U');
  odtd <= "H";
  odtd <= "1";
end v;



-- Seed after: 12236596964354060662,1834764876137802293
