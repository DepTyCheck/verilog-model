-- Seed: 5192264552664009220,14641901754878719179

entity be is
  port (ijwg : linkage real; g : inout severity_level; bztwcxch : in real);
end be;

architecture hlntnrk of be is
  
begin
  -- Single-driven assignments
  g <= g;
end hlntnrk;

entity bcbpisfcw is
  port (fjuhsefwyv : in real; rie : in time_vector(4 downto 3); hnhnqo : inout bit; sozciqxpb : in integer_vector(1 downto 0));
end bcbpisfcw;

architecture uc of bcbpisfcw is
  signal ncmjdp : severity_level;
  signal cxdbrkrcd : real;
  signal dbxpx : severity_level;
  signal suitstpnm : real;
begin
  wf : entity work.be
    port map (ijwg => suitstpnm, g => dbxpx, bztwcxch => cxdbrkrcd);
  lypfcm : entity work.be
    port map (ijwg => cxdbrkrcd, g => ncmjdp, bztwcxch => suitstpnm);
  
  -- Single-driven assignments
  hnhnqo <= '0';
end uc;

library ieee;
use ieee.std_logic_1164.all;

entity sh is
  port (zs : linkage integer; rsrmyai : in std_logic_vector(2 downto 4); urqlvhdnf : inout real);
end sh;

architecture fw of sh is
  signal qxeqokzy : severity_level;
  signal ofpyot : integer_vector(1 downto 0);
  signal bnmulkeg : bit;
  signal ruhvex : time_vector(4 downto 3);
begin
  fwp : entity work.bcbpisfcw
    port map (fjuhsefwyv => urqlvhdnf, rie => ruhvex, hnhnqo => bnmulkeg, sozciqxpb => ofpyot);
  pll : entity work.be
    port map (ijwg => urqlvhdnf, g => qxeqokzy, bztwcxch => urqlvhdnf);
  
  -- Single-driven assignments
  ofpyot <= (8#2_5_0_1_4#, 14201);
  ruhvex <= (1 hr, 0 hr);
end fw;

entity fsxnmcaa is
  port (gnxm : linkage real; qw : out boolean);
end fsxnmcaa;

library ieee;
use ieee.std_logic_1164.all;

architecture cz of fsxnmcaa is
  signal vnkzhu : std_logic_vector(2 downto 4);
  signal ncytgl : integer;
  signal hrnn : real;
  signal sxvzrxrl : std_logic_vector(2 downto 4);
  signal wglaqghz : integer;
  signal gmxfyhjlkn : severity_level;
  signal l : real;
  signal gpqtcxala : real;
  signal bwtnarp : severity_level;
begin
  sfv : entity work.be
    port map (ijwg => gnxm, g => bwtnarp, bztwcxch => gpqtcxala);
  rojmxazl : entity work.be
    port map (ijwg => l, g => gmxfyhjlkn, bztwcxch => gpqtcxala);
  bczxwoz : entity work.sh
    port map (zs => wglaqghz, rsrmyai => sxvzrxrl, urqlvhdnf => hrnn);
  ojm : entity work.sh
    port map (zs => ncytgl, rsrmyai => vnkzhu, urqlvhdnf => gpqtcxala);
  
  -- Single-driven assignments
  qw <= TRUE;
  
  -- Multi-driven assignments
  sxvzrxrl <= "";
  sxvzrxrl <= "";
end cz;



-- Seed after: 8116440209423139798,14641901754878719179
