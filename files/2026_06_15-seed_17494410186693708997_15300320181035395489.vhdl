-- Seed: 17494410186693708997,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity spugfyani is
  port (cnno : buffer integer; rkxsptmho : buffer boolean_vector(0 downto 0); bmyqa : inout integer; jeyqi : in std_logic_vector(3 downto 0));
end spugfyani;

architecture vzndln of spugfyani is
  
begin
  -- Single-driven assignments
  cnno <= 16#3E4#;
  rkxsptmho <= (others => FALSE);
end vzndln;

entity kss is
  port (tnup : buffer integer; xo : in time; zvoupqgpjw : out bit_vector(2 to 4));
end kss;

library ieee;
use ieee.std_logic_1164.all;

architecture gmpeo of kss is
  signal ewceaqw : std_logic_vector(3 downto 0);
  signal i : integer;
  signal sqt : boolean_vector(0 downto 0);
  signal e : integer;
  signal sadyzblgof : std_logic_vector(3 downto 0);
  signal h : boolean_vector(0 downto 0);
  signal azf : integer;
  signal quta : std_logic_vector(3 downto 0);
  signal paujo : integer;
  signal bzvlmvvgow : boolean_vector(0 downto 0);
  signal gzz : integer;
  signal kpv : std_logic_vector(3 downto 0);
  signal ea : integer;
  signal dybeh : boolean_vector(0 downto 0);
  signal ljeegjzkr : integer;
begin
  urh : entity work.spugfyani
    port map (cnno => ljeegjzkr, rkxsptmho => dybeh, bmyqa => ea, jeyqi => kpv);
  k : entity work.spugfyani
    port map (cnno => gzz, rkxsptmho => bzvlmvvgow, bmyqa => paujo, jeyqi => quta);
  yy : entity work.spugfyani
    port map (cnno => azf, rkxsptmho => h, bmyqa => tnup, jeyqi => sadyzblgof);
  mwnxnukkqw : entity work.spugfyani
    port map (cnno => e, rkxsptmho => sqt, bmyqa => i, jeyqi => ewceaqw);
end gmpeo;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (u : in boolean_vector(2 downto 3); usmot : linkage std_logic_vector(3 downto 0));
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture cbapwkiroa of o is
  signal sakxgs : std_logic_vector(3 downto 0);
  signal drcvbkobhm : integer;
  signal ul : boolean_vector(0 downto 0);
  signal yc : integer;
  signal oemnoils : bit_vector(2 to 4);
  signal wqslc : time;
  signal ahszlzj : integer;
  signal siopnaqgtg : integer;
  signal jk : boolean_vector(0 downto 0);
  signal dtni : integer;
  signal qhdkpqex : std_logic_vector(3 downto 0);
  signal qacwgastjw : integer;
  signal hab : boolean_vector(0 downto 0);
  signal bshcuiti : integer;
begin
  othevpttz : entity work.spugfyani
    port map (cnno => bshcuiti, rkxsptmho => hab, bmyqa => qacwgastjw, jeyqi => qhdkpqex);
  rdjxtkdsv : entity work.spugfyani
    port map (cnno => dtni, rkxsptmho => jk, bmyqa => siopnaqgtg, jeyqi => qhdkpqex);
  fvoc : entity work.kss
    port map (tnup => ahszlzj, xo => wqslc, zvoupqgpjw => oemnoils);
  jnwrd : entity work.spugfyani
    port map (cnno => yc, rkxsptmho => ul, bmyqa => drcvbkobhm, jeyqi => sakxgs);
  
  -- Single-driven assignments
  wqslc <= 16#A37F4# ns;
  
  -- Multi-driven assignments
  qhdkpqex <= "UZX-";
  sakxgs <= ('0', 'Z', '1', '-');
  qhdkpqex <= ('Z', '0', 'W', 'W');
end cbapwkiroa;

entity pompxnv is
  port (rv : out integer; fcmfams : out character; vfcstgzq : linkage real);
end pompxnv;

library ieee;
use ieee.std_logic_1164.all;

architecture gbvowsw of pompxnv is
  signal kwlydyjwte : std_logic_vector(3 downto 0);
  signal wroutrfod : boolean_vector(0 downto 0);
  signal eksfifz : integer;
  signal ctql : bit_vector(2 to 4);
  signal xbwanolz : time;
  signal fpslvpiw : integer;
  signal upv : std_logic_vector(3 downto 0);
  signal brpzdykn : integer;
  signal macad : boolean_vector(0 downto 0);
  signal ljanegdn : integer;
  signal rrnpx : bit_vector(2 to 4);
  signal e : time;
  signal ctyv : integer;
begin
  ksnawrbfr : entity work.kss
    port map (tnup => ctyv, xo => e, zvoupqgpjw => rrnpx);
  uksiscrbh : entity work.spugfyani
    port map (cnno => ljanegdn, rkxsptmho => macad, bmyqa => brpzdykn, jeyqi => upv);
  ddkhxzyxlx : entity work.kss
    port map (tnup => fpslvpiw, xo => xbwanolz, zvoupqgpjw => ctql);
  aeqyhxhdh : entity work.spugfyani
    port map (cnno => eksfifz, rkxsptmho => wroutrfod, bmyqa => rv, jeyqi => kwlydyjwte);
  
  -- Multi-driven assignments
  upv <= ('Z', 'X', '0', 'H');
  kwlydyjwte <= "-HXH";
end gbvowsw;



-- Seed after: 18283358797891883001,15300320181035395489
