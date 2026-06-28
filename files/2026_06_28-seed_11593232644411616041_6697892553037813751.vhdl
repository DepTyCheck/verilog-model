-- Seed: 11593232644411616041,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity rdaxs is
  port (rerimoa : buffer bit; exwqcyxn : linkage std_logic; ivits : buffer bit_vector(0 downto 2); yvxhrymgrg : buffer time);
end rdaxs;

architecture jdwmjnt of rdaxs is
  
begin
  
end jdwmjnt;

library ieee;
use ieee.std_logic_1164.all;

entity qn is
  port (dmxgpyji : buffer integer; ip : out std_logic; oie : buffer std_logic);
end qn;

library ieee;
use ieee.std_logic_1164.all;

architecture pedgyajuu of qn is
  signal fxm : time;
  signal o : bit_vector(0 downto 2);
  signal eyoadljg : bit;
  signal hvkwa : time;
  signal qborobal : bit_vector(0 downto 2);
  signal jqis : std_logic;
  signal kddjkiqkoy : bit;
  signal u : time;
  signal nxklsvogqv : bit_vector(0 downto 2);
  signal sij : std_logic;
  signal dpbnyurdj : bit;
  signal tajcrwsa : time;
  signal uynn : bit_vector(0 downto 2);
  signal bpsvdj : bit;
begin
  tmqaqbmpi : entity work.rdaxs
    port map (rerimoa => bpsvdj, exwqcyxn => ip, ivits => uynn, yvxhrymgrg => tajcrwsa);
  mfegcnnkh : entity work.rdaxs
    port map (rerimoa => dpbnyurdj, exwqcyxn => sij, ivits => nxklsvogqv, yvxhrymgrg => u);
  jwv : entity work.rdaxs
    port map (rerimoa => kddjkiqkoy, exwqcyxn => jqis, ivits => qborobal, yvxhrymgrg => hvkwa);
  zydpwziui : entity work.rdaxs
    port map (rerimoa => eyoadljg, exwqcyxn => jqis, ivits => o, yvxhrymgrg => fxm);
  
  -- Single-driven assignments
  dmxgpyji <= 04;
  
  -- Multi-driven assignments
  oie <= 'W';
  ip <= '-';
  jqis <= 'L';
end pedgyajuu;

entity qhx is
  port (myiofemb : in bit_vector(2 downto 3); bzawh : out severity_level; iwymlt : linkage integer; te : buffer real);
end qhx;

library ieee;
use ieee.std_logic_1164.all;

architecture qynohjs of qhx is
  signal qrhnwpz : std_logic;
  signal cdyvzkmuqt : integer;
  signal vivo : time;
  signal qvctxeesy : bit_vector(0 downto 2);
  signal clijmf : std_logic;
  signal yxndpqgra : bit;
  signal qgaoz : time;
  signal rzwbayu : bit_vector(0 downto 2);
  signal z : std_logic;
  signal npuzj : bit;
  signal cphmeqde : time;
  signal rnhz : bit_vector(0 downto 2);
  signal dwlalbigik : std_logic;
  signal qbh : bit;
begin
  jvgbxtejs : entity work.rdaxs
    port map (rerimoa => qbh, exwqcyxn => dwlalbigik, ivits => rnhz, yvxhrymgrg => cphmeqde);
  jtn : entity work.rdaxs
    port map (rerimoa => npuzj, exwqcyxn => z, ivits => rzwbayu, yvxhrymgrg => qgaoz);
  anuv : entity work.rdaxs
    port map (rerimoa => yxndpqgra, exwqcyxn => clijmf, ivits => qvctxeesy, yvxhrymgrg => vivo);
  i : entity work.qn
    port map (dmxgpyji => cdyvzkmuqt, ip => qrhnwpz, oie => qrhnwpz);
  
  -- Single-driven assignments
  bzawh <= ERROR;
  te <= 8#0.5_6#;
  
  -- Multi-driven assignments
  clijmf <= '1';
  clijmf <= 'X';
  dwlalbigik <= 'U';
  z <= 'Z';
end qynohjs;

library ieee;
use ieee.std_logic_1164.all;

entity fnojboq is
  port (wgstlatvf : linkage real; hkpzafqrd : buffer std_logic; crlhjyrdyx : out std_logic_vector(0 to 0));
end fnojboq;

library ieee;
use ieee.std_logic_1164.all;

architecture znqatl of fnojboq is
  signal iyceon : real;
  signal vhop : integer;
  signal wmxnichdt : severity_level;
  signal ykjeo : time;
  signal dlefu : bit_vector(2 downto 3);
  signal pucnvgrcpv : std_logic;
  signal kytgpdzwwl : bit;
begin
  tazumbrnp : entity work.rdaxs
    port map (rerimoa => kytgpdzwwl, exwqcyxn => pucnvgrcpv, ivits => dlefu, yvxhrymgrg => ykjeo);
  f : entity work.qhx
    port map (myiofemb => dlefu, bzawh => wmxnichdt, iwymlt => vhop, te => iyceon);
  
  -- Multi-driven assignments
  crlhjyrdyx <= (others => 'Z');
  pucnvgrcpv <= 'Z';
  crlhjyrdyx <= "1";
  hkpzafqrd <= 'X';
end znqatl;



-- Seed after: 6851016352688713668,6697892553037813751
