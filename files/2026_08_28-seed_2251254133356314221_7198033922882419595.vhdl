-- Seed: 2251254133356314221,7198033922882419595

entity olkxgmv is
  port (jttulkqvb : buffer time; hsc : buffer bit_vector(0 to 3));
end olkxgmv;

architecture vwop of olkxgmv is
  
begin
  -- Single-driven assignments
  jttulkqvb <= jttulkqvb;
  hsc <= ('1', '0', '0', '1');
end vwop;

entity apuurgo is
  port (hy : buffer integer; omghg : inout time; eicaeynj : out time_vector(0 to 2));
end apuurgo;

architecture yqqkhmub of apuurgo is
  
begin
  -- Single-driven assignments
  eicaeynj <= eicaeynj;
  hy <= hy;
end yqqkhmub;

library ieee;
use ieee.std_logic_1164.all;

entity bxwxrffiw is
  port (wjcghjugs : out std_logic_vector(1 downto 3));
end bxwxrffiw;

architecture kjvisddsa of bxwxrffiw is
  signal rwmoezkyf : bit_vector(0 to 3);
  signal hkdbvfpvol : time;
  signal xhndosuwgi : bit_vector(0 to 3);
  signal koeqiva : time;
  signal xld : time_vector(0 to 2);
  signal qvzywbtpw : time;
  signal bllb : integer;
  signal oinl : bit_vector(0 to 3);
  signal cdisf : time;
begin
  ijzmmlvovg : entity work.olkxgmv
    port map (jttulkqvb => cdisf, hsc => oinl);
  zn : entity work.apuurgo
    port map (hy => bllb, omghg => qvzywbtpw, eicaeynj => xld);
  ghjw : entity work.olkxgmv
    port map (jttulkqvb => koeqiva, hsc => xhndosuwgi);
  zeau : entity work.olkxgmv
    port map (jttulkqvb => hkdbvfpvol, hsc => rwmoezkyf);
  
  -- Multi-driven assignments
  wjcghjugs <= (others => '0');
  wjcghjugs <= wjcghjugs;
  wjcghjugs <= wjcghjugs;
  wjcghjugs <= wjcghjugs;
end kjvisddsa;



-- Seed after: 6779173557994017953,7198033922882419595
