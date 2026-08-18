-- Seed: 17665099048788980254,5983430343285687595

entity okdkfrak is
  port (ohnyn : in boolean_vector(1 to 3); ptz : buffer integer);
end okdkfrak;

architecture pahoym of okdkfrak is
  
begin
  -- Single-driven assignments
  ptz <= 16#D4#;
end pahoym;

library ieee;
use ieee.std_logic_1164.all;

entity nbpxpprqm is
  port (mvzvuwjod : out std_logic_vector(1 to 0); ugayek : out boolean; eoqibqsrl : buffer real);
end nbpxpprqm;

architecture d of nbpxpprqm is
  signal ehphuahkdk : integer;
  signal fgiak : boolean_vector(1 to 3);
begin
  rtmdderfr : entity work.okdkfrak
    port map (ohnyn => fgiak, ptz => ehphuahkdk);
  
  -- Single-driven assignments
  ugayek <= ugayek;
  eoqibqsrl <= eoqibqsrl;
  fgiak <= (FALSE, FALSE, FALSE);
  
  -- Multi-driven assignments
  mvzvuwjod <= "";
end d;

entity zrmuzidhvn is
  port (ma : out time_vector(4 to 3));
end zrmuzidhvn;

library ieee;
use ieee.std_logic_1164.all;

architecture kf of zrmuzidhvn is
  signal huzxiez : real;
  signal xucjbrcxby : boolean;
  signal iodisbaa : integer;
  signal siorn : boolean_vector(1 to 3);
  signal dkblpmz : real;
  signal ztwvslzewq : boolean;
  signal kbeolumony : std_logic_vector(1 to 0);
  signal oepysugrw : integer;
  signal jrhodelp : boolean_vector(1 to 3);
begin
  shzri : entity work.okdkfrak
    port map (ohnyn => jrhodelp, ptz => oepysugrw);
  usca : entity work.nbpxpprqm
    port map (mvzvuwjod => kbeolumony, ugayek => ztwvslzewq, eoqibqsrl => dkblpmz);
  bakhshv : entity work.okdkfrak
    port map (ohnyn => siorn, ptz => iodisbaa);
  woanvllx : entity work.nbpxpprqm
    port map (mvzvuwjod => kbeolumony, ugayek => xucjbrcxby, eoqibqsrl => huzxiez);
  
  -- Single-driven assignments
  ma <= (others => 0 ns);
  
  -- Multi-driven assignments
  kbeolumony <= kbeolumony;
end kf;

entity ucmeais is
  port (jowubbejye : out severity_level; uuwl : linkage integer);
end ucmeais;

architecture akchdlndpr of ucmeais is
  
begin
  -- Single-driven assignments
  jowubbejye <= jowubbejye;
end akchdlndpr;



-- Seed after: 7135898478800469994,5983430343285687595
