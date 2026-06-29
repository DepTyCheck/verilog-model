-- Seed: 14617185489987115269,17047277710231705797

entity skgvwjgy is
  port (pdrtwoohmv : linkage character; nzrogt : buffer boolean_vector(2 to 4); otko : in character);
end skgvwjgy;

architecture egw of skgvwjgy is
  
begin
  -- Single-driven assignments
  nzrogt <= (TRUE, FALSE, TRUE);
end egw;

library ieee;
use ieee.std_logic_1164.all;

entity tx is
  port (kvkenvt : inout std_logic_vector(2 downto 4); cgyicciuve : inout time; sfqayrw : linkage std_logic_vector(1 to 0));
end tx;

architecture q of tx is
  signal hdv : boolean_vector(2 to 4);
  signal hnwwph : character;
  signal hxonsve : character;
  signal mneku : boolean_vector(2 to 4);
  signal rlqtmyv : character;
  signal mlvjeya : boolean_vector(2 to 4);
  signal gmrmclrms : character;
begin
  xubwqg : entity work.skgvwjgy
    port map (pdrtwoohmv => gmrmclrms, nzrogt => mlvjeya, otko => gmrmclrms);
  dflhvzou : entity work.skgvwjgy
    port map (pdrtwoohmv => rlqtmyv, nzrogt => mneku, otko => hxonsve);
  ugy : entity work.skgvwjgy
    port map (pdrtwoohmv => hnwwph, nzrogt => hdv, otko => hnwwph);
  
  -- Multi-driven assignments
  kvkenvt <= (others => '0');
  kvkenvt <= (others => '0');
end q;

entity jcupq is
  port (dwvdvafga : buffer boolean_vector(3 downto 0));
end jcupq;

library ieee;
use ieee.std_logic_1164.all;

architecture saeqymrnqa of jcupq is
  signal kw : character;
  signal hgugpoup : boolean_vector(2 to 4);
  signal ixuhj : character;
  signal hudwwljk : boolean_vector(2 to 4);
  signal zcpw : std_logic_vector(1 to 0);
  signal zomfbjadzx : time;
  signal sry : std_logic_vector(2 downto 4);
  signal eg : character;
  signal urcwve : boolean_vector(2 to 4);
  signal phna : character;
begin
  ifjqtxhm : entity work.skgvwjgy
    port map (pdrtwoohmv => phna, nzrogt => urcwve, otko => eg);
  qfhpcf : entity work.tx
    port map (kvkenvt => sry, cgyicciuve => zomfbjadzx, sfqayrw => zcpw);
  dd : entity work.skgvwjgy
    port map (pdrtwoohmv => eg, nzrogt => hudwwljk, otko => ixuhj);
  oycvrcubg : entity work.skgvwjgy
    port map (pdrtwoohmv => ixuhj, nzrogt => hgugpoup, otko => kw);
  
  -- Single-driven assignments
  dwvdvafga <= (FALSE, TRUE, FALSE, TRUE);
  kw <= 'a';
  
  -- Multi-driven assignments
  sry <= "";
  sry <= "";
  sry <= (others => '0');
end saeqymrnqa;



-- Seed after: 8064748984504093168,17047277710231705797
