-- Seed: 7912244811846682439,4122021602305298647

entity arnuycvtiv is
  port (pceqp : linkage severity_level; dhf : in time; ofnkij : out integer; pwukqw : inout bit);
end arnuycvtiv;

architecture lhkyubs of arnuycvtiv is
  
begin
  -- Single-driven assignments
  pwukqw <= '1';
end lhkyubs;

entity jpj is
  port (syoioxc : out time; cgkxfkauji : out time_vector(4 downto 2));
end jpj;

architecture yxonza of jpj is
  signal ttf : bit;
  signal hb : integer;
  signal fwhzei : severity_level;
  signal mhuuqznsp : bit;
  signal eqy : integer;
  signal jjovcnm : time;
  signal mjdbqxd : severity_level;
  signal dq : bit;
  signal f : integer;
  signal gltwfympm : time;
  signal sawpin : severity_level;
  signal t : bit;
  signal pmitrqpiiu : integer;
  signal eqoerwydus : time;
  signal kdxzulq : severity_level;
begin
  hl : entity work.arnuycvtiv
    port map (pceqp => kdxzulq, dhf => eqoerwydus, ofnkij => pmitrqpiiu, pwukqw => t);
  zfno : entity work.arnuycvtiv
    port map (pceqp => sawpin, dhf => gltwfympm, ofnkij => f, pwukqw => dq);
  sg : entity work.arnuycvtiv
    port map (pceqp => mjdbqxd, dhf => jjovcnm, ofnkij => eqy, pwukqw => mhuuqznsp);
  r : entity work.arnuycvtiv
    port map (pceqp => fwhzei, dhf => syoioxc, ofnkij => hb, pwukqw => ttf);
end yxonza;

library ieee;
use ieee.std_logic_1164.all;

entity hsfltpveal is
  port (qnjriredz : in integer_vector(2 downto 0); cnvu : linkage std_logic; oc : buffer severity_level);
end hsfltpveal;

architecture ydfxexkh of hsfltpveal is
  signal bdn : time_vector(4 downto 2);
  signal ivrcgbcwr : time;
  signal ws : bit;
  signal b : integer;
  signal jrsrlpv : bit;
  signal y : integer;
  signal esex : time;
  signal kbzkifj : severity_level;
begin
  yqxitt : entity work.arnuycvtiv
    port map (pceqp => kbzkifj, dhf => esex, ofnkij => y, pwukqw => jrsrlpv);
  ptqiiwykf : entity work.arnuycvtiv
    port map (pceqp => oc, dhf => esex, ofnkij => b, pwukqw => ws);
  zpwawfw : entity work.jpj
    port map (syoioxc => ivrcgbcwr, cgkxfkauji => bdn);
  
  -- Single-driven assignments
  esex <= esex;
end ydfxexkh;

library ieee;
use ieee.std_logic_1164.all;

entity fhwvlahnn is
  port (flctbqrag : in std_logic_vector(1 downto 0); nimyvpq : buffer std_logic; ug : out std_logic);
end fhwvlahnn;

architecture fwsurtuca of fhwvlahnn is
  signal awblsr : bit;
  signal bmiotmhrlt : integer;
  signal ewugghvvaz : time;
  signal l : severity_level;
  signal n : severity_level;
  signal rnnja : integer_vector(2 downto 0);
begin
  jfxmg : entity work.hsfltpveal
    port map (qnjriredz => rnnja, cnvu => ug, oc => n);
  nmdhlwjs : entity work.arnuycvtiv
    port map (pceqp => l, dhf => ewugghvvaz, ofnkij => bmiotmhrlt, pwukqw => awblsr);
  
  -- Single-driven assignments
  rnnja <= (4, 21300, 2);
  ewugghvvaz <= 2 min;
  
  -- Multi-driven assignments
  ug <= 'X';
  ug <= 'U';
end fwsurtuca;



-- Seed after: 3197797872867128676,4122021602305298647
