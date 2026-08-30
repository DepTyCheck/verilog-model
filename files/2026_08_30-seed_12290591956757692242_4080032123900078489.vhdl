-- Seed: 12290591956757692242,4080032123900078489

entity emtfka is
  port (hasyx : in character; uwjbt : linkage character);
end emtfka;

architecture c of emtfka is
  
begin
  
end c;

entity btu is
  port (hz : buffer bit);
end btu;

architecture xmqcj of btu is
  signal fawvjeftg : character;
  signal yfsasut : character;
  signal giyxufisw : character;
  signal muolshbx : character;
  signal kvpkxdswu : character;
  signal qpp : character;
begin
  ofjgbuno : entity work.emtfka
    port map (hasyx => qpp, uwjbt => kvpkxdswu);
  g : entity work.emtfka
    port map (hasyx => qpp, uwjbt => muolshbx);
  woghqvbux : entity work.emtfka
    port map (hasyx => giyxufisw, uwjbt => yfsasut);
  skroufjbe : entity work.emtfka
    port map (hasyx => fawvjeftg, uwjbt => qpp);
  
  -- Single-driven assignments
  hz <= hz;
  fawvjeftg <= kvpkxdswu;
  giyxufisw <= qpp;
end xmqcj;

library ieee;
use ieee.std_logic_1164.all;

entity fdmmnxbta is
  port (tjagdiakbz : in time; h : linkage std_logic; mxkdy : out integer);
end fdmmnxbta;

architecture w of fdmmnxbta is
  signal tpu : character;
  signal ex : bit;
  signal zllvdd : character;
begin
  j : entity work.emtfka
    port map (hasyx => zllvdd, uwjbt => zllvdd);
  f : entity work.btu
    port map (hz => ex);
  ukd : entity work.emtfka
    port map (hasyx => zllvdd, uwjbt => tpu);
  
  -- Single-driven assignments
  mxkdy <= 1_1;
end w;

entity nhozizxovn is
  port (v : out time; unntwmrqc : inout time; duwsnhqfx : inout bit);
end nhozizxovn;

library ieee;
use ieee.std_logic_1164.all;

architecture cguif of nhozizxovn is
  signal aos : character;
  signal ueoukivi : integer;
  signal rszvwzlrd : std_logic;
  signal hwwnjog : time;
begin
  kxklmvju : entity work.fdmmnxbta
    port map (tjagdiakbz => hwwnjog, h => rszvwzlrd, mxkdy => ueoukivi);
  lu : entity work.emtfka
    port map (hasyx => aos, uwjbt => aos);
  
  -- Single-driven assignments
  unntwmrqc <= 2#11100.0# ps;
  duwsnhqfx <= duwsnhqfx;
  hwwnjog <= v;
end cguif;



-- Seed after: 14541748698681700115,4080032123900078489
