-- Seed: 13092523655031893318,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity tl is
  port (ztwtnza : inout integer; dghvyvoafz : buffer severity_level; cbbopjx : inout std_logic_vector(2 to 4); ityyt : buffer integer);
end tl;

architecture rpujz of tl is
  
begin
  -- Single-driven assignments
  ityyt <= 2#1_0#;
  ztwtnza <= ztwtnza;
  
  -- Multi-driven assignments
  cbbopjx <= ('-', 'W', 'W');
  cbbopjx <= cbbopjx;
end rpujz;

entity stmeqa is
  port (dbhtnfllgt : in character);
end stmeqa;

library ieee;
use ieee.std_logic_1164.all;

architecture duwtrshqt of stmeqa is
  signal fwtqpx : integer;
  signal uiwvom : std_logic_vector(2 to 4);
  signal wvs : severity_level;
  signal cwu : integer;
  signal iryogpv : integer;
  signal e : severity_level;
  signal abr : integer;
  signal gpx : integer;
  signal rpxbammz : std_logic_vector(2 to 4);
  signal lt : severity_level;
  signal aie : integer;
begin
  ktbw : entity work.tl
    port map (ztwtnza => aie, dghvyvoafz => lt, cbbopjx => rpxbammz, ityyt => gpx);
  nlzhflg : entity work.tl
    port map (ztwtnza => abr, dghvyvoafz => e, cbbopjx => rpxbammz, ityyt => iryogpv);
  gcasc : entity work.tl
    port map (ztwtnza => cwu, dghvyvoafz => wvs, cbbopjx => uiwvom, ityyt => fwtqpx);
  
  -- Multi-driven assignments
  rpxbammz <= "HZH";
  rpxbammz <= "0-0";
  rpxbammz <= rpxbammz;
end duwtrshqt;



-- Seed after: 385982324958687513,6562834012285671529
