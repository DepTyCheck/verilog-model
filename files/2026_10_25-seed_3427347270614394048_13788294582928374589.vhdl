-- Seed: 3427347270614394048,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity xhjlvq is
  port (fjd : in std_logic_vector(0 to 4); oss : inout severity_level; ag : inout time; ziahx : linkage bit);
end xhjlvq;

architecture ryydmz of xhjlvq is
  
begin
  
end ryydmz;

entity mqcwrmbpqx is
  port (cznooqhe : buffer time);
end mqcwrmbpqx;

library ieee;
use ieee.std_logic_1164.all;

architecture bw of mqcwrmbpqx is
  signal mqsllj : bit;
  signal gkqec : time;
  signal psjhjzk : severity_level;
  signal xff : std_logic_vector(0 to 4);
  signal p : bit;
  signal iri : severity_level;
  signal kpwvmu : std_logic_vector(0 to 4);
  signal ig : bit;
  signal cmzypnbucy : time;
  signal rktrddthz : severity_level;
  signal kggbrug : std_logic_vector(0 to 4);
  signal imenbfyctd : bit;
  signal fbwn : time;
  signal ducdfru : severity_level;
  signal e : std_logic_vector(0 to 4);
begin
  h : entity work.xhjlvq
    port map (fjd => e, oss => ducdfru, ag => fbwn, ziahx => imenbfyctd);
  c : entity work.xhjlvq
    port map (fjd => kggbrug, oss => rktrddthz, ag => cmzypnbucy, ziahx => ig);
  wvcxstaym : entity work.xhjlvq
    port map (fjd => kpwvmu, oss => iri, ag => cznooqhe, ziahx => p);
  mhwfusj : entity work.xhjlvq
    port map (fjd => xff, oss => psjhjzk, ag => gkqec, ziahx => mqsllj);
  
  -- Multi-driven assignments
  xff <= xff;
  xff <= ('-', 'H', '-', 'L', 'L');
  kpwvmu <= e;
  e <= ('W', 'X', 'U', 'W', 'H');
end bw;



-- Seed after: 15037351585785448578,13788294582928374589
