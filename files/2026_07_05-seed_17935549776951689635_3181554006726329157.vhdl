-- Seed: 17935549776951689635,3181554006726329157

use std.reflection.all;

entity tfqqhmqhi is
  port (empimnmse : inout bit_vector(1 to 3); iwx : inout integer_value_mirror);
end tfqqhmqhi;

architecture wvgasxgz of tfqqhmqhi is
  
begin
  -- Single-driven assignments
  empimnmse <= empimnmse;
end wvgasxgz;

use std.reflection.all;

entity rnz is
  port (trwe : inout access_value_mirror);
end rnz;

use std.reflection.all;

architecture dhxsf of rnz is
  shared variable vlnqrnz : integer_value_mirror;
  signal i : bit_vector(1 to 3);
  shared variable os : integer_value_mirror;
  signal milntvx : bit_vector(1 to 3);
begin
  buxbrhrg : entity work.tfqqhmqhi
    port map (empimnmse => milntvx, iwx => os);
  nkjo : entity work.tfqqhmqhi
    port map (empimnmse => i, iwx => vlnqrnz);
end dhxsf;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (dynucfnj : buffer character; uwoyz : out std_logic; ymja : inout time);
end w;

use std.reflection.all;

architecture txesqr of w is
  shared variable vixqbobqb : integer_value_mirror;
  signal cqsyf : bit_vector(1 to 3);
  shared variable tyei : integer_value_mirror;
  signal kuy : bit_vector(1 to 3);
  shared variable zsfhkdabpz : integer_value_mirror;
  signal usst : bit_vector(1 to 3);
  shared variable fbuccysdew : access_value_mirror;
begin
  wavoai : entity work.rnz
    port map (trwe => fbuccysdew);
  f : entity work.tfqqhmqhi
    port map (empimnmse => usst, iwx => zsfhkdabpz);
  bc : entity work.tfqqhmqhi
    port map (empimnmse => kuy, iwx => tyei);
  owwfgcymr : entity work.tfqqhmqhi
    port map (empimnmse => cqsyf, iwx => vixqbobqb);
  
  -- Multi-driven assignments
  uwoyz <= 'Z';
  uwoyz <= uwoyz;
end txesqr;



-- Seed after: 11525527269902768687,3181554006726329157
