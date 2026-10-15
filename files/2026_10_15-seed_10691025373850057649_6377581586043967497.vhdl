-- Seed: 10691025373850057649,6377581586043967497

entity bhu is
  port (kwg : out time; jhzeki : linkage time);
end bhu;

architecture xlnqbx of bhu is
  
begin
  -- Single-driven assignments
  kwg <= 16#B7EDA# ps;
end xlnqbx;

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (nnohi : in real; ousprhgyad : out severity_level; vwivvxfdb : out std_logic_vector(3 downto 2));
end k;

architecture yg of k is
  signal uhj : time;
  signal vzsiwpyb : time;
  signal v : time;
  signal xztgervxgr : time;
  signal wd : time;
  signal gn : time;
begin
  sqhqd : entity work.bhu
    port map (kwg => gn, jhzeki => wd);
  gy : entity work.bhu
    port map (kwg => xztgervxgr, jhzeki => v);
  azmjnoqeqc : entity work.bhu
    port map (kwg => vzsiwpyb, jhzeki => uhj);
  
  -- Multi-driven assignments
  vwivvxfdb <= ('X', 'Z');
end yg;

entity frgb is
  port (ker : buffer integer_vector(4 downto 1); ayvxb : buffer integer);
end frgb;

architecture jhyl of frgb is
  signal icefssglig : time;
  signal swznhqjpk : time;
  signal euaorvdi : time;
  signal mmfypzn : time;
  signal uyexqvkhe : time;
  signal ylx : time;
begin
  egm : entity work.bhu
    port map (kwg => ylx, jhzeki => uyexqvkhe);
  byjjtuty : entity work.bhu
    port map (kwg => mmfypzn, jhzeki => euaorvdi);
  raqr : entity work.bhu
    port map (kwg => swznhqjpk, jhzeki => icefssglig);
  
  -- Single-driven assignments
  ayvxb <= 8#31#;
  ker <= (14, 8#0#, 14443, 8#6#);
end jhyl;



-- Seed after: 10614246075256582503,6377581586043967497
