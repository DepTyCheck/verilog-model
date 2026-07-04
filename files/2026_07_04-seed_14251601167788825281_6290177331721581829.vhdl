-- Seed: 14251601167788825281,6290177331721581829

use std.reflection.all;

entity jgeramur is
  port (bmdxmk : inout value_mirror; udigoj : inout integer);
end jgeramur;

architecture vinrtzmw of jgeramur is
  
begin
  -- Single-driven assignments
  udigoj <= 16#40F0#;
end vinrtzmw;

entity hek is
  port (rdgbsezh : out time; rkyubn : in time; xldqsru : out integer_vector(2 downto 3));
end hek;

use std.reflection.all;

architecture smklwstba of hek is
  signal qojkbuerf : integer;
  shared variable zlartbxuf : value_mirror;
  signal zuqszghdkg : integer;
  shared variable qgpvzuaogn : value_mirror;
  signal r : integer;
  shared variable vlwvhznb : value_mirror;
begin
  yfpqrsb : entity work.jgeramur
    port map (bmdxmk => vlwvhznb, udigoj => r);
  k : entity work.jgeramur
    port map (bmdxmk => qgpvzuaogn, udigoj => zuqszghdkg);
  wdkqngv : entity work.jgeramur
    port map (bmdxmk => zlartbxuf, udigoj => qojkbuerf);
end smklwstba;

entity ox is
  port (sppuibggzj : out time);
end ox;

use std.reflection.all;

architecture i of ox is
  signal jgyfqxrzy : integer;
  shared variable alczzsvxl : value_mirror;
  signal zcxmplcn : integer;
  shared variable bnxjqvawfn : value_mirror;
  signal otvxdvkh : integer;
  shared variable cxh : value_mirror;
begin
  qy : entity work.jgeramur
    port map (bmdxmk => cxh, udigoj => otvxdvkh);
  wvqzxmor : entity work.jgeramur
    port map (bmdxmk => bnxjqvawfn, udigoj => zcxmplcn);
  ilp : entity work.jgeramur
    port map (bmdxmk => alczzsvxl, udigoj => jgyfqxrzy);
  
  -- Single-driven assignments
  sppuibggzj <= 1 min;
end i;

use std.reflection.all;

entity bxuzsqcurh is
  port (sia : inout access_subtype_mirror);
end bxuzsqcurh;

use std.reflection.all;

architecture vq of bxuzsqcurh is
  signal oftznlbc : integer_vector(2 downto 3);
  signal fw : time;
  signal jh : integer;
  shared variable tn : value_mirror;
  signal opknyn : integer_vector(2 downto 3);
  signal bcyaieshq : time;
begin
  cjd : entity work.hek
    port map (rdgbsezh => bcyaieshq, rkyubn => bcyaieshq, xldqsru => opknyn);
  nsqxqrh : entity work.jgeramur
    port map (bmdxmk => tn, udigoj => jh);
  ddtfxqm : entity work.hek
    port map (rdgbsezh => fw, rkyubn => bcyaieshq, xldqsru => oftznlbc);
end vq;



-- Seed after: 16482482555512260151,6290177331721581829
