-- Seed: 13830648354712976282,2983771601630957889

use std.reflection.all;

entity zg is
  port (rxrorji : in time; apmy : in integer; variable p : inout file_value_mirror_pt; variable tzpnarrmc : inout access_subtype_mirror_pt);
end zg;

architecture ewkhnappqb of zg is
  
begin
  
end ewkhnappqb;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ewrbkscd is
  port ( variable odns : inout physical_value_mirror_pt
  ; dccw : in std_logic_vector(0 to 4)
  ; mdnupbmakq : in integer_vector(4 downto 2)
  ; czhrqn : inout time
  );
end ewrbkscd;

use std.reflection.all;

architecture sxqpiq of ewrbkscd is
  shared variable buugcda : access_subtype_mirror_pt;
  shared variable m : file_value_mirror_pt;
  signal ea : time;
  shared variable vootbgs : access_subtype_mirror_pt;
  shared variable fpj : file_value_mirror_pt;
  shared variable zpqykqhzh : access_subtype_mirror_pt;
  shared variable aqr : file_value_mirror_pt;
  signal sad : integer;
  signal ri : time;
begin
  fhrg : entity work.zg
    port map (rxrorji => ri, apmy => sad, p => aqr, tzpnarrmc => zpqykqhzh);
  gj : entity work.zg
    port map (rxrorji => czhrqn, apmy => sad, p => fpj, tzpnarrmc => vootbgs);
  fwrawy : entity work.zg
    port map (rxrorji => ea, apmy => sad, p => m, tzpnarrmc => buugcda);
  
  -- Single-driven assignments
  sad <= sad;
  czhrqn <= czhrqn;
  ri <= czhrqn;
end sxqpiq;

entity tqourz is
  port (ugtqfrfldz : buffer boolean_vector(0 downto 2); yyitzyvd : linkage time; qt : buffer bit);
end tqourz;

use std.reflection.all;

architecture tga of tqourz is
  shared variable d : access_subtype_mirror_pt;
  shared variable kodwod : file_value_mirror_pt;
  signal rngip : time;
  shared variable pfmblhy : access_subtype_mirror_pt;
  shared variable s : file_value_mirror_pt;
  shared variable bcb : access_subtype_mirror_pt;
  shared variable due : file_value_mirror_pt;
  shared variable xi : access_subtype_mirror_pt;
  shared variable xouf : file_value_mirror_pt;
  signal mnwqa : integer;
  signal ltfi : time;
begin
  ypijpaykj : entity work.zg
    port map (rxrorji => ltfi, apmy => mnwqa, p => xouf, tzpnarrmc => xi);
  fi : entity work.zg
    port map (rxrorji => ltfi, apmy => mnwqa, p => due, tzpnarrmc => bcb);
  xmopai : entity work.zg
    port map (rxrorji => ltfi, apmy => mnwqa, p => s, tzpnarrmc => pfmblhy);
  rpfnjxot : entity work.zg
    port map (rxrorji => rngip, apmy => mnwqa, p => kodwod, tzpnarrmc => d);
  
  -- Single-driven assignments
  qt <= '0';
  ltfi <= 16#C.271D# fs;
  ugtqfrfldz <= (others => TRUE);
end tga;

use std.reflection.all;

entity nclf is
  port (xdzapdsglc : buffer bit_vector(3 downto 2); variable otvqzgyu : inout access_subtype_mirror_pt; efveswqlz : in bit_vector(2 to 2));
end nclf;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture lyublhaas of nclf is
  signal jckorvhlz : bit;
  signal zr : time;
  signal qqz : boolean_vector(0 downto 2);
  signal kntmxaf : time;
  signal ddbq : integer_vector(4 downto 2);
  signal bpkyq : std_logic_vector(0 to 4);
  shared variable ftsjfs : physical_value_mirror_pt;
begin
  chgapmq : entity work.ewrbkscd
    port map (odns => ftsjfs, dccw => bpkyq, mdnupbmakq => ddbq, czhrqn => kntmxaf);
  n : entity work.tqourz
    port map (ugtqfrfldz => qqz, yyitzyvd => zr, qt => jckorvhlz);
  
  -- Single-driven assignments
  xdzapdsglc <= ('0', '0');
  ddbq <= (4113, 3_0_1, 2#1110#);
  
  -- Multi-driven assignments
  bpkyq <= bpkyq;
  bpkyq <= ('L', 'W', 'W', '1', '1');
  bpkyq <= bpkyq;
end lyublhaas;



-- Seed after: 3421492709542364942,2983771601630957889
