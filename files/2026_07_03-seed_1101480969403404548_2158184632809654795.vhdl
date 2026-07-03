-- Seed: 1101480969403404548,2158184632809654795

use std.reflection.all;

entity cr is
  port (suybhpdbzl : inout integer_subtype_mirror; cufk : inout bit_vector(4 to 3));
end cr;

architecture tnq of cr is
  
begin
  -- Single-driven assignments
  cufk <= (others => '0');
end tnq;

use std.reflection.all;

entity hzonk is
  port (omq : out boolean; poip : inout enumeration_subtype_mirror; bpkerfyjc : in boolean);
end hzonk;

use std.reflection.all;

architecture girpvrsugi of hzonk is
  signal zmazec : bit_vector(4 to 3);
  shared variable dvtikp : integer_subtype_mirror;
  signal sxhdeg : bit_vector(4 to 3);
  shared variable i : integer_subtype_mirror;
  signal hiqlsw : bit_vector(4 to 3);
  shared variable guowv : integer_subtype_mirror;
begin
  ktcgqgdj : entity work.cr
    port map (suybhpdbzl => guowv, cufk => hiqlsw);
  kb : entity work.cr
    port map (suybhpdbzl => i, cufk => sxhdeg);
  p : entity work.cr
    port map (suybhpdbzl => dvtikp, cufk => zmazec);
  
  -- Single-driven assignments
  omq <= TRUE;
end girpvrsugi;

library ieee;
use ieee.std_logic_1164.all;

entity wivedyy is
  port (ualmcwrfv : buffer real; vc : buffer std_logic; gc : out std_logic);
end wivedyy;

use std.reflection.all;

architecture osxvxdeavb of wivedyy is
  signal qiloztxhjx : bit_vector(4 to 3);
  shared variable ogtmsc : integer_subtype_mirror;
  shared variable gpk : enumeration_subtype_mirror;
  signal mlbvibi : boolean;
  signal trnssbg : bit_vector(4 to 3);
  shared variable rgmpfdvlpt : integer_subtype_mirror;
  signal e : bit_vector(4 to 3);
  shared variable tjgysg : integer_subtype_mirror;
begin
  k : entity work.cr
    port map (suybhpdbzl => tjgysg, cufk => e);
  n : entity work.cr
    port map (suybhpdbzl => rgmpfdvlpt, cufk => trnssbg);
  f : entity work.hzonk
    port map (omq => mlbvibi, poip => gpk, bpkerfyjc => mlbvibi);
  dkx : entity work.cr
    port map (suybhpdbzl => ogtmsc, cufk => qiloztxhjx);
  
  -- Multi-driven assignments
  vc <= '-';
  gc <= vc;
end osxvxdeavb;



-- Seed after: 2783389637933788464,2158184632809654795
