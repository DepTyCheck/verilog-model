-- Seed: 509696035439672419,2983771601630957889

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity yzqfa is
  port (bmsyqnzc : linkage std_logic; bw : linkage time; variable dcvldpmy : inout access_subtype_mirror_pt);
end yzqfa;

architecture tapfby of yzqfa is
  
begin
  
end tapfby;

library ieee;
use ieee.std_logic_1164.all;

entity jm is
  port (oxz : buffer std_logic_vector(3 downto 0); c : inout std_logic; prbzbupw : buffer time);
end jm;

use std.reflection.all;

architecture cvcoqscv of jm is
  shared variable kxv : access_subtype_mirror_pt;
begin
  orcvlknrwi : entity work.yzqfa
    port map (bmsyqnzc => c, bw => prbzbupw, dcvldpmy => kxv);
  
  -- Multi-driven assignments
  c <= c;
  c <= 'X';
  oxz <= ('0', '1', 'X', 'L');
end cvcoqscv;

use std.reflection.all;

entity txtbc is
  port (cftuifp : buffer real; variable zvikhjrhcf : inout record_value_mirror_pt);
end txtbc;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture qdmxfakbd of txtbc is
  shared variable ffrmzm : access_subtype_mirror_pt;
  signal wxd : time;
  shared variable mnehkxg : access_subtype_mirror_pt;
  signal eawcjzr : time;
  signal jyj : std_logic;
  shared variable zvmgtewox : access_subtype_mirror_pt;
  signal asddcnrtto : time;
  signal eosmfakphp : std_logic;
begin
  vcqwtrha : entity work.yzqfa
    port map (bmsyqnzc => eosmfakphp, bw => asddcnrtto, dcvldpmy => zvmgtewox);
  faqz : entity work.yzqfa
    port map (bmsyqnzc => jyj, bw => eawcjzr, dcvldpmy => mnehkxg);
  rlnoy : entity work.yzqfa
    port map (bmsyqnzc => eosmfakphp, bw => wxd, dcvldpmy => ffrmzm);
  
  -- Single-driven assignments
  cftuifp <= 3_0.3_3_2_0;
  
  -- Multi-driven assignments
  eosmfakphp <= eosmfakphp;
end qdmxfakbd;



-- Seed after: 11429897714693351745,2983771601630957889
