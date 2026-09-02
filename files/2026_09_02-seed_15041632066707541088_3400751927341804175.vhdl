-- Seed: 15041632066707541088,3400751927341804175

entity fd is
  port (a : linkage character; anrdfl : out real; vgbvotlev : in bit);
end fd;

architecture dbdwn of fd is
  
begin
  -- Single-driven assignments
  anrdfl <= 2_3_4_4.13414;
end dbdwn;

library ieee;
use ieee.std_logic_1164.all;

entity nccjbezjn is
  port (bnb : inout integer_vector(3 to 2); addekgwzm : out real; x : inout std_logic_vector(3 to 4); zvzbwjg : in time);
end nccjbezjn;

architecture wjalohmjrh of nccjbezjn is
  
begin
  -- Single-driven assignments
  addekgwzm <= 2#10.01#;
  
  -- Multi-driven assignments
  x <= x;
  x <= "Z0";
end wjalohmjrh;

library ieee;
use ieee.std_logic_1164.all;

entity ok is
  port (la : in integer_vector(3 downto 0); ete : out std_logic_vector(3 downto 0));
end ok;

library ieee;
use ieee.std_logic_1164.all;

architecture ehiizwtte of ok is
  signal ektncwq : bit;
  signal ofzw : real;
  signal tnc : character;
  signal vmnttapkv : bit;
  signal bcfqlkag : real;
  signal s : character;
  signal ji : time;
  signal mlq : std_logic_vector(3 to 4);
  signal nnb : real;
  signal pzxrjjdg : integer_vector(3 to 2);
begin
  dnzk : entity work.nccjbezjn
    port map (bnb => pzxrjjdg, addekgwzm => nnb, x => mlq, zvzbwjg => ji);
  elce : entity work.fd
    port map (a => s, anrdfl => bcfqlkag, vgbvotlev => vmnttapkv);
  oetcdy : entity work.fd
    port map (a => tnc, anrdfl => ofzw, vgbvotlev => ektncwq);
  
  -- Single-driven assignments
  ektncwq <= vmnttapkv;
  vmnttapkv <= '0';
  ji <= 16#6.7D67# ms;
  
  -- Multi-driven assignments
  ete <= ('U', 'X', '1', 'L');
  ete <= ('H', 'Z', '0', 'Z');
end ehiizwtte;



-- Seed after: 11143651478728870126,3400751927341804175
