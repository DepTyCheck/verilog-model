-- Seed: 12431364678573972457,10754487200446211253

entity bdd is
  port (dmvq : out bit; hjeimhy : linkage bit_vector(1 downto 3));
end bdd;

architecture ucheikjlw of bdd is
  
begin
  
end ucheikjlw;

library ieee;
use ieee.std_logic_1164.all;

entity xtanzj is
  port (dxhrkgoy : out std_logic_vector(0 to 2); yykfyn : buffer character; bucmvuh : in boolean_vector(4 downto 4); xrlfy : buffer real);
end xtanzj;

architecture wrr of xtanzj is
  signal rmxspluse : bit_vector(1 downto 3);
  signal hcbyavd : bit;
  signal xm : bit_vector(1 downto 3);
  signal erjqkfgxej : bit;
  signal bt : bit_vector(1 downto 3);
  signal v : bit;
begin
  wwee : entity work.bdd
    port map (dmvq => v, hjeimhy => bt);
  tujhqztz : entity work.bdd
    port map (dmvq => erjqkfgxej, hjeimhy => xm);
  apeij : entity work.bdd
    port map (dmvq => hcbyavd, hjeimhy => rmxspluse);
  
  -- Single-driven assignments
  yykfyn <= 'z';
  xrlfy <= xrlfy;
  
  -- Multi-driven assignments
  dxhrkgoy <= ('0', 'U', 'U');
  dxhrkgoy <= ('L', 'U', 'X');
  dxhrkgoy <= "LXX";
  dxhrkgoy <= "WL1";
end wrr;



-- Seed after: 12646348683356942221,10754487200446211253
