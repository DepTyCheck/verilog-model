-- Seed: 17285766740275239728,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity sxxyqkdcrz is
  port (gjabdrox : out std_logic_vector(3 to 3); y : out std_logic_vector(0 to 1); qeuyth : out real; huvfwveym : buffer real);
end sxxyqkdcrz;

architecture euo of sxxyqkdcrz is
  
begin
  -- Single-driven assignments
  qeuyth <= huvfwveym;
  huvfwveym <= 3.0_0_1_2_2;
  
  -- Multi-driven assignments
  y <= ('1', '1');
  y <= y;
  y <= y;
end euo;

library ieee;
use ieee.std_logic_1164.all;

entity aavm is
  port (u : buffer integer; odxz : in integer; owmoixj : linkage integer; quqleik : out std_logic_vector(4 downto 3));
end aavm;

library ieee;
use ieee.std_logic_1164.all;

architecture vg of aavm is
  signal h : real;
  signal narato : real;
  signal n : real;
  signal nf : real;
  signal odfqjbum : std_logic_vector(3 to 3);
  signal czdxspli : real;
  signal mcr : real;
  signal adeg : std_logic_vector(0 to 1);
  signal umelppbcx : std_logic_vector(3 to 3);
begin
  yjzymh : entity work.sxxyqkdcrz
    port map (gjabdrox => umelppbcx, y => adeg, qeuyth => mcr, huvfwveym => czdxspli);
  c : entity work.sxxyqkdcrz
    port map (gjabdrox => odfqjbum, y => adeg, qeuyth => nf, huvfwveym => n);
  gboulq : entity work.sxxyqkdcrz
    port map (gjabdrox => umelppbcx, y => quqleik, qeuyth => narato, huvfwveym => h);
  
  -- Single-driven assignments
  u <= 2#1_1#;
end vg;



-- Seed after: 337521226301156616,7808623373429384027
