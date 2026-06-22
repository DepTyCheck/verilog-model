-- Seed: 17428220081995046272,13479070923501788437

entity q is
  port (omhxlbonzw : linkage real_vector(1 to 4));
end q;

architecture mxfkrzna of q is
  
begin
  
end mxfkrzna;

entity hiridjmg is
  port (tgawwpuhg : buffer character; kwvtel : linkage integer_vector(2 downto 3));
end hiridjmg;

architecture wbj of hiridjmg is
  signal jebet : real_vector(1 to 4);
  signal botqjlclw : real_vector(1 to 4);
begin
  boaqxdni : entity work.q
    port map (omhxlbonzw => botqjlclw);
  sovpcorxrk : entity work.q
    port map (omhxlbonzw => jebet);
  
  -- Single-driven assignments
  tgawwpuhg <= 'c';
end wbj;

library ieee;
use ieee.std_logic_1164.all;

entity ok is
  port (iqmjvqv : in std_logic_vector(1 downto 0); rnkmk : inout integer; irytcmpipz : buffer severity_level; wp : buffer time);
end ok;

architecture iuhktsh of ok is
  signal caeq : integer_vector(2 downto 3);
  signal yxgqoi : character;
  signal tkykb : real_vector(1 to 4);
  signal pxyn : integer_vector(2 downto 3);
  signal okjqtqpmqt : character;
begin
  yvwbflndgw : entity work.hiridjmg
    port map (tgawwpuhg => okjqtqpmqt, kwvtel => pxyn);
  awmgoc : entity work.q
    port map (omhxlbonzw => tkykb);
  u : entity work.hiridjmg
    port map (tgawwpuhg => yxgqoi, kwvtel => caeq);
end iuhktsh;



-- Seed after: 11066422363303721898,13479070923501788437
