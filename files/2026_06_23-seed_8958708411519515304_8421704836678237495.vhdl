-- Seed: 8958708411519515304,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity jrvclzeyi is
  port (ojsjc : linkage std_logic_vector(1 downto 0); tzzyyf : out integer);
end jrvclzeyi;

architecture jiqp of jrvclzeyi is
  
begin
  -- Single-driven assignments
  tzzyyf <= 8#0_2_7_3_7#;
end jiqp;

entity jkygihntw is
  port (hgywa : out real; sbzsremor : linkage boolean_vector(4 to 0); grcbsa : in bit);
end jkygihntw;

architecture la of jkygihntw is
  
begin
  -- Single-driven assignments
  hgywa <= 0402.3_1_2_4_4;
end la;

library ieee;
use ieee.std_logic_1164.all;

entity eqj is
  port (buaky : buffer std_logic_vector(3 to 2); jfsukpiceo : out real; gj : in std_logic_vector(1 downto 1));
end eqj;

library ieee;
use ieee.std_logic_1164.all;

architecture nuz of eqj is
  signal azgvr : boolean_vector(4 to 0);
  signal svnxxaie : integer;
  signal wihhhmmoj : bit;
  signal tq : boolean_vector(4 to 0);
  signal p : real;
  signal sxxpg : integer;
  signal yhbsjrmnpl : std_logic_vector(1 downto 0);
begin
  yda : entity work.jrvclzeyi
    port map (ojsjc => yhbsjrmnpl, tzzyyf => sxxpg);
  e : entity work.jkygihntw
    port map (hgywa => p, sbzsremor => tq, grcbsa => wihhhmmoj);
  wg : entity work.jrvclzeyi
    port map (ojsjc => yhbsjrmnpl, tzzyyf => svnxxaie);
  ptknxnqow : entity work.jkygihntw
    port map (hgywa => jfsukpiceo, sbzsremor => azgvr, grcbsa => wihhhmmoj);
  
  -- Single-driven assignments
  wihhhmmoj <= '1';
end nuz;



-- Seed after: 11261795387144883911,8421704836678237495
