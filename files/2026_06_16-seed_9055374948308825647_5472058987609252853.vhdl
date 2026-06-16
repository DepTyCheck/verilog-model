-- Seed: 9055374948308825647,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity dbgfrsz is
  port (ugwhp : inout boolean_vector(3 downto 0); awccgb : out severity_level; q : linkage std_logic; offqkbksz : inout std_logic_vector(2 downto 4));
end dbgfrsz;

architecture esuvl of dbgfrsz is
  
begin
  -- Single-driven assignments
  ugwhp <= (TRUE, FALSE, FALSE, TRUE);
  awccgb <= ERROR;
  
  -- Multi-driven assignments
  offqkbksz <= (others => '0');
  offqkbksz <= "";
  offqkbksz <= (others => '0');
end esuvl;

library ieee;
use ieee.std_logic_1164.all;

entity qn is
  port (nmebmmv : in std_logic_vector(2 to 1); fass : in time; xmuizezze : linkage severity_level);
end qn;

architecture xcnw of qn is
  
begin
  
end xcnw;

entity kdtviqox is
  port (jfvte : inout real; dududn : inout integer_vector(3 downto 2); kwag : out severity_level; beact : buffer integer);
end kdtviqox;

architecture gpyrn of kdtviqox is
  
begin
  -- Single-driven assignments
  jfvte <= 8#0.2_4_0_0_2#;
  dududn <= (16#9_E_4_0_1#, 0_1_3);
  kwag <= WARNING;
  beact <= 0224;
end gpyrn;

library ieee;
use ieee.std_logic_1164.all;

entity wlmuvuwca is
  port (r : out std_logic);
end wlmuvuwca;

architecture njyaja of wlmuvuwca is
  signal ijnmvqvhgz : integer;
  signal f : severity_level;
  signal a : integer_vector(3 downto 2);
  signal gyrnbzf : real;
begin
  xogvv : entity work.kdtviqox
    port map (jfvte => gyrnbzf, dududn => a, kwag => f, beact => ijnmvqvhgz);
  
  -- Multi-driven assignments
  r <= '-';
  r <= '-';
  r <= 'W';
end njyaja;



-- Seed after: 10789594211167073326,5472058987609252853
