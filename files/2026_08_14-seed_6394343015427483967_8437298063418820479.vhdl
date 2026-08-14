-- Seed: 6394343015427483967,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (cnlyjetteq : out std_logic);
end e;

architecture edys of e is
  
begin
  -- Multi-driven assignments
  cnlyjetteq <= '0';
  cnlyjetteq <= cnlyjetteq;
  cnlyjetteq <= cnlyjetteq;
end edys;

entity bvt is
  port (eb : buffer integer; dqaphhhcei : buffer bit);
end bvt;

library ieee;
use ieee.std_logic_1164.all;

architecture yiae of bvt is
  signal vnhggevikh : std_logic;
begin
  ozhlsw : entity work.e
    port map (cnlyjetteq => vnhggevikh);
  
  -- Single-driven assignments
  dqaphhhcei <= dqaphhhcei;
  eb <= 8#5_7#;
end yiae;



-- Seed after: 2008565536358525971,8437298063418820479
