-- Seed: 413697123077377183,5472058987609252853

entity iobhruynn is
  port (nqbk : inout time);
end iobhruynn;

architecture sjbal of iobhruynn is
  
begin
  -- Single-driven assignments
  nqbk <= 2#0_1_0_1.0001# ms;
end sjbal;



-- Seed after: 18124781440759848942,5472058987609252853
