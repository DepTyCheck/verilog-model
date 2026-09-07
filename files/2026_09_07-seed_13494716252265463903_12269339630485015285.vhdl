-- Seed: 13494716252265463903,12269339630485015285

entity czb is
  port (iff : buffer bit);
end czb;

architecture g of czb is
  
begin
  -- Single-driven assignments
  iff <= iff;
end g;



-- Seed after: 8157389154154659526,12269339630485015285
