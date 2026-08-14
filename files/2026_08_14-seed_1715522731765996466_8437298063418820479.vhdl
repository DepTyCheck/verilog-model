-- Seed: 1715522731765996466,8437298063418820479

entity ok is
  port (vgi : buffer bit);
end ok;

architecture a of ok is
  
begin
  -- Single-driven assignments
  vgi <= vgi;
end a;



-- Seed after: 13497890038867184115,8437298063418820479
