-- Seed: 3296768585887978149,5983430343285687595

entity n is
  port (kevl : inout integer);
end n;

architecture ttfi of n is
  
begin
  -- Single-driven assignments
  kevl <= kevl;
end ttfi;



-- Seed after: 6262634250043521165,5983430343285687595
