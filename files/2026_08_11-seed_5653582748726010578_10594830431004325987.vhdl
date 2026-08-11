-- Seed: 5653582748726010578,10594830431004325987

entity h is
  port (lfrko : buffer string(1 to 5));
end h;

architecture a of h is
  
begin
  -- Single-driven assignments
  lfrko <= lfrko;
end a;



-- Seed after: 16871879995235846874,10594830431004325987
