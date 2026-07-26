-- Seed: 3009099910559053531,7808623373429384027

entity l is
  port (uh : inout real; ui : out real_vector(3 to 2));
end l;

architecture dl of l is
  
begin
  -- Single-driven assignments
  ui <= ui;
  uh <= uh;
end dl;



-- Seed after: 9779690361361381326,7808623373429384027
