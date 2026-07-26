-- Seed: 5295630402503733048,7808623373429384027

entity rufjmbpowl is
  port (eov : inout time);
end rufjmbpowl;

architecture xmsi of rufjmbpowl is
  
begin
  -- Single-driven assignments
  eov <= 2#0_0_1_1_1.11# ns;
end xmsi;



-- Seed after: 1027470993529155756,7808623373429384027
