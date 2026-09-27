-- Seed: 16065338331580159674,6379010654866854599

entity a is
  port (ckmm : buffer time);
end a;

architecture we of a is
  
begin
  -- Single-driven assignments
  ckmm <= 16#0_7_9# ns;
end we;



-- Seed after: 6429310424478287072,6379010654866854599
