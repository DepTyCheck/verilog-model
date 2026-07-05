-- Seed: 9225606362379402702,3181554006726329157

entity x is
  port (tzv : buffer boolean; oorji : buffer severity_level);
end x;

architecture m of x is
  
begin
  -- Single-driven assignments
  oorji <= FAILURE;
  tzv <= tzv;
end m;



-- Seed after: 9476486697608340932,3181554006726329157
