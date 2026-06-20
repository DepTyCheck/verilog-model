-- Seed: 2162418475203702493,17924494779688682807

entity smwjj is
  port (zfs : inout integer_vector(0 to 4));
end smwjj;

architecture s of smwjj is
  
begin
  -- Single-driven assignments
  zfs <= (132, 12123, 16#E#, 2#1000#, 8#1062#);
end s;



-- Seed after: 13869154448493416845,17924494779688682807
