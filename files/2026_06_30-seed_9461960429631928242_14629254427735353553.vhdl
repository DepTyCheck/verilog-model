-- Seed: 9461960429631928242,14629254427735353553

entity pw is
  port (mk : inout integer; d : inout integer);
end pw;

architecture j of pw is
  
begin
  -- Single-driven assignments
  d <= 8#64#;
  mk <= 2#0_1_1#;
end j;



-- Seed after: 16989055729897607399,14629254427735353553
