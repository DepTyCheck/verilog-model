-- Seed: 12084016978089020993,4122021602305298647

entity dsr is
  port (jbi : inout time);
end dsr;

architecture ffq of dsr is
  
begin
  -- Single-driven assignments
  jbi <= jbi;
end ffq;



-- Seed after: 18346223044074322833,4122021602305298647
