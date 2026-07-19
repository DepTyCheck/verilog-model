-- Seed: 1406404851788608207,5511103086789671269

entity szxcjc is
  port (a : out time_vector(0 to 4));
end szxcjc;

architecture wh of szxcjc is
  
begin
  -- Single-driven assignments
  a <= (2#0_1_0_0# ns, 16#0.62# ps, 3 sec, 1.0_4 ns, 8#67015.3# ps);
end wh;



-- Seed after: 13237121509441015672,5511103086789671269
