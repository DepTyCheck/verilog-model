-- Seed: 2898670765964949061,3042374792655995433

entity u is
  port (d : buffer time_vector(0 to 2));
end u;

architecture cmw of u is
  
begin
  -- Single-driven assignments
  d <= (2#1_0_0.100# us, 2#110.1_0_0_0_1# ns, 0.2_4_1_4_1 ms);
end cmw;



-- Seed after: 14702555319327320818,3042374792655995433
