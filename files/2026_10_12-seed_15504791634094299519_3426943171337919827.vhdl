-- Seed: 15504791634094299519,3426943171337919827

entity bs is
  port (g : inout integer_vector(4 downto 0));
end bs;

architecture vgpa of bs is
  
begin
  -- Single-driven assignments
  g <= (0_0_2_2_3, 8#2_4_1_5#, 412, 2_3_1_0, 2#1_1_1_1_1#);
end vgpa;



-- Seed after: 12352197337336610896,3426943171337919827
