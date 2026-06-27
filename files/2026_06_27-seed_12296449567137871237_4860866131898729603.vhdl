-- Seed: 12296449567137871237,4860866131898729603

entity lg is
  port (sxl : inout time_vector(2 downto 2));
end lg;

architecture sypsre of lg is
  
begin
  -- Single-driven assignments
  sxl <= (others => 2_3_2_4_0 ps);
end sypsre;



-- Seed after: 7017730103815270425,4860866131898729603
