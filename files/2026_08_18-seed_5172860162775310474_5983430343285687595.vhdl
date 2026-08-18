-- Seed: 5172860162775310474,5983430343285687595

entity ick is
  port (g : buffer real; slcpd : inout integer_vector(4 downto 4); ebrb : out real);
end ick;

architecture zc of ick is
  
begin
  -- Single-driven assignments
  ebrb <= ebrb;
  slcpd <= (others => 2_1_3);
  g <= 2#1100.1_1#;
end zc;



-- Seed after: 9290453955899680567,5983430343285687595
