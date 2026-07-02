-- Seed: 13359347298164631557,13694093582652240945

entity ndmp is
  port (enoyx : linkage character; z : inout bit_vector(4 downto 0); wseclz : buffer real);
end ndmp;

architecture suao of ndmp is
  
begin
  -- Single-driven assignments
  wseclz <= 2#110.1_1_0_1#;
  z <= ('1', '0', '1', '0', '0');
end suao;



-- Seed after: 3697231589567363327,13694093582652240945
